#include "UsedFunctionAndTypeCollector.hpp"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "llvm/Support/raw_ostream.h"


using namespace clang;
using namespace ast_matchers;
using namespace llvm;

extern cl::opt<std::string> entryFunctionName;
extern cl::opt<bool> debug;

// any functions in this list will not be marked as used, and in turn will be
// commented out later by the preprocessor (if UnusedDeclCommenter is used)
static const std::vector<std::string> ignoredFuns = {
    "__assert_fail", "__assert_perror_fail", "__assert", "reach_error", 
    "__VERIFIER_error", "static_assert", "assert", "assume", "malloc",
    "__VERIFIER_assume", "calloc", "realloc", "free", "abort", "exit",
    "memset", "memcmp"};

void handleFunDecl(const clang::FunctionDecl* funDecl,
                   clang::ast_matchers::MatchFinder::MatchCallback* handler,
                   llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls,
                   clang::ASTContext *Ctx);

// A visitor to find functions that are used as pointers.
class FunctionReferenceVisitor
    : public clang::RecursiveASTVisitor<FunctionReferenceVisitor> {
public:
  FunctionReferenceVisitor(llvm::SetVector<FunctionInfo *> &seenFunctions,
                           clang::ast_matchers::MatchFinder::MatchCallback *handler,
                           llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls,
                           clang::ASTContext *Ctx)
      : seenFunctions(seenFunctions), handler(handler), seenFieldDecls(seenFieldDecls), Ctx(Ctx) {}

  // Handles implicit decay of a function to a pointer.
  // This covers assignments and initializations of function pointers.
  bool VisitImplicitCastExpr(clang::ImplicitCastExpr *E) {
    if (E->getCastKind() == clang::CK_FunctionToPointerDecay) {
      if (auto *DRE = dyn_cast<clang::DeclRefExpr>(E->getSubExpr()->IgnoreParenImpCasts())) {
        if (auto *FD = dyn_cast<clang::FunctionDecl>(DRE->getDecl())) {
          handleFoundFunction(FD);
        }
      }
    }
    return true;
  }

  // Handles explicit taking of a function's address.
  bool VisitUnaryOperator(clang::UnaryOperator *E) {
    if (E->getOpcode() == clang::UO_AddrOf) {
      if (auto *DRE =
          dyn_cast<clang::DeclRefExpr>(E->getSubExpr()->IgnoreParenImpCasts())) {
        if (auto *FD = dyn_cast<clang::FunctionDecl>(DRE->getDecl())) {
          handleFoundFunction(FD);
        }
      }
    }
    return true;
  }

private:
  llvm::SetVector<FunctionInfo *> &seenFunctions;
  llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls;
  clang::ast_matchers::MatchFinder::MatchCallback *handler;
  clang::ASTContext *Ctx;

  void handleFoundFunction(const clang::FunctionDecl *FD) {
    for (const auto *seenFuncInfo : seenFunctions) {
      if (declaresSameEntity(seenFuncInfo->getDecl(), FD)) {
        return; // Already seen.
      }
    }

    const clang::FunctionDecl *funcWithBody = FD;
    if (!funcWithBody->hasBody()) {
      for (const auto* redecl : funcWithBody->redecls()) {
        if (redecl->hasBody()) {
          funcWithBody = redecl;
          break;
        }
      }
    }

    seenFunctions.insert(new FunctionInfo(funcWithBody, false));
    handleFunDecl(funcWithBody, handler, seenFieldDecls, Ctx);
  }
};

class FindDesignatedInitMatcher : public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  explicit FindDesignatedInitMatcher(llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls)
      : seenFieldDecls(seenFieldDecls) {}

  void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
    if (const auto *initExpr = Result.Nodes.getNodeAs<clang::DesignatedInitExpr>("designatedInit")) {
      // A designated initializer uses a field, mark it as seen.
      for (const auto &designator : initExpr->designators()) {
        if (const auto *fieldDecl = designator.getField()) {
          if (debug) {
            llvm::dbgs() << "[Collector] Found DESIGNATED INITIALIZER access to field '"
                         << fieldDecl->getParent()->getNameAsString() << "::"
                         << fieldDecl->getNameAsString() << "' at ";
            initExpr->getBeginLoc().print(llvm::dbgs(), *Result.SourceManager);
            llvm::dbgs() << ". Adding to seen set.\n";
          }
          seenFieldDecls.insert(fieldDecl->getCanonicalDecl());
        }
      }
    }
  }
private:
  llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls;
};

class FindFunPtrRefMatcher : public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  explicit FindFunPtrRefMatcher(llvm::SetVector<const clang::VarDecl*> &seenFunPtrDecls)
      : seenFunPtrDecls(seenFunPtrDecls) {}

  void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
    // Finds a reference to a variable that is a function pointer.
    if (const auto *vd = Result.Nodes.getNodeAs<clang::VarDecl>("funPtrVar")) {
      if (debug) {
        llvm::dbgs() << "[Collector] Matched a reference to function pointer '"
                     << vd->getNameAsString() << "' at ";
        Result.SourceManager->getSpellingLoc(Result.Nodes.getNodeAs<clang::DeclRefExpr>("declRefExpr")->getBeginLoc())
            .print(llvm::dbgs(), *Result.SourceManager);
        llvm::dbgs() << ". Adding to seen set.\n";
      }
      seenFunPtrDecls.insert(vd->getCanonicalDecl());
    }
  }
private:
  llvm::SetVector<const clang::VarDecl*> &seenFunPtrDecls;
};

class FindPositionalInitMatcher : public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  explicit FindPositionalInitMatcher(llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls)
      : seenFieldDecls(seenFieldDecls) {}

  void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
    if (const auto *initList = Result.Nodes.getNodeAs<clang::InitListExpr>("initList")) {
      const auto *recordType = initList->getType()->getAs<RecordType>();
      if (!recordType) return;

      const RecordDecl *recordDecl = recordType->getDecl();
      int i = 0;
      for (const auto *field : recordDecl->fields()) {
        // initializer, field is used.
        if (i < initList->getNumInits()) {
          if (debug) {
            llvm::dbgs() << "[Collector] Found POSITIONAL INITIALIZER access to field '"
                         << field->getParent()->getNameAsString() << "::"
                         << field->getNameAsString() << "'. Adding to seen set.";
          }
          seenFieldDecls.insert(field->getCanonicalDecl());
          i++;
        } else {
          break; // no more initializers
        }
      }
    }
  }
private:
  llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls;
};

class FindFieldAccessMatcher : public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  explicit FindFieldAccessMatcher(llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls)
      : seenFieldDecls(seenFieldDecls) {}

  void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
    if (const auto *memberExpr = Result.Nodes.getNodeAs<clang::MemberExpr>("memberAccess")) {
      if (const auto *fieldDecl = dyn_cast<clang::FieldDecl>(memberExpr->getMemberDecl())) {
        const RecordDecl *parent = fieldDecl->getParent();
        if (parent->isUnion()) {
          // If it's a union, an access to one field means all fields are "live".
          if (debug) {
            llvm::dbgs() << "[Collector] Access to union field '" << fieldDecl->getNameAsString()
                         << "'. Marking ALL fields in union '" << parent->getNameAsString() << "' as seen.\n";
          }
          for (const auto *otherField: parent->fields()) {
            seenFieldDecls.insert(otherField->getCanonicalDecl());
          }
        } else {
          if (debug) {
            llvm::dbgs() << "[Collector] Found access to field '"
                         << fieldDecl->getParent()->getNameAsString() << "::"
                         << fieldDecl->getNameAsString() << "' at ";
            memberExpr->getBeginLoc().print(llvm::dbgs(), *Result.SourceManager);
            llvm::dbgs() << ". Adding to seen set.\n";
          }
          seenFieldDecls.insert(fieldDecl->getCanonicalDecl());
        }
      }
    }
  }
private:
  llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls;
};

// todo: add another handler to only collect types
void handleFunDecl(const clang::FunctionDecl* funDecl,
                   clang::ast_matchers::MatchFinder::MatchCallback* handler,
                   llvm::SetVector<const clang::FieldDecl*> &seenFieldDecls,
                   clang::ASTContext *Ctx) {

  //std::string funName = funDecl->getNameAsString();
  //if (std::find(ignoredFuns.begin(), ignoredFuns.end(), funName) != 
  //  ignoredFuns.end())
  //  return;
  
  StatementMatcher CallSiteOrDeclRefMatcher = anyOf(
  callExpr(
    hasAncestor(functionDecl(hasName(funDecl->getName())).bind("enclosing")),
    callee(functionDecl().bind("callee"))).bind("caller"),
  declRefExpr(allOf(hasAncestor(functionDecl(hasName(funDecl->getName())).bind("enclosing")),
    unless(anyOf(hasType(functionProtoType()),
      hasType(functionType()),
      hasType(builtinType()))),
    anyOf(hasType(typedefType().bind("typedefTyp")),
      hasType(arrayType().bind("arrayTyp")),
      hasType(recordType().bind("recordTyp")),
      anything()))
  ).bind("referenceExpr"));
  
  TypeLocMatcher usedTypesMatcher = typeLoc(
    loc(qualType().bind("usedType")), 
    hasAncestor(expr()), // this ensures that the type is used in an expr.
    hasAncestor(functionDecl(hasName(funDecl->getName())))).bind("usedTypeLoc");

  clang::ast_matchers::MatchFinder finder;
  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, CallSiteOrDeclRefMatcher), handler);
  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, usedTypesMatcher), handler);
  finder.matchAST(*Ctx);

  if (funDecl->hasBody()) {
    auto *matcher = static_cast<FindFunctionMatcher *>(handler);
    FunctionReferenceVisitor refVisitor(matcher->getSeenFunctions(), handler, matcher->getSeenFieldDecls(), Ctx);
    refVisitor.TraverseStmt(funDecl->getBody());

    FindFieldAccessMatcher fieldHandler(seenFieldDecls);
    clang::ast_matchers::MatchFinder fieldFinder;
    fieldFinder.addMatcher(
        clang::ast_matchers::memberExpr().bind("memberAccess"),
        &fieldHandler
    );
    fieldFinder.match(*funDecl->getBody(), *Ctx);
  }
}

// this is the first consumer for the AST, it only finds the entry point,
// and creates sub-consumers (UsedFunAndTypeASTConsumerHelper) to find
// called functions and used types
UsedFunAndTypeASTConsumer::UsedFunAndTypeASTConsumer(
    llvm::SetVector<FunctionInfo *> &seenFunctions,
    llvm::SetVector<const clang::Type *> &seenTypes,
    llvm::SetVector<const clang::FieldDecl *> &seenFieldDecls,
    bool collectAllFuns)
    : handler(seenFunctions, seenTypes, seenFieldDecls, collectAllFuns),
      seenFunctions(seenFunctions), seenTypes(seenTypes),
      seenFieldDecls(seenFieldDecls), collectAllFuns(collectAllFuns) {

  if (collectAllFuns) {
    DeclarationMatcher entryMatcher = anyOf(
      functionDecl(hasDescendant(callExpr(callee(functionDecl( // a recursive fun
        equalsBoundNode("allFunDecl")))).bind("recCall"))).bind("allFunDecl"),
      functionDecl().bind("allFunDecl")); // a non-recursive fun
    finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, entryMatcher), &handler);
  } else {
    DeclarationMatcher entryMatcher = functionDecl(hasName(entryFunctionName)).bind("main");
    // todo: other entry points than main
    finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, entryMatcher), &handler);
  }
}

// This is the handler that is called when the entry function is found
void FindEntryFunctionMatcher::run(const MatchFinder::MatchResult &Result) {
  clang::ASTContext *Ctx = Result.Context;

  const auto * entryFunction =
    Result.Nodes.getNodeAs<clang::FunctionDecl>("main"); 
  
  const auto * allFunDecl =
    Result.Nodes.getNodeAs<clang::FunctionDecl>("allFunDecl"); 

  if (entryFunction) {
    //llvm::outs() << "main found...\n";
    seenFunctions.insert(new FunctionInfo(entryFunction, false)); // todo: add main args as seen types
    handleFunDecl(entryFunction, &subHandler, subHandler.getSeenFieldDecls(), Ctx);
  } else if (allFunDecl) {
    const auto * recCall =
      Result.Nodes.getNodeAs<clang::CallExpr>("allFunDecl");
    seenFunctions.insert(new FunctionInfo(allFunDecl, recCall != nullptr));
    handleFunDecl(allFunDecl, &subHandler, subHandler.getSeenFieldDecls(), Ctx);
    // todo: another handler which only collects types would be more efficient above
  }else {
    // if entryFunction is null, the entry point could not be found
    llvm_unreachable("main function could not be found.");
  }
}

void FindFunctionMatcher::run(const MatchFinder::MatchResult &Result) {
  // ASTContext is used to retrieve the source locations
  ASTContext *Ctx = Result.Context;

  const auto * CalleeDecl =
      Result.Nodes.getNodeAs<clang::FunctionDecl>("callee");
  const auto * EnclosingDecl =
      Result.Nodes.getNodeAs<clang::FunctionDecl>("enclosing");
  const auto * TheCall = Result.Nodes.getNodeAs<clang::CallExpr>("caller");
  
  //llvm::outs() << CalleeDecl->getNameAsString() << " called from " << 
  //  EnclosingDecl->getNameAsString() << "\n";

  const auto * usedTypeLoc = Result.Nodes.getNodeAs<clang::TypeLoc>("usedTypeLoc");
  
  const auto * TheRef = Result.Nodes.getNodeAs<clang::DeclRefExpr>("referenceExpr");
  if(TheRef) { // matched a DeclRefExpr                              
    //llvm::outs() << "matched a ref\n";
    //TheRef->dump();
    QualType typ = TheRef->getType();
    //bool hasBuiltinType = typ->isBuiltinType();

    //if (!hasBuiltinType) {
      TypeCollectorVisitor typeVisitor(*Ctx, seenFunctions, seenTypes);
      typeVisitor.TraverseType(typ);
    //}
  } else if (CalleeDecl) { // matched a CallExpr
    //llvm::outs() << EnclosingDecl->getNameAsString() << "\n";
   
    assert(declaresSameEntity(TheCall->getDirectCallee(), CalleeDecl));

    // todo: maybe detect mutually recursive funs, or recursion after several steps
    bool functionIsRecursive = declaresSameEntity(CalleeDecl, EnclosingDecl);
    
    bool functionSeenBefore = false;
    if (std::find(ignoredFuns.begin(), ignoredFuns.end(), 
        CalleeDecl->getNameAsString()) != ignoredFuns.end())
      functionSeenBefore = true;
    else
      for(auto seenFunction : seenFunctions){
        if(declaresSameEntity(seenFunction->getDecl(), CalleeDecl)){
          functionSeenBefore = true;
          if(!seenFunction->isRecursive() && functionIsRecursive)
            seenFunction->setRecursive();
          break;
        }
      }
    //if(functionIsRecursive)
    //    llvm::outs() << "  " << CalleeDecl->getNameAsString() << " is recursive!\n";

    if (!functionSeenBefore) {
      //llvm::outs() << "seeing " << CalleeDecl->getNameAsString() << " for the first time!\n";
      
      // try to find a declaration with a body, if one exists
      const FunctionDecl * calleeWithBody = CalleeDecl;
      for (auto decl : CalleeDecl->redecls()) {
        if (decl->hasBody())
          calleeWithBody = decl;
      }
      seenFunctions.insert(new FunctionInfo(calleeWithBody, functionIsRecursive)); // callee not seen before

      // mark the return and argument types of this function as seen
      TypeCollectorVisitor typeVisitor(*Ctx, seenFunctions, seenTypes);
      typeVisitor.TraverseType(CalleeDecl->getReturnType());
      for (auto paramDecl : CalleeDecl->parameters())
        typeVisitor.TraverseType(paramDecl->getType());

      //llvm::outs() << "  " << CalleeDecl->getNameAsString() << " called from " << 
      //  EnclosingDecl->getNameAsString() << "\n";
      
      handleFunDecl(calleeWithBody, this, this->seenFieldDecls, Ctx);
    }
  } else if (usedTypeLoc) {
      const auto* usedType =  Result.Nodes.getNodeAs<clang::QualType>("usedType");
    //if (!usedType->getTypePtr()->isBuiltinType()) {
      //llvm::outs() << "seen type\n";
      //usedType->dump();
      TypeCollectorVisitor typeVisitor(*Ctx, seenFunctions, seenTypes);
      typeVisitor.TraverseType(*usedType);
    //}
  } else {
    llvm_unreachable("Handler without any matches should not be possible...");
  }
}

bool TypeCollectorVisitor::VisitType(clang::Type *typ) {
  //typ->dump();
  const clang::Type* canonicalType = typ->getCanonicalTypeUnqualified().getTypePtr();
  if (/*!canonicalType->isBuiltinType() && */!canonicalType->isPointerType()) {
    bool res = seenTypes.insert(typ->getCanonicalTypeUnqualified().getTypePtr());
    if (res) {
      //llvm::outs() << "inserted unseen type: \n";
      //typ->getCanonicalTypeUnqualified().dump();
      if (typ->isRecordType()) {
        //llvm::outs() << "this is a record type, checking fields...\n";
        RecordDecl* recordDecl = typ->getAsRecordDecl();
        //recordDecl->dump();
        TypeCollectorVisitor typeVisitor(Ctx, seenFunctions, seenTypes);
        for (auto field : recordDecl->fields()) {
          //llvm::outs() << "field: \n";
          //field->dump();
          typeVisitor.TraverseType(field->getType());
        }
      }
    }
  } else if (canonicalType->isPointerType()) {
    //llvm::outs() << "pointer type\n";
    TypeCollectorVisitor typeVisitor(Ctx, seenFunctions, seenTypes);
    typeVisitor.TraverseType(canonicalType->getPointeeType());
  } /*else {
    llvm::outs() << "builtin type\n";
    llvm::outs() << "------------------------------------------------\n";
    return false;
  }*/
  return true;
}

UsedFunAndTypeCollector::UsedFunAndTypeCollector(clang::ASTContext &Ctx, bool collectAllFuns, bool collectAllTypes)
  : collectAllTypes(collectAllTypes) {
  UsedFunAndTypeASTConsumer c(seenFunctions, seenTypes, seenFieldDecls, collectAllFuns);
  c.HandleTranslationUnit(Ctx);

  clang::ast_matchers::MatchFinder funPtrFinder;
  FindFunPtrRefMatcher funPtrHandler(seenFunPtrDecls);

  FindDesignatedInitMatcher designatedInitHandler(seenFieldDecls);
  MatchFinder designatedInitFinder;
  designatedInitFinder.addMatcher(
      designatedInitExpr().bind("designatedInit"),
      &designatedInitHandler
  );
  designatedInitFinder.matchAST(Ctx);

  FindPositionalInitMatcher positionalInitHandler(seenFieldDecls);
  MatchFinder positionalInitFinder;
  // This matcher is tricky: we want init lists for records,
  // but only if they don't have designated initializers (which we already handle).
  positionalInitFinder.addMatcher(
      initListExpr(
          hasType(recordType()),
          unless(has(designatedInitExpr()))
      ).bind("initList"),
      &positionalInitHandler
  );
  positionalInitFinder.matchAST(Ctx);

// Match any expression that refers to a VarDecl of a function pointer type.
  auto funPtrVarMatcher = clang::ast_matchers::varDecl(
      clang::ast_matchers::hasType(
          clang::ast_matchers::pointerType(
              clang::ast_matchers::pointee(
                  clang::ast_matchers::functionProtoType()
              )
          )
      )
  ).bind("funPtrVar");

  funPtrFinder.addMatcher(
        clang::ast_matchers::declRefExpr(clang::ast_matchers::to(funPtrVarMatcher)).bind("declRefExpr"),
        &funPtrHandler
    );

  funPtrFinder.matchAST(Ctx);

  if (debug) {
    llvm::dbgs() << "[Collector] Starting scan of global variable initializers...\n";
  }

  auto Decls = Ctx.getTranslationUnitDecl()->decls();
  for (auto *Decl : Decls) {
    if (const auto *VD = dyn_cast<VarDecl>(Decl)) {
      if (VD->hasInit()) {
        FunctionReferenceVisitor refVisitor(seenFunctions,
                                            &c.getHandler()->getSubHandler(),
                                            seenFieldDecls, &Ctx);
        refVisitor.TraverseStmt(const_cast<Expr *>(VD->getInit()));
      }
    }
  }
}

bool UsedFunAndTypeCollector::functionPointerIsSeen(const clang::VarDecl* vd) const {
  return seenFunPtrDecls.count(vd->getCanonicalDecl());
}

bool UsedFunAndTypeCollector::fieldIsSeen(const clang::FieldDecl* fd) const {
  return seenFieldDecls.count(fd->getCanonicalDecl());
}

FunctionInfo* UsedFunAndTypeCollector::getFunctionInfo (
  const clang::FunctionDecl* funDecl) const {
  for (auto funInfo : seenFunctions)
    if (declaresSameEntity(funDecl, funInfo->getDecl()))
      return funInfo;
  return nullptr;
}

bool UsedFunAndTypeCollector::functionIsSeen(
  const clang::FunctionDecl* funDecl) const {
  return getFunctionInfo(funDecl) != nullptr;
}

bool UsedFunAndTypeCollector::functionIsRecursive(
  const clang::FunctionDecl* funDecl) const {
  const FunctionInfo* funInfo = getFunctionInfo(funDecl);
  if (funInfo)
    return funInfo->isRecursive();
  else 
    llvm_unreachable("Tried to check if unseen function is recursive!");
}

bool UsedFunAndTypeCollector::typeIsSeen(const clang::Type* t) const {
  if (collectAllTypes) return true;
  for (auto typ : seenTypes)
    if (typ == t)
      return true;
  return false;
}

bool UsedFunAndTypeCollector::typeIsSeen(const clang::QualType* t) const {
  return typeIsSeen(t->getTypePtr());
}

UsedFunAndTypeCollector::~UsedFunAndTypeCollector() {
  // cleanup
  for (int i = 0; i < seenFunctions.size(); ++i)
    delete seenFunctions[i];
}

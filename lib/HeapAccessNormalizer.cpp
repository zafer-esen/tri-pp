#include "HeapAccessNormalizer.hpp"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

using namespace clang;

class HeapAccessNormalizerImpl {
private:
  Rewriter &TheRewriter;
  ASTContext &Context;
  bool hasMadeChanges = false;
  int tempVarCounter = 1;

  std::string heapType;

  llvm::DenseMap<const Stmt *, const Stmt *> ParentMap;
  class ParentMapper : public RecursiveASTVisitor<ParentMapper> {
  public:
    explicit ParentMapper(llvm::DenseMap<const Stmt *, const Stmt *> &PM)
        : TheParentMap(PM) {}
    bool TraverseStmt(Stmt *S) {
      if (!S)
        return true;
      for (Stmt *Child : S->children()) {
        if (Child)
          TheParentMap[Child] = S;
      }
      return RecursiveASTVisitor<ParentMapper>::TraverseStmt(S);
    }

  private:
    llvm::DenseMap<const Stmt *, const Stmt *> &TheParentMap;
  };

  const Stmt *getEnclosingTopLevelStmt(const Expr *E) {
    const Stmt *current = E;
    while (current) {
      auto it = ParentMap.find(current);
      if (it == ParentMap.end() || !it->second ||
          isa<CompoundStmt>(it->second)) {
        return current;
      }
      current = it->second;
    }
    return nullptr;
  }

  class StackVariableLocator
      : public RecursiveASTVisitor<StackVariableLocator> {
  public:
    std::set<const VarDecl *> stackDecls;
    bool VisitUnaryOperator(UnaryOperator *UO) {
      if (UO->getOpcode() == UO_AddrOf) {
        if (const auto *DRE =
                dyn_cast<DeclRefExpr>(UO->getSubExpr()->IgnoreParenCasts())) {
          if (const auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
            stackDecls.insert(VD);
          }
        }
      }
      return true;
    }
    bool VisitParmVarDecl(ParmVarDecl *PD) {
      if (PD->getType()->isPointerType() &&
          PD->getType()->getPointeeType()->isPointerType()) {
        stackDecls.insert(PD);
      }
      return true;
    }
  };

  class HeapAccessLocator : public RecursiveASTVisitor<HeapAccessLocator> {
  public:
    std::vector<const MemberExpr *> arrowExprs;
    bool VisitMemberExpr(MemberExpr *ME) {
      if (ME->isArrow())
        arrowExprs.push_back(ME);
      return true;
    }
    bool TraverseWhileStmt(WhileStmt *S) {
      if (S->getCond()) TraverseStmt(S->getCond());
      if (S->getBody()) TraverseStmt(S->getBody());
      return true;
    }
    bool TraverseForStmt(ForStmt *S) {
      if (S->getInit()) TraverseStmt(S->getInit());
      if (S->getCond()) TraverseStmt(S->getCond());
      if (S->getInc()) TraverseStmt(S->getInc());
      if (S->getBody()) TraverseStmt(S->getBody());
      return true;
    }
    bool TraverseIfStmt(IfStmt *S) {
      if (S->getCond()) TraverseStmt(S->getCond());
      if (S->getThen()) TraverseStmt(S->getThen());
      if (S->getElse()) TraverseStmt(S->getElse());
      return true;
    }
    bool TraverseDoStmt(DoStmt *S) {
      if (S->getBody()) TraverseStmt(S->getBody());
      if (S->getCond()) TraverseStmt(S->getCond());
      return true;
    }
  };

  class MallocInitializerHandler
      : public RecursiveASTVisitor<MallocInitializerHandler> {
  private:
    HeapAccessNormalizerImpl &Normalizer;
    std::set<const Stmt *> ConsumedStmts;

    struct MallocSiteInfo {
      const VarDecl *PointerVar = nullptr;
      QualType StructureType;
      const RecordDecl *StructDecl = nullptr;
      const Stmt* MallocStmt = nullptr;
    };

    llvm::Optional<MallocSiteInfo> getMallocSiteInfo(const Stmt *S) {
      const VarDecl *VD = nullptr;
      const CallExpr *CE = nullptr;

      if (const auto *DS = dyn_cast<DeclStmt>(S)) {
        if (DS->isSingleDecl()) {
          if(const auto* SingleVD = dyn_cast<VarDecl>(DS->getSingleDecl())) {
            VD = SingleVD;
            if (VD && VD->hasInit()) {
                CE = dyn_cast<CallExpr>(VD->getInit()->IgnoreParenCasts());
            }
          }
        }
      } else if (const auto *BO = dyn_cast<BinaryOperator>(S)) {
        if (BO->isAssignmentOp()) {
          if (const auto *DRE =
                  dyn_cast<DeclRefExpr>(BO->getLHS()->IgnoreParenCasts())) {
            VD = dyn_cast<VarDecl>(DRE->getDecl());
          }
          CE = dyn_cast<CallExpr>(BO->getRHS()->IgnoreParenCasts());
        }
      }

      if (!VD || !CE)
        return llvm::None;

      if (auto *callee = CE->getDirectCallee()) {
        if (callee->getNameAsString() == "malloc") {
          QualType pointerType = VD->getType();
          if (pointerType->isPointerType()) {
            QualType pointeeType = pointerType->getPointeeType();
            if (const auto *RT = pointeeType->getAs<RecordType>()) {
              if (RT->getDecl()->isStruct()) {
                MallocSiteInfo Info;
                Info.PointerVar = VD;
                Info.StructureType = pointeeType;
                Info.StructDecl = RT->getDecl();
                Info.MallocStmt = S;
                return Info;
              }
            }
          }
        }
      }
      return llvm::None;
    }

    bool isFieldInitializerForVar(const BinaryOperator *BO,
                                  const VarDecl *targetVar) {
      if (!BO->isAssignmentOp())
        return false;
      if (const auto *ME =
              dyn_cast<MemberExpr>(BO->getLHS()->IgnoreParenImpCasts())) {
        if (ME->isArrow()) {
          if (const auto *DRE =
                  dyn_cast<DeclRefExpr>(ME->getBase()->IgnoreParenImpCasts())) {
            return DRE->getDecl() == targetVar;
          }
        }
      }
      return false;
    }

  public:
    MallocInitializerHandler(HeapAccessNormalizerImpl &N) : Normalizer(N) {}

    const std::set<const Stmt *> &getConsumedStmts() const {
      return ConsumedStmts;
    }

    bool VisitCompoundStmt(CompoundStmt *CS) {
      for (auto it = CS->body_begin(); it != CS->body_end(); ++it) {
        const Stmt *currentStmt = *it;
        if (ConsumedStmts.count(currentStmt))
          continue;

        auto siteInfoOpt = getMallocSiteInfo(currentStmt);
        if (!siteInfoOpt)
          continue;

        MallocSiteInfo &siteInfo = *siteInfoOpt;
        Normalizer.analyzeHeapType(siteInfo.StructureType);

        llvm::outs() << "LOG: Found malloc for var '"
                     << siteInfo.PointerVar->getNameAsString() << "' of type '"
                     << siteInfo.StructureType.getAsString() << "'.\n";

        std::vector<const BinaryOperator *> initializers;
        std::map<const FieldDecl *, const BinaryOperator *> fieldInitializers;
        auto next_it = std::next(it);

        while (next_it != CS->body_end()) {
          const Stmt *nextStmt = *next_it;
          if (const auto *BO = dyn_cast<BinaryOperator>(nextStmt)) {
            if (isFieldInitializerForVar(BO, siteInfo.PointerVar)) {
              const auto *ME = cast<MemberExpr>(BO->getLHS()->IgnoreParenCasts());
              const auto *FD = cast<FieldDecl>(ME->getMemberDecl());
              if (fieldInitializers.count(FD)) {
                  llvm::outs() << "LOG: Found duplicate initialization for field '" << FD->getNameAsString() << "'. Stopping scan.\n";
                  break;
              }
              fieldInitializers[FD] = BO;
              initializers.push_back(BO);
            } else {
              break;
            }
          } else {
            break;
          }
          ++next_it;
        }

        llvm::outs() << "LOG: Found " << fieldInitializers.size()
                     << " explicit initializers.\n";

        std::string varName = siteInfo.PointerVar->getNameAsString();
        std::string typeName = siteInfo.StructureType.getAsString();

        std::stringstream replacement;
        replacement << "{\n";
        replacement << "    " << typeName << " __heap_init_obj;\n";

        std::set<const FieldDecl *> initializedFields;

        for(const auto& pair : fieldInitializers) {
            const FieldDecl* field = pair.first;
            const BinaryOperator* BO = pair.second;
            std::string rhsText = Normalizer.TheRewriter.getRewrittenText(BO->getRHS()->getSourceRange());
            replacement << "    __heap_init_obj." << field->getNameAsString() << " = " << rhsText << ";\n";
            initializedFields.insert(field);
        }

        for (const auto* field : siteInfo.StructDecl->fields()) {
            if (initializedFields.find(field) == initializedFields.end()) {
                if (field->getType()->isPointerType()) {
                    llvm::outs() << "LOG: Auto-initializing uninitialized pointer field '" << field->getNameAsString() << "' to NULL.\n";
                    replacement << "    __heap_init_obj." << field->getNameAsString() << " = NULL;\n";
                } else {
                    llvm::outs() << "LOG: Leaving non-pointer field '" << field->getNameAsString() << "' uninitialized.\n";
                }
            }
        }

        replacement << "    write(" << varName << ", __heap_init_obj);\n";
        replacement << "  }";

        if (const auto *DS = dyn_cast<DeclStmt>(siteInfo.MallocStmt)) {
            if(DS->isSingleDecl()){
                // FIX: Use dyn_cast to safely convert Decl* to VarDecl*
                if (const auto* VD = dyn_cast<VarDecl>(DS->getSingleDecl())) {
                    if (VD->hasInit()) {
                         Normalizer.TheRewriter.ReplaceText(VD->getInit()->getSourceRange(), "alloc()");
                    }
                }
            }
        } else if (const auto *BO = dyn_cast<BinaryOperator>(siteInfo.MallocStmt)) {
            Normalizer.TheRewriter.ReplaceText(BO->getRHS()->getSourceRange(), "alloc()");
        }

        Normalizer.hasMadeChanges = true;
        ConsumedStmts.insert(siteInfo.MallocStmt);

        if (!initializers.empty()) {
            SourceLocation startLoc = initializers.front()->getBeginLoc();
            SourceLocation endLoc = initializers.back()->getEndLoc();
            Normalizer.TheRewriter.ReplaceText(SourceRange(startLoc, endLoc), replacement.str());
            for (const auto *bo : initializers) {
                ConsumedStmts.insert(bo);
            }
        } else {
            SourceLocation endLoc = siteInfo.MallocStmt->getEndLoc();
            SourceLocation insertLoc = Lexer::getLocForEndOfToken(endLoc, 0, Normalizer.Context.getSourceManager(), Normalizer.Context.getLangOpts());
            Normalizer.TheRewriter.InsertTextAfter(insertLoc, ";\n" + replacement.str());
        }

        it = std::prev(next_it);
      }
      return true;
    }
  };

  std::string getSourceText(SourceRange range) const {
    return Lexer::getSourceText(CharSourceRange::getTokenRange(range),
                                TheRewriter.getSourceMgr(),
                                TheRewriter.getLangOpts())
        .str();
  }

  void analyzeHeapType(QualType deducedType) {
    if (deducedType.isNull() || deducedType->isIncompleteType())
      return;
    hasMadeChanges = true;
    std::string typeStr = deducedType.getCanonicalType().getAsString();
    if (heapType.empty()) {
      heapType = typeStr;
    } else if (heapType != typeStr) {
      std::string errorMsg =
          "ERROR: Normalization failed. Multiple different HEAP object types "
          "found: '" +
          heapType + "' and '" + typeStr + "'.";
      throw std::runtime_error(errorMsg);
    }
  }

  void removeStdlibInclude() {
    SourceManager &SM = TheRewriter.getSourceMgr();
    FileID mainFileID = SM.getMainFileID();
    StringRef fileBuffer = SM.getBufferData(mainFileID);
    const std::string includeStr = "#include <stdlib.h>";
    size_t includePos = fileBuffer.find(includeStr);
    if (includePos == StringRef::npos)
      return;
    SourceLocation loc =
        SM.getLocForStartOfFile(mainFileID).getLocWithOffset(includePos);
    unsigned len = includeStr.length();
    if (includePos + len < fileBuffer.size()) {
      if (fileBuffer.substr(includePos + len, 2) == "\r\n")
        len += 2;
      else if (fileBuffer[includePos + len] == '\n')
        len += 1;
    }
    TheRewriter.RemoveText(loc, len);
  }

  bool isDirectStackWrite(const BinaryOperator *BO,
                          const std::set<const VarDecl *> &stackDecls) {
    if (!BO->isAssignmentOp())
      return false;
    if (const auto *UO =
            dyn_cast<UnaryOperator>(BO->getLHS()->IgnoreParenCasts())) {
      if (UO->getOpcode() == UO_Deref) {
        if (const auto *DRE =
                dyn_cast<DeclRefExpr>(UO->getSubExpr()->IgnoreParenCasts())) {
          if (const auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
            return stackDecls.count(VD);
          }
        }
      }
    }
    return false;
  }

public:
  HeapAccessNormalizerImpl(Rewriter &R, ASTContext &Ctx)
      : TheRewriter(R), Context(Ctx) {}

  void normalize() {
    ParentMapper(ParentMap).TraverseDecl(Context.getTranslationUnitDecl());

    MallocInitializerHandler mallocHandler(*this);
    mallocHandler.TraverseDecl(Context.getTranslationUnitDecl());
    const auto &consumedStmts = mallocHandler.getConsumedStmts();

    StackVariableLocator stackLocator;
    stackLocator.TraverseDecl(Context.getTranslationUnitDecl());
    HeapAccessLocator heapLocator;
    heapLocator.TraverseDecl(Context.getTranslationUnitDecl());

    std::set<std::string> stackPointerTypes;
    std::map<const Stmt *, std::vector<const MemberExpr *>> accessesByStmt;
    for (const auto *ME : heapLocator.arrowExprs) {
      if (const Stmt *topStmt = getEnclosingTopLevelStmt(ME)) {
          if(consumedStmts.count(topStmt)) continue;
          accessesByStmt[topStmt].push_back(ME);
      }
    }

    for (const auto &pair : accessesByStmt) {
      const Stmt *stmt = pair.first;
      const std::vector<const MemberExpr *> &accesses = pair.second;

      if (const auto *BO = dyn_cast<BinaryOperator>(stmt)) {
        if (isDirectStackWrite(BO, stackLocator.stackDecls)) {
          const auto *UO =
              cast<UnaryOperator>(BO->getLHS()->IgnoreParenCasts());
          stackPointerTypes.insert(
              UO->getSubExpr()->getType().getCanonicalType().getAsString());
          continue;
        }
      }

      std::vector<const BinaryOperator *> writesInStmt;
      std::set<const MemberExpr *> writeLHSsInStmt;

      for (const auto *ME : accesses) {
        if (const auto *BO = dyn_cast<BinaryOperator>(ParentMap[ME])) {
          if (getEnclosingTopLevelStmt(BO) == stmt && BO->isAssignmentOp() &&
              BO->getLHS()->IgnoreParenImpCasts() == ME) {
            writesInStmt.push_back(BO);
            writeLHSsInStmt.insert(ME);
          }
        }
      }

      for (const auto *ME : accesses) {
        if (writeLHSsInStmt.count(ME))
          continue;

        analyzeHeapType(ME->getBase()->getType()->getPointeeType());
        std::string replacement =
            "read(" + getSourceText(ME->getBase()->getSourceRange()) + ")." +
            ME->getMemberDecl()->getNameAsString();
        TheRewriter.ReplaceText(ME->getSourceRange(), replacement);
      }

      for (const auto *BO : writesInStmt) {
        const auto *ME =
            cast<MemberExpr>(BO->getLHS()->IgnoreParenImpCasts());
        analyzeHeapType(ME->getBase()->getType()->getPointeeType());

        QualType type = ME->getBase()->getType()->getPointeeType();
        std::string ptr = getSourceText(ME->getBase()->getSourceRange());
        std::string field = ME->getMemberDecl()->getNameAsString();
        std::string rhs =
            TheRewriter.getRewrittenText(BO->getRHS()->getSourceRange());

        std::string temp = "__heap_rd_" + std::to_string(tempVarCounter++);
        std::string replacement = "{\n    " + type.getAsString() + " " + temp +
                                  " = read(" + ptr + ");\n" + "    " + temp +
                                  "." + field + " = " + rhs + ";\n" +
                                  "    write(" + ptr + ", " + temp + ");\n  }";

        TheRewriter.ReplaceText(BO->getSourceRange(), replacement);
      }
    }

    if (!heapType.empty() && stackPointerTypes.count(heapType)) {
      throw std::runtime_error("ERROR: Normalization failed. Ambiguous type '" +
                               heapType +
                               "' is used for both heap objects and stack "
                               "pointers.");
    }
    if (hasMadeChanges && heapType.empty()) {
        bool hasOtherHeapOps = false;
        for (const auto &pair : accessesByStmt) {
            if (!pair.second.empty()) {
                hasOtherHeapOps = true;
                break;
            }
        }
        if (hasOtherHeapOps) {
          throw std::runtime_error("ERROR: Normalization failed. "
                                   "Heap operations were found, but could not "
                                   "deduce a concrete heap type.");
        }
    }


    if (hasMadeChanges) {
      removeStdlibInclude();
      std::stringstream hint;
      hint << "/*\n";
      if (!heapType.empty()) {
        hint << "  HEAP_TYPE: " << heapType << "\n";
      }
      for (const auto &stackType : stackPointerTypes) {
        hint << "  STACK_PTR_TYPE: " << stackType << "\n";
      }
      hint << "*/\n\n";
      TheRewriter.InsertText(
          Context.getSourceManager().getLocForStartOfFile(
              Context.getSourceManager().getMainFileID()),
          hint.str(), true, true);
    }
  }
};

HeapAccessNormalizer::HeapAccessNormalizer(Rewriter &R, ASTContext &Ctx)
    : Impl(std::make_unique<HeapAccessNormalizerImpl>(R, Ctx)) {
  Impl->normalize();
}

HeapAccessNormalizer::~HeapAccessNormalizer() = default;
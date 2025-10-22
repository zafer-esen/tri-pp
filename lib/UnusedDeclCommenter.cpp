#include "UnusedDeclCommenter.hpp"
#include "Utilities.hpp"

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

extern cl::opt<bool> debug;

UnusedDeclCommenter::UnusedDeclCommenter(clang::Rewriter &r, clang::ASTContext &Ctx,
                      UsedFunAndTypeCollector &usedFunsAndTypes) {
    UnusedDeclCommenterASTConsumer c(r, usedFunsAndTypes);
    c.HandleTranslationUnit(Ctx);
}

UnusedDeclCommenterASTConsumer::UnusedDeclCommenterASTConsumer(clang::Rewriter &r,
                                     UsedFunAndTypeCollector &usedFunsAndTypes)
                           : rewriter(r) {
  handler = std::make_unique<UnusedDeclCommenterMatcher>(rewriter, usedFunsAndTypes);
  DeclarationMatcher usedDeclMatcher =
  anyOf(
    // comment out unused function decls
    functionDecl(unless(isImplicit())).bind("functionDecl"),
    // comment out unused record decls
    recordDecl().bind("recordDecl"),
    // comment out unused fields that are not implicit (e.g., anonymous unions)
    fieldDecl(unless(isImplicit())).bind("fieldDecl"),
    // comment out function pointer VarDecls
    varDecl(
        hasType(pointerType(pointee(functionProtoType()))),
        unless(parmVarDecl())
    ).bind("functionPointerVarDecl"),

    // comment out var decls using records,
    varDecl(
      unless(parmVarDecl()), // unless they are fun args
      anyOf(
        hasDescendant(qualType(hasCanonicalType(
          qualType(anyOf(recordType(), enumType())).bind("varBaseType")))),
        hasType(qualType(arrayType()).bind("varBaseType"))
      )
    ).bind("varDecl")
    // more?
  );

  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource,
                             usedDeclMatcher), handler.get());
}

void UnusedDeclCommenterMatcher::run(const MatchFinder::MatchResult &Result) {
  ASTContext *Ctx = Result.Context;
  const Decl *declToComment = nullptr;

  if (debug) {
    llvm::dbgs() << "--------------------------------------------------\n"
                 << "[Commenter] Matcher fired. Checking declaration...\n";
  }

  if (const auto *fd = Result.Nodes.getNodeAs<clang::FunctionDecl>("functionDecl")) {
    if (debug) {
      llvm::dbgs() << "[Commenter] Matched as 'functionDecl': " << fd->getNameAsString() << " at ";
      fd->getBeginLoc().print(llvm::dbgs(), *Result.SourceManager);
      llvm::dbgs() << "\n";
      llvm::dbgs() << "[Commenter] -> Is it seen? " << (usedFunsAndTypes.functionIsSeen(fd) ? "YES" : "NO") << "\n";
    }
    if (!usedFunsAndTypes.functionIsSeen(fd))
      declToComment = fd;
  }
  else if (const auto *rd = Result.Nodes.getNodeAs<clang::RecordDecl>("recordDecl")) {
    if (debug) {
      llvm::dbgs() << "[Commenter] Matched as 'recordDecl': " << rd->getNameAsString() << " at ";
      rd->getBeginLoc().print(llvm::dbgs(), *Result.SourceManager);
      llvm::dbgs() << "\n";
      bool isSeen = usedFunsAndTypes.typeIsSeen(Ctx->getTypeDeclType(rd).getTypePtr());
      llvm::dbgs() << "[Commenter] -> Is its type seen? " << (isSeen ? "YES" : "NO") << "\n";
    }
    if (!usedFunsAndTypes.typeIsSeen(Ctx->getTypeDeclType(rd).getTypePtr()))
      declToComment = rd;
  }
  else if (const auto *fd = Result.Nodes.getNodeAs<clang::FieldDecl>("fieldDecl")) {
    if (debug) {
      llvm::dbgs() << "[Commenter] Matched as 'fieldDecl': "
                   << fd->getParent()->getNameAsString() << "::" << fd->getNameAsString() << " at ";
      fd->getBeginLoc().print(llvm::dbgs(), *Result.SourceManager);
      llvm::dbgs() << "\n";
      bool isSeen = usedFunsAndTypes.fieldIsSeen(fd);
      llvm::dbgs() << "[Commenter] -> Is this field ever accessed? " << (isSeen ? "YES" : "NO") << "\n";
    }
    if (!usedFunsAndTypes.fieldIsSeen(fd)) {
      declToComment = fd;
    }
  }
  else if (const auto *vd = Result.Nodes.getNodeAs<clang::VarDecl>("functionPointerVarDecl")) {
    if (debug) {
      llvm::dbgs() << "[Commenter] Matched as 'functionPointerVarDecl': " << vd->getNameAsString() << " at ";
      vd->getBeginLoc().print(llvm::dbgs(), *Result.SourceManager);
      llvm::dbgs() << "\n";
      bool isSeen = usedFunsAndTypes.functionPointerIsSeen(vd);
      llvm::dbgs() << "[Commenter] -> Is this function pointer variable seen? " << (isSeen ? "YES" : "NO") << "\n";
    }
    if (!usedFunsAndTypes.functionPointerIsSeen(vd)) {
      declToComment = vd;
    }
  }
  else if (const auto *vd = Result.Nodes.getNodeAs<clang::VarDecl>("varDecl")) {
    if (debug) {
      llvm::dbgs() << "[Commenter] Matched as generic 'varDecl': " << vd->getNameAsString() << " at ";
      vd->getBeginLoc().print(llvm::dbgs(), *Result.SourceManager);
      llvm::dbgs() << "\n";
      const auto *varBaseType = Result.Nodes.getNodeAs<clang::QualType>("varBaseType");
      bool isSeen = usedFunsAndTypes.typeIsSeen(varBaseType);
      llvm::dbgs() << "[Commenter] -> Does it use a seen type? " << (isSeen ? "YES" : "NO") << "\n";
      llvm::dbgs() << "[Commenter] -> (Base type that was matched: " << varBaseType->getAsString() << ")\n";
    }
    const auto *varBaseType = Result.Nodes.getNodeAs<clang::QualType>("varBaseType");
    if (!usedFunsAndTypes.typeIsSeen(varBaseType))
      declToComment = vd;
  }
  else {
    llvm_unreachable("UnusedDeclCommenter unreachable case\n");
  }

  if (declToComment) {
    if (debug) {
      llvm::dbgs() << "[Commenter] >>> Decision: COMMENT OUT this declaration.\n";
    }
    if (editedLocations.insert(declToComment->getBeginLoc()).second) {
      doubleSlashCommentOutDeclaration(declToComment, *Ctx, rewriter);
    }
  } else {
    if (debug) {
      llvm::dbgs() << "[Commenter] >>> Decision: KEEP this declaration.\n";
    }
  }
}

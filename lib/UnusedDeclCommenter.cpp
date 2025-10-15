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

  if (const auto *fd = Result.Nodes.getNodeAs<clang::FunctionDecl>("functionDecl")) {
    if (!usedFunsAndTypes.functionIsSeen(fd))
      declToComment = fd;
  }
  else if (const auto *rd = Result.Nodes.getNodeAs<clang::RecordDecl>("recordDecl")) {
    if (!usedFunsAndTypes.typeIsSeen(
        Ctx->getTypeDeclType(rd).getTypePtr()))
      declToComment = rd;
  }
  else if (const auto *vd = Result.Nodes.getNodeAs<clang::VarDecl>("varDecl")) {
    const auto *varBaseType = Result.Nodes.getNodeAs<clang::QualType>("varBaseType");
    if (!usedFunsAndTypes.typeIsSeen(varBaseType))
      declToComment = vd;
  }
  else {
    llvm_unreachable("UnusedDeclCommenter unreachable case\n");
  }

  if (declToComment && editedLocations.insert(declToComment->getBeginLoc()).second) {
    doubleSlashCommentOutDeclaration(declToComment, *Ctx, rewriter);
  }
}

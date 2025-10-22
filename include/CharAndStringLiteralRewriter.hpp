// Rewrites char initializers as integer values and merges string literals (e.g., f("aa" "bb") -> f("aabb"))

#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "UsedFunctionAndTypeCollector.hpp"
#include "Utilities.hpp"

class CharAndStringLiteralRewriterMatcher :
    public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  CharAndStringLiteralRewriterMatcher(clang::Rewriter &r,
                                      UsedFunAndTypeCollector &usedFunsAndTypes)
      : rewriter(r), usedFunsAndTypes(usedFunsAndTypes) {}
  // this callback executes on a match
  void run(const clang::ast_matchers::MatchFinder::MatchResult &) override;

  // this callback executes at the end of the translation unit
  void onEndOfTranslationUnit() override{};

private:
  clang::Rewriter &rewriter;
  UsedFunAndTypeCollector &usedFunsAndTypes;
  static std::string escapeCString(llvm::StringRef str);
};

class CharAndStringLiteralRewriterASTConsumer : public clang::ASTConsumer {
public:
  CharAndStringLiteralRewriterASTConsumer(clang::Rewriter &r,
                                          UsedFunAndTypeCollector &usedFunsAndTypes);
  void HandleTranslationUnit(clang::ASTContext &Ctx) override {
    finder.matchAST(Ctx);
  }
private:
  clang::ast_matchers::MatchFinder finder;
  std::unique_ptr<CharAndStringLiteralRewriterMatcher> handler;
  clang::Rewriter &rewriter;
};

class CharAndStringLiteralRewriter {
public:
  CharAndStringLiteralRewriter(clang::Rewriter &r, clang::ASTContext &Ctx,
                               UsedFunAndTypeCollector &usedFunsAndTypes);
};

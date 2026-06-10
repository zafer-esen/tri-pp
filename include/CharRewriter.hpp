// Rewriter char initializers as integer values

#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "TrackedRewriter.hpp"
#include "UsedFunctionAndTypeCollector.hpp"
#include "Utilities.hpp"

class CharRewriterMatcher : 
  public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  CharRewriterMatcher(TrackedRewriter &r, 
                       UsedFunAndTypeCollector &usedFunsAndTypes) 
                       : rewriter(r), usedFunsAndTypes(usedFunsAndTypes) {}
  // this callback executes on a match
  void run(const clang::ast_matchers::MatchFinder::MatchResult &) override;
  
  // this callback executes at the end of the translation unit
  void onEndOfTranslationUnit() override{};

  private:
  TrackedRewriter &rewriter;
  UsedFunAndTypeCollector &usedFunsAndTypes;
};

class CharRewriterASTConsumer : public clang::ASTConsumer {
public:
  CharRewriterASTConsumer(TrackedRewriter &r, 
                           UsedFunAndTypeCollector &usedFunsAndTypes);
  void HandleTranslationUnit(clang::ASTContext &Ctx) override {
    finder.matchAST(Ctx);
  }
private:
  clang::ast_matchers::MatchFinder finder;
  std::unique_ptr<CharRewriterMatcher> handler;
  TrackedRewriter &rewriter;
};

class CharRewriter {
public:
  CharRewriter(TrackedRewriter &r, clang::ASTContext &Ctx, 
               UsedFunAndTypeCollector &usedFunsAndTypes);
};

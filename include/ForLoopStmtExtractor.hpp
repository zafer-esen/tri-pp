// Extract the declaration statements from for loops to just outside of them

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

class ForLoopStmtExtractorMatcher : 
  public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  ForLoopStmtExtractorMatcher(TrackedRewriter &r, 
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

class ForLoopStmtExtractorASTConsumer : public clang::ASTConsumer {
public:
  ForLoopStmtExtractorASTConsumer(TrackedRewriter &r, 
                           UsedFunAndTypeCollector &usedFunsAndTypes);
  void HandleTranslationUnit(clang::ASTContext &Ctx) override {
    finder.matchAST(Ctx);
  }
private:
  clang::ast_matchers::MatchFinder finder;
  TrackedRewriter &rewriter;
  std::unique_ptr<ForLoopStmtExtractorMatcher> handler;
};

class ForLoopStmtExtractor {
public:
  ForLoopStmtExtractor(TrackedRewriter &r, clang::ASTContext &Ctx, 
                       UsedFunAndTypeCollector &usedFunsAndTypes);
};

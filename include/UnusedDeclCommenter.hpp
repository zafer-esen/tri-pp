// Remove all unused declarations from source

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

class UnusedDeclCommenterMatcher : 
  public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  UnusedDeclCommenterMatcher(TrackedRewriter &r, 
                       UsedFunAndTypeCollector &usedFunsAndTypes) 
                       : rewriter(r), usedFunsAndTypes(usedFunsAndTypes) {}
  // this callback executes on a match
  void run(const clang::ast_matchers::MatchFinder::MatchResult &) override;
  
  // this callback executes at the end of the translation unit
  void onEndOfTranslationUnit() override{};

  private:
  TrackedRewriter &rewriter;
  //llvm::SmallSet<const clang::Decl*, 32> commentedDecls;
  // or
  //llvm::SmallSet<const clang::Decl*, 32> commentedLines;
  UsedFunAndTypeCollector &usedFunsAndTypes;
};

class UnusedDeclCommenterASTConsumer : public clang::ASTConsumer {
public:
  UnusedDeclCommenterASTConsumer(TrackedRewriter &r, 
                           UsedFunAndTypeCollector &usedFunsAndTypes);
  void HandleTranslationUnit(clang::ASTContext &Ctx) override {
    finder.matchAST(Ctx);
  }
private:
  clang::ast_matchers::MatchFinder finder;
  TrackedRewriter &rewriter;
  std::unique_ptr<UnusedDeclCommenterMatcher> handler;
};

// collects all seen functions and types on construction
class UnusedDeclCommenter {
public:
  UnusedDeclCommenter(TrackedRewriter &r, clang::ASTContext &Ctx, 
                      UsedFunAndTypeCollector &usedFunsAndTypes);
};

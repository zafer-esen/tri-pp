// Eliminates all designated initializers by replacing them with positional
// initializers. Clang's semantic analysis provides the fully-constructed
// initializer list with all members in the correct order, which is used
// to generate the positional equivalent.
// e.g. struct Point { int x; int y; };
//      "struct Point p = {.y = 1, .x = 0};" is rewritten to
//      "struct Point p = { 0, 1 };"

#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/ADT/SmallSet.h"

class DesignatedInitializerEliminatorMatcher :
  public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  explicit DesignatedInitializerEliminatorMatcher(clang::Rewriter &r);
  // this callback executes on a match
  void run(const clang::ast_matchers::MatchFinder::MatchResult &) override;

private:
  clang::Rewriter &rewriter;
  llvm::SmallSet<clang::SourceLocation, 32> editedLocations;
};

class DesignatedInitializerEliminatorASTConsumer : public clang::ASTConsumer {
public:
  explicit DesignatedInitializerEliminatorASTConsumer(clang::Rewriter &r);
  void HandleTranslationUnit(clang::ASTContext &Ctx) override;
private:
  clang::ast_matchers::MatchFinder finder;
  std::unique_ptr<DesignatedInitializerEliminatorMatcher> handler;
};

class DesignatedInitializerEliminator {
public:
  DesignatedInitializerEliminator(clang::Rewriter &r, clang::ASTContext &Ctx);
};

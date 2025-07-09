#include "NondetLoopGuardRewriter.hpp"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/raw_ostream.h"

#include <string>
#include <vector>
#include <algorithm>

using namespace clang;

NondetLoopGuardRewriter::NondetLoopGuardRewriter(Rewriter &R, ASTContext &Ctx)
    : rewriter(R), context(Ctx), tempLoopIdxCounter(0) {

  TraverseDecl(Ctx.getTranslationUnitDecl());

  SourceManager &sm = context.getSourceManager();
  std::sort(loopsToRewrite.begin(), loopsToRewrite.end(),
            [&sm](const WhileStmt *A, const WhileStmt *B) {
              return sm.isBeforeInTranslationUnit(B->getBeginLoc(), A->getBeginLoc());
            });

  for (WhileStmt *S : loopsToRewrite) {
    rewriteWhileStmt(S);
  }
}

bool NondetLoopGuardRewriter::VisitWhileStmt(WhileStmt *S) {
  const Expr *cond = S->getCond()->IgnoreParenImpCasts();
  if (const auto *callExpr = dyn_cast<CallExpr>(cond)) {
    const FunctionDecl *funcDecl = callExpr->getDirectCallee();
    if (funcDecl && funcDecl->getStorageClass() == SC_Extern && !funcDecl->getDefinition()) {
      loopsToRewrite.push_back(S);
    }
  }
  return true;
}

void NondetLoopGuardRewriter::rewriteWhileStmt(WhileStmt *S) {
  SourceManager &sm = context.getSourceManager();
  const LangOptions &langOpts = context.getLangOpts();

  // Get Indentation of the original `while` line
  SourceLocation stmtBeginLoc = S->getBeginLoc();
  unsigned startCol = sm.getSpellingColumnNumber(stmtBeginLoc);
  SourceLocation lineBeginLoc = stmtBeginLoc.getLocWithOffset(-(startCol - 1));
  StringRef indentStr = Lexer::getSourceText(
      CharSourceRange::getCharRange(lineBeginLoc, stmtBeginLoc),
      sm, langOpts);

  const auto *callExpr = cast<CallExpr>(S->getCond()->IgnoreParenImpCasts());
  std::string tempVarName = "__loop_idx_" + std::to_string(tempLoopIdxCounter++);
  std::string callText = Lexer::getSourceText(
      CharSourceRange::getTokenRange(callExpr->getSourceRange()),
      sm, langOpts).str();

  std::string suffix = "\n" + indentStr.str() + "}";
  SourceLocation endLoc = Lexer::getLocForEndOfToken(S->getEndLoc(), 0, sm, langOpts);
  rewriter.InsertTextAfter(endLoc, suffix);
  rewriter.ReplaceText(S->getCond()->getSourceRange(), tempVarName + "-->0");

  std::string prefix;
  llvm::raw_string_ostream ss(prefix);
  ss << "{\n";
  ss << indentStr << "  int " << tempVarName << " = " << callText << ";\n";
  ss << indentStr << "  ";
  rewriter.InsertTextBefore(S->getBeginLoc(), ss.str());
}
#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include <vector>

namespace clang {
    class ASTContext;
    class WhileStmt;
}

class NondetLoopGuardRewriter :
  public clang::RecursiveASTVisitor<NondetLoopGuardRewriter> {
public:
    explicit NondetLoopGuardRewriter(clang::Rewriter &R, clang::ASTContext &Ctx);

    bool VisitWhileStmt(clang::WhileStmt *S);

private:
    void rewriteWhileStmt(clang::WhileStmt *S);

    clang::Rewriter &rewriter;
    clang::ASTContext &context;
    int tempLoopIdxCounter;
    std::vector<clang::WhileStmt *> loopsToRewrite;
};
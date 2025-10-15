#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/ADT/SmallVector.h"

#include <map>

namespace clang {
    class ASTContext;
    class VarDecl;
    class FunctionDecl;
}

class FuncPtrDevirtualizer {
public:
    FuncPtrDevirtualizer(clang::Rewriter &R, clang::ASTContext &Ctx);

private:
    class AssignmentCollector;
    class DevirtualizeMatcher;

    clang::Rewriter &rewriter;
    clang::ASTContext &context;

    std::map<const clang::VarDecl *, llvm::SmallVector<const clang::FunctionDecl *, 2>> assignments;
    std::map<const clang::VarDecl *, const clang::FunctionDecl *> devirtualizedTargets;
};

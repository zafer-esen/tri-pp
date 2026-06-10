#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "TrackedRewriter.hpp"
#include "ExecutionCountAnalyzer.hpp"
#include <string>
#include <vector>

class DeterminizerVisitor;

struct UnboundedInputInfo {
    std::string globalArrayName;
    std::string localArrayName;
    std::string indexName;
};

class Determinizer {
public:
    Determinizer(TrackedRewriter &R, clang::ASTContext &Ctx,
                 const ExecutionCountAnalyzer &Analyzer);

    // inputs that were added to the program by this pass
    std::vector<std::string> addedInputVariables() const;
    std::vector<std::string> addedInputArrays() const;

private:
    friend class DeterminizerVisitor;

    void run();

    TrackedRewriter &TheRewriter;
    clang::ASTContext &Context;
    const ExecutionCountAnalyzer &ExecAnalyzer;

    const clang::FunctionDecl* mainFunc = nullptr;
    int inputCounter = 1;

    std::vector<std::string> headerInputNames;
    std::string globalDeclarations;
    std::string mainInitializations;
    std::map<std::string, UnboundedInputInfo> typeToArrayInfoMap;
};

class DeterminizerVisitor : public clang::RecursiveASTVisitor<DeterminizerVisitor> {
public:
    explicit DeterminizerVisitor(Determinizer &D, clang::ASTContext &Ctx);
    bool VisitFunctionDecl(clang::FunctionDecl *F);
    bool VisitCallExpr(clang::CallExpr *CE);

private:
    bool isNondetCall(const clang::CallExpr* call);

    Determinizer &D;
    clang::ASTContext &Context;
};

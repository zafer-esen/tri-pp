#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "ExecutionCountAnalyzer.hpp" // Include the new analyzer
#include <string>
#include <vector>
#include <map>

class DeterminizerVisitor;

struct UnboundedInputInfo {
    std::string globalArrayName;
    std::string localArrayName;
    std::string indexName;
};

class Determinizer {
public:
    Determinizer(clang::Rewriter &R, clang::ASTContext &Ctx,
                 const ExecutionCountAnalyzer &Analyzer);

private:
    friend class DeterminizerVisitor;

    void run();

    clang::Rewriter &TheRewriter;
    clang::ASTContext &Context;
    const ExecutionCountAnalyzer &ExecAnalyzer; // Store a reference to the analysis results

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
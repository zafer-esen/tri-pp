#pragma once

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Stmt.h"
#include "llvm/ADT/DenseMap.h"
#include "UsedFunctionAndTypeCollector.hpp"


#include <vector>

class StmtFrequencyVisitor;

// The abstract domain for execution frequency.
enum class ExecutionFrequency {
    ONCE,
    MANY
  };

inline ExecutionFrequency combine(ExecutionFrequency a, ExecutionFrequency b) {
    return (a == ExecutionFrequency::MANY || b == ExecutionFrequency::MANY)
               ? ExecutionFrequency::MANY
               : ExecutionFrequency::ONCE;
}

class ExecutionCountAnalyzer {
public:
    ExecutionCountAnalyzer(clang::ASTContext &Ctx,
                           const UsedFunAndTypeCollector& usedFunsAndTypes,
                           const std::string& entryFunction);

    ExecutionFrequency getFrequency(const clang::Stmt *S) const;
    void printFrequencies(llvm::raw_ostream& OS) const;

private:
    friend class StmtFrequencyVisitor;
    class FrequencyPrinterVisitor;

    clang::ASTContext &Ctx;
    const UsedFunAndTypeCollector& usedFunsAndTypes;

    llvm::DenseMap<const clang::Stmt *, ExecutionFrequency> StmtFrequencies;
    llvm::DenseMap<const clang::FunctionDecl *, ExecutionFrequency> FunctionFrequencies;

    std::vector<const clang::FunctionDecl *> Worklist;

    void run();
    void analyzeFunction(const clang::FunctionDecl *FD);
};
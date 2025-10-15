#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/ADT/DenseMap.h"
#include "UsedFunctionAndTypeCollector.hpp"

#include <map>
#include <string>

class UniqueCallTransformer {
public:
  UniqueCallTransformer(clang::Rewriter &R, clang::ASTContext &Ctx,
                                    const UsedFunAndTypeCollector &U);

  void transform();

private:
  class CallExprVisitor : public clang::RecursiveASTVisitor<CallExprVisitor> {
  public:
    explicit CallExprVisitor(std::vector<const clang::CallExpr*> &calls) : calls(calls) {}
    bool VisitCallExpr(const clang::CallExpr *CE) {
      if (CE->getDirectCallee()) {
        calls.push_back(CE);
      }
      return true;
    }
  private:
    std::vector<const clang::CallExpr*> &calls;
  };

  // Given an original function, it creates a
  // unique clone, including clones of all its callees. It returns the new
  // name of the cloned function.
  std::string getOrCreateClone(const clang::FunctionDecl *FD);

  clang::Rewriter &rewriter;
  clang::ASTContext &Ctx;
  const UsedFunAndTypeCollector &usedFunsAndTypes;

  llvm::DenseMap<const clang::FunctionDecl *, std::vector<const clang::CallExpr *>> allCallSites;
  llvm::DenseMap<const clang::FunctionDecl *, unsigned> globalCloneCounters;

  // Memoization table for the recursive cloning process. It maps an original
  // function to the name of a clone we have already generated.
  struct PtrCompare {
    bool operator()(const clang::FunctionDecl* a, const clang::FunctionDecl* b) const {
      return a < b;
    }
  };
  std::map<const clang::FunctionDecl*, std::string, PtrCompare> cloneCache;
};
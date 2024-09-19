// Replaces all (used) types appearing in the AST with their canonical versions.
// A type visitor (TypeCanoniser) is used for this purpose, which tries to
// extract the canonical type from a given type.
// e.g. typedef struct A* B; 
//      "B* x" is canonised to "struct A** x"

#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "UsedFunctionAndTypeCollector.hpp"

class TypeCanoniserMatcher : 
  public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  TypeCanoniserMatcher(clang::Rewriter &r, 
                  UsedFunAndTypeCollector &usedFunsAndTypes)
                  : rewriter(r), usedFunsAndTypes(usedFunsAndTypes) {}
  // this callback executes on a match
  void run(const clang::ast_matchers::MatchFinder::MatchResult &) override;
  
  // this callback executes at the end of the translation unit
  void onEndOfTranslationUnit() override{};

private:
  clang::Rewriter &rewriter;
  llvm::SmallSet<clang::SourceLocation, 32> editedLocations;
  UsedFunAndTypeCollector &usedFunsAndTypes;
};

class TypeCanoniserASTConsumer : public clang::ASTConsumer {
public:
  TypeCanoniserASTConsumer(clang::Rewriter &r, 
                           UsedFunAndTypeCollector &usedFunsAndTypes);
  void HandleTranslationUnit(clang::ASTContext &Ctx) override;
private:
  clang::ast_matchers::MatchFinder finder;
  clang::Rewriter &rewriter;
  std::unique_ptr<TypeCanoniserMatcher> handler;
};

// collects all seen functions and types on construction
class TypeCanoniser {
  public:
  TypeCanoniser(clang::Rewriter &r, clang::ASTContext &Ctx, 
                UsedFunAndTypeCollector &usedFunsAndTypes) {
    TypeCanoniserASTConsumer c(r, usedFunsAndTypes);
    c.HandleTranslationUnit(Ctx);
  }
};

// AST Visitors
/**
 * \brief A visitor for extracting type information from the Abstract Syntax Tree (AST).
 *
 * Clang does not provide built-in support for collecting type information, so this visitor
 * traverses the AST in a bottom-up manner and extracts type information as needed.
 */
class TypeCanoniserVisitor
: public clang::RecursiveASTVisitor<TypeCanoniserVisitor> {
public:
  explicit TypeCanoniserVisitor(clang::ASTContext &Ctx) : Ctx(Ctx) {
    unqualType = clang::QualType();
  }

    bool shouldTraversePostOrder() const { return true; }
    bool VisitType(clang::Type *typ);
    bool VisitConstantArrayType(clang::ConstantArrayType *typ);
    clang::QualType getUnqualType() { return unqualType; }

    int getNumPointers() const {
      return numPointers;
    }
    std::string getUnqualifiedTypeName() const {
      return (unqualTypeName + std::string(numPointers, '*')); 
    }
    std::string getUnqualifiedTypeNameWithoutPtrs() const {
      return unqualTypeName; 
    }
    std::string getOnlyPtrs() const {
      return std::string(numPointers, '*');
    }
    bool isFunctionType() const { return isFunctionPrototype; }
    bool convertedArrayToPtr() const { return _convertedArrayToPtr; }
    std::string getFunctionPtrTypeFullName(std::string funName) const {
      return (functionReturnTypeName + 
        " (" + std::string(numPointers, '*') + funName + ") " +
        unqualTypeName); 
    }
  
  private:
    clang::ASTContext &Ctx;
    clang::QualType unqualType;
    std::string unqualTypeName = ""; // set by VisitType after traversal
    std::string functionReturnTypeName = "";
    int numPointers = 0;
    bool isFunctionPrototype = false;
    bool _convertedArrayToPtr = false; // true when converted T x[1] to T* x
};

/**
 * \brief A visitor class for counting the number of pointer dereferences in an expression.
 *
 * This class derives from clang::RecursiveASTVisitor to traverse the AST of an expression
 * and count the number of unary operator expressions that represent pointer dereferences.
 */
class DereferenceCounterVisitor
: public clang::RecursiveASTVisitor<DereferenceCounterVisitor> {
public:
  explicit DereferenceCounterVisitor(clang::ASTContext &Ctx) : Ctx(Ctx) {
    dereferenceCount = 0;
  }
  
  bool shouldTraversePostOrder() const { return true; }
  bool VisitExpr(clang::Expr *expr) {
        // Check if the expression is a unary operator with opcode UO_Deref
    if (clang::UnaryOperator *UO = llvm::dyn_cast<clang::UnaryOperator>(expr)) {
      if (UO->getOpcode() == clang::UO_Deref) {
        dereferenceCount++;
      }
    }
    return true; // continue traversal
  }
  int getDereferenceCount() const {
    return dereferenceCount;
  }
  
  private:
    clang::ASTContext &Ctx;
    int dereferenceCount;
};
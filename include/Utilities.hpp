#pragma once

#include "clang/Rewrite/Core/Rewriter.h"
#include "TrackedRewriter.hpp"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

// removes the declaration by blanking it out, together with a trailing
// semicolon and a preceding stand-alone __extension__ line.
// returns false if the declaration is not in the main file.
bool blankOutDeclaration (const clang::Decl* declaration,
                          clang::ASTContext &ctx,
                          TrackedRewriter& rewriter);

// This visitor is used to recursively traverse types
// and extract the underlying type free of pointers and qualifiers
class BaseTypeExtractor
: public clang::RecursiveASTVisitor<BaseTypeExtractor> {
public:
  explicit BaseTypeExtractor(clang::ASTContext &Ctx) : Ctx(Ctx) {}
  bool VisitType(clang::Type *typ);
  // more concrete matchers can be added here if required
  // bool VisitRecordType(clang::RecordType *typ);
  // bool VisitPointerType(clang::PointerType *typ);
  clang::Type* getExtractedType () const { return extractedType; }
private:
  clang::ASTContext &Ctx;
  clang::Type* extractedType;
};


// returns the previous declaration in the same statement if there exists one,
// returns nullptr otherwise.
// e.g. returns "int a" in "int a, b" if called with "int b"
const clang::Decl* getPrevDeclInSameStmt(const clang::Decl* decl);

// returns the next declaration in the same statement if there exists one,
// returns nullptr otherwise. the returned decl always have the same type
// as the passed decl, e.g. if TypedefDecl* decl, then a TypedefDecl* will
// be returned.
// e.g. returns "int b" in "int a, b" if called with "int a"
const clang::Decl* getNextDeclInSameStmt(const clang::Decl* decl);

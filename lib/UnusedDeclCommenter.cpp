#include "UnusedDeclCommenter.hpp"
#include "Utilities.hpp"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "llvm/Support/raw_ostream.h"


using namespace clang;
using namespace ast_matchers;
using namespace llvm;

UnusedDeclCommenter::UnusedDeclCommenter(TrackedRewriter &r, clang::ASTContext &Ctx, 
                      UsedFunAndTypeCollector &usedFunsAndTypes) {
    UnusedDeclCommenterASTConsumer c(r, usedFunsAndTypes);
    c.HandleTranslationUnit(Ctx);
}

UnusedDeclCommenterASTConsumer::UnusedDeclCommenterASTConsumer(TrackedRewriter &r,
                                     UsedFunAndTypeCollector &usedFunsAndTypes)
                           : rewriter(r) {
  handler = std::make_unique<UnusedDeclCommenterMatcher>(rewriter, usedFunsAndTypes);
  DeclarationMatcher usedDeclMatcher = 
  anyOf(
    // comment out unused function decls
    functionDecl(unless(isImplicit())).bind("functionDecl"), 
    // remove unused record decls; those inside a typedef belong to the
    // TypedefRemover
    recordDecl(unless(hasAncestor(typedefDecl()))).bind("recordDecl"),
    // remove unused enum decls
    enumDecl(unless(hasAncestor(typedefDecl()))).bind("enumDecl"),
    // comment out var decls using records,
    varDecl(
      unless(parmVarDecl()), // unless they are fun args
      anyOf(
        hasDescendant(qualType(hasCanonicalType(
          qualType(anyOf(recordType(), enumType())).bind("varBaseType")))),
        hasType(qualType(arrayType()).bind("varBaseType"))
      )
    ).bind("varDecl")
    // more?
  );
    
  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, 
                             usedDeclMatcher), handler.get());
}

void UnusedDeclCommenterMatcher::run(const MatchFinder::MatchResult &Result) {
  ASTContext *Ctx = Result.Context;

  const RecordDecl * recordDecl = 
    Result.Nodes.getNodeAs<clang::RecordDecl>("recordDecl"); 

  const FunctionDecl * functionDecl =
    Result.Nodes.getNodeAs<clang::FunctionDecl>("functionDecl"); 
  
  const VarDecl * varDecl =
    Result.Nodes.getNodeAs<clang::VarDecl>("varDecl"); 

  const EnumDecl * enumDecl =
    Result.Nodes.getNodeAs<clang::EnumDecl>("enumDecl");

  if (functionDecl) 
  { // remove unused function declarations
    if (!usedFunsAndTypes.functionIsSeen(functionDecl))
      blankOutDeclaration(functionDecl, *Ctx, rewriter);
  } 
  else if (recordDecl) 
  { // remove unused record declarations
    if (!usedFunsAndTypes.typeIsSeen(
        Ctx->getTypeDeclType(recordDecl).getTypePtr()))
      blankOutDeclaration(recordDecl, *Ctx, rewriter);
  }
  else if (enumDecl)
  { // remove unused enum declarations
    if (!usedFunsAndTypes.typeIsSeen(
        Ctx->getTypeDeclType(enumDecl).getTypePtr()))
      blankOutDeclaration(enumDecl, *Ctx, rewriter);
  }
  else if (varDecl) 
  { // remove unused var declarations
    // note that this might blank out some record declarations
    // several times if the vars are declared as part of the
    // record declaration: e.g. struct s {...} s1;
    const QualType * varBaseType =
      Result.Nodes.getNodeAs<clang::QualType>("varBaseType"); 
    if (!usedFunsAndTypes.typeIsSeen(varBaseType))
      blankOutDeclaration(varDecl, *Ctx, rewriter);
  }
  else {
    llvm_unreachable("UnusedDeclCommenter unreachable case\n");
  }
}

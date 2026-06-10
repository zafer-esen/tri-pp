// Translates C++ to C+, the C-like subset TriCera parses: monomorphises
// template instantiations, replaces their uses with mangled names, makes
// implicit member accesses explicit (this->), qualifies non-virtual member
// calls and casts throw operands. The mangled name / original name pairs
// are recorded in the facts sidecar for back-translation.

#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "TrackedRewriter.hpp"
#include "FactsCollector.hpp"

#include <map>
#include <set>
#include <string>
#include <vector>

class CXXToCPlusTranslator : public clang::RecursiveASTVisitor<CXXToCPlusTranslator> {
public:
  explicit CXXToCPlusTranslator(clang::ASTContext &Context,
                            TrackedRewriter &Rewriter,
                            std::vector<MangledName> &MangledNamesOut)
      : Context(Context), Rewriter(Rewriter),
        MangledNamesOut(MangledNamesOut) {}

  bool VisitClassTemplateDecl(clang::ClassTemplateDecl *D);
  bool VisitClassTemplateSpecializationDecl(clang::ClassTemplateSpecializationDecl *D);
  bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl *D);
  bool VisitFunctionDecl(clang::FunctionDecl *D);
  bool VisitCallExpr(clang::CallExpr *E);
  bool VisitTypeLoc(clang::TypeLoc TL);
  bool VisitFieldDecl(clang::FieldDecl *D);
  bool VisitMemberExpr(clang::MemberExpr *ME);
  bool VisitCXXThrowExpr(clang::CXXThrowExpr *E);
  bool VisitCXXMemberCallExpr(clang::CXXMemberCallExpr *CE);

  bool shouldVisitTemplateInstantiations() const;
  bool shouldVisitImplicitCode() const;

private:
  std::string mangleTemplateName(const clang::ClassTemplateSpecializationDecl *D);
  std::string mangleFunctionName(const clang::FunctionDecl *D);
  // records the pair once, keyed by mangled name
  void recordMangledName(const std::string &mangled, const std::string &original);
  // true if `loc` is inside a template definition we remove; such text and
  // everything in it is gone, so it must not be rewritten
  bool insideRemovedTemplate(clang::SourceLocation loc) const;

  clang::ASTContext &Context;
  TrackedRewriter &Rewriter;
  std::vector<MangledName> &MangledNamesOut;
  // source ranges of the template definitions removed by this pass
  std::vector<clang::SourceRange> RemovedTemplates;
  std::map<const clang::ClassTemplateSpecializationDecl*, std::string> MangledNames;
  std::map<const clang::FunctionDecl*, std::string> FunctionMangledNames;
  std::set<const clang::Decl*> VisitedDecls;
  std::set<const clang::MemberExpr*> VisitedMemberExprs;
};

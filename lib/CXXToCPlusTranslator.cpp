#include "CXXToCPlusTranslator.hpp"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceManager.h"
#include <regex>
#include <algorithm>
#include <cctype>

using namespace clang;
using namespace clang::ast_matchers;

// flattens a printed name to an identifier: template arguments and
// qualifiers become underscores, other non-alphanumerics are dropped,
// trailing underscore runs collapse
static std::string flattenName(std::string name) {
  std::replace(name.begin(), name.end(), '<', '_');
  std::replace(name.begin(), name.end(), '>', '_');
  std::replace(name.begin(), name.end(), ',', '_');
  std::replace(name.begin(), name.end(), ' ', '_');
  std::replace(name.begin(), name.end(), ':', '_');
  name.erase(std::remove_if(name.begin(), name.end(), [](char c) {
    return !std::isalnum((unsigned char)c) && c != '_';
  }), name.end());
  while (name.length() > 1 && name[name.length()-1] == '_' &&
         name[name.length()-2] == '_')
    name.pop_back();
  return name;
}

// quotes regex metacharacters, so names like operator() stay literal
static std::string escapeForRegex(const std::string &name) {
  std::string out;
  for (size_t i = 0; i < name.size(); ++i) {
    if (!std::isalnum((unsigned char)name[i]) && name[i] != '_')
      out += '\\';
    out += name[i];
  }
  return out;
}

// the original line a synthetic insertion is attributed to: the line of
// `loc` in the main file, or 0
static unsigned originLineOf(SourceManager &SM, SourceLocation loc) {
  if (loc.isInvalid())
    return 0;
  std::pair<FileID, unsigned> d = SM.getDecomposedLoc(loc);
  if (d.first != SM.getMainFileID())
    return 0;
  return SM.getLineNumber(d.first, d.second);
}

void CXXToCPlusTranslator::recordMangledName(const std::string &mangled,
                                         const std::string &original) {
  for (size_t i = 0; i < MangledNamesOut.size(); ++i)
    if (MangledNamesOut[i].mangled == mangled)
      return;
  MangledName m;
  m.mangled = mangled;
  m.original = original;
  MangledNamesOut.push_back(m);
}

bool CXXToCPlusTranslator::insideRemovedTemplate(SourceLocation loc) const {
  if (loc.isInvalid())
    return false;
  SourceManager &SM = Context.getSourceManager();
  unsigned off = SM.getDecomposedLoc(loc).second;
  FileID fid = SM.getDecomposedLoc(loc).first;
  for (size_t i = 0; i < RemovedTemplates.size(); ++i) {
    SourceRange r = RemovedTemplates[i];
    if (SM.getDecomposedLoc(r.getBegin()).first != fid)
      continue;
    if (SM.getDecomposedLoc(r.getBegin()).second <= off &&
        off <= SM.getDecomposedLoc(r.getEnd()).second)
      return true;
  }
  return false;
}

bool CXXToCPlusTranslator::VisitClassTemplateDecl(ClassTemplateDecl *D) {
  if (Context.getSourceManager().isInSystemHeader(D->getLocation()))
    return true;

  // remove the template definition; the instantiations replace it
  SourceRange Range = D->getSourceRange();
  if (Range.isValid()) {
    RemovedTemplates.push_back(Range);
    Rewriter.BlankText(Range);
  }

  return true;
}

bool CXXToCPlusTranslator::VisitFunctionTemplateDecl(FunctionTemplateDecl *D) {
  if (Context.getSourceManager().isInSystemHeader(D->getLocation()))
    return true;

  // remove the function template definition; the instantiations replace it
  SourceRange Range = D->getSourceRange();
  if (Range.isValid()) {
    RemovedTemplates.push_back(Range);
    Rewriter.BlankText(Range);
  }

  return true;
}

bool CXXToCPlusTranslator::VisitFunctionDecl(FunctionDecl *D) {
  SourceManager &SM = Context.getSourceManager();
  if (SM.isInSystemHeader(D->getLocation()))
    return true;

  if (D->isTemplateInstantiation()) {
    if (llvm::isa<clang::CXXMethodDecl>(D))
      return true;
    // inject a concrete version with a mangled name
    if (D->isThisDeclarationADefinition()) {
      std::string Mangled = mangleFunctionName(D);

      std::string OriginalName;
      llvm::raw_string_ostream OrigOS(OriginalName);
      D->getNameForDiagnostic(OrigOS, Context.getPrintingPolicy(), true);
      OrigOS.flush();
      recordMangledName(Mangled, OriginalName);

      std::string InstCode;
      llvm::raw_string_ostream OS(InstCode);
      PrintingPolicy Policy = Context.getPrintingPolicy();
      Policy.TerseOutput = false;

      D->print(OS, Policy);
      OS << "\n";
      OS.flush();

      std::regex TemplatePrefixRegex("^\\s*template\\s*<[^>]*>\\s*");
      InstCode = std::regex_replace(InstCode, TemplatePrefixRegex, "");

      std::regex SigNameRegex(
          "\\b" + escapeForRegex(D->getNameAsString()) + "\\s*(<[^>]*>)?");
      InstCode = std::regex_replace(InstCode, SigNameRegex, Mangled);

      SourceLocation EndLoc;
      if (FunctionTemplateDecl *TD = D->getDescribedFunctionTemplate()) {
        EndLoc = TD->getSourceRange().getEnd();
      } else if (D->getMemberSpecializationInfo()) {
        EndLoc = D->getSourceRange().getEnd();
      } else {
        EndLoc = D->getEndLoc();
      }

      if (EndLoc.isValid())
        Rewriter.InsertTextAfterToken(EndLoc, "\n\n" + InstCode,
                                      originLineOf(SM, D->getBeginLoc()));
    }
  }

  return true;
}

bool CXXToCPlusTranslator::VisitCallExpr(CallExpr *E) {
  if (Context.getSourceManager().isInSystemHeader(E->getBeginLoc()))
    return true;
  if (insideRemovedTemplate(E->getBeginLoc()))
    return true;

  if (FunctionDecl *FD = E->getDirectCallee()) {
    if (FD->isTemplateInstantiation()) {
      if (llvm::isa<clang::CXXMethodDecl>(FD))
        return true;
      std::string Mangled = mangleFunctionName(FD);
      SourceRange Range;
      if (auto *DRE = dyn_cast<DeclRefExpr>(E->getCallee()->IgnoreParenImpCasts())) {
        Range = DRE->getSourceRange();
      } else {
        Range = E->getCallee()->getSourceRange();
      }

      if (Range.isValid()) {
        Rewriter.ReplaceText(Range, Mangled);
      }
    }
  }

  return true;
}

bool CXXToCPlusTranslator::VisitClassTemplateSpecializationDecl(ClassTemplateSpecializationDecl *D) {
  if (Context.getSourceManager().isInSystemHeader(D->getLocation()))
    return true;

  if (!D->isThisDeclarationADefinition())
    return true;

  std::string Mangled = mangleTemplateName(D);
  std::string BaseName = D->getNameAsString();

  std::string OriginalName;
  llvm::raw_string_ostream OrigOS(OriginalName);
  D->getNameForDiagnostic(OrigOS, Context.getPrintingPolicy(), true);
  OrigOS.flush();
  recordMangledName(Mangled, OriginalName);

  std::string InstCode;
  llvm::raw_string_ostream OS(InstCode);
  PrintingPolicy Policy = Context.getPrintingPolicy();
  Policy.IncludeTagDefinition = true;
  Policy.TerseOutput = false;
  Policy.Indentation = 2;

  OS << "class " << Mangled << " {\n";
  for (auto *Decl : D->decls()) {
    if (Decl->isImplicit()) continue;

    if (auto *AS = dyn_cast<AccessSpecDecl>(Decl)) {
      if (AS->getAccess() == AS_public) OS << "public:\n";
      else if (AS->getAccess() == AS_protected) OS << "protected:\n";
      else OS << "private:\n";
      continue;
    }

    std::string DeclCode;
    llvm::raw_string_ostream DeclOS(DeclCode);

    // graft the template pattern's body onto the instantiated method so
    // print() emits it; this mutates the AST (the one exception to the
    // no-mutation rule, the mutated decl is implicit anyway)
    if (auto *MD = dyn_cast<CXXMethodDecl>(Decl)) {
      if (!MD->isPure() && !MD->hasBody()) {
        if (auto *Pattern = MD->getTemplateInstantiationPattern()) {
          if (Pattern->hasBody()) {
            MD->setInnerLocStart(Pattern->getInnerLocStart());
            MD->setBody(Pattern->getBody());
          }
        }
      }
    }

    Decl->print(DeclOS, Policy);
    DeclOS.flush();

    while (!DeclCode.empty() && std::isspace(DeclCode.back()))
      DeclCode.pop_back();

    OS << "  " << DeclCode;
    if (!DeclCode.empty() && DeclCode.back() != ';' && DeclCode.back() != '}') {
        OS << ";";
    }
    OS << "\n";
  }
  OS << "};\n";
  OS.flush();

  // uses of the specialised type inside the body get the mangled name
  std::string FullSpecName;
  llvm::raw_string_ostream SpecOS(FullSpecName);
  D->getNameForDiagnostic(SpecOS, Policy, true);
  SpecOS.flush();

  if (!FullSpecName.empty()) {
    size_t pos = 0;
    while ((pos = InstCode.find(FullSpecName, pos)) != std::string::npos) {
      InstCode.replace(pos, FullSpecName.length(), Mangled);
      pos += Mangled.length();
    }
  }

  // constructors and destructors carry the base name
  std::regex BaseNameRegex("\\b" + escapeForRegex(BaseName) + "\\b");
  InstCode = std::regex_replace(InstCode, BaseNameRegex, Mangled);

  // insert before the template definition
  if (ClassTemplateDecl *TD = D->getSpecializedTemplate()) {
    SourceLocation InsertLoc = TD->getBeginLoc();
    if (InsertLoc.isValid())
      Rewriter.InsertTextBefore(
          InsertLoc, InstCode + "\n\n",
          originLineOf(Context.getSourceManager(), InsertLoc));
  }

  return true;
}

std::string CXXToCPlusTranslator::mangleFunctionName(const FunctionDecl *D) {
  if (FunctionMangledNames.count(D)) return FunctionMangledNames[D];

  std::string FullName;
  llvm::raw_string_ostream OS(FullName);
  D->getNameForDiagnostic(OS, Context.getPrintingPolicy(), true);
  OS.flush();

  std::string Mangled = flattenName(FullName);

  FunctionMangledNames[D] = Mangled;
  return Mangled;
}

std::string CXXToCPlusTranslator::mangleTemplateName(const ClassTemplateSpecializationDecl *D) {
  if (MangledNames.count(D)) return MangledNames[D];

  std::string FullName;
  llvm::raw_string_ostream OS(FullName);
  D->getNameForDiagnostic(OS, Context.getPrintingPolicy(), true);
  OS.flush();

  std::string Mangled = flattenName(FullName);

  MangledNames[D] = Mangled;
  return Mangled;
}

static std::string getTagKeyword(const CXXRecordDecl *RD) {
  switch (RD->getTagKind()) {
    case TTK_Class:  return "class";
    case TTK_Struct: return "struct";
    case TTK_Union:  return "union";
    default:         return "";
  }
}


bool CXXToCPlusTranslator::VisitTypeLoc(TypeLoc TL) {
  if (Context.getSourceManager().isInSystemHeader(TL.getBeginLoc()))
    return true;
  if (insideRemovedTemplate(TL.getBeginLoc()))
    return true;

  PrintingPolicy Policy(Context.getLangOpts());
  Policy.SuppressTagKeyword = false;
  Policy.SuppressScope = false;
  Policy.SuppressUnwrittenScope = false;

  if (auto TSTL = TL.getAs<TemplateSpecializationTypeLoc>()) {
    const TemplateSpecializationType *TST = TSTL.getTypePtr();
    if (auto *RD = TST->getAsCXXRecordDecl()) {
      if (auto *D = dyn_cast<ClassTemplateSpecializationDecl>(RD)) {
        std::string elabTypeSpec = getTagKeyword(RD);
        std::string NewName = elabTypeSpec + " " + mangleTemplateName(D);
        Rewriter.ReplaceText(TSTL.getSourceRange(), NewName);
      }
    }
  }
  return true;
}

bool CXXToCPlusTranslator::shouldVisitTemplateInstantiations() const { return true; }
bool CXXToCPlusTranslator::shouldVisitImplicitCode() const { return false; }


bool CXXToCPlusTranslator::VisitFieldDecl(FieldDecl *Decl) {
  if (VisitedDecls.count(Decl)) return true;
  VisitedDecls.insert(Decl);
  if (insideRemovedTemplate(Decl->getBeginLoc()))
    return true;

  QualType T = Decl->getType();
  if (T->isRecordType()) {
    std::string Name = Decl->getNameAsString();

    PrintingPolicy Policy(Context.getLangOpts());
    Policy.SuppressTagKeyword = false;
    Policy.SuppressScope = false;
    Policy.SuppressUnwrittenScope = false;

    std::string NewName = T.getCanonicalType().getAsString(Policy) + " " + Name;
    Rewriter.ReplaceText(Decl->getSourceRange(), NewName);
  }
  return true;
}

bool CXXToCPlusTranslator::VisitMemberExpr(MemberExpr *ME) {
  if (VisitedMemberExprs.count(ME)) return true;
  VisitedMemberExprs.insert(ME);
  if (insideRemovedTemplate(ME->getMemberLoc()))
    return true;

  SourceLocation Loc = ME->getMemberLoc();
  SourceManager &SM = Context.getSourceManager();

  if (SM.isInSystemHeader(Loc) || Loc.isMacroID())
    return true;

  if (!ME->isImplicitAccess())
    return true;

  const FieldDecl *FD = dyn_cast<FieldDecl>(ME->getMemberDecl());
  if (!FD)
    return true;

  if (isa<ClassTemplateSpecializationDecl>(FD->getParent()))
    return true;

  Rewriter.InsertText(Loc, "this->");

  return true;
}

bool CXXToCPlusTranslator::VisitCXXThrowExpr(CXXThrowExpr *E) {
  if (insideRemovedTemplate(E->getBeginLoc()))
    return true;
  if (const Expr *SubExpr = E->getSubExpr()) {
    QualType T = SubExpr->getType();
    PrintingPolicy Policy(Context.getLangOpts());
    Policy.SuppressTagKeyword = false;
    std::string TypeStr = T.getAsString(Policy);

    Rewriter.InsertTextBefore(SubExpr->getBeginLoc(), "(" + TypeStr + ") (");
    Rewriter.InsertTextAfterToken(SubExpr->getEndLoc(), ")");
  }
  return true;
}


bool CXXToCPlusTranslator::VisitCXXMemberCallExpr(CXXMemberCallExpr *CE) {
  SourceManager &SM = Context.getSourceManager();
  if (SM.isInSystemHeader(CE->getBeginLoc()))
    return true;
  if (insideRemovedTemplate(CE->getBeginLoc()))
    return true;

  MemberExpr *ME = dyn_cast<MemberExpr>(CE->getCallee()->IgnoreParenImpCasts());
  if (!ME) return true;

  if (ME->getQualifier()) return true; // already qualified

  CXXMethodDecl *MD = CE->getMethodDecl();
  if (!MD) return true;

  if (MD->isImplicit()) return true;

  // virtual calls are not statically dispatched, do not qualify them
  if (MD->isVirtual()) return true;

  CXXRecordDecl *RD = MD->getParent();
  if (!RD) return true;

  SourceLocation MemberLoc = ME->getMemberLoc();
  if (MemberLoc.isInvalid() ||
      (MemberLoc.isMacroID() && !SM.isMacroArgExpansion(MemberLoc)))
    return true;

  SourceLocation InsertLoc = SM.isMacroArgExpansion(MemberLoc)
    ? SM.getSpellingLoc(MemberLoc)
    : MemberLoc;

  std::string QualName;
  if (auto *Spec = dyn_cast<ClassTemplateSpecializationDecl>(RD)) {
    QualName = mangleTemplateName(Spec) + "::";
  } else {
    QualName = RD->getQualifiedNameAsString() + "::";
  }

  Rewriter.InsertTextBefore(InsertLoc, QualName);
  return true;
}

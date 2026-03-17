#include "CXXInfoExtractor.hpp"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceManager.h"
#include <regex>
#include <algorithm>
#include <cctype>
#include "clang/AST/ParentMapContext.h"
using namespace clang;

/**
 * CXXInfoExtractor - Extracts structural and semantic information from C++ code.
 */
bool CXXInfoExtractor::VisitCXXRecordDecl(CXXRecordDecl *Decl) {
  if (Decl->isImplicit() || !Decl->isThisDeclarationADefinition())
    return true;

  if (VisitedDecls.count(Decl)) return true;
  VisitedDecls.insert(Decl);

  if (Decl->getDescribedClassTemplate() && !isa<ClassTemplateSpecializationDecl>(Decl)) {
    return true;
  }

  if (Context.getSourceManager().isInSystemHeader(Decl->getLocation())) {
    return true;
  }

  CXXClassInfo ClassInfo;
  if (auto *Spec = dyn_cast<ClassTemplateSpecializationDecl>(Decl)) {
    ClassInfo.QualifiedName = mangleTemplateName(Spec);
  } else {
    ClassInfo.QualifiedName = Decl->getQualifiedNameAsString();
  }

  for (const auto &Base : Decl->bases()) {
    ClassInfo.BaseClasses.push_back(Base.getType().getCanonicalType().getAsString());
  }

  for (auto *Method : Decl->methods()) {
    if (Method->isImplicit()) continue;
    if (Method->getDescribedFunctionTemplate()) continue;

    CXXMethodInfo MInfo;
    MInfo.Name = Method->getNameAsString();
    MInfo.ReturnType = Method->getReturnType().getCanonicalType().getAsString();
    MInfo.IsStatic = Method->isStatic();
    MInfo.IsVirtual = Method->isVirtual();
    MInfo.IsPure = Method->isPure();
    MInfo.IsConst = Method->isConst();

    for (auto *Param : Method->parameters()) {
      MInfo.Parameters.push_back(Param->getType().getCanonicalType().getAsString());
    }

    ClassInfo.Methods.push_back(MInfo);
  }

  for (auto *Field : Decl->fields()) {
    if (Field->isImplicit()) continue;

    CXXFieldInfo FInfo;
    FInfo.Name = Field->getNameAsString();
    FInfo.Type = Field->getType().getCanonicalType().getAsString();
    ClassInfo.Fields.push_back(FInfo);
  }

  if (ReadOnly) {
    Result.Classes.push_back(ClassInfo);
  }
  return true;
}

bool CXXInfoExtractor::VisitClassTemplateDecl(ClassTemplateDecl *D) {
  if (Context.getSourceManager().isInSystemHeader(D->getLocation()))
    return true;

  // Comment out the template definition
  if (!ReadOnly) {
    SourceRange Range = D->getSourceRange();
    if (Range.isValid()) {
      Rewriter.InsertTextBefore(Range.getBegin(), "/* ");
      Rewriter.InsertTextAfterToken(Range.getEnd(), " */");
    }
  }

  return true;
}

bool CXXInfoExtractor::VisitFunctionTemplateDecl(FunctionTemplateDecl *D) {
  if (Context.getSourceManager().isInSystemHeader(D->getLocation()))
    return true;

  // Comment out the function template definition
  if (!ReadOnly) {
    SourceRange Range = D->getSourceRange();
    if (Range.isValid()) {
      Rewriter.InsertTextBefore(Range.getBegin(), "/* ");
      Rewriter.InsertTextAfterToken(Range.getEnd(), " */");
    }
  }

  return true;
}

bool CXXInfoExtractor::VisitFunctionDecl(FunctionDecl *D) {
  if (Context.getSourceManager().isInSystemHeader(D->getLocation()))
    return true;

  if (D->isTemplateInstantiation()) {
    if (llvm::isa<clang::CXXMethodDecl>(D))
      return true;
    // For function template instantiations, inject a concrete version
    // with a mangled name if not in read-only mode.
    if (!ReadOnly && D->isThisDeclarationADefinition()) {
        std::string MangledName = mangleFunctionName(D);

        std::string OriginalName;
        llvm::raw_string_ostream OrigOS(OriginalName);
        D->getNameForDiagnostic(OrigOS, Context.getPrintingPolicy(), true);
        OrigOS.flush();

        bool already_mapped = false;
        for (const auto& mapping : Result.MangledNames) {
            if (mapping.MangledName == MangledName) {
                already_mapped = true;
                break;
            }
        }
        if (!already_mapped) {
            NameMapping Mapping;
            Mapping.MangledName = MangledName;
            Mapping.OriginalName = OriginalName;
            Result.MangledNames.push_back(Mapping);
        }

        std::string InstCode;
        llvm::raw_string_ostream OS(InstCode);
        PrintingPolicy Policy = Context.getPrintingPolicy();
        Policy.TerseOutput = false;

        D->print(OS, Policy);
        OS << "\n";
        OS.flush();

        std::regex TemplatePrefixRegex("^\\s*template\\s*<[^>]*>\\s*");
        InstCode = std::regex_replace(InstCode, TemplatePrefixRegex, "");

        std::regex SigNameRegex("\\b" + D->getNameAsString() + "\\s*(<[^>]*>)?");
        InstCode = std::regex_replace(InstCode, SigNameRegex, MangledName);

        SourceLocation EndLoc;
        if (FunctionTemplateDecl *TD = D->getDescribedFunctionTemplate()) {
             EndLoc = TD->getSourceRange().getEnd();
        } else if (D->getMemberSpecializationInfo()) {
             EndLoc = D->getSourceRange().getEnd();
        } else {
             EndLoc = D->getEndLoc();
        }

        if (EndLoc.isValid()) {
             Rewriter.InsertTextAfterToken(EndLoc, "\n\n" + InstCode);
        }
    }
  }

  return true;
}

bool CXXInfoExtractor::VisitCallExpr(CallExpr *E) {
  if (ReadOnly) return true;
  if (Context.getSourceManager().isInSystemHeader(E->getBeginLoc()))
    return true;

  if (FunctionDecl *FD = E->getDirectCallee()) {
    if (FD->isTemplateInstantiation()) {
      if (llvm::isa<clang::CXXMethodDecl>(FD))
        return true;
      std::string MangledName = mangleFunctionName(FD);
      SourceRange Range;
      if (auto *DRE = dyn_cast<DeclRefExpr>(E->getCallee()->IgnoreParenImpCasts())) {
        Range = DRE->getSourceRange();
      } else {
        Range = E->getCallee()->getSourceRange();
      }

      if (Range.isValid()) {
        Rewriter.ReplaceText(Range, MangledName);
      }
    }
  }

  return true;
}

bool CXXInfoExtractor::VisitClassTemplateSpecializationDecl(ClassTemplateSpecializationDecl *D) {
  if (Context.getSourceManager().isInSystemHeader(D->getLocation()))
    return true;

  if (!D->isThisDeclarationADefinition())
    return true;

  std::string MangledName = mangleTemplateName(D);
  std::string BaseName = D->getNameAsString();

  std::string OriginalName;
  llvm::raw_string_ostream OrigOS(OriginalName);
  D->getNameForDiagnostic(OrigOS, Context.getPrintingPolicy(), true);
  OrigOS.flush();

  bool already_mapped = false;
  for (const auto& mapping : Result.MangledNames) {
      if (mapping.MangledName == MangledName) {
          already_mapped = true;
          break;
      }
  }
  if (!already_mapped) {
      NameMapping Mapping;
      Mapping.MangledName = MangledName;
      Mapping.OriginalName = OriginalName;
      Result.MangledNames.push_back(Mapping);
  }

  std::string InstCode;
  llvm::raw_string_ostream OS(InstCode);
  PrintingPolicy Policy = Context.getPrintingPolicy();
  Policy.IncludeTagDefinition = true;
  Policy.TerseOutput = false;
  Policy.Indentation = 2;

  OS << "class " << MangledName << " {\n";
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

    // For methods, try to ensure we get the body if it's available in the pattern
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

    // Trim trailing whitespace to check last char
    while (!DeclCode.empty() && std::isspace(DeclCode.back())) {
        DeclCode.pop_back();
    }

    OS << "  " << DeclCode;
    if (!DeclCode.empty() && DeclCode.back() != ';' && DeclCode.back() != '}') {
        OS << ";";
    }
    OS << "\n";
  }
  OS << "};\n";
  OS.flush();

  // Replace any specialized type references inside the class body
  std::string FullSpecName;
  llvm::raw_string_ostream SpecOS(FullSpecName);
  D->getNameForDiagnostic(SpecOS, Policy, true);
  SpecOS.flush();

  if (!FullSpecName.empty()) {
    size_t pos = 0;
    while ((pos = InstCode.find(FullSpecName, pos)) != std::string::npos) {
      InstCode.replace(pos, FullSpecName.length(), MangledName);
      pos += MangledName.length();
    }
  }

  // Replace remaining instances of the base name (for constructors/destructors)
  std::regex BaseNameRegex("\\b" + BaseName + "\\b");
  InstCode = std::regex_replace(InstCode, BaseNameRegex, MangledName);

  // Insert before the template definition
  if (!ReadOnly) {
    if (ClassTemplateDecl *TD = D->getSpecializedTemplate()) {
      SourceLocation InsertLoc = TD->getBeginLoc();
      if (InsertLoc.isValid()) {
        Rewriter.InsertTextBefore(InsertLoc, InstCode + "\n\n");
      }
    }
  }

  return true;
}

std::string CXXInfoExtractor::mangleFunctionName(const FunctionDecl *D) {
  if (FunctionMangledNames.count(D)) return FunctionMangledNames[D];

  std::string FullName;
  llvm::raw_string_ostream OS(FullName);
  D->getNameForDiagnostic(OS, Context.getPrintingPolicy(), true);
  OS.flush();

  std::string Mangled = FullName;
  std::replace(Mangled.begin(), Mangled.end(), '<', '_');
  std::replace(Mangled.begin(), Mangled.end(), '>', '_');
  std::replace(Mangled.begin(), Mangled.end(), ',', '_');
  std::replace(Mangled.begin(), Mangled.end(), ' ', '_');
  std::replace(Mangled.begin(), Mangled.end(), ':', '_');
  std::replace(Mangled.begin(), Mangled.end(), '(', '_');
  std::replace(Mangled.begin(), Mangled.end(), ')', '_');
  Mangled.erase(std::remove(Mangled.begin(), Mangled.end(), '*'), Mangled.end());
  Mangled.erase(std::remove(Mangled.begin(), Mangled.end(), '&'), Mangled.end());

  Mangled.erase(std::remove_if(Mangled.begin(), Mangled.end(), [](char c) {
    return !std::isalnum(c) && c != '_';
  }), Mangled.end());

  while (Mangled.length() > 1 && Mangled.back() == '_' && Mangled[Mangled.length()-2] == '_') {
      Mangled.pop_back();
  }

  FunctionMangledNames[D] = Mangled;
  return Mangled;
}

std::string CXXInfoExtractor::mangleTemplateName(const ClassTemplateSpecializationDecl *D) {
  if (MangledNames.count(D)) return MangledNames[D];

  std::string FullName;
  llvm::raw_string_ostream OS(FullName);
  D->getNameForDiagnostic(OS, Context.getPrintingPolicy(), true);
  OS.flush();

  std::string Mangled = FullName;
  std::replace(Mangled.begin(), Mangled.end(), '<', '_');
  std::replace(Mangled.begin(), Mangled.end(), '>', '_');
  std::replace(Mangled.begin(), Mangled.end(), ',', '_');
  std::replace(Mangled.begin(), Mangled.end(), ' ', '_');
  std::replace(Mangled.begin(), Mangled.end(), ':', '_');

  Mangled.erase(std::remove_if(Mangled.begin(), Mangled.end(), [](char c) {
    return !std::isalnum(c) && c != '_';
  }), Mangled.end());

  while (Mangled.length() > 1 && Mangled.back() == '_' && Mangled[Mangled.length()-2] == '_') {
      Mangled.pop_back();
  }

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


bool CXXInfoExtractor::VisitTypeLoc(TypeLoc TL) {
  if (Context.getSourceManager().isInSystemHeader(TL.getBeginLoc()))
    return true;

  if (ReadOnly) return true;
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

bool CXXInfoExtractor::shouldVisitTemplateInstantiations() const { return true; }
bool CXXInfoExtractor::shouldVisitImplicitCode() const { return false; }


bool CXXInfoExtractor::VisitFieldDecl(FieldDecl *Decl) {
  if (VisitedDecls.count(Decl)) return true;
  VisitedDecls.insert(Decl);

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


bool CXXInfoExtractor::VisitVarDecl(VarDecl *Decl) {
  if (Context.getSourceManager().isInSystemHeader(Decl->getLocation())) {
    return true;
  }

  if (VisitedDecls.count(Decl)) return true;
  VisitedDecls.insert(Decl);

  return true;
}


bool CXXInfoExtractor::VisitMemberExpr(MemberExpr *ME) {
  if (ReadOnly) return true;

  if (VisitedMemberExprs.count(ME)) return true;
  VisitedMemberExprs.insert(ME);

  SourceLocation Loc = ME->getMemberLoc();
  SourceManager &SM = Context.getSourceManager();

  if (SM.isInSystemHeader(Loc) || Loc.isMacroID())
    return true;

  if (!ME->isImplicitAccess())
    return true;

  const FieldDecl *FD = dyn_cast<FieldDecl>(ME->getMemberDecl());
  if (!FD)
    return true;

  Rewriter.InsertText(Loc, "this->");

  return true;
}

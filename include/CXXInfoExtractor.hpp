#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/ASTContext.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "llvm/Support/YAMLTraits.h"
#include <string>
#include <vector>

struct CXXMethodInfo {
  std::string Name;
  std::string ReturnType;
  std::vector<std::string> Parameters;
  bool IsStatic;
  bool IsVirtual;
  bool IsPure;
  bool IsConst;
};

struct CXXFieldInfo {
  std::string Name;
  std::string Type;
};

struct CXXObjectInfo {
  std::string Name;
  std::string Type;
  int LineNumber;
  int ColumnNumber;
  std::string EnclosingFunction;
};

struct CXXClassInfo {
  std::string QualifiedName;
  std::vector<std::string> BaseClasses;
  std::vector<CXXMethodInfo> Methods;
  std::vector<CXXFieldInfo> Fields;
};

struct NameMapping {
  std::string MangledName;
  std::string OriginalName;
};

struct SemanticAnalysisResult {
  std::vector<CXXClassInfo> Classes;
  std::vector<CXXObjectInfo> Objects;
  std::vector<NameMapping> MangledNames;
};

// LLVM YAML I/O Traits
LLVM_YAML_IS_SEQUENCE_VECTOR(CXXMethodInfo)
LLVM_YAML_IS_SEQUENCE_VECTOR(CXXFieldInfo)
LLVM_YAML_IS_SEQUENCE_VECTOR(CXXClassInfo)
LLVM_YAML_IS_SEQUENCE_VECTOR(CXXObjectInfo)
LLVM_YAML_IS_SEQUENCE_VECTOR(NameMapping)

namespace llvm {
namespace yaml {

template <>
struct MappingTraits<CXXMethodInfo> {
  static void mapping(IO &io, CXXMethodInfo &info) {
    io.mapRequired("Name", info.Name);
    io.mapRequired("ReturnType", info.ReturnType);
    io.mapOptional("Parameters", info.Parameters);
    io.mapRequired("IsStatic", info.IsStatic);
    io.mapRequired("IsVirtual", info.IsVirtual);
    io.mapRequired("IsPure", info.IsPure);
    io.mapRequired("IsConst", info.IsConst);
  }
};

template <>
struct MappingTraits<CXXFieldInfo> {
  static void mapping(IO &io, CXXFieldInfo &info) {
    io.mapRequired("Name", info.Name);
    io.mapRequired("Type", info.Type);
  }
};

template <>
struct MappingTraits<CXXObjectInfo> {
  static void mapping(IO &io, CXXObjectInfo &info) {
    io.mapRequired("Name", info.Name);
    io.mapRequired("Type", info.Type);
    io.mapRequired("LineNumber", info.LineNumber);
    io.mapRequired("ColumnNumber", info.ColumnNumber);
    io.mapOptional("EnclosingFunction", info.EnclosingFunction);
  }
};

template <>
struct MappingTraits<NameMapping> {
  static void mapping(IO &io, NameMapping &info) {
    io.mapRequired("MangledName", info.MangledName);
    io.mapRequired("OriginalName", info.OriginalName);
  }
};

template <>
struct MappingTraits<CXXClassInfo> {
  static void mapping(IO &io, CXXClassInfo &info) {
    io.mapRequired("QualifiedName", info.QualifiedName);
    io.mapOptional("BaseClasses", info.BaseClasses);
    io.mapOptional("Methods", info.Methods);
    io.mapOptional("Fields", info.Fields);
  }
};

template <>
struct MappingTraits<SemanticAnalysisResult> {
  static void mapping(IO &io, SemanticAnalysisResult &result) {
    io.mapRequired("Classes", result.Classes);
    io.mapOptional("Objects", result.Objects);
    io.mapOptional("MangledNames", result.MangledNames);
  }
};

} // namespace yaml
} // namespace llvm

class CXXInfoExtractor : public clang::RecursiveASTVisitor<CXXInfoExtractor> {
public:
  explicit CXXInfoExtractor(clang::ASTContext &Context, clang::Rewriter &Rewriter, SemanticAnalysisResult &Result, bool ReadOnly = false)
      : Context(Context), Rewriter(Rewriter), Result(Result), ReadOnly(ReadOnly) {}

  bool VisitCXXRecordDecl(clang::CXXRecordDecl *Declaration);
  bool VisitVarDecl(clang::VarDecl *Declaration);
  bool VisitClassTemplateDecl(clang::ClassTemplateDecl *D);
  bool VisitClassTemplateSpecializationDecl(clang::ClassTemplateSpecializationDecl *D);
  bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl *D);
  bool VisitFunctionDecl(clang::FunctionDecl *D);
  bool VisitCallExpr(clang::CallExpr *E);
  bool VisitTypeLoc(clang::TypeLoc TL);
  bool VisitFieldDecl(clang::FieldDecl *D);
  bool VisitMemberExpr(clang::MemberExpr *ME);
  bool VisitCXXThrowExpr(clang::CXXThrowExpr *E);

  bool shouldVisitTemplateInstantiations() const;
  bool shouldVisitImplicitCode() const;

  const SemanticAnalysisResult& getResult() const { return Result; }

private:
  std::string mangleTemplateName(const clang::ClassTemplateSpecializationDecl *D);
  std::string mangleFunctionName(const clang::FunctionDecl *D);
  clang::ASTContext &Context;
  clang::Rewriter &Rewriter;
  bool ReadOnly;
  SemanticAnalysisResult &Result;
  std::map<const clang::ClassTemplateSpecializationDecl*, std::string> MangledNames;
  std::map<const clang::FunctionDecl*, std::string> FunctionMangledNames;
  std::set<const clang::Decl*> VisitedDecls;
  std::set<const clang::MemberExpr*> VisitedMemberExprs;
};

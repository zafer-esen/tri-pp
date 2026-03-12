#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "TriCeraPreprocessorMain.hpp"
#include "CXXInfoExtractor.hpp"

class TemplateExpansionConsumer : public clang::ASTConsumer {
public:
  TemplateExpansionConsumer(clang::Rewriter &rewriter, SemanticAnalysisResult &Result) 
      : rewriter(rewriter), Result(Result) {}
  void HandleTranslationUnit(clang::ASTContext& Ctx) override;
private:
  clang::Rewriter &rewriter;
  SemanticAnalysisResult &Result;
};

class MainConsumer : public clang::ASTConsumer {
public:
  MainConsumer(clang::Rewriter &rewriter,
               PreprocessOutput &out) : 
               rewriter(rewriter), preprocessOutput(out) {}
  void HandleTranslationUnit(clang::ASTContext& Ctx) override;
  ~MainConsumer() override;

private:
  clang::Rewriter &rewriter;
  PreprocessOutput &preprocessOutput;
};

class CXXInfoExtractionConsumer : public clang::ASTConsumer {
public:
  CXXInfoExtractionConsumer(clang::Rewriter &rewriter, const std::string &yamlOutput, SemanticAnalysisResult &Result) 
      : rewriter(rewriter), yamlOutput(yamlOutput), Result(Result) {}
  void HandleTranslationUnit(clang::ASTContext& Ctx) override;
private:
  clang::Rewriter &rewriter;
  std::string yamlOutput;
  SemanticAnalysisResult &Result;
};

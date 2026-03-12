#include "TriceraPreprocessor.hpp"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/PrettyPrinter.h"

#include "UsedFunctionAndTypeCollector.hpp"
#include "TypedefRemover.hpp"
#include "UnusedDeclCommenter.hpp"
#include "TypeCanoniser.hpp"
#include "ForLoopStmtExtractor.hpp"
#include "CharRewriter.hpp"
#include "Determinizer.hpp"
#include "ExecutionCountAnalyzer.hpp"
#include "NondetLoopGuardRewriter.hpp"
#include "UniqueCallSiteTransformer.hpp"
#include "CXXInfoExtractor.hpp"
#include "llvm/Support/YAMLTraits.h"
#include <fstream>

using namespace clang;
using namespace ast_matchers;
using namespace std;
extern llvm::cl::opt<bool> determinize;
extern llvm::cl::opt<bool> makeCallsUnique;
extern llvm::cl::opt<bool> noDeclSlice;
extern llvm::cl::opt<std::string> entryFunctionName;
extern llvm::cl::opt<std::string> encode;
extern llvm::cl::opt<std::string> cxxYamlOutput;

#include <string>

//-----------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Pass 1: Template Expansion
//-----------------------------------------------------------------------------
void TemplateExpansionConsumer::HandleTranslationUnit(clang::ASTContext& Ctx) {
  if (!Ctx.getLangOpts().CPlusPlus) return;
  
  // Create extractor in a mode that handles template expansion
  CXXInfoExtractor extractor(Ctx, rewriter, Result);
  extractor.TraverseDecl(Ctx.getTranslationUnitDecl());
}

//-----------------------------------------------------------------------------
// Pass 2: Standard Preprocessors
//-----------------------------------------------------------------------------
void MainConsumer::HandleTranslationUnit(clang::ASTContext& Ctx) {
  // todo: process files where an error has occurred?
  // errors also occur on some inputs which TriCera would accept
  bool hadError = Ctx.getDiagnostics().hasErrorOccurred();
  bool isCXX = Ctx.getLangOpts().CPlusPlus;
  bool collectAllFuns = hadError || noDeclSlice || isCXX;
  bool collectAllTypes = hadError || noDeclSlice || isCXX;
  // todo: or return without doing anything?
  // if (Ctx.getDiagnostics().hasErrorOccurred()) return;

  // collect all used functions and types
  UsedFunAndTypeCollector usedFunsAndTypes(Ctx, collectAllFuns,
                                           collectAllTypes);

  if (makeCallsUnique) {
    UniqueCallTransformer uniqueCaller(rewriter, Ctx, usedFunsAndTypes);
    uniqueCaller.transform();
  } else if (determinize) {
    ExecutionCountAnalyzer execAnalyzer(Ctx, usedFunsAndTypes, entryFunctionName);
    // execAnalyzer.printFrequencies(llvm::outs()); // only for debugging
    Determinizer determinizer(rewriter, Ctx, execAnalyzer);
  } else { // Run the default stages
    // then remove all typedefs and remove unused record typedef declarations
    TypedefRemover typedefRemover(rewriter, Ctx, usedFunsAndTypes);
    // then comment out all unused declarations
    if (!hadError && !noDeclSlice)
      UnusedDeclCommenter declCommenter(rewriter, Ctx, usedFunsAndTypes);
    // and canonise all used types
    TypeCanoniser typeCanoniser(rewriter, Ctx, usedFunsAndTypes);
    // extract declStmts from inside for loop declarations
    ForLoopStmtExtractor forLoopStmtExtractor(rewriter, Ctx, usedFunsAndTypes);
    // replace char init expressions with their corresponding integer values
    CharRewriter charRewriter(rewriter, Ctx, usedFunsAndTypes);
    // rewrite while loops with nondet (extern) function calls in their guards
    // "while(nondet())" --> "{tmp = nondet(); while(tmp-->0)}"
    NondetLoopGuardRewriter externLoopRewriter(rewriter, Ctx);

    // finally add contract annotations to recursive functions
    preprocessOutput.numRecursiveFuns = 0;
    for (auto funInfo : usedFunsAndTypes.getSeenFunctions()) {
      if (funInfo->isRecursive()) {
        auto decl = funInfo->getDecl();
        if (decl->hasBody()) { // todo: ignore decls without bodies?
          rewriter.InsertTextBefore(decl->getBeginLoc(),
                                    "/*@ contract @*/ ");
          preprocessOutput.numRecursiveFuns ++;
        }
      }
    }
  }

  preprocessOutput.usesArrays = 0;
  for (const Type* typ : usedFunsAndTypes.getSeenTypes()) {
    if (typ->isArrayType())
      preprocessOutput.usesArrays = 1;
  }

  // todo: set this to 1 if any unsupported features exist
  preprocessOutput.isUnsupported = 0;

  // todo: any string to pass to output?
  std::string s = "foo";
  preprocessOutput.outputBuffer = new char[s.length()+1];
  std::strcpy(preprocessOutput.outputBuffer, s.c_str());
  delete[] preprocessOutput.outputBuffer;

  //llvm::outs() << "--------------------------\n";
  //llvm::outs() << "Seen functions:\n";
  //llvm::outs() << "--------------------------\n";
  //for (auto funInfo : usedFunsAndTypes.getSeenFunctions()) {
  //  llvm::outs() << funInfo->getDecl()->getNameAsString();
  //  if(funInfo->isRecursive())
  //    llvm::outs() << " (recursive)";
  //  llvm::outs() << "\n";
  //}
  //llvm::outs() << "--------------------------\n";
  //llvm::outs() << "Seen types:\n";
  //llvm::outs() << "--------------------------\n";
  //for (const Type* typ : usedFunsAndTypes.getSeenTypes()) {
  //  typ->dump();
  //}
  //rewriter.getEditBuffer(rewriter.getSourceMgr().getMainFileID())
  //    .write(llvm::outs());
}

MainConsumer::~MainConsumer() {
  // any cleanup?
}

//-----------------------------------------------------------------------------
// Pass 3: Semantic Extraction on the final file
//-----------------------------------------------------------------------------
void CXXInfoExtractionConsumer::HandleTranslationUnit(clang::ASTContext& Ctx) {
  if (yamlOutput.empty()) return;

  CXXInfoExtractor extractor(Ctx, rewriter, Result, true /* ReadOnly */);
  extractor.TraverseDecl(Ctx.getTranslationUnitDecl());
  
  std::error_code EC;
  llvm::raw_fd_ostream yamlFile(yamlOutput, EC, llvm::sys::fs::OF_None);
  if (!EC) {
    llvm::yaml::Output yamlOut(yamlFile);
    yamlOut << Result;
  } else {
    llvm::errs() << "Error opening YAML output file: " << EC.message() << "\n";
  }
}

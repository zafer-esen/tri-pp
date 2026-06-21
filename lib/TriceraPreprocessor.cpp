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
#include "CXXToCPlusTranslator.hpp"

using namespace clang;
using namespace ast_matchers;
using namespace std;
extern llvm::cl::opt<bool> noDeclSlice;
extern llvm::cl::opt<std::string> entryFunctionName;
extern llvm::cl::opt<std::string> factsFilename;

#include <string>

//-----------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------
void MainConsumer::runStage(Stage stage, clang::ASTContext &Ctx,
                            UsedFunAndTypeCollector &usedFunsAndTypes,
                            bool hadError) {
  switch (stage) {
  case Stage::CXX_TO_CPLUS: {
    // translate C++ to the C+ subset
    if (!Ctx.getLangOpts().CPlusPlus)
      break;
    rewriter.setActiveTransformer("CXXToCPlusTranslator");
    CXXToCPlusTranslator translator(Ctx, rewriter, state.facts.mangledNames);
    translator.TraverseDecl(Ctx.getTranslationUnitDecl());
    break;
  }
  case Stage::TYPE_CANONISE: {
    // canonise all used types
    rewriter.setActiveTransformer("TypeCanoniser");
    TypeCanoniser typeCanoniser(rewriter, Ctx, usedFunsAndTypes);
    // collect while the typedefs still exist, before the removal round
    if (!factsFilename.empty())
      collectTypedefs(Ctx, state.facts);
    break;
  }
  case Stage::TYPEDEF_REMOVE: {
    // remove all typedefs; their uses were canonised in an earlier round
    rewriter.setActiveTransformer("TypedefRemover");
    TypedefRemover typedefRemover(rewriter, Ctx, usedFunsAndTypes);
    break;
  }
  case Stage::CHAR_REWRITE: {
    // replace char literals with their corresponding integer values
    rewriter.setActiveTransformer("CharRewriter");
    CharRewriter charRewriter(rewriter, Ctx, usedFunsAndTypes);
    break;
  }
  case Stage::FOR_LOOP_EXTRACT: {
    // extract declStmts from inside for loop declarations
    rewriter.setActiveTransformer("ForLoopStmtExtractor");
    ForLoopStmtExtractor forLoopStmtExtractor(rewriter, Ctx, usedFunsAndTypes);
    break;
  }
  case Stage::NONDET_LOOP_GUARD_REWRITE: {
    // rewrite while loops with nondet (extern) function calls in their
    // guards: "while(nondet())" --> "{tmp = nondet(); while(tmp-->0)}"
    rewriter.setActiveTransformer("NondetLoopGuardRewriter");
    NondetLoopGuardRewriter externLoopRewriter(rewriter, Ctx);
    break;
  }
  case Stage::CONTRACT_ANNOTATE: {
    // add contract annotations to recursive functions
    rewriter.setActiveTransformer("RecursiveFunAnnotator");
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
    break;
  }
  case Stage::UNUSED_DECL_REMOVE: {
    // remove all unused declarations
    if (!hadError && !noDeclSlice) {
      rewriter.setActiveTransformer("UnusedDeclCommenter");
      UnusedDeclCommenter declCommenter(rewriter, Ctx, usedFunsAndTypes);
    }
    break;
  }
  case Stage::MAKE_CALLS_UNIQUE: {
    rewriter.setActiveTransformer("UniqueCallSiteTransformer");
    UniqueCallTransformer uniqueCaller(rewriter, Ctx, usedFunsAndTypes);
    uniqueCaller.transform();
    break;
  }
  case Stage::DETERMINIZE: {
    ExecutionCountAnalyzer execAnalyzer(Ctx, usedFunsAndTypes,
                                        entryFunctionName);
    // execAnalyzer.printFrequencies(llvm::outs()); // only for debugging
    rewriter.setActiveTransformer("Determinizer");
    Determinizer determinizer(rewriter, Ctx, execAnalyzer);
    // the added inputs are reported in the facts sidecar
    if (!factsFilename.empty()) {
      state.facts.inputVariables = determinizer.addedInputVariables();
      state.facts.inputArrays = determinizer.addedInputArrays();
    }
    break;
  }
  }
}

void MainConsumer::HandleTranslationUnit(clang::ASTContext& Ctx) {

  // todo: process files where an error has occurred?
  // errors also occur on some inputs which TriCera would accept
  bool hadError = Ctx.getDiagnostics().hasErrorOccurred();
  // C++ slicing is not reliable yet, so keep all declarations for C++
  bool isCXX = Ctx.getLangOpts().CPlusPlus;
  bool collectAllFuns = hadError || noDeclSlice || isCXX;
  bool collectAllTypes = hadError || noDeclSlice || isCXX;

  // collect all used functions and types
  UsedFunAndTypeCollector usedFunsAndTypes(Ctx, collectAllFuns,
                                           collectAllTypes);

  // facts for the facts sidecar; if a round below rewrites the text, the
  // driver collects them again from a parse of the final output
  if (!factsFilename.empty())
    collectFacts(Ctx, state.facts);

  // run rounds on this parse until one rewrites something; the round
  // after that needs a fresh parse of the rewritten text
  size_t r = state.nextRound;
  while (r < state.rounds.size()) {
    size_t editsBefore = state.tracker.size();
    const Round &round = state.rounds[r];
    for (size_t i = 0; i < round.size(); ++i)
      runStage(round[i], Ctx, usedFunsAndTypes, hadError);
    if (state.tracker.size() != editsBefore && r + 1 < state.rounds.size())
      break;
    ++r;
  }
  state.executedThrough = r < state.rounds.size() ? r
                                                  : state.rounds.size() - 1;

  preprocessOutput.usesArrays = 0;
  for (const Type* typ : usedFunsAndTypes.getSeenTypes()) {
    if (typ->isArrayType())
      preprocessOutput.usesArrays = 1;
  }

  // todo: set this to 1 if any unsupported features exist
  preprocessOutput.isUnsupported = 0;
}

MainConsumer::~MainConsumer() {
  // any cleanup?
}

#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "TrackedRewriter.hpp"
#include "FactsCollector.hpp"
#include "TriCeraPreprocessorMain.hpp"

#include <string>
#include <vector>

class UsedFunAndTypeCollector;

// one transformer stage; each round runs on a fresh parse of the previous
// round's text, so rounds cannot conflict
enum class Stage {
  TYPE_CANONISE,
  TYPEDEF_REMOVE,
  CHAR_REWRITE,
  FOR_LOOP_EXTRACT,
  NONDET_LOOP_GUARD_REWRITE,
  CONTRACT_ANNOTATE,
  UNUSED_DECL_REMOVE,
  MAKE_CALLS_UNIQUE,
  DETERMINIZE
};

typedef std::vector<Stage> Round;

// state carried across the parses. a parse executes rounds from nextRound
// until one rewrites something; the next round then needs a fresh parse.
struct StagedState {
  std::vector<Round> rounds;
  size_t nextRound = 0;        // first round of the coming parse
  size_t executedThrough = 0;  // last round index the parse executed
  std::string currentText;     // the text the parse produced
  bool producedOutput = false; // the parse reached the end of its action
  ProgramFacts facts;          // facts collected by the most recent parse
  EditTracker tracker;         // edits made by the most recent parse
  std::string lastOriginal;    // the text the most recent parse was given
};

class MainConsumer : public clang::ASTConsumer {
public:
  MainConsumer(TrackedRewriter &rewriter,
               PreprocessOutput &out,
               StagedState &state) :
               rewriter(rewriter), preprocessOutput(out), state(state) {}
  void HandleTranslationUnit(clang::ASTContext& Ctx) override;
  ~MainConsumer() override;

private:
  void runStage(Stage stage, clang::ASTContext &Ctx,
                UsedFunAndTypeCollector &usedFunsAndTypes, bool hadError);

  TrackedRewriter &rewriter;
  PreprocessOutput &preprocessOutput;
  StagedState &state;
};

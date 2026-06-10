#include "TriceraPreprocessor.hpp"
#include "TriceraConfig.hpp"
#include "LineMarkerEmitter.hpp"
#include "FactsCollector.hpp"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include <unistd.h>
#include <fcntl.h>

#include "llvm/ADT/SmallString.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Error.h"
#include <string.h>
#include <iostream>
#include <signal.h>
#include <regex>

using namespace llvm;
using namespace clang;
using namespace tooling;

//===----------------------------------------------------------------------===//
// Command line options
//===----------------------------------------------------------------------===//
std::string optionsText = "tricera-preprocessor v" TRI_PP_VERSION " options";
static cl::OptionCategory TPCategory(optionsText);
cl::opt<std::string> outputFilename("o",
                                    cl::desc("Specify output absolute path"),
                                    cl::value_desc("output file absolute path"),
                                    cl::cat(TPCategory), cl::init(""));
cl::opt<std::string> entryFunctionName("m",
                                    cl::desc("Specify entry function (default: main)"),
                                    cl::value_desc("entry function name"),
                                    cl::cat(TPCategory), cl::init("main"));
cl::opt<bool> quiet ("q", cl::desc("Suppress error and warning messages"),
                     cl::cat(TPCategory));
cl::opt<bool> dispVer ("v", cl::desc("Display tri-pp version number"),
                     cl::cat(TPCategory));
cl::opt<bool> determinize("determinize",
                     cl::desc("Make non-deterministic programs deterministic"),
                     cl::cat(TPCategory));
cl::opt<bool> makeCallsUnique("make-calls-unique",
                     cl::desc("Ensure each function call site for non-recursive functions "
                                     "invokes a unique function declaration/definition."),
                     cl::cat(TPCategory));
cl::opt<bool> noDeclSlice("no-decl-slice",
                     cl::desc("Disable slicing: keep all declarations (don't comment out unused code)"),
                     cl::cat(TPCategory));
cl::opt<bool> lineMarkers("line-markers",
                     cl::desc("Emit GNU linemarkers (# <line> \"<file>\") mapping "
                              "rewritten output lines back to original input lines"),
                     cl::cat(TPCategory));
cl::opt<std::string> factsFilename("facts",
                     cl::desc("Write facts about the produced program (YAML) "
                              "to the given path"),
                     cl::value_desc("facts file path"),
                     cl::cat(TPCategory), cl::init(""));

//===----------------------------------------------------------------------===//
// PluginASTAction
//===----------------------------------------------------------------------===//
class TriCeraPreprocessAction : public clang::ASTFrontendAction {
public:
  TriCeraPreprocessAction(PreprocessOutput &out, StagedState &state) :
    preprocessOutput(out), state(state) {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    rewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    state.tracker = EditTracker(); // this parse's edits only
    trackedRewriter = std::make_unique<TrackedRewriter>(rewriter,
                                                        state.tracker);
    return std::make_unique<MainConsumer>(*trackedRewriter, preprocessOutput,
                                          state);
  }

  // single-line, length-capped rendering of edit text for diagnostics
  static std::string printableText(const std::string &text) {
    std::string s;
    for (size_t i = 0; i < text.size() && s.size() < 60; ++i) {
      if (text[i] == '\n') s += "\\n";
      else if (text[i] == '\t') s += "\\t";
      else s += text[i];
    }
    if (s.size() >= 60) s += "...";
    return s;
  }

  void describeEdit(const TrackedEdit &e, raw_ostream &os) {
    SourceManager &sm = rewriter.getSourceMgr();
    FileID fid = sm.getMainFileID();
    unsigned line = sm.getLineNumber(fid, e.offset);
    unsigned col = sm.getColumnNumber(fid, e.offset);
    os << "  [" << e.transformer << "] ";
    if (e.isInsertion())
      os << "insertion at line " << line << ":" << col
         << " (offset " << e.offset << ")";
    else
      os << "replacement at line " << line << ":" << col
         << " (offsets " << e.offset << "-" << (e.offset + e.length) << ")";
    os << ": \"" << printableText(e.text) << "\"\n";
  }

  void EndSourceFileAction() override {
    SourceManager &sm = rewriter.getSourceMgr();

    // order-dependent rewrites are a bug; report them, produce no output
    std::vector<std::pair<TrackedEdit, TrackedEdit> > conflicts =
        state.tracker.nonCommutingPairs();
    if (!conflicts.empty()) {
      llvm::errs() << "[tricera-preprocessor] error: " << conflicts.size()
                   << " conflicting rewrite pair(s) detected, output not written\n";
      for (size_t i = 0; i < conflicts.size(); ++i) {
        llvm::errs() << "conflict " << (i + 1) << ":\n";
        describeEdit(conflicts[i].first, llvm::errs());
        describeEdit(conflicts[i].second, llvm::errs());
      }
      preprocessOutput.hasRewriteConflict = true;
      return;
    }

    StringRef original = sm.getBufferData(sm.getMainFileID());
    state.lastOriginal = original.str();
    const RewriteBuffer *Buf = rewriter.getRewriteBufferFor(sm.getMainFileID());
    if (Buf && lineMarkers) {
      // markers of an earlier round (or of cpp) are consumed via the
      // presumed locations and re-emitted fresh
      std::string rewritten(Buf->begin(), Buf->end());
      std::string error;
      if (!emitLineMarkers(original, rewritten, state.tracker, sm,
                           state.currentText, error)) {
        llvm::errs() << "[tricera-preprocessor] error: " << error << "\n";
        preprocessOutput.hasErrorOccurred = true;
        return;
      }
    } else if (Buf) {
      state.currentText.assign(Buf->begin(), Buf->end());
    } else {
      // an unrewritten input passes through verbatim
      state.currentText = original.str();
    }
    state.producedOutput = true;
  }

  PreprocessOutput &preprocessOutput;
private:
  clang::Rewriter rewriter;
  StagedState &state;
  std::unique_ptr<TrackedRewriter> trackedRewriter;
  StringRef inFile;
};

std::unique_ptr<FrontendActionFactory>
newTPFrontendActionFactory(PreprocessOutput &out, StagedState &state) {
  class TPFrontendActionFactory : public FrontendActionFactory {
  public:
    TPFrontendActionFactory(PreprocessOutput &out, StagedState &state) :
      out(out), state(state) {}
    std::unique_ptr<FrontendAction> create() override {
      return std::make_unique<TriCeraPreprocessAction>(out, state);
    }
  private:
    PreprocessOutput &out;
    StagedState &state;
  };

  return std::unique_ptr<FrontendActionFactory>(
                              new TPFrontendActionFactory(out, state));
}

// collects the facts from a parse of the final text; needed when the last
// round rewrote something (the facts must describe the output)
class FactsCollectAction : public clang::ASTFrontendAction {
public:
  FactsCollectAction(ProgramFacts &facts) : facts(facts) {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    class FactsConsumer : public ASTConsumer {
    public:
      FactsConsumer(ProgramFacts &facts) : facts(facts) {}
      void HandleTranslationUnit(clang::ASTContext &Ctx) override {
        collectFacts(Ctx, facts);
      }
    private:
      ProgramFacts &facts;
    };
    return std::make_unique<FactsConsumer>(facts);
  }

private:
  ProgramFacts &facts;
};

std::unique_ptr<FrontendActionFactory>
newFactsActionFactory(ProgramFacts &facts) {
  class FactsActionFactory : public FrontendActionFactory {
  public:
    FactsActionFactory(ProgramFacts &facts) : facts(facts) {}
    std::unique_ptr<FrontendAction> create() override {
      return std::make_unique<FactsCollectAction>(facts);
    }
  private:
    ProgramFacts &facts;
  };

  return std::unique_ptr<FrontendActionFactory>(
                              new FactsActionFactory(facts));
}

// One transformer per round, so the transformers cannot conflict. The
// order matters: unused declarations go first, the later rounds never see
// them; the for-loop and loop-guard rounds leave copies that later rounds
// rewrite as ordinary code; types are canonised before the typedefs are
// removed (TypeCanonise also names anonymous tags, so its output parses
// on its own).
static std::vector<Round> computeRounds() {
  std::vector<Round> rounds;
  if (makeCallsUnique) {
    rounds.push_back(Round{Stage::MAKE_CALLS_UNIQUE});
  } else if (determinize) {
    rounds.push_back(Round{Stage::DETERMINIZE});
  } else {
    rounds.push_back(Round{Stage::UNUSED_DECL_REMOVE});
    rounds.push_back(Round{Stage::FOR_LOOP_EXTRACT});
    rounds.push_back(Round{Stage::NONDET_LOOP_GUARD_REWRITE});
    rounds.push_back(Round{Stage::CHAR_REWRITE});
    rounds.push_back(Round{Stage::TYPE_CANONISE});
    rounds.push_back(Round{Stage::TYPEDEF_REMOVE});
    rounds.push_back(Round{Stage::CONTRACT_ANNOTATE});
  }
  return rounds;
}

constexpr int EXIT_CODE_SUCCESS = 0;
constexpr int EXIT_CODE_SOFT_FAIL = 2; // For parsing errors, file errors, etc.
constexpr int EXIT_CODE_CMD_LINE_ERROR = 3;
constexpr int EXIT_CODE_REWRITE_CONFLICT = 4; // conflicting transformer edits

/// @brief Immediately terminate the process using an exit code that reflects
///        the signal number.
/// @param signal_number The signal that triggered the handler.
void immediate_exit_handler(int signal_number) {
  _exit(128 + signal_number);
}

int main(int argc, const char **argv) {
  signal(SIGABRT, immediate_exit_handler);
  signal(SIGSEGV, immediate_exit_handler);
  signal(SIGILL, immediate_exit_handler);
  signal(SIGFPE, immediate_exit_handler);

  try {
    auto OptionsParser = clang::tooling::CommonOptionsParser::create(argc, argv, TPCategory);
    if (!OptionsParser) {
      logAllUnhandledErrors(OptionsParser.takeError(), llvm::errs(), "[tricera-preprocessor] ");
      _exit(EXIT_CODE_CMD_LINE_ERROR);
    }

    if (argc == 1) { // no arguments are passed, show help
      cl::PrintHelpMessage(false, true);
      return 0;
    }
    CommonOptionsParser &Parser = *OptionsParser;
    if (dispVer)
    {
      outs() << "tricera-preprocessor v" TRI_PP_VERSION << "\n";
      return 0;
    }

    // suppress stderr output
    int fd = -1, n = -1;
    if (quiet) {
      fd = dup(2);
      n = open("/dev/null", O_WRONLY);
      dup2(n, 2);
      close(n);
    }

    PreprocessOutput preprocessOutput = {};
    StagedState state;
    state.rounds = computeRounds();

    SmallString<256> absInputPath(
        StringRef(Parser.getSourcePathList()[0]));
    sys::fs::make_absolute(absInputPath);

    // each parse executes as many rounds as it can, the next parse reads
    // the produced text from memory
    bool parseFailed = false;
    while (true) {
      ClangTool tool(Parser.getCompilations(),
                                    Parser.getSourcePathList());
      if (state.nextRound > 0)
        tool.mapVirtualFile(absInputPath, state.currentText);
      state.producedOutput = false;
      state.executedThrough = state.nextRound - 1; // "no round executed"
      if (tool.run(
              newTPFrontendActionFactory(preprocessOutput, state).get()) != 0)
        parseFailed = true; // tolerated; TriCera accepts more than clang does
      if (preprocessOutput.hasRewriteConflict ||
          preprocessOutput.hasErrorOccurred || !state.producedOutput)
        break;
      if (state.executedThrough + 1 <= state.nextRound) {
        // the parse did not execute its round (e.g. a fatal error skipped
        // the consumer); stop instead of spinning
        parseFailed = true;
        break;
      }
      if (state.executedThrough + 1 >= state.rounds.size())
        break;
      state.nextRound = state.executedThrough + 1;
    }

    // when the last round rewrote nothing, the facts of the last parse
    // already describe the output; otherwise collect them again
    if (!factsFilename.empty() && state.producedOutput &&
        state.currentText != state.lastOriginal) {
      ClangTool factsTool(Parser.getCompilations(),
                                        Parser.getSourcePathList());
      factsTool.mapVirtualFile(absInputPath, state.currentText);
      factsTool.run(newFactsActionFactory(state.facts).get());
    }

    if (quiet) {
      dup2(fd, 2); // restore stderr output
      close(fd);
    }

    if (preprocessOutput.hasRewriteConflict) {
      _exit(EXIT_CODE_REWRITE_CONFLICT);
    }

    if (state.producedOutput) {
      std::unique_ptr<raw_pwrite_stream> outFile;
      if (outputFilename.empty()) {
        outFile = std::make_unique<raw_fd_ostream>(STDOUT_FILENO, false);
      } else {
        std::error_code error_code;
        outFile = std::make_unique<raw_fd_ostream>(outputFilename.c_str(), error_code, sys::fs::OF_None);
        if (error_code) {
          llvm::errs() << "Error opening output file '" << outputFilename << "': " << error_code.message() << "\n";
          _exit(EXIT_CODE_SOFT_FAIL);
        }
      }
      *outFile << state.currentText;

      if (!factsFilename.empty()) {
        std::string factsError;
        if (!writeFacts(factsFilename, state.facts, state.currentText,
                        factsError)) {
          llvm::errs() << "[tricera-preprocessor] error: " << factsError << "\n";
          preprocessOutput.hasErrorOccurred = true;
        }
      }
    }

    if (parseFailed || preprocessOutput.hasErrorOccurred ||
        !state.producedOutput) {
      _exit(EXIT_CODE_SOFT_FAIL);
    }

  } catch (...) {
    abort();
  }

  return EXIT_CODE_SUCCESS;
}

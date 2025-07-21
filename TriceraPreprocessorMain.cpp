#include "TriceraPreprocessor.hpp"
#include "TriceraConfig.hpp"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include <unistd.h>
#include <fcntl.h>

#include "llvm/Support/CommandLine.h"
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
cl::opt<bool> normalize("normalize",
                     cl::desc("Normalize heap access operations to read/write/alloc calls"),
                     cl::cat(TPCategory));
cl::opt<std::string> encode("encode",
                     cl::desc("Encode heap operations using the specified encoding file"),
                     cl::value_desc("encoding_file"),
                     cl::cat(TPCategory));
cl::opt<std::string> backend("backend",
                     cl::desc("Specify backend for the heap encoder (tricera or seahorn)"),
                     cl::value_desc("backend"),
                     cl::cat(TPCategory), cl::init("tricera"));

namespace {
  // Helper function to perform final text replacements on the output string.
  void performFinalReplacements(std::string& code, const std::string& backend) {
    auto replaceAll = [](std::string& str, const std::string& from, const std::string& to) {
      if (from.empty()) return;
      size_t start_pos = 0;
      while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
      }
    };

    replaceAll(code, "NULL", "0");
    if (backend == "seahorn") {
      // matches the whole word "assert" when it is
      // followed by optional whitespace and an opening parenthesis.
      // `\\b` matches "assert" as a whole word".
      // `(?=\\s*\\()` is a positive lookahead that checks for the pattern without including it in the match that gets replaced.
      std::regex assert_regex("\\bassert\\b(?=\\s*\\()");
      code = std::regex_replace(code, assert_regex, "sassert");
      code = "#include \"seahorn/seasynth.h\"\n" + code;
    }
  }
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// PluginASTAction
//===----------------------------------------------------------------------===//
class TriCeraPreprocessAction : public clang::ASTFrontendAction {
public:
  TriCeraPreprocessAction(PreprocessOutput &out) :
    preprocessOutput(out) {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    rewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<MainConsumer>(rewriter, preprocessOutput);
  }

  void EndSourceFileAction() override {
    SourceManager &sm = rewriter.getSourceMgr();
    std::unique_ptr<raw_pwrite_stream> outFile;

    if (outputFilename.empty()) {
      outFile = std::make_unique<raw_fd_ostream>(STDOUT_FILENO, false);
    } else {
      std::error_code error_code;
      outFile = std::make_unique<raw_fd_ostream>(outputFilename.c_str(), error_code, sys::fs::OF_None);
      if (error_code) {
        llvm::errs() << "Error opening output file '" << outputFilename << "': " << error_code.message() << "\n";
        preprocessOutput.hasErrorOccurred = true;
        return;
      }
    }

    const RewriteBuffer *Buf = rewriter.getRewriteBufferFor(sm.getMainFileID());
    if (Buf) {
      std::string finalCode(Buf->begin(), Buf->end());
      performFinalReplacements(finalCode, backend.getValue());
      *outFile << finalCode;
    } else {
      rewriter.getEditBuffer(sm.getMainFileID()).write(*outFile);
    }
  }

  PreprocessOutput &preprocessOutput;
private:
  clang::Rewriter rewriter;
  StringRef inFile;
};

std::unique_ptr<FrontendActionFactory>
newTPFrontendActionFactory(PreprocessOutput &out) {
  class TPFrontendActionFactory : public FrontendActionFactory {
  public:
    TPFrontendActionFactory(PreprocessOutput &out) :
      out(out) {}
    std::unique_ptr<FrontendAction> create() override {
      return std::make_unique<TriCeraPreprocessAction>(out);
    }
  private:
    PreprocessOutput &out;
  };

  return std::unique_ptr<FrontendActionFactory>(
                                                new TPFrontendActionFactory(out));
}

constexpr int EXIT_CODE_SUCCESS = 0;
constexpr int EXIT_CODE_SOFT_FAIL = 2; // For parsing errors, file errors, etc.
constexpr int EXIT_CODE_CMD_LINE_ERROR = 3;

/// @brief Immediately terminates the process using an exit code that reflects
///        the signal number, preserving shell error-checking conventions.
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
    ClangTool tool(Parser.getCompilations(),
                                  Parser.getSourcePathList());

    // suppress stderr output
    int fd = -1, n = -1;
    if (quiet) {
      fd = dup(2);
      n = open("/dev/null", O_WRONLY);
      dup2(n, 2);
      close(n);
    }

    PreprocessOutput preprocessOutput;
    int tool_result = tool.run(newTPFrontendActionFactory(preprocessOutput).get());

    if (quiet) {
      dup2(fd, 2); // restore stderr output
      close(fd);
    }

    if (tool_result != 0 || preprocessOutput.hasErrorOccurred) {
      _exit(EXIT_CODE_SOFT_FAIL);
    }

  } catch (...) {
    abort();
  }

  return EXIT_CODE_SUCCESS;
}
#include "TriceraPreprocessor.hpp"
#include "TriceraConfig.hpp"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include <unistd.h>
#include <fcntl.h>

#include "llvm/Support/CommandLine.h"
#include <string.h>
#include <iostream>

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

    rewriter.getEditBuffer(sm.getMainFileID()).write(*outFile);
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

int main(int argc, const char **argv) {
  auto OptionsParser = clang::tooling::CommonOptionsParser::create(argc, argv, TPCategory);
  if (argc == 1) { // no arguments are passed, show help
    cl::PrintHelpMessage(false, true);
    return 0;
  }
  clang::tooling::CommonOptionsParser &Parser = *OptionsParser;
  if (dispVer)
  {
    llvm::outs() << "tricera-preprocessor v" TRI_PP_VERSION << "\n";
    return 0;
  }
  clang::tooling::ClangTool tool(Parser.getCompilations(),
                                 Parser.getSourcePathList());

  // suppress stderr output
  int fd, n;
  if (quiet) {
    fd = dup(2);
    n = open("/dev/null", O_WRONLY);
    dup2(n, 2);
    close(n);
  }

  PreprocessOutput preprocessOutput;
  preprocessOutput.hasErrorOccurred =
    tool.run(newTPFrontendActionFactory(preprocessOutput).get()) != 0;

  if (quiet) {
    dup2(fd, 2); // restore stderr output
    close(fd);
  }

  return preprocessOutput.hasErrorOccurred;
}

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
                                    cl::desc("Specify output absolute path (required)"),
                                    cl::value_desc("output file absolute path"),
                                    cl::cat(TPCategory), cl::Required);
cl::opt<std::string> entryFunctionName("m",
                                    cl::desc("Specify entry function (default: main)"),
                                    cl::value_desc("entry function name"),
                                    cl::cat(TPCategory), cl::init("main"));
cl::opt<bool> quiet ("q", cl::desc("Suppress error and warning messages"),
                     cl::cat(TPCategory));
cl::opt<bool> dispVer ("v", cl::desc("Display tri-pp version number"),
                     cl::cat(TPCategory));


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
    std::error_code error_code;
    raw_fd_ostream outFile(outputFilename.c_str(), error_code, sys::fs::OF_None);
    rewriter.getEditBuffer(sm.getMainFileID()).write(outFile);
    outFile.close();
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
  if (!OptionsParser) {
    llvm::errs() << "Warning: Failed to create CommonOptionsParser. Proceeding without a compilation database.\n";
  }
  clang::tooling::CommonOptionsParser &Parser = *OptionsParser;
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

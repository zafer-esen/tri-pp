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

#include <fstream>

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
cl::opt<std::string> cxxYamlOutput("cxx-yaml",
                     cl::desc("Specify path for C++ semantic extraction YAML output"),
                     cl::value_desc("YAML file path"),
                     cl::cat(TPCategory), cl::init(""));

//===----------------------------------------------------------------------===//
// Frontend Actions for multi-pass
//===----------------------------------------------------------------------===//

class TemplateExpansionAction : public clang::ASTFrontendAction {
public:
  TemplateExpansionAction(std::string &ResultBuffer, SemanticAnalysisResult &Result) 
      : ResultBuffer(ResultBuffer), Result(Result) {}
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override {
    rewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<TemplateExpansionConsumer>(rewriter, Result);
  }
  void EndSourceFileAction() override {
    SourceManager &sm = rewriter.getSourceMgr();
    const RewriteBuffer *Buf = rewriter.getRewriteBufferFor(sm.getMainFileID());
    if (Buf) {
      ResultBuffer.clear();
      llvm::raw_string_ostream OS(ResultBuffer);
      Buf->write(OS);
      OS.flush();
    }
  }
private:
  clang::Rewriter rewriter;
  std::string &ResultBuffer;
  SemanticAnalysisResult &Result;
};

class MainPreprocessAction : public clang::ASTFrontendAction {
public:
  MainPreprocessAction(std::string &ResultBuffer, PreprocessOutput &out) 
      : ResultBuffer(ResultBuffer), preprocessOutput(out) {}
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override {
    rewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<MainConsumer>(rewriter, preprocessOutput);
  }
  void EndSourceFileAction() override {
    SourceManager &sm = rewriter.getSourceMgr();
    const RewriteBuffer *Buf = rewriter.getRewriteBufferFor(sm.getMainFileID());
    if (Buf) {
      ResultBuffer.clear();
      llvm::raw_string_ostream OS(ResultBuffer);
      Buf->write(OS);
      OS.flush();
    }
  }
private:
  clang::Rewriter rewriter;
  std::string &ResultBuffer;
  PreprocessOutput &preprocessOutput;
};

class CXXInfoExtractionAction : public clang::ASTFrontendAction {
public:
  CXXInfoExtractionAction(const std::string &yamlOutput, SemanticAnalysisResult &Result) 
      : yamlOutput(yamlOutput), Result(Result) {}
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override {
    rewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<CXXInfoExtractionConsumer>(rewriter, yamlOutput, Result);
  }
  void EndSourceFileAction() override {} // Read-only
private:
  clang::Rewriter rewriter;
  std::string yamlOutput;
  SemanticAnalysisResult &Result;
};

// FrontendActionFactories
class TemplateExpansionFactory : public FrontendActionFactory {
  std::string &ResultBuffer;
  SemanticAnalysisResult &Result;
public:
  TemplateExpansionFactory(std::string &RB, SemanticAnalysisResult &Res) 
      : ResultBuffer(RB), Result(Res) {}
  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<TemplateExpansionAction>(ResultBuffer, Result);
  }
};

class MainPreprocessFactory : public FrontendActionFactory {
  std::string &ResultBuffer;
  PreprocessOutput &out;
public:
  MainPreprocessFactory(std::string &RB, PreprocessOutput &out) : ResultBuffer(RB), out(out) {}
  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<MainPreprocessAction>(ResultBuffer, out);
  }
};

class CXXInfoExtractionFactory : public FrontendActionFactory {
  std::string yamlOutput;
  SemanticAnalysisResult &Result;
public:
  CXXInfoExtractionFactory(const std::string &YO, SemanticAnalysisResult &Res) 
      : yamlOutput(YO), Result(Res) {}
  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<CXXInfoExtractionAction>(yamlOutput, Result);
  }
};

constexpr int EXIT_CODE_SUCCESS = 0;
constexpr int EXIT_CODE_SOFT_FAIL = 2; // For parsing errors, file errors, etc.
constexpr int EXIT_CODE_CMD_LINE_ERROR = 3;

/// @brief Immediately terminate the process using an exit code that reflects
///        the signal number.
/// @param signal_number The signal that triggered the handler.
void immediate_exit_handler(int signal_number) {
  _exit(128 + signal_number);
}

#include "clang/Basic/Diagnostic.h"

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
    if (dispVer) {
      outs() << "tricera-preprocessor v" TRI_PP_VERSION << "\n";
      return 0;
    }

    std::vector<std::string> Sources = Parser.getSourcePathList();
    if (Sources.empty()) return 0;
    std::string PrimarySource = Sources[0];

    // Read initial source code
    auto BufferOrErr = llvm::MemoryBuffer::getFile(PrimarySource);
    if (!BufferOrErr) {
      llvm::errs() << "Error opening input file '" << PrimarySource << "': " 
                   << BufferOrErr.getError().message() << "\n";
      _exit(EXIT_CODE_SOFT_FAIL);
    }
    std::string CurrentCode = BufferOrErr.get()->getBuffer().str();

    bool isCXX = PrimarySource.find(".cpp") != std::string::npos || 
                 PrimarySource.find(".hpp") != std::string::npos ||
                 PrimarySource.find(".cc") != std::string::npos;

    PreprocessOutput preprocessOutput;
    SemanticAnalysisResult semanticAnalysisResult;
    IgnoringDiagConsumer Ignoring;
    
    // Pass 1: C++ Template Expansion
    if (isCXX) {
      std::string ExpandedCode;
      ClangTool pass1Tool(Parser.getCompilations(), Sources);
      if (quiet) pass1Tool.setDiagnosticConsumer(&Ignoring);
      pass1Tool.mapVirtualFile(PrimarySource, CurrentCode);
      TemplateExpansionFactory Factory(ExpandedCode, semanticAnalysisResult);
      if (pass1Tool.run(&Factory) != 0) {
        _exit(EXIT_CODE_SOFT_FAIL);
      }
      if (!ExpandedCode.empty()) CurrentCode = ExpandedCode;
    }

    // Pass 2: Standard Preprocessors
    std::string FinalCode;
    ClangTool pass2Tool(Parser.getCompilations(), Sources);
    if (quiet) pass2Tool.setDiagnosticConsumer(&Ignoring);
    pass2Tool.mapVirtualFile(PrimarySource, CurrentCode);
    MainPreprocessFactory Pass2Factory(FinalCode, preprocessOutput);
    if (pass2Tool.run(&Pass2Factory) != 0) {
      _exit(EXIT_CODE_SOFT_FAIL);
    }
    if (!FinalCode.empty()) CurrentCode = FinalCode;

    // Pass 3: C++ Semantic Extraction (on the final result)
    if (isCXX && !cxxYamlOutput.empty()) {
      ClangTool pass3Tool(Parser.getCompilations(), Sources);
      if (quiet) pass3Tool.setDiagnosticConsumer(&Ignoring);
      pass3Tool.mapVirtualFile(PrimarySource, CurrentCode);
      CXXInfoExtractionFactory Pass3Factory(cxxYamlOutput.getValue(), semanticAnalysisResult);
      pass3Tool.run(&Pass3Factory);
    }

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
    *outFile << CurrentCode;

  } catch (...) {
    abort();
  }

  return EXIT_CODE_SUCCESS;
}

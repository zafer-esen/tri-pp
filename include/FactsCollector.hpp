// A pass to create a YAML file with facts about the final preprocessed program.

#pragma once

#include "clang/AST/ASTContext.h"
#include "llvm/ADT/StringRef.h"

#include <set>
#include <string>
#include <vector>

// a synthesized name and the original name it stands for
// e.g. "Pair_int_" for "Pair<int>"
struct MangledName {
  std::string mangled;
  std::string original;
};

struct ProgramFacts {
  // names of the allocation functions the program references
  std::set<std::string> allocationFunctions;
  // some declaration has an array type
  bool usesArrays = false;
  // some declaration has an array type of unknown bound
  bool usesUnboundedArrays = false;
  // the program contains a throw expression / a try-catch block
  bool usesThrow = false;
  bool usesTryCatch = false;
  // input variables and input arrays added by the determinizer
  std::vector<std::string> inputVariables;
  std::vector<std::string> inputArrays;
  // mangled names from C++ template instantiation
  std::vector<MangledName> mangledNames;
};

// collects the facts above from the AST
void collectFacts(clang::ASTContext &Ctx, ProgramFacts &facts);

// writes the facts file. `finalText` is the produced output
bool writeFacts(llvm::StringRef path, const ProgramFacts &facts,
                llvm::StringRef finalText, std::string &error);

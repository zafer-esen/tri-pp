// A pass to create a YAML file with facts about the final preprocessed program.

#pragma once

#include "clang/AST/ASTContext.h"
#include "llvm/ADT/StringRef.h"

#include <set>
#include <string>
#include <vector>

struct ProgramFacts {
  // names of the allocation functions the program references
  std::set<std::string> allocationFunctions;
  // some declaration has an array type
  bool usesArrays = false;
  // some declaration has an array type of unknown bound
  bool usesUnboundedArrays = false;
  // input variables and input arrays added by the determinizer
  std::vector<std::string> inputVariables;
  std::vector<std::string> inputArrays;
};

// collects the facts above from the AST
void collectFacts(clang::ASTContext &Ctx, ProgramFacts &facts);

// writes the facts file. `finalText` is the produced output
bool writeFacts(llvm::StringRef path, const ProgramFacts &facts,
                llvm::StringRef finalText, std::string &error);

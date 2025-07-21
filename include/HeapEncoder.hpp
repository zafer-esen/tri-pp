#pragma once

#include "clang/Rewrite/Core/Rewriter.h"
#include <string>
#include <vector>
#include <map>

#include "clang/AST/ASTContext.h"

enum class Backend { TriCera, SeaHorn };

struct ProgramInfo {
    bool hintFound = false;
    std::string heapType;
    std::string heapTypeName;
    std::vector<std::string> stackPtrTypes;
    std::vector<std::string> inputs;
};

struct EncodingInfo {
    std::string ptrType = "";
    std::string initBlock;
    std::string globalDecls;
    std::map<std::string, std::string> predicates; // Maps predicate name to its signature, e.g., "R" -> "R(int, int, ...)"
    std::string readFn;
    std::string writeFn;
    std::string allocFn;
    bool loaded = false;
};

class HeapEncoder {
public:
    HeapEncoder(clang::Rewriter &R, clang::ASTContext &Ctx,
                const std::string &encodingFile, Backend backend);
};
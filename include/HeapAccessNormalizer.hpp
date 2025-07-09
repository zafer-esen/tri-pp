#pragma once

#include <memory>

namespace clang {
    class Rewriter;
    class ASTContext;
}

class HeapAccessNormalizerImpl;

class HeapAccessNormalizer {
public:
    HeapAccessNormalizer(clang::Rewriter &R, clang::ASTContext &Ctx);
    ~HeapAccessNormalizer();
private:
    std::unique_ptr<HeapAccessNormalizerImpl> Impl;
};
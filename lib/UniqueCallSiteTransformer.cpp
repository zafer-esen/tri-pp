#include "UniqueCallSiteTransformer.hpp"
#include "clang/Lex/Lexer.h"
#include "llvm/ADT/Optional.h"

#include <algorithm>

using clang::CallExpr;
using clang::FunctionDecl;
using clang::RecursiveASTVisitor;
using clang::SourceLocation;
using clang::SourceManager;
using clang::SourceRange;
using clang::LangOptions;
using clang::Token;

static const std::set<std::string> functionsToNotClone = {
  // reach_error etc. we do not need to clone
  "reach_error", "__assert_fail", "malloc", "calloc", "realloc", "free",
  "myexit", "assume_abort_if_not", "abort", "assert", "assume", "printf"
};

static bool isLeafFunction(const FunctionDecl *FD) {
  if (!FD->hasBody()) {
    return false;
  }

  class NonIgnoredCallFinder : public RecursiveASTVisitor<NonIgnoredCallFinder> {
    bool hasNonIgnoredCall = false;
  public:
    bool VisitCallExpr(const CallExpr *CE) {
      const FunctionDecl* calleeFD = CE->getDirectCallee();

      if (!calleeFD) {
        hasNonIgnoredCall = true;
        return false;
      }

      if (functionsToNotClone.count(calleeFD->getNameAsString())) {
        return true;
      }

      hasNonIgnoredCall = true;
      return false;
    }

    bool getHasNonIgnoredCall() const { return hasNonIgnoredCall; }
  };

  NonIgnoredCallFinder finder;
  finder.TraverseStmt(FD->getBody());

  return !finder.getHasNonIgnoredCall();
}

UniqueCallTransformer::UniqueCallTransformer(
    clang::Rewriter &R, clang::ASTContext &Ctx, const UsedFunAndTypeCollector &U)
    : rewriter(R), Ctx(Ctx), usedFunsAndTypes(U) {}

void UniqueCallTransformer::transform() {
  // Pass 1: Collect all direct call sites.
  class SiteCollectorVisitor : public RecursiveASTVisitor<SiteCollectorVisitor> {
  public:
    explicit SiteCollectorVisitor(
        llvm::DenseMap<const FunctionDecl *, std::vector<const CallExpr *>> &sites)
        : allCallSites(sites) {}
    bool VisitCallExpr(const CallExpr *CE) {
      if (const FunctionDecl* FD = CE->getDirectCallee()) {
        allCallSites[FD->getCanonicalDecl()].push_back(CE);
      }
      return true;
    }
  private:
    llvm::DenseMap<const FunctionDecl *, std::vector<const CallExpr *>> &allCallSites;
  };

  SiteCollectorVisitor siteCollector(allCallSites);
  siteCollector.TraverseDecl(Ctx.getTranslationUnitDecl());

  // Pass 2: Iterate over functions called more than once.
  for (auto const& pair : allCallSites) {
    const auto& FD = pair.first;
    const auto& sites = pair.second;
    auto funInfo = usedFunsAndTypes.getFunctionInfo(FD);
    if ((funInfo && funInfo->isRecursive()) || sites.size() <= 1) {
      continue;
    }

    if (FD->getReturnType()->isVoidType() || isLeafFunction(FD) ||
      functionsToNotClone.count(FD->getNameAsString())) {
      continue;
    }

    std::vector<const CallExpr*> sortedSites = sites;
    std::sort(sortedSites.begin(), sortedSites.end(),
        [&](const CallExpr* a, const CallExpr* b) {
            return a->getBeginLoc() < b->getBeginLoc();
    });

    for (size_t i = 1; i < sortedSites.size(); ++i) {
      const CallExpr *callToRewrite = sortedSites[i];
      cloneCache.clear();
      std::string newFuncName = getOrCreateClone(FD->getCanonicalDecl());
      rewriter.ReplaceText(callToRewrite->getCallee()->getSourceRange(), newFuncName);
    }
  }
}

std::string UniqueCallTransformer::getOrCreateClone(const FunctionDecl *FD) {
  FD = FD->getCanonicalDecl();

  if (cloneCache.count(FD)) {
    return cloneCache[FD];
  }

  unsigned copyNum = ++globalCloneCounters[FD];
  std::string newName = FD->getNameAsString() + "_copy_" + std::to_string(copyNum);

  cloneCache[FD] = newName;

  const FunctionDecl *declToClone = FD->getDefinition() ? FD->getDefinition() : FD;

  SourceManager &SM = Ctx.getSourceManager();
  const LangOptions &LangOpts = Ctx.getLangOpts();

  SourceLocation startLoc = declToClone->getBeginLoc();
  SourceLocation endLoc = declToClone->getEndLoc();

  if (!declToClone->hasBody()) {
    llvm::Optional<Token> nextToken = clang::Lexer::findNextToken(endLoc, SM, LangOpts);
    if (nextToken && nextToken->is(clang::tok::semi)) {
      endLoc = nextToken->getLocation();
    }
  }

  SourceLocation realEndLoc = clang::Lexer::getLocForEndOfToken(endLoc, 0, SM, LangOpts);
  SourceRange rangeToCopy(startLoc, realEndLoc);

  std::string originalFuncText = clang::Lexer::getSourceText(
      clang::CharSourceRange::getCharRange(rangeToCopy), SM, LangOpts).str();

  std::vector<const CallExpr *> innerCalls;
  class CallExprVisitor : public RecursiveASTVisitor<CallExprVisitor> {
    std::vector<const CallExpr *> &innerCalls;
  public:
    explicit CallExprVisitor(std::vector<const CallExpr *> &calls) : innerCalls(calls) {}
    bool VisitCallExpr(const CallExpr *CE) {
        if (CE->getDirectCallee()) { innerCalls.push_back(CE); }
        return true;
    }
  };
  CallExprVisitor visitor(innerCalls);
  if (declToClone->hasBody()) {
    visitor.TraverseStmt(declToClone->getBody());
  }

  std::sort(innerCalls.begin(), innerCalls.end(),
            [&](const CallExpr *a, const CallExpr *b) {
              return a->getBeginLoc() > b->getBeginLoc();
            });

  for (const auto *innerCE : innerCalls) {
    const FunctionDecl* innerCallee = innerCE->getDirectCallee()->getCanonicalDecl();
    auto funInfo = usedFunsAndTypes.getFunctionInfo(innerCallee);
    if (funInfo && funInfo->isRecursive()) continue;

    if (allCallSites.count(innerCallee) && allCallSites[innerCallee].size() > 1) {
      if (innerCallee->getReturnType()->isVoidType() || isLeafFunction(innerCallee) ||
          functionsToNotClone.count(innerCallee->getNameAsString())) {
        continue;
      }

      std::string newInnerName = getOrCreateClone(innerCallee);
      SourceRange range = innerCE->getCallee()->getSourceRange();

      unsigned offset = SM.getDecomposedLoc(range.getBegin()).second -
                        SM.getDecomposedLoc(rangeToCopy.getBegin()).second;
      unsigned len = clang::Lexer::MeasureTokenLength(range.getBegin(), SM, LangOpts);
      originalFuncText.replace(offset, len, newInnerName);
    }
  }

  SourceRange nameRange = declToClone->getNameInfo().getSourceRange();
  unsigned nameOffset = SM.getDecomposedLoc(nameRange.getBegin()).second -
                        SM.getDecomposedLoc(rangeToCopy.getBegin()).second;
  unsigned nameLen = clang::Lexer::MeasureTokenLength(nameRange.getBegin(), SM, LangOpts);
  originalFuncText.replace(nameOffset, nameLen, newName);

  if (!declToClone->hasBody() && !llvm::StringRef(originalFuncText).startswith("extern ")) {
      originalFuncText.insert(0, "extern ");
  }

  const FunctionDecl *mostRecentDecl = FD->getMostRecentDecl();
  SourceLocation insertPos;
  SourceRange mostRecentRange = mostRecentDecl->getSourceRange();
  if (!mostRecentDecl->hasBody()) {
      llvm::Optional<Token> nextToken = clang::Lexer::findNextToken(mostRecentRange.getEnd(), SM, LangOpts);
      if (nextToken && nextToken->is(clang::tok::semi)) {
          insertPos = nextToken->getEndLoc();
      } else {
          insertPos = clang::Lexer::getLocForEndOfToken(mostRecentRange.getEnd(), 0, SM, LangOpts);
      }
  } else {
      insertPos = clang::Lexer::getLocForEndOfToken(mostRecentDecl->getEndLoc(), 0, SM, LangOpts);
  }

  rewriter.InsertText(insertPos, "\n\n" + originalFuncText);

  return newName;
}
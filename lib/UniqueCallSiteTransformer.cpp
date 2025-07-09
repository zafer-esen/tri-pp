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
    // Find the semicolon and update the end location.
    llvm::Optional<Token> nextToken = clang::Lexer::findNextToken(endLoc, SM, LangOpts);
    if (nextToken && nextToken->is(clang::tok::semi)) {
      endLoc = nextToken->getLocation(); // Get start of the semi token
    }
  }
  // For a function with a body, declToClone->getEndLoc() correctly points to the '}'.

  SourceLocation realEndLoc = clang::Lexer::getLocForEndOfToken(endLoc, 0, SM, LangOpts);
  SourceRange fullDeclRange(startLoc, realEndLoc);

  std::string originalFuncText = clang::Lexer::getSourceText(
      clang::CharSourceRange::getCharRange(fullDeclRange), SM, LangOpts).str();


  std::vector<const CallExpr *> innerCalls;
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

    std::string newInnerName = getOrCreateClone(innerCallee);
    SourceRange range = innerCE->getCallee()->getSourceRange();

    unsigned offset = SM.getDecomposedLoc(range.getBegin()).second -
                      SM.getDecomposedLoc(fullDeclRange.getBegin()).second;
    unsigned len = clang::Lexer::MeasureTokenLength(range.getBegin(), SM, LangOpts);
    originalFuncText.replace(offset, len, newInnerName);
  }

  SourceRange nameRange = declToClone->getNameInfo().getSourceRange();
  unsigned nameOffset = SM.getDecomposedLoc(nameRange.getBegin()).second -
                        SM.getDecomposedLoc(fullDeclRange.getBegin()).second;
  unsigned nameLen = clang::Lexer::MeasureTokenLength(nameRange.getBegin(), SM, LangOpts);
  originalFuncText.replace(nameOffset, nameLen, newName);

  const FunctionDecl *mostRecentDecl = FD->getMostRecentDecl();
  SourceLocation insertPos;

  if (mostRecentDecl->hasBody()) {
    insertPos = clang::Lexer::getLocForEndOfToken(mostRecentDecl->getEndLoc(), 0, SM, LangOpts);
  } else {
    llvm::Optional<Token> nextToken = clang::Lexer::findNextToken(mostRecentDecl->getEndLoc(), SM, LangOpts);
    if (nextToken && nextToken->is(clang::tok::semi)) {
        insertPos = nextToken->getEndLoc();
    } else {
        insertPos = clang::Lexer::getLocForEndOfToken(mostRecentDecl->getEndLoc(), 0, SM, LangOpts);
    }
  }

  rewriter.InsertText(insertPos, "\n\n" + originalFuncText);

  return newName;
}
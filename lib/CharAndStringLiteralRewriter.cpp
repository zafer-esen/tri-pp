#include "TypeCanoniser.hpp"
#include "Utilities.hpp"
#include "CharAndStringLiteralRewriter.hpp"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/Expr.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"

#include "llvm/Support/raw_ostream.h"
#include "string"


using namespace clang;
using namespace ast_matchers;
using namespace llvm;

CharAndStringLiteralRewriter::CharAndStringLiteralRewriter(clang::Rewriter &r, clang::ASTContext &Ctx,
                                                           UsedFunAndTypeCollector &usedFunsAndTypes) {
  CharAndStringLiteralRewriterASTConsumer c(r, usedFunsAndTypes);
  c.HandleTranslationUnit(Ctx);
}

CharAndStringLiteralRewriterASTConsumer::CharAndStringLiteralRewriterASTConsumer(clang::Rewriter &r,
                                                                                 UsedFunAndTypeCollector &usedFunsAndTypes)
    : rewriter(r) {
  handler = std::make_unique<CharAndStringLiteralRewriterMatcher>(rewriter, usedFunsAndTypes);

  // char literals
  StatementMatcher charLitStmtMatcher = characterLiteral().bind("charLitExpr");
  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource,
                             charLitStmtMatcher), handler.get());

  //string literals
  StatementMatcher stringLitMatcher = stringLiteral().bind("strLitExpr");
  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, stringLitMatcher), handler.get());
}

std::string CharAndStringLiteralRewriterMatcher::escapeCString(StringRef str) {
  std::string result = "\"";
  for (unsigned char c : str) {
    switch (c) {
      case '"':  result += "\\\""; break;
      case '\\': result += "\\\\"; break;
      case '\n': result += "\\n"; break;
      case '\t': result += "\\t"; break;
      case '\r': result += "\\r"; break;
      default:   result += c; break;
    }
  }
  result += "\"";
  return result;
}

void CharAndStringLiteralRewriterMatcher::run(const MatchFinder::MatchResult &Result) {
  const auto *charLit =
      Result.Nodes.getNodeAs<clang::CharacterLiteral>("charLitExpr");
  const auto *strLit = Result.Nodes.getNodeAs<clang::StringLiteral>("strLitExpr");

  if (charLit) {
    wrapWithCComment(charLit->getSourceRange(), rewriter, true, false);
    rewriter.InsertTextBefore(charLit->getBeginLoc(), std::to_string(charLit->getValue()));
  }
  else if (strLit) {
    if (strLit->getNumConcatenated() > 1) {
      StringRef combinedValue = strLit->getBytes();
      std::string newLiteral = escapeCString(combinedValue);
      rewriter.ReplaceText(strLit->getSourceRange(), newLiteral);
    }
  }
}

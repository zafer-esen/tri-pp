#include "TypeCanoniser.hpp"
#include "Utilities.hpp"
#include "CharRewriter.hpp"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "llvm/Support/raw_ostream.h"
#include "string"


using namespace clang;
using namespace ast_matchers;
using namespace llvm;

CharRewriter::CharRewriter(clang::Rewriter &r, clang::ASTContext &Ctx, 
                      UsedFunAndTypeCollector &usedFunsAndTypes) {
    CharRewriterASTConsumer c(r, usedFunsAndTypes);
    c.HandleTranslationUnit(Ctx);
}

CharRewriterASTConsumer::CharRewriterASTConsumer(clang::Rewriter &r,
                                     UsedFunAndTypeCollector &usedFunsAndTypes)
                           : rewriter(r) {
  handler = std::make_unique<CharRewriterMatcher>(rewriter, usedFunsAndTypes);
  StatementMatcher charLitStmtMatcher = characterLiteral().bind("charLitExpr");

  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, 
                             charLitStmtMatcher), handler.get());
}

void CharRewriterMatcher::run(const MatchFinder::MatchResult &Result) {
  ASTContext *Ctx = Result.Context;

  const CharacterLiteral * charLit = 
    Result.Nodes.getNodeAs<clang::CharacterLiteral>("charLitExpr"); 

  if (charLit) 
  {
    // comment out the original literal
    wrapWithCComment(charLit->getSourceRange(), rewriter, true, false);
    // add the integer value of the literal
    rewriter.InsertTextBefore(charLit->getBeginLoc(),
                 std::to_string(charLit->getValue())
    );
    
  } else {
    llvm_unreachable("CharRewriter unreachable case\n");
  }
}

#include "DesignatedInitializerEliminator.hpp"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/Lex/Lexer.h"

#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace ast_matchers;
using namespace llvm;

DesignatedInitializerEliminatorASTConsumer::DesignatedInitializerEliminatorASTConsumer(clang::Rewriter &r) {
  handler = std::make_unique<DesignatedInitializerEliminatorMatcher>(r);

  StatementMatcher designatedInitMatcher =
      initListExpr(
          has(designatedInitExpr())
      ).bind("initListWithDesignated");

  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, designatedInitMatcher), handler.get());
}

void DesignatedInitializerEliminatorASTConsumer::HandleTranslationUnit(clang::ASTContext &Ctx) {
  finder.matchAST(Ctx);
}

DesignatedInitializerEliminatorMatcher::DesignatedInitializerEliminatorMatcher(clang::Rewriter &r)
    : rewriter(r) {}

void DesignatedInitializerEliminatorMatcher::run(const MatchFinder::MatchResult &Result) {
  ASTContext *Ctx = Result.Context;
  const InitListExpr *ile = Result.Nodes.getNodeAs<InitListExpr>("initListWithDesignated");

  if (!ile) {
    return;
  }

  // the initializer as written in the source
  const InitListExpr *syntacticILE = ile->getSyntacticForm();

  if (!syntacticILE || !editedLocations.insert(syntacticILE->getBeginLoc()).second) {
    return;
  }

  std::string newInitializer = "{ ";
  bool first = true;

  // the semantic form has the correct positional order
  for (unsigned i = 0; i < ile->getNumInits(); ++i) {
    if (!first) {
      newInitializer += ", ";
    }
    first = false;

    const Expr *init = ile->getInit(i);

    if (isa<ImplicitValueInitExpr>(init)) {
      // for implicit initializers (e.g., zero-initialization), use a default value based on type.
      QualType type = init->getType();
      if (type->isAggregateType() || type->isRecordType()) {
        newInitializer += "{}";
      } else {
        newInitializer += "0";
      }
    } else {
      // use explicit initializers as-is
      SourceManager &sm = Ctx->getSourceManager();
      LangOptions lo = Ctx->getLangOpts();
      SourceRange initRange = init->getSourceRange();
      StringRef text = Lexer::getSourceText(CharSourceRange::getTokenRange(initRange), sm, lo);
      newInitializer += text.str();
    }
  }
  newInitializer += " }";

  rewriter.ReplaceText(syntacticILE->getSourceRange(), newInitializer);
}

DesignatedInitializerEliminator::DesignatedInitializerEliminator(clang::Rewriter &r, clang::ASTContext &Ctx) {
  DesignatedInitializerEliminatorASTConsumer consumer(r);
  consumer.HandleTranslationUnit(Ctx);
}

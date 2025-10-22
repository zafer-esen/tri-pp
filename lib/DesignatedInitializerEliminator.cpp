#include "DesignatedInitializerEliminator.hpp"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/Lex/Lexer.h"

#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace ast_matchers;
using namespace llvm;

static std::string buildPositionalInitializer(const InitListExpr *ile, ASTContext *Ctx) {
    std::string newInitializer = "{ ";
    bool first = true;

    for (const Expr *init : ile->inits()) {
        if (!first) {
            newInitializer += ", ";
        }
        first = false;

        if (const auto *nestedILE = dyn_cast<InitListExpr>(init->IgnoreParenImpCasts())) {
            newInitializer += buildPositionalInitializer(nestedILE, Ctx);
        }
        else if (isa<ImplicitValueInitExpr>(init)) {
            QualType type = init->getType();
            if (type->isAggregateType() || type->isRecordType()) {
                newInitializer += "{}";
            } else {
                newInitializer += "0";
            }
        }
        else {
            SourceManager &sm = Ctx->getSourceManager();
            LangOptions lo = Ctx->getLangOpts();
            SourceRange initRange = init->getSourceRange();
            StringRef text = Lexer::getSourceText(CharSourceRange::getTokenRange(initRange), sm, lo);
            newInitializer += text.str();
        }
    }
    newInitializer += " }";
    return newInitializer;
}


DesignatedInitializerEliminatorASTConsumer::DesignatedInitializerEliminatorASTConsumer(clang::Rewriter &r) {
  handler = std::make_unique<DesignatedInitializerEliminatorMatcher>(r);

  DeclarationMatcher designatedInitMatcher =
      varDecl(
          hasInitializer(
              initListExpr(
                  hasDescendant(designatedInitExpr())
              )
          )
      ).bind("topLevelVarDecl");

  finder.addMatcher(traverse(TK_IgnoreUnlessSpelledInSource, designatedInitMatcher), handler.get());
}

void DesignatedInitializerEliminatorASTConsumer::HandleTranslationUnit(clang::ASTContext &Ctx) {
  finder.matchAST(Ctx);
}

DesignatedInitializerEliminatorMatcher::DesignatedInitializerEliminatorMatcher(clang::Rewriter &r)
    : rewriter(r) {}

void DesignatedInitializerEliminatorMatcher::run(const MatchFinder::MatchResult &Result) {
  ASTContext *Ctx = Result.Context;
  const VarDecl *vd = Result.Nodes.getNodeAs<VarDecl>("topLevelVarDecl");

  if (!vd) {
    return;
  }

  const Expr *initializer = vd->getInit();
  const InitListExpr *ile = cast<InitListExpr>(initializer);

  const InitListExpr *syntacticILE = ile->getSyntacticForm();

  if (!syntacticILE || !editedLocations.insert(syntacticILE->getBeginLoc()).second) {
    return;
  }

  SourceManager &sm = Ctx->getSourceManager();
  LangOptions lo = Ctx->getLangOpts();

  SourceRange originalRange = syntacticILE->getSourceRange();
  StringRef originalText = Lexer::getSourceText(CharSourceRange::getTokenRange(originalRange), sm, lo);

  std::string newInitializer = buildPositionalInitializer(ile, Ctx);
  std::string replacementText = "/* " + originalText.str() + " */ " + newInitializer;
  rewriter.ReplaceText(originalRange, replacementText);
}

DesignatedInitializerEliminator::DesignatedInitializerEliminator(clang::Rewriter &r, clang::ASTContext &Ctx) {
    DesignatedInitializerEliminatorASTConsumer consumer(r);
    consumer.HandleTranslationUnit(Ctx);
}

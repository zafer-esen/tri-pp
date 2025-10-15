#include "FuncPtrDevirtualizer.hpp"
#include "Utilities.hpp"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/STLExtras.h"

using namespace clang;
using namespace ast_matchers;

extern llvm::cl::opt<bool> debug;

static const FunctionDecl *getAssignedFunction(const Expr *E) {
    if (!E) return nullptr;
    const Expr *e = E->IgnoreParenImpCasts();
    if (const auto *DRE = dyn_cast<DeclRefExpr>(e)) {
        if (const auto *FD = dyn_cast<FunctionDecl>(DRE->getDecl())) {
            return FD;
        }
    }
    return nullptr;
}

class FuncPtrDevirtualizer::AssignmentCollector
        : public RecursiveASTVisitor<AssignmentCollector> {
public:
    AssignmentCollector(
            std::map<const VarDecl *, SmallVector<const FunctionDecl *, 2>> &A)
            : assignments(A) {}

    bool VisitVarDecl(VarDecl *D) {
        if (D && D->getType()->isFunctionPointerType()) {
            if (D->hasInit()) {
                if (const FunctionDecl *target = getAssignedFunction(D->getInit())) {
                    assignments[D->getCanonicalDecl()].push_back(target);
                }
            }
        }
        return true;
    }

    bool VisitBinaryOperator(BinaryOperator *O) {
        if (O && O->getOpcode() == BO_Assign) {
            if (const auto *DRE = dyn_cast<DeclRefExpr>(O->getLHS()->IgnoreParenImpCasts())) {
                if (const auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
                    if (VD->getType()->isFunctionPointerType()) {
                        if (const FunctionDecl *target = getAssignedFunction(O->getRHS())) {
                            assignments[VD->getCanonicalDecl()].push_back(target);
                        }
                    }
                }
            }
        }
        return true;
    }

private:
    std::map<const VarDecl *, SmallVector<const FunctionDecl *, 2>> &assignments;
};

class FuncPtrDevirtualizer::DevirtualizeMatcher
        : public MatchFinder::MatchCallback {
public:
    DevirtualizeMatcher(
            std::map<const VarDecl *, const FunctionDecl *> &Targets,
            Rewriter &R, ASTContext &Ctx)
            : devirtualizedTargets(Targets), rewriter(R), context(Ctx) {}

    void run(const MatchFinder::MatchResult &Result) override {
        const auto *funcPtrVar = Result.Nodes.getNodeAs<VarDecl>("funcPtrVar");
        if (!funcPtrVar) return;

        auto it = devirtualizedTargets.find(funcPtrVar->getCanonicalDecl());
        if (it == devirtualizedTargets.end()) {
            return;
        }
        const FunctionDecl *targetFunc = it->second;

        if (debug) {
            llvm::dbgs() << "[Devirtualizer] Matched safe pointer '" << funcPtrVar->getName() << "' for rewriting.\n";
        }

        if (const auto *call = Result.Nodes.getNodeAs<CallExpr>("callExpr")) {
            if (debug) {
                llvm::dbgs() << "  -> Rewriting CallExpr at: ";
                call->getBeginLoc().print(llvm::dbgs(), context.getSourceManager());
                llvm::dbgs() << "\n";
            }
            const Expr *callee = call->getCallee();
            std::string originalText = Lexer::getSourceText(
                    CharSourceRange::getTokenRange(callee->getSourceRange()),
                    *Result.SourceManager, context.getLangOpts()).str();

            std::string replacement = "/*" + originalText + "*/ " + targetFunc->getNameAsString();
            rewriter.ReplaceText(callee->getSourceRange(), replacement);
        }
        else if (const auto *assignmentOp = Result.Nodes.getNodeAs<BinaryOperator>("assignment")) {
            if (debug) {
                llvm::dbgs() << "  -> Commenting out Assignment at: ";
                assignmentOp->getBeginLoc().print(llvm::dbgs(), context.getSourceManager());
                llvm::dbgs() << "\n";
            }
            rewriter.InsertText(assignmentOp->getBeginLoc(), "// ");
        }
        else if (const auto* decl = Result.Nodes.getNodeAs<VarDecl>("varDecl")) {
            if (debug) {
                llvm::dbgs() << "  -> Commenting out VarDecl at: ";
                decl->getBeginLoc().print(llvm::dbgs(), context.getSourceManager());
                llvm::dbgs() << "\n";
            }
            doubleSlashCommentOutDeclaration(funcPtrVar, context, rewriter);
        }
    }

private:
    std::map<const VarDecl *, const FunctionDecl *> &devirtualizedTargets;
    Rewriter &rewriter;
    ASTContext &context;
};

FuncPtrDevirtualizer::FuncPtrDevirtualizer(Rewriter &R, ASTContext &Ctx)
        : rewriter(R), context(Ctx) {

    AssignmentCollector collector(assignments);
    collector.TraverseDecl(context.getTranslationUnitDecl());

    for (const auto &pair : assignments) {
        const VarDecl *funcPtrVar = pair.first;
        const SmallVector<const FunctionDecl *, 2> &targets = pair.second;

        if (targets.empty()) continue;

        const FunctionDecl *firstTarget = targets[0];
        bool allSame = llvm::all_of(targets, [&](const FunctionDecl *fd) {
            return fd->getCanonicalDecl() == firstTarget->getCanonicalDecl();
        });

        if (allSame) {
            devirtualizedTargets[funcPtrVar] = firstTarget;
        }
    }

    if (!devirtualizedTargets.empty()) {
        if (debug) {
            llvm::dbgs() << "[Devirtualizer] Found " << devirtualizedTargets.size() << " safe function pointers to devirtualize.\n";
        }

        DevirtualizeMatcher handler(devirtualizedTargets, rewriter, context);
        MatchFinder finder;

        auto funcPtrVarMatcher = varDecl(
                hasType(pointerType(pointee(ignoringParens(functionType()))))
        ).bind("funcPtrVar");

        finder.addMatcher(
                callExpr(
                        hasDescendant(declRefExpr(to(funcPtrVarMatcher)))
                ).bind("callExpr"),
                &handler
        );

        finder.addMatcher(
                binaryOperator(
                        hasOperatorName("="),
                        hasLHS(ignoringParenImpCasts(declRefExpr(to(funcPtrVarMatcher))))
                ).bind("assignment"),
                &handler
        );

        finder.addMatcher(
                decl(
                        allOf(
                                varDecl(isDefinition()).bind("varDecl"),
                                funcPtrVarMatcher
                        )
                ),
                &handler
        );

        finder.matchAST(context);
    }
}

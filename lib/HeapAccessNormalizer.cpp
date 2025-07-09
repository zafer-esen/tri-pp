#include "HeapAccessNormalizer.hpp"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include <set>
#include <string>
#include <vector>

using namespace clang;

class HeapAccessNormalizerImpl {
private:
    Rewriter &TheRewriter;
    ASTContext &Context;
    bool hasMadeChanges = false;
    int tempVarCounter = 1;

    std::string heapType; // Initially empty
    bool heapTypeConflict = false;

    // Pass 1: Finds all potential heap access nodes.
    class HeapAccessLocator : public RecursiveASTVisitor<HeapAccessLocator> {
    public:
        std::vector<const BinaryOperator *> writes;
        std::vector<const MemberExpr *> arrowExprs;
        std::vector<const UnaryOperator *> derefExprs;

        // Find potential writes (p->f = ..., *p = ...)
        bool VisitBinaryOperator(BinaryOperator *BO) {
            if (BO->isAssignmentOp()) {
                Expr *lhs = BO->getLHS()->IgnoreParenImpCasts();
                if ((isa<MemberExpr>(lhs) && dyn_cast<MemberExpr>(lhs)->isArrow()) ||
                    (isa<UnaryOperator>(lhs) && dyn_cast<UnaryOperator>(lhs)->getOpcode() == UO_Deref)) {
                    writes.push_back(BO);
                }
            }
            return true; // continue traversal to find nested accesses.
        }

        // Find ALL arrow expressions (p->f)
        bool VisitMemberExpr(MemberExpr *ME) {
            if (ME->isArrow()) {
                arrowExprs.push_back(ME);
            }
            return true;
        }

        // Find ALL dereference expressions (*p)
        bool VisitUnaryOperator(UnaryOperator *UO) {
            if (UO->getOpcode() == UO_Deref) {
                derefExprs.push_back(UO);
            }
            return true;
        }
    };

    std::string getSourceText(const Expr *E) const {
        return Lexer::getSourceText(CharSourceRange::getTokenRange(E->getSourceRange()),
            TheRewriter.getSourceMgr(), TheRewriter.getLangOpts()).str();
    }

    void analyzeHeapType(QualType deducedType) {
        if (deducedType.isNull() || deducedType->isIncompleteType()) return;
        hasMadeChanges = true;
        std::string typeStr = deducedType.getCanonicalType().getAsString();
        if (heapType.empty()) heapType = typeStr;
        else if (heapType != typeStr) heapTypeConflict = true;
    }

    void removeStdlibInclude() {
        SourceManager &SM = TheRewriter.getSourceMgr();
        FileID mainFileID = SM.getMainFileID();
        StringRef fileBuffer = SM.getBufferData(mainFileID);
        const std::string includeStr = "#include <stdlib.h>";
        size_t includePos = fileBuffer.find(includeStr);
        if (includePos == StringRef::npos) return;
        SourceLocation loc = SM.getLocForStartOfFile(mainFileID).getLocWithOffset(includePos);
        unsigned len = includeStr.length();
        if (includePos + len < fileBuffer.size()) {
            if (fileBuffer.substr(includePos + len, 2) == "\r\n") len += 2;
            else if (fileBuffer[includePos + len] == '\n') len += 1;
        }
        TheRewriter.RemoveText(loc, len);
    }

public:
    HeapAccessNormalizerImpl(Rewriter &R, ASTContext &Ctx) : TheRewriter(R), Context(Ctx) {}

    void normalize() {
        class MallocVisitor : public RecursiveASTVisitor<MallocVisitor> {
        public:
            MallocVisitor(Rewriter &R, bool &c) : R(R), changes(c) {}
            bool VisitCallExpr(CallExpr *CE) {
                if (auto *callee = CE->getDirectCallee()) {
                    if (callee->getNameAsString() == "malloc") {
                        R.ReplaceText(CE->getSourceRange(), "alloc()");
                        changes = true;
                    }
                }
                return true;
            }
        private:
            Rewriter &R; bool &changes;
        };
        MallocVisitor(TheRewriter, hasMadeChanges).TraverseDecl(Context.getTranslationUnitDecl());

        HeapAccessLocator locator;
        locator.TraverseDecl(Context.getTranslationUnitDecl());

        std::set<const Expr *> rewrittenLHS;

        for (const auto *BO : locator.writes) {
            Expr *lhs = BO->getLHS()->IgnoreParenImpCasts();
            rewrittenLHS.insert(lhs);

            if (const auto *ME = dyn_cast<MemberExpr>(lhs)) {
                QualType type = ME->getBase()->getType()->getPointeeType();
                analyzeHeapType(type);
                std::string ptr = getSourceText(ME->getBase());
                std::string rhs = getSourceText(BO->getRHS());
                std::string field = ME->getMemberDecl()->getNameAsString();
                std::string temp = "__heap_rd_" + std::to_string(tempVarCounter++);
                std::string replacement = "{\n    " + type.getAsString() + " " + temp + " = read(" + ptr + ");\n"
                                          + "    " + temp + "." + field + " = " + rhs + ";\n"
                                          + "    write(" + ptr + ", " + temp + ");\n  }";
                TheRewriter.ReplaceText(BO->getSourceRange(), replacement);
            } else if (const auto *UO = dyn_cast<UnaryOperator>(lhs)) {
                analyzeHeapType(UO->getSubExpr()->getType()->getPointeeType());
                TheRewriter.ReplaceText(BO->getSourceRange(), "write(" +
                    getSourceText(UO->getSubExpr()) + ", " + getSourceText(BO->getRHS()) + ")");
            }
        }

        // rewrite any remaining arrow/deref expressions as reads.
        for (const auto *ME : locator.arrowExprs) {
            if (rewrittenLHS.count(ME)) continue; // Skip if it was the LHS of a write.
            analyzeHeapType(ME->getBase()->getType()->getPointeeType());
            std::string ptr = getSourceText(ME->getBase());
            std::string field = ME->getMemberDecl()->getNameAsString();
            TheRewriter.ReplaceText(ME->getSourceRange(), "read(" + ptr + ")." + field);
        }
        for (const auto *UO : locator.derefExprs) {
            if (rewrittenLHS.count(UO)) continue; // Skip if it was the LHS of a write.
            analyzeHeapType(UO->getSubExpr()->getType()->getPointeeType());
            TheRewriter.ReplaceText(UO->getSourceRange(), "read(" + getSourceText(UO->getSubExpr()) + ")");
        }

        if (heapTypeConflict) { llvm::errs() << "ERROR: Multiple different object types on heap.\n"; return; }
        if (hasMadeChanges && heapType.empty()) { llvm::errs() << "ERROR: Heap ops found, but could not deduce type.\n"; return; }
        if (hasMadeChanges) {
            removeStdlibInclude();
            std::string hint = "/*\n  HEAP_TYPE: " + heapType + "\n*/\n\n";
            TheRewriter.InsertText(Context.getSourceManager().getLocForStartOfFile(Context.getSourceManager().getMainFileID()), hint, true, true);
        }
    }
};

HeapAccessNormalizer::HeapAccessNormalizer(Rewriter &R, ASTContext &Ctx) : Impl(std::make_unique<HeapAccessNormalizerImpl>(R, Ctx)) {
    Impl->normalize();
}

HeapAccessNormalizer::~HeapAccessNormalizer() = default;
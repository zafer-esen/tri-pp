#include "Determinizer.hpp"
#include <sstream>
#include "clang/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"

Determinizer::Determinizer(clang::Rewriter &R, clang::ASTContext &Ctx,
                           const ExecutionCountAnalyzer &Analyzer)
    : TheRewriter(R), Context(Ctx), ExecAnalyzer(Analyzer) {
    run();
}

void Determinizer::run() {
    // 1. Create and run the visitor.
    DeterminizerVisitor visitor(*this, Context);
    visitor.TraverseDecl(Context.getTranslationUnitDecl());

    // 2. After the visitor runs, inject the collected code blocks.
    if (mainFunc && mainFunc->hasBody() && !mainInitializations.empty()) {
        clang::Stmt* body = mainFunc->getBody();
        clang::SourceLocation insertLoc = body->getBeginLoc().getLocWithOffset(1);
        std::string finalInits = "\n" + mainInitializations + "\n";
        TheRewriter.InsertText(insertLoc, finalInits, true, true);
    }

    if (!globalDeclarations.empty() || !headerInputNames.empty()) {
        clang::SourceManager &SM = Context.getSourceManager();
        clang::SourceLocation topOfFile = SM.getLocForStartOfFile(SM.getMainFileID());

        if (!headerInputNames.empty()) {
            std::stringstream ss;
            ss << "/*\n  INPUT: ";
            for (size_t i = 0; i < headerInputNames.size(); ++i) {
                ss << headerInputNames[i] << (i == headerInputNames.size() - 1 ? "" : ", ");
            }
            ss << "\n*/\n";
            TheRewriter.InsertText(topOfFile, ss.str(), true, true);
        }

        // Inject globals first.
        if (!globalDeclarations.empty()) {
            // Add newlines for spacing.
            std::string globalsToInsert = "\n" + globalDeclarations + "\n";
            TheRewriter.InsertText(topOfFile, globalsToInsert, true, true);
        }
    }
}


//===----------------------------------------------------------------------===//
// DeterminizerVisitor Implementation
//===----------------------------------------------------------------------===//

DeterminizerVisitor::DeterminizerVisitor(Determinizer &Det, clang::ASTContext &Ctx)
    : D(Det), Context(Ctx) {}

bool DeterminizerVisitor::VisitFunctionDecl(clang::FunctionDecl *F) {
    if (F->isMain()) {
        D.mainFunc = F;
    }
    return true;
}

static const std::set<std::string> functionsToIgnore = {
    "reach_error", "__assert_fail", "malloc", "calloc", "realloc", "free",
    "myexit", "assume_abort_if_not", "abort", "assert", "assume", "printf"
};

bool DeterminizerVisitor::isNondetCall(const clang::CallExpr* call) {
    const clang::FunctionDecl* callee = call->getDirectCallee();
    if (!callee) return false;

    if (callee->getReturnType()->isVoidType()) {
        return false;
    }

    if (functionsToIgnore.count(callee->getNameAsString())) {
        return false;
    }

    const clang::FunctionDecl* canonicalDecl = callee->getCanonicalDecl();

    if (canonicalDecl->getStorageClass() != clang::SC_Extern) {
        return false;
    }

    if (Context.getSourceManager().isInSystemHeader(canonicalDecl->getLocation())) {
        return false;
    }

    return true;
}

bool DeterminizerVisitor::VisitCallExpr(clang::CallExpr *call) {
    if (!isNondetCall(call)) {
        return true;
    }

    ExecutionFrequency freq = D.ExecAnalyzer.getFrequency(call);

    const clang::FunctionDecl* callee = call->getDirectCallee();
    std::string returnTypeStr = callee->getReturnType().getAsString();
    std::string calleeName = callee->getNameAsString();

    if (freq == ExecutionFrequency::ONCE) {
        // --- Scalar Input Case ---
        std::string baseName = "IN" + std::to_string(D.inputCounter++);
        std::string globalVar = baseName + "_g";
        std::string localVar = baseName;

        D.headerInputNames.push_back(globalVar);
        D.globalDeclarations += returnTypeStr + " " + globalVar + ";\n";
        D.mainInitializations += "  " + returnTypeStr + " " + localVar + " = " + calleeName + "();\n";
        D.mainInitializations += "  " + globalVar + " = " + localVar + ";\n";
        D.TheRewriter.ReplaceText(call->getSourceRange(), globalVar);

    } else { // freq == ExecutionFrequency::MANY
        // --- Deterministic Havoc Case ---
        if (returnTypeStr == "int") {
            if (!D.dethavocIntFunctionsInjected) {
                D.dethavocIntFunctionsInjected = true;
                std::string inputVar = "IN_DETHAVOC_g";
                D.headerInputNames.push_back(inputVar);
                D.globalDeclarations += "int " + inputVar + ";\n\n";

                D.globalDeclarations += "/* Deterministic havoc functions */\n";
                D.globalDeclarations += "int __dethavoc_int_01() {\n";
                D.globalDeclarations += "  int result = " + inputVar + " % 2;\n";
                D.globalDeclarations += "  " + inputVar + " /= 2;\n";
                D.globalDeclarations += "  return result;\n";
                D.globalDeclarations += "}\n\n";

                D.globalDeclarations += "int __dethavoc_int() {\n";
                D.globalDeclarations += "  int result = 0;\n";
                D.globalDeclarations += "  while (__dethavoc_int_01())\n";
                D.globalDeclarations += "    result = 2 * result + __dethavoc_int_01();\n";
                D.globalDeclarations += "  return result;\n";
                D.globalDeclarations += "}\n";
            }
            D.TheRewriter.ReplaceText(call->getSourceRange(), "__dethavoc_int()");
        } else {
            clang::FullSourceLoc fullLoc(call->getBeginLoc(), Context.getSourceManager());
            llvm::errs() << "ERROR: Deterministic havoc for functions called multiple times is only supported for 'int' return types.\n";
            llvm::errs() << "Unsupported call to '" << calleeName << "' with return type '" << returnTypeStr << "' found at ";
            if (fullLoc.isValid()) {
                llvm::errs() << fullLoc.getSpellingLineNumber() << ":" << fullLoc.getSpellingColumnNumber();
            } else {
                llvm::errs() << "(unknown location)";
            }
            llvm::errs() << ".\n";
            exit(1);
        }
    }

    return true;
}
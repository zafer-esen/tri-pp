#include "Determinizer.hpp"
#include <sstream>
#include "clang/Basic/SourceManager.h"

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

    // The function must be declared 'extern'.
    if (canonicalDecl->getStorageClass() != clang::SC_Extern) {
        return false;
    }

    // It must not be a system library call.
    if (Context.getSourceManager().isInSystemHeader(canonicalDecl->getLocation())) {
        return false;
    }

    return true;
}

bool DeterminizerVisitor::VisitCallExpr(clang::CallExpr *call) {
    if (!isNondetCall(call)) {
        return true;
    }

    // Query the analyzer for the execution frequency of this specific call site.
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
        //D.mainInitializations += "  " + returnTypeStr + " " + localVar + " = " + calleeName + "();\n";
        //D.mainInitializations += "  " + globalVar + " = " + localVar + ";\n";
        D.TheRewriter.ReplaceText(call->getSourceRange(), globalVar);

    } else { // freq == ExecutionFrequency::MANY
        // --- Array Input Case ---
        UnboundedInputInfo names;
        auto it = D.typeToArrayInfoMap.find(returnTypeStr);
        if (it == D.typeToArrayInfoMap.end()) {
            std::string baseName = "IN_ARR_" + std::to_string(D.inputCounter++);
            names.globalArrayName = baseName + "_g";
            names.localArrayName = baseName;
            names.indexName = baseName + "_idx_g";

            D.typeToArrayInfoMap[returnTypeStr] = names;
            D.headerInputNames.push_back(names.globalArrayName);

            D.globalDeclarations += "" + returnTypeStr + " " + names.globalArrayName + "[];\n";
            D.globalDeclarations += "int " + names.indexName + " = 0;\n";
            // D.mainInitializations += "  " + returnTypeStr + " " + names.localArrayName + "[] = _; // only supported by TriCera using -mathArrays\n";
            // D.mainInitializations += "  " + names.globalArrayName + " = " + names.localArrayName + ";\n";
        } else {
            names = it->second;
        }
        D.TheRewriter.ReplaceText(call->getSourceRange(), names.globalArrayName + "[" + names.indexName + "++]");
    }

    return true;
}
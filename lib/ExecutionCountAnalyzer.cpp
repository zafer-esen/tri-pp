#include "ExecutionCountAnalyzer.hpp"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

class StmtFrequencyVisitor : public RecursiveASTVisitor<StmtFrequencyVisitor> {
public:
  StmtFrequencyVisitor(ExecutionCountAnalyzer &Analyzer, ExecutionFrequency F)
      : Analyzer(Analyzer), CurrentFrequency(F) {}

  bool TraverseStmt(Stmt *S) {
    if (!S) {
      return true;
    }

    ExecutionFrequency OldFreq = Analyzer.StmtFrequencies.count(S)
                                   ? Analyzer.StmtFrequencies[S]
                                   : ExecutionFrequency::ONCE;
    ExecutionFrequency NewFreq = combine(OldFreq, CurrentFrequency);
    Analyzer.StmtFrequencies[S] = NewFreq;

    if (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S)) {
      StmtFrequencyVisitor loop_visitor(Analyzer, ExecutionFrequency::MANY);
      return loop_visitor.RecursiveASTVisitor<StmtFrequencyVisitor>::TraverseStmt(S);
    }

    return RecursiveASTVisitor<StmtFrequencyVisitor>::TraverseStmt(S);
  }

  bool VisitCallExpr(CallExpr *CE) {
    const FunctionDecl *CalleeDecl = CE->getDirectCallee();
    if (!CalleeDecl || !CalleeDecl->hasBody()) {
      return true;
    }

    auto it = Analyzer.FunctionFrequencies.find(CalleeDecl);
    bool wasSeen = (it != Analyzer.FunctionFrequencies.end());

    ExecutionFrequency OldFunFreq = wasSeen ? it->second : ExecutionFrequency::ONCE;

    ExecutionFrequency callSiteFrequency = CurrentFrequency;
    if (Analyzer.usedFunsAndTypes.functionIsRecursive(CalleeDecl)) {
        callSiteFrequency = ExecutionFrequency::MANY;
    }

    ExecutionFrequency NewFunFreq = combine(OldFunFreq, callSiteFrequency);

    if (!wasSeen || NewFunFreq != OldFunFreq) {
      Analyzer.FunctionFrequencies[CalleeDecl] = NewFunFreq;
      Analyzer.Worklist.push_back(CalleeDecl);
    }

    return true;
  }

private:
  ExecutionCountAnalyzer &Analyzer;
  ExecutionFrequency CurrentFrequency;
};


// The main entry point for the analysis.
ExecutionCountAnalyzer::ExecutionCountAnalyzer(
    ASTContext &Ctx, const UsedFunAndTypeCollector& usedFunsAndTypes, const std::string& entryFunction)
    : Ctx(Ctx), usedFunsAndTypes(usedFunsAndTypes) {
  TranslationUnitDecl *TU = Ctx.getTranslationUnitDecl();
  const FunctionDecl *MainFunc = nullptr;
  for (auto *D : TU->decls()) {
    if (auto *FD = dyn_cast<FunctionDecl>(D)) {
      if (FD->getNameAsString() == entryFunction && FD->hasBody()) {
        MainFunc = FD;
        break;
      }
    }
  }

  if (!MainFunc) {
    return; // No entry point found.
  }

  FunctionFrequencies[MainFunc] = ExecutionFrequency::ONCE;
  Worklist.push_back(MainFunc);
  run();
}

void ExecutionCountAnalyzer::run() {
  while (!Worklist.empty()) {
    const FunctionDecl *FD = Worklist.back();
    Worklist.pop_back();
    analyzeFunction(FD);
  }
}

void ExecutionCountAnalyzer::analyzeFunction(const clang::FunctionDecl *FD) {
  if (!FD->hasBody()) return;

  ExecutionFrequency Freq = FunctionFrequencies[FD];

  StmtFrequencyVisitor Visitor(*this, Freq);
  Visitor.TraverseDecl(const_cast<FunctionDecl*>(FD));
}

ExecutionFrequency ExecutionCountAnalyzer::getFrequency(const Stmt *S) const {
  auto It = StmtFrequencies.find(S);
  if (It != StmtFrequencies.end()) {
    return It->second;
  }
  // Default for statements not found
  return ExecutionFrequency::ONCE;
}

class ExecutionCountAnalyzer::FrequencyPrinterVisitor
    : public RecursiveASTVisitor<ExecutionCountAnalyzer::FrequencyPrinterVisitor> {
public:
  FrequencyPrinterVisitor(const ExecutionCountAnalyzer& analyzer,
                          llvm::raw_ostream& OS)
      : analyzer(analyzer),
        OS(OS),
        sm(analyzer.Ctx.getSourceManager()),
        langOpts(analyzer.Ctx.getLangOpts()) {}

  bool VisitStmt(Stmt* S) {
    if (!sm.isInMainFile(S->getBeginLoc()) || S->getBeginLoc().isInvalid()) {
      return true;
    }
    auto freq = analyzer.getFrequency(S);
    std::string freqStr = (freq == ExecutionFrequency::MANY) ? "MANY" : "ONCE";
    auto range = CharSourceRange::getTokenRange(S->getSourceRange());
    StringRef sourceText = Lexer::getSourceText(range, sm, langOpts);

    OS << S->getBeginLoc().printToString(sm) << " [" << freqStr
             << "]: " << sourceText.trim() << "\n";
    return true;
  }

private:
  const ExecutionCountAnalyzer& analyzer;
  llvm::raw_ostream& OS;
  SourceManager& sm;
  const LangOptions& langOpts;
};

void ExecutionCountAnalyzer::printFrequencies(llvm::raw_ostream& OS) const {
  OS << "--- Execution Frequency Analysis Results ---\n";
  FrequencyPrinterVisitor printer(*this, OS);
  printer.TraverseDecl(Ctx.getTranslationUnitDecl());
  OS << "--- End of Execution Frequency Analysis ---\n";
}

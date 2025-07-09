#include "HeapEncoder.hpp"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include <fstream>
#include <regex>
#include <sstream>
#include <map>

using namespace clang;

namespace {

void replaceAll(std::string& str, const std::string& from, const std::string& to) {
    if (from.empty()) return;
    size_t start_pos = 0;
    while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
}

ProgramInfo parseProgramHints(ASTContext &Ctx) {
    ProgramInfo info;
    SourceManager &SM = Ctx.getSourceManager();
    FileID mainFileID = SM.getMainFileID();
    if (mainFileID.isInvalid()) return info;
    StringRef fileContent = SM.getBufferData(mainFileID);
    std::string contentStr = fileContent.str();
    std::regex heapRegex(R"(HEAP_TYPE:\s*([^\r\n]*))"), inputRegex(R"(INPUT:\s*([^\r\n]*))");
    std::smatch match;
    if (std::regex_search(contentStr, match, heapRegex) && match.size() > 1) {
        info.heapType = match[1].str();
        info.heapType.erase(info.heapType.find_last_not_of(" \t") + 1);
        info.hintFound = true;
    }
    if (std::regex_search(contentStr, match, inputRegex) && match.size() > 1) {
        std::stringstream ss(match[1].str());
        std::string input;
        while (std::getline(ss, input, ',')) {
            input.erase(0, input.find_first_not_of(" \t"));
            input.erase(input.find_last_not_of(" \t") + 1);
            if (!input.empty()) info.inputs.push_back(input);
        }
    }
    return info;
}

EncodingInfo parseEncodingFile(const std::string& filename) {
    EncodingInfo info;
    std::ifstream file(filename);
    if (!file.is_open()) return info;

    enum class State { None, PtrType, Init, GlobalDecls, Predicate, ReadFn, WriteFn, AllocFn };
    std::map<std::string, State> tagMap = {
        {"@@PTR_TYPE", State::PtrType},
        {"@@INIT", State::Init},
        {"@@GLOBAL_DECLS", State::GlobalDecls},
        {"@@PREDICATE", State::Predicate},
        {"@@READ_FN", State::ReadFn},
        {"@@WRITE_FN", State::WriteFn},
        {"@@ALLOC_FN", State::AllocFn}
    };

    State currentState = State::None;
    std::stringstream currentContent;
    bool predicateTagFound = false;
    std::string line;

    auto commitContent = [&](State s) {
        if (s == State::None) return;
        std::string content = currentContent.str();

        // **FIX**: Robustly trim all leading/trailing whitespace, including newlines.
        size_t first = content.find_first_not_of(" \t\r\n");
        if (std::string::npos == first) {
            content = "";
        } else {
            size_t last = content.find_last_not_of(" \t\r\n");
            content = content.substr(first, (last - first + 1));
        }

        switch (s) {
            case State::PtrType:     info.ptrType = content; break;
            case State::Init:        info.initBlock = content; break;
            case State::GlobalDecls: info.globalDecls = content; break;
            case State::ReadFn:      info.readFn = content; break;
            case State::WriteFn:     info.writeFn = content; break;
            case State::AllocFn:     info.allocFn = content; break;
            default: break;
        }
        currentContent.str("");
        currentContent.clear();
    };

    while (std::getline(file, line)) {
        std::string trimmed_line = line;
        trimmed_line.erase(0, line.find_first_not_of(" \t\r\n"));
        trimmed_line.erase(trimmed_line.find_last_not_of(" \t\r\n") + 1);

        auto it = tagMap.find(trimmed_line);
        if (it != tagMap.end()) {
            commitContent(currentState);
            currentState = it->second;
            if (currentState == State::Predicate) {
                predicateTagFound = true;
            }
        } else if (predicateTagFound) {
            size_t name_start = 0;
            while(name_start < trimmed_line.length() && isspace(trimmed_line[name_start])) name_start++;
            size_t sig_start = trimmed_line.find('(');
            if(sig_start != std::string::npos) {
                std::string name = trimmed_line.substr(name_start, sig_start - name_start);
                name.erase(name.find_last_not_of(" \t") + 1);
                info.predicates[name] = trimmed_line;
            }
            predicateTagFound = false;
        } else {
            if (currentState != State::None) {
                currentContent << line << "\n";
            }
        }
    }
    commitContent(currentState);

    info.loaded = true;
    return info;
}

class NondetFinderVisitor : public RecursiveASTVisitor<NondetFinderVisitor> {
public:
    explicit NondetFinderVisitor(ASTContext &Ctx, const ProgramInfo& PInfo) : Context(Ctx), pInfo(PInfo), SM(Ctx.getSourceManager()) {}
    bool VisitFunctionDecl(FunctionDecl* FD) { if (!SM.isInMainFile(FD->getLocation())) return true; if (intNondetFnName.empty() && isSuitableIntNondet(FD)) intNondetFnName = FD->getNameAsString(); if (heapNondetFnName.empty() && isSuitableHeapNondet(FD)) heapNondetFnName = FD->getNameAsString(); return true; }
    std::string getIntFnName() const { return intNondetFnName; }
    std::string getHeapFnName() const { return heapNondetFnName; }
private:
    bool isSuitableIntNondet(FunctionDecl* FD) { return FD->getStorageClass() == SC_Extern && FD->getNumParams() == 0 && FD->getReturnType()->isIntegerType(); }
    bool isSuitableHeapNondet(FunctionDecl* FD) { return FD->getStorageClass() == SC_Extern && FD->getNumParams() == 0 && FD->getReturnType().getAsString() == pInfo.heapType; }
    ASTContext &Context; const ProgramInfo& pInfo; SourceManager &SM; std::string intNondetFnName, heapNondetFnName;
};

class InjectionPointVisitor : public RecursiveASTVisitor<InjectionPointVisitor> {
public:
    explicit InjectionPointVisitor(ASTContext &Ctx, const ProgramInfo& PInfo) : Context(Ctx), pInfo(PInfo) {}
    bool VisitFunctionDecl(FunctionDecl* FD) { if (FD->isMain() && FD->hasBody()) mainFuncBody = FD->getBody(); return true; }
    bool VisitVarDecl(VarDecl *D) { if (D->hasGlobalStorage()) for(const auto& n : pInfo.inputs) if(D->getNameAsString() == n) updateInjectionPoint(D->getEndLoc()); return true; }
    bool VisitTagDecl(TagDecl *D) { if (D->isThisDeclarationADefinition()) { std::string n=D->getNameAsString(); if(!n.empty() && pInfo.heapType.find(n) != std::string::npos) updateInjectionPoint(D->getEndLoc()); } return true; }
    SourceLocation getInjectionPoint() const { if (injectionPoint.isValid()) return Lexer::findLocationAfterToken(injectionPoint, tok::semi, Context.getSourceManager(), Context.getLangOpts(), false); return Context.getSourceManager().getLocForStartOfFile(Context.getSourceManager().getMainFileID()); }
    Stmt* getMainBody() const { return mainFuncBody; }
private:
    void updateInjectionPoint(SourceLocation newLoc) { if (injectionPoint.isInvalid() || Context.getSourceManager().isBeforeInTranslationUnit(injectionPoint, newLoc)) injectionPoint = newLoc; }
    ASTContext &Context; const ProgramInfo& pInfo; SourceLocation injectionPoint; Stmt* mainFuncBody = nullptr;
};

class PointerRewriterVisitor : public RecursiveASTVisitor<PointerRewriterVisitor> {
public:
    explicit PointerRewriterVisitor(Rewriter &R, const std::string& ptrTypeStr)
      : TheRewriter(R), ptrType(ptrTypeStr) {}

    bool VisitVarDecl(VarDecl* D) {
        // FieldDecls and ParmVarDecls are handled by their own more specific visitors below.
        if (isa<FieldDecl>(D) || isa<ParmVarDecl>(D)) {
            return true;
        }

        if (D->getType()->isPointerType()) {
            rewrite(D->getTypeSourceInfo());
        }
        return true;
    }

    bool VisitFieldDecl(FieldDecl* FD) {
        if (FD->getType()->isPointerType()) {
            rewrite(FD->getTypeSourceInfo());
        }
        return true;
    }

    // Handles function parameters, with an exception for main().
    bool VisitParmVarDecl(ParmVarDecl* PVD) {
        if (auto* FD = dyn_cast<FunctionDecl>(PVD->getDeclContext())) {
            if (FD->isMain()) {
                return true; // Skip rewriting parameters of main
            }
        }

        if (PVD->getType()->isPointerType()) {
            rewrite(PVD->getTypeSourceInfo());
        }
        return true;
    }

    // Handles function return types.
    bool VisitFunctionDecl(FunctionDecl* FD) {
        if (FD->getReturnType()->isPointerType()) {
            SourceRange range = FD->getReturnTypeSourceRange();
            if (range.isValid()) {
                rewrite(range);
            }
        }
        return true;
    }

private:
    void rewrite(TypeSourceInfo* TSI) {
        if (TSI) {
            rewrite(TSI->getTypeLoc().getSourceRange());
        }
    }

    void rewrite(const SourceRange& range) {
        std::string replacementText = ptrType;
        if (!replacementText.empty() && replacementText.back() != '*' && !isspace(replacementText.back())) {
            replacementText += ' ';
        }
        TheRewriter.ReplaceText(range, replacementText);
    }

    Rewriter &TheRewriter;
    const std::string ptrType;
};

} // anonymous namespace

HeapEncoder::HeapEncoder(Rewriter &R, ASTContext &Ctx, const std::string &encodingFile, Backend backend) {
    ProgramInfo pInfo = parseProgramHints(Ctx);
    if (!pInfo.hintFound) return;

    EncodingInfo eInfo = parseEncodingFile(encodingFile);
    if (!eInfo.loaded) { llvm::errs() << "Error: Could not load encoding file '" << encodingFile << "'.\n"; return; }

    NondetFinderVisitor ndVisitor(Ctx, pInfo);
    ndVisitor.TraverseDecl(Ctx.getTranslationUnitDecl());

    std::string intHavocFn = ndVisitor.getIntFnName().empty() ? "nondet_int" : ndVisitor.getIntFnName();
    std::string heapHavocFn = ndVisitor.getHeapFnName().empty() ? "nondet_heap_obj" : ndVisitor.getHeapFnName();

    std::string finalAbstractPtrType = eInfo.ptrType;
    replaceAll(finalAbstractPtrType, "HEAP_TYPE", pInfo.heapType);

    replaceAll(eInfo.globalDecls, "HEAP_TYPE", pInfo.heapType);
    replaceAll(eInfo.readFn, "HEAP_TYPE", pInfo.heapType);
    replaceAll(eInfo.writeFn, "HEAP_TYPE", pInfo.heapType);
    replaceAll(eInfo.allocFn, "HEAP_TYPE", pInfo.heapType);

    replaceAll(eInfo.readFn, "PTR_TYPE", finalAbstractPtrType);
    replaceAll(eInfo.writeFn, "PTR_TYPE", finalAbstractPtrType);
    replaceAll(eInfo.allocFn, "PTR_TYPE", finalAbstractPtrType);

    replaceAll(eInfo.initBlock, "HAVOC_INT", intHavocFn + "()");
    replaceAll(eInfo.initBlock, "HAVOC_HEAP", heapHavocFn + "()");
    replaceAll(eInfo.readFn, "HAVOC_HEAP", heapHavocFn + "()");

    std::string nondetDeclarations;
    if (ndVisitor.getIntFnName().empty()) nondetDeclarations += "extern int " + intHavocFn + "();\n";
    if (ndVisitor.getHeapFnName().empty()) nondetDeclarations += "extern " + pInfo.heapType + " " + heapHavocFn + "();\n";

    std::string inputDeclsStr, inputArgsStr, predicateDeclarations;
    if (!pInfo.inputs.empty()) {
        for (size_t i = 0; i < pInfo.inputs.size(); ++i) {
            const auto& n = pInfo.inputs[i];
            inputDeclsStr += (n.find("ARR") != std::string::npos ? "int " + n + "[]" : "int " + n) + (i < pInfo.inputs.size() - 1 ? ", " : "");
            inputArgsStr += n + (i < pInfo.inputs.size() - 1 ? ", " : "");
        }
    }

    for (auto const& pair : eInfo.predicates) {
        const std::string& name = pair.first;
        const std::string& sig = pair.second;

        if (!pInfo.inputs.empty()) {
            replaceAll(eInfo.readFn, name + "(", name + "(" + inputArgsStr + ", ");
            replaceAll(eInfo.writeFn, name + "(", name + "(" + inputArgsStr + ", ");
        }
        std::string finalSig = sig;
        replaceAll(finalSig, "HEAP_TYPE", pInfo.heapType);
        if (!pInfo.inputs.empty()) {
            size_t openParen = finalSig.find('(');
            if (openParen != std::string::npos) {
                std::string content = finalSig.substr(openParen + 1);
                content.erase(0, content.find_first_not_of(" \t"));
                finalSig.insert(openParen + 1, inputDeclsStr + (content.rfind(')', 0) != 0 ? ", " : ""));
            }
        }

        if (backend == Backend::TriCera) {
            predicateDeclarations += "/*$ " + finalSig + " $*/\n";
        } else { // SeaHorn
            std::string predName = name;
            std::string externPredName = "pred" + predName;
            std::string argsWithTypes = finalSig.substr(finalSig.find("("));
            std::string argsOnlyNames = "()";
            size_t openParen = argsWithTypes.find('(');
            size_t closeParen = argsWithTypes.rfind(')');
            if (openParen != std::string::npos && closeParen != std::string::npos) {
                std::string params = argsWithTypes.substr(openParen + 1, closeParen - openParen - 1);
                std::stringstream ss(params);
                std::string segment, callArgs;
                bool first = true;
                while (std::getline(ss, segment, ',')) {
                    segment.erase(0, segment.find_first_not_of(" \t"));
                    segment.erase(segment.find_last_not_of(" \t") + 1);
                    if (segment.empty() || segment == "void") continue;
                    size_t pos = segment.rfind(' ');
                    if (pos == std::string::npos) pos = segment.rfind('*');
                    else {
                        size_t star_pos = segment.rfind('*');
                        if (star_pos != std::string::npos && star_pos > pos) pos = star_pos;
                    }
                    std::string varName = (pos == std::string::npos) ? segment : segment.substr(pos + 1);
                    if (varName.back() == ']') { varName.pop_back(); varName.pop_back(); }
                    if (!first) callArgs += ", ";
                    callArgs += varName;
                    first = false;
                }
                argsOnlyNames = "(" + callArgs + ")";
            }
            predicateDeclarations += "extern bool " + externPredName + argsWithTypes + ";\n";
            predicateDeclarations += "bool PARTIAL_FN " + predName + argsWithTypes + " {\n";
            predicateDeclarations += "  return " + externPredName + argsOnlyNames + ";\n}\n";
        }
    }

    InjectionPointVisitor ipVisitor(Ctx, pInfo);
    ipVisitor.TraverseDecl(Ctx.getTranslationUnitDecl());
    SourceManager &SM = R.getSourceMgr();
    FileID mainFileID = SM.getMainFileID();

    std::string fileContent(SM.getBufferData(mainFileID).str());
    std::regex hintRegex(R"(/\*[\s\S]*?HEAP_TYPE:[\s\S]*?\*/\n*)");
    std::smatch match;
    if (std::regex_search(fileContent, match, hintRegex)) {
        R.RemoveText(SM.getLocForStartOfFile(mainFileID).getLocWithOffset(match.position(0)), match.length(0));
    }

    if (Stmt* mainBody = ipVisitor.getMainBody()) {
        R.InsertText(mainBody->getBeginLoc().getLocWithOffset(1), "\n/* Start init block for the encoding */\n" + eInfo.initBlock + "\n/* End init block */\n", true, true);
    }

    std::string headerDecls = "\n// --- Injected by HeapEncoder ---\n";
    headerDecls += nondetDeclarations;
    if(!predicateDeclarations.empty()) headerDecls += predicateDeclarations;
    if(!eInfo.globalDecls.empty()) headerDecls += eInfo.globalDecls + "\n";
    if(!eInfo.readFn.empty()) headerDecls += eInfo.readFn + "\n";
    if(!eInfo.writeFn.empty()) headerDecls += eInfo.writeFn + "\n";
    if(!eInfo.allocFn.empty()) headerDecls += eInfo.allocFn + "\n";
    headerDecls += "// --- End Injection ---\n\n";
    R.InsertText(ipVisitor.getInjectionPoint(), headerDecls, true, true);

    if (!finalAbstractPtrType.empty()) {
        PointerRewriterVisitor ptrVisitor(R, finalAbstractPtrType);
        ptrVisitor.TraverseDecl(Ctx.getTranslationUnitDecl());
    }

    if (backend == Backend::SeaHorn) {
        RewriteBuffer &EB = R.getEditBuffer(mainFileID);
        std::string finalCode(EB.begin(), EB.end());
        replaceAll(finalCode, "assert", "sassert");
        finalCode = "#include \"seahorn/seasynth.h\"\n" + finalCode;
        R.ReplaceText(SourceRange(SM.getLocForStartOfFile(mainFileID), SM.getLocForEndOfFile(mainFileID)), finalCode);
    }
}
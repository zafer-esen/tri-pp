#include "Utilities.hpp"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Lex/Lexer.h"
#include "clang/AST/PrettyPrinter.h"

// todo: does not take into account possible declarations in the same line
// maybe switch to C-style /*...*/ comments in that case?
// another option might be to insert a new line after each declaration,
// but this would change line numbers
bool doubleSlashCommentOutDeclaration (const clang::Decl* declaration,
                                       clang::ASTContext &ctx,
                                       clang::Rewriter& rewriter) {
    clang::SourceManager &SM = ctx.getSourceManager();

    clang::FullSourceLoc B_full = ctx.getFullLoc(declaration->getBeginLoc());
    clang::FullSourceLoc E_full = ctx.getFullLoc(declaration->getEndLoc());

    // return if this declaration is not in the main file
    if (B_full.isInvalid() || B_full.getFileID() != SM.getMainFileID())
        return false;

    unsigned int startLineNum = B_full.getLineNumber();

    if (startLineNum > 1) {
        unsigned int prevLineNum = startLineNum - 1;

        clang::SourceLocation prevLineStartLoc = SM.translateLineCol(SM.getMainFileID(), prevLineNum, 1);

        if (prevLineStartLoc.isValid()) {
            const char* prevLineStartPtr = SM.getCharacterData(prevLineStartLoc);
            llvm::StringRef restOfFile(prevLineStartPtr);
            llvm::StringRef prevLineText = restOfFile.substr(0, restOfFile.find_first_of("\r\n"));

            if (prevLineText.trim() == "__extension__") {
                startLineNum = prevLineNum;
            }
        }
    }

    for (unsigned i = startLineNum; i <= E_full.getLineNumber(); ++i) {
        clang::SourceLocation lineStart = SM.translateLineCol(SM.getMainFileID(), i, 1);
        if(lineStart.isInvalid()) continue;
        rewriter.InsertText(lineStart, "// ");
    }
    return true;
}

void wrapWithCComment (clang::SourceRange sourceRange,
                       clang::Rewriter& rewriter,
                       bool insertBeforeBegin,
                       bool insertBeforeEnd){
  if(insertBeforeBegin)
    rewriter.InsertTextBefore(sourceRange.getBegin(), "/* ");
  else
    rewriter.InsertTextAfterToken(sourceRange.getBegin(), "/* ");
  if(insertBeforeEnd)
    rewriter.InsertTextBefore(sourceRange.getEnd(), " */ ");
  else
    rewriter.InsertTextAfterToken(sourceRange.getEnd(), " */ ");
}

bool BaseTypeExtractor::VisitType(clang::Type *typ) {
  //typ->dump();
  const clang::Type* canonicalType = typ->getCanonicalTypeUnqualified().getTypePtr();
  if (/*!canonicalType->isBuiltinType() && */!canonicalType->isPointerType()) {
    extractedType = const_cast<clang::Type *>(canonicalType);
    return false;
  } else if (canonicalType->isPointerType()) {
    //llvm::outs() << "pointer type\n";
    BaseTypeExtractor typeVisitor(Ctx);
    typeVisitor.TraverseType(canonicalType->getPointeeType());
  }
  return true;
}

// returns the previous declaration in the same statement if there exists one,
// returns nullptr otherwise.
// e.g. returns "int a" in "int a, b" if called with "int b"
const clang::Decl* getPrevDeclInSameStmt(const clang::Decl* decl) {
  clang::Decl* prevDecl = nullptr;
  for (auto d : decl->getDeclContext()->decls()) {
    if (declaresSameEntity(d, decl)) {
      if(prevDecl) {
        if(prevDecl->getBeginLoc() != d->getBeginLoc())
          prevDecl = nullptr;
      }
      break;
    }
    else prevDecl = d;
  }
  return prevDecl;
}

// returns the next declaration in the same statement if there exists one,
// returns nullptr otherwise.
// e.g. returns "int b" in "int a, b" if called with "int a"
const clang::Decl* getNextDeclInSameStmt(const clang::Decl* decl) {
  const clang::Decl* nextDecl = decl->getNextDeclInContext();
  if(nextDecl && nextDecl->getBeginLoc() == decl->getBeginLoc())
    return nextDecl;
  else return nullptr;
}

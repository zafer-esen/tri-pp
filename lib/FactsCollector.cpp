#include "FactsCollector.hpp"

#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <set>

using namespace clang;
using namespace llvm;

// the allocation routines TriCera knows (Symex.scala)
static const std::set<std::string> allocationFunctionNames = {
    "malloc", "calloc", "realloc", "free", "alloca", "__builtin_alloca"
};

void collectFacts(ASTContext &Ctx, ProgramFacts &facts) {
  facts.allocationFunctions.clear();
  facts.usesArrays = false;
  facts.usesUnboundedArrays = false;

  class FactsVisitor : public RecursiveASTVisitor<FactsVisitor> {
  public:
    FactsVisitor(ASTContext &Ctx, ProgramFacts &facts)
        : Ctx(Ctx), facts(facts) {}

    bool VisitDeclRefExpr(DeclRefExpr *expr) {
      const FunctionDecl *fun = dyn_cast<FunctionDecl>(expr->getDecl());
      if (fun && allocationFunctionNames.count(fun->getNameAsString()) &&
          inMainFile(expr->getBeginLoc()))
        facts.allocationFunctions.insert(fun->getNameAsString());
      return true;
    }

    bool VisitVarDecl(VarDecl *decl) {
      // determinizer-added declarations are reported as inputs instead
      std::string name = decl->getNameAsString();
      if (std::find(facts.inputArrays.begin(), facts.inputArrays.end(),
                    name) != facts.inputArrays.end() ||
          std::find(facts.inputVariables.begin(), facts.inputVariables.end(),
                    name) != facts.inputVariables.end())
        return true;
      // check the type as written: "int a[]" parameters decay to "int *"
      // and a tentative "int a[];" is completed to int[1] in the AST
      QualType typ = decl->getType();
      if (const ParmVarDecl *parm = dyn_cast<ParmVarDecl>(decl))
        typ = parm->getOriginalType();
      else if (decl->getTypeSourceInfo())
        typ = decl->getTypeSourceInfo()->getType();
      recordArrayDecl(typ, decl->getBeginLoc());
      return true;
    }

    bool VisitFieldDecl(FieldDecl *decl) {
      recordArrayDecl(decl->getType(), decl->getBeginLoc());
      return true;
    }

  private:
    void recordArrayDecl(QualType typ, SourceLocation loc) {
      if (!inMainFile(loc))
        return;
      if (typ.getCanonicalType()->isArrayType())
        facts.usesArrays = true;
      if (typ.getCanonicalType()->isIncompleteArrayType())
        facts.usesUnboundedArrays = true;
    }

    // only the main file ends up in the produced program
    bool inMainFile(SourceLocation loc) {
      SourceManager &SM = Ctx.getSourceManager();
      return loc.isValid() &&
             SM.getDecomposedLoc(loc).first == SM.getMainFileID();
    }

    ASTContext &Ctx;
    ProgramFacts &facts;
  };

  FactsVisitor visitor(Ctx, facts);
  visitor.TraverseDecl(Ctx.getTranslationUnitDecl());
}

// token-level scan for an identifier, skipping comments, strings and char
// literals; textual because clang cannot parse the TriCera extensions
static bool containsIdentifier(StringRef text, StringRef name) {
  enum class ScanState {
    NORMAL, IN_BLOCK_COMMENT, IN_AT_COMMENT, IN_STRING, IN_CHAR_LITERAL,
    IN_LINE_COMMENT
  };
  ScanState st = ScanState::NORMAL;
  std::string ident;
  for (unsigned i = 0; i < text.size(); ++i) {
    char c = text[i];
    char next = i + 1 < text.size() ? text[i + 1] : '\0';
    switch (st) {
    case ScanState::NORMAL:
      if (c == '/' && next == '*') { st = ScanState::IN_BLOCK_COMMENT; ++i; }
      else if (c == '/' && next == '/') { st = ScanState::IN_LINE_COMMENT; ++i; }
      else if (c == '/' && next == '@') { st = ScanState::IN_AT_COMMENT; ++i; }
      else if (c == '"') st = ScanState::IN_STRING;
      else if (c == '\'') st = ScanState::IN_CHAR_LITERAL;
      if (st != ScanState::NORMAL) {
        if (ident == name) return true;
        ident.clear();
        break;
      }
      if (isalpha((unsigned char)c) || c == '_' ||
          (!ident.empty() && isdigit((unsigned char)c)))
        ident += c;
      else {
        if (ident == name) return true;
        ident.clear();
      }
      break;
    case ScanState::IN_BLOCK_COMMENT:
      if (c == '*' && next == '/') { st = ScanState::NORMAL; ++i; }
      break;
    case ScanState::IN_AT_COMMENT:
      if (c == '@' && next == '/') { st = ScanState::NORMAL; ++i; }
      break;
    case ScanState::IN_STRING:
      if (c == '\\') ++i;
      else if (c == '"') st = ScanState::NORMAL;
      break;
    case ScanState::IN_CHAR_LITERAL:
      if (c == '\\') ++i;
      else if (c == '\'') st = ScanState::NORMAL;
      break;
    case ScanState::IN_LINE_COMMENT:
      if (c == '\\' && (next == '\n' || next == '\r')) ++i;
      else if (c == '\n') st = ScanState::NORMAL;
      break;
    }
  }
  return ident == name;
}

bool writeFacts(StringRef path, const ProgramFacts &facts,
                StringRef finalText, std::string &error) {
  bool clockTokenSeen = containsIdentifier(finalText, "clock");
  bool durationTokenSeen = containsIdentifier(finalText, "duration");

  std::error_code ec;
  raw_fd_ostream out(path, ec, sys::fs::OF_None);
  if (ec) {
    error = "cannot write facts file '" + path.str() + "': " + ec.message();
    return false;
  }
  out << "---\n";
  out << "# facts about the produced program; a missing fact means"
         " \"not determined\"\n";
  out << "# allocation functions the program references\n";
  out << "allocationFunctions: [";
  for (std::set<std::string>::const_iterator it =
           facts.allocationFunctions.begin();
       it != facts.allocationFunctions.end(); ++it)
    out << (it != facts.allocationFunctions.begin() ? ", " : "") << *it;
  out << "]\n";
  out << "# declarations with array types / with arrays of unknown bound\n";
  out << "usesArrays: " << (facts.usesArrays ? "true" : "false") << "\n";
  out << "usesUnboundedArrays: "
      << (facts.usesUnboundedArrays ? "true" : "false") << "\n";
  out << "# inputs added by the determinizer (also in the INPUT header"
         " comment)\n";
  out << "inputVariables: [";
  for (size_t i = 0; i < facts.inputVariables.size(); ++i)
    out << (i > 0 ? ", " : "") << facts.inputVariables[i];
  out << "]\n";
  out << "inputArrays: [";
  for (size_t i = 0; i < facts.inputArrays.size(); ++i)
    out << (i > 0 ? ", " : "") << facts.inputArrays[i];
  out << "]\n";
  out << "# approximate: token-level scan for TriCera's clock/duration"
         " extensions\n";
  out << "clockTokenSeen: " << (clockTokenSeen ? "true" : "false") << "\n";
  out << "durationTokenSeen: "
      << (durationTokenSeen ? "true" : "false") << "\n";
  return true;
}

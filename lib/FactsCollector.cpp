#include "FactsCollector.hpp"
#include "TypeCanoniser.hpp"

#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/Type.h"
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
  facts.usesThrow = false;
  facts.usesTryCatch = false;

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

    bool VisitCXXThrowExpr(CXXThrowExpr *expr) {
      if (inMainFile(expr->getBeginLoc()))
        facts.usesThrow = true;
      return true;
    }

    bool VisitCXXTryStmt(CXXTryStmt *stmt) {
      if (inMainFile(stmt->getBeginLoc()))
        facts.usesTryCatch = true;
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

void collectTypedefs(ASTContext &Ctx, ProgramFacts &facts) {
  class TypedefVisitor : public RecursiveASTVisitor<TypedefVisitor> {
  public:
    TypedefVisitor(ASTContext &Ctx, ProgramFacts &facts)
        : Ctx(Ctx), facts(facts) {}

    bool VisitTypedefDecl(TypedefDecl *decl) {
      SourceLocation loc = decl->getBeginLoc();
      if (!loc.isValid() ||
          Ctx.getSourceManager().getDecomposedLoc(loc).first !=
              Ctx.getSourceManager().getMainFileID())
        return true;

      QualType typedefType = Ctx.getTypedefType(decl);
      QualType canonicalType =
          QualType(typedefType->getCanonicalTypeUnqualified());

      TypeCanoniserVisitor typeVisitor(Ctx);
      typeVisitor.TraverseType(canonicalType);
      std::string unqualName = typeVisitor.getUnqualifiedTypeName();

      std::string kindName;
      if (const TagType *tagType =
              canonicalType->getAs<TagType>()) {
        kindName = tagType->getDecl()->getKindName().str();
      }
      std::string tagName;
      if (!kindName.empty()) {
        std::string expectedPrefix = kindName + " ";
        if (unqualName.find(expectedPrefix) != 0)
          tagName = expectedPrefix;
      }

      TypedefMapping mapping;
      mapping.name = decl->getNameAsString();
      mapping.underlying = tagName + unqualName;
      facts.typedefs.push_back(mapping);
      return true;
    }

  private:
    ASTContext &Ctx;
    ProgramFacts &facts;
  };

  TypedefVisitor visitor(Ctx, facts);
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
  out << "# C++ exceptions\n";
  out << "usesThrow: " << (facts.usesThrow ? "true" : "false") << "\n";
  out << "usesTryCatch: " << (facts.usesTryCatch ? "true" : "false") << "\n";
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
  out << "# clock/duration are TriCera keywords; reported when the token"
         " appears\n";
  out << "clockTokenSeen: " << (clockTokenSeen ? "true" : "false") << "\n";
  out << "durationTokenSeen: "
      << (durationTokenSeen ? "true" : "false") << "\n";
  out << "# C++ names introduced by template instantiation, and what they"
         " stand for\n";
  if (facts.mangledNames.empty())
    out << "mangledNames: []\n";
  else {
    out << "mangledNames:\n";
    for (size_t i = 0; i < facts.mangledNames.size(); ++i)
      out << "  - mangled: " << facts.mangledNames[i].mangled << "\n"
          << "    original: \"" << facts.mangledNames[i].original << "\"\n";
  }
  if (facts.typedefs.empty())
    out << "typedefs: []\n";
  else {
    out << "typedefs:\n";
    for (size_t i = 0; i < facts.typedefs.size(); ++i)
      out << "  - name: " << facts.typedefs[i].name << "\n"
          << "    underlying: \"" << facts.typedefs[i].underlying << "\"\n";
  }
  return true;
}

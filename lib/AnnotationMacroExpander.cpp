#include "AnnotationMacroExpander.hpp"

#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/TokenConcatenation.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSet.h"

#include <algorithm>

using namespace clang;
using namespace llvm;

// records the ranges of the macro invocations expanded while active,
// including those of macros that expand to nothing
class InvocationRecorder : public PPCallbacks {
public:
  void MacroExpands(const Token &, const MacroDefinition &, SourceRange range,
                    const MacroArgs *) override {
    if (active)
      ranges.push_back(range);
  }

  bool active = false;
  std::vector<SourceRange> ranges;
};

namespace {

// identifiers that are keywords, not macros, at the start of a clause
const StringSet<> clauseKeywords = {
    "assert", "check", "admit", "requires", "ensures", "assigns", "loop",
    "invariant", "variant", "decreases", "terminates", "behavior",
    "behaviors", "assumes", "complete", "disjoint", "predicate", "logic",
    "ghost", "frees", "allocates", "exits", "breaks", "continues", "returns",
    "axiomatic", "axiom", "lemma", "type", "reads", "inductive", "global",
    "contract"};

// logic labels, as in \at(x, Pre)
const StringSet<> logicLabels = {"Pre",  "Here",      "Old",        "Post",
                                 "Init", "LoopEntry", "LoopCurrent"};

Token makeToken(tok::TokenKind kind, SourceLocation loc, unsigned length) {
  Token t;
  t.startToken();
  t.setKind(kind);
  t.setLocation(loc);
  t.setLength(length);
  return t;
}

bool isFirstOnLine(StringRef buf, unsigned offset) {
  size_t p = buf.substr(0, offset).find_last_not_of(" \t");
  return p == StringRef::npos || buf[p] == '\n' || buf[p] == '\r';
}

// a name with side effects on the translation unit that some macro
// reachable from the tokens uses, or ""
StringRef sideEffectingName(Preprocessor &PP, const std::vector<Token> &toks) {
  SmallPtrSet<const IdentifierInfo *, 16> seen;
  std::vector<const IdentifierInfo *> work;
  for (const Token &t : toks)
    if (t.getIdentifierInfo())
      work.push_back(t.getIdentifierInfo());
  while (!work.empty()) {
    const IdentifierInfo *II = work.back();
    work.pop_back();
    if (!seen.insert(II).second)
      continue;
    StringRef name = II->getName();
    if (name == "_Pragma" || name == "__pragma" || name == "__COUNTER__")
      return name;
    if (const MacroInfo *MI = PP.getMacroInfo(II))
      for (const Token &t : MI->tokens())
        if (t.getIdentifierInfo())
          work.push_back(t.getIdentifierInfo());
  }
  return "";
}

// whether a and b, printed without a space between, lex differently
bool needSpace(const TokenConcatenation &concat, const Token &beforeA,
               const Token &a, const Token &b) {
  // keep ACSL ranges like 0..N together
  if ((a.is(tok::period) && b.is(tok::numeric_constant)) ||
      (a.is(tok::numeric_constant) && b.is(tok::period)))
    return false;
  return concat.AvoidConcat(beforeA, a, b);
}

// whether the expanded comment ends where the original one did
bool keepsExtent(StringRef text, bool isBlock) {
  if (isBlock)
    return text.find("*/") == text.size() - 2;
  // a backslash at the end would continue a line comment
  return !text.rtrim(" \t\r").ends_with("\\");
}

} // namespace

AnnotationMacroExpander::AnnotationMacroExpander(Preprocessor &PP) : PP(PP) {
  PP.addCommentHandler(this);
  auto rec = std::make_unique<InvocationRecorder>();
  recorder = rec.get();
  PP.addPPCallbacks(std::move(rec));
}

void AnnotationMacroExpander::detach() { PP.removeCommentHandler(this); }

unsigned AnnotationMacroExpander::offsetOf(const Token &t) const {
  return PP.getSourceManager().getFileOffset(t.getLocation());
}

bool AnnotationMacroExpander::HandleComment(Preprocessor &,
                                            SourceRange comment) {
  SourceManager &SM = PP.getSourceManager();
  std::pair<FileID, unsigned> begin = SM.getDecomposedLoc(comment.getBegin());
  if (begin.first != SM.getMainFileID())
    return false;
  fid = begin.first;
  buf = SM.getBufferData(fid);
  StringRef text =
      buf.slice(begin.second, SM.getFileOffset(comment.getEnd()));
  bool isBlock = text.starts_with("/*@");
  if (!isBlock && !text.starts_with("//@"))
    return false;
  commentBegin = begin.second;
  bodyBegin = commentBegin + 3;
  bodyEnd = commentBegin + text.size() - (isBlock ? 2 : 0);

  std::vector<Token> toks = lexBody(isBlock);
  bool namesMacro = std::any_of(toks.begin(), toks.end(), [&](const Token &t) {
    return t.getIdentifierInfo() && !t.isExpandDisabled() &&
           PP.isMacroDefined(t.getIdentifierInfo());
  });
  if (!namesMacro)
    return false;

  AnnotationExpansion a = {fid, commentBegin, text.str(), text.str(), ""};
  a.skipReason = expand(toks, isBlock, a.expandedText);
  if (!a.skipReason.empty() || a.expandedText != a.originalText)
    annots.push_back(a);
  return false;
}

// raw-lexes the annotation body; ACSL builtins like \valid, clause
// keywords and logic labels are marked not to expand
std::vector<Token> AnnotationMacroExpander::lexBody(bool isBlock) {
  Lexer lexer(PP.getSourceManager().getLocForStartOfFile(fid),
              PP.getLangOpts(), buf.begin(), buf.begin() + bodyBegin,
              buf.end());
  std::vector<Token> toks;
  bool clauseStart = true, afterLoop = false;
  int depth = 0;
  while (true) {
    Token t;
    lexer.LexFromRawLexer(t);
    unsigned off = offsetOf(t);
    if (t.is(tok::eof) || off + t.getLength() > bodyEnd)
      break;
    // the '@' starting the lines of a block annotation, or before its end
    if (isBlock && buf[off] == '@' &&
        (isFirstOnLine(buf, off) || off + 1 == bodyEnd))
      continue;

    if (t.is(tok::raw_identifier)) {
      PP.LookUpIdentifierInfo(t);
      StringRef name = t.getIdentifierInfo()->getName();
      bool keyword = (clauseStart && clauseKeywords.contains(name)) ||
                     afterLoop;
      if (keyword || buf[off - 1] == '\\' || logicLabels.contains(name))
        t.setFlag(Token::DisableExpand);
      afterLoop = clauseStart && name == "loop";
      clauseStart = false;
    } else {
      afterLoop = false;
      if (t.isOneOf(tok::l_paren, tok::l_square, tok::l_brace))
        ++depth;
      else if (t.isOneOf(tok::r_paren, tok::r_square, tok::r_brace))
        --depth;
      clauseStart = depth <= 0 && t.isOneOf(tok::semi, tok::colon);
    }

    // a pp-number like 0..N-1 is an ACSL range: split off the number and
    // the two periods, and lex the rest again
    size_t dots = t.is(tok::numeric_constant)
                      ? buf.substr(off, t.getLength()).find("..")
                      : StringRef::npos;
    if (dots != StringRef::npos) {
      t.setLength(dots);
      toks.push_back(t);
      for (unsigned i = 0; i < 2; ++i)
        toks.push_back(makeToken(
            tok::period, t.getLocation().getLocWithOffset(dots + i), 1));
      lexer.seek(off + dots + 2, /*IsAtStartOfLine=*/false);
      continue;
    }
    toks.push_back(t);
  }
  return toks;
}

// replaces the macro invocations in the comment text by their expansions;
// returns why the annotation must be kept verbatim, or ""
std::string AnnotationMacroExpander::expand(const std::vector<Token> &toks,
                                            bool isBlock, std::string &text) {
  StringRef effect = sideEffectingName(PP, toks);
  if (!effect.empty())
    return ("expanding it would use " + effect).str();

  // suppressed errors do not count for the translation unit, but the trap
  // sees them
  DiagnosticsEngine &diags = PP.getDiagnostics();
  bool suppressed = diags.getSuppressAllDiagnostics();
  diags.setSuppressAllDiagnostics(true);
  DiagnosticErrorTrap trap(diags);
  std::vector<Token> out;
  bool enabled = lexExpanded(toks, out);
  diags.setSuppressAllDiagnostics(suppressed);
  if (!enabled)
    return "macro expansion is disabled at this position";
  if (trap.hasErrorOccurred())
    return "expanding its macros raises a preprocessor error";

  SourceManager &SM = PP.getSourceManager();
  std::string expanded = text;
  std::vector<std::pair<unsigned, unsigned> > ranges = invocationRanges(out);
  for (auto r = ranges.rbegin(); r != ranges.rend(); ++r) {
    unsigned b = r->first, e = r->second;
    // newlines after the expansion keep the line count, like cpp; in a line
    // annotation they would be backslash-newline splices
    size_t newlines = buf.slice(b, e).count('\n');
    if (newlines && !isBlock)
      return "a macro invocation spans several lines";
    std::vector<Token> produced;
    for (const Token &t : out) {
      std::pair<FileID, unsigned> loc =
          SM.getDecomposedLoc(SM.getExpansionLoc(t.getLocation()));
      if (loc.first == fid && b <= loc.second && loc.second < e)
        produced.push_back(t);
    }
    expanded.replace(b - commentBegin, e - b,
                     spell(toks, produced, b, e) + std::string(newlines, '\n'));
  }
  if (!keepsExtent(expanded, isBlock))
    return "the expansion would change where the comment ends";
  text = expanded;
  return "";
}

// lexes the tokens with macro expansion at the current position; false if
// macro expansion is disabled there
bool AnnotationMacroExpander::lexExpanded(const std::vector<Token> &toks,
                                          std::vector<Token> &out) {
  SourceManager &SM = PP.getSourceManager();
  // a __LINE__ before the body expands to a number iff expansion is
  // enabled; the eof token ends the stream and stops any look-ahead for a '('
  std::vector<Token> stream;
  stream.push_back(makeToken(tok::identifier,
                             SM.getComposedLoc(fid, commentBegin), 0));
  stream.back().setIdentifierInfo(PP.getIdentifierInfo("__LINE__"));
  stream.insert(stream.end(), toks.begin(), toks.end());
  stream.push_back(makeToken(tok::eof, SM.getComposedLoc(fid, bodyEnd), 0));

  recorder->ranges.clear();
  recorder->active = true;
  PP.EnterTokenStream(stream, /*DisableMacroExpansion=*/false,
                      /*IsReinject=*/false);
  Token t;
  for (PP.Lex(t); t.isNot(tok::eof); PP.Lex(t))
    out.push_back(t);
  PP.RemoveTopOfLexerStack();
  recorder->active = false;

  if (out.empty() || out.front().isNot(tok::numeric_constant))
    return false;
  out.erase(out.begin());
  return true;
}

// the file ranges [b, e) of the top-level macro invocations in the body:
// the union of the expanded ranges and of the expansions of the tokens
std::vector<std::pair<unsigned, unsigned> >
AnnotationMacroExpander::invocationRanges(const std::vector<Token> &out) {
  SourceManager &SM = PP.getSourceManager();
  std::vector<SourceRange> found = recorder->ranges;
  for (const Token &t : out)
    if (t.getLocation().isMacroID())
      found.push_back(t.getLocation());

  std::vector<std::pair<unsigned, unsigned> > ranges;
  for (SourceRange r : found) {
    SourceLocation b = SM.getExpansionRange(r.getBegin()).getBegin();
    CharSourceRange end = SM.getExpansionRange(r.getEnd());
    std::pair<FileID, unsigned> db = SM.getDecomposedLoc(b);
    std::pair<FileID, unsigned> de = SM.getDecomposedLoc(end.getEnd());
    unsigned e = de.second;
    if (end.isTokenRange())
      e += Lexer::MeasureTokenLength(end.getEnd(), SM, PP.getLangOpts());
    if (db.first == fid && de.first == fid && bodyBegin <= db.second &&
        db.second < e && e <= bodyEnd)
      ranges.push_back(std::make_pair(db.second, e));
  }

  // merge overlapping and adjacent ranges
  std::sort(ranges.begin(), ranges.end());
  std::vector<std::pair<unsigned, unsigned> > merged;
  for (const std::pair<unsigned, unsigned> &r : ranges) {
    if (!merged.empty() && r.first <= merged.back().second)
      merged.back().second = std::max(merged.back().second, r.second);
    else
      merged.push_back(r);
  }
  return merged;
}

// the spelling of the tokens produced by the invocation [b, e), with the
// spaces needed between them and towards the adjacent annotation tokens
std::string AnnotationMacroExpander::spell(const std::vector<Token> &toks,
                                           const std::vector<Token> &produced,
                                           unsigned b, unsigned e) {
  Token none = makeToken(tok::unknown, SourceLocation(), 0);
  const Token *beforePrev = &none, *prev = nullptr, *next = nullptr;
  for (size_t i = 0; i < toks.size(); ++i) {
    unsigned off = offsetOf(toks[i]);
    if (off + toks[i].getLength() == b) {
      prev = &toks[i];
      beforePrev = i > 0 ? &toks[i - 1] : &none;
    }
    if (off == e)
      next = &toks[i];
  }

  TokenConcatenation concat(PP);
  std::string s;
  for (size_t i = 0; i < produced.size(); ++i) {
    const Token &t = produced[i];
    if (prev && ((i > 0 && t.hasLeadingSpace()) ||
                 needSpace(concat, *beforePrev, *prev, t)))
      s += ' ';
    s += PP.getSpelling(t);
    beforePrev = prev ? prev : &none;
    prev = &t;
  }
  if (prev && next && needSpace(concat, *beforePrev, *prev, *next))
    s += ' ';
  return s;
}

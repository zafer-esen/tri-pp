#include "TrackedRewriter.hpp"

#include "clang/Lex/Lexer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"

#include <algorithm>
#include <map>
#include <utility>

using namespace clang;
using namespace llvm;

//===----------------------------------------------------------------------===//
// TrackedRewriter
//===----------------------------------------------------------------------===//

void TrackedRewriter::recordAt(SourceLocation Loc, unsigned extraOffset,
                               TrackedEdit::Kind kind, unsigned length,
                               StringRef text, bool insertAfter,
                               unsigned originLine) {
  SourceManager &SM = rewriter.getSourceMgr();
  std::pair<FileID, unsigned> loc = SM.getDecomposedLoc(Loc);
  if (loc.first != SM.getMainFileID())
    return; // only main-file edits reach the output buffer
  tracker.record(activeTransformer, kind, loc.second + extraOffset, length,
                 text, insertAfter, originLine);
}

// same predicate Rewriter::InsertText uses for indentNewLines
static bool isWhitespaceExceptNL(unsigned char c) {
  switch (c) {
  case ' ':
  case '\t':
  case '\f':
  case '\v':
  case '\r':
    return true;
  default:
    return false;
  }
}

bool TrackedRewriter::InsertText(SourceLocation Loc, StringRef Str,
                                 bool InsertAfter, bool indentNewLines,
                                 unsigned originLine) {
  if (!Rewriter::isRewritable(Loc))
    return true;

  // expand indentNewLines like Rewriter::InsertText would, so the recorded
  // text equals the buffer text
  SmallString<128> indentedStr;
  if (indentNewLines && Str.contains('\n')) {
    SourceManager &SM = rewriter.getSourceMgr();
    std::pair<FileID, unsigned> loc = SM.getDecomposedLoc(Loc);
    StringRef MB = SM.getBufferData(loc.first);
    unsigned lineNo = SM.getLineNumber(loc.first, loc.second);
    unsigned lineOffs = SM.getDecomposedLoc(
        SM.translateLineCol(loc.first, lineNo, 1)).second;
    unsigned i = lineOffs;
    while (i < MB.size() && isWhitespaceExceptNL(MB[i]))
      ++i;
    StringRef indentSpace = MB.substr(lineOffs, i - lineOffs);

    SmallVector<StringRef, 4> lines;
    Str.split(lines, "\n");
    for (unsigned l = 0, e = lines.size(); l != e; ++l) {
      indentedStr += lines[l];
      if (l < e - 1) {
        indentedStr += '\n';
        indentedStr += indentSpace;
      }
    }
    Str = indentedStr.str();
  }

  if (rewriter.InsertText(Loc, Str, InsertAfter, /*indentNewLines=*/false))
    return true;
  recordAt(Loc, 0, TrackedEdit::Kind::INSERTION, 0, Str, InsertAfter,
           originLine);
  return false;
}

bool TrackedRewriter::InsertTextAfterToken(SourceLocation Loc, StringRef Str) {
  if (!Rewriter::isRewritable(Loc))
    return true;
  unsigned tokLen = Lexer::MeasureTokenLength(Loc, rewriter.getSourceMgr(),
                                              rewriter.getLangOpts());
  if (rewriter.InsertTextAfterToken(Loc, Str))
    return true;
  recordAt(Loc, tokLen, TrackedEdit::Kind::INSERTION, 0, Str,
           /*insertAfter=*/true);
  return false;
}

bool TrackedRewriter::ReplaceText(SourceRange range, StringRef NewStr) {
  SourceLocation B = range.getBegin(), E = range.getEnd();
  if (!Rewriter::isRewritable(B) || !Rewriter::isRewritable(E))
    return true;

  SourceManager &SM = rewriter.getSourceMgr();
  std::pair<FileID, unsigned> beg = SM.getDecomposedLoc(B);
  std::pair<FileID, unsigned> end = SM.getDecomposedLoc(E);
  if (beg.first != end.first || end.second < beg.second)
    return true; // Rewriter::getRangeSize would report -1 for these

  if (rewriter.ReplaceText(range, NewStr))
    return true;

  // length of the original token range
  unsigned tokLen = Lexer::MeasureTokenLength(E, SM, rewriter.getLangOpts());
  recordAt(B, 0, TrackedEdit::Kind::REPLACEMENT,
           end.second + tokLen - beg.second, NewStr, /*insertAfter=*/true);
  return false;
}

// whitespace of the same geometry as the original buffer at [offset,
// offset+length): newlines kept, everything else a space
static std::string whitespaceFor(StringRef buf, unsigned offset,
                                 unsigned length) {
  std::string spaces;
  spaces.reserve(length);
  for (unsigned i = 0; i < length; ++i) {
    char c = buf[offset + i];
    spaces += (c == '\n' || c == '\r') ? c : ' ';
  }
  return spaces;
}

bool TrackedRewriter::BlankText(SourceRange range) {
  SourceLocation B = range.getBegin(), E = range.getEnd();
  if (!Rewriter::isRewritable(B) || !Rewriter::isRewritable(E))
    return true;

  SourceManager &SM = rewriter.getSourceMgr();
  std::pair<FileID, unsigned> beg = SM.getDecomposedLoc(B);
  std::pair<FileID, unsigned> end = SM.getDecomposedLoc(E);
  if (beg.first != end.first || end.second < beg.second)
    return true;

  unsigned tokLen = Lexer::MeasureTokenLength(E, SM, rewriter.getLangOpts());
  unsigned length = end.second + tokLen - beg.second;
  StringRef buf = SM.getBufferData(beg.first);
  if (beg.second + length > buf.size())
    return true;

  std::string spaces = whitespaceFor(buf, beg.second, length);
  if (rewriter.ReplaceText(range, spaces))
    return true;
  recordAt(B, 0, TrackedEdit::Kind::BLANK, length, spaces,
           /*insertAfter=*/true);
  return false;
}

bool TrackedRewriter::BlankChars(SourceLocation B, SourceLocation E) {
  if (!Rewriter::isRewritable(B) || !Rewriter::isRewritable(E))
    return true;

  SourceManager &SM = rewriter.getSourceMgr();
  std::pair<FileID, unsigned> beg = SM.getDecomposedLoc(B);
  std::pair<FileID, unsigned> end = SM.getDecomposedLoc(E);
  if (beg.first != end.first || end.second < beg.second)
    return true;

  unsigned length = end.second - beg.second;
  if (length == 0)
    return false;
  StringRef buf = SM.getBufferData(beg.first);
  if (beg.second + length > buf.size())
    return true;

  std::string spaces = whitespaceFor(buf, beg.second, length);
  if (rewriter.ReplaceText(B, length, spaces))
    return true;
  recordAt(B, 0, TrackedEdit::Kind::BLANK, length, spaces,
           /*insertAfter=*/true);
  return false;
}

//===----------------------------------------------------------------------===//
// EditTracker
//===----------------------------------------------------------------------===//

std::vector<TrackedEdit> EditTracker::editsWithoutRepeats() const {
  // a repeated identical replacement changes nothing (TypeCanoniser runs
  // its sizeof matcher twice); keep the first
  std::vector<TrackedEdit> result;
  for (size_t i = 0; i < edits.size(); ++i) {
    const TrackedEdit &e = edits[i];
    if (!e.isInsertion()) {
      bool repeated = false;
      for (size_t j = 0; j < result.size(); ++j) {
        const TrackedEdit &p = result[j];
        if (!p.isInsertion() && p.offset == e.offset &&
            p.length == e.length && p.text == e.text) {
          repeated = true;
          break;
        }
      }
      if (repeated)
        continue;
    }
    result.push_back(e);
  }
  return result;
}

std::vector<std::pair<TrackedEdit, TrackedEdit> >
EditTracker::nonCommutingPairs() const {
  std::vector<std::pair<TrackedEdit, TrackedEdit> > pairs;
  std::vector<TrackedEdit> es = editsWithoutRepeats(); // application order
  for (size_t i = 0; i < es.size(); ++i) {
    for (size_t j = i + 1; j < es.size(); ++j) {
      const TrackedEdit &a = es[i], &b = es[j];
      bool sameTransformer = a.transformer == b.transformer;
      bool clash;
      if (a.isInsertion() && b.isInsertion()) {
        // same-offset insertions compose, but only deliberately so within
        // one transformer
        clash = a.offset == b.offset && !sameTransformer;
      } else if (a.isInsertion() != b.isInsertion()) {
        const TrackedEdit &ins = a.isInsertion() ? a : b;
        const TrackedEdit &rep = a.isInsertion() ? b : a;
        // an insertion inside a replaced range; the edges are fine
        clash = rep.offset < ins.offset &&
                ins.offset < rep.offset + rep.length;
      } else if (a.isBlank() && b.isBlank() && sameTransformer) {
        // overlapping blanks give their union either way
        clash = false;
      } else {
        // no other replacements may intersect
        clash = a.offset < b.offset + b.length &&
                b.offset < a.offset + a.length;
      }
      if (clash)
        pairs.push_back(std::make_pair(a, b));
    }
  }
  return pairs;
}

std::vector<NetEdit> EditTracker::disjointEdits(StringRef original) const {
  std::vector<TrackedEdit> es = editsWithoutRepeats();

  // 1. merge overlapping blanks into their union
  std::vector<std::pair<unsigned, unsigned> > blankIvs; // [start, end)
  for (size_t i = 0; i < es.size(); ++i)
    if (es[i].isBlank())
      blankIvs.push_back(std::make_pair(es[i].offset,
                                        es[i].offset + es[i].length));
  std::sort(blankIvs.begin(), blankIvs.end());
  std::vector<std::pair<unsigned, unsigned> > merged;
  for (size_t i = 0; i < blankIvs.size(); ++i) {
    if (!merged.empty() && blankIvs[i].first <= merged.back().second)
      merged.back().second = std::max(merged.back().second, blankIvs[i].second);
    else
      merged.push_back(blankIvs[i]);
  }

  // 2. group by offset and compose like RewriteBuffer: InsertAfter
  //    appends, InsertBefore prepends, replacement text comes last
  struct OffsetState {
    std::string insertText;
    std::string replaceText;
    unsigned replaceLen;
    unsigned originLine;
    OffsetState() : replaceLen(0), originLine(0) {}
  };
  std::map<unsigned, OffsetState> byOffset;
  for (size_t i = 0; i < es.size(); ++i) {
    const TrackedEdit &e = es[i];
    if (e.isBlank())
      continue; // added from `merged` below
    OffsetState &s = byOffset[e.offset];
    if (e.isInsertion())
      s.insertText = e.insertAfter ? s.insertText + e.text
                                   : e.text + s.insertText;
    else {
      // conflict detection guarantees at most one replacement per offset
      s.replaceText = e.text;
      s.replaceLen = e.length;
    }
    if (s.originLine == 0)
      s.originLine = e.originLine;
  }
  for (size_t i = 0; i < merged.size(); ++i) {
    OffsetState &s = byOffset[merged[i].first];
    unsigned len = merged[i].second - merged[i].first;
    s.replaceLen = len;
    s.replaceText.clear();
    for (unsigned k = 0; k < len; ++k) {
      char c = merged[i].first + k < original.size()
                   ? original[merged[i].first + k] : ' ';
      s.replaceText += (c == '\n' || c == '\r') ? c : ' ';
    }
  }

  std::vector<NetEdit> result;
  for (std::map<unsigned, OffsetState>::iterator it = byOffset.begin();
       it != byOffset.end(); ++it) {
    NetEdit n;
    n.offset = it->first;
    n.length = it->second.replaceLen;
    n.text = it->second.insertText + it->second.replaceText;
    n.originLine = it->second.originLine;
    result.push_back(n);
  }
  return result;
}

std::string EditTracker::apply(StringRef original) const {
  std::vector<NetEdit> nets = disjointEdits(original);
  std::string out;
  out.reserve(original.size());
  unsigned pos = 0;
  for (size_t i = 0; i < nets.size(); ++i) {
    const NetEdit &n = nets[i];
    if (n.offset > original.size() || n.offset < pos)
      break; // defensive; cannot happen for in-range, conflict-free edits
    out.append(original.data() + pos, original.data() + n.offset);
    out.append(n.text);
    pos = n.offset + n.length;
  }
  out.append(original.data() + pos, original.data() + original.size());
  return out;
}

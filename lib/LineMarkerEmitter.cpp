#include "LineMarkerEmitter.hpp"

#include "llvm/Support/raw_ostream.h"

#include <vector>

using namespace clang;
using namespace llvm;

// offsets at which each line starts (line i, 1-based, starts at result[i-1])
static std::vector<unsigned> lineStartsOf(StringRef text) {
  std::vector<unsigned> starts;
  starts.push_back(0);
  for (unsigned i = 0; i < text.size(); ++i)
    if (text[i] == '\n')
      starts.push_back(i + 1);
  // a final newline opens no further line
  if (!starts.empty() && starts.back() == text.size() && text.size() > 0)
    starts.pop_back();
  return starts;
}

// 1-based line containing `offset`, via binary search over line starts
static unsigned lineAt(const std::vector<unsigned> &starts, unsigned offset) {
  unsigned lo = 0, hi = (unsigned)starts.size();
  while (lo + 1 < hi) {
    unsigned mid = (lo + hi) / 2;
    if (starts[mid] <= offset)
      lo = mid;
    else
      hi = mid;
  }
  return lo + 1;
}

// a linemarker or #line directive; such lines come from cpp or an earlier
// tri-pp stage and must not be passed through
static bool isMarkerLine(StringRef line) {
  size_t i = 0;
  while (i < line.size() && (line[i] == ' ' || line[i] == '\t')) ++i;
  if (i >= line.size() || line[i] != '#') return false;
  ++i;
  while (i < line.size() && (line[i] == ' ' || line[i] == '\t')) ++i;
  if (line.substr(i).startswith("line")) {
    i += 4;
    if (i >= line.size() || (line[i] != ' ' && line[i] != '\t')) return false;
    while (i < line.size() && (line[i] == ' ' || line[i] == '\t')) ++i;
  }
  if (i >= line.size() || line[i] < '0' || line[i] > '9') return false;
  return true;
}

// markers may only be inserted where a line starts outside any comment or
// string literal
enum class ScanState {
  NORMAL,
  IN_BLOCK_COMMENT,  // /* ... */ including ACSL /*@ ... */ and /*$ ... $*/
  IN_AT_COMMENT,     // /@ ... @/ (TriCera ACSL variant)
  IN_STRING,         // "..." continued over a backslash-newline
  IN_CHAR_LITERAL,   // '...' continued over a backslash-newline
  IN_LINE_COMMENT    // // ... continued over a backslash-newline
};

// state of `text` at the start of each line (index = line - 1)
static std::vector<ScanState> scanStatesAtLineStarts(StringRef text) {
  std::vector<ScanState> states;
  ScanState st = ScanState::NORMAL;
  states.push_back(st);
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
      break;
    case ScanState::IN_BLOCK_COMMENT:
      if (c == '*' && next == '/') { st = ScanState::NORMAL; ++i; }
      break;
    case ScanState::IN_AT_COMMENT:
      if (c == '@' && next == '/') { st = ScanState::NORMAL; ++i; }
      break;
    case ScanState::IN_STRING:
      if (c == '\\') ++i; // skip escaped char (also a backslash-newline)
      else if (c == '"') st = ScanState::NORMAL;
      break;
    case ScanState::IN_CHAR_LITERAL:
      if (c == '\\') ++i;
      else if (c == '\'') st = ScanState::NORMAL;
      break;
    case ScanState::IN_LINE_COMMENT:
      if (c == '\\' && (next == '\n' || next == '\r')) ++i; // continued
      else if (c == '\n') st = ScanState::NORMAL;
      break;
    }
    if (i >= text.size()) // ++i above may step past the end
      break;
    if (text[i] == '\n')
      states.push_back(st);
  }
  return states;
}

bool emitLineMarkers(StringRef original, StringRef rewritten,
                     const EditTracker &tracker, SourceManager &SM,
                     std::string &result, std::string &error) {
  // the log must reproduce the buffer exactly, otherwise the markers
  // would be fiction
  std::string replayed = tracker.apply(original);
  if (StringRef(replayed) != rewritten) {
    error = "tracked edits do not reproduce the rewritten output; "
            "an edit bypassed the TrackedRewriter";
    return false;
  }

  std::vector<NetEdit> nets = tracker.disjointEdits(original);
  std::vector<unsigned> origStarts = lineStartsOf(original);
  std::vector<unsigned> outStarts = lineStartsOf(rewritten);
  unsigned numOutLines = (unsigned)outStarts.size();

  // claimedBy[o-1]: first original line whose start maps into output line
  // o (0 = inserted text only). originOf[o-1]: for unclaimed lines, the
  // original line the inserted text is attributed to.
  std::vector<unsigned> claimedBy(numOutLines, 0);
  std::vector<unsigned> originOf(numOutLines, 0);

  // walk the pieces [unchanged span | edit text]* in step with the output
  {
    unsigned origPos = 0; // position in original
    unsigned outPos = 0;  // position in rewritten
    size_t netIdx = 0;
    while (origPos <= original.size()) {
      unsigned spanEnd = netIdx < nets.size() ? nets[netIdx].offset
                                              : (unsigned)original.size();
      // unchanged original span [origPos, spanEnd)
      for (unsigned L = lineAt(origStarts, origPos); L <= origStarts.size();
           ++L) {
        unsigned start = origStarts[L - 1];
        if (start >= spanEnd)
          break;
        if (start >= origPos) {
          unsigned mapped = outPos + (start - origPos);
          unsigned outLine = lineAt(outStarts, mapped);
          if (claimedBy[outLine - 1] == 0)
            claimedBy[outLine - 1] = L;
        }
      }
      outPos += spanEnd - origPos;
      origPos = spanEnd;
      if (netIdx >= nets.size())
        break;
      // edit text replacing [origPos, origPos+length)
      const NetEdit &n = nets[netIdx];
      unsigned anchorLine = n.originLine
                                ? n.originLine
                                : lineAt(origStarts, n.offset);
      unsigned editOutEnd = outPos + (unsigned)n.text.size();
      // attribute output lines starting inside the edit text
      unsigned firstCand = lineAt(outStarts, outPos);
      if (outStarts[firstCand - 1] != outPos)
        ++firstCand; // the edit starts mid-line
      for (unsigned o = firstCand;
           o <= numOutLines && outStarts[o - 1] < editOutEnd; ++o)
        if (originOf[o - 1] == 0)
          originOf[o - 1] = anchorLine;
      outPos = editOutEnd;
      origPos = n.offset + n.length;
      ++netIdx;
      if (origPos > original.size())
        break;
    }
  }

  // presumed location of each original line (this consumes incoming
  // linemarkers)
  FileID mainFID = SM.getMainFileID();
  std::vector<unsigned> presumedLine(origStarts.size() + 1, 0);
  std::vector<std::string> presumedFile(origStarts.size() + 1);
  for (unsigned L = 1; L <= origStarts.size(); ++L) {
    SourceLocation loc = SM.translateLineCol(mainFID, L, 1);
    PresumedLoc ploc = SM.getPresumedLoc(loc);
    if (ploc.isValid()) {
      presumedLine[L] = ploc.getLine();
      presumedFile[L] = ploc.getFilename();
    } else {
      presumedLine[L] = L;
      presumedFile[L] = "<unknown>";
    }
  }

  // input marker lines are consumed above and must not pass through
  std::vector<bool> origIsMarker(origStarts.size() + 1, false);
  {
    std::vector<ScanState> origStates = scanStatesAtLineStarts(original);
    for (unsigned L = 1; L <= origStarts.size(); ++L) {
      unsigned end = L < origStarts.size() ? origStarts[L] : (unsigned)original.size();
      StringRef line = original.substr(origStarts[L - 1], end - origStarts[L - 1]);
      if (L - 1 < origStates.size() && origStates[L - 1] == ScanState::NORMAL &&
          isMarkerLine(line))
        origIsMarker[L] = true;
    }
  }

  std::vector<ScanState> outStates = scanStatesAtLineStarts(rewritten);

  // splice in the markers, simulating the consumer: a marker means "the
  // next line is line N of file F", every line advances the count by one
  result.clear();
  result.reserve(rewritten.size() + 256);
  // before any marker the mapping is the identity; the file is named the
  // way presumed locations name it with no directive in effect, so on-disk
  // and in-memory (later round) inputs agree
  PresumedLoc firstLoc = SM.getPresumedLoc(SM.getLocForStartOfFile(mainFID),
                                           /*UseLineDirectives=*/false);
  std::string consumerFile =
      firstLoc.isValid() ? firstLoc.getFilename() : "<unknown>";
  long consumerNext = 1; // line number the consumer gives the next line
  for (unsigned o = 1; o <= numOutLines; ++o) {
    unsigned end = o < numOutLines ? outStarts[o] : (unsigned)rewritten.size();
    StringRef lineText = rewritten.substr(outStarts[o - 1],
                                          end - outStarts[o - 1]);
    unsigned orig = claimedBy[o - 1];

    // drop stale input marker lines; their effect is already part of the
    // presumed locations, and the next claimed line re-syncs below
    if (orig != 0 && origIsMarker[orig])
      continue;

    unsigned intendedLine = 0;
    const std::string *intendedFile = nullptr;
    if (orig != 0) {
      intendedLine = presumedLine[orig];
      intendedFile = &presumedFile[orig];
    } else {
      unsigned originAt = originOf[o - 1];
      // only the first line of a run of inserted lines gets a marker, the
      // rest continue from it
      bool firstOfRun = o == 1 || claimedBy[o - 2] != 0 ||
                        originOf[o - 2] != originAt;
      if (originAt != 0 && firstOfRun) {
        intendedLine = presumedLine[originAt];
        intendedFile = &presumedFile[originAt];
      }
    }

    if (intendedLine != 0 &&
        (consumerNext != (long)intendedLine ||
         consumerFile != *intendedFile)) {
      // emit only at safe line starts; when unsafe, the mismatch persists
      // and the marker comes at the next safe line instead
      if (o - 1 < outStates.size() && outStates[o - 1] == ScanState::NORMAL) {
        result += "# ";
        result += std::to_string(intendedLine);
        result += " \"";
        result += *intendedFile;
        result += "\"\n";
        consumerNext = intendedLine;
        consumerFile = *intendedFile;
      }
    }

    result += lineText;
    ++consumerNext;
  }

  return true;
}

// Edit tracking (to not silently fail on conflicts).
//
// Transformers edit through a TrackedRewriter: each call is forwarded
// unchanged to the clang::Rewriter and recorded in an EditTracker, in
// coordinates of the unmodified main-file buffer. The log gives the
// conflict check (nonCommutingPairs) and the line marker mapping.
//
// The idea is similar to clang::tooling::Replacements, which was not used
// to make implementing a future rewrite algebra easier.

#pragma once

#include "clang/Basic/SourceManager.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/ADT/StringRef.h"

#include <string>
#include <utility>
#include <vector>

struct TrackedEdit {
  enum class Kind {
    INSERTION,   // length == 0
    REPLACEMENT,
    BLANK // whitespace replacement, identical geometry, newlines kept
  };

  std::string transformer;
  Kind kind;
  // start offset in the original main-file buffer
  unsigned offset;
  // number of original characters replaced, 0 for insertions
  unsigned length;
  // new text (after indentNewLines expansion)
  std::string text;
  // same-offset composition order for insertions
  bool insertAfter;
  // application order, 0-based
  unsigned seq;
  // original line the first line of `text` comes from (function clones),
  // 0 if none better than the line at `offset`
  unsigned originLine;

  bool isInsertion() const { return kind == Kind::INSERTION; }
  bool isBlank() const { return kind == Kind::BLANK; }
};

// original range [offset, offset+length) becomes `text`
struct NetEdit {
  unsigned offset;
  unsigned length;
  std::string text;
  // see TrackedEdit::originLine; 0 if none was given
  unsigned originLine;
};

class EditTracker {
public:
  void record(llvm::StringRef transformer, TrackedEdit::Kind kind,
              unsigned offset, unsigned length, llvm::StringRef text,
              bool insertAfter, unsigned originLine = 0) {
    TrackedEdit e;
    e.transformer = transformer.str();
    e.kind = kind;
    e.offset = offset;
    e.length = length;
    e.text = text.str();
    e.insertAfter = insertAfter;
    e.seq = (unsigned)edits.size();
    e.originLine = originLine;
    edits.push_back(std::move(e));
  }

  const std::vector<TrackedEdit> &getEdits() const { return edits; }
  size_t size() const { return edits.size(); }
  bool empty() const { return edits.empty(); }

  // the pairs of edits whose result depends on application order. within
  // one transformer some edits commute: identical replacements,
  // several insertions at one offset, overlapping blanks
  // (their union), insertions at the edges of a replaced range.
  std::vector<std::pair<TrackedEdit, TrackedEdit> > nonCommutingPairs() const;

  // the log as a sorted list of pairwise disjoint edits with the same
  // effect; only meaningful when nonCommutingPairs() is empty. `original`
  // is the unmodified main-file buffer.
  std::vector<NetEdit> disjointEdits(llvm::StringRef original) const;

  // applies disjointEdits() to the original buffer; used to check the log
  // reproduces the RewriteBuffer output
  std::string apply(llvm::StringRef original) const;

private:
  // the log with repeated identical replacements applied once
  std::vector<TrackedEdit> editsWithoutRepeats() const;

  std::vector<TrackedEdit> edits;
};

// records every main-file edit under the active transformer name.
class TrackedRewriter {
public:
  TrackedRewriter(clang::Rewriter &rewriter, EditTracker &tracker)
      : rewriter(rewriter), tracker(tracker), activeTransformer("<unknown>") {}

  void setActiveTransformer(llvm::StringRef name) {
    activeTransformer = name.str();
  }

  clang::SourceManager &getSourceMgr() const { return rewriter.getSourceMgr(); }
  const clang::LangOptions &getLangOpts() const { return rewriter.getLangOpts(); }

  // originLine: see TrackedEdit
  bool InsertText(clang::SourceLocation Loc, llvm::StringRef Str,
                  bool InsertAfter = true, bool indentNewLines = false,
                  unsigned originLine = 0);

  bool InsertTextAfter(clang::SourceLocation Loc, llvm::StringRef Str) {
    return InsertText(Loc, Str);
  }

  bool InsertTextBefore(clang::SourceLocation Loc, llvm::StringRef Str) {
    return InsertText(Loc, Str, false);
  }

  bool InsertTextAfterToken(clang::SourceLocation Loc, llvm::StringRef Str);

  bool ReplaceText(clang::SourceRange range, llvm::StringRef NewStr);

  // replaces the token range with whitespace of identical geometry
  // keeps newlines
  bool BlankText(clang::SourceRange range);

  // same, for the character range [B, E)
  bool BlankChars(clang::SourceLocation B, clang::SourceLocation E);

  std::string getRewrittenText(clang::SourceRange Range) const {
    return rewriter.getRewrittenText(Range);
  }

private:
  // records an edit if Loc is in the main file
  void recordAt(clang::SourceLocation Loc, unsigned extraOffset,
                TrackedEdit::Kind kind, unsigned length, llvm::StringRef text,
                bool insertAfter, unsigned originLine = 0);

  clang::Rewriter &rewriter;
  EditTracker &tracker;
  std::string activeTransformer;
};

// Expand C macros inside ACSL annotation comments (//@ ..., /*@ ... */),
// using the macro definitions in effect at the comment's position

#pragma once

#include "clang/Lex/Preprocessor.h"

#include <string>
#include <utility>
#include <vector>

class InvocationRecorder;

// an annotation comment in the main file that was expanded or skipped
struct AnnotationExpansion {
  clang::FileID fid;
  unsigned offset;          // of the comment start
  std::string originalText; // the whole comment
  std::string expandedText;
  std::string skipReason;   // set if the comment is kept verbatim
};

// while the preprocessor lexes the comments, expands the macros of each
// annotation by entering its tokens into the preprocessor; only the text
// of the macro invocations changes, so the comment keeps its lines
class AnnotationMacroExpander : public clang::CommentHandler {
public:
  explicit AnnotationMacroExpander(clang::Preprocessor &PP);

  // unregisters the comment handler; call while PP is still alive
  void detach();

  bool HandleComment(clang::Preprocessor &PP,
                     clang::SourceRange comment) override;

  const std::vector<AnnotationExpansion> &results() const { return annots; }

private:
  std::vector<clang::Token> lexBody(bool isBlock);
  std::string expand(const std::vector<clang::Token> &toks, bool isBlock,
                     std::string &text);
  bool lexExpanded(const std::vector<clang::Token> &toks,
                   std::vector<clang::Token> &out);
  std::vector<std::pair<unsigned, unsigned> >
  invocationRanges(const std::vector<clang::Token> &out);
  std::string spell(const std::vector<clang::Token> &toks,
                    const std::vector<clang::Token> &produced, unsigned b,
                    unsigned e);
  unsigned offsetOf(const clang::Token &t) const;

  clang::Preprocessor &PP;
  // owned by PP, so only used while PP calls HandleComment
  InvocationRecorder *recorder;
  std::vector<AnnotationExpansion> annots;

  // the annotation being expanded, as offsets into its file buffer
  clang::FileID fid;
  llvm::StringRef buf;
  unsigned commentBegin = 0, bodyBegin = 0, bodyEnd = 0;
};

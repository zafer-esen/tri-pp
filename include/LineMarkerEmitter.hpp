// Emission of GNU linemarkers ("# <line> \"<file>\"") at the output, 
// mapping back to the original.
//
// The mapping is derived from EditTracker's net edits, in original
// main-file coordinates. Marker values take into account existing ones
// for example possibly from cpp.

#pragma once

#include "TrackedRewriter.hpp"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/StringRef.h"

#include <string>

// `original`: unrewritten main-file buffer, 
// `rewritten`: RewriteBuffer's result. 
// The tracked edits are first checked to reproduce `rewritten` exactly; 
// on mismatch the function returns false and sets `error`.
bool emitLineMarkers(llvm::StringRef original, llvm::StringRef rewritten,
                     const EditTracker &tracker, clang::SourceManager &SM,
                     std::string &result, std::string &error);

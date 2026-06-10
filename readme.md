# TriCera Preprocessor

A clang tool to preprocess C files before feeding them to TriCera.

A build script (mk) is provided, which first downloads the prebuilt LLVM
dependency (LLVM 18.1.8), then builds the project against it, bundling the
needed LLVM/clang static libraries into the executable `tri-pp`.

On Linux the build must run inside the pinned Ubuntu 18.04 image (use
`docker-build.sh`, which builds the included `Dockerfile`): the prebuilt LLVM
static libraries were compiled with that era's libstdc++, and building against
a newer one produces an ABI mismatch that crashes the binary as soon as it
calls into libclang. The old glibc baseline also makes the resulting binary
run on a wide range of Linux systems. On macOS the build runs natively with
Apple's clang and `libtool` (`./mk`).

Supported OS / architectures: Linux x86_64, Linux arm64, macOS arm64. Intel
macOS (x86_64) has no native LLVM 18.1.8 package and is not currently built;
Windows users can run the Linux x86_64 binary under WSL. Prebuilt binaries are
attached to the GitHub releases (`tri-pp-linux-x64`, `tri-pp-linux-arm64`,
`tri-pp-macos-arm64`).

# Usage
`./tri-pp --help` to see version number and usage information of the tool.

# Building
The build uses `cmake`. The LLVM 18.1.8 dependency is downloaded automatically;
the only system packages needed are `zlib1g-dev` and `libncurses-dev` (the
latter provides the static `libtinfo.a` that LLVM's terminal-colour detection
links). On Linux run `./docker-build.sh` (which builds in the Ubuntu 18.04
container described above); on macOS run `./mk` directly.

You can also have a look at the GitHub Action file at `.github/workflows` for the build setup on supported OS.

If the build was successful `tri-pp` binary will be copied to the root directory. To test the tool try the command
```
./tri-pp in.c -o out.c -- -xc ; cat out.c
```
which should print out the preprocessed file.

# Implementation
Read on if you are interested in the implementation.

tri-pp runs its transformers in rounds, one transformer per round. Each
round runs on a fresh parse of the previous round's text (held in memory),
so the transformers cannot conflict: every round sees the earlier work as
plain source text. A round that rewrites nothing reuses its parse for the
next round, so the extra parses cost little. All edits are still tracked,
and edits whose combined effect depends on application order (a bug within
a single transformer) are reported loudly, with exit code 4, instead of
being composed silently.

Removed source text (typedefs, unused declarations, extracted for-loop
declarations, ...) is replaced by whitespace of identical geometry: newlines
are kept and every other character becomes a space. Byte offsets and line
numbers of the surrounding code therefore stay stable.

With `--line-markers`, the produced file carries GNU linemarkers
(`# <line> "<file>"`) wherever the line mapping between input and output
jumps, so a consumer can map every output line back to an authentic line of
the original input. Inputs that already contain linemarkers (from cpp, or
from an earlier tri-pp stage; TriCera invokes tri-pp up to three times in a
row) are consumed and re-emitted in the original coordinates. An input that
tri-pp does not rewrite is passed through unchanged.

With `--facts <path>`, a small YAML file is written next to the output
stating facts about the produced program.

The transformer components:

## UsedFunctionAndTypeCollector
It tries to find the main function, and follows all function calls from main
(and from the called functions) until a fixed point in order to determine
used functions and types.

## TypedefRemover
Removes all typedefs. If a typedef contains a record declaration, it is
converted to a regular record declaration.
E.g. `typedef struct {...} S;` is converted to `struct S {...};`.

## UnusedDeclCommenter
Removes all unused declarations, which include both function and type
declarations. It uses the results from "UsedFunctionAndTypeCollector".

This also removes some function declarations (or their implementations)
coming from included libraries, which are handled by TriCera natively. Examples
are `__assert_fail`, `__assert_perror_fail`, `__assert` or SV-COMP functions
such as `reach_error` (https://sv-comp.sosy-lab.org/2021/benchmarks.php).

## TypeCanoniser
Replaces all types with their canonical versions in the source code.
If a type is a typedef type, it is replaced with the completely desugared
(i.e., canonical) type.
E.g. for the typedefs `typedef T1 T2; typedef T2 * T3;`, `T3 * x` is replaced
with `T1 ** x`.

## ForLoopStmtExtractor
Moves declarations from inside the for loop declaration to outside. E.g.,
`for (int i = 0, j = 0; ...){...}` becomes 
`{int i = 0, j = 0; for (; ...){...}}`
TriCera cannot parse the former directly.

# Known Issues
- Preprocessor might not always work correctly when using non-standard C that
  TriCera accepts. E.g. `thread {...}` blocks are not parsed correctly by the
  preprocessor, so TypeCanoniser does not work inside these blocks (i.e., use
  canonical types in these situations).

# Links
Tutorial project on clang::libtooling
https://github.com/banach-space/clang-tutor

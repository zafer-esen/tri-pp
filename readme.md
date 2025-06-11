# TriCera Preprocessor

A clang tool to preprocess C files before feeding them to TriCera.

A build script (mk) is provided for Linux, which first downloads the prebuilt LLVM dependency, then builds the project against it, finally producing a static binary.

Currently only Linux is supported, there are plans to add support for other operating systems.

# Usage
`./tri-pp --help` to see version number and usage information of the tool.

# Building
The build uses `cmake`, has LLVM 13.0.0 and a few packages (`zlib1g-dev`, `libtool-bin`, and possibly others) as dependencies. If everything is set up correctly, call `./mk` to build and produce the executable `tri-pp`.
If you installed LLVM-13 but `mk` complains that it cannot find the header files, you can try to directly point to them. E.g.,:
```
CMAKE_CXX_FLAGS="-I/opt/llvm-13/include" ./mk
```

# Implementation
Read on if you are interested in the implementation.

tri-pp has several components which are called sequentially.

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

This also comments out some function declarations (or their implementations) 
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
- UnusedDeclCommenter will comment out whole lines that the declaration is in. If
  another declaration is in the same line, it will also be commented out. This
  should usually not be an issue if the other declarations are using the same base
  type, because UnusedDeclCommenter only comments out a declaration (except
  function declarations) if the type of that declaration is never seen in the
  program.

  E.g. For `T1 x; T2 y;`, if either T1 or T2 is not a seen type, both declarations
  will be commented out since they are in the same line.

- TypedefRemover comments out some parts of the record declarations using
  C-style `/*...*/` comments. If the commented-out region has nested comments
  this might lead to unparseable output.
  
- Preprocessor might not always work correctly when using non-standard C that
  TriCera accepts. E.g. `thread {...}` blocks are not parsed correctly by the
  preprocessor, so TypeCanoniser does not work inside these blocks (i.e., use
  canonical types in these situations).

# Links
Tutorial project on clang::libtooling
https://github.com/banach-space/clang-tutor

cmake_minimum_required(VERSION 3.13)
project(tricera-preprocessors)
set(PROJECT_VERSION "0.3.2")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/include/TriceraConfig.hpp.in"
  "${CMAKE_CURRENT_SOURCE_DIR}/include/TriceraConfig.hpp")

#===============================================================================
# 1. LOAD LLVM CONFIGURATION
#===============================================================================
# Set this to a valid LLVM installation dir
#set(CT_LLVM_INSTALL_DIR "" CACHE PATH "LLVM installation directory")

#include_directories(SYSTEM "${LLVM_INCLUDE_DIRS};${CLANG_INCLUDE_DIRS}")
include_directories(
  "${CMAKE_CURRENT_SOURCE_DIR}/llvm/build/_deps/libclang_prebuilt-src/include"
  )

#===============================================================================
# 2. TRICERA PREPROCESSOR BUILD CONFIGURATION
#===============================================================================
# Use the same C++ standard as LLVM does (LLVM >= 16 headers require C++17)
set(CMAKE_CXX_STANDARD 17 CACHE STRING "")

#if(NOT CMAKE_BUILD_TYPE)
#    set(CMAKE_BUILD_TYPE Release)
#endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -Wuninitialized")

# TOOLS
# ===================
set(TRICERA_TOOLS
    tri-pp
)

set(tri-pp_SOURCES
  TriceraPreprocessorMain.cpp
  lib/TrackedRewriter.cpp
  lib/LineMarkerEmitter.cpp
  lib/FactsCollector.cpp
  lib/TriceraPreprocessor.cpp
  lib/UsedFunctionAndTypeCollector.cpp
  lib/TypedefRemover.cpp
  lib/UnusedDeclCommenter.cpp
  lib/TypeCanoniser.cpp
  lib/CXXToCPlusTranslator.cpp
  lib/ForLoopStmtExtractor.cpp
  lib/CharRewriter.cpp
  lib/Utilites.cpp
  lib/Determinizer.cpp
  lib/ExecutionCountAnalyzer.cpp
  lib/NondetLoopGuardRewriter.cpp
  lib/UniqueCallSiteTransformer.cpp
)

# CONFIGURATION
# ===================
# LLVMSupport needs terminfo (setupterm etc.) for terminal color detection.
# On macOS the system libncurses is stable across machines and is linked
# dynamically; on Linux libtinfo.a is linked statically, since a dynamic
# libtinfo would tie the shipped binary to the build host's distro.
if(APPLE)
  find_library(TERMINFO_LIBRARY NAMES ncurses curses)
  # the macOS LLVM prebuilt enables zstd compression in LLVMSupport; link the
  # static libzstd.a (from Homebrew) so the binary has no Homebrew runtime dep
  find_library(ZSTD_STATIC_LIBRARY NAMES libzstd.a
    HINTS /opt/homebrew/opt/zstd/lib /usr/local/opt/zstd/lib)
  if(NOT ZSTD_STATIC_LIBRARY)
    message(FATAL_ERROR "static libzstd.a not found; brew install zstd")
  endif()
else()
  set(SAVED_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})
  set(CMAKE_FIND_LIBRARY_SUFFIXES .a)
  find_library(TERMINFO_LIBRARY NAMES tinfo ncurses)
  set(CMAKE_FIND_LIBRARY_SUFFIXES ${SAVED_FIND_LIBRARY_SUFFIXES})
  if(NOT TERMINFO_LIBRARY)
    message(FATAL_ERROR "static terminfo (libtinfo.a/libncurses.a) not found; install libncurses-dev")
  endif()
endif()

foreach( tool ${TRICERA_TOOLS} )
# Create a library/executable corresponding to 'plugin'
    add_executable(
      ${tool}
      ${${tool}_SOURCES}
            include/Determinizer.hpp
    )

#    add_library(${tool} SHARED
#      ${${tool}_SOURCES}
#    )

# Link with libclangTooling

    if(APPLE)
        set(LIBCLANG_BUNDLED_PATH "${CMAKE_CURRENT_SOURCE_DIR}/llvm/build/libclang_bundled.a")
    else()
        set(LIBCLANG_BUNDLED_PATH "${CMAKE_CURRENT_SOURCE_DIR}/llvm/build/_all_archives/libclang_bundled.a")
endif()

#    include_directories(${CMAKE_SOURCE_DIR}/llvm/include)
    target_link_libraries(
      ${tool}
#      clangTooling
      "${CMAKE_CURRENT_SOURCE_DIR}/llvm/build/_deps/libclang_prebuilt-src/lib/libclangTooling.a"
      "${LIBCLANG_BUNDLED_PATH}"
      m
      dl
      pthread
      z
      ${TERMINFO_LIBRARY}
      ${ZSTD_STATIC_LIBRARY}
    )

# Configure include directories for 'tool'
    target_include_directories(
      ${tool}
      PRIVATE
      "${CMAKE_CURRENT_SOURCE_DIR}/include"
      )

    set_target_properties(${tool} PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/preprocessor/build)
endforeach()

#===============================================================================
# 3. ADD THE TARGET
#===============================================================================
# add_library(tricera-preprocessor SHARED TriceraPreprocessorMain.cpp)

# Allow undefined symbols in shared objects on Darwin (this is the default
# behaviour on Linux)

# add_executable(tricera-preprocessor TriceraPreprocessorMain.cpp)

# target_link_libraries(tricera-preprocessor clangTooling)
 INSTALL(TARGETS tri-pp DESTINATION ${CMAKE_CURRENT_SOURCE_DIR})

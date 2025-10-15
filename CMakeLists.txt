cmake_minimum_required(VERSION 3.5)
project(tricera-preprocessors)
set(PROJECT_VERSION "0.2.0")
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
# Use the same C++ standard as LLVM does
set(CMAKE_CXX_STANDARD 14 CACHE STRING "")

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
  lib/TriceraPreprocessor.cpp
  lib/UsedFunctionAndTypeCollector.cpp
  lib/TypedefRemover.cpp
  lib/UnusedDeclCommenter.cpp
  lib/TypeCanoniser.cpp
  lib/ForLoopStmtExtractor.cpp
  lib/CharRewriter.cpp
  lib/Utilites.cpp
  lib/Determinizer.cpp
  lib/ExecutionCountAnalyzer.cpp
  lib/NondetLoopGuardRewriter.cpp
  lib/UniqueCallSiteTransformer.cpp
  lib/FuncPtrDevirtualizer.cpp
)

# CONFIGURATION
# ===================
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

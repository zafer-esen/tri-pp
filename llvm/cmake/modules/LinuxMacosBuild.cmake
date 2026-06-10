# Downloads the official LLVM/clang prebuilt for this host and merges its
# static component archives into a single bundle for the tri-pp link. Nothing
# is compiled here; tri-pp uses the libTooling C++ API only.

set(LLVM_RELEASE 18.1.8)
set(LLVM_BASE_URL
  https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_RELEASE})

# 18.1.8 is the newest release whose packages ship native static archives for
# all targets below; the 19+ "LLVM-*" packages hold LLVM bitcode (LTO) that
# GNU ld cannot link. The package names are per-target rather than one scheme.
if(APPLE)
  if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
    set(LIBCLANG_PREBUILT_URL ${LLVM_BASE_URL}/clang+llvm-${LLVM_RELEASE}-arm64-apple-macos11.tar.xz)
  else()
    message(FATAL_ERROR
      "No prebuilt LLVM ${LLVM_RELEASE} exists for x86_64 macOS; "
      "build on an arm64 macOS host.")
  endif()
else()
  if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64")
    set(LIBCLANG_PREBUILT_URL ${LLVM_BASE_URL}/clang+llvm-${LLVM_RELEASE}-aarch64-linux-gnu.tar.xz)
  else()
    set(LIBCLANG_PREBUILT_URL ${LLVM_BASE_URL}/clang+llvm-${LLVM_RELEASE}-x86_64-linux-gnu-ubuntu-18.04.tar.xz)
  endif()
endif()

include(Download)
message(STATUS "Downloading prebuilt LLVM/clang ${LLVM_RELEASE} (~1.5GB compressed, several minutes) ...")
# The download name must stay 'libclang_prebuilt' so the extracted tree lands at
# _deps/libclang_prebuilt-src/{include,lib}, the paths the root CMakeLists.txt
# hardcodes for its include and link directories.
set(LIBCLANG_PREBUILT_DIR)
download(libclang_prebuilt ${LIBCLANG_PREBUILT_URL} LIBCLANG_PREBUILT_DIR)

# Every clang and LLVM component archive; the final link pulls only the
# members it needs, so the superset costs nothing in the binary. The prebuilt
# ships no monolithic libLLVM.a, so no duplicate symbols are merged, and
# MLIR/Flang/lld archives match neither pattern.
file(GLOB LIBCLANG_PREBUILT_LIBS
  "${LIBCLANG_PREBUILT_DIR}/lib/libclang*.a"
  "${LIBCLANG_PREBUILT_DIR}/lib/libLLVM*.a"
  )
list(LENGTH LIBCLANG_PREBUILT_LIBS LIBCLANG_PREBUILT_LIB_COUNT)
if(LIBCLANG_PREBUILT_LIB_COUNT EQUAL 0)
  message(FATAL_ERROR
    "No static archives under ${LIBCLANG_PREBUILT_DIR}/lib -- the prebuilt "
    "layout may have changed.")
endif()
message(STATUS "Bundling ${LIBCLANG_PREBUILT_LIB_COUNT} static archives into libclang_bundled.a")

include(GatherArchives)

if(APPLE)
  # Apple's libtool merges static archives into a normal archive for ld64
  add_custom_target(
    libclang_bundled ALL
    COMMAND ${CMAKE_LIBTOOL} -static -o libclang_bundled.a ${LIBCLANG_PREBUILT_LIBS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
else()
  # Copy the archives side by side and splice their members into one thin
  # archive; GNU ld rescans a single archive until no more members are pulled,
  # so the circular dependencies among the libs resolve without --start-group.
  gatherArchives(
    ALL_ARCHIVES_DIRECTORY
    ALL_ARCHIVE_NAMES
    ALL_ARCHIVE_PATHS
    ${LIBCLANG_PREBUILT_LIBS}
  )
  add_custom_target(
    gather_archives ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${ALL_ARCHIVES_DIRECTORY}
    COMMAND ${CMAKE_COMMAND} -E copy ${LIBCLANG_PREBUILT_LIBS} ${ALL_ARCHIVES_DIRECTORY}
  )
  add_custom_target(
    libclang_bundled ALL
    COMMAND ${CMAKE_AR} crsT libclang_bundled.a ${ALL_ARCHIVE_NAMES}
    WORKING_DIRECTORY ${ALL_ARCHIVES_DIRECTORY}
    DEPENDS gather_archives
  )
endif()

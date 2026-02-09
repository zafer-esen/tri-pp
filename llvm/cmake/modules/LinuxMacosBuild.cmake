if(APPLE)
  if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
      # On ARM64 macOS, use Homebrew LLVM@13 which has all required headers
      # The HOMEBREW_LLVM_PATH env var is set by the CI workflow
      if(DEFINED ENV{HOMEBREW_LLVM_PATH} AND IS_DIRECTORY "$ENV{HOMEBREW_LLVM_PATH}")
         message(STATUS "Using Homebrew LLVM at: $ENV{HOMEBREW_LLVM_PATH}")
         set(LIBCLANG_PREBUILT_DIR "$ENV{HOMEBREW_LLVM_PATH}")
         set(USE_HOMEBREW_LLVM TRUE)
      else()
         # Fallback: try standard Homebrew path
         if(IS_DIRECTORY "/opt/homebrew/opt/llvm@13")
            message(STATUS "Using Homebrew LLVM at default path: /opt/homebrew/opt/llvm@13")
            set(LIBCLANG_PREBUILT_DIR "/opt/homebrew/opt/llvm@13")
            set(USE_HOMEBREW_LLVM TRUE)
         else()
            message(FATAL_ERROR "Homebrew LLVM@13 not found. Please install with: brew install llvm@13")
         endif()
      endif()
      
      # Use Z3 (4.12.1) prebuilt for ARM64.
      set(Z3_PREBUILT_URL https://github.com/Z3Prover/z3/releases/download/z3-4.12.1/z3-4.12.1-arm64-osx-11.0.zip)
  else()
      set(LIBCLANG_PREBUILT_URL https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.0/clang+llvm-13.0.0-x86_64-apple-darwin.tar.xz)
      set(Z3_PREBUILT_URL https://github.com/Z3Prover/z3/releases/download/z3-4.8.7/z3-4.8.7-x64-osx-10.14.6.zip)
      set(USE_HOMEBREW_LLVM FALSE)
  endif()
else()
  set(LIBCLANG_PREBUILT_URL https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.0/clang+llvm-13.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz)
  set(Z3_PREBUILT_URL https://github.com/Z3Prover/z3/releases/download/z3-4.8.7/z3-4.8.7-x64-ubuntu-16.04.zip)
  set(USE_HOMEBREW_LLVM FALSE)
endif()

set(CLANG_SOURCES_URL https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.0/clang-13.0.0.src.tar.xz)
set(NCURSES_SOURCES_URL https://ftp.gnu.org/pub/gnu/ncurses/ncurses-6.2.tar.gz)

include(Download)
message(STATUS "Downloading ncurses sources, z3 & libclang; this is ~500MB, please be patient...")
set(NCURSES_SOURCE_DIR)
download(ncurses_sources ${NCURSES_SOURCES_URL} NCURSES_DOWNLOAD_DIR)
set(LIBCLANG_SOURCES_DIR)
download(clang_sources ${CLANG_SOURCES_URL} LIBCLANG_SOURCES_DIR)

# Download prebuilt LLVM only if not using Homebrew
if(NOT USE_HOMEBREW_LLVM)
  set(LIBCLANG_PREBUILT_DIR)
  download(libclang_prebuilt ${LIBCLANG_PREBUILT_URL} LIBCLANG_PREBUILT_DIR)
endif()

set(Z3_PREBUILT_DIR)
download(z3_prebuilt ${Z3_PREBUILT_URL} Z3_PREBUILT_DIR)

include(ExternalProject)
ExternalProject_Add(ncurses
  SOURCE_DIR ${NCURSES_DOWNLOAD_DIR}
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --enable-rpath --prefix=${CMAKE_INSTALL_PREFIX} --with-shared --with-static --with-normal --without-debug --without-ada --enable-widec --disable-pc-files --with-cxx-binding --without-cxx-shared --with-abi-version=5
  BUILD_COMMAND make
  INSTALL_COMMAND ""
  )


if(APPLE AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
  # Fix for Z3 zip structure (contains top-level directory)
  if(IS_DIRECTORY "${Z3_PREBUILT_DIR}/z3-4.12.1-arm64-osx-11.0")
     message(STATUS "DEBUG: Found Z3 ARM64 subdirectory, setting Z3_PREBUILT_DIR.")
     set(Z3_PREBUILT_DIR "${Z3_PREBUILT_DIR}/z3-4.12.1-arm64-osx-11.0")
  endif() 
  
  set(Z3_SHARED_LIB ${Z3_PREBUILT_DIR}/bin/libz3.dylib)
  set(Z3_STATIC_LIB ${Z3_PREBUILT_DIR}/bin/libz3.a)
  set(Z3_INCLUDE_DIR ${Z3_PREBUILT_DIR}/include)
else()
  if(APPLE)
     if(IS_DIRECTORY "${Z3_PREBUILT_DIR}/z3-4.8.7-x64-osx-10.14.6")
        set(Z3_PREBUILT_DIR "${Z3_PREBUILT_DIR}/z3-4.8.7-x64-osx-10.14.6")
     endif()
     set(Z3_SHARED_LIB ${Z3_PREBUILT_DIR}/bin/libz3.dylib)
     set(Z3_STATIC_LIB ${Z3_PREBUILT_DIR}/bin/libz3.a)
  else()
     # Ubuntu
     if(IS_DIRECTORY "${Z3_PREBUILT_DIR}/z3-4.8.7-x64-ubuntu-16.04")
        set(Z3_PREBUILT_DIR "${Z3_PREBUILT_DIR}/z3-4.8.7-x64-ubuntu-16.04")
     endif()
     set(Z3_SHARED_LIB ${Z3_PREBUILT_DIR}/bin/libz3.so)
     set(Z3_STATIC_LIB ${Z3_PREBUILT_DIR}/bin/libz3.a)
  endif()
endif()


list(APPEND CMAKE_MODULE_PATH "${LIBCLANG_PREBUILT_DIR}/lib/cmake/clang")
list(APPEND CMAKE_MODULE_PATH "${LIBCLANG_PREBUILT_DIR}/lib/cmake/llvm")
if(APPLE AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
   list(APPEND CMAKE_MODULE_PATH "${LIBCLANG_PREBUILT_DIR}/lib/cmake/clang")
   list(APPEND CMAKE_MODULE_PATH "${LIBCLANG_PREBUILT_DIR}/lib/cmake/llvm")
endif()

message(STATUS "DEBUG: LIBCLANG_PREBUILT_DIR=${LIBCLANG_PREBUILT_DIR}")
message(STATUS "DEBUG: CMAKE_MODULE_PATH=${CMAKE_MODULE_PATH}")


list(APPEND CMAKE_MODULE_PATH "${LIBCLANG_SOURCES_DIR}/cmake/modules")
include(LibClangBuild)
include(HandleLLVMOptions)
include(AddLLVM)
include(AddClang)
include(GatherArchives)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")

get_libclang_sources_and_headers(
  ${LIBCLANG_SOURCES_DIR}
  ${LIBCLANG_PREBUILT_DIR}
  LIBCLANG_SOURCES
  LIBCLANG_ADDITIONAL_HEADERS
  LIBCLANG_PREBUILT_LIBS
  )

include_directories(${LIBCLANG_PREBUILT_DIR}/include)
if(APPLE AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
   include_directories(${Z3_INCLUDE_DIR})
endif()

ExternalProject_Get_Property(ncurses BINARY_DIR)
set(NCURSES_BINARY_DIR ${BINARY_DIR})
set(NCURSES_SHARED_LIB)
if(APPLE)
  set(NCURSES_SHARED_LIB ${NCURSES_BINARY_DIR}/lib/libncursesw.dylib ${NCURSES_BINARY_DIR}/lib/libncursesw.5.dylib)
else()
  set(NCURSES_SHARED_LIB ${NCURSES_BINARY_DIR}/lib/libncursesw.so ${NCURSES_BINARY_DIR}/lib/libncursesw.so.5 ${NCURSES_BINARY_DIR}/lib/libncursesw.so.5.9)
endif()
unset(BINARY_DIR)

add_clang_library(libclang
  SHARED
  OUTPUT_NAME clang
  ${LIBCLANG_SOURCES}
  ADDITIONAL_HEADERS ${LIBCLANG_ADDITIONAL_HEADERS}
  LINK_LIBS
  ${LIBCLANG_PREBUILT_LIBS} ${NCURSES_SHARED_LIB} dl pthread z
  LINK_COMPONENTS ${LLVM_TARGETS_TO_BUILD}
  DEPENDS ncurses
  )

add_clang_library(libclang_static
  STATIC
  OUTPUT_NAME clang_static
  ${LIBCLANG_SOURCES}
  ADDITIONAL_HEADERS ${LIBCLANG_ADDITIONAL_HEADERS}
  DEPENDS ncurses
  )

set_target_properties(libclang PROPERTIES VERSION 13)

if(APPLE)
  set(LIBCLANG_LINK_FLAGS " -Wl,-compatibility_version -Wl,1")
  set_property(TARGET libclang APPEND_STRING PROPERTY
               LINK_FLAGS ${LIBCLANG_LINK_FLAGS})
else()
  set_target_properties(libclang
    PROPERTIES
    DEFINE_SYMBOL _CINDEX_LIB_)
endif()

if(APPLE)
  add_custom_target(
    libclang_bundled ALL
    COMMAND ${CMAKE_LIBTOOL} -static -o libclang_bundled.a
              ${CMAKE_CURRENT_BINARY_DIR}/libclang_static.a
              ${LIBCLANG_PREBUILT_LIBS}
              ${NCURSES_BINARY_DIR}/lib/libncursesw.a
              ${Z3_STATIC_LIB}
    DEPENDS ncurses libclang libclang_static
  )
else()
  gatherArchives(
    ALL_ARCHIVES_DIRECTORY
    ALL_ARCHIVE_NAMES
    ALL_ARCHIVE_PATHS
    ${CMAKE_CURRENT_BINARY_DIR}/libclang_static.a
    ${LIBCLANG_PREBUILT_LIBS}
    ${NCURSES_BINARY_DIR}/lib/libncursesw.a
    ${Z3_STATIC_LIB}
  )
  add_custom_target(
    gather_archives ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${ALL_ARCHIVES_DIRECTORY}
    COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_BINARY_DIR}/libclang_static.a
      ${LIBCLANG_PREBUILT_LIBS}
      ${NCURSES_BINARY_DIR}/lib/libncursesw.a
      ${Z3_STATIC_LIB}
      ${ALL_ARCHIVES_DIRECTORY}
    DEPENDS ncurses libclang libclang_static
  )
  add_custom_target(
    libclang_bundled ALL
    COMMAND ${CMAKE_AR} crsT libclang_bundled.a ${ALL_ARCHIVE_NAMES}
    WORKING_DIRECTORY ${ALL_ARCHIVES_DIRECTORY}
    DEPENDS gather_archives
  )
endif()

set(MAKEFILE_LIBCLANG_INCLUDE ${CMAKE_INSTALL_PREFIX}/include)
if(APPLE)
  set(MAKEFILE_LIBCLANG_INCLUDE "${MAKEFILE_LIBCLANG_INCLUDE} -I${CMAKE_OSX_SYSROOT}/usr/include")
endif()
set(MAKEFILE_LIBCLANG_LIBDIR ${CMAKE_INSTALL_PREFIX}/lib)

if(APPLE)
  set(LIBCLANG_INSTALL_LIBS
    ${CMAKE_CURRENT_BINARY_DIR}/libclang_bundled.a
    ${Z3_STATIC_LIB}
    ${Z3_SHARED_LIB}
    ${NCURSES_BINARY_DIR}/lib/libncursesw.a
    ${NCURSES_SHARED_LIB}
  )
else()
  set(LIBCLANG_INSTALL_LIBS
    ${ALL_ARCHIVES_DIRECTORY}/libclang_bundled.a
    ${ALL_ARCHIVE_PATHS}
    ${Z3_SHARED_LIB}
    ${NCURSES_SHARED_LIB}
  )
endif()

#set(LIBCLANG_INSTALL_LIBS
#  ${ALL_ARCHIVES_DIRECTORY}/libclang_bundled.a
#  ${ALL_ARCHIVE_PATHS}
#  ${Z3_SHARED_LIB}
#  ${NCURSES_SHARED_LIB}
#  )

#install(FILE ${ALL_ARCHIVES_DIRECTORY}/libclang_bundled.a DESTINATION llvm/lib)
#install(DIRECTORY ${LIBCLANG_PREBUILT_DIR}/include DESTINATION llvm/build/include)

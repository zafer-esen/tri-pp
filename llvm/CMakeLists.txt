cmake_minimum_required(VERSION 3.13)
if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW) # use extraction time for downloaded archives
endif()
project(libclang-static-build)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")
if(NOT (UNIX))
  message(FATAL_ERROR "This build currenly works only with Linux and MacOS.")
endif()
# macOS merges the static archives with Apple's libtool (cctools); Linux uses
# the compiler's ar (CMAKE_AR), so libtool is not required there.
if(APPLE)
  find_program(CMAKE_LIBTOOL libtool)
  if(NOT CMAKE_LIBTOOL)
    message(FATAL_ERROR "Apple 'libtool' (cctools) is necessary for merging static archives")
  endif()
endif()
include(LinuxMacosBuild)

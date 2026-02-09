#!/bin/bash
set -e

buildDir=build
llvmBuildDir=llvm/build

# download and build llvm dependency
mkdir -p ${llvmBuildDir}
cmake ${llvmBuildDir}/.. -B ${llvmBuildDir}
make -C ${llvmBuildDir} -j 3

# build the actual project
mkdir -p ${buildDir}
cmake ${buildDir}/.. -B ${buildDir}
make install -C ${buildDir} -j 3

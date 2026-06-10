# The build must run on the same toolchain era as the prebuilt LLVM 18.1.8
# static libraries, or the libstdc++ ABI differs (some std types change
# trivial-copyability, which changes how they are passed by value) and the
# binary crashes as soon as it calls into libclang. The per-target packages
# were built on different distros, so the base image is per-arch:
#   x86_64  -> ubuntu:18.04 (package is *-ubuntu-18.04; old glibc also gives
#              the shipped binary a wide compatibility range)
#   aarch64 -> ubuntu:22.04 (package was built with a newer GCC: it needs the
#              GCC >= 9 libstdc++ and the aarch64 outline-atomics helpers,
#              which 18.04's toolchain does not provide)
ARG BASE=ubuntu:18.04
FROM ${BASE}

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    python3 \
    python3-pip \
    zlib1g-dev \
    libncurses-dev \
    && rm -rf /var/lib/apt/lists/*

# Ubuntu 18.04 ships CMake 3.10; the llvm/ subproject needs >= 3.13
RUN python3 -m pip install --upgrade pip && python3 -m pip install "cmake>=3.13"

WORKDIR /build-zone

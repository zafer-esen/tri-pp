# Ubuntu 18.04 on purpose: the prebuilt LLVM 18.1.8 static libraries were
# compiled with that era's libstdc++, and a newer one changes how some std
# types are passed by value (an ABI mismatch that crashes the binary as soon
# as it calls into libclang). The old glibc also gives the shipped binary a
# wide compatibility range.
FROM ubuntu:18.04

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

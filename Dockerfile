FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    gdb \
    git \
    python3 \
    libtool \
    libtool-bin \
    libncurses5-dev \
    libncursesw5-dev \
    zlib1g-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build-zone


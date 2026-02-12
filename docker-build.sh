#!/bin/sh
podman build -t llvm-build-ubuntu-2204 .

podman run --rm -v $(pwd):/build-zone:Z llvm-build-ubuntu-2204 /bin/bash -c "chmod +x mk && ./mk"

#!/bin/bash
# Build the wile Docker image.
#
# Usage:
#   build/docker-build.sh
#
# Examples:
#   build/docker-build.sh                              # native platform
#   DOCKER_PLATFORM=linux/amd64 build/docker-build.sh  # cross-platform
#   DOCKER_IMAGE=wile-dev build/docker-build.sh        # custom image name
#
# Environment variables:
#   DOCKER_IMAGE      image name (default: wile)
#   DOCKER_PLATFORM   target platform (e.g., linux/amd64, linux/arm64)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
IMAGE="${DOCKER_IMAGE:-wile}"

build_args=(build -f "$REPO_ROOT/docker/Dockerfile" -t "$IMAGE")
if [ -n "${DOCKER_PLATFORM:-}" ]; then
    build_args+=(--platform "$DOCKER_PLATFORM")
fi
build_args+=("$REPO_ROOT")

docker "${build_args[@]}"

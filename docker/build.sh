#!/bin/bash
# Build the Wile Docker image.
# Delegates to build/docker-build.sh.
set -e

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
exec "$REPO_ROOT/build/docker-build.sh"

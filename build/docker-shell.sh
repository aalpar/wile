#!/bin/bash
# Open an interactive shell inside the wile Docker container.
# The first line of stdout is always the container ID.
# Delegates to docker-run.sh with DOCKER_INTERACTIVE=1.
#
# The image must be built first (make docker-build or build/docker-build.sh).
#
# Usage:
#   build/docker-shell.sh [shell]
#
# Examples:
#   build/docker-shell.sh              # bash (default)
#   build/docker-shell.sh /bin/sh      # sh
#
# Environment variables:
#   DOCKER_IMAGE   image name (default: wile)
#   DOCKER_FLAGS   extra flags passed to docker create

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SHELL_CMD="${1:-/bin/bash}"

export DOCKER_INTERACTIVE=1
exec "$SCRIPT_DIR/docker-run.sh" "$SHELL_CMD"

#!/bin/bash
# Run a command inside the wile Docker container.
# The first line of stdout is always the container ID.
#
# The image must be built first (make docker-build or build/docker-build.sh).
#
# Usage:
#   build/docker-run.sh <command> [args...]
#
# Examples:
#   build/docker-run.sh make test           # run the test suite
#   build/docker-run.sh make lint           # run the linter
#   build/docker-run.sh ../dist/scheme      # start the REPL
#   build/docker-run.sh ../dist/scheme --file examples/hello.scm
#
# Environment variables:
#   DOCKER_IMAGE         image name (default: wile)
#   DOCKER_FLAGS         extra flags passed to docker create
#   DOCKER_INTERACTIVE   set to 1 for interactive mode (-i, and -t if TTY)

set -euo pipefail

IMAGE="${DOCKER_IMAGE:-wile}"

if [ $# -eq 0 ]; then
    echo "Usage: $(basename "$0") <command> [args...]" >&2
    exit 1
fi

create_args=(create)
start_args=(start -a)

if [ "${DOCKER_INTERACTIVE:-0}" = "1" ]; then
    create_args+=(-i)
    start_args+=(-i)
    if [ -t 0 ]; then
        create_args+=(-t)
    fi
fi

if [ -n "${DOCKER_FLAGS:-}" ]; then
    read -ra flags <<< "$DOCKER_FLAGS"
    create_args+=("${flags[@]}")
fi
create_args+=("$IMAGE" "$@")

CID=$(docker "${create_args[@]}")
echo "$CID"

trap 'docker rm -f "$CID" >/dev/null 2>&1' EXIT

docker "${start_args[@]}" "$CID"

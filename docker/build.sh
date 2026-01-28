#!/bin/bash
# Build the Wile Docker image from the docker/ directory
set -e

cd "$(dirname "$0")"
docker build -f Dockerfile -t wile ..

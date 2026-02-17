#!/usr/bin/env bash
# Run all Scheme benchmarks

set -e

SCHEME="${SCHEME:-../../dist/scheme}"

if [ ! -e "$SCHEME" ]; then
    echo "Error: Scheme interpreter not found at $SCHEME"
    echo "Build it first with: make build"
    echo "Or set SCHEME environment variable to the correct path"
    exit 1
fi

# Resolve symlink if needed
if [ -L "$SCHEME" ]; then
    SCHEME_DIR=$(dirname "$SCHEME")
    SCHEME_TARGET=$(readlink "$SCHEME")
    SCHEME="$SCHEME_DIR/$SCHEME_TARGET"
fi

# Resolve SCHEME to absolute path before changing directory
SCHEME="$(cd "$(dirname "$SCHEME")" && pwd)/$(basename "$SCHEME")"

# Run from the directory containing the benchmark files
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme binary not executable: $SCHEME"
    exit 1
fi

echo "Running Wile Scheme Benchmarks"
echo "==============================="
echo ""

for bench in *.scm; do
    echo "----------------------------------------"
    echo "Running $(basename "$bench" .scm)..."
    echo "----------------------------------------"
    "$SCHEME" --file "$bench"
    echo ""
done

echo "==============================="
echo "All benchmarks complete"

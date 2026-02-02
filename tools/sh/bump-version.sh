#!/bin/bash
# bump-version.sh - Semantic version bumping for Wile
#
# Usage: ./tools/sh/bump-version.sh [major|minor|patch]
#
# Reads VERSION file at repo root, increments the specified component,
# and writes back to VERSION. Preserves prerelease suffixes like "-alpha".
#
# Examples:
#   v0.8.5-alpha + patch → v0.8.6-alpha
#   v0.8.5-alpha + minor → v0.9.0-alpha
#   v0.8.5-alpha + major → v1.0.0-alpha

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
VERSION_FILE="$REPO_ROOT/VERSION"

usage() {
    echo "Usage: $0 [major|minor|patch]"
    echo ""
    echo "Increments the version in $VERSION_FILE"
    echo ""
    echo "Arguments:"
    echo "  major    Increment major version (x.0.0)"
    echo "  minor    Increment minor version (0.x.0)"
    echo "  patch    Increment patch version (0.0.x)"
    echo ""
    echo "Prerelease suffixes (e.g., -alpha) are preserved."
    exit 1
}

if [ $# -ne 1 ]; then
    usage
fi

BUMP_TYPE="$1"

if [[ ! "$BUMP_TYPE" =~ ^(major|minor|patch)$ ]]; then
    echo "Error: Invalid bump type '$BUMP_TYPE'"
    usage
fi

if [ ! -f "$VERSION_FILE" ]; then
    echo "Error: VERSION file not found at $VERSION_FILE"
    exit 1
fi

# Read current version
CURRENT_VERSION=$(cat "$VERSION_FILE" | tr -d '[:space:]')

# Parse version: v1.2.3-alpha → major=1, minor=2, patch=3, suffix=-alpha
if [[ "$CURRENT_VERSION" =~ ^v?([0-9]+)\.([0-9]+)\.([0-9]+)(.*)$ ]]; then
    MAJOR="${BASH_REMATCH[1]}"
    MINOR="${BASH_REMATCH[2]}"
    PATCH="${BASH_REMATCH[3]}"
    SUFFIX="${BASH_REMATCH[4]}"
else
    echo "Error: Cannot parse version '$CURRENT_VERSION'"
    echo "Expected format: v1.2.3 or v1.2.3-prerelease"
    exit 1
fi

# Bump the requested component
case "$BUMP_TYPE" in
    major)
        MAJOR=$((MAJOR + 1))
        MINOR=0
        PATCH=0
        ;;
    minor)
        MINOR=$((MINOR + 1))
        PATCH=0
        ;;
    patch)
        PATCH=$((PATCH + 1))
        ;;
esac

NEW_VERSION="v${MAJOR}.${MINOR}.${PATCH}${SUFFIX}"

echo "$CURRENT_VERSION → $NEW_VERSION"
echo "$NEW_VERSION" > "$VERSION_FILE"
echo "Updated $VERSION_FILE"

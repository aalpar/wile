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

# ── CHANGELOG link-reference maintenance ────────────────────────────
# Keep a Changelog keeps a block of link references at the end of
# CHANGELOG.md:
#
#   [Unreleased]: <repo>/compare/<latest-release>...HEAD
#   [1.16.0]:     <repo>/compare/<prev-release>...v1.16.0
#
# These drifted because the release ceremony only ever finalized the
# section header, never the refs. A version bump is the point at which
# the new release identifier becomes known, so the refs are reconciled
# here. Each guard below is a no-op early return — a repo that does not
# maintain this block is left untouched.
update_changelog_links() {
    local changelog="$REPO_ROOT/CHANGELOG.md"
    [ -f "$changelog" ] || return 0

    # Reuse the base URL already on the [Unreleased] line rather than
    # hardcoding the repo slug. Absent line → block not maintained here.
    local unreleased
    unreleased=$(grep -m1 '^\[Unreleased\]: ' "$changelog" || true)
    [ -n "$unreleased" ] || return 0
    local rest="${unreleased#*: }"      # drop "[Unreleased]: "
    local base="${rest%%/compare/*}"    # drop "/compare/<...>...HEAD"

    # Previous release: most recent tag reachable from HEAD. During a
    # release bump HEAD is master before the new tag is cut, so this
    # resolves to the prior release. No tags yet → nothing to anchor.
    local prev
    prev=$(git -C "$REPO_ROOT" describe --tags --abbrev=0 2>/dev/null || true)
    [ -n "$prev" ] || return 0

    local label="${NEW_VERSION#v}"

    # Idempotent: a ref for this version already present → done.
    if grep -q "^\[${label}\]: " "$changelog"; then
        return 0
    fi

    # Rewrite the [Unreleased] anchor and insert the new release ref
    # below it. cat-into-file (not mv) preserves the original mode.
    local tmp
    tmp=$(mktemp)
    while IFS= read -r line || [ -n "$line" ]; do
        if [[ "$line" == '[Unreleased]: '* ]]; then
            printf '[Unreleased]: %s/compare/%s...HEAD\n' "$base" "$NEW_VERSION"
            printf '[%s]: %s/compare/%s...%s\n' "$label" "$base" "$prev" "$NEW_VERSION"
        else
            printf '%s\n' "$line"
        fi
    done < "$changelog" > "$tmp"
    cat "$tmp" > "$changelog"
    rm -f "$tmp"

    echo "Updated CHANGELOG.md link refs: [Unreleased], [${label}]"
}

update_changelog_links

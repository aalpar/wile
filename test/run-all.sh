#!/bin/bash
# Run all Scheme-level tests
#
# Usage:
#   ./test/run-all.sh
#   SCHEME=/path/to/wile ./test/run-all.sh
#
# Discovers all *-test.scm files and executes them.

set -e

# Change to repository root (parent of test directory where this script lives)
cd "$(dirname "$0")/.."

SCHEME="${SCHEME:-./dist/wile}"

# Check if scheme binary exists
if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme interpreter not found at $SCHEME"
    echo "Build it with: make build"
    exit 1
fi

# Discover all test files
TEST_FILES=$(find test pkg/stdlib/lib -name '*-test.scm' 2>/dev/null | sort)

if [ -z "$TEST_FILES" ]; then
    echo "No test files found"
    echo "Test files should match pattern: *-test.scm"
    exit 0
fi

DISCOVERED=$(echo "$TEST_FILES" | wc -l | tr -d ' ')

echo "Discovered $DISCOVERED test file(s)"
echo ""

# ── Gate: every discovered suite must be able to signal failure ─────────────
#
# A (chibi test) suite reports failure only by calling (test-exit), which exits
# with status 1 when any assertion failed (pkg/stdlib/lib/chibi/test.sld). The
# run loop below gates purely on the interpreter's exit status, so a suite that
# omits (test-exit) prints "FAIL: expected X but got Y" and still exits 0 — the
# regression is invisible to this script, to `make test-scheme`, and to `make ci`.
#
# The exemption is DERIVED, not curated: a *-test.scm that a sibling .sld in the
# same directory pulls in with (include "<basename>") is a library body, not a
# suite, and must not be required to call (test-exit). No allowlist of names —
# adding a library-include file, or promoting one to a real suite, needs no edit
# here.
#
# What this check does NOT cover: it proves a suite *can* signal failure, not
# that any assertion in it is non-vacuous. A suite whose every `test` form is
# trivially true passes this check and still gates nothing.

# included_by_sibling_library FILE — true when a .sld beside FILE (include)s it.
included_by_sibling_library() {
    local file="$1"
    local dir
    local base
    local sld
    dir=$(dirname "$file")
    base=$(basename "$file")
    for sld in "$dir"/*.sld; do
        if [ ! -f "$sld" ]; then
            continue
        fi
        if grep -hoE '\(include[^)]*\)' "$sld" | grep -qF "\"$base\""; then
            return 0
        fi
    done
    return 1
}

UNGATED=""
NO_EXIT_COUNT=0
EXEMPT_COUNT=0
UNGATED_COUNT=0

for file in $TEST_FILES; do
    if grep -q 'test-exit' "$file"; then
        continue
    fi
    NO_EXIT_COUNT=$((NO_EXIT_COUNT + 1))
    if included_by_sibling_library "$file"; then
        EXEMPT_COUNT=$((EXEMPT_COUNT + 1))
        continue
    fi
    UNGATED="${UNGATED}  $file"$'\n'
    UNGATED_COUNT=$((UNGATED_COUNT + 1))
done

# Both numbers, so the derivation is auditable from a CI log: the raw count is
# what a name-blind grep sees, the residual is what this script gates on.
echo "test-exit gate: $DISCOVERED discovered, $NO_EXIT_COUNT without (test-exit), $EXEMPT_COUNT exempt as library include(s), $UNGATED_COUNT ungated"
echo ""

if [ "$UNGATED_COUNT" -ne 0 ]; then
    echo "✗ $UNGATED_COUNT of $DISCOVERED discovered test file(s) cannot signal failure to CI:"
    printf '%s' "$UNGATED"
    echo ""
    echo "A (chibi test) suite reports failure only through (test-exit)."
    echo "Without it a failing assertion prints FAIL: and the process exits 0."
    echo "Fix: add (test-exit) after the suite's final (test-end)."
    exit 1
fi

# Run each test file
FAILED=0
PASSED=0

for file in $TEST_FILES; do
    echo "▶ $file"
    if "$SCHEME" --quiet -f "$file"; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        echo "✗ FAILED: $file"
    fi
    echo ""
done

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test Summary:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ $FAILED -eq 0 ]; then
    echo "✓ All Scheme tests passed"
    exit 0
else
    echo "✗ $FAILED test file(s) failed"
    exit 1
fi

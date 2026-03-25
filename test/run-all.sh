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
TEST_FILES=$(find test stdlib/lib -name '*-test.scm' 2>/dev/null | sort)

if [ -z "$TEST_FILES" ]; then
    echo "No test files found"
    echo "Test files should match pattern: *-test.scm"
    exit 0
fi

echo "Discovered $(echo "$TEST_FILES" | wc -l | tr -d ' ') test file(s)"
echo ""

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

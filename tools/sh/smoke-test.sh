#!/usr/bin/env bash
# Smoke test: verify the built binary starts, prints version, and evaluates.
#
# Usage:
#   ./tools/sh/smoke-test.sh <path-to-scheme-binary>

set -euo pipefail

SCHEME="${1:?Usage: smoke-test.sh <scheme-binary>}"

if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme binary not found or not executable: $SCHEME"
    exit 1
fi

passed=0
failed=0

# Test 1: --version exits 0 and contains "Wile Scheme"
echo -n "  version check... "
version_output=$("$SCHEME" --version 2>&1)
if echo "$version_output" | grep -q "Wile Scheme"; then
    echo "ok"
    passed=$((passed + 1))
else
    echo "FAIL (got: $version_output)"
    failed=$((failed + 1))
fi

# Test 2: arithmetic evaluation
echo -n "  eval (+ 1 2)... "
result=$(echo '(+ 1 2)' | "$SCHEME" -q -f /dev/stdin 2>&1)
if [ "$result" = "3" ]; then
    echo "ok"
    passed=$((passed + 1))
else
    echo "FAIL (got: $result)"
    failed=$((failed + 1))
fi

# Test 3: display output
echo -n "  eval (display)... "
result=$(echo '(display "hello")' | "$SCHEME" -q -f /dev/stdin 2>&1)
if [ "$result" = "hello" ]; then
    echo "ok"
    passed=$((passed + 1))
else
    echo "FAIL (got: $result)"
    failed=$((failed + 1))
fi

echo ""
echo "Smoke tests: $passed/3 passed"
if [ "$failed" -gt 0 ]; then
    exit 1
fi

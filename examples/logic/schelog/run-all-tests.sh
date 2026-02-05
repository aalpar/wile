#!/bin/bash
# Schelog Validation Suite for Wile
#
# This script runs all schelog examples to validate Wile's compatibility
# with the Schelog logic programming library by Dorai Sitaram.
#
# Usage:
#   cd <wile-root>
#   ./examples/logic/schelog/run-all-tests.sh
#
# Expected output: All tests should pass with no errors.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WILE_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
SCHEME="$WILE_ROOT/dist/scheme"
SCHELOG_DIR="$SCRIPT_DIR"

echo "=== Schelog Validation Suite for Wile ==="
echo ""
echo "Wile binary: $SCHEME"
echo "Schelog dir: $SCHELOG_DIR"
echo ""

# Check that scheme binary exists
if [ ! -x "$SCHEME" ]; then
    echo "ERROR: Scheme binary not found at $SCHEME"
    echo "Run 'make build' or 'go build -o dist/scheme ./cmd' first"
    exit 1
fi

TESTS_PASSED=0
TESTS_FAILED=0

run_test() {
    local name="$1"
    local files="$2"
    local code="$3"

    echo "Testing: $name"

    # Build the command
    local cmd="$SCHEME -q"
    for f in $files; do
        cmd="$cmd -i -f $SCHELOG_DIR/$f"
    done

    # Run the test
    if output=$(echo "$code" | $cmd 2>&1); then
        echo "  PASS"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo "  FAIL: $output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

run_test_with_output() {
    local name="$1"
    local files="$2"
    local code="$3"
    local expected="$4"

    echo "Testing: $name"

    # Build the command
    local cmd="$SCHEME -q"
    for f in $files; do
        cmd="$cmd -i -f $SCHELOG_DIR/$f"
    done

    # Run the test and capture output
    if output=$(echo "$code" | $cmd 2>&1); then
        if echo "$output" | grep -q "$expected"; then
            echo "  PASS"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            echo "  FAIL: expected '$expected' in output"
            echo "  Got: $output"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            return 1
        fi
    else
        echo "  FAIL: $output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

echo "--- toys.scm ---"
run_test_with_output "toys: %length" "schelog.scm toys.scm" \
    '(display (%which (n) (%length (quote (a b c)) n)))' \
    "((n 3))"

run_test_with_output "toys: %append" "schelog.scm toys.scm" \
    '(display (%which (z) (%append (quote (1 2)) (quote (3 4)) z)))' \
    "((z (1 2 3 4)))"

run_test_with_output "toys: %reverse" "schelog.scm toys.scm" \
    '(display (%which (y) (%reverse (quote (a b c d)) y)))' \
    "((y (d c b a)))"

run_test_with_output "toys: %fact 5" "schelog.scm toys.scm" \
    '(display (%which (n) (%fact 5 n)))' \
    "((n 120))"

echo ""
echo "--- holland.scm ---"
run_test_with_output "holland: city amsterdam" "schelog.scm holland.scm" \
    '(display (if (%which () (%city (quote amsterdam))) "yes" "no"))' \
    "yes"

run_test_with_output "holland: country amsterdam (should be no)" "schelog.scm holland.scm" \
    '(display (if (%which () (%country (quote amsterdam))) "yes" "no"))' \
    "no"

echo ""
echo "--- england.scm ---"
run_test_with_output "england: Philip is male" "schelog.scm england.scm" \
    '(display (if (%which () (%male (quote philip))) "yes" "no"))' \
    "yes"

run_test_with_output "england: Philip father of Charles" "schelog.scm england.scm" \
    '(display (if (%which () (%father-of (quote philip) (quote charles))) "yes" "no"))' \
    "yes"

echo ""
echo "--- mapcol.scm ---"
run_test_with_output "mapcol: test map" "schelog.scm mapcol.scm" \
    '(display (if (%which (M) (%test-color (quote test) M)) "solved" "failed"))' \
    "solved"

run_test_with_output "mapcol: western-europe" "schelog.scm mapcol.scm" \
    '(display (if (%which (M) (%test-color (quote western-europe) M)) "solved" "failed"))' \
    "solved"

echo ""
echo "--- bible.scm ---"
run_test_with_output "bible: terachs-kids-test" "schelog.scm bible.scm" \
    '(display (if (terachs-kids-test) "found" "not found"))' \
    "found"

echo ""
echo "--- games.scm ---"
run_test_with_output "games: puzzle solution" "schelog.scm puzzle.scm games.scm" \
    '(let ((r (solve-puzzle %games))) (display (if r "solved" "failed")))' \
    "solved"

run_test_with_output "games: michael is australian" "schelog.scm puzzle.scm games.scm" \
    '(let ((r (solve-puzzle %games))) (display (schelog:deref* (car (cdr (car r))))))' \
    "(michael is the australian)"

echo ""
echo "--- houses.scm (Zebra puzzle) ---"
run_test_with_output "houses: zebra puzzle solved" "schelog.scm puzzle.scm houses.scm" \
    '(begin (set! *schelog-use-occurs-check?* #t) (let ((r (solve-puzzle %houses))) (display (if r "solved" "failed"))))' \
    "solved"

run_test_with_output "houses: japan owns zebra" "schelog.scm puzzle.scm houses.scm" \
    '(begin (set! *schelog-use-occurs-check?* #t) (let ((r (solve-puzzle %houses))) (display (schelog:deref* (car (car (cdr (car r))))))))' \
    "(japan owns the zebra)"

run_test_with_output "houses: norway drinks water" "schelog.scm puzzle.scm houses.scm" \
    '(begin (set! *schelog-use-occurs-check?* #t) (let ((r (solve-puzzle %houses))) (display (schelog:deref* (cadr (car (cdr (car r))))))))' \
    "(norway drinks water)"

echo ""
echo "=== Test Summary ==="
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo "All tests passed! Wile is fully compatible with Schelog."
    exit 0
else
    echo "Some tests failed. See above for details."
    exit 1
fi

#!/bin/bash
# Compare test results across multiple Scheme implementations
#
# Usage:
#   ./test/compare-schemes.sh [scheme1] [scheme2] ...
#
# Example:
#   ./test/compare-schemes.sh ./dist/darwin/arm64/scheme chez-scheme chibi-scheme
#
# If no arguments provided, compares Wile with Chibi-Scheme (if available)

set -e

# Default implementations to test
SCHEMES=("${@:-./dist/darwin/arm64/scheme chibi-scheme}")

echo "═══════════════════════════════════════════════════════════"
echo "Scheme Implementation Comparison"
echo "═══════════════════════════════════════════════════════════"
echo ""

RESULTS_DIR=./build/test-comparison
mkdir -p "$RESULTS_DIR"

# Run tests for each implementation
for scheme in "${SCHEMES[@]}"; do
    echo "─────────────────────────────────────────────────────────"
    echo "Testing: $scheme"
    echo "─────────────────────────────────────────────────────────"

    # Sanitize scheme name for filename
    scheme_name=$(echo "$scheme" | tr '/' '_' | tr ' ' '_')
    output_file="$RESULTS_DIR/$scheme_name.log"

    # Check if scheme exists
    if ! command -v "$scheme" >/dev/null 2>&1 && [ ! -x "$scheme" ]; then
        echo "⚠  SKIP: $scheme not found"
        echo "not found" > "$output_file"
        echo ""
        continue
    fi

    # Run tests
    if SCHEME="$scheme" ./test/run-all.sh > "$output_file" 2>&1; then
        echo "✓  PASS: All tests passed"
        passed=$(grep "Passed:" "$output_file" | awk '{print $2}')
        echo "   Tests: $passed passed"
    else
        echo "✗  FAIL: Some tests failed"
        passed=$(grep "Passed:" "$output_file" | awk '{print $2}')
        failed=$(grep "Failed:" "$output_file" | awk '{print $2}')
        echo "   Tests: $passed passed, $failed failed"
    fi

    echo ""
done

echo "═══════════════════════════════════════════════════════════"
echo "Summary"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Detailed logs saved to: $RESULTS_DIR/"
echo ""
echo "Files:"
for scheme in "${SCHEMES[@]}"; do
    scheme_name=$(echo "$scheme" | tr '/' '_' | tr ' ' '_')
    output_file="$RESULTS_DIR/$scheme_name.log"
    if [ -f "$output_file" ]; then
        echo "  - $output_file"
    fi
done

echo ""
echo "To view differences:"
echo "  diff $RESULTS_DIR/scheme1.log $RESULTS_DIR/scheme2.log"

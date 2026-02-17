#!/usr/bin/env bash
# Run canonical Gabriel benchmark suite benchmarks only
# These are comparable across Scheme implementations

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

# Canonical Gabriel suite benchmarks
CANONICAL_BENCHMARKS=(
    "tak"
    "takl"
    "ctak"
    "cpstak"
    "fib"
    "triangl"
    "sum"
    "sumfp"
    "diviter"
    "divrec"
    "deriv"
    "ackermann"
    "sieve"
    "nqueens"
    "primes"
    "peval"
)

echo "Running Canonical Gabriel Benchmark Suite"
echo "=========================================="
echo ""
echo "Benchmarks: ${#CANONICAL_BENCHMARKS[@]}"
echo "Scheme: $SCHEME"
echo ""

RESULTS_CSV="canonical-results-$(date +%Y%m%d-%H%M%S).csv"

echo "benchmark,total_time_seconds" > "$RESULTS_CSV"

for bench in "${CANONICAL_BENCHMARKS[@]}"; do
    echo "----------------------------------------"
    echo "Running $bench..."
    echo "----------------------------------------"

    OUTPUT=$("$SCHEME" --file "${bench}.scm" 2>&1)
    echo "$OUTPUT"

    # Extract time and save to CSV
    TIME=$(echo "$OUTPUT" | grep "Total time:" | awk '{print $3}' | tr -d 's')
    if [ -n "$TIME" ]; then
        echo "$bench,$TIME" >> "$RESULTS_CSV"
    fi

    echo ""
done

echo "==============================="
echo "All canonical benchmarks complete"
echo ""
echo "Results saved to: $RESULTS_CSV"
echo ""
echo "Summary:"
column -t -s, "$RESULTS_CSV"

#!/usr/bin/env bash
# Run canonical Gabriel benchmark suite benchmarks only
# These are comparable across Scheme implementations

set -e

RUNS="${RUNS:-6}"
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
echo "Runs: $RUNS"
echo "Scheme: $SCHEME"
echo ""

RESULTS_CSV="canonical-results-$(date +%Y%m%d-%H%M%S).csv"
RAW_CSV=$(mktemp)

echo "run,benchmark,total_time_seconds" > "$RAW_CSV"

for run in $(seq 1 "$RUNS"); do
    echo "========== Run $run / $RUNS =========="
    for bench in "${CANONICAL_BENCHMARKS[@]}"; do
        echo -n "  $bench... "

        OUTPUT=$("$SCHEME" --file "${bench}.scm" 2>&1)

        TIME=$(echo "$OUTPUT" | grep "Total time:" | awk '{print $3}' | tr -d 's')
        if [ -n "$TIME" ]; then
            echo "$run,$bench,$TIME" >> "$RAW_CSV"
            echo "${TIME}s"
        else
            echo "FAILED"
        fi
    done
    echo ""
done

# Compute avg/min/max/spread from raw data
echo "benchmark,avg_s,min_s,max_s,spread_pct,runs" > "$RESULTS_CSV"
awk -F, 'NR > 1 {
    bench = $2; time = $3 + 0
    sum[bench] += time; count[bench]++
    if (!(bench in mn) || time < mn[bench]) mn[bench] = time
    if (!(bench in mx) || time > mx[bench]) mx[bench] = time
    if (count[bench] == 1) order[++n] = bench
}
END {
    for (i = 1; i <= n; i++) {
        b = order[i]
        avg = sum[b] / count[b]
        spread = ((mx[b] - mn[b]) / avg) * 100
        printf "%s,%.4f,%.4f,%.4f,%.1f,%d\n", b, avg, mn[b], mx[b], spread, count[b]
    }
}' "$RAW_CSV" >> "$RESULTS_CSV"

rm -f "$RAW_CSV"

echo "==============================="
echo "All canonical benchmarks complete ($RUNS runs averaged)"
echo ""
echo "Results saved to: $RESULTS_CSV"
echo ""
echo "Summary:"
column -t -s, "$RESULTS_CSV"

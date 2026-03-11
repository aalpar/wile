#!/usr/bin/env bash
# Compare Gabriel benchmark results against a checked-in baseline.
# Fails if the geo-mean regresses more than THRESHOLD percent.
#
# Environment variables:
#   SCHEME    — path to scheme binary (required)
#   BASELINE  — path to baseline CSV (default: canonical-baseline.csv)
#   THRESHOLD — max allowed regression percentage (default: 5)
#   RUNS      — number of benchmark runs (default: 3, fewer than canonical for speed)
#
# Must be run from examples/benchmarks/ directory.
#
# Usage (from repo root):
#   cd examples/benchmarks && \
#     SCHEME=../../dist/linux/amd64/wile \
#     BASELINE=canonical-baseline.csv \
#     THRESHOLD=5 \
#     ../../tools/sh/bench-regression.sh

set -euo pipefail

SCHEME="${SCHEME:?SCHEME must be set to the path of the scheme binary}"
BASELINE="${BASELINE:-canonical-baseline.csv}"
THRESHOLD="${THRESHOLD:-5}"
RUNS="${RUNS:-3}"

if [ ! -f "$BASELINE" ]; then
    echo "Error: Baseline file not found: $BASELINE"
    exit 1
fi

if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme binary not found or not executable: $SCHEME"
    exit 1
fi

echo "Benchmark regression check"
echo "  Baseline:  $BASELINE"
echo "  Threshold: ${THRESHOLD}%"
echo "  Runs:      $RUNS"
echo ""

# Run benchmarks with fewer runs for CI speed
export SCHEME RUNS
bench_output=$(RUNS="$RUNS" ./run-canonical.sh 2>&1) || {
    echo "Error: run-canonical.sh failed"
    echo "$bench_output"
    exit 1
}
RESULTS_CSV=$(echo "$bench_output" | grep "Results saved to:" | awk '{print $NF}' || true)

if [ -z "$RESULTS_CSV" ] || [ ! -f "$RESULTS_CSV" ]; then
    echo "Error: Failed to produce benchmark results"
    echo "$bench_output"
    exit 1
fi

# Compare baseline vs new results.
# Both CSVs have: benchmark,avg_s,min_s,max_s,spread_pct,...
# Baseline may have extra columns (date,commit) — we only use columns 1 and 2.
awk -F, -v threshold="$THRESHOLD" '
BEGIN {
    n = 0
    geo_log_sum = 0
    geo_count = 0
}

# Read baseline (first file)
FNR == NR && NR > 1 {
    baseline[$1] = $2 + 0
    next
}

# Read new results (second file)
FNR > 1 {
    name = $1
    new_avg = $2 + 0
    if (name in baseline && baseline[name] > 0 && new_avg > 0) {
        ratio = new_avg / baseline[name]
        change_pct = (ratio - 1) * 100
        benchmarks[++n] = name
        ratios[name] = ratio
        changes[name] = change_pct
        baselines[name] = baseline[name]
        results[name] = new_avg
        geo_log_sum += log(ratio)
        geo_count++
    }
}

END {
    if (geo_count == 0) {
        print "Error: No matching benchmarks between baseline and results"
        exit 1
    }

    geo_mean = exp(geo_log_sum / geo_count)
    geo_change = (geo_mean - 1) * 100

    printf "\n%-12s %10s %10s %8s\n", "Benchmark", "Baseline", "Current", "Change"
    printf "%-12s %10s %10s %8s\n", "─────────", "────────", "───────", "──────"

    for (i = 1; i <= n; i++) {
        b = benchmarks[i]
        sign = (changes[b] >= 0) ? "+" : ""
        printf "%-12s %9.4fs %9.4fs %+7.1f%%\n", b, baselines[b], results[b], changes[b]
    }

    printf "%-12s %10s %10s %+7.1f%%\n", "─────────", "", "", geo_change
    printf "%-12s %10s %10s %+7.1f%%\n", "GEO-MEAN", "", "", geo_change

    if (geo_change > threshold + 0) {
        printf "\nFAIL: geo-mean regression %.1f%% exceeds threshold %s%%\n", geo_change, threshold
        exit 1
    } else {
        printf "\nPASS: geo-mean change %.1f%% within threshold %s%%\n", geo_change, threshold
    }
}
' "$BASELINE" "$RESULTS_CSV" || rc=$?

# Clean up the temporary results CSV so it doesn't dirty the git tree
# (GoReleaser refuses to release with untracked files).
rm -f "$RESULTS_CSV"

exit "${rc:-0}"

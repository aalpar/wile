#!/usr/bin/env bash
# Run extended benchmark suite: Larceny R7RS + Schelog Zebra + miniKanren
# Produces CSV output comparable to run-canonical.sh

set -e

RUNS="${RUNS:-3}"
SCHEME="${SCHEME:-../../dist/wile}"
LARCENY_COUNT="${LARCENY_COUNT:-1}"
TIMEOUT="${TIMEOUT:-120}"

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

# Script and project directories
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LARCENY_DIR="$PROJECT_ROOT/benchmarks/larceny"
LARCENY_SRC="$LARCENY_DIR/src"
LARCENY_INPUTS="$LARCENY_DIR/inputs"
LARCENY_TMP="$LARCENY_DIR/tmp"
LIBDIR="$PROJECT_ROOT/lib"

cd "$SCRIPT_DIR"

if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme binary not executable: $SCHEME"
    exit 1
fi

# Working Larceny benchmarks (excludes known-incompatible ones).
# Skipped: compiler mazefun nucleic ray dynamic mbrotZ pi slatex parsing
#          cat tail wc read1 sum1 string bv2string
LARCENY_BENCHMARKS=(
    browse deriv destruc diviter divrec puzzle triangl tak takl ntakl cpstak ctak
    fib fibc fibfp sum sumfp fft mbrot pnpoly simplex
    ack
    conform maze matrix nqueens paraffins peval primes quicksort scheme
    nboyer sboyer gcbench mperm
    equal
)

# Non-Larceny benchmarks (custom timing harnesses)
EXTRA_BENCHMARKS=(
    schelog-zebra
    kanren-zebra
    kanren-appendo-20
    kanren-arith
)

ALL_BENCHMARKS=("${LARCENY_BENCHMARKS[@]}" "${EXTRA_BENCHMARKS[@]}")

echo "Running Extended Benchmark Suite"
echo "=========================================="
echo ""
echo "Larceny benchmarks: ${#LARCENY_BENCHMARKS[@]} (count=${LARCENY_COUNT})"
echo "Extra benchmarks: ${#EXTRA_BENCHMARKS[@]}"
echo "Total: ${#ALL_BENCHMARKS[@]}"
echo "Runs: $RUNS"
echo "Timeout: ${TIMEOUT}s per benchmark"
echo "Scheme: $SCHEME"
echo ""

RESULTS_CSV="extended-results-$(date +%Y%m%d-%H%M%S).csv"
RAW_CSV=$(mktemp)

echo "run,benchmark,total_time_seconds" > "$RAW_CSV"

mkdir -p "$LARCENY_TMP"

# run_larceny NAME — run one Larceny benchmark, echo elapsed seconds or empty
run_larceny() {
    local name="$1"

    if [ ! -f "$LARCENY_SRC/$name.scm" ]; then
        return
    fi
    if [ ! -f "$LARCENY_INPUTS/$name.input" ]; then
        return
    fi

    cat "$LARCENY_SRC/$name.scm" "$LARCENY_SRC/common.scm" > "$LARCENY_TMP/$name.scm"

    local input
    input=$(echo "$LARCENY_COUNT"; tail -n +2 "$LARCENY_INPUTS/$name.input")

    local output
    output=$(echo "$input" | timeout "$TIMEOUT" "$SCHEME" -q -L "$LIBDIR" --file "$LARCENY_TMP/$name.scm" 2>&1) || true

    # Parse "Elapsed time: X seconds (...) for NAME"
    local time
    time=$(echo "$output" | grep "^Elapsed time:" | awk '{print $3}')
    echo "$time"
}

# run_extra NAME — run a non-Larceny benchmark, echo elapsed seconds or empty
run_extra() {
    local name="$1"
    local output time

    case "$name" in
        schelog-zebra)
            output=$(SCHEME_INCLUDE_PATH="$PROJECT_ROOT" \
                timeout "$TIMEOUT" "$SCHEME" -q \
                --file "$SCRIPT_DIR/schelog-zebra-bench.scm" 2>&1) || true
            time=$(echo "$output" | grep "^Total time:" | awk '{print $3}' | tr -d 's')
            ;;
        kanren-zebra)
            output=$(SCHEME_LIBRARY_PATH="$LIBDIR" \
                timeout "$TIMEOUT" "$SCHEME" -q \
                --file "$SCRIPT_DIR/kanren-benchmark.scm" 2>&1) || true
            time=$(echo "$output" | grep "^  zebra:" | awk '{print $2}' | tr -d 's')
            ;;
        kanren-appendo-20)
            # Reuse output from kanren run — but we need to run it again.
            output=$(SCHEME_LIBRARY_PATH="$LIBDIR" \
                timeout "$TIMEOUT" "$SCHEME" -q \
                --file "$SCRIPT_DIR/kanren-benchmark.scm" 2>&1) || true
            time=$(echo "$output" | grep "^  appendo-20:" | awk '{print $2}' | tr -d 's')
            ;;
        kanren-arith)
            output=$(SCHEME_LIBRARY_PATH="$LIBDIR" \
                timeout "$TIMEOUT" "$SCHEME" -q \
                --file "$SCRIPT_DIR/kanren-benchmark.scm" 2>&1) || true
            time=$(echo "$output" | grep "^  arith:" | awk '{print $2}' | tr -d 's')
            ;;
    esac

    echo "$time"
}

for run in $(seq 1 "$RUNS"); do
    echo "========== Run $run / $RUNS =========="

    # Run kanren once per run and cache output (it produces all 3 timings)
    KANREN_OUTPUT=""
    if KANREN_OUTPUT=$(SCHEME_LIBRARY_PATH="$LIBDIR" \
        timeout "$TIMEOUT" "$SCHEME" -q \
        --file "$SCRIPT_DIR/kanren-benchmark.scm" 2>&1); then
        true
    fi

    for bench in "${ALL_BENCHMARKS[@]}"; do
        echo -n "  $bench... "

        TIME=""

        # Check if it's a Larceny benchmark or extra
        case "$bench" in
            schelog-zebra)
                OUTPUT=$(SCHEME_INCLUDE_PATH="$PROJECT_ROOT" \
                    timeout "$TIMEOUT" "$SCHEME" -q \
                    --file "$SCRIPT_DIR/schelog-zebra-bench.scm" 2>&1) || true
                TIME=$(echo "$OUTPUT" | grep "^Total time:" | awk '{print $3}' | tr -d 's')
                ;;
            kanren-zebra)
                TIME=$(echo "$KANREN_OUTPUT" | grep "^  zebra:" | awk '{print $2}' | tr -d 's')
                ;;
            kanren-appendo-20)
                TIME=$(echo "$KANREN_OUTPUT" | grep "^  appendo-20:" | awk '{print $2}' | tr -d 's')
                ;;
            kanren-arith)
                TIME=$(echo "$KANREN_OUTPUT" | grep "^  arith:" | awk '{print $2}' | tr -d 's')
                ;;
            *)
                TIME=$(run_larceny "$bench")
                ;;
        esac

        if [ -n "$TIME" ]; then
            echo "$run,$bench,$TIME" >> "$RAW_CSV"
            echo "${TIME}s"
        else
            echo "SKIP"
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
echo "All extended benchmarks complete ($RUNS runs averaged)"
echo ""
echo "Results saved to: $RESULTS_CSV"
echo ""
echo "Summary:"
column -t -s, "$RESULTS_CSV"

#!/bin/bash
# Run Larceny R7RS benchmarks under Wile.
#
# Usage:
#   ./bench.sh [OPTIONS] <benchmark|group|all>
#
# Options:
#   -n COUNT    Override iteration count (default: use input file's count)
#   -t TIMEOUT  Per-benchmark timeout in seconds (default: 600)
#   -q          Quiet mode (only show timing lines)
#   -l          List available benchmarks and exit
#
# Groups:
#   all         All working benchmarks
#   gabriel     Gabriel benchmarks
#   numerical   Numerical benchmarks
#   kvw         Kernighan and Van Wyk benchmarks
#   other       Other benchmarks
#   gc          Garbage collection benchmarks
#   synth       Synthetic benchmarks
#   quick       Fast subset (~30s total at -n 1)
#
# Examples:
#   ./bench.sh -n 1 all            # Run all benchmarks, 1 iteration each
#   ./bench.sh -n 1 quick          # Run fast subset
#   ./bench.sh fib tak ack         # Run specific benchmarks with original counts
#   ./bench.sh -n 5 fib            # Run fib, 5 iterations
#   ./bench.sh -l                  # List all benchmarks

set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
WILE_ROOT="$(cd "$DIR/../.." && pwd)"
SCHEME="${WILE_SCHEME:-$WILE_ROOT/dist/wile}"
# Absolutize against the caller's CWD: run_benchmark runs the interpreter from
# $DIR, and the Makefile passes WILE_SCHEME as a repo-root-relative path.
if [ "${SCHEME#/}" = "$SCHEME" ]; then
    SCHEME="$PWD/$SCHEME"
fi
LIBDIR="$WILE_ROOT/pkg/stdlib/lib"
SRC="$DIR/src"
INPUTS="$DIR/inputs"
TMP="$DIR/tmp"

COUNT_OVERRIDE=""
TIMEOUT=30
QUIET=false

# Benchmark groups (matching Larceny's categorization).
GABRIEL="browse deriv destruc diviter divrec puzzle triangl tak takl ntakl cpstak ctak"
NUMERICAL="fib fibc fibfp sum sumfp fft mbrot pnpoly simplex"
KVW="ack"
OTHER="compiler conform dynamic earley graphs lattice matrix maze mazefun nqueens nucleic paraffins peval primes quicksort ray scheme"
GC="nboyer sboyer gcbench mperm"
SYNTH="equal"

# Not runnable here, and listed so the gap stays visible. mbrotZ, pi, slatex,
# parsing and bv2string appear in download.sh's SOURCES but are absent from src/;
# cat, tail, wc, read1, sum1 and string are not fetched at all. run_benchmark
# skips any absent source anyway, so this list is documentation, not a gate.
# mbrotZ:    needs (scheme complex) -- not tested yet
# pi:        needs bignum arithmetic -- not tested yet
# slatex:    needs file I/O (slatex-data) -- not tested yet
# parsing:   needs file I/O (parsing.data) -- not tested yet
# cat/tail/wc/read1/sum1/string: need file I/O (Bible text etc.)
# bv2string: needs bytevector<->string conversion -- not tested yet
#
# compiler, mazefun, nucleic, ray and dynamic were skipped here as Wile bugs
# ("vector-ref gets pair instead of vector", "number->string gets ()",
# "vector-ref index out of bounds"). All five pass as of 2026-07-28; the three
# bug claims no longer reproduce, and the other two were harness faults, fixed
# by running the interpreter from $DIR (dynamic reads inputs/dynamic.data
# relative to CWD) and pre-creating outputs/ (ray writes outputs/ray.output).
SKIP="mbrotZ pi slatex parsing cat tail wc read1 sum1 string bv2string"

ALL="$GABRIEL $NUMERICAL $KVW $OTHER $GC $SYNTH"

# Quick subset: benchmarks that complete in <2s each at count=1. ray is left out
# -- it straddles the bound (1.96-2.06s measured over four runs on an M-series Mac).
QUICK="fib tak ack cpstak ctak deriv destruc diviter divrec primes nqueens sum fibc fibfp sumfp fft mbrot pnpoly simplex compiler conform dynamic earley graphs lattice maze matrix mazefun nucleic peval quicksort scheme gcbench browse puzzle"

usage() {
    sed -n '2,/^$/s/^# //p' "$0"
    exit 0
}

list_benchmarks() {
    echo "Available benchmarks:"
    echo ""
    echo "Gabriel:    $GABRIEL"
    echo "Numerical:  $NUMERICAL"
    echo "KVW:        $KVW"
    echo "Other:      $OTHER"
    echo "GC:         $GC"
    echo "Synthetic:  $SYNTH"
    echo ""
    echo "Skipped (incompatible): $SKIP"
    exit 0
}

is_skipped() {
    local name="$1"
    for s in $SKIP; do
        if [ "$s" = "$name" ]; then
            return 0
        fi
    done
    return 1
}

run_benchmark() {
    local name="$1"

    if is_skipped "$name"; then
        echo "SKIP $name (known incompatibility)"
        return 0
    fi

    if [ ! -f "$SRC/$name.scm" ]; then
        echo "SKIP $name (source not found: $SRC/$name.scm)"
        return 0
    fi

    if [ ! -f "$INPUTS/$name.input" ]; then
        echo "SKIP $name (input not found: $INPUTS/$name.input)"
        return 0
    fi

    # Concatenate benchmark source + common harness.
    cat "$SRC/$name.scm" "$SRC/common.scm" > "$TMP/$name.scm"

    # Build input: optionally override the iteration count (first line).
    local input
    if [ -n "$COUNT_OVERRIDE" ]; then
        input=$(echo "$COUNT_OVERRIDE"; tail -n +2 "$INPUTS/$name.input")
    else
        input=$(cat "$INPUTS/$name.input")
    fi

    # Run from $DIR in a subshell: some benchmarks resolve data paths against the
    # CWD (dynamic reads "inputs/dynamic.data", ray writes "outputs/ray.output"),
    # so the interpreter's CWD must not depend on where bench.sh was invoked. Every
    # other path here is absolute, and the subshell keeps $0 valid for usage().
    if [ "$QUIET" = true ]; then
        ( cd "$DIR" && echo "$input" | timeout "$TIMEOUT" "$SCHEME" -q -L "$LIBDIR" --file "$TMP/$name.scm" 2>&1 ) \
            | grep -E "^(Running |Elapsed time:|ERROR:)" || echo "FAIL $name (timeout or crash)"
    else
        ( cd "$DIR" && echo "$input" | timeout "$TIMEOUT" "$SCHEME" -q -L "$LIBDIR" --file "$TMP/$name.scm" 2>&1 ) \
            || echo "FAIL $name (exit $?)"
    fi
}

# Parse options.
while getopts "n:t:qlh" opt; do
    case $opt in
        n) COUNT_OVERRIDE="$OPTARG" ;;
        t) TIMEOUT="$OPTARG" ;;
        q) QUIET=true ;;
        l) list_benchmarks ;;
        h) usage ;;
        *) usage ;;
    esac
done
shift $((OPTIND - 1))

if [ $# -eq 0 ]; then
    usage
fi

# Check binary exists.
if [ ! -x "$SCHEME" ]; then
    echo "Error: Wile binary not found at $SCHEME"
    echo "Build it first: make build (or set WILE_SCHEME=/path/to/wile)"
    exit 1
fi

# Resolve benchmark list from arguments.
benchmarks=""
for arg in "$@"; do
    case "$arg" in
        all)       benchmarks="$benchmarks $ALL" ;;
        gabriel)   benchmarks="$benchmarks $GABRIEL" ;;
        numerical) benchmarks="$benchmarks $NUMERICAL" ;;
        kvw)       benchmarks="$benchmarks $KVW" ;;
        other)     benchmarks="$benchmarks $OTHER" ;;
        gc)        benchmarks="$benchmarks $GC" ;;
        synth)     benchmarks="$benchmarks $SYNTH" ;;
        quick)     benchmarks="$benchmarks $QUICK" ;;
        *)         benchmarks="$benchmarks $arg" ;;
    esac
done

mkdir -p "$TMP" "$DIR/outputs"

echo "Larceny R7RS Benchmarks — Wile"
echo "Binary: $SCHEME"
echo "Date:   $(date)"
echo "Host:   $(uname -ms)"
echo "=========================================="

for bench in $benchmarks; do
    run_benchmark "$bench"
done

echo "=========================================="
echo "Done."

#!/usr/bin/env bash
# Compare Wile against other Scheme implementations
# Runs a subset of benchmarks on all available Schemes

set -e

# Default benchmarks (quick-running ones for comparison)
BENCHMARKS="${BENCHMARKS:-tak fib deriv peval}"

# Determine Wile path - prefer symlink, fall back to platform-specific
if [ -e "../../dist/wile" ]; then
    WILE_BIN="../../dist/wile"
    # Resolve symlink if needed
    if [ -L "$WILE_BIN" ]; then
        WILE_DIR=$(dirname "$WILE_BIN")
        WILE_TARGET=$(readlink "$WILE_BIN")
        WILE_BIN="$WILE_DIR/$WILE_TARGET"
    fi
elif [ -x "../../dist/darwin/arm64/wile" ]; then
    WILE_BIN="../../dist/darwin/arm64/wile"
elif [ -x "../../dist/linux/amd64/wile" ]; then
    WILE_BIN="../../dist/linux/amd64/wile"
elif [ -x "../../dist/linux/arm64/wile" ]; then
    WILE_BIN="../../dist/linux/arm64/wile"
elif [ -x "../../dist/darwin/amd64/wile" ]; then
    WILE_BIN="../../dist/darwin/amd64/wile"
else
    WILE_BIN=""
fi

# Scheme implementations to try
declare -A SCHEMES=(
    ["wile"]="$WILE_BIN --file"
    ["chez"]="scheme --script"
    ["racket"]="racket"
    ["chibi"]="chibi-scheme"
    ["guile"]="guile"
)

echo "Scheme Implementation Comparison"
echo "================================="
echo ""
echo "Benchmarks: $BENCHMARKS"
echo ""

# Check which implementations are available
AVAILABLE=()
for scheme in "${!SCHEMES[@]}"; do
    CMD=${SCHEMES[$scheme]}
    BIN=$(echo $CMD | awk '{print $1}')

    if [ "$scheme" = "wile" ]; then
        if [ -n "$WILE_BIN" ]; then
            AVAILABLE+=("$scheme")
            echo "✓ $scheme ($WILE_BIN)"
        else
            echo "✗ $scheme (not built - run 'make build')"
        fi
    elif command -v "$BIN" &> /dev/null; then
        AVAILABLE+=("$scheme")
        echo "✓ $scheme"
    else
        echo "✗ $scheme (not installed)"
    fi
done

if [ ${#AVAILABLE[@]} -eq 0 ]; then
    echo ""
    echo "Error: No Scheme implementations found"
    exit 1
fi

echo ""
echo "Will compare: ${AVAILABLE[*]}"
echo ""

RESULTS_FILE="comparison-$(date +%Y%m%d-%H%M%S).csv"

# CSV header
echo -n "benchmark" > "$RESULTS_FILE"
for scheme in "${AVAILABLE[@]}"; do
    echo -n ",$scheme" >> "$RESULTS_FILE"
done
echo "" >> "$RESULTS_FILE"

# Run each benchmark on each implementation
for bench in $BENCHMARKS; do
    echo "========================================="
    echo "Benchmark: $bench"
    echo "========================================="

    echo -n "$bench" >> "$RESULTS_FILE"

    for scheme in "${AVAILABLE[@]}"; do
        echo -n "  $scheme: "

        CMD=${SCHEMES[$scheme]}

        # Run benchmark with timeout
        if OUTPUT=$(timeout 30s $CMD "${bench}.scm" 2>&1); then
            # Extract time
            if TIME=$(echo "$OUTPUT" | grep "Total time:" | awk '{print $3}' | tr -d 's'); then
                if [ -n "$TIME" ]; then
                    echo "${TIME}s"
                    echo -n ",$TIME" >> "$RESULTS_FILE"
                else
                    echo "ERROR (no time found)"
                    echo -n ",ERROR" >> "$RESULTS_FILE"
                fi
            else
                echo "ERROR (parse failed)"
                echo -n ",ERROR" >> "$RESULTS_FILE"
            fi
        else
            EXIT_CODE=$?
            if [ $EXIT_CODE -eq 124 ]; then
                echo "TIMEOUT (>30s)"
                echo -n ",TIMEOUT" >> "$RESULTS_FILE"
            else
                echo "FAILED (exit $EXIT_CODE)"
                echo -n ",FAILED" >> "$RESULTS_FILE"
            fi
        fi
    done

    echo "" >> "$RESULTS_FILE"
    echo ""
done

echo "========================================="
echo "Comparison complete"
echo ""
echo "Results saved to: $RESULTS_FILE"
echo ""
echo "Summary:"
column -t -s, "$RESULTS_FILE"
echo ""

# Calculate relative performance if multiple schemes available
if [ ${#AVAILABLE[@]} -gt 1 ]; then
    echo "Relative Performance (lower is faster):"
    echo "========================================="

    # Find fastest implementation for each benchmark
    while IFS=, read -r bench times; do
        if [ "$bench" = "benchmark" ]; then
            continue
        fi

        IFS=, read -ra TIMES <<< "$times"
        MIN=999999
        MIN_SCHEME=""

        # Find minimum time
        for i in "${!AVAILABLE[@]}"; do
            TIME=${TIMES[$i]}
            if [[ $TIME =~ ^[0-9.]+$ ]]; then
                FLOAT=$(echo "$TIME" | awk '{print $1+0}')
                IS_MIN=$(echo "$FLOAT < $MIN" | bc -l)
                if [ "$IS_MIN" -eq 1 ]; then
                    MIN=$FLOAT
                    MIN_SCHEME=${AVAILABLE[$i]}
                fi
            fi
        done

        if [ -n "$MIN_SCHEME" ]; then
            echo -n "  $bench (fastest: $MIN_SCHEME ${MIN}s): "

            for i in "${!AVAILABLE[@]}"; do
                SCHEME=${AVAILABLE[$i]}
                TIME=${TIMES[$i]}

                if [[ $TIME =~ ^[0-9.]+$ ]]; then
                    RATIO=$(echo "scale=1; $TIME / $MIN" | bc -l)
                    echo -n "$SCHEME=${RATIO}x  "
                else
                    echo -n "$SCHEME=$TIME  "
                fi
            done
            echo ""
        fi
    done < "$RESULTS_FILE"
fi

echo ""
echo "To compare more benchmarks, run:"
echo "  BENCHMARKS=\"tak fib ack deriv sieve\" $0"

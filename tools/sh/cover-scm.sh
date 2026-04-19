#!/usr/bin/env bash
# Run the Scheme-level test suite with --cover and merge per-test
# coverage files into a single Go-format coverage profile.
#
# The output profile is compatible with `go tool cover -html`. It is
# NOT compatible with `go tool cover -func` because that tool parses
# the referenced source files as Go.
#
# Env:
#   SCHEME         Path to the wile binary (required)
#   COVERAGE_OUT   Merged coverage output path (required)

set -euo pipefail

SCHEME="${SCHEME:?SCHEME env var required}"
COVERAGE_OUT="${COVERAGE_OUT:?COVERAGE_OUT env var required}"

if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme interpreter not found at $SCHEME" >&2
    exit 1
fi

cd "$(dirname "$0")/../.."

TEST_FILES=$(find test stdlib/lib -name '*-test.scm' 2>/dev/null | sort)
if [ -z "$TEST_FILES" ]; then
    echo "No *-test.scm files found under test/ or stdlib/lib/" >&2
    exit 1
fi

TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

N=0
FAILED=0
for f in $TEST_FILES; do
    N=$((N + 1))
    out="$TMP_DIR/cov-$N.out"
    if ! "$SCHEME" --quiet --cover "$out" --cover-stdlib -f "$f" >/dev/null 2>&1; then
        FAILED=$((FAILED + 1))
        echo "  (failed: $f)" >&2
    fi
done

echo "Ran $N test file(s); $FAILED failed" >&2

# Merge Go cover profiles (mode: set). Same position across files →
# logical OR of counts, which falls out of "sort by count descending,
# keep highest-count row per position".
{
    echo "mode: set"
    find "$TMP_DIR" -name 'cov-*.out' -exec tail -q -n +2 {} + \
        | sort -k3,3nr \
        | awk '!seen[$1]++' \
        | sort
} > "$COVERAGE_OUT"

# Per-file summary: group by file prefix (before ':'), count hit vs total.
echo ""
awk '
    NR == 1 { next }                           # skip "mode: set"
    {
        split($1, parts, ":")
        file = parts[1]
        total[file]++
        if ($3 == "1") hit[file]++
        gtotal++
        if ($3 == "1") ghit++
    }
    END {
        for (f in total) {
            printf "%-60s %d/%d\n", f, hit[f]+0, total[f]
        }
        if (gtotal > 0) {
            pct = 100 * ghit / gtotal
            printf "TOTAL%55s %d/%d (%.1f%%)\n", "", ghit+0, gtotal, pct
        }
    }
' "$COVERAGE_OUT" | sort

if [ $FAILED -gt 0 ]; then
    exit 1
fi

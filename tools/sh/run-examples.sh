#!/usr/bin/env bash
# Run all Scheme example files and verify they exit 0.
# Excludes benchmarks, schelog, lib, and embedding directories.
# Reads examples/.ci-exclude for files that intentionally exit non-zero.
# Each example gets a 30-second timeout to catch hangs.
#
# Usage:
#   ./tools/sh/run-examples.sh <path-to-scheme-binary>

set -euo pipefail

SCHEME="${1:?Usage: run-examples.sh <scheme-binary>}"
TIMEOUT="${EXAMPLE_TIMEOUT:-30}"

if [ ! -x "$SCHEME" ]; then
    echo "Error: Scheme binary not found or not executable: $SCHEME"
    exit 1
fi

# Resolve to absolute path
SCHEME="$(cd "$(dirname "$SCHEME")" && pwd)/$(basename "$SCHEME")"

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLES_DIR="$REPO_ROOT/examples"
EXCLUDE_FILE="$EXAMPLES_DIR/.ci-exclude"

# Detect a timeout command that actually supports --kill-after. Presence is not
# enough: BSD/MacPorts ship a `timeout` that rejects the flag, and picking it by
# name alone makes EVERY example fail with a usage error. Probe each candidate
# and take the first that works; gtimeout (coreutils) is tried first because on
# macOS it is the GNU one. No candidate means run without a timeout. The
# duration ($TIMEOUT) is folded into the array so that an empty TIMEOUT_CMD
# expands to nothing — otherwise the bare duration would be run as a command.
TIMEOUT_CMD=()
for candidate in gtimeout timeout; do
    if command -v "$candidate" >/dev/null 2>&1 &&
       "$candidate" --kill-after=5 1 true >/dev/null 2>&1; then
        TIMEOUT_CMD=("$candidate" --kill-after=5 "$TIMEOUT")
        break
    fi
done

# Load exclusion list (paths relative to examples/, comments and blanks skipped)
declare -A excluded
if [ -f "$EXCLUDE_FILE" ]; then
    while IFS= read -r line; do
        line="${line%%#*}"       # strip comments
        line="${line// /}"       # strip whitespace
        [ -z "$line" ] && continue
        excluded["examples/$line"]=1
    done < "$EXCLUDE_FILE"
fi

passed=0
failed=0
skipped=0
failures=()

while IFS= read -r -d '' scm; do
    rel="${scm#"$REPO_ROOT/"}"

    if [ "${excluded[$rel]+_}" ]; then
        skipped=$((skipped + 1))
        continue
    fi

    if "${TIMEOUT_CMD[@]}" "$SCHEME" --quiet -f "$scm" >/dev/null 2>&1; then
        passed=$((passed + 1))
    else
        failed=$((failed + 1))
        failures+=("$rel")
    fi
done < <(find "$EXAMPLES_DIR" \
    -name '*.scm' \
    -not -path '*/benchmarks/*' \
    -not -path '*/schelog/*' \
    -not -path '*/lib/*' \
    -not -path '*/embedding/*' \
    -print0 | tr '\0' '\n' | sort | tr '\n' '\0')

total=$((passed + failed + skipped))
echo "Examples: $passed passed, $skipped skipped, $failed failed ($total total)"

if [ "$failed" -gt 0 ]; then
    echo ""
    echo "Failures:"
    for f in "${failures[@]}"; do
        echo "  FAIL  $f"
    done
    exit 1
fi

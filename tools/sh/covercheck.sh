#!/usr/bin/env bash
# Enforce per-package statement coverage threshold.
# Usage: covercheck.sh <threshold> <coverage.out>
# Exit 0 if all included packages meet threshold, 1 otherwise.

set -euo pipefail

if [ $# -ne 2 ]; then
	echo "Usage: $0 <threshold> <coverage.out>" >&2
	exit 2
fi

THRESHOLD="$1"
COVERAGE_FILE="$2"

if [ ! -f "$COVERAGE_FILE" ]; then
	echo "Error: coverage file not found: $COVERAGE_FILE" >&2
	exit 2
fi

MODULE="github.com/aalpar/wile"

# Packages excluded from coverage enforcement.
# Entry points, registration/wiring, helpers, test infrastructure,
# trivial embed-only packages, and newly-extracted packages pending
# test reorganization.
EXCLUDED_PKGS=(
	"cmd/wile"
	"cmd/typeswitchlint"
	"internal/testutil"
	"repl"
	"machine/testutil"
	"registry/testhelpers"
	"examples/embedding"
	"examples/embedding/source-tracking"
	"integration"
	"ruleguard"
	"stdlib"
)

is_excluded() {
	local pkg="$1"
	for excl in "${EXCLUDED_PKGS[@]}"; do
		if [ "$pkg" = "$excl" ]; then
			return 0
		fi
	done
	return 1
}

# Compute per-package coverage from the raw coverage profile.
# Each line (after the mode: header) looks like:
#   github.com/aalpar/wile/values/integer.go:42.15,44.2 3 1
# Fields: file:startLine.startCol,endLine.endCol numStatements count
# We aggregate numStatements and covered (count>0) per package directory.

declare -A PKG_TOTAL
declare -A PKG_COVERED

while IFS= read -r line; do
	# Skip mode line.
	if [[ "$line" == mode:* ]]; then
		continue
	fi

	# Extract file path (everything before the colon+line number).
	file="${line%%:*}"

	# Extract package from file path by stripping the filename.
	pkg_path="${file%/*}"

	# Strip module prefix.
	short_pkg="${pkg_path#"$MODULE"/}"
	if [ "$short_pkg" = "$MODULE" ]; then
		short_pkg="wile"
	fi

	# Extract numStatements and count from the line.
	# Format: <file:range> <numStmts> <count>
	rest="${line##* }"       # last field = count
	middle="${line% *}"      # strip count
	stmts="${middle##* }"    # last field of what remains = numStatements
	count="$rest"

	PKG_TOTAL["$short_pkg"]=$(( ${PKG_TOTAL["$short_pkg"]:-0} + stmts ))
	if [ "$count" -gt 0 ]; then
		PKG_COVERED["$short_pkg"]=$(( ${PKG_COVERED["$short_pkg"]:-0} + stmts ))
	fi
done < "$COVERAGE_FILE"

declare -a FAILED=()
declare -a PASSED=()
declare -a SKIPPED=()

for pkg in $(echo "${!PKG_TOTAL[@]}" | tr ' ' '\n' | sort); do
	total="${PKG_TOTAL[$pkg]}"
	covered="${PKG_COVERED[$pkg]:-0}"

	if [ "$total" -eq 0 ]; then
		pct="0.0"
	else
		pct=$(awk "BEGIN { printf \"%.1f\", ($covered / $total) * 100 }")
	fi

	if is_excluded "$pkg"; then
		SKIPPED+=("  skip  $pkg: ${pct}%")
		continue
	fi

	if awk "BEGIN { exit ($pct >= $THRESHOLD) ? 1 : 0 }"; then
		FAILED+=("  FAIL  $pkg: ${pct}% < ${THRESHOLD}%")
	else
		PASSED+=("  pass  $pkg: ${pct}%")
	fi
done

# Report results.
echo "Coverage threshold: ${THRESHOLD}%"
echo ""

if [ ${#PASSED[@]} -gt 0 ]; then
	echo "Passing (${#PASSED[@]}):"
	printf '%s\n' "${PASSED[@]}"
	echo ""
fi

if [ ${#FAILED[@]} -gt 0 ]; then
	echo "Failing (${#FAILED[@]}):"
	printf '%s\n' "${FAILED[@]}"
	echo ""
fi

if [ ${#SKIPPED[@]} -gt 0 ]; then
	echo "Excluded (${#SKIPPED[@]}):"
	printf '%s\n' "${SKIPPED[@]}"
	echo ""
fi

if [ ${#FAILED[@]} -gt 0 ]; then
	echo "FAIL: ${#FAILED[@]} package(s) below ${THRESHOLD}% coverage"
	exit 1
fi

echo "OK: all ${#PASSED[@]} package(s) meet ${THRESHOLD}% coverage"
exit 0

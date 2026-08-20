#!/usr/bin/env bash
# Select and run the multi-threaded tests under the race detector.
#
# The race detector reports only on genuinely concurrent access, so instrumenting
# a single-threaded test buys nothing and costs 10-30x wall clock. Measured on
# this tree: test/wile/algebra-tree-test.scm is 2.3s plain and 65.6s under -race,
# and the whole-tree race job hit the 30-minute wall on 2026-08-15 with
# TestSchemeSuiteInProcess still making progress.
#
# The selection is DERIVED from the source, not carried as a curated list, so a
# concurrency test is covered the day it lands rather than the day someone
# remembers to add it. A curated list fails OPEN: it goes green while testing
# nothing.
#
# A test file bears concurrency when it spawns a goroutine, calls t.Parallel (two
# parallel subtests can race on package state just as two goroutines can), or
# calls one of Scheme's thread/channel/mutex/timer primitives. A package also
# qualifies when its own production code spawns a goroutine, since that is where
# the shared state lives. Granularity is the FILE that declares a test, so a
# concurrency-bearing file contributes all of its test functions - conservative
# on purpose, because over-including costs seconds and under-including costs the
# gate.
#
# The Scheme and t.Parallel patterns are anchored to look like CALLS, which is
# what separates them from prose and from a denial assertion naming the primitive
# it expects to be absent. Three files in this tree mention t.Parallel only to say
# they must not use it.
#
# Cost, measured 2026-08-15 on an M4 Max: 1339 tests in 19 packages, ~4m40 cold
# against a 30-minute timeout for ./... . Over half of that is cmd/wile alone (12s
# plain, 145s instrumented), selected because main.go's setupSignals spawns a
# SIGQUIT stack-dump goroutine - which no test triggers, and which shares nothing
# but its own channel, so it can surface no race. It stays in anyway: dropping it
# means naming 66 individual tests, which is the curation this script exists to
# avoid. If the budget ever gets tight again, that is the first lever, and
# splitting the job per package is the second.
#
# Usage: race-selection.sh [--list] [-timeout <dur>]
#   --list  print the selection and exit without running anything

set -euo pipefail

# Per `go test` invocation, and there is one per package — so this bounds a single
# hung package and NOT the run. The run is bounded by timeout-minutes in
# .github/workflows/race.yml, which is sized to clear this one firing.
#
# 30m, raised from 8m on 2026-08-19 by request. 8m was ~3x the slowest package
# (cmd/wile, 93s local and ~2m30s projected on a runner, next slowest 37s), sized
# so no legitimate package could trip it.
#
# The current selection does not need the headroom and nothing here is near either
# bound. What it buys is room for a slow test that is not a concurrency test at
# all: selection is FILE-level, so one `bears_concurrency` or `spawns_in_production`
# hit takes every Test in that file or package with it. pkg/wile's
# TestHostCrash_InternalDefinesExceedingLocalSlots compiles 40,000 internal defines
# and takes 55s UNINSTRUMENTED — enough that `go test -race ./pkg/wile/`, the form
# a developer types by hand, already blows Go's own 10m default. It is not in this
# selection today; a widening predicate is all it would take.
#
# The cost is stated rather than hidden: a genuine deadlock now takes up to 30m to
# report instead of 8m. The dump is still what makes it diagnosable — `go test`
# prints every goroutine when its OWN -timeout fires, where a job-level kill prints
# nothing — so the job budget in .github/workflows/race.yml moves with this number
# and must stay above step + one firing.
TIMEOUT="30m"
LIST_ONLY=0
while [ $# -gt 0 ]; do
	case "$1" in
	--list)
		LIST_ONLY=1
		shift
		;;
	-timeout)
		TIMEOUT="$2"
		shift 2
		;;
	*)
		echo "Usage: $0 [--list] [-timeout <dur>]" >&2
		exit 2
		;;
	esac
done

# A goroutine spawn: `go f(`, `go p.f(`, or `go func(`.
GO_SPAWN='^[[:space:]]*go[[:space:]]+([a-zA-Z_][a-zA-Z0-9_.]*\(|func[[:space:]]*\()'

# A t.Parallel() call, as a statement rather than as prose about one.
TEST_PARALLEL='^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*\.Parallel\(\)'

# A call to a Scheme concurrency primitive, as it appears inside a test's source
# string. The leading paren is what separates a CALL from prose.
SCM_CONCURRENCY='\((thread-start!|make-thread|thread-join!|thread-terminate!|thread-sleep!|thread-yield!|make-channel|channel-send!|channel-receive!|channel-close!|make-mutex|mutex-lock!|mutex-unlock!|make-condition-variable|condition-variable-(wait|signal|broadcast)!|make-timer|timer-start!|parallel-map)'

# Test functions excluded despite living in a concurrency-bearing file, with the
# reason each is safe to exclude. Keep this list short and keep every entry
# falsifiable: the ratchet below re-checks the claim on every run.
#
#   TestSchemeSuiteInProcess - runs 47 Scheme conformance files in-process. Its
#     only goroutine is captureStdout's pipe drainer, whose one shared value is
#     handed over through a buffered channel after io.Copy returns, and whose own
#     doc comment records that the driver must stay serial. No *-test.scm it runs
#     uses a thread primitive. It is also ~25 of the 30 minutes the job used to
#     spend.
EXCLUDED_TESTS=(
	"TestSchemeSuiteInProcess"
)

is_excluded() {
	local t="$1" e
	for e in "${EXCLUDED_TESTS[@]}"; do
		if [ "$t" = "$e" ]; then
			return 0
		fi
	done
	return 1
}

# Packages where the production-goroutine rule does NOT apply. The goroutine is
# real, but no test can make it race, so selecting every test in the package buys
# nothing. This does not exclude the package outright: its concurrency-bearing
# test FILES are still selected on their own merit.
#
#   cmd/wile - its one production goroutine is setupSignals' SIGQUIT stack-dump
#     handler. TestSetupSignalsDirect does start it, but nothing sends SIGQUIT, so
#     it sits in `range sigChan` for the life of the process and touches only that
#     channel and a local buffer: there is no shared state for the detector to
#     find. The rule was pulling in 29 unrelated tests worth 51.7s instrumented
#     locally, 65% of the whole run's time on a CI runner. mcp_test.go keeps its 36
#     tests, which spawn goroutines that contend on the MCP session lock and are
#     the regression test for a real one - see TestHandleEval_ZeroTimeoutDoesNotWedgeServer.
EXCLUDED_PROD_PKGS=(
	"cmd/wile"
)

is_prod_excluded() {
	local importpath="$1" e
	for e in "${EXCLUDED_PROD_PKGS[@]}"; do
		case "$importpath" in
		*"/$e")
			return 0
			;;
		esac
	done
	return 1
}

# bears_concurrency reports whether a single Go file matches any predicate.
bears_concurrency() {
	local f="$1"
	if grep -Eq "$GO_SPAWN" "$f"; then
		return 0
	fi
	if grep -Eq "$TEST_PARALLEL" "$f"; then
		return 0
	fi
	if grep -Eq "$SCM_CONCURRENCY" "$f"; then
		return 0
	fi
	return 1
}

# spawns_in_production reports whether a package's own non-test sources spawn a
# goroutine. Every test in such a package is selected: the concurrency is in the
# code under test rather than in the test.
spawns_in_production() {
	local dir="$1" g
	for g in "$dir"/*.go; do
		if [ ! -f "$g" ]; then
			continue
		fi
		case "$g" in
		*_test.go)
			continue
			;;
		esac
		if grep -Eq "$GO_SPAWN" "$g"; then
			return 0
		fi
	done
	return 1
}

# Every package in the module, with its directory. Derived from go list rather
# than from a directory walk so a new top-level tree cannot be silently left out
# of the scan - which is what a hardcoded root list did to coverage/, integration/
# and tools/ on the first draft of this script.
pkgs=$(go list -f '{{.ImportPath}} {{.Dir}}' ./... 2>/dev/null)
if [ -z "$pkgs" ]; then
	echo "race-selection: go list returned nothing" >&2
	exit 1
fi

# Ratchet for the TestSchemeSuiteInProcess exclusion: the moment a Scheme test
# file under the roots that driver walks starts a thread, excluding it stops being
# safe and this fails instead of silently dropping the coverage.
scm_thread_users=$(grep -rlE "$SCM_CONCURRENCY" --include='*-test.scm' test pkg/stdlib/lib 2>/dev/null || true)
if [ -n "$scm_thread_users" ]; then
	echo "race-selection: a Scheme conformance test now uses a concurrency primitive:" >&2
	echo "$scm_thread_users" >&2
	echo "race-selection: TestSchemeSuiteInProcess can no longer be excluded - drop it from EXCLUDED_TESTS" >&2
	exit 1
fi

# Ratchet for EXCLUDED_PROD_PKGS. Each entry's premise is "one goroutine site, and
# it cannot race". A second site means a different goroutine has appeared and the
# premise has to be re-argued rather than inherited.
for excluded in "${EXCLUDED_PROD_PKGS[@]}"; do
	prod_files=$(ls "$excluded"/*.go 2>/dev/null | grep -v '_test\.go$' || true)
	if [ -z "$prod_files" ]; then
		echo "race-selection: $excluded is in EXCLUDED_PROD_PKGS but has no non-test sources" >&2
		exit 1
	fi
	# shellcheck disable=SC2086
	sites=$(grep -hE "$GO_SPAWN" $prod_files 2>/dev/null | wc -l | tr -d ' ')
	if [ "$sites" != "1" ]; then
		echo "race-selection: $excluded has $sites production goroutine site(s), not the 1 its exclusion assumes" >&2
		echo "race-selection: re-argue the exclusion in EXCLUDED_PROD_PKGS or drop it" >&2
		exit 1
	fi
done

# importpath<TAB>test, one line per selected test function.
selected=$(
	printf '%s\n' "$pkgs" | while read -r importpath dir; do
		prod=0
		if ! is_prod_excluded "$importpath"; then
			if spawns_in_production "$dir"; then
				prod=1
			fi
		fi
		for f in "$dir"/*_test.go; do
			if [ ! -f "$f" ]; then
				continue
			fi
			keep=$prod
			if [ "$keep" -eq 0 ]; then
				if bears_concurrency "$f"; then
					keep=1
				fi
			fi
			if [ "$keep" -eq 0 ]; then
				continue
			fi
			grep -Eo '^func Test[A-Za-z0-9_]*' "$f" | awk '{print $2}' | while IFS= read -r t; do
				if is_excluded "$t"; then
					continue
				fi
				printf '%s\t%s\n' "$importpath" "$t"
			done
		done
	done | sort -u
)

if [ -z "$selected" ]; then
	echo "race-selection: selected no tests, which cannot be right" >&2
	exit 1
fi

pkg_count=$(printf '%s\n' "$selected" | cut -f1 | sort -u | wc -l | tr -d ' ')
test_count=$(printf '%s\n' "$selected" | wc -l | tr -d ' ')
echo "race-selection: $test_count multi-threaded test(s) in $pkg_count package(s)"

# One `go test` per package, with -run naming that package's selected tests.
# Anchored so TestFoo does not also pull in TestFooBar.
runs=$(printf '%s\n' "$selected" |
	awk -F'\t' '{ if ($1 != p) { if (p != "") printf "%s\t^(%s)$\n", p, r; p = $1; r = $2 } else r = r "|" $2 } END { if (p != "") printf "%s\t^(%s)$\n", p, r }')

if [ "$LIST_ONLY" -eq 1 ]; then
	printf '%s\n' "$runs" | while IFS=$'\t' read -r pkg regex; do
		n=$(printf '%s' "$regex" | awk -F'|' '{print NF}')
		printf '  %-48s %s test(s)\n' "$pkg" "$n"
	done
	exit 0
fi

started=$SECONDS
status=0
while IFS=$'\t' read -r pkg regex; do
	if ! go test -race -timeout "$TIMEOUT" -run "$regex" "$pkg"; then
		status=1
	fi
done <<EOF
$runs
EOF
elapsed=$((SECONDS - started))

# Printed on every run so the next person sizing the budget reads a number
# instead of re-measuring by hand. -timeout above is PER PACKAGE, so it bounds an
# individual hang and NOT this total; the job-level timeout-minutes in
# .github/workflows/race.yml is what bounds the whole thing.
printf 'race-selection: %dm%02ds total across %s package(s)\n' "$((elapsed / 60))" "$((elapsed % 60))" "$pkg_count"

if [ "$status" -ne 0 ]; then
	echo "race-selection: FAILED" >&2
	exit 1
fi
echo "race-selection: all selected package(s) passed under -race"

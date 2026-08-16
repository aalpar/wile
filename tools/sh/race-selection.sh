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
# 8m is ~3x the slowest package: cmd/wile, 93s local and ~2m30s projected on a
# runner (it was 3m46s there before its production-rule exclusion), with the next
# slowest at 37s. So no legitimate package can trip it. It stays deliberately short
# because a `go test` timeout dumps every goroutine, and that dump is the whole
# diagnostic when something deadlocks.
TIMEOUT="8m"
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

# importpath<TAB>test, one line per selected test function.
selected=$(
	printf '%s\n' "$pkgs" | while read -r importpath dir; do
		prod=0
		if spawns_in_production "$dir"; then
			prod=1
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

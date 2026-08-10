#!/usr/bin/env bash
# Check that TODO.md, the index of record, is complete in both directions.
#
# TODO.md is the index into plans/ (plans/CLAUDE.local.md, "Where the index
# lives") and, at synthesis time, into a review run's confirmed findings. Every
# other process gate in this tree is keyed on that index -- planlint.sh's
# "index" signal is a TODO.md lookup (planlint.sh:144-150) -- so a gate keyed on
# the index is blind to whatever the index omits, and its silence is
# indistinguishable from a pass. The index must therefore be checked for
# completeness before anything keyed on it is trusted. That is why `make
# planlint` depends on this target.
#
# Two directions, one question:
#
#   plans    every file in plans/ has a row in TODO.md. A plan with no row is
#            outside every index-keyed gate's reach, including planlint's.
#   reviews  every CONFIRMED finding in a review run -- each top-level entry
#            under a REVIEW.md "## 2. Confirmed findings" -- has a TODO.md row.
#            A confirmed finding with no row is unactioned work that no triage
#            pass will ever see again.
#
# Neither direction is seeded from a list: both derive their subject by walking
# the directory, because the list is the thing being derived. A hand-written set
# of expected members would make the check agree with whoever wrote the set.
#
# Membership test, both directions: the name appears anywhere in TODO.md, by
# literal substring. Deliberately loose -- this asks whether the index knows the
# item exists at all, not whether the row says anything true. planlint.sh is
# what reads the row.
#
# The reviews direction is a RATCHET, not a clean gate. The confirmed-finding
# backlog predates the check by five review runs, so a bare assertion would be
# red forever and get switched off. The per-run ceilings below are DERIVED by
# running this script, never curated by hand, and may only be lowered -- the
# same discipline, and for the same reason, as `uncleanedDocs` in
# cmd/wile/docs_refs_test.go ("exists to stop the set from growing ... never add
# one"). The plans direction needs no ratchet: it goes to zero.
#
# plans/, memory/ and reviews/ are all gitignored, so this check has inputs only
# on a maintainer's checkout and is deliberately NOT in `make ci` -- wiring it
# there would yield a check that passes vacuously on a clone, which is strictly
# worse than leaving it out. Same boundary, same decision, as `make planlint`
# (Makefile:642-645).
#
# Usage (run from the repo root):
#   ./tools/sh/indexlint.sh [plans-dir] [reviews-dir] [index-file]

set -euo pipefail

PLANS_DIR="${1:-plans}"
REVIEWS_DIR="${2:-reviews}"
INDEX_FILE="${3:-TODO.md}"

if [ ! -f "$INDEX_FILE" ]; then
    echo "indexlint: no $INDEX_FILE -- nothing to check against"
    exit 0
fi

findings=0

# ---------------------------------------------------------------------------
# Direction: plans/ -> TODO.md
# ---------------------------------------------------------------------------

if [ -d "$PLANS_DIR" ]; then
    plans_total=0
    plans_missing=0
    missing_list=""

    for plan in "$PLANS_DIR"/*.md; do
        [ -f "$plan" ] || continue
        plans_total=$((plans_total + 1))
        grep -qF "$(basename "$plan")" "$INDEX_FILE" && continue
        plans_missing=$((plans_missing + 1))
        missing_list="$missing_list  $plan"$'\n'
    done

    echo "== plans/ -> $INDEX_FILE =="
    if [ "$plans_missing" -gt 0 ]; then
        printf '%s' "$missing_list"
        findings=$((findings + plans_missing))
    fi
    echo "indexlint: $plans_total plan file(s), $plans_missing with no $INDEX_FILE row"
else
    echo "indexlint: no $PLANS_DIR/ directory -- plans direction skipped (it is gitignored)"
fi

# ---------------------------------------------------------------------------
# Direction: reviews/<date>/REVIEW.md section 2 -> TODO.md
# ---------------------------------------------------------------------------

# review_ceiling reports the ratchet for a run: the number of confirmed-finding
# citations that may still be missing a TODO.md row. Derived by running this
# script; lower it when rows are opened, never raise it. A run not listed here
# has a ceiling of zero, which is the point -- a new review run must index its
# own findings at synthesis time.
#
# 2026-07-25 is set one below its measured backlog because that run owes exactly
# one row this check was built to demand: pkg/wile/dialect_nomutation.go,
# CONFIRMED at reviews/2026-07-25/VERDICTS.md:180 and absent from TODO.md.
review_ceiling() {
    case "$1" in
        2026-07-12) echo 3 ;;
        2026-07-13) echo 31 ;;
        2026-07-17) echo 13 ;;
        2026-07-25) echo 71 ;;
        2026-08-07) echo 63 ;;
        *) echo 0 ;;
    esac
}

# confirmed_citations echoes the distinct repo paths cited by the entries of a
# REVIEW.md's "## 2. Confirmed findings" section.
#
# An entry states its subject as a `path:line` code span on the entry's opening
# line, and the five archived runs write that opening line three different ways
# -- a bullet ("- `pkg/foo.go:42` -- ..."), a bold header ("**2.1.1 `pkg/foo.go:42`
# [group]**", "**S1 -- title** . `pkg/foo.go:42`"), and a numbered item
# ("1. `pkg/foo.go:42` -- ..."). All three are matched; continuation lines are
# not, which is what keeps a SCOPE note's incidental citations out.
#
# The trailing ":<line>" is dropped: line numbers drift, and the index tracks
# work, not offsets.
confirmed_citations() {
    awk '/^## 2\./ { inside = 1; next } /^## [0-9]+\./ { inside = 0 } inside' "$1" \
        | grep -E '^(- |\*\*|[0-9]+\. )' \
        | grep -oE '`[A-Za-z0-9_./-]+\.(go|scm|sld|md|sh):[0-9]+' \
        | sed -E 's/^`//; s/:[0-9]+$//' \
        | sort -u
}

if [ -d "$REVIEWS_DIR" ]; then
    for review in "$REVIEWS_DIR"/*/REVIEW.md; do
        [ -f "$review" ] || continue
        run="$(basename "$(dirname "$review")")"
        ceiling="$(review_ceiling "$run")"

        total=0
        missing=0
        missing_list=""
        while IFS= read -r cited; do
            [ -n "$cited" ] || continue
            total=$((total + 1))
            grep -qF "$cited" "$INDEX_FILE" && continue
            missing=$((missing + 1))
            missing_list="$missing_list  $cited"$'\n'
        done < <(confirmed_citations "$review")

        echo
        echo "== $review section 2 -> $INDEX_FILE =="
        if [ "$missing" -gt "$ceiling" ]; then
            printf '%s' "$missing_list"
            echo "indexlint: $run: $missing of $total confirmed-finding citation(s) have no" \
                 "$INDEX_FILE row, ceiling is $ceiling"
            findings=$((findings + 1))
        else
            echo "indexlint: $run: $missing of $total confirmed-finding citation(s) have no" \
                 "$INDEX_FILE row, at or under the ceiling of $ceiling"
        fi
    done
else
    echo "indexlint: no $REVIEWS_DIR/ directory -- reviews direction skipped (it is gitignored)"
fi

echo
echo "indexlint: $findings finding(s)"
[ "$findings" -eq 0 ] || exit 1

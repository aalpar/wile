#!/usr/bin/env bash
# Flag plan files whose Status header is stale with respect to reality.
#
# A plan's status lives in two places -- its own "**Status:**" header and
# TODO.md, the index of record (see plans/CLAUDE.local.md, "Where the index
# lives"). Only TODO.md is on the post-merge checklist, so per-file headers rot,
# and a triage pass then inherits a false "open" verdict from the header.
#
# A status keyword alone is never a finding: plenty of plans are legitimately
# unstarted. A finding requires an open-sounding header AND a positive signal
# that the work landed. Three such signals, in the order they have proven
# themselves:
#
#   twin   an "-impl" twin archived to memory/ -- memory/ is where SHIPPED and
#          SUPERSEDED plans go, so a twin's presence there is direct evidence.
#          This is the failure that bit three triage passes running
#          (escape-gated frame reclaim, the layered-environment carve, frame
#          reclaim), where the -impl twin was archived COMPLETE while the design
#          note left in plans/ still read "not started".
#   index  TODO.md's own row for the plan reports the work shipped.
#   pr     a "PR #N" cited *on the Status line* that GitHub reports as MERGED.
#
# The Status-line restriction on "pr" is deliberate and is a departure from the
# cheap version this check was specified as ("extract any cited PR #N"). Scanning
# the whole body was measured against this repo and every hit was a reference to
# some *neighbouring* plan's PR -- plans cite each other's PRs constantly, so
# body-wide scanning is all noise. A merged PR on the plan's own Status line is
# instead a self-contradiction, and carries no false positives.
#
# The "pr" signal needs a working `gh`; without one it is skipped and said so,
# rather than failing the run. plans/ is gitignored, so a checkout without it
# is normal and exits 0.
#
# Companion to check-docs-orphans.sh (reverse-direction doc reachability).
#
# Usage (run from the repo root):
#   ./tools/sh/planlint.sh [plans-dir] [memory-dir] [index-file]

set -euo pipefail

PLANS_DIR="${1:-plans}"
MEMORY_DIR="${2:-memory}"
INDEX_FILE="${3:-TODO.md}"

if [ ! -d "$PLANS_DIR" ]; then
    echo "planlint: no $PLANS_DIR/ directory -- nothing to check (it is gitignored)"
    exit 0
fi

# Header phrasings that claim the work has not landed. Matched case-insensitively
# against the Status line only, never the body.
open_re='not started|no code written|not yet approved|not scheduled|design only|design-level|design draft|design note|ready to implement|implementation-ready|implementation not started|proposed|pending|awaiting|parked|queued|stub|draft|0/[0-9]+ (tasks|phases)'

# Phrasings that report the work landed. Used for the index row, and to let a
# header that already says so off the hook.
done_re='SHIPPED|COMPLETE|COMPLETED|MERGED|IMPLEMENTED|LANDED|ARCHIVED|DONE'

# slug_of strips the date prefix, the .local/.md suffixes, and a trailing
# -design/-impl role marker, leaving the hyphenated topic tokens.
slug_of() {
    local base
    base="$(basename "$1")"
    base="${base%.md}"
    base="${base%.local}"
    base="$(printf '%s' "$base" | sed -E 's/^[0-9]{4}-[0-9]{2}-[0-9]{2}-//; s/-(design|impl|impl-plan)$//')"
    printf '%s' "$base"
}

# slugs_pair reports whether two slugs name the same topic, by their shared
# leading hyphen-separated tokens. Two conditions, both needed:
#
#   at least 2 shared leading tokens   keeps one-token families apart, since
#                                      "free-*" and "scope-*" are each several
#                                      unrelated plans
#   shared >= 60% of the shorter slug  keeps two-token families apart, since
#                                      "wile-goast-*" and "engine-services-*"
#                                      are likewise several unrelated plans
#
# Calibrated against this repo: the pair together admit
# layered-environment-impl <-> layered-environment-architecture (the documented
# failure) and reject wile-goast-ac-match-migration <->
# wile-goast-algebra-extraction-impl and engine-services-generic-keyed-slot <->
# engine-services-seam-and-forms-tenant-impl, which share a namespace and
# nothing else.
slugs_pair() {
    local a="$1" b="$2" n=0 shorter
    local -a at bt
    IFS='-' read -r -a at <<< "$a"
    IFS='-' read -r -a bt <<< "$b"
    local i
    for ((i = 0; i < ${#at[@]} && i < ${#bt[@]}; i++)); do
        [ "${at[$i]}" = "${bt[$i]}" ] || break
        n=$((n + 1))
    done
    [ "$n" -ge 2 ] || return 1
    shorter=${#at[@]}
    [ "${#bt[@]}" -lt "$shorter" ] && shorter=${#bt[@]}
    [ $((n * 10)) -ge $((shorter * 6)) ]
}

# status_line_of echoes the plan's Status header, or nothing if it has none.
# Only the first 40 lines are considered -- a "Status:" deeper in the body is a
# section heading about something else, not the file's own verdict.
status_line_of() {
    head -n 40 "$1" \
        | grep -m1 -E '^[[:space:]]*(>[[:space:]]*)?(\*\*)?Status(\*\*)?[[:space:]]*(:|\*\*:)' \
        || true
}

gh_ok=0
if command -v gh >/dev/null 2>&1 && gh auth status >/dev/null 2>&1; then
    gh_ok=1
fi

findings=0
checked=0

for plan in "$PLANS_DIR"/*.md; do
    [ -f "$plan" ] || continue

    status="$(status_line_of "$plan")"
    [ -n "$status" ] || continue
    checked=$((checked + 1))

    # A header that already reports the work landed is not stale in this
    # direction, whatever else it says.
    printf '%s' "$status" | grep -qE "$done_re" && continue
    printf '%s' "$status" | grep -qiE "$open_re" || continue

    plan_slug="$(slug_of "$plan")"
    signals=""

    # Signal "twin": an -impl twin sitting in memory/.
    if [ -d "$MEMORY_DIR" ]; then
        for twin in "$MEMORY_DIR"/*impl*.md; do
            [ -f "$twin" ] || continue
            if slugs_pair "$plan_slug" "$(slug_of "$twin")"; then
                signals="$signals  twin:  archived impl twin exists: $twin"$'\n'
                break
            fi
        done
    fi

    # Signal "index": TODO.md's row for this plan reports it shipped.
    if [ -f "$INDEX_FILE" ]; then
        row="$(grep -m1 -F "$(basename "$plan")" "$INDEX_FILE" || true)"
        if [ -n "$row" ] && printf '%s' "$row" | grep -qE "$done_re"; then
            signals="$signals  index: $INDEX_FILE reports this shipped"$'\n'
        fi
    fi

    # Signal "pr": a PR cited on the Status line that GitHub reports merged.
    if [ "$gh_ok" -eq 1 ]; then
        while IFS= read -r num; do
            [ -n "$num" ] || continue
            state="$(gh pr view "$num" --json state --jq .state 2>/dev/null || true)"
            if [ "$state" = "MERGED" ]; then
                signals="$signals  pr:    PR #$num on the Status line is MERGED"$'\n'
            fi
        done < <(printf '%s' "$status" | grep -oE '#[0-9]+' \
                     | tr -d '#' | sort -un)
    fi

    [ -n "$signals" ] || continue
    echo "$plan"
    printf '%s' "$signals"
    echo "  status: ${status#"${status%%[![:space:]]*}"}"
    findings=$((findings + 1))
done

echo
echo "planlint: $checked plan(s) with a Status header, $findings finding(s)"
if [ "$gh_ok" -eq 0 ]; then
    echo "planlint: 'gh' unavailable or unauthenticated -- merged-PR signal skipped"
fi

[ "$findings" -eq 0 ] || exit 1

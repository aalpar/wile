# Copilot Review Data Mining

**Status:** Imminent, not started. Tier 2 is the target output; Tier 3/4 gated on Tier 2 results.

## Motivation

Copilot PR reviews have been running on this repo for a while. Before deciding whether they're worth the friction — or what categories of comments are most useful — we need raw data on what Copilot has actually been saying, and whether/how those comments were addressed.

## Scale (as of 2026-04-20)

| Source | Count |
|---|---|
| Total commits | 1,768 |
| Commits with "copilot" in message | 97 (5.5%) |
| PRs with Copilot reviews | **285** |

The 3× gap between commit-message hits and PR-review count is the key signal — most Copilot-influenced commits don't self-identify. Commit-message grep is a ~3× undercount and is not worth producing on its own.

## Tiers

### Tier 1 — commit-message grep (rejected)
`git log --grep='[Cc]opilot'` → 97 commits. Cheap (~5 min) but dishonest. Skip.

### Tier 2 — PR review enumeration (target)
For each of the 285 PRs with Copilot reviews, extract:
- Copilot's review comments (file path, line, body)
- The commits that followed each comment (PR head SHAs after review timestamp)
- Final merged-state diff for the files/lines Copilot commented on

Output: a flat table (CSV or Scheme s-expr file in `plans/` or `audit/`) with columns:
```
(pr-number, review-timestamp, file, line, comment-body, follow-up-shas, file-diff-around-line)
```

Approx cost: 285 PRs × ~3 gh-api calls each ≈ 850 requests. Rate-limited but tractable in 30-60 min wall-clock.

### Tier 3 — accepted/rejected/ignored classification (gated)
Per Copilot comment, determine:
- **Accepted** — follow-up commit modified the flagged line in a way consistent with the suggestion
- **Partially accepted** — a related change was made but not exactly what was suggested
- **Acknowledged, disagreed** — a reply comment or commit with explicit counter-reasoning
- **Ignored** — no follow-up change to the flagged region before merge

Requires reading diffs and interpreting review threads. Cost: 2-4 hrs after Tier 2 data is in hand.

### Tier 4 — categorization and trend analysis (gated)
After Tier 3 classification, bucket comments by category (correctness, style, naming, docs, false positive, type-consistency) and produce:
- Acceptance rate over time
- Acceptance rate by category
- File/subsystem hotspots
- Comparison to any human reviews if present

Cost: 4-8 hrs after Tier 3.

## Method for Tier 2

### API calls per PR
1. `gh api /repos/aalpar/wile/pulls/{N}/reviews` — list reviews, filter to `user.login == "copilot-pull-request-reviewer[bot]"`
2. `gh api /repos/aalpar/wile/pulls/{N}/comments` — get review-line comments; filter to the same bot user
3. `gh pr view {N} --json commits,mergedAt,mergeCommit,headRefOid` — get the commits on the PR

### Enumeration
```sh
gh pr list --state all --limit 1000 \
  --search 'reviewed-by:app/copilot-pull-request-reviewer' \
  --json number,title,mergedAt,state \
  > copilot-prs.json
```

### Rate limiting
Default `gh` rate is 5,000 req/hr authenticated. 850 requests ≈ 17% of budget; comfortable. No explicit sleep needed, but use a small delay (`sleep 0.1`) between batched calls to be polite. Capture any `x-ratelimit-remaining` headers when they drop below 500 and pause if so.

### Output shape (proposed)
Scheme s-expressions in `audit/copilot-review-inventory.scm` — keeps it consistent with other wile-internal data mining outputs (`audit/wile-axis-b.scm`, `plans/axis-b-manifest.scm`). Example entry:
```scheme
((pr 674)
 (timestamp "2026-04-20T14:32:17Z")
 (file "registry/helpers/args.go")
 (line 79)
 (body "ParseOptionalStartEnd now rejects non-empty tail after parsing end...")
 (follow-up-shas ("3c8f1f70"))
 (status pending))  ; set by Tier 3
```

Alternative: TSV for easier spreadsheet ingestion. Choose based on whether Tier 3/4 will live in Scheme tooling or elsewhere.

## Decision Criteria for Tier 3/4

Go to Tier 3 if Tier 2 shows:
- Comment volume per PR is non-trivial (>1 comment avg) — implies classification is useful
- Comment bodies are distinct enough that manual inspection is tractable (not 285 identical complaints)
- The user wants to answer "is Copilot worth it?" rather than "what does Copilot say?"

Go to Tier 4 if Tier 3 shows:
- Clear category clusters (>3 distinct buckets with non-trivial membership)
- Time-based drift in acceptance rate (suggests Copilot or the codebase is changing)
- Subsystem hotspots that suggest actionable curation (e.g., "Copilot is usually right about X subsystem, usually wrong about Y")

## Non-goals

- Comparing Copilot to any specific human reviewer — there is no human-reviewer baseline in this repo worth comparing against.
- Inferring intent from review silence — "no follow-up commit" doesn't imply "disagreement"; it may mean "agreed but moot" or "agreed but PR already merged before Copilot posted."
- Generating Copilot-quality metrics that could feed back into training/config — out of scope; analytic only.

## Files to produce

| Path | Content | Tier |
|---|---|---|
| `audit/copilot-review-inventory.scm` (or `.tsv`) | Raw Tier 2 data | Tier 2 |
| `plans/2026-MM-DD-copilot-review-findings.md` | Summary of Tier 3 classification | Tier 3 |
| `plans/2026-MM-DD-copilot-review-trends.md` | Tier 4 synthesis | Tier 4 |

## Open questions

- Should the inventory include PRs where Copilot reviewed but posted no line comments (generic "LGTM" or summary-only)? Probably yes for completeness, with `(comments ())` for those.
- Should we include PRs reviewed by `copilot-pull-request-reviewer[bot]` but also by a human reviewer, for comparison? Only if Tier 3 reveals this subset is interesting.
- Output format: Scheme s-expr (consistent with repo conventions) vs. TSV (easier external tooling). Defer until Tier 3 tooling choice is made.

# Architectural Code Review

**Date:** 2026-02-12
**Scope:** Full codebase review across values/, machine/, tokenizer/parser/syntax, registry/core/, extensions, and public API.

---

## Completed

All HIGH (H4, H6-H9), Thread Safety (T1-T5), MEDIUM (M1-M11), and recommended fix phases 1-2 are fixed. See `ARCHITECTURAL_REVIEW_FIXES.md` for fix details.

LOW priority: 18 of 19 fixed/resolved, 1 documented (L10), 1 deferred (L19).

---

## Resolved LOW Items

| # | Location | Issue | Resolution |
|---|----------|-------|------------|
| L3 | `values/channel.go` | `ChannelSelect` busy-spins without `reflect.Select` | **Debunked** — implementation uses `reflect.Select` (line 296); no busy-spin exists |
| L11 | `internal/extensions/eval/prim_eval.go` | `eval` doesn't inherit dynamic context | **Fixed** — commit `a722464` added `SetThread` propagation to eval/load sub-contexts |
| L15 | `internal/extensions/threads/prim_threads.go` | `thread-sleep!` ignores context cancellation | **Debunked** — lines 243-246 properly handle `ctx.Done()` via select |

## Open LOW Items

| # | Location | Issue | Status |
|---|----------|-------|--------|
| L19 | `internal/tokenizer/tokenizer.go:2294` | `isExtendedExponentMarkerForRadix` ignores radix | Deferred (exotic edge case) |

### Deferral Criteria

Fix only if:
- R7RS test suite compliance requires it
- Users report actual issues
- Part of broader refactoring efforts

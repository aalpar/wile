# Architectural Code Review

**Date:** 2026-02-12
**Scope:** Full codebase review across values/, machine/, tokenizer/parser/syntax, registry/core/, extensions, and public API.

---

## Completed

All HIGH (H4, H6-H9), Thread Safety (T1-T5), and MEDIUM (M1-M11) issues are fixed. See `ARCHITECTURAL_REVIEW_FIXES.md` for fix details and git history for the original document.

LOW priority: 15 of 19 fixed, 1 documented (L10), 4 deferred.

---

## Open LOW Items

| # | Location | Issue | Status |
|---|----------|-------|--------|
| L3 | `values/channel.go:253` | `ChannelSelect` busy-spins without `reflect.Select` | Deferred (extension-only, low usage) |
| L11 | `internal/extensions/eval/prim_eval.go:35` | `eval` doesn't inherit dynamic context | Deferred (rare use case) |
| L15 | `internal/extensions/threads/prim_threads.go:214` | `thread-sleep!` ignores context cancellation | Deferred (improves shutdown) |
| L19 | `internal/tokenizer/tokenizer.go:2280` | `isExtendedExponentMarkerForRadix` ignores radix | Deferred (exotic edge case) |

### Deferral Criteria

Fix these only if:
- R7RS test suite compliance requires them
- Users report actual issues
- Part of broader refactoring efforts

---

## Recommended Fix Order

| Phase | Items | Risk |
|-------|-------|------|
| 1 (Quick wins) | 1.1 BigFloat Hashable (see REFACTORING.md) | None |
| 2 (Convention) | 3.1 Bare sentinels, 3.2 `== io.EOF` in tests | Low |
| 3 (Deferred LOW) | L3, L11, L15, L19 | Low-Medium |

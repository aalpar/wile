# Error Creation Audit — Non-Wrap Patterns

**Status:** COMPLETE — All actionable items resolved

## Summary

All error creation sites now follow the two-layer convention (sentinel + `WrapForeignErrorf`).

**Resolved:**
- `NewForeignErrorf` violations (~75 sites across 13 files) eliminated by panic-to-error refactoring (PR #212, #215)
- `NewForeignError` bare calls (24 sites across 7 files in `registry/core/`) wrapped with sentinels (commit `a469ce9`)
- `fmt.Errorf` in production reduced from 6 to 2 (both are panic-recovery wrappers — acceptable)
- `ErrNonContinuableException` sentinel added (commit `a469ce9`)

## Remaining (Acceptable — No Action Needed)

### `fmt.Errorf` in Production (2 sites)

| File | Notes |
|------|-------|
| `values/scheme_equals.go:40` | Panic recovery wrapper |
| `internal/syntax/syntax_equals.go:40` | Panic recovery wrapper |

These wrap recovered panics in `defer`/`recover` blocks. Using `fmt.Errorf` here is acceptable since the panic value is unknown and may not be a `ForeignError`.

### Not Runtime Errors

- `values/scheme_equals.go` — quicktest `qt.Checker`, returns `error` to signal check failure
- `internal/syntax/syntax_equals.go` — same pattern
- All `_test.go` files — test fixtures

## Open Design Question

Should `WrapForeignErrorf` track both the sentinel and a "root cause" error (via `errors` multi-error)? Currently wrapping with `%v` loses valuable information from the original error. Consider either multi-error or a separate root-cause field on `ForeignError`.

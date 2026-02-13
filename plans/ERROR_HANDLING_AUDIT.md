# Error Creation Audit — Non-Wrap Patterns

**Status:** Audit reference document (updated 2026-02-13)

## Summary

Catalogs error creation sites not using the two-layer convention (sentinel + `WrapForeignErrorf`).

**Resolved:** `NewForeignErrorf` violations (~75 sites across 13 files) eliminated by panic-to-error refactoring (PR #212, #215). `fmt.Errorf` in production reduced from 6 to 2 (both are panic-recovery wrappers in `scheme_equals.go` and `syntax_equals.go` — acceptable).

**Remaining:** 24 bare `NewForeignError` (no sentinel) across 7 files in `registry/core/`.

## Open Design Question

Should `WrapForeignErrorf` track both the sentinel and a "root cause" error (via `errors` multi-error)? Currently wrapping with `%v` loses valuable information from the original error. Consider either multi-error or a separate root-cause field on `ForeignError`.

## Remaining Violations

### Bare `NewForeignError` Without Sentinels (24 sites)

| File | Instances |
|------|-----------|
| `registry/core/prim_lists.go` | 8 |
| `registry/core/prim_syntax.go` | 5 |
| `registry/core/prim_byte_vectors.go` | 4 |
| `registry/core/prim_pairs.go` | 3 |
| `registry/core/prim_vectors.go` | 2 |
| `registry/core/prim_parameters.go` | 1 |
| `registry/core/prim_strings.go` | 1 |

These use bare `NewForeignError()` without a sentinel, violating the project convention. Creates opaque errors that callers can't match with `errors.Is()`.

**Fix:** Add appropriate sentinels (likely `ErrOutOfRange` or `ErrInvalidArgument`) and wrap with `WrapForeignErrorf`.

### `fmt.Errorf` in Production (2 sites — acceptable)

| File | Notes |
|------|-------|
| `values/scheme_equals.go:40` | Panic recovery wrapper |
| `internal/syntax/syntax_equals.go:40` | Panic recovery wrapper |

These wrap recovered panics in `defer`/`recover` blocks. Using `fmt.Errorf` here is acceptable since the panic value is unknown and may not be a `ForeignError`.

### ACCEPTABLE (No Change Needed)

- `ErrStopIteration` returns — iteration control, not errors
- `internal/syntax` panics — programming errors
- `values/utils.go` panics — fatal initialization errors

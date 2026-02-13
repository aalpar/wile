# Error Creation Audit — Non-Wrap Patterns

**Status:** Audit reference document

## Summary

Catalogs error creation sites not using the two-layer convention (sentinel + `WrapForeignErrorf`). Total: ~81 violations.

## Open Design Question

Should `WrapForeignErrorf` track both the sentinel and a "root cause" error (via `errors` multi-error)? Currently wrapping with `%v` loses valuable information from the original error. Consider either multi-error or a separate root-cause field on `ForeignError`.

## Violation Counts

| Category | Count | Files |
|----------|-------|-------|
| `fmt.Errorf` in production | 6 | values/utils.go, values/thread.go, internal/syntax/syntax_equals.go, machine/operation_foreign_function_call.go |
| `values.NewForeignErrorf` | ~75 | 13 files (see below) |
| Bare sentinel returns (need wrapping) | 5 | Various |
| Bare sentinel returns (acceptable — control flow) | 5 | `ErrStopIteration` usage |

## By Priority

### HIGH (User-Facing Errors)

| File | Instances | Error Category |
|------|-----------|----------------|
| `machine/compile_syntax_rules.go` | 17 | syntax-rules validation |
| `machine/expander_time_continuation.go` | 16 | let-syntax/letrec-syntax validation |
| `machine/compile_time_continuation.go` | 15 | core compiler errors |
| `machine/library_loader.go` | 9 | library loading |

### MEDIUM (Internal Errors)

| File | Instances | Error Category |
|------|-----------|----------------|
| `machine/import_set_datum.go` | 4 | import parsing |
| `machine/compile_validated.go` | 2 | internal compiler |
| `registry/core/prim_hashtables.go` | 1 | runtime primitive |

### LOW (Edge Cases)

| File | Instances | Notes |
|------|-----------|-------|
| `machine/operation_make_closure.go` | 2 | Bare sentinels, rare VM errors |
| `registry/helpers/numeric.go` | 1 | Bare sentinel |
| `values/thread.go` | 1 | Panic path |
| `machine/operation_foreign_function_call.go` | 1 | Panic recovery |

### ACCEPTABLE (No Change Needed)

- `ErrStopIteration` returns — iteration control, not errors
- `internal/syntax` panics — programming errors
- `values/utils.go` panics — fatal initialization errors

# Missing Static Error Sentinels

Identified during NewForeignError → WrapForeignErrorf conversion (2026-02-13).

All items resolved in commit `a469ce9`.

## ~~Needs New Sentinel~~ (Done)

| Error Message | Location | Sentinel | Status |
|---|---|---|---|
| "exception handler returned from non-continuable exception" | `internal/extensions/exceptions/prim_exceptions.go:165` | `ErrNonContinuableException` | Done |

## ~~Misuse: Should Be NewStaticError, Not ForeignError~~ (Done)

| Current Code | Location | Status |
|---|---|---|
| `var errNeedsBigInt = values.NewStaticError("needs big int")` | `registry/helpers/integer.go:79` | Already correct |

## Not Converted (Not Runtime Errors)

These use `errors.New` / `fmt.Errorf` but are **not** Scheme runtime error paths:

- `values/scheme_equals.go` — quicktest `qt.Checker` implementation, returns `error` to signal check failure
- `internal/syntax/syntax_equals.go` — same pattern
- All `_test.go` files — test fixtures

Suppressed with `//nolint:gocritic` annotations in commit `a19ea37`.

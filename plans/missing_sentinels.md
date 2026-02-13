# Missing Static Error Sentinels

Identified during NewForeignError → WrapForeignErrorf conversion (2026-02-13).

## Needs New Sentinel

| Error Message | Location | Proposed Sentinel |
|---|---|---|
| "exception handler returned from non-continuable exception" | `internal/extensions/exceptions/prim_exceptions.go:165` | `ErrNonContinuableException` |

## Misuse: Should Be NewStaticError, Not ForeignError

| Current Code | Location | Reason |
|---|---|---|
| `var errNeedsBigInt = values.NewForeignError("needs big int")` | `registry/helpers/integer.go:79` | Internal control-flow sentinel, not a user-facing error. Stack trace capture is wasteful. Should be `values.NewStaticError("needs big int")`. |

## Not Converted (Not Runtime Errors)

These use `errors.New` / `fmt.Errorf` but are **not** Scheme runtime error paths:

- `values/scheme_equals.go` — quicktest `qt.Checker` implementation, returns `error` to signal check failure
- `internal/syntax/syntax_equals.go` — same pattern
- All `_test.go` files — test fixtures

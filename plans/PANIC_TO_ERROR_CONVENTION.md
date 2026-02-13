# Panic-to-Error Convention Conversion Plan

**Status:** Complete

## Summary

Convert 14 panic sites to use the two-layer error convention (sentinel + `WrapForeignErrorf`). Some panics should remain as panics (API misuse), others should become errors (runtime failures).

## Classification

### Keep as Panic (API Misuse)

| Site | File | Why |
|------|------|-----|
| Nil parent (1) | `environment_frame.go:138` | Impossible if API used correctly |
| Missing PhaseRegistry (1) | `environment_frame.go:174` | Impossible if constructed correctly |
| Missing TopLevelEnvironment (5) | `global_environment_frame.go:294,306,318,329`, `environment_frame.go:703` | Impossible if constructed correctly |
| Double-wrapping SyntaxObjects (2) | `internal/syntax/syntax_value.go:105,107` | Documented precondition violation |

**Change**: Replace bare string/`NewForeignErrorf` panics with `WrapForeignErrorf(sentinel, ...)` panics for consistency and `errors.Is` matching in tests.

### Convert to Errors (Runtime Failures)

| Site | File | Why |
|------|------|-----|
| FFI callback error (1) | `ffi.go:607` | Go caller should handle, not crash |
| FFI result conversion (1) | `ffi.go:635` | Type mismatch at runtime |
| FFI hashtable insertion (1) | `ffi.go:818` | Runtime failure |
| RNG errors (2) | `values/utils.go:267,270` | Environmental (entropy pool). **Deferred** — keep as panic with sentinel (API change too broad) |

## New Sentinels (`values/foreign_error.go`)

`ErrFFICallbackError`, `ErrCallbackResultConversion`, `ErrHashtableInsertionFailed`, `ErrMissingTopLevelEnvironment`, `ErrMissingPhaseRegistry`, `ErrNilParentEnvironment`, `ErrRandomGenerationFailed`, `ErrCannotDoubleSyntaxWrap`

## Implementation Order

| Phase | Description | Risk |
|-------|-------------|------|
| 1 | FFI error conversion (3 sites in `ffi.go`) — user-facing, HIGH priority | ✅ Complete |
| 2 | Utility errors (`values/utils.go`) — keep as panic with sentinel | ✅ Complete (already done) |
| 3 | Environment/syntax panic sentinels (9 sites) — no behavior change | ✅ Complete (8 already done, 1 converted) |

## Files Modified

| File | Changes |
|------|---------|
| `values/foreign_error.go` | Add 8 sentinels |
| `ffi.go` | Convert 3 panics to errors |
| `environment/environment_frame.go` | Use sentinels in 2 panics |
| `environment/global_environment_frame.go` | Use sentinels in 4 panics |
| `internal/syntax/syntax_value.go` | Use sentinel in 2 panics |
| `values/utils.go` | Use sentinel in 2 panics (no signature change) |

**Total**: ~154 lines changed + ~80 lines tests

## Open Decisions

1. **`NewTemporaryVariableName` return error?** — Deferred. Keep as panic (RNG failures are catastrophic, API change too broad).
2. **Environment construction errors return errors?** — No. Impossible states if API used correctly; panic is appropriate.
3. **Syntax double-wrap: panic or error?** — Keep as panic per documented contract.

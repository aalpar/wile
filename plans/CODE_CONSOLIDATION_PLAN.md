# Code Consolidation Plan: Reducing Code Volume Through Parameterization

**Status:** COMPLETE — All 6 phases implemented

## Executive Summary

This plan identifies opportunities to reduce code volume in the Wile Scheme interpreter through parameterization techniques. Analysis identified **~3,700 bytes** of consolidation opportunities across primitives, machine, and extension packages.

**Approach**: Extend existing successful patterns (`NumericChainCompare()`, `MakeTypePredicate()`, `RequireArg[T]()`) rather than risky architectural changes.

**Not recommended**: Values package arithmetic methods (high risk, R7RS exactness semantics require type-specific behavior).

---

## Analysis Summary

| Area | Potential Savings | Risk | Status |
|------|------------------|------|--------|
| Primitive type extraction | ~2,000 bytes | LOW | **✅ COMPLETE** — `RequireArg[T]` + `RequireIndex` |
| Optional argument parsing | ~650 bytes | LOW | **✅ COMPLETE** — `ParseOptionalStartEnd` in `helpers/args.go` |
| Compile-time execution pattern | ~400 bytes | MEDIUM | **✅ COMPLETE** — `expandCompileExecute` in `compile_helpers.go` |
| Operation EqualTo helpers | ~300 bytes | LOW | **✅ COMPLETE** — all 34 ops migrated |
| Duplicate math helpers | ~250 bytes | LOW | **✅ COMPLETE** |
| Index bounds checking | ~100 bytes | LOW | **✅ COMPLETE** — `CheckIndexBounds` + `RequireIndex` |
| Values arithmetic methods | ~1,600 bytes | HIGH | **Do not implement** |
| IsVoid boilerplate | ~50 bytes | LOW | **Do not implement** (idiomatic Go) |

---

## Phase 1: Remove Duplicate Math Helpers ✅ COMPLETE

**Savings**: ~250 bytes
**Risk**: LOW

### Problem

Functions in `internal/extensions/math/prim_math.go` are duplicates of `registry/helpers/value_conv.go` and `registry/helpers/integer.go`:

| Function | extensions/math location | helpers location |
|----------|------------------------|------------------|
| `ToComplex128` | prim_math.go | value_conv.go |
| `ComplexOrFloat` | prim_math.go | value_conv.go |
| `ToFloat64` | prim_math.go | value_conv.go |
| `FloorDivide` | prim_math.go | integer.go |

### Solution

Delete duplicate functions from `internal/extensions/math/prim_math.go` and import from `registry/helpers`.

### Files to Modify

| File | Action |
|------|--------|
| `internal/extensions/math/prim_math.go` | Delete duplicate functions, add import `"github.com/aalpar/wile/registry/helpers"` |

### Verification

```bash
make test
```

---

## Phase 2: Type Extraction Migration — `RequireArg[T]` Adoption ✅ COMPLETE

**Savings**: ~18 lines of boilerplate across 13 convertible sites
**Risk**: LOW

### Current State

`RequireArg[T]` already exists in `registry/helpers/args.go`:

```go
func RequireArg[T any](mc *machine.MachineContext, index int, sentinel error, name string) (T, error)
func RequireType[T any](v values.Value, sentinel error, name string) (T, error)
```

**Adoption**: 54 call sites already use `RequireArg[T]` / `RequireType[T]`.

### Triage of Remaining Assertions

The original estimate of "~200 remaining sites" overcounted by conflating three structurally
different patterns that happen to use Go type assertions:

| Category | Count | `RequireArg[T]` fit? | Reason |
|----------|------:|:-------------------:|--------|
| **Direct extractions** — `mc.Arg(n).(T)` + error on failure | ~13 | **YES** | Exact same partial function `Value ⇀ T` |
| **Predicate assertions** — `_, ok := v.(T)` for boolean result | ~14 | NO | No error; false is a valid result |
| **Loop-interior assertions** — `pr, ok = next.(*Pair)` in traversal | ~12 | NO | Not from `mc.Arg(n)`; reassignment in loop |
| **Optional-arg extraction** — `rest.(Tuple)` then `tuple.Car().(T)` | ~8 | NO | Nested inside `if !IsEmptyList` blocks |
| **`values.ExactInteger` calls** — returns `(int64, bool)` | ~6 | NO | Different return shape; semantic query not type assertion |

**Why the non-convertible sites should stay as-is:**

- **Predicate assertions** implement total functions `Value → Bool`. `RequireArg[T]` implements
  partial functions `Value ⇀ T`. Conflating them would require a second helper (`TryArg[T]`
  returning `(T, bool)`) for no meaningful gain — the existing code is already 2 lines.
- **Loop-interior assertions** operate on intermediate values during list traversal, not on
  `mc.Arg(n)`. `RequireArg` requires a `MachineContext` + arg index, which doesn't exist here.
- **Optional-arg extraction** is always nested inside `if !values.IsEmptyList(rest)` guards.
  The Tuple assertion is part of the optional-argument protocol, not standalone extraction.
- **`values.ExactInteger`** returns `(int64, bool)` — a projection that extracts the int64
  from any Number type that happens to be an exact integer. Different algebra from `RequireArg[T]`.

### Convertible Sites (all converted)

| File | Function | Arg | Type | Sentinel | Status |
|------|----------|-----|------|----------|--------|
| `prim_characters.go` | `PrimCharToInteger` | 0 | `*values.Character` | `ErrNotACharacter` | ✅ |
| `prim_characters.go` | `PrimIntegerToChar` | 0 | `*values.Integer` | `ErrNotANumber` | ✅ |
| `prim_pairs.go` | `PrimCar` | 0 | `values.Tuple` | `ErrNotAPair` | ✅ |
| `prim_pairs.go` | `PrimCdr` | 0 | `values.Tuple` | `ErrNotAPair` | ✅ |
| `prim_pairs.go` | `PrimSetCar` | 0 | `*values.Pair` | `ErrNotAPair` | ✅ |
| `prim_pairs.go` | `PrimSetCdr` | 0 | `*values.Pair` | `ErrNotAPair` | ✅ |
| `prim_predicates.go` | `PrimExactQ` | 0 | `values.Number` | `ErrNotANumber` | ✅ |
| `prim_predicates.go` | `PrimInexactQ` | 0 | `values.Number` | `ErrNotANumber` | ✅ |
| `prim_predicates.go` | `PrimZeroQ` | 0 | `values.Number` | `ErrNotANumber` | ✅ |
| `prim_predicates.go` | `PrimPositiveQ` | 0 | `values.RealNumber` | `ErrNotANumber` | ✅ |
| `prim_predicates.go` | `PrimNegativeQ` | 0 | `values.RealNumber` | `ErrNotANumber` | ✅ |
| `prim_lists.go` | `PrimMakeList` | 0 | `*values.Integer` | `ErrNotAnInteger` | ✅ |
| `prim_lists.go` | `PrimListSet` | 0 | `*values.Pair` | `ErrNotAList` | ✅ |

### Also Fixed: Error Wrapping Inconsistency

`PrimMakeList` previously used `values.NewForeignError(...)` (no sentinel) for integer
type mismatch. Now uses `RequireArg[*values.Integer]` which wraps `ErrNotAnInteger`,
making `errors.Is(err, ErrNotAnInteger)` work consistently.

### Verification

```bash
make test
make lint
```

---

## Phase 3: Index Bounds Checking Helper ✅ COMPLETE

**Savings**: ~100 bytes
**Risk**: LOW

### Status

Implemented as two complementary helpers in `registry/helpers/args.go`:

1. **`CheckIndexBounds(idx int64, length int, name string) error`** — low-level bounds check
2. **`RequireIndex(mc, argIdx, length, name) (int, error)`** — extracts exact integer index + bounds check in one call

`RequireIndex` uses `values.ExactInteger` (accepts `*Integer`, `*BigInteger`, integer-valued `*Rational`)
which is more R7RS-correct than the previous `RequireArg[*values.Integer]` (only accepted `*Integer`).

### Files Modified

| File | Change |
|------|--------|
| `registry/helpers/args.go` | Added `RequireIndex` (~15 lines) |
| `registry/core/prim_vectors.go` | `PrimVectorRef`, `PrimVectorSet` use `RequireIndex` |
| `registry/core/prim_byte_vectors.go` | `PrimBytevectorU8Ref`, `PrimBytevectorU8Set` use `RequireIndex` |
| `registry/core/prim_strings.go` | `PrimStringRef`, `PrimStringSet` use `RequireIndex` |

---

## Phase 4: Optional Start/End Argument Parser ✅ COMPLETE

**Status**: Implemented in `registry/helpers/args.go` as `ParseOptionalStartEnd`.

Used at 12 call sites:
- `prim_strings.go`: string->list, string-copy
- `prim_byte_vectors.go`: bytevector-copy, bytevector-copy!, utf8->string, string->utf8
- `prim_vectors.go`: vector->list, vector-copy, vector-copy!, vector-fill!, vector->string, string->vector

No further work needed.

---

## Phase 5: Compile-Time Execution Helper ✅ COMPLETE

**Savings**: ~400 bytes
**Risk**: MEDIUM

### Status

Implemented as `expandCompileExecute` on `*CompileTimeContinuation` in `machine/compile_helpers.go`.
All 3 call sites (`begin-for-syntax`, `define-for-syntax`, `eval-when`) use the helper.

---

## Phase 6: Operation EqualTo Migration ✅ COMPLETE

**Savings**: ~300 bytes
**Risk**: LOW

### Status

All 34 operations across `machine/operation_*.go` files are fully migrated:
- 20 zero-field operations → `sameType`
- 11 single-field comparable → `fieldMatches`
- 2 single-field non-comparable → `fieldMethodMatches`
- 1 slice field → `sliceMatches`

No hand-written `EqualTo` implementations remain.

### Solution

Migrate operation files to use existing helpers.

**Zero-field operations** (use `sameType`):
```go
// Before:
func (p *OperationPop) EqualTo(o values.Value) bool {
    _, ok := o.(*OperationPop)
    return ok
}

// After:
func (p *OperationPop) EqualTo(o values.Value) bool {
    return sameType[*OperationPop](o)
}
```

**Single-field operations** (use `fieldMatches`):
```go
// Before:
func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
    v, ok := o.(*OperationBranchOffsetImmediate)
    if !ok {
        return false
    }
    return p.Offset == v.Offset
}

// After:
func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
    return fieldMatches(p, o, func(op *OperationBranchOffsetImmediate) int { return op.Offset })
}
```

### Files to Modify

All 30 `machine/operation_*.go` files with hand-written `EqualTo`.

### Verification

```bash
go test -v ./machine/...
```

---

## What NOT to Consolidate

### Values Package Arithmetic Methods

**Analysis**: 28 methods (Add/Subtract/Multiply/Divide across 7 numeric types) have similar switch/case structure totaling ~1,600 lines.

**Why NOT to consolidate**:
1. **R7RS exactness semantics**: Each type has specific exactness contagion rules
2. **Performance**: Arithmetic is a hot path; generic dispatch adds overhead
3. **Type promotion rules**: Integer→Float→Complex promotion varies by operation
4. **Precision handling**: BigInteger/BigFloat have special precision requirements
5. **Working code**: The existing implementation is correct and well-tested

**Recommendation**: Document the pattern in CODING_STYLE.md for consistency, but do not attempt abstraction.

### IsVoid Boilerplate

**Pattern**: `func (p *Type) IsVoid() bool { return p == nil }` appears 7+ times.

**Why NOT to consolidate**:
1. Go interfaces require explicit method definitions
2. Each implementation is one line
3. No mechanism to share implementation without embedding

**Recommendation**: This is idiomatic Go; leave as-is.

### Tokenizer Refactoring

Already documented as a separate initiative in `plans/TOKENIZER_CONSOLIDATION_PLAN.md`.

**Recommendation**: Implement as a separate project per existing plan.

---

## Implementation Summary

| Phase | Description | Savings | Risk | Status |
|-------|-------------|---------|------|--------|
| 1 | Remove duplicate math helpers | 250 bytes | LOW | **✅ COMPLETE** |
| 2 | `RequireArg[T]` migration | ~18 lines | LOW | **✅ COMPLETE** (67/67 convertible; see triage for non-convertible) |
| 3 | Index bounds checking | 100 bytes | LOW | **✅ COMPLETE** — `CheckIndexBounds` + `RequireIndex` |
| 4 | Optional start/end parser | 650 bytes | LOW | **✅ COMPLETE** |
| 5 | Compile-time execution helper | 400 bytes | MEDIUM | **✅ COMPLETE** — `expandCompileExecute` |
| 6 | Operation EqualTo migration | 300 bytes | LOW | **✅ COMPLETE** |

---

## Verification Strategy

After each phase:

1. **Unit tests**: `make test`
2. **Linting**: `make lint`
3. **Specific tests for Phase 5**: `go test -v -run "TestMacro|TestSyntax" ./machine/...`

---

## References

- Existing helpers: `registry/helpers/` — `args.go`, `numeric.go`, `type.go`, `value_conv.go`, `integer.go`, `equality.go`, `list.go`, `char.go`, `string.go`
- Operation helpers: `machine/operation_helpers.go` — `sameType[T]`, `fieldMatches[T, Op]`
- Tokenizer refactoring: `plans/TOKENIZER_CONSOLIDATION_PLAN.md` (separate initiative)

# Code Consolidation Plan: Reducing Code Volume Through Parameterization

**Status:** IN PROGRESS — Phases 2 and 4 partially complete via `RequireArg[T]` and `ParseOptionalStartEnd`

## Executive Summary

This plan identifies opportunities to reduce code volume in the Wile Scheme interpreter through parameterization techniques. Analysis identified **~3,700 bytes** of consolidation opportunities across primitives, machine, and extension packages.

**Approach**: Extend existing successful patterns (`NumericChainCompare()`, `MakeTypePredicate()`, `RequireArg[T]()`) rather than risky architectural changes.

**Not recommended**: Values package arithmetic methods (high risk, R7RS exactness semantics require type-specific behavior).

---

## Analysis Summary

| Area | Potential Savings | Risk | Status |
|------|------------------|------|--------|
| Primitive type extraction | ~2,000 bytes | LOW | **IN PROGRESS** — `RequireArg[T]` exists, 54/260 sites migrated |
| Optional argument parsing | ~650 bytes | LOW | **✅ COMPLETE** — `ParseOptionalStartEnd` in `helpers/args.go` |
| Compile-time execution pattern | ~400 bytes | MEDIUM | Not started |
| Operation EqualTo helpers | ~300 bytes | LOW | **IN PROGRESS** — helpers exist, 30 ops need migration |
| Duplicate math helpers | ~250 bytes | LOW | Not started |
| Index bounds checking | ~100 bytes | LOW | Not started |
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

## Phase 2: Type Extraction Migration — `RequireArg[T]` Adoption ⚠️ IN PROGRESS

**Savings**: ~800 lines (~2,400 bytes remaining)
**Risk**: LOW

### Current State

`RequireArg[T]` already exists in `registry/helpers/args.go`:

```go
func RequireArg[T any](mc *machine.MachineContext, index int, sentinel error, name string) (T, error)
```

**Adoption**: 54 call sites use `RequireArg[T]` across 9 files. ~200 type-assertion-then-error patterns remain unconverted in production code.

### Remaining Work

Migrate remaining type-assertion patterns. The original plan proposed per-type `ExtractString`, `ExtractVector`, etc. — this is now unnecessary since `RequireArg[T]` is a single generic function that covers all types.

### Pattern

**Before** (4 lines per site):
```go
o := mc.Arg(0)
s, ok := o.(*values.String)
if !ok {
    return values.WrapForeignErrorf(values.ErrNotAString, "string-length: expected a string but got %T", o)
}
```

**After** (3 lines per site):
```go
s, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "string-length")
if err != nil {
    return err
}
```

### Files with Remaining Assertions (Production Code)

| File | Pointer assertions (`*values.X`) | Interface assertions (`values.X`) | Total |
|------|----------------------------------|-----------------------------------|-------|
| `registry/core/prim_lists.go` | 8 | 12 | 20 |
| `registry/core/prim_predicates.go` | 8 | 10 | 18 |
| `registry/core/prim_strings.go` | 4 | 4 | 8 |
| `registry/core/prim_arithmetic.go` | 8 | 0 | 8 |
| `registry/core/prim_vectors.go` | 1 | 5 | 6 |
| `registry/core/prim_pairs.go` | 2 | 3 | 5 |
| `registry/core/prim_byte_vectors.go` | 3 | 3 | 6 |
| `registry/core/prim_characters.go` | 2 | 0 | 2 |
| `registry/core/prim_equality.go` | 1 | 2 | 3 |
| `registry/core/prim_prompt.go` | 1 | 2 | 3 |
| `registry/core/prim_control.go` | 0 | 3 | 3 |
| `registry/core/prim_hashtables.go` | 1 | 1 | 2 |
| `registry/core/prim_parameters.go` | 1 | 0 | 1 |
| `registry/core/prim_boxes.go` | 1 | 0 | 1 |

**Note**: Some assertions in `prim_predicates.go` and `prim_control.go` are interface assertions (e.g., `values.Number`, `values.RealNumber`) that may not fit the `RequireArg[T]` pattern cleanly. Evaluate case by case.

### Verification

```bash
make test
make lint
```

---

## Phase 3: Index Bounds Checking Helper

**Savings**: ~100 bytes
**Risk**: LOW

### Problem

6 identical bounds-check patterns in ref/set operations:

```go
if idx.Value < 0 || idx.Value >= int64(len(*v)) {
    return values.NewForeignError("string-ref: index out of bounds")
}
```

### Solution

Add to `registry/helpers/args.go`:

```go
// CheckIndexBounds validates that idx is in range [0, length).
func CheckIndexBounds(idx int64, length int, name string) error {
    if idx < 0 || idx >= int64(length) {
        return values.WrapForeignErrorf(values.ErrIndexOutOfRange, "%s: index %d out of bounds for length %d", name, idx, length)
    }
    return nil
}
```

### Files to Modify

| File | Pattern |
|------|---------|
| `registry/core/prim_strings.go` | string-ref, string-set! |
| `registry/core/prim_vectors.go` | vector-ref, vector-set! |
| `registry/core/prim_byte_vectors.go` | bytevector-u8-ref, bytevector-u8-set! |

### Verification

```bash
go test -v -run "TestPrimString|TestPrimVector|TestPrimByte" ./registry/core/...
```

---

## Phase 4: Optional Start/End Argument Parser ✅ COMPLETE

**Status**: Implemented in `registry/helpers/args.go` as `ParseOptionalStartEnd`.

Used at 12 call sites:
- `prim_strings.go`: string->list, string-copy
- `prim_byte_vectors.go`: bytevector-copy, bytevector-copy!, utf8->string, string->utf8
- `prim_vectors.go`: vector->list, vector-copy, vector-copy!, vector-fill!, vector->string, string->vector

No further work needed.

---

## Phase 5: Compile-Time Execution Helper

**Savings**: ~400 bytes
**Risk**: MEDIUM

### Problem

3 files share identical 30-line expand-compile-execute pattern:

| File | Form |
|------|------|
| `machine/compile_begin_for_syntax.go` | begin-for-syntax |
| `machine/compile_define_for_syntax.go` | define-for-syntax |
| `machine/compile_eval_when.go` | eval-when |

Common pattern:
```go
expandEnv := p.env.Expand()
ectx := NewExpandTimeCallContext()
expander := NewExpanderTimeContinuation(p.env)

expandedExpr, err := expander.ExpandExpression(ectx, stxVal)
if err != nil {
    return values.WrapForeignErrorf(err, "[form]: expansion failed")
}

tmpTpl := NewNativeTemplate(0, 0, false)
tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)
err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
if err != nil {
    return values.WrapForeignErrorf(err, "[form]: compilation failed")
}

cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
mc := NewMachineContext(context.Background(), cont)
err = mc.Run()
if err != nil {
    return values.WrapForeignErrorf(err, "[form]: evaluation failed")
}
```

### Solution

Extract `ExecuteAtCompileTime` helper on `*CompileTimeContinuation`. Create in `machine/compile_helpers.go` or add to existing helpers file.

### Files to Modify

| File | Change |
|------|--------|
| `machine/compile_begin_for_syntax.go` | Replace expand-compile-execute block with helper call |
| `machine/compile_define_for_syntax.go` | Replace expand-compile-execute block with helper call |
| `machine/compile_eval_when.go` | Replace expand-compile-execute block with helper call |

### Verification

```bash
go test -v -run "TestMacro|TestSyntax|TestBeginForSyntax|TestDefineForSyntax|TestEvalWhen" ./machine/...
```

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
| 2 | `RequireArg[T]` migration | 2,400 bytes | LOW | **54/260 sites done** |
| 3 | Index bounds checking | 100 bytes | LOW | Not started |
| 4 | Optional start/end parser | 650 bytes | LOW | **✅ COMPLETE** |
| 5 | Compile-time execution helper | 400 bytes | MEDIUM | Not started |
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

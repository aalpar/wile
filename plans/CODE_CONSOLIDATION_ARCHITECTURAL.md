# Higher-Risk Architectural Consolidation Plan

**Status:** PARTIALLY COMPLETE — Indexable interface and numeric predicates done; generic helpers and operation codegen remain

## Executive Summary

This document explores higher-risk architectural changes that could save significantly more code than the low-risk helpers approach. These changes involve introducing new abstractions, interfaces, or code generation that would require more extensive testing and carry higher implementation risk.

**Estimated remaining savings: ~4,200-7,200 bytes**
**Risk level: MEDIUM to HIGH**
**Recommended approach: Implement incrementally with comprehensive testing**

---

## Architectural Change 1: Indexable Container Generic Helpers ⚠️ PARTIALLY COMPLETE

**Risk: MEDIUM**
**Estimated remaining savings: ~800 bytes**
**Files affected: 3 primitive files, 1 new helper file**

### Current State

The `Indexable` interface **already exists** in `values/values.go`:

```go
type Indexable interface {
    Value
    Length() int
    Get(int) Value
    Set(int, Value)
}
```

**Implementors**: `Vector` and `ByteVector` implement `Indexable` (verified via compile-time assertions in their respective files).

**String does NOT implement `Indexable`**: String has `Get()` and `Length()` but its mutation method is `SetChar(int, rune)`, not `Set(int, Value)`. This is a deliberate type safety decision — string mutation requires a `Character`, not an arbitrary `Value`.

### Remaining Problem

The interface exists but no generic helpers consume it. Three sets of `*-length`, `*-ref`, `*-set!` primitives are still separate hand-written functions with identical structure:

| Operation | Vector (`prim_vectors.go`) | ByteVector (`prim_byte_vectors.go`) |
|-----------|---------------------------|-------------------------------------|
| length | `PrimVectorLength` (~7 lines) | `PrimBytevectorLength` (~7 lines) |
| ref | `PrimVectorRef` (~14 lines) | `PrimBytevectorU8Ref` (~12 lines) |
| set! | `PrimVectorSet` (~15 lines) | `PrimBytevectorU8Set` (~16 lines) |

String primitives remain separate due to different mutation semantics.

### Proposed Solution

Create `registry/helpers/indexable.go` with generic helpers that operate on the existing `Indexable` interface:

```go
// IndexableLength implements the length operation for Vector and ByteVector.
func IndexableLength(mc *machine.MachineContext, sentinel error, name string) error {
    container, err := RequireArg[values.Indexable](mc, 0, sentinel, name)
    if err != nil {
        return err
    }
    mc.SetValue(values.NewInteger(int64(container.Length())))
    return nil
}
```

This consolidates Vector + ByteVector operations (2 types × 3 ops = 6 functions → 3 generic functions + 3 string-specific functions).

### Files to Create/Modify

| File | Action |
|------|--------|
| `registry/helpers/indexable.go` | Create generic helpers (~50 lines) |
| `registry/core/prim_vectors.go` | Simplify length/ref/set (~36 lines removed) |
| `registry/core/prim_byte_vectors.go` | Simplify length/ref/set (~35 lines removed) |

**Note**: `prim_strings.go` retains its own implementations due to String's different mutation semantics.

**Net change**: +50 lines infrastructure, -71 lines primitives = ~21 lines saved, plus guaranteed behavioral consistency between Vector and ByteVector operations.

### Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Performance overhead from interface dispatch | Go interface dispatch is fast; profile if concerned |
| ByteVector byte-range validation on set | Validation stays in `ByteVector.Set()` method |

---

## Architectural Change 2: Operation Code Generation

**Risk: HIGH**
**Estimated savings: ~2,000-3,000 bytes**
**Files affected: 20+ operation files**
**Status: Not started**

### Problem

36 operation files follow repetitive patterns. Example categories:

**Zero-field operations** (6 files, ~270 lines total):
- `operation_pop.go`, `operation_pop_all.go`, `operation_push.go`
- `operation_brk.go`, `operation_pull.go`, `operation_drop.go`

Each has identical boilerplate:
```go
type OperationXxx struct{}
func NewOperationXxx() *OperationXxx { return &OperationXxx{} }
func (p *OperationXxx) SchemeString() string { return "#<machine-operation-xxx>" }
func (p *OperationXxx) IsVoid() bool { return p == nil }
func (p *OperationXxx) EqualTo(o values.Value) bool { /* boilerplate */ }
func (p *OperationXxx) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) { /* unique */ }
```

**Single-field operations** (8+ files, ~400 lines total):
- `operation_branch_offset_immediate.go`
- `operation_branch_on_false_offset_immediate.go`
- `operation_load_literal_integer.go`
- etc.

### Proposed Solution: Code Generation

Create `machine/gen_operations_main.go` with `//go:generate` directive. Generator produces `operation_generated.go` containing all simple operations. Complex operations (`Apply`, `MakeClosure`, `SyntaxCase`) remain hand-written.

### Alternative: Embedding with Generics (Lower Risk)

Instead of code generation, use embedding:

```go
type zeroFieldOperation[T any] struct{}

func (p *zeroFieldOperation[T]) IsVoid() bool { return p == nil }
func (p *zeroFieldOperation[T]) EqualTo(o values.Value) bool {
    return sameType[*T](o)
}
```

This saves less code (~100 lines) but carries lower risk.

### Impact Analysis

| Category | Files | Lines Removed | Generated Lines |
|----------|-------|---------------|-----------------|
| Zero-field | 6 | 273 | 180 (in one file) |
| Single-field (simple) | 5 | 255 | 150 (in one file) |
| Branch variants | 3 | 161 | 90 (parameterized) |
| **Total** | **14** | **689** | **420** |

**Net savings**: ~270 lines (~2,700 bytes)

### Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Generated code harder to debug | Keep Apply bodies readable; good comments |
| Build dependency on generator | Simple Go code, easy to maintain |
| Complex operations don't fit template | Keep complex ops as manual files |

---

## Architectural Change 3: Numeric Predicate Consolidation ✅ COMPLETE

**Status**: Already implemented. The `RealNumber` interface has `IsPositive()`, `IsNegative()`, `Sign()` methods. Numeric predicates in `prim_predicates.go` use these methods.

**Implemented on**: `Integer`, `Float`, `Rational`, `BigInteger`, `BigFloat` (all via the `RealNumber` interface).

**Predicates consolidated**:
- `positive?` → uses `r.IsPositive()`
- `negative?` → uses `r.IsNegative()`
- `odd?` / `even?` → use `parityCheck()` helper with `Sign()` / bit tests

No further work needed.

---

## Architectural Change 4: Declarative Primitive Registration

**Risk: HIGH**
**Estimated savings: ~1,500-2,000 bytes**
**Files affected: All prim_*.go files, registry system**
**Status: Not started — deferred**

### Problem

Primitive implementations have repetitive boilerplate:
1. Type extraction from arguments
2. Error formatting with primitive name
3. Result setting

### Assessment

With `RequireArg[T]` now available, much of the boilerplate that motivated this change has been addressed. The remaining boilerplate is:
- The `func(_ context.Context, mc *machine.MachineContext) error` signature
- The `mc.SetValue(...)` call
- The `return nil` at the end

This is minimal — 3 lines per primitive. Declarative registration would save these 3 lines at the cost of reflection overhead, lost IDE navigation, and harder debugging.

### Recommendation

**Do not implement.** The cost-benefit ratio has shifted unfavorably since `RequireArg[T]` eliminated the primary boilerplate source. Revisit only if the primitive count grows significantly.

---

## Implementation Roadmap

### Phase A: Indexable Generic Helpers (MEDIUM risk)

1. Create `registry/helpers/indexable.go` with generic helpers consuming existing `Indexable` interface
2. Migrate Vector length/ref/set to use generic helpers
3. Migrate ByteVector length/ref/set to use generic helpers
4. String primitives remain as-is (different mutation semantics)
5. Comprehensive testing after each type

### Phase B: Operation EqualTo Migration (LOW risk)

**See CODE_CONSOLIDATION_PLAN.md Phase 6** — helpers already exist in `operation_helpers.go`. This is a mechanical migration of 30 files.

### Phase C: Operation Code Generation (HIGH risk — optional)

1. Create generator for simple operations
2. Generate zero-field operations
3. Generate single-field operations
4. Validate generated code matches behavior
5. Remove manual files only after validation

### ~~Phase D: Declarative Primitives~~ — DEFERRED INDEFINITELY

`RequireArg[T]` eliminated the primary motivation. Not recommended.

---

## Summary

| Change | Risk | Savings | Status |
|--------|------|---------|--------|
| Indexable generic helpers | MEDIUM | ~800 bytes | **Interface exists, helpers needed** |
| Operation EqualTo migration | LOW | ~300 bytes | **✅ COMPLETE** |
| Numeric predicate methods | MEDIUM | ~800 bytes | **✅ COMPLETE** |
| Operation code generation | HIGH | ~2,700 bytes | Not started (optional) |
| Declarative primitives | HIGH | ~2,000 bytes | **DEFERRED** — `RequireArg[T]` reduced need |

**Remaining recommended savings: ~1,100 bytes** (Indexable helpers + EqualTo migration)
**Optional additional savings: ~2,700 bytes** (operation code generation)

---

## Verification Strategy

For each architectural change:

1. **Before**: Full test suite passes (`make test`)
2. **Implementation**: Create feature branch
3. **Unit tests**: Add tests for new abstractions
4. **Integration**: Verify primitive behavior unchanged
5. **Performance**: Profile hot paths if interface dispatch involved
6. **Review**: Code review focusing on edge cases
7. **Merge**: Only after all tests pass

---

## Files Summary

### New Files to Create

| File | Purpose | Lines |
|------|---------|-------|
| `registry/helpers/indexable.go` | Generic helpers for `Indexable` interface | ~50 |

### Already Existing Infrastructure

| File | What's There |
|------|-------------|
| `values/values.go` | `Indexable` interface with `Length()`, `Get()`, `Set()` |
| `values/vector.go` | `Indexable` implementation |
| `values/byte_vector.go` | `Indexable` implementation |
| `machine/operation_helpers.go` | `sameType[T]`, `fieldMatches[T, Op]` |
| `registry/helpers/args.go` | `RequireArg[T]`, `ParseOptionalStartEnd` |

### Files to Modify

| File | Change |
|------|--------|
| `registry/core/prim_vectors.go` | Use Indexable helpers for length/ref/set |
| `registry/core/prim_byte_vectors.go` | Use Indexable helpers for length/ref/set |
| 30× `machine/operation_*.go` | Use `sameType` / `fieldMatches` for EqualTo |

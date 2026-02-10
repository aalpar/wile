# Higher-Risk Architectural Consolidation Plan

**Status:** MOSTLY COMPLETE — Indexable ref/set consolidated via `RequireIndex`, numeric predicates done, EqualTo done; operation codegen remains optional

## Executive Summary

This document explores higher-risk architectural changes that could save significantly more code than the low-risk helpers approach. These changes involve introducing new abstractions, interfaces, or code generation that would require more extensive testing and carry higher implementation risk.

**All recommended changes complete.** Optional remaining savings: ~2,700 bytes (operation code generation — HIGH risk).
**Recommended approach: No further action unless operation boilerplate becomes a maintenance burden.**

---

## Architectural Change 1: Index Extraction Consolidation ✅ COMPLETE

**Risk: LOW**
**Savings: ~100 bytes across 6 call sites**

### Decision

The original proposal was to create `registry/helpers/indexable.go` with generic helpers consuming the `Indexable` interface. This was **rejected** in favor of `RequireIndex` — a simpler approach that consolidates at the argument-extraction level rather than the container-dispatch level.

**Why `RequireIndex` over `Indexable` helpers:**

1. **Different error sentinels**: Vector uses `ErrNotAVector`, ByteVector uses `ErrNotAByteVector`, String uses `ErrNotAString`. A generic `IndexableRef` would need the sentinel passed in, making it no simpler than the current per-type code.
2. **Different value semantics**: ByteVector ref returns `NewInteger(int64(byte))`, Vector ref returns the element directly, String ref returns `NewCharacter(rune)`. The post-extraction logic differs per type.
3. **String doesn't implement `Indexable`**: String's mutation method is `SetChar(int, rune)`, not `Set(int, Value)`, so it can't participate in generic helpers anyway.
4. **The real duplication was in index extraction**: All 6 ref/set sites repeated the same "extract integer, validate bounds" pattern. `RequireIndex` eliminates that.

### What Was Done

`RequireIndex` in `registry/helpers/args.go` combines:
- Exact integer extraction via `values.ExactInteger` (accepts `*Integer`, `*BigInteger`, integer-valued `*Rational`)
- Bounds checking via `CheckIndexBounds`
- Conversion to Go `int`

Migrated 6 call sites:
- `PrimVectorRef`, `PrimVectorSet` (prim_vectors.go)
- `PrimBytevectorU8Ref`, `PrimBytevectorU8Set` (prim_byte_vectors.go)
- `PrimStringRef`, `PrimStringSet` (prim_strings.go)

R7RS improvement: `RequireIndex` accepts any exact integer, not just `*Integer`. This is more correct per R7RS §6.1 (indices are "exact non-negative integers").

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

### ~~Phase A: Indexable Generic Helpers~~ — SUPERSEDED by `RequireIndex`

See Architectural Change 1 above. The real duplication was in index extraction, not in container dispatch. `RequireIndex` solved it with less abstraction overhead.

### Phase B: Operation EqualTo Migration ✅ COMPLETE

**See CODE_CONSOLIDATION_PLAN.md Phase 6** — all 34 operations migrated to `sameType`/`fieldMatches`/`fieldMethodMatches`/`sliceMatches`.

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
| Index extraction consolidation | LOW | ~100 bytes | **✅ COMPLETE** — `RequireIndex` (superseded Indexable approach) |
| Operation EqualTo migration | LOW | ~300 bytes | **✅ COMPLETE** |
| Numeric predicate methods | MEDIUM | ~800 bytes | **✅ COMPLETE** |
| Operation code generation | HIGH | ~2,700 bytes | Not started (optional) |
| Declarative primitives | HIGH | ~2,000 bytes | **DEFERRED** — `RequireArg[T]` reduced need |

**Remaining recommended savings: 0 bytes** — all recommended changes complete
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

None — all consolidation was achieved through additions to existing files.

### Already Existing Infrastructure

| File | What's There |
|------|-------------|
| `values/values.go` | `Indexable` interface with `Length()`, `Get()`, `Set()` |
| `values/vector.go` | `Indexable` implementation |
| `values/byte_vector.go` | `Indexable` implementation |
| `machine/operation_helpers.go` | `sameType[T]`, `fieldMatches[T, Op]` |
| `registry/helpers/args.go` | `RequireArg[T]`, `RequireIndex`, `ParseOptionalStartEnd`, `ParseSubrange` |

### Files Modified

| File | Change |
|------|--------|
| `registry/helpers/args.go` | Added `RequireIndex` helper |
| `registry/core/prim_vectors.go` | `PrimVectorRef`, `PrimVectorSet` use `RequireIndex` |
| `registry/core/prim_byte_vectors.go` | `PrimBytevectorU8Ref`, `PrimBytevectorU8Set` use `RequireIndex` |
| `registry/core/prim_strings.go` | `PrimStringRef`, `PrimStringSet` use `RequireIndex` |
| 30× `machine/operation_*.go` | Use `sameType` / `fieldMatches` for EqualTo |

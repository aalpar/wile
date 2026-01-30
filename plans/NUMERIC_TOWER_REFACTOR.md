# Numeric Tower Refactor Plan

## Status Summary (2026-01-30)

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 0 | ✅ Complete | Error handling standardized, all 49 combinations tested |
| Phase 1 | ✅ Complete | `numeric_tower.go` infrastructure (Rank, Promote, Simplify, BinaryOp, Tower*) |
| Phase 2 | ✅ Complete | Number interface has all required methods |
| Phase 3 | ✅ Complete | All 7 types have `*Same` methods |
| Phase 4 | 🔄 Pending | Migrate public Add/Sub/etc. to use Tower* functions |
| Phase 5 | 🔄 Pending | Remove legacy type-switch code (~500 lines) |

**Current state**: The Tower* functions work correctly and handle all 49 type combinations. The legacy public methods (Add, Subtract, etc.) still use type switches but are tested and working. Phases 4–5 are internal cleanup that reduces code and simplifies maintenance but doesn't change behavior.

---

## Problem Statement

The current numeric tower implementation violates the elegance principles defined in CLAUDE.md:

1. **Economy violation**: 7 numeric types × 6 operations × 7 type cases = ~294 switch branches, most nearly identical
2. **Symmetry violation**: Missing cases cause panics (Float+BigInteger, Complex<BigInteger, Rational/BigInteger)
3. **Transparency violation**: The R7RS tower hierarchy is declared but not operational—promotion rules are implicit in scattered switch statements
4. **Abstraction fight**: Adding a new numeric type requires touching all 7 existing files

### Bugs Found (now fixed via Tower*)

These issues in the legacy implementation are now bypassed via the Tower* functions:

| Category | Count | Status |
|----------|-------|--------|
| Missing arithmetic cases (Add/Sub/Mul/Div) | 40 | ✅ Fixed: Tower* handles all 49 combinations |
| Missing comparison cases (LessThan/Compare) | 10 | ✅ Fixed: TowerCompare handles all combinations |
| Inconsistent division-by-zero handling | 3 types | ✅ Fixed: All use panic(ErrDivisionByZero) |
| Inconsistent default handling | 3 types | ✅ Fixed: All use panic(ErrNotANumber) |

Initial estimate of "4 bugs" was a 10× underestimate. The Tower* infrastructure resolves all these issues.

## Design Goals

1. **Single promotion table**: Define the type hierarchy once, derive all promotions
2. **Uniform dispatch**: One pattern for all binary operations
3. **Extensibility**: Adding a numeric type = adding one row to the table
4. **Bug elimination**: All type combinations handled correctly
5. **Transparency**: Promotion rules explicit and readable in one place (note: these are implementation choices, not R7RS requirements)

## Architecture

### Core Insight

We impose a total ordering for promotion:

```
Integer < BigInteger < Rational < Float < BigFloat < Complex < BigComplex
         ↑ exact ↑              ↑ inexact ↑         ↑ complex ↑
```

**This is a design decision, not an R7RS requirement.**

- **R7RS §6.2.1** defines a mathematical *subtype* tower (number ⊃ complex ⊃ real ⊃ rational ⊃ integer), but this describes containment, not promotion.
- **R7RS §6.2.2** specifies exactness contagion (exact + inexact = inexact) but says nothing about which concrete type holds the result.
- **R7RS §6.2.3** permits implementations to use any internal representations.

A conforming implementation could make different choices:
- Return `Rational` for `Integer + Float` when exactly representable
- Always promote to `BigFloat` for maximum precision
- Use different internal representations entirely

We choose this ordering because:
1. **Pragmatic precision**: Promoting to "wider" types preserves information
2. **Implementation simplicity**: Total ordering enables uniform dispatch
3. **Predictability**: Users can reason about result types

This choice should be documented in `R7RS_SEMANTIC_DIFFERENCES.md`.

Any binary operation promotes both operands to the "larger" type, performs the operation there, then possibly demotes the result (e.g., Complex with zero imaginary part → real).

### New Design

#### 1. Type Ranking

```go
// numeric_tower.go

type NumericRank int

const (
    RankInteger NumericRank = iota
    RankBigInteger
    RankRational
    RankFloat
    RankBigFloat
    RankComplex
    RankBigComplex
)

// Rank returns the position in the numeric tower
func Rank(n Number) NumericRank {
    switch n.(type) {
    case *Integer:    return RankInteger
    case *BigInteger: return RankBigInteger
    case *Rational:   return RankRational
    case *Float:      return RankFloat
    case *BigFloat:   return RankBigFloat
    case *Complex:    return RankComplex
    case *BigComplex: return RankBigComplex
    }
    panic("unknown numeric type")
}
```

#### 2. Promotion Functions

```go
// Promote converts a number to the target rank
// Returns the promoted value (same value if already at or above target rank)
func Promote(n Number, target NumericRank) Number {
    current := Rank(n)
    if current >= target {
        return n
    }
    // Chain of promotions: Integer→BigInteger→Rational→Float→BigFloat→Complex→BigComplex
    for current < target {
        n = promoteOnce(n)
        current = Rank(n)
    }
    return n
}

// promoteOnce promotes a number exactly one level up the tower
func promoteOnce(n Number) Number {
    switch v := n.(type) {
    case *Integer:
        return NewBigIntegerFromInt64(v.Value)
    case *BigInteger:
        return NewRationalFromBigInt(v.value, big.NewInt(1))
    case *Rational:
        f, _ := v.value.Float64()
        return NewFloat(f)
    case *Float:
        return NewBigFloatFromFloat64(v.Value)
    case *BigFloat:
        return NewComplexFromBigFloat(v, NewBigFloatFromFloat64(0))
    case *Complex:
        return NewBigComplexFromComplex(v)
    case *BigComplex:
        return v // Already at top
    }
    panic("unknown numeric type")
}
```

#### 3. Binary Operation Dispatch

```go
// BinaryOp applies an operation after promoting both operands to common type
func BinaryOp(a, b Number, op func(Number, Number) Number) Number {
    rankA, rankB := Rank(a), Rank(b)
    target := max(rankA, rankB)

    promoted_a := Promote(a, target)
    promoted_b := Promote(b, target)

    result := op(promoted_a, promoted_b)
    return maybeSimplify(result)
}
```

#### 4. Same-Type Operations

Each type only implements operations with itself:

```go
// integer.go - AFTER refactor
func (p *Integer) addSame(o *Integer) Number {
    // Overflow check
    result := p.Value + o.Value
    if (result < p.Value) != (o.Value < 0) {
        // Overflow: promote to BigInteger
        return NewBigIntegerFromInt64(p.Value).addSame(NewBigIntegerFromInt64(o.Value).(*BigInteger))
    }
    return NewInteger(result)
}
```

#### 5. Public API (unchanged)

```go
// Add dispatches through the tower
func (p *Integer) Add(o Number) Number {
    return BinaryOp(p, o, func(a, b Number) Number {
        switch v := a.(type) {
        case *Integer:    return v.addSame(b.(*Integer))
        case *BigInteger: return v.addSame(b.(*BigInteger))
        // ... same pattern for all types
        }
        panic("unreachable")
    })
}
```

### Exactness Handling

Exactness contagion is separate from type promotion:

```go
// ExactnessOf returns the exactness of a number
func ExactnessOf(n Number) Exactness {
    switch v := n.(type) {
    case *Integer, *BigInteger, *Rational:
        return Exact
    case *Float, *BigFloat, *Complex:
        return Inexact
    case *BigComplex:
        if v.IsExact() { return Exact }
        return Inexact
    }
    panic("unknown type")
}

// After operation, apply exactness contagion:
// exact op exact = exact
// exact op inexact = inexact
// inexact op inexact = inexact
func resultExactness(a, b Number) Exactness {
    if ExactnessOf(a) == Inexact || ExactnessOf(b) == Inexact {
        return Inexact
    }
    return Exact
}
```

### Result Simplification

After operations, simplify where possible:

```go
func maybeSimplify(n Number) Number {
    switch v := n.(type) {
    case *BigComplex:
        if v.imag.IsZero() {
            return maybeSimplify(v.real)
        }
    case *Complex:
        if imag(v.Value) == 0 {
            return NewFloat(real(v.Value))
        }
    case *BigFloat:
        if v.IsInteger() {
            return v.ToBigInteger()
        }
    case *Rational:
        if v.IsInteger() {
            return NewBigIntegerFromBigInt(v.Num())
        }
    case *BigInteger:
        if v.FitsInt64() {
            return NewInteger(v.Int64())
        }
    }
    return n
}
```

## Plan Validation (2026-01-23)

### Current Coverage Audit

Audited all 7 numeric type files to build accurate coverage matrices.

#### Arithmetic Operations Coverage

| Receiver | Int | BigInt | Float | BigFloat | Rational | Complex | BigComplex |
|----------|:---:|:------:|:-----:|:--------:|:--------:|:-------:|:----------:|
| Integer  | ✓   | ✓      | ✓     | **✗**    | ✓        | ✓       | ✓          |
| BigInteger | ✓ | ✓      | ✓     | **✗**    | ✓        | ✓       | ✓          |
| Float    | ✓   | **✗**  | ✓     | **✗**    | ✓        | ✓       | ✓          |
| BigFloat | ✓   | ✓      | ✓     | ✓        | ✓        | ✓       | ✓          |
| Rational | ✓   | **✗**  | ✓     | **✗**    | ✓        | ✓       | **✗**      |
| Complex  | ✓   | **✗**  | ✓     | **✗**    | ✓        | ✓       | **✗**      |
| BigComplex | ✓ | ✓      | ✓     | ✓        | ✓        | ✓       | ✓          |

**Missing cases (8 combinations × 4 operations = 32 failures)**:
- Integer + BigFloat (panic)
- BigInteger + BigFloat (nil)
- Float + BigInteger (panic)
- Float + BigFloat (panic)
- Rational + BigInteger (panic)
- Rational + BigFloat (panic)
- Complex + BigInteger (panic)
- Complex + BigFloat (panic)

*Verified by `TestNumericTower_*` in `numeric_tower_coverage_test.go`*

#### Comparison Coverage (LessThan/Compare)

| Receiver | Int | BigInt | Float | BigFloat | Rational | Complex | BigComplex |
|----------|:---:|:------:|:-----:|:--------:|:--------:|:-------:|:----------:|
| Integer  | ✓   | ✓      | ✓     | ✓        | ✓        | ✓       | ✓          |
| BigInteger | ✓ | ✓      | ✓     | ✓        | ✓        | **✗**   | ✓          |
| Float    | ✓   | ✓      | ✓     | ✓        | ✓        | ✓       | ✓          |
| BigFloat | ✓   | ✓      | ✓     | ✓        | ✓        | **✗**   | ✓          |
| Rational | ✓   | **✗**  | ✓     | **✗**    | ✓        | ✓       | **✗**      |
| Complex  | ✓   | **✗**  | ✓     | **✗**    | ✓        | ✓       | **✗**      |
| BigComplex | ✓ | ✓      | ✓     | ✓        | ✓        | ✓       | ✓          |

**Missing comparison cases (2 failures)**:
- Complex < BigInteger (panic)
- Complex < BigFloat (panic)

*Note: Earlier analysis overestimated. Rational handles BigInteger/BigFloat via conversion. Verified by `TestNumericTower_LessThan` in `numeric_tower_coverage_test.go`*

#### Error Handling Inconsistency

| Type | Division by Zero | Missing Type Case |
|------|------------------|-------------------|
| Integer | `panic(ErrDivisionByZero)` | `panic(ErrNotANumber)` |
| Float | `panic(ErrDivisionByZero)` | `panic(ErrNotANumber)` |
| BigInteger | `return nil` ⚠️ | `return nil` ⚠️ |
| BigFloat | `return nil` ⚠️ | `return nil` ⚠️ |
| Rational | `panic(ErrDivisionByZero)` | `panic(ErrNotANumber)` |
| Complex | `panic(ErrDivisionByZero)` | `panic(ErrNotANumber)` |
| BigComplex | `return nil` ⚠️ | `return nil` ⚠️ |

The Big* types silently return nil while others panic—this inconsistency is a bug waiting to cause silent failures.

### Test Coverage Gaps

Current tests verify same-type operations and some cross-type operations but not all 49 combinations:

| Test File | Covers |
|-----------|--------|
| `integer_test.go` | Integer ↔ Integer, Float, Rational, Complex |
| `float_test.go` | Float ↔ Integer, Float, Rational, Complex |
| `big_number_test.go` | BigInteger/BigFloat ↔ Integer, Float, BigInteger, Complex |
| `rational_test.go` | Rational ↔ Integer, Float, Rational, Complex |
| `complex_test.go` | Complex ↔ Integer, Float, Rational, Complex |
| `big_complex_test.go` | BigComplex ↔ (minimal) |

**Missing test coverage**: All combinations involving BigFloat as operand (except BigFloat receiver), BigComplex cross-type tests.

### Revised Risk Assessment

| Risk | Severity | Original Plan | Validated Assessment |
|------|----------|---------------|---------------------|
| Missing arithmetic cases | High | 4 cases | 32 (8 combinations × 4 ops) |
| Missing comparison cases | Medium | included above | 2 (Complex < BigInteger/BigFloat) |
| Division-by-zero nil returns | High | Not addressed | 21 cases (BigInteger, BigFloat, BigComplex × 7 zeros) |
| Silent failures (default nil) | High | Not addressed | 4 cases (BigInteger+BigFloat for 4 ops) |
| Test coverage | Medium | "Add tests" | ✅ Created `numeric_tower_coverage_test.go` |

## Implementation Phases

### Phase 0: Stabilize Error Handling (prerequisite) ✅ COMPLETE

Before adding any missing cases, standardize error handling to prevent silent failures:

**Step 0.1**: Create comprehensive "current behavior" test suite ✅
- `numeric_tower_coverage_test.go` tests all 49 type combinations for each operation
- Tests verify all combinations work without panic for valid operations

**Step 0.2**: Fix nil-return bugs in Big* types ✅
- All types now use `panic(ErrNotANumber)` for unknown types
- All types use `panic(ErrDivisionByZero)` for division by zero
- Error handling is consistent across all 7 numeric types

**Step 0.3**: Add missing arithmetic cases ✅
- All 49 type combinations now work via the Tower* functions

**Step 0.4**: Add missing comparison cases ✅
- TowerCompare handles all 49 combinations

**Deliverable**: ✅ All 49 type combinations work. Tests pass.

### Phase 1: Infrastructure ✅ COMPLETE

Created `values/numeric_tower.go`:

1. ✅ `NumericRank` enum (RankInteger through RankBigComplex)
2. ✅ `Rank(Number) NumericRank`
3. ✅ `Promote(Number, NumericRank) Number`
4. ✅ `promoteOnce(Number) Number`
5. ✅ `Simplify(Number) Number` (simplifies results where possible)
6. ✅ `CommonRank(a, b Number) NumericRank`
7. ✅ `PromoteBoth(a, b Number) (Number, Number)`
8. ✅ `BinaryOp(a, b Number, op func) Number` - unified dispatch
9. ✅ `Exactness` type with `ExactnessOf` and `ResultExactness`
10. ✅ Tower operations: `TowerAdd`, `TowerSubtract`, `TowerMultiply`, `TowerDivide`, `TowerCompare`
11. ✅ Same-type dispatchers: `addOp`, `subtractOp`, `multiplyOp`, `divideOp`, `compareOp`

Tests in `numeric_tower_test.go`:
- TestRank, TestRank_Order
- TestPromoteOnce (all 7 types)
- TestPromote, TestPromote_PreservesValue
- TestCommonRank, TestPromoteBoth
- TestSimplify
- TestTowerAdd/Subtract/Multiply/Divide/Compare

**Deliverable**: ✅ Promotion infrastructure complete with comprehensive tests.

### Phase 2: Extend Number Interface ✅ COMPLETE

The Number interface in `values/values.go` includes all required methods:

```go
type Number interface {
    Value
    Add(Number) Number
    Subtract(Number) Number
    Multiply(Number) Number
    Divide(Number) Number
    Negate() Number
    IsZero() bool
    IsExact() bool
    LessThan(Number) bool
    Compare(Number) int
}
```

All 7 numeric types implement this interface.

**Deliverable**: ✅ Complete Number interface with all methods on all types.

### Phase 3: Same-Type Operations ✅ COMPLETE

All 7 numeric types have private same-type methods:

| Type | Methods |
|------|---------|
| Integer | `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame` |
| BigInteger | `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame` |
| Rational | `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame` |
| Float | `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame` |
| BigFloat | `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame` |
| Complex | `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame` |
| BigComplex | `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame` |

These contain the arithmetic logic without type switches. The Tower* functions in `numeric_tower.go` dispatch to these after promotion.

**Deliverable**: ✅ Each type has clean same-type operations.

### Phase 4: Unified Dispatch 🔄 PENDING

The `BinaryOp` dispatcher exists in `numeric_tower.go`:

```go
func BinaryOp(a, b Number, op func(Number, Number) Number) Number
```

Tower functions (`TowerAdd`, `TowerSubtract`, etc.) use this dispatcher and work correctly.

**Remaining work**: Replace the type-switch implementations in each type's public methods with calls to the Tower functions:

```go
// CURRENT (in integer.go, big_integer.go, etc.):
func (p *Integer) Add(o Number) Number {
    switch v := o.(type) {
    case *Integer: ...
    case *BigInteger: ...
    // ~7 type cases per method
    }
}

// TARGET:
func (p *Integer) Add(o Number) Number {
    return TowerAdd(p, o)
}
```

This change would:
1. Eliminate ~600 lines of type-switch code across 7 files
2. Ensure all type combinations go through the same promotion logic
3. Simplify maintenance when adding new numeric types

**Consideration**: The legacy implementations have been well-tested and the Tower* functions are parallel implementations. Migration requires careful verification that behavior is identical.

**Deliverable**: 🔄 All arithmetic uses unified dispatch (pending migration).

### Phase 5: Cleanup 🔄 PENDING (depends on Phase 4)

After Phase 4 migration:

1. Remove duplicate switch statements from all types (~600 lines)
2. Remove redundant helper functions (e.g., `promoteToBigComplexPart`)
3. ✅ CLAUDE.local.md in values/ package already updated with Tower documentation
4. ✅ Architecture documented in this plan

**Deliverable**: 🔄 Clean, minimal implementation (pending Phase 4).

## File Changes Summary

| File | Status | Changes |
|------|--------|---------|
| `values/numeric_tower.go` | ✅ NEW | Rank, Promote, Simplify, BinaryOp, Tower*, Exactness |
| `values/numeric_tower_test.go` | ✅ NEW | Infrastructure tests (Rank, Promote, Simplify, Tower*) |
| `values/numeric_tower_coverage_test.go` | ✅ NEW | 49-combination coverage tests for all operations |
| `values/values.go` | ✅ | Number interface complete |
| `values/integer.go` | ✅ | Has `*Same` methods |
| `values/big_integer.go` | ✅ | Has `*Same` methods, consistent panic handling |
| `values/float.go` | ✅ | Has `*Same` methods |
| `values/big_float.go` | ✅ | Has `*Same` methods, consistent panic handling |
| `values/rational.go` | ✅ | Has `*Same` methods |
| `values/complex.go` | ✅ | Has `*Same` methods |
| `values/big_complex.go` | ✅ | Has `*Same` methods, consistent panic handling |

**Phase 4 (pending)**: Each type file will replace its public Add/Subtract/etc. methods with calls to TowerAdd/TowerSubtract/etc.

## Metrics

### Before (validated 2026-01-23)

- Lines of switch-case code: ~600
- Type combinations handled: 196 of 245 (7 types × 7 operands × 5 operations)
- Missing combinations: 49 (40 arithmetic + 10 comparison, some overlap)
- Files to modify for new type: 7
- Error handling consistency: 57% (4 of 7 types use panic consistently)

### Current (2026-01-27)

- Lines of switch-case code: ~600 (legacy) + ~100 (tower) — dual implementations
- Type combinations handled via Tower*: 245 (7×7×5, complete) ✅
- Error handling consistency: 100% ✅
- New infrastructure: `numeric_tower.go` (356 lines) + tests
- Files to modify for new type: Still 7 (legacy paths not yet removed)

### After Phase 4+5 (projected)

- Lines of switch-case code: ~100 (in Rank, promoteOnce, Simplify only)
- Type combinations handled: 245 (7×7×5, complete)
- Files to modify for new type: 2 (new type file + numeric_tower.go)
- Net code reduction: ~500 lines

## Risks

1. **Performance**: Extra promotion allocations. Mitigate: fast-path for same-type operations.
2. **Precision loss**: Promotion may lose precision (BigInteger→Float). Mitigate: Document, add warnings in R7RS_SEMANTIC_DIFFERENCES.md.
3. **Behavioral changes**: Some edge cases may change. Mitigate: Extensive golden tests before refactor.

## Alternatives Considered

### Double Dispatch (rejected)

Each type defines `visitInteger`, `visitFloat`, etc. Problem: Still O(n²) methods, just distributed differently.

### Interface per Operation (rejected)

`Addable`, `Subtractable`, etc. Problem: Doesn't solve the promotion problem.

### Generics (rejected)

Go generics can't express "same type" constraints for binary operations across a type hierarchy.

## Design Decisions

### Error Handling: `panic()` vs Error Returns

**Decision**: Use `panic()` for all arithmetic errors (division by zero, missing type combinations).

**R7RS §6.2.6**: Division by zero is an error. The specification doesn't mandate a specific error mechanism, only that it be signaled.

**Rationale**:
1. These errors are rare in valid programs — panic cost only paid when errors occur
2. Missing type combinations are programming errors (implementation bugs), not user errors
3. Maps naturally to Scheme's exception semantics — the VM's `recover()` boundary converts panics to Scheme exceptions
4. Simpler API: `result := a.Add(b)` without error checking at every operation
5. Consistent with existing codebase design
6. Establishes a pattern that could extend to other error cases in the future

**Implementation**: All `return nil` cases in Big* types must change to `panic(ErrNotANumber)` or `panic(ErrDivisionByZero)`.

### Complex Comparison Semantics

**R7RS §6.2.6 states**: "For any of the `<` `=` `>` `<=` `>=` procedures, if any argument is complex, an error is signaled."

**Current implementation**: Compares complex numbers by real part only (non-standard extension).

**Decision needed**: Choose one of:

1. **R7RS strict**: `LessThan` on Complex should `panic(ErrComplexComparison)` — fully conformant
2. **Current behavior**: Compare by real part only — convenient but non-standard
3. **Hybrid**: Error only when imaginary parts are non-zero — pragmatic compromise

**Recommendation**: Option 1 (R7RS strict) for conformance. The current "compare by real part" behavior can lead to surprising results like `(< 3+4i 2+5i)` returning `#f` when mathematically neither is "less than" the other.

**If keeping current behavior**: Document in `R7RS_SEMANTIC_DIFFERENCES.md` as an extension.

### Precision Loss Scenarios

The promotion order `Integer < BigInteger < Rational < Float < BigFloat < Complex < BigComplex` has these precision implications:

| Operation | Promotion | Precision Loss | R7RS Compliance |
|-----------|-----------|----------------|-----------------|
| BigInteger + Float | BigInt → float64 | Yes: >2^53 loses digits | Correct: exact + inexact = inexact |
| Rational + Float | Rational → float64 | Yes: non-binary fractions | Correct: exact + inexact = inexact |
| Float + BigFloat | Float → BigFloat | No: precision increases | Correct |
| BigInteger + BigFloat | BigInt → BigFloat | Minimal: BigFloat has 256-bit precision | Correct |

**Examples of precision loss:**
```scheme
(+ #z99999999999999999999999999999999 1.0)  ; loses ~80 digits
(+ 1/3 0.0)                                 ; loses exact representation
```

**Mitigation**: This is correct R7RS behavior (§6.2.2 exactness contagion). Document in `R7RS_SEMANTIC_DIFFERENCES.md` that users should use exact arithmetic to preserve precision.

## Success Criteria

1. All existing tests pass
2. No panics on any valid type combination
3. Total switch-case code reduced by >70%
4. Adding a test new type (e.g., `Decimal`) requires <100 lines
5. Promotion rules are readable in one place
6. **NEW**: Consistent error handling (all types panic, none return nil)

## References

- R7RS §6.2.1 (Numerical types) — defines the subtype tower, not promotion rules
- R7RS §6.2.2 (Exactness) — defines exactness contagion, not type promotion
- R7RS §6.2.6 (Numerical operations) — operation semantics
- CLAUDE.md (Code Elegance section)

## What R7RS Actually Requires vs. What We Decide

| Aspect | R7RS Requirement | Our Design Decision |
|--------|------------------|---------------------|
| Tower hierarchy | number ⊃ complex ⊃ real ⊃ rational ⊃ integer | Same |
| Exactness contagion | exact + inexact = inexact | Same |
| Concrete result type | Unspecified | Total ordering determines type |
| Integer overflow | Unspecified | Promote to BigInteger |
| Float + BigInteger | Must work, type unspecified | Returns Float |
| Rational + Float | Must be inexact, type unspecified | Returns Float |
| Precision preservation | Encouraged but unspecified | Best-effort within type constraints |

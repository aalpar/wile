# Numeric Tower Refactor Plan

## Status Summary (2026-02-05)

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 0 | ✅ Complete | Error handling standardized, all 49 combinations tested |
| Phase 1 | ✅ Complete | `numeric_tower.go` infrastructure (Rank, Promote, Simplify, BinaryOp, Tower*) |
| Phase 2 | ✅ Complete | Number interface has all required methods |
| Phase 3 | ✅ Complete | All 7 types have `*Same` methods |
| Phase 4 | ❌ **Abandoned** | Migrate public Add/Sub/etc. to use Tower* functions |
| Phase 5 | ❌ **Abandoned** | Remove "legacy" type-switch code |
| Phase 6 | ❌ **Abandoned** | Fix Tower* complex number handling |

**Final state**: Direct method dispatch (`a.Add(b)`) is the intentional architecture, not "legacy code awaiting cleanup." It correctly handles all cases including exact complex numbers. The Tower* functions remain as unused infrastructure with known design flaws (see Phase 6 section for details).

---

## Actual Promotion Behavior (Direct Dispatch)

The direct dispatch implementation does NOT use a linear tower. It implements a **lattice** where result types depend on both operands.

### Result Type Matrix (A op B → Result)

For arithmetic operations (`+`, `-`, `*`, `/`):

| A ↓ / B → | Integer | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
|-----------|---------|------------|----------|-------|----------|---------|------------|
| **Integer** | Integer¹ | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
| **BigInteger** | BigInteger | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
| **Rational** | Rational | Rational | Rational | Float | BigFloat | Complex | BigComplex |
| **Float** | Float | Float | Float | Float | BigFloat | Complex | BigComplex |
| **BigFloat** | BigFloat | BigFloat | BigFloat | BigFloat | BigFloat | Complex² | BigComplex |
| **Complex** | Complex | Complex | Complex | Complex | Complex² | Complex | BigComplex |
| **BigComplex** | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex |

¹ Integer + Integer may overflow to BigInteger
² BigFloat + Complex → Complex loses BigFloat precision (converts to float64)

### Exactness Preservation

| A ↓ / B → | Exact | Inexact |
|-----------|-------|---------|
| **Exact** | Exact | Inexact |
| **Inexact** | Inexact | Inexact |

Where:
- **Exact**: Integer, BigInteger, Rational, BigComplex(with exact parts)
- **Inexact**: Float, BigFloat, Complex, BigComplex(with inexact parts)

### Visual: Two Orthogonal Dimensions

```
                    REAL                          COMPLEX
                      │                              │
     Exact:   Integer → BigInteger → Rational       BigComplex(exact)
                      │         ╲        │              │
                      │          ╲       │              │
                      ↓           ╲      ↓              ↓
   Inexact:        Float    →    BigFloat    →     Complex / BigComplex(inexact)
```

**Promotion rules:**
1. **Within exact reals**: Integer → BigInteger → Rational (increasing generality)
2. **Within inexact reals**: Float → BigFloat (increasing precision)
3. **Exact + Inexact**: Result is inexact (exactness contagion, per R7RS §6.2.2)
4. **Real + Complex**: Result is complex (dimensionality wins)
5. **BigFloat + Complex**: Returns Complex (loses BigFloat precision—converts to float64). This is a known limitation; use BigComplex explicitly to preserve arbitrary precision with complex numbers.

### The Lattice Structure

There is no single linear tower. The direct dispatch implements a **lattice**:

```
                    BigComplex
                   ↗    ↑    ↖
            Complex   BigFloat   (exact BigComplex path)
               ↑    ↗    ↑         ↑
             Float    Rational ────┘
               ↑        ↑
            Integer → BigInteger
```

The Tower* functions attempted to linearize this into:
```
Integer < BigInteger < Rational < Float < BigFloat < Complex < BigComplex
```

This linearization breaks exact complex numbers because it forces exact types through Float before reaching Complex/BigComplex.

---

**Decision (2026-02-05)**: Phases 4-6 abandoned. Rationale:
1. Direct dispatch is correct and battle-tested
2. Tower* has latent bugs with exact complex number handling
3. The ~500 line "savings" from migration isn't worth the regression risk
4. Per CLAUDE.md: "Avoid over-engineering. Only make changes that are directly requested or clearly necessary."

The Tower* functions (`TowerAdd`, etc.) may be removed in a future cleanup, or kept for testing purposes. They should NOT be used by primitives.

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

### Phase 4: Unified Dispatch ❌ ABANDONED

The `BinaryOp` dispatcher exists in `numeric_tower.go`:

```go
func BinaryOp(a, b Number, op func(Number, Number) Number) Number
```

Tower functions (`TowerAdd`, `TowerSubtract`, etc.) use this dispatcher.

**Original goal**: Replace type-switch implementations with Tower* calls to reduce code.

**Why abandoned**: The Tower* functions have a latent bug where exact complex numbers lose exactness during promotion (see Phase 6). Direct dispatch handles this correctly. Migrating would introduce a regression.

**Outcome**: Direct method dispatch is the intentional architecture. The type-switch code in each numeric type is not "legacy"—it's the correct, tested implementation.

### Phase 5: Cleanup ❌ ABANDONED

**Original goal**: Remove ~600 lines of type-switch code after Phase 4 migration.

**Why abandoned**: Phase 4 was abandoned. The type-switch code stays.

**Note**: The code is more verbose than a unified dispatcher, but it's correct and each case is explicit. When adding a new numeric type, the cost of adding switch cases is acceptable.

## File Changes Summary

| File | Status | Changes |
|------|--------|---------|
| `values/numeric_tower.go` | ✅ Complete | Rank, Promote, Simplify, BinaryOp, Tower*, Exactness |
| `values/numeric_tower_test.go` | ✅ Complete | Infrastructure tests (Rank, Promote, Simplify, Tower*) |
| `values/numeric_tower_coverage_test.go` | ✅ Complete | 49-combination coverage tests for all operations |
| `values/values.go` | ✅ Complete | Number interface complete |
| `values/integer.go` | ✅ Complete | Has `*Same` methods, direct dispatch |
| `values/big_integer.go` | ✅ Complete | Has `*Same` methods, consistent panic handling |
| `values/float.go` | ✅ Complete | Has `*Same` methods, direct dispatch |
| `values/big_float.go` | ✅ Complete | Has `*Same` methods, consistent panic handling |
| `values/rational.go` | ✅ Complete | Has `*Same` methods, direct dispatch |
| `values/complex.go` | ✅ Complete | Has `*Same` methods, direct dispatch |
| `values/big_complex.go` | ✅ Complete | Has `*Same` methods, consistent panic handling |

**Final state**: All files complete. Direct dispatch is the production implementation.
Tower* functions exist but are unused. No further changes planned.

## Metrics

### Before (2026-01-23)

- Lines of switch-case code: ~600
- Type combinations handled: 196 of 245 (7 types × 7 operands × 5 operations)
- Missing combinations: 49 (40 arithmetic + 10 comparison, some overlap)
- Files to modify for new type: 7
- Error handling consistency: 57% (4 of 7 types use panic consistently)

### Final State (2026-02-05)

- Lines of switch-case code: ~600 (direct dispatch) + ~100 (Tower* unused)
- Type combinations handled: 245 (7×7×5, complete) ✅
- Error handling consistency: 100% ✅
- Infrastructure: `numeric_tower.go` exists but Tower* functions are unused
- Files to modify for new type: 7 (acceptable cost for correctness)
- Exact complex numbers: ✅ Handled correctly by direct dispatch

### Projected Savings (NOT REALIZED - Phases 4-5 Abandoned)

The original plan projected ~500 lines of code reduction by migrating to Tower*.
This was abandoned because:
1. Tower* has latent bugs with exact complex numbers
2. Risk of regression outweighed code reduction benefit
3. Direct dispatch is correct and maintainable

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

**R7RS §6.2.6 states**: "For any of the `<` `>` `<=` `>=` procedures, if any argument is complex, an error is signaled."

**Note**: The `=` procedure IS defined for complex numbers—it compares both real and imaginary parts for numerical equality. Only the ordering predicates (`<`, `>`, `<=`, `>=`) are prohibited.

**Current implementation**: The `Compare` and `LessThan` methods compare complex numbers by real part only (non-standard extension for ordering, correct for equality).

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

---

## Phase 6: Complex as Orthogonal Container ❌ ABANDONED

### Problem: Linear Tower Would Lose Exactness for Complex Numbers

**IMPORTANT: This bug is NOT currently user-visible.** The Tower* functions (`TowerAdd`, etc.)
are unused infrastructure. Primitives use direct method dispatch (`a.Add(b)`), which works
correctly. This issue only becomes real if Phase 4 (migrate to Tower*) is completed.

The Tower* promotion path has a latent bug:

```
Integer < BigInteger < Rational < Float < BigFloat < Complex < BigComplex
```

Forces promotion through inexact types when combining exact reals with complex numbers:

```go
// promoteOnce path for exact Integer → BigComplex:
Integer → BigInteger → Rational → Float (LOSES EXACTNESS!) → BigFloat → Complex → BigComplex
```

This would violate R7RS §6.2.2: a complex number should be exact if both its real and
imaginary parts are exact.

**Example of the latent bug (if TowerAdd were used):**
```scheme
(exact? (+ 1+2i 3))  ; Should be #t (both exact), but TowerAdd would make it #f
```

### Why the Tower Design Has This Problem

The tower conflates two orthogonal dimensions:

1. **Precision/representation**: `int64 → big.Int → big.Rat → float64 → big.Float`
2. **Dimensionality**: real → complex

Complex numbers aren't "higher" than reals—they're *containers* parameterized by a real type.

Additionally, there's a **precision loss** bug at `BigFloat → Complex`:
```go
case *BigFloat:
    return NewComplex(complex(v.Float64(), 0))  // Loses arbitrary precision!
```

### Why It Doesn't Currently Affect Users

Three independent systems handle complex numbers correctly:

1. **Parser**: `parseComplex()` creates `BigComplex` for exact literals, `Complex` for inexact
2. **Direct dispatch**: `BigComplex.Add(Integer)` converts Integer → BigInteger, preserving exactness
3. **make-rectangular**: Checks `ExactnessOf()` and creates appropriate type

The Tower* functions are parallel infrastructure that's NOT integrated with primitives yet.

### Critical Sequencing Requirement

**Phase 4 MUST NOT be completed before Phase 6.**

If we replace direct dispatch with Tower* calls (Phase 4) without fixing the complex
promotion logic (Phase 6), we would INTRODUCE a regression where exact complex arithmetic
becomes inexact.

Current state:
- Direct dispatch: ✅ Works correctly
- Tower* functions: ❌ Has latent exactness bug
- Phase 4 (migrate to Tower*): 🔄 Pending
- Phase 6 (fix Tower* for complex): 🔄 Proposed

Safe ordering: Phase 6 → Phase 4 (or abandon Phase 4 entirely)

### Proposed Architecture

#### Two Orthogonal Hierarchies

**Real Tower** (linear, for promotion):
```
Integer < BigInteger < Rational < Float < BigFloat
```

**Complex Wrapper** (orthogonal, wraps any real type):
```
Complex[T] where T is any real Number
```

Concrete types:
- `Complex` — wraps `complex128` (always inexact, performance optimization)
- `BigComplex` — wraps any `Number` pair (exactness depends on components)

#### New Classification Functions

```go
// IsComplex returns true if n has an imaginary part.
func IsComplex(n Number) bool {
    switch n.(type) {
    case *Complex, *BigComplex:
        return true
    }
    return false
}

// RealRank returns the rank of a number's real component.
type RealRank int

const (
    RealRankInteger RealRank = iota
    RealRankBigInteger
    RealRankRational
    RealRankFloat
    RealRankBigFloat
)

func GetRealRank(n Number) RealRank {
    switch v := n.(type) {
    case *Integer:
        return RealRankInteger
    case *BigInteger:
        return RealRankBigInteger
    case *Rational:
        return RealRankRational
    case *Float:
        return RealRankFloat
    case *BigFloat:
        return RealRankBigFloat
    case *Complex:
        return RealRankFloat  // complex128 uses float64
    case *BigComplex:
        return getRealRankOf(v.Real())
    }
    panic(ErrNotANumber)
}
```

#### Two Bugs in promoteOnce

The current `promoteOnce` function has two problems:

**Bug 1: Exactness loss at Rational → Float**
```go
case *Rational:
    f, _ := v.value.Float64()
    return NewFloat(f)  // Exact → Inexact!
```

**Bug 2: Precision loss at BigFloat → Complex**
```go
case *BigFloat:
    return NewComplex(complex(v.Float64(), 0))  // Loses arbitrary precision!
```

Both bugs only manifest if code uses the Tower* functions (which primitives currently don't).

### Exactness-Preserving Complex Promotion

```go
// ToComplex converts a real number to complex with zero imaginary.
// Preserves exactness.
func ToComplex(n Number) *BigComplex {
    switch v := n.(type) {
    case *Integer:
        bi := NewBigIntegerFromInt64(v.Value)
        return NewBigComplex(bi, NewBigIntegerFromInt64(0))  // exact!
    case *BigInteger:
        return NewBigComplex(v, NewBigIntegerFromInt64(0))   // exact!
    case *Rational:
        zero := NewRationalFromInt64(0, 1)
        return NewBigComplex(v, zero)                         // exact!
    case *Float:
        bf := NewBigFloatFromFloat64(v.Value)
        return NewBigComplex(bf, NewBigFloatFromFloat64(0))  // inexact
    case *BigFloat:
        return NewBigComplex(v, NewBigFloatFromFloat64(0))   // inexact
    case *Complex:
        return NewBigComplexFromBigFloats(
            NewBigFloatFromFloat64(real(v.Value)),
            NewBigFloatFromFloat64(imag(v.Value)),
        )
    case *BigComplex:
        return v
    }
    panic(ErrNotANumber)
}
```

#### New Binary Operation Dispatch

```go
// BinaryOpV2 handles real and complex numbers correctly.
func BinaryOpV2(a, b Number, realOp, complexOp func(Number, Number) Number) Number {
    aComplex := IsComplex(a)
    bComplex := IsComplex(b)

    if !aComplex && !bComplex {
        // Both real: use existing real tower
        target := CommonRealRank(GetRealRank(a), GetRealRank(b))
        pa := PromoteReal(a, target)
        pb := PromoteReal(b, target)
        return Simplify(realOp(pa, pb))
    }

    // At least one complex: convert both to BigComplex (preserving exactness)
    ca := ToComplex(a)
    cb := ToComplex(b)

    // Promote real parts to common rank within BigComplex
    // (BigComplex arithmetic already handles this)
    result := complexOp(ca, cb)
    return Simplify(result)
}
```

### Migration Path

#### Option A: Minimal Change (Recommended)

Keep the existing tower for real numbers. Only change how complex numbers enter the picture:

1. Add `ToComplex()` function that preserves exactness
2. Modify `TowerAdd`/etc. to detect complex operands and use `ToComplex()` instead of `Promote()`
3. Remove `Complex` and `BigComplex` from `NumericRank` enum (they're not part of the linear tower)

**Changes required:**
- `numeric_tower.go`: Add complex detection, modify Tower* functions
- No changes to individual type files

#### Option B: Full Refactor

Replace `NumericRank` with separate `RealRank` and `Complexness` dimensions:

1. New `RealRank` enum (Integer through BigFloat only)
2. New `Complexness` enum (Real, Complex)
3. Rewrite `Promote()` to only work on reals
4. Add `ToComplex()` for dimension change
5. Rewrite `BinaryOp()` to handle dimensions separately

**Pros:** Cleaner conceptual model
**Cons:** More invasive, higher risk

#### Option C: Eliminate `Complex` Type

Use only `BigComplex` for all complex numbers:

```go
type BigComplex struct {
    real Number  // any real type
    imag Number  // any real type
}
```

When both parts are `*Float`, internally use `complex128` for arithmetic as an optimization, but the external type is always `BigComplex`.

**Pros:** Single complex type, no tower confusion
**Cons:** Loses some `complex128` performance, requires changing all `Complex` references

### Testing Requirements

**Current behavior (direct dispatch):** All these tests PASS because primitives use
`BigComplex.Add()` directly, which preserves exactness.

**Latent bug (Tower* functions):** If we switched to `TowerAdd`, tests marked ⚠️ would FAIL.

```scheme
;; Exactness preservation tests
(exact? (+ 1+2i 3))           ; #t - exact + exact          ⚠️ TowerAdd would return #f
(exact? (+ 1+2i 3.0))         ; #f - exact + inexact        ✓ Both paths correct
(exact? (+ 1.0+2.0i 3))       ; #f - inexact complex        ✓ Both paths correct
(exact? (+ 1/2+1/3i 1/4))     ; #t - rational complex + rational  ⚠️ TowerAdd would return #f

;; make-rectangular exactness (NOT affected - doesn't use Tower*)
(exact? (make-rectangular 1 2))    ; #t  ✓ Always correct
(exact? (make-rectangular 1.0 2))  ; #f  ✓ Always correct
(exact? (make-rectangular 1 2.0))  ; #f  ✓ Always correct

;; Arithmetic correctness (values correct, but exactness may differ)
(= (+ 1+2i 3+4i) 4+6i)                           ; #t  ✓ Value correct
(= (real-part (+ 1/2 1/4+0i)) 3/4)               ; #t  ⚠️ Value correct but would be inexact
(= (* 1/2+1/2i 1/2-1/2i) 1/2)                    ; #t  ⚠️ Value correct but would be inexact
```

**Go-level unit tests needed:**
- `TestToComplex_PreservesExactness` - verify Integer/BigInteger/Rational → exact BigComplex
- `TestPromoteOnce_PrecisionLoss` - document the BigFloat → Complex precision loss
- `TestTowerAdd_ExactComplex` - verify Tower* handles exact complex (after Phase 6 fix)

### Open Questions (Resolved)

These questions were relevant when Phase 6 was under consideration. With Phases 4-6
abandoned, they're now just historical notes.

1. **Should `make-polar` always return inexact?**
   - **Resolution:** Current implementation already does this correctly. No change needed.

2. **What about `Complex` (`complex128`) performance?**
   - **Resolution:** Direct dispatch uses `Complex` for inexact, `BigComplex` for exact.
     This is the correct design. No migration to unified type needed.

3. **Should we allow `Integer`/`Float` as BigComplex components?**
   - **Resolution:** Current restriction to `BigInteger`/`Rational`/`BigFloat` is correct.
     `make-rectangular` and parser handle promotion to these types appropriately.

### Implementation Priority ❌ ABANDONED

This section described work to fix the Tower* functions. Since Phases 4-6 are abandoned,
this work is not needed. The direct dispatch implementation is correct.

| Task | Status | Notes |
|------|--------|-------|
| Add exactness preservation tests | Optional | Current tests already verify correct behavior |
| Fix Tower* for complex | Abandoned | Tower* functions are unused |
| Benchmark complex arithmetic | Not needed | Direct dispatch performance is acceptable |
| Unify Complex/BigComplex types | Not needed | Current type separation works correctly |

### Corrections to Original Analysis (2026-02-05)

The following misconceptions were identified and corrected:

| Original Claim | Correction |
|----------------|------------|
| "The bug is in `TowerAdd`/etc." | True, but **not user-visible** since primitives use direct dispatch |
| "R7RS prohibits `=` on complex" | **Wrong**: only `<`, `>`, `<=`, `>=` are prohibited; `=` IS defined |
| Phase 4 → Phase 5 ordering | **Dangerous**: Phase 6 must come BEFORE Phase 4 or we introduce a regression |
| Only exactness loss mentioned | **Incomplete**: also precision loss at BigFloat → Complex |
| "Current Workaround" framing | **Misleading**: direct dispatch is the primary implementation, not a workaround |

**Architecture clarification**: The codebase has THREE independent systems handling complex numbers:

1. **Parser** (`parseComplex`): Creates correct types from literals
2. **Direct dispatch** (`BigComplex.Add`, etc.): Used by primitives, handles exactness correctly
3. **Tower* functions** (`TowerAdd`, etc.): Unused infrastructure with latent bugs

The Tower* functions were designed for a future refactoring (Phase 4) that would simplify
the type-switch code. However, Phase 4 should NOT proceed until Phase 6 fixes the Tower*
complex handling, or we would introduce user-visible regressions.

**Decision**: Phases 4-6 abandoned. Direct dispatch is the intentional architecture.
The Tower* functions remain as unused infrastructure and may be removed in future cleanup.

### References

- R7RS §6.2.2: "A complex number is exact if and only if both its real and imaginary parts are exact."
- R7RS §6.2.6: `=` compares complex numbers for numerical equality; `<`/`>`/`<=`/`>=` signal error
- Chez Scheme: Complex is parameterized, promotes components independently
- Racket: Similar to Chez, complex wraps any numeric type
- Flatt 2016 (Binding as Sets of Scopes): Not directly relevant, but same author discusses Racket numerics elsewhere

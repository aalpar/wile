# Numeric Tower Refactor Plan

## Problem Statement

The current numeric tower implementation violates the elegance principles defined in CLAUDE.md:

1. **Economy violation**: 7 numeric types × 6 operations × 7 type cases = ~294 switch branches, most nearly identical
2. **Symmetry violation**: Missing cases cause panics (Float+BigInteger, Complex<BigInteger, Rational/BigInteger)
3. **Transparency violation**: The R7RS tower hierarchy is declared but not operational—promotion rules are implicit in scattered switch statements
4. **Abstraction fight**: Adding a new numeric type requires touching all 7 existing files

### Bugs Found

| Bug | Location | Impact |
|-----|----------|--------|
| Float.Add/Subtract missing BigInteger, BigFloat | float.go | panic on valid operations |
| Complex.LessThan missing BigInteger, BigFloat | complex.go | panic on valid comparisons |
| Rational.Divide missing BigInteger | rational.go | panic on valid division |
| Division by zero returns nil (not panic) | big_integer.go, big_float.go, big_complex.go | silent failure |

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

**This is a design decision, not an R7RS requirement.** R7RS §6.2.1 defines a mathematical *subtype* tower (number ⊃ complex ⊃ real ⊃ rational ⊃ integer), but this describes containment, not promotion. R7RS §6.2.2 specifies exactness contagion (exact + inexact = inexact) but says nothing about which concrete type holds the result.

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

## Implementation Phases

### Phase 0: Fix Critical Bugs (prerequisite)

Before refactoring, fix the panicking cases to establish a working baseline:

1. Add missing cases to `Float.Add`, `Float.Subtract` for BigInteger, BigFloat
2. Add missing cases to `Complex.LessThan` for BigInteger, BigFloat
3. Add missing case to `Rational.Divide` for BigInteger
4. Change `return nil` to `panic(ErrDivisionByZero)` in BigInteger, BigFloat, BigComplex

**Tests**: Add tests for each fixed case before fixing.

### Phase 1: Infrastructure

Create `values/numeric_tower.go`:

1. Define `NumericRank` enum
2. Implement `Rank(Number) NumericRank`
3. Implement `Promote(Number, NumericRank) Number`
4. Implement `promoteOnce(Number) Number`
5. Implement `maybeSimplify(Number) Number`
6. Add comprehensive tests for promotion chains

**Deliverable**: Promotion infrastructure with 100% test coverage.

### Phase 2: Extend Number Interface

Update `values/values.go`:

```go
type Number interface {
    Value
    Add(Number) Number
    Subtract(Number) Number
    Multiply(Number) Number
    Divide(Number) Number
    Negate() Number        // NEW
    IsZero() bool
    IsExact() bool         // NEW (move from concrete types)
    LessThan(Number) bool
    Compare(Number) int    // NEW: -1, 0, 1
}
```

Implement missing methods on all types.

**Deliverable**: Complete Number interface with all methods on all types.

### Phase 3: Same-Type Operations

For each numeric type, add private same-type methods:

- `addSame(self) Number`
- `subtractSame(self) Number`
- `multiplySame(self) Number`
- `divideSame(self) Number`
- `compareSame(self) int`

These contain the actual arithmetic logic without type switches.

**Deliverable**: Each type has clean same-type operations.

### Phase 4: Unified Dispatch

Create `BinaryOp` dispatcher:

```go
func BinaryOp(a, b Number, op BinaryOperation) Number
```

Rewrite public methods to use dispatcher:

```go
func (p *Integer) Add(o Number) Number {
    return BinaryOp(p, o, OpAdd)
}
```

**Deliverable**: All arithmetic uses unified dispatch.

### Phase 5: Cleanup

1. Remove duplicate switch statements from all types
2. Remove redundant helper functions (e.g., `promoteToBigComplexPart`)
3. Update CLAUDE.md in values/ package
4. Add architecture documentation

**Deliverable**: Clean, minimal implementation.

## File Changes Summary

| File | Changes |
|------|---------|
| `values/numeric_tower.go` | NEW: Rank, Promote, BinaryOp, maybeSimplify |
| `values/values.go` | Extend Number interface |
| `values/integer.go` | Add same-type ops, simplify public methods |
| `values/big_integer.go` | Add same-type ops, simplify public methods, fix div-by-zero |
| `values/float.go` | Add missing cases (Phase 0), then same-type ops |
| `values/big_float.go` | Add same-type ops, fix div-by-zero |
| `values/rational.go` | Add missing BigInteger case, then same-type ops |
| `values/complex.go` | Add missing LessThan cases, then same-type ops |
| `values/big_complex.go` | Add same-type ops, fix div-by-zero |
| `values/numeric_tower_test.go` | NEW: Comprehensive promotion tests |

## Metrics

### Before

- Lines of switch-case code: ~600
- Type combinations handled: ~250 (with gaps causing panics)
- Files to modify for new type: 7

### After (projected)

- Lines of switch-case code: ~100 (in Rank, promoteOnce, maybeSimplify only)
- Type combinations handled: 49 (7×7, complete)
- Files to modify for new type: 2 (new type file + numeric_tower.go)

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

## Success Criteria

1. All existing tests pass
2. No panics on any valid type combination
3. Total switch-case code reduced by >70%
4. Adding a test new type (e.g., `Decimal`) requires <100 lines
5. Promotion rules are readable in one place

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

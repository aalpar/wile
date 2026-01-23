# Numeric Tower API

This document describes the unified numeric tower infrastructure in `go/values/numeric_tower.go`.

---

## Overview

The numeric tower provides a unified dispatch system for cross-type numeric operations. Instead of each type implementing 7 switch cases for every operation, the tower:

1. Promotes both operands to a common type
2. Performs the operation using same-type methods
3. Simplifies the result when possible

---

## Promotion Hierarchy

```
Integer < BigInteger < Rational < Float < BigFloat < Complex < BigComplex
         ↑ exact ↑              ↑ inexact ↑         ↑ complex ↑
```

**This ordering is a design decision, not an R7RS requirement.**

- R7RS §6.2.1 defines a mathematical *subtype* tower (number ⊃ complex ⊃ real ⊃ rational ⊃ integer), describing containment not promotion.
- R7RS §6.2.2 specifies exactness contagion (exact + inexact = inexact) but not which concrete type holds the result.
- R7RS §6.2.3 permits implementations to use any internal representations.

We choose this ordering because:
1. **Pragmatic precision**: Promoting to "wider" types preserves information
2. **Implementation simplicity**: Total ordering enables uniform dispatch
3. **Predictability**: Users can reason about result types

---

## API Reference

### Types

```go
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

type Exactness int

const (
    Exact Exactness = iota
    Inexact
)
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Rank` | `func(n Number) NumericRank` | Returns the position of a number in the tower |
| `Promote` | `func(n Number, target NumericRank) Number` | Promotes a number to the target rank |
| `PromoteBoth` | `func(a, b Number) (Number, Number)` | Promotes both numbers to their common rank |
| `Simplify` | `func(n Number) Number` | Reduces a number to simpler type when possible |
| `CommonRank` | `func(a, b Number) NumericRank` | Returns the higher rank of two numbers |
| `ExactnessOf` | `func(n Number) Exactness` | Returns `Exact` or `Inexact` |
| `ResultExactness` | `func(a, b Number) Exactness` | Computes exactness of binary operation result |
| `BinaryOp` | `func(a, b Number, op func(Number, Number) Number) Number` | Unified dispatch for binary operations |

### High-Level Operations

```go
func TowerAdd(a, b Number) Number      // a + b
func TowerSubtract(a, b Number) Number // a - b
func TowerMultiply(a, b Number) Number // a * b
func TowerDivide(a, b Number) Number   // a / b
func TowerCompare(a, b Number) int     // -1, 0, or 1
```

These handle all 49 (7×7) type combinations via promotion.

---

## Usage Examples

### Basic Tower Operations

```go
// Cross-type addition
result := values.TowerAdd(values.NewInteger(3), values.NewFloat(4.5))
// result: Float(7.5)

// Mixed exactness
result = values.TowerMultiply(values.NewRational(1, 2), values.NewInteger(4))
// result: Integer(2) - simplified from Rational(2/1)

// Complex arithmetic
result = values.TowerAdd(values.NewInteger(3), values.NewComplex(complex(1, 2)))
// result: Complex(4+2i)
```

### Using BinaryOp Directly

```go
// Custom operation with tower dispatch
result := values.BinaryOp(a, b, func(x, y values.Number) values.Number {
    // x and y are guaranteed to be the same type
    switch v := x.(type) {
    case *values.Integer:
        return v.addSame(y.(*values.Integer))
    // ... handle other types
    }
    panic("unreachable")
})
```

### Checking Exactness

```go
exact := values.NewInteger(5)
inexact := values.NewFloat(5.0)

values.ExactnessOf(exact)   // Exact
values.ExactnessOf(inexact) // Inexact

values.ResultExactness(exact, exact)     // Exact
values.ResultExactness(exact, inexact)   // Inexact
```

---

## Migration Guide

### Old Code (Direct Type Methods)

The existing `Add`, `Subtract`, `Multiply`, `Divide` methods on each type still work and are fully supported. They handle cross-type operations through large switch statements.

```go
// Old approach - still works
result := intVal.Add(floatVal)  // Integer.Add handles Float case
```

### New Code (Tower Functions)

For new code, prefer the tower functions for cleaner, more uniform dispatch:

```go
// New approach - cleaner
result := values.TowerAdd(intVal, floatVal)
```

### When to Use Which

| Scenario | Recommendation |
|----------|----------------|
| Existing code | No change needed |
| New arithmetic implementations | Use `TowerAdd`, `TowerSubtract`, etc. |
| Custom operations | Use `BinaryOp` with same-type handlers |
| Type-specific optimizations | Use direct methods (e.g., `Integer.Add`) |

### Migrating Existing Code

To convert old-style cross-type dispatch to tower-based dispatch:

**Before (in each type file):**
```go
func (p *Integer) Add(o Number) Number {
    switch v := o.(type) {
    case *Integer:
        return NewInteger(p.Value + v.Value)
    case *Float:
        return NewFloat(float64(p.Value) + v.Value)
    case *BigInteger:
        // ... 40+ lines of type handling
    }
    panic(ErrNotANumber)
}
```

**After (using tower):**
```go
func (p *Integer) Add(o Number) Number {
    return TowerAdd(p, o)
}
```

**Note:** This migration is optional. Both approaches work correctly.

---

## Same-Type Operations

Each numeric type implements private same-type methods used by the tower dispatch:

- `addSame(o *SameType) Number`
- `subtractSame(o *SameType) Number`
- `multiplySame(o *SameType) Number`
- `divideSame(o *SameType) Number`
- `compareSame(o *SameType) int`

These methods assume both operands are the same type and handle only that case.

---

## Simplification Rules

The `Simplify` function reduces numbers to simpler types when no information is lost:

| Input | Simplification |
|-------|---------------|
| BigComplex with zero imaginary | → real part (recursive) |
| Complex with zero imaginary | → Float → possibly Integer |
| BigFloat that is an integer | → BigInteger → possibly Integer |
| Float that is a whole number | → Integer |
| Rational with denominator 1 | → BigInteger → possibly Integer |
| BigInteger that fits int64 | → Integer |

---

## Error Handling

- Unknown types: `panic(ErrNotANumber)`
- Division by zero: `panic(ErrDivisionByZero)`

All 49 type combinations (7×7) are handled without panics for valid operations.

---

## Testing

The tower infrastructure is tested in:

- `numeric_tower_test.go` - Unit tests for Rank, Promote, Simplify, etc.
- `numeric_tower_coverage_test.go` - 245-case coverage matrix (7×7×5 operations)

Run tests:
```bash
cd go && go test -v ./values/ -run "TestTower|TestNumericTower"
```

---

## References

- R7RS §6.2.1 - Numerical types (tower definition)
- R7RS §6.2.2 - Exactness (contagion rules)
- R7RS §6.2.3 - Implementation restrictions
- `go/values/CLAUDE.md` - Package documentation
- `plans/NUMERIC_TOWER_REFACTOR.md` - Design rationale

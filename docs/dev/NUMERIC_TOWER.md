# Numeric Tower Architecture

This document describes the numeric tower implementation in `values/numeric_tower.go`.

**Status:** Stable (2026-02-05)

---

## Overview

The numeric tower uses **direct dispatch** — each numeric type's `Add`, `Subtract`, `Multiply`, `Divide`, and `Compare` methods handle all 7 incoming types via type switches. This is the intentional architecture.

### Why Direct Dispatch (Not Unified Tower)

A unified tower dispatch (`TowerAdd`, etc.) was prototyped but **abandoned** because:

1. **Exact complex bug**: Linear promotion (Integer → BigInteger → Rational → Float → Complex) loses exactness when combining exact reals with complex numbers
2. **Battle-tested code**: Direct dispatch has been tested across all 49 type combinations
3. **Explicit cases**: Each type switch case is explicit and debuggable

See "Why Direct Dispatch" section above for the decision rationale.

---

## Current API

### Utility Functions

```go
// Simplify reduces a number to simpler type when possible
func Simplify(n Number) Number

// Exactness classification
func ExactnessOf(n Number) Exactness        // Returns Exact or Inexact
```

### Exactness Type

```go
type Exactness int

const (
    Exact Exactness = iota
    Inexact
)
```

**Deleted (2026-02-05):** `NumericRank`, `Rank`, `Promote`, `PromoteBoth`, `CommonRank`, `BinaryOp`, `TowerAdd`, `TowerSubtract`, `TowerMultiply`, `TowerDivide`, `TowerCompare`.

---

## Type Promotion (Lattice Model)

Direct dispatch implements a **lattice** with two dimensions:

```
                    BigComplex
                   ↗    ↑    ↖
            Complex   BigFloat   (exact BigComplex path)
               ↑    ↗    ↑         ↑
             Float    Rational ────┘
               ↑        ↑
            Integer → BigInteger
```

### Result Type Matrix

| A ↓ / B → | Integer | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
|-----------|---------|------------|----------|-------|----------|---------|------------|
| **Integer** | Integer¹ | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
| **BigInteger** | BigInteger | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
| **Rational** | Rational | Rational | Rational | Float | BigFloat | Complex | BigComplex |
| **Float** | Float | Float | Float | Float | BigFloat | Complex | BigComplex |
| **BigFloat** | BigFloat | BigFloat | BigFloat | BigFloat | BigFloat | BigComplex | BigComplex |
| **Complex** | Complex | Complex | Complex | Complex | BigComplex | Complex | BigComplex |
| **BigComplex** | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex |

¹ Integer + Integer may overflow to BigInteger

### Exactness Preservation

| A ↓ / B → | Exact | Inexact |
|-----------|-------|---------|
| **Exact** | Exact | Inexact |
| **Inexact** | Inexact | Inexact |

Where:
- **Exact**: Integer, BigInteger, Rational, BigComplex (with exact parts)
- **Inexact**: Float, BigFloat, Complex, BigComplex (with inexact parts)

---

## Simplification Rules

`Simplify` reduces numbers to simpler types when no information is lost:

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

All types use consistent panic-based error handling:

- Unknown types: `panic(ErrNotANumber)`
- Division by zero: `panic(ErrDivisionByZero)`

All 49 type combinations (7×7) are handled without panics for valid operations.

---

## Testing

Coverage tests are in:

- `numeric_tower_coverage_test.go` — 245-case coverage matrix (7×7×5 operations)
- `numeric_lattice_test.go` — Lattice-based promotion model validation

Run tests:
```bash
go test -v ./values/ -run "TestNumericTower|TestLattice"
```

---

## References

- R7RS §6.2.1 — Numerical types (tower definition)
- R7RS §6.2.2 — Exactness (contagion rules)
- R7RS §6.2.3 — Implementation restrictions
- `values/CLAUDE.local.md` — Package documentation
- Design rationale: Unified tower dispatch was prototyped and abandoned (see "Why Direct Dispatch" above)

# CLAUDE.md

Package `helpers` provides shared utility functions for primitive implementations.

## Purpose

This package contains helper functions used across multiple primitive registration packages (core, extensions). By centralizing these utilities, we avoid code duplication and ensure consistent behavior.

## Key Files

| File | Purpose |
|------|---------|
| `numeric.go` | Variadic arithmetic helpers (fold, compare, extremum) |
| `integer.go` | Integer fold operations (gcd, lcm) and floor division |
| `equality.go` | eqv? semantics for memv/assv |
| `list.go` | List operations (list->vector, alist lookup) |
| `char.go` | Character comparison helpers (binary and variadic) |
| `string.go` | String comparison helpers (binary and variadic) |
| `value_conv.go` | Numeric type conversions (to complex128, to float64) |
| `type.go` | Type predicate factory function |

## Usage

Import this package in primitive implementation files:

```go
import "wile/registry/helpers"

// Use helpers in primitive implementations
func PrimAdd(ctx context.Context, mc *machine.MachineContext) error {
    return helpers.NumericFoldVariadic(mc, "+", values.NewInteger(0), func(a, b values.Number) values.Number {
        return a.Add(b)
    })
}
```

## Dependencies

This package only depends on:
- `wile/machine` - MachineContext for primitive execution
- `wile/values` - Scheme value types
- `wile/utils` - Boolean conversion utilities
- Standard library (`context`, `math`, `math/big`)

It does NOT depend on:
- Any extension packages
- Any circular imports

## Exported Functions

### Numeric Helpers

| Function | Description |
|----------|-------------|
| `NumericFoldVariadic(mc, name, identity, binOp)` | Fold variadic args with binary operation (for +, *) |
| `NumericFoldWithFirst(mc, name, unaryOp, binOp)` | Fold with required first arg (for -, /) |
| `NumericChainCompare(mc, name, fails)` | Chain comparison for =, <, >, <=, >= |
| `NumericExtremum(mc, name, isBetter)` | Find min/max with exactness contagion |
| `IsInexact(n)` | Check if number is inexact (Float, BigFloat, Complex) |
| `MaybeToInexact(n, hasInexact)` | Convert to inexact if needed |
| `IsNaN(n)` | Check if number is NaN float |

### Integer Helpers

| Function | Description |
|----------|-------------|
| `IntegerFold(mc, op, identity, combiner)` | Fold integers for gcd/lcm |
| `FloorDivide(n0, n1)` | Floor division returning quotient and remainder |
| `GcdInt(a, b)` | GCD of two int64 values |

### Equality Helpers

| Function | Description |
|----------|-------------|
| `Eqv(a, b)` | eqv? semantics for numeric/character comparison |

### List Helpers

| Function | Description |
|----------|-------------|
| `ListToVector(mc, name)` | Convert list argument to vector |
| `AssocLookup(mc, name, eq)` | Generic alist lookup with custom equality |

### Character Helpers

| Function | Description |
|----------|-------------|
| `CharCompare(mc, name, cmp)` | Binary character comparison |
| `CharCompareVariadic(mc, name, cmp)` | Variadic character comparison chain |

### String Helpers

| Function | Description |
|----------|-------------|
| `StringCompare(mc, name, cmp)` | Binary string comparison |
| `StringCompareVariadic(mc, name, cmp)` | Variadic string comparison chain |

### Value Conversion Helpers

| Function | Description |
|----------|-------------|
| `ToComplex128(v)` | Convert any Scheme number to Go complex128 |
| `ToFloat64(v)` | Convert real Scheme number to Go float64 |
| `ComplexOrFloat(c)` | Return Float if imaginary=0, else Complex |

### Type Helpers

| Function | Description |
|----------|-------------|
| `MakeTypePredicate(check)` | Factory for type predicate primitives |

## Gotchas

- **Exactness contagion**: NumericExtremum implements R7RS exactness rules
- **NaN handling**: NumericExtremum returns NaN if any argument is NaN
- **BigInteger support**: IntegerFold automatically switches to big.Int when needed
- **Empty list identity**: NumericFoldVariadic returns identity for empty args

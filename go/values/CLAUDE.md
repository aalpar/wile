# CLAUDE.md

Package `values` implements all Scheme runtime value types.

## Purpose

Complete R7RS value system including:
- Core types: integers, floats, rationals, complex, booleans, characters, strings, symbols
- Compound types: pairs, vectors, hash tables, records, byte vectors
- Advanced: promises, boxes, error objects
- I/O ports: string, binary, bytevector, character
- Concurrency: threads, mutexes, condition variables, channels

## Key Interfaces

**Value** - All 40+ types implement:
- `SchemeString()` - Scheme representation
- `IsVoid()` - Absence of value check
- `EqualTo(Value)` - Structural equality

**Number** - Numeric tower:
- `Add`, `Subtract`, `Multiply`, `Divide`
- `IsZero` - Check for zero
- Implemented by: Integer, Float, Rational, Complex, BigInteger, BigFloat, BigComplex
- `IsExact` - Exactness checks
- `IsNegative` - Sign checks
- `Negate` - Unary negation
- Any object that implements Number must not also implement `IsInexact`, `IsPositive`, etc. to avoid ambiguity
- All objects that implement `Number` must also implement `Value` and `Comparable`
- All `Number` types support cross-type arithmetic and comparison with other `Number` types

**Comparable** - Ordering:
- `CompareTo(Value) int` - Total ordering (-1,0,1)
- Implemented by: all numeric types, strings, characters, symbols
- Records compare by identity only
- Complex compares real parts only
- Bytevectors compare lexicographically
- Strings compare lexicographically
- Characters compare by Unicode code point
- Symbols compare by name lexicographically
- Booleans: `#f < #t`
- Others: no ordering
- Cross-type comparisons supported for all numeric types

## R7RS Numeric Tower Mapping

R7RS defines a hierarchy: `number ⊃ complex ⊃ real ⊃ rational ⊃ integer`

| R7RS Type | Go Type(s) | Exactness | Notes |
|-----------|------------|-----------|-------|
| integer | Integer, BigInteger | exact | Arbitrary precision via BigInteger |
| integer | Float, BigFloat | inexact | When `(= x (round x))`, e.g., `7.0` |
| rational | Rational | exact | Numerator/denominator as big.Rat |
| real | Float | inexact | IEEE 754 float64 |
| real | BigFloat | inexact | Arbitrary precision via big.Float (`#m` prefix) |
| complex | Complex | inexact | Real and imaginary as float64 |
| complex | BigComplex | exact or inexact | Real and imaginary as BigInteger or Rational (exact) or BigFloat (inexact) |

### Exactness Properties

| Type | `exact?` | `inexact?` | Notes |
|------|----------|------------|-------|
| Integer | #t | #f | Always exact |
| BigInteger | #t | #f | Always exact |
| Rational | #t | #f | Always exact |
| Float | #f | #t | Always inexact |
| BigFloat | #f | #t | Always inexact, arbitrary precision |
| Complex | #f | #t | Always inexact |
| BigComplex | varies | varies | Exact if both parts are BigInteger/Rational, inexact if either is BigFloat |

### Special Value Properties

| Type | `finite?` | `infinite?` | `nan?` | Notes |
|------|-----------|-------------|--------|-------|
| Integer | #t | #f | #f | Always finite |
| BigInteger | #t | #f | #f | Always finite |
| Rational | #t | #f | #f | Always finite |
| Float | varies | varies | varies | IEEE 754 supports ±inf, NaN |
| BigFloat | #t | #f | #f | big.Float has no Inf/NaN |
| Complex | varies | varies | varies | Follows Float rules |
| BigComplex | #t | #f | #f | Always finite (uses BigInteger/Rational/BigFloat) |

### Type Predicates

A value satisfies multiple predicates due to the tower hierarchy:
- `(integer? 5)` → #t, `(rational? 5)` → #t, `(real? 5)` → #t, `(complex? 5)` → #t
- `(integer? 5.0)` → #t (inexact integer), `(exact? 5.0)` → #f
- `(integer? 5.5)` → #f, `(real? 5.5)` → #t

**Tuple** - List/sequence protocol:
- `Car`, `Cdr`, `Append`, `ForEach`, `IsList`
- Implemented by: Pair, ArrayList, Vector

**SchemeWriter** - Cycle-aware output (R7RS §2.4, §6.13.3):
- Two-pass algorithm: pass 1 identifies shared/circular objects, pass 2 outputs with datum labels
- `WriteValueToString(v)` - Write with `#n=` and `#n#` labels for shared structures
- `DisplayValueToString(v)` - Display mode (no quotes on strings)
- Handles both pairs and vectors

## Optimizations

- **Integer cache**: -32768 to 32767 reused from global array
- **String interning**: Strings ≤64 chars interned via sync.Map
- **Singletons**: Void, EofObject, EmptyList, TrueValue, FalseValue

## Cross-Type Arithmetic

Integer arithmetic methods handle BigInteger operands automatically:

| Operation | Integer op BigInteger | Result Type |
|-----------|----------------------|-------------|
| `Add` | `5 + #z10000000000000000000` | BigInteger |
| `Subtract` | `5 - #z10000000000000000000` | BigInteger |
| `Multiply` | `5 * #z10000000000000000000` | BigInteger |
| `Divide` | `5 / #z10000000000000000000` | Rational |
| `LessThan` | `5 < #z10000000000000000000` | bool |

BigInteger division returns exact types when possible:

| Operation | Result |
|-----------|--------|
| `#z100 / #z10` | BigInteger (10) - exact division |
| `#z100 / #z3` | Rational (100/3) - inexact division |

## Numeric Tower Infrastructure

The `numeric_tower.go` file provides a unified dispatch system for cross-type numeric operations.

### Promotion Ordering

`NumericRank` defines the promotion hierarchy:

```
Integer < BigInteger < Rational < Float < BigFloat < Complex < BigComplex
```

This ordering ensures information preservation when promoting operands for mixed-type operations.

### Key Functions

| Function | Purpose |
|----------|---------|
| `Rank(n)` | Returns the NumericRank of a number |
| `Promote(n, target)` | Promotes a number to a higher rank |
| `PromoteBoth(a, b)` | Promotes both numbers to their common rank |
| `Simplify(n)` | Reduces a number to simpler type when possible |
| `BinaryOp(a, b, op)` | Unified dispatch: promote, operate, simplify |

### Tower Operations

High-level operations that use the tower dispatch:

```go
TowerAdd(a, b Number) Number      // a + b
TowerSubtract(a, b Number) Number // a - b
TowerMultiply(a, b Number) Number // a * b
TowerDivide(a, b Number) Number   // a / b
TowerCompare(a, b Number) int     // -1, 0, or 1
```

These handle all 49 (7×7) type combinations via promotion.

### Same-Type Operations

Each numeric type implements private same-type methods:

- `addSame`, `subtractSame`, `multiplySame`, `divideSame`, `compareSame`

These are used by the tower dispatch after promotion.

### Exactness

```go
ExactnessOf(n Number) Exactness       // Exact or Inexact
ResultExactness(a, b Number) Exactness // exact op exact = exact
```

## Cross-Type Comparison

All numeric types support `LessThan` comparison with all other numeric types:

| Type | Can Compare With |
|------|------------------|
| Integer | Integer, BigInteger, Float, BigFloat, Rational, Complex, BigComplex |
| BigInteger | Integer, BigInteger, Float, BigFloat, Rational, BigComplex |
| Float | Integer, BigInteger, Float, BigFloat, Rational, Complex, BigComplex |
| BigFloat | Integer, BigInteger, Float, BigFloat, Rational, BigComplex |
| Rational | Integer, BigInteger, Float, BigFloat, Rational, Complex, BigComplex |
| Complex | Integer, Float, Rational, Complex, BigComplex (compares real parts only) |
| BigComplex | All numeric types (compares real parts only for complex comparisons) |

## Gotchas

- **Void vs nil**: `nil` pointer distinct from `Void` singleton; `IsVoid()` checks both
- **Empty list**: Singleton `EmptyList` (Pair with both elements nil)
- **Integer caching boundary**: -32768 to 32767 only
- **String interning limit**: Only ≤64 character strings
- **Rational from division**: Integer / Integer produces Rational
- **BigInteger exact division**: BigInteger / BigInteger returns BigInteger if exact, Rational otherwise
- **Complex ordering**: `LessThan` compares only real parts
- **Record identity**: Records equal only if same object
- **Cross-thread continuations**: Not allowed (`ErrCrossThreadContinuation`)
- **Pair Car/Cdr are methods**: Use `pair.Car()` and `pair.Cdr()` with parentheses - they are methods, not fields

## Testing

Uses quicktest with custom `SchemeEquals` checker. Extensive type-specific and cross-type tests.

### Test File Organization

This package uses **1:1 mapping** with type-based consolidation for related types:

| Test File | Tests For |
|-----------|-----------|
| `integer_test.go` | Integer type |
| `float_test.go` | Float type |
| `big_number_test.go` | BigInteger and BigFloat types |
| `rational_test.go` | Rational type |
| `complex_test.go` | Complex type |
| `big_complex_test.go` | BigComplex type |
| `numeric_tower_test.go` | Numeric tower infrastructure |
| `pair_test.go` | Pair/cons cells |
| `string_test.go` | String type |
| `character_test.go` | Character type |
| `symbol_test.go` | Symbol type |
| `vector_test.go` | Vector type |
| `hashtable_test.go` | Hashtable type |
| `*_port_test.go` | Port types (string, bytevector, character) |
| `*_error_test.go` | Error types (foreign, native) |

When adding new value type tests, create `<typename>_test.go` or add to an existing related type's test file.

## References

See `BIBLIOGRAPHY.md` at project root for R7RS numeric tower specification (§6.2) and IEEE 754 floating-point standard.

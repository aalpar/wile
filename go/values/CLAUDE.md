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
- `IsZero`, `LessThan`
- Implemented by: Integer, Float, Rational, Complex, BigInteger, BigFloat, BigComplex

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

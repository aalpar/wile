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
- Implemented by: Integer, Float, Rational, Complex, BigInteger, BigFloat

**Tuple** - List/sequence protocol:
- `Car`, `Cdr`, `Append`, `ForEach`, `IsList`
- Implemented by: Pair, ArrayList, Vector

## Optimizations

- **Integer cache**: -32768 to 32767 reused from global array
- **String interning**: Strings ≤64 chars interned via sync.Map
- **Singletons**: Void, EofObject, EmptyList, TrueValue, FalseValue

## Gotchas

- **Void vs nil**: `nil` pointer distinct from `Void` singleton; `IsVoid()` checks both
- **Empty list**: Singleton `EmptyList` (Pair with both elements nil)
- **Integer caching boundary**: -32768 to 32767 only
- **String interning limit**: Only ≤64 character strings
- **Rational from division**: Integer / Integer produces Rational
- **Complex ordering**: `LessThan` compares only real parts
- **Record identity**: Records equal only if same object
- **Cross-thread continuations**: Not allowed (`ErrCrossThreadContinuation`)

## Testing

Uses quicktest with custom `SchemeEquals` checker. Extensive type-specific and cross-type tests.

# CLAUDE.md

Package `primitives` implements all R7RS Scheme built-in procedures.

## Purpose

224+ primitive implementations as Go foreign functions, organized by:
- One file per primitive: `prim_<name>.go`
- Helper modules for shared patterns
- Comprehensive test coverage

## Key Files

| File | Purpose |
|------|---------|
| `state.go` | Global I/O port state, weak-referenced caches |
| `numeric_fold.go` | Variadic arithmetic helpers (+, -, *, /) |
| `numeric_compare.go` | Comparison chain helpers (=, <, >, <=, >=) |
| `numeric_extremum.go` | min/max with exactness contagion and NaN handling |
| `integer_fold.go` | gcd/lcm with BigInteger support |
| `numeric_unary_ops.go` | Unary ops (abs, floor, ceiling, etc.) with BigInteger |
| `char_compare.go` | Character comparison predicates |
| `call_with_file.go` | File I/O wrapper with cleanup |
| `eqv.go` | Equality semantics for memv/assv |

## Primitive Implementation Pattern

```go
func PrimXxx(ctx context.Context, mc *machine.MachineContext) error {
    arg := mc.Arg(0)
    typed, ok := arg.(*values.SomeType)
    if !ok {
        return values.WrapForeignErrorf(values.ErrSomeError, "...")
    }
    // ... work ...
    mc.SetValue(result)
    return nil
}
```

## Registration

Primitives listed in `runtime/environment_tiny.go`:
```go
var runtimePrimitives = []PrimitiveSpec{
    {Name: "+", ParamCount: 0, IsVariadic: true, Impl: primitives.PrimAdd},
    // ...
}
```

## Gotchas

- **Variadic args as Pair**: Rest arguments passed as linked list in last parameter
- **Identity elements**: `(+)` returns 0, `(*)` returns 1
- **Append complexity**: Uses vector intermediate for O(n) despite linked lists
- **Continuation escape**: `call/cc` copies continuation, uses sentinel error
- **Arguments via mc.Arg()**: Not via environment frame bindings
- **Weak caching**: Tokenizers/parsers cached per port with weak pointers
- **Pair methods**: Use `pair.Car()` and `pair.Cdr()` - these are methods, not fields
- **Character `#\x` in tests**: Avoid `#\x` in test code as it starts a hex escape sequence; use `#\a` or other letters instead
- **Simple errors**: Use `values.NewForeignError("message")` for validation errors without predefined constants
- **String Unicode escapes**: Use `\xHEX;` format (R7RS) or embed Unicode directly; `\U` escape is not valid Scheme syntax
- **R7RS conformance tests**: Some tests verify R7RS behavior that is not yet implemented (e.g., variadic char-ci/string-ci comparisons). These tests are intentionally failing until the implementation is fixed—do not remove them

## Variadic Registration Patterns

The relationship between `ParamCount` and `IsVariadic` determines how arguments are passed:

| ParamCount | IsVariadic | Arg Access | Example |
|------------|------------|------------|---------|
| 1 | true | `mc.Arg(0)` = all args as Pair | `+`, `*` |
| 2 | true | `mc.Arg(0)` = first arg directly, `mc.Arg(1)` = rest as Pair | `-`, `/`, `char=?` |

For primitives requiring at least 2 arguments (like comparisons), use `ParamCount: 2, IsVariadic: true`.

## Testing

Uses quicktest with `runProgram()` and `runSchemeCode()` helpers. Table-driven tests cover operations, edge cases, and error conditions.

**Important**: Do not remove or revert tests that conform to R7RS. If a test fails but correctly reflects R7RS behavior, the implementation must be fixed to conform to R7RS—not the test.

## R7RS Numeric Tower

Numbers form a hierarchy where each level is a subset of the one above:

```
number (z)
  └─ complex (z)
       └─ real (x)
            └─ rational (q)
                 └─ integer (n)
```

**Critical**: Each level includes both **exact** and **inexact** representations:
- `7` is an exact integer
- `7.0` is an inexact integer (satisfies `integer?` because `(= 7.0 (round 7.0))`)
- Both are valid arguments where "integer" (n) is required

### Parameter Type Notation

| Symbol | Type | Go Types |
|--------|------|----------|
| **z** | complex (any number) | Integer, BigInteger, Float, BigFloat, Rational, Complex |
| **x** | real | Integer, BigInteger, Float, BigFloat, Rational |
| **q** | rational | Integer, BigInteger, BigFloat, Rational |
| **n** | integer (exact OR inexact) | Integer, BigInteger, Float (if integral), BigFloat (if integral) |
| **k** | exact integer only | Integer, BigInteger |

**Note**: BigFloat (`#m` prefix) is always inexact and always finite (no Inf/NaN support).

## R7RS Arithmetic Primitive Type Requirements

### Accept ANY number (z - complex)

| Procedure | Notes |
|-----------|-------|
| `+`, `-`, `*`, `/` | Basic arithmetic; `(+)` → 0, `(*)` → 1 |
| `sqrt` | Returns complex for negative reals |
| `expt`, `square` | Exponentiation |
| `exp`, `log`, `sin`, `cos`, `tan`, etc. | Transcendental functions |

### Accept REAL numbers only (x)

| Procedure | Notes |
|-----------|-------|
| `max`, `min` | Requires ≥1 arg; inexact contagion applies |
| `abs` | Absolute value |
| `floor`, `ceiling`, `truncate`, `round` | Return integers |
| `rationalize` | `(rationalize x tolerance)` |

### Accept INTEGERS only (n) - includes inexact integers like 7.0

| Procedure | Notes |
|-----------|-------|
| `quotient`, `remainder`, `modulo` | `(quotient 7.0 3.0)` → `2.0` is valid |
| `gcd`, `lcm` | `(gcd)` → 0, `(lcm)` → 1; always non-negative |
| `floor/`, `truncate/`, etc. | Return 2 values |

### Accept EXACT integers only (k)

| Procedure | Notes |
|-----------|-------|
| `exact-integer-sqrt` | Returns 2 exact integers; error on inexact input |

## Exactness Contagion Rules

1. **General rule**: Operations return inexact results when given any inexact arguments
2. **Exception**: May return exact if provably unaffected by inexactness
3. **Specific cases**:
   - `(* 0 x)` → exact 0 (exact zero dominates)
   - `(* 0.0 x)` → inexact 0.0
   - `(* 0 +inf.0)` → may return 0, +nan.0, or error (implementation-defined)
   - `(max 3 2.5)` → `3.0` (inexact, because 2.5 is inexact)
   - `(+ 1 1/2)` → `3/2` (exact)
   - `(+ 1 0.5)` → `1.5` (inexact)

## Helper Functions for R7RS Compliance

### Integer Extraction (`prim_quotient.go`)

```go
// extractInteger extracts an integer value from Integer, BigInteger, or Float (if integral).
// Returns (int64Value, bigIntValue, isInexact, error).
extractInteger(v values.Value, name string) (int64, *big.Int, bool, error)
```

Use for primitives accepting integers (n) - handles exact and inexact integers uniformly.

### Exactness Tracking (`numeric_extremum.go`)

```go
// isInexact returns true if the number is inexact (Float, BigFloat, or Complex)
isInexact(n values.Number) bool

// maybeToInexact converts exact to Float if hasInexact is true
maybeToInexact(n values.Number, hasInexact bool) values.Value
```

### BigFloat Integer Checking (`prim_odd_q.go`, `prim_even_q.go`)

For `odd?` and `even?` with BigFloat, use `v.BigFloatValue().IsInt()` to verify the value
represents an integer before checking parity.

Use for implementing exactness contagion in primitives like max/min.

## R7RS Normative Sources

| Source | URL |
|--------|-----|
| R7RS-small PDF | https://small.r7rs.org/attachment/r7rs.pdf |
| R7RS Corrected (HTML) | https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html |
| SRFI-141 (Integer Division) | https://srfi.schemers.org/srfi-141/srfi-141.html |
| Division Spec Wiki | https://small.r7rs.org/wiki/DivisionRiastradh/ |
| R7RS-large Wiki (in progress) | https://codeberg.org/scheme/r7rs/wiki |

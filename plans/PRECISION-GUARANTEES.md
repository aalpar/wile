# PRECISION GUARANTEES

Precision loss is a P1 conformance issue. Wile guarantees exact arithmetic for exact inputs unless the operation inherently produces irrational or non-representable results.

This document defines WHERE precision is guaranteed, WHERE loss is unavoidable, and WHERE loss is a bug.

## R7RS Foundation

R7RS §6.2.2 establishes the rules:

- **Exact operations on exact inputs produce exact results**, unless mathematically impossible.
- **Inexactness is contagious**: if any argument is inexact, the result is inexact.
- **Implementations SHOULD preserve exactness** and produce results as close as practical to the mathematically ideal result.
- When exact results are not possible, implementations may either **report an implementation restriction** or **produce an inexact result**.

## Guarantee Tiers

### Tier 1: EXACT — No Precision Loss Permitted

These operations on exact inputs MUST produce exact results. Precision loss here is a **P1 bug**.

| Operation | Types | Guarantee |
|-----------|-------|-----------|
| `+`, `-`, `*` | Integer × Integer | Exact (BigInteger on overflow) |
| `+`, `-`, `*` | Rational × Rational | Exact Rational |
| `+`, `-`, `*` | BigInteger × BigInteger | Exact BigInteger |
| `+`, `-`, `*` | BigComplex × BigComplex (exact parts) | Exact BigComplex |
| `/` | Integer / Integer | Exact Rational |
| `/` | Rational / Rational | Exact Rational |
| `quotient`, `remainder`, `modulo` | Integer × Integer | Exact Integer |
| `gcd`, `lcm` | Integer × Integer | Exact Integer |
| `numerator`, `denominator` | Rational | Exact Integer |
| `exact` | Any exact number | Identity (no conversion) |
| `exact` | Float/Complex | Exact rational equivalent |
| Comparison (`=`, `<`, `>`, `<=`, `>=`) | Exact × Exact | Exact comparison (no float64 intermediary) |

### Tier 2: INEXACT — Precision Loss Is Inherent

These operations inherently produce results that cannot be represented exactly. Precision loss is expected and conformant, but implementations SHOULD maximize available precision (prefer BigFloat over Float where possible).

| Operation | Why |
|-----------|-----|
| `sin`, `cos`, `tan`, `asin`, `acos`, `atan` | Transcendental — results are generally irrational |
| `exp`, `log` | Transcendental |
| `sqrt` (non-perfect-square) | Irrational result |
| `expt` (non-integer exponent) | Generally irrational |
| `atan2` | Transcendental (uses `math.Atan2`) |
| `magnitude` of complex (non-trivial) | Uses `sqrt(a² + b²)` |
| `angle` / `phase` of complex | Uses `atan2` |
| Constants: π, e | Irrational by definition |

### Tier 3: BOUNDARY — Precision Loss at System Edges

Precision loss at system boundaries (FFI, I/O, Go interop) is unavoidable when the target type cannot represent the full value. These are NOT bugs, but should be documented and controllable.

| Boundary | Types | Why |
|----------|-------|-----|
| FFI to Go (`float64` target) | BigInteger, Rational | Go's `float64` has 53-bit mantissa |
| FFI to Go (`complex128` target) | BigComplex | Go's `complex128` uses two `float64` values |
| `number->string` (decimal notation) | BigFloat, Rational | Finite decimal may not represent value exactly |
| `inexact` | Any exact type | Explicit user request to lose exactness |

### Tier 4: GUARD — Precision Loss in IEEE 754 Special-Value Handling

BigFloat and BigComplex cannot represent Inf/NaN. When Float(Inf/NaN) participates in arithmetic with these types, the dispatch guard falls back to float64/complex128.

| Condition | What Happens | Why Acceptable |
|-----------|-------------|----------------|
| `Float(Inf/NaN) op BigFloat` | Result is `Float(float64)` | Inf/NaN dominates real result; no exact BigFloat alternative exists |
| `Float(Inf/NaN) op BigComplex` | Result is `Complex(complex128)` (fix #362) | Inf/NaN dominates real part; imaginary part truncated to float64 because no complex type exists that combines Inf-capable real with arbitrary-precision imaginary |

**This is the ONLY place where "precision loss is acceptable" as a blanket statement.** The justification is structural: the type system cannot represent the result without loss, and the alternative (error/panic) would be worse for most use cases.

## Known Precision Bugs

Issues where precision is lost unnecessarily — these are P1 conformance bugs.

### `toExactPart`: Unnecessary float64 roundtrip

**File**: `values/big_complex.go:493`
**Bug**: Converts `BigFloat → float64 → big.Rat` instead of `BigFloat → big.Rat` directly.
**Impact**: 256-bit precision truncated to 53 bits before conversion to exact rational.
**Fix**: Use `v.value.Rat(nil)` directly instead of going through `.Float64()`.

### `makeInexact` / `numberToInexact`: Truncates to Float instead of BigFloat

**File**: `internal/parser/parser.go:1722-1727` (numberToInexact), `parser.go:1758-1761` (makeInexact)
**Bug**: BigInteger and Rational are converted to `Float` (float64) instead of `BigFloat` (256-bit).
**Impact**: Values larger than 2^53 or rationals with large denominators lose precision unnecessarily. The conversion IS intentional (exact→inexact), but the target precision is lower than necessary.
**Fix**: Convert BigInteger/Rational → BigFloat instead of Float. Both are inexact, but BigFloat preserves ~77 decimal digits vs Float's ~15.

## Proposed: Precision Control Setting

A runtime or engine-level setting to control precision loss behavior at Tier 3 and Tier 4 boundaries.

### Motivation

Different use cases have different tolerance for precision loss:
- **Scripting / exploratory**: Allow silent precision loss at boundaries (current behavior).
- **Financial / scientific**: Refuse to compute if precision would be lost; raise an implementation restriction error instead.
- **Conformance testing**: Strict mode that flags any non-Tier-2 precision loss.

### Proposed API

```go
type PrecisionMode int

const (
    // PrecisionPermissive allows silent precision loss at Tier 3/4 boundaries.
    // This is the default for backward compatibility.
    PrecisionPermissive PrecisionMode = iota

    // PrecisionStrict raises an implementation restriction error when
    // an operation would lose precision outside of Tier 2 (inherently inexact).
    // FFI calls to float64-target functions will error if the source value
    // exceeds float64 representable range.
    PrecisionStrict
)

// Engine option:
engine := wile.New(
    wile.WithPrecisionMode(wile.PrecisionStrict),
)
```

### Scope

| Tier | Permissive | Strict |
|------|-----------|--------|
| 1 (Exact) | Exact (no change) | Exact (no change) |
| 2 (Transcendental) | Inexact result | Inexact result |
| 3 (Boundary) | Silent truncation | Error: implementation restriction |
| 4 (Inf/NaN guard) | float64/complex128 fallback | Error: implementation restriction |

### Open Questions

1. Should `PrecisionStrict` also affect Tier 2 operations (e.g., refuse `(sqrt 2)` unless the user explicitly requests inexact)?
2. Should there be a `PrecisionWarn` mode that logs but doesn't error?
3. Should the setting be per-engine or per-call?
4. How does this interact with R7RS `parameterize`? Should it be a Scheme-level parameter?

## Audit Checklist

Sites to verify conform to the tier model:

- [ ] `values/promotion.go:numberToFloat64` — Tier 4, justified
- [ ] `values/promotion.go:numberToComplex128` (new, #362) — Tier 4, justified
- [ ] `values/big_complex.go:toExactPart` — **BUG** (unnecessary float64 roundtrip)
- [ ] `internal/parser/parser.go:numberToInexact` — **BUG** (Float instead of BigFloat)
- [ ] `internal/parser/parser.go:makeInexact` — **BUG** (Float instead of BigFloat)
- [ ] `registry/helpers/value_conv.go:ExtractReal` — Tier 3, verify callers use exactness bool correctly
- [ ] `registry/helpers/value_conv.go:ToFloat64` — Tier 3
- [ ] `registry/helpers/value_conv.go:ToComplex128` — Tier 3
- [ ] `ffi.go:convertArg` — Tier 3
- [ ] `extensions/math/prim_math.go` (transcendentals) — Tier 2, justified
- [ ] `values/big_complex.go:Phase` — Tier 2, justified (atan2)
- [ ] `values/big_complex.go:EqualTo` (BigComplex vs Complex) — Tier 3, justified (Complex is already float64)

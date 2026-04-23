# PRECISION GUARANTEES

Precision loss is a P1 conformance issue. Wile guarantees exact arithmetic for exact inputs unless the operation inherently produces irrational or non-representable results.

This document defines WHERE precision is guaranteed, WHERE loss is unavoidable, and WHERE loss is a bug.

## Algebraic Bias of Scheme Numerics

Scheme's numeric tower is biased toward algebra — symbolic manipulation and exact representation. Operations on exact inputs should produce exact results; types should preserve mathematical identity where possible. Wile maps this onto Go's concrete machine types, which creates a pragmatic boundary: operations within `int64` (< 2^53 for float64 interop), `float64`, and `complex128` are fast and precise within their domains. Operations that promote to `BigInteger`, `BigFloat`, `Rational`, or `BigComplex` leave the machine-optimized path.

This document's tier model reflects that boundary. Tier 1 (exact) and Tier 2 (inherently inexact) are the algebraic core. Tiers 3 and 4 are where machine-type limitations create precision gaps — gaps that are acknowledged, scoped, and (in the future) controllable via precision mode settings or machine-type constraint flags.

See [`tower.md`](tower.md) § "Design Philosophy" for the full architectural rationale.

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

### Tier 4: IEEE 754 Special Values in Arbitrary-Precision Types

`values.BigFloat` and `values.BigComplex` support IEEE 754 Inf and NaN representation. This eliminates the previous Inf/NaN guard paths in the dispatch table — operations stay in their domain, and the promotion lattice produces the correct result type regardless of special values.

| Operation | Result Type | Precision |
|-----------|------------|-----------|
| `Float(Inf/NaN) op BigFloat` | `BigFloat` (per lattice) | No precision loss — BigFloat represents Inf/NaN natively |
| `Float(Inf/NaN) op BigComplex` | `BigComplex` (per lattice) | No precision loss — imaginary part preserved exactly |
| `BigFloat(Inf) op BigFloat` | `BigFloat` | IEEE 754 rules apply |
| `BigComplex(Inf real) op BigComplex` | `BigComplex` | IEEE 754 rules apply to each component |

**No blanket precision loss is acceptable at this tier.** The type system can represent all results without domain switching. See [`tower.md`](tower.md) § "IEEE 754 Semantic Uniformity".

**Go `math/big.Float` limitations:** Go's `big.Float` supports Inf (`SetInf`) but not NaN. Wile's `values.BigFloat` extends beyond `big.Float` with internal state to track NaN. See #362 plan for implementation details.

## Known Precision Bugs

Issues where precision is lost unnecessarily — these are P1 conformance bugs.

### `toExactPart`: Unnecessary float64 roundtrip

**File**: `values/big_complex.go:379`
**Bug**: Converts `BigFloat → float64 → big.Rat` instead of `BigFloat → big.Rat` directly.
**Impact**: 256-bit precision truncated to 53 bits before conversion to exact rational.
**Fix**: Use `v.value.Rat(nil)` directly instead of going through `.Float64()`.

### `makeInexact` / `numberToInexact`: Truncates to Float instead of BigFloat

**File**: `internal/parser/parser_number.go:627` (`numberToInexact`), `internal/parser/parser_number.go:649` (`makeInexact`)
**Bug**: BigInteger and Rational are converted to `Float` (float64) instead of `BigFloat` (256-bit).
**Impact**: Values larger than 2^53 or rationals with large denominators lose precision unnecessarily. The conversion IS intentional (exact→inexact), but the target precision is lower than necessary.
**Fix**: Convert BigInteger/Rational → BigFloat instead of Float. Both are inexact, but BigFloat preserves ~77 decimal digits vs Float's ~15.

## Proposed: Precision Control Setting

A runtime or engine-level setting to control precision loss behavior at Tier 3 boundaries.

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
| 4 (IEEE 754 specials) | Normal lattice promotion (no precision loss) | Normal lattice promotion (no precision loss) |

### Open Questions

1. Should `PrecisionStrict` also affect Tier 2 operations (e.g., refuse `(sqrt 2)` unless the user explicitly requests inexact)?
2. Should there be a `PrecisionWarn` mode that logs but doesn't error?
3. Should the setting be per-engine or per-call?
4. How does this interact with R7RS `parameterize`? Should it be a Scheme-level parameter?

## Proposed: Machine-Type Constraint Flags

Orthogonal to `PrecisionMode`, a **machine-type constraint** would keep all numeric computation within Go's hardware-accelerated types, raising an implementation-restriction error rather than promoting to Big* types.

### Motivation

Some use cases (game scripting, real-time systems, embedded contexts) need predictable performance and bounded memory. Automatic promotion to `BigInteger`/`BigFloat`/`BigComplex` introduces heap allocation and unbounded computation cost. A constraint flag would guarantee that all arithmetic stays in the machine-type domain.

### Proposed API

```go
type NumericDomain int

const (
    // NumericDomainFull allows promotion to Big* types (default).
    NumericDomainFull NumericDomain = iota

    // NumericDomainMachine constrains arithmetic to int64, float64, complex128.
    // Operations that would promote to Big* types raise an implementation
    // restriction error instead.
    NumericDomainMachine
)

// Engine option:
engine := wile.New(
    wile.WithNumericDomain(wile.NumericDomainMachine),
)
```

### Behavior Under Machine Domain

| Scenario | Full (default) | Machine |
|----------|---------------|---------|
| Integer overflow (int64) | Promote to BigInteger | Error: implementation restriction |
| Exact division (Integer / Integer) | Rational | Error unless result is exact integer |
| Float × BigComplex | BigComplex (lattice promotion) | complex128 (stays in machine domain) |
| `(expt 2 1000)` | BigInteger | Error: implementation restriction |
| Reader literal `#z...` | BigInteger | Error at parse time |

### Interaction with #362

Under `NumericDomainFull` (default), the #362 fix extends BigFloat/BigComplex to support Inf/NaN natively, and the promotion lattice produces the correct result type without guards. Under `NumericDomainMachine`, BigComplex would never be constructed — all complex arithmetic stays in `complex128`, and Inf/NaN is handled by Go's native complex128 type.

### Status

Desired feature, not yet a priority. No implementation work planned.

## Audit Checklist

Sites to verify conform to the tier model:

- [ ] `values/big_float.go` — Tier 4: BigFloat must support Inf and NaN (IEEE 754 uniformity)
- [ ] `values/big_complex.go` — Tier 4: BigComplex must support Inf/NaN parts via BigFloat
- [ ] `values/promotion.go:isSpecialFloat` guard (line 318) — **REMOVE** after BigFloat Inf/NaN support (#362)
- [ ] `values/big_complex.go:toBigFloat` (line 99) — Tier 3 helper used by BigComplex for `.Float64()` conversions at the FFI boundary; verify no dispatch-path uses remain after #362
- [ ] `values/big_complex.go:toExactPart` (line 379) — **BUG** (unnecessary float64 roundtrip)
- [ ] `internal/parser/parser_number.go:numberToInexact` (line 627) — **BUG** (Float instead of BigFloat)
- [ ] `internal/parser/parser_number.go:makeInexact` (line 649) — **BUG** (Float instead of BigFloat)
- [ ] `registry/helpers/value_conv.go:ExtractReal` (line 96) — Tier 3, verify callers use exactness bool correctly
- [ ] `registry/helpers/value_conv.go:ToFloat64` (line 72) — Tier 3
- [ ] `registry/helpers/value_conv.go:ToComplex128` (line 28) — Tier 3
- [ ] `ffi.go` argument conversion (no single `convertArg` function; per-argument conversion is distributed across `buildFFISpec` and the call path it constructs) — Tier 3
- [ ] `extensions/math/prim_math.go` (transcendentals) — Tier 2, justified
- [ ] `values/big_complex.go:Phase` — Tier 2, justified (atan2)
- [ ] `values/big_complex.go:EqualTo` (BigComplex vs Complex) — Tier 3, justified (Complex is already float64)

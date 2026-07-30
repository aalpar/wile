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
| Comparison (`=`, `<`, `>`, `<=`, `>=`) | Exact × Inexact | Exact comparison. Comparison uses a **separate, lossless** table from arithmetic: neither operand is rounded to reach a common domain. See [`tower.md`](tower.md) § "Two Promotion Tables". |

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
| FFI to Go (`float64` target) | BigInteger, BigFloat, Rational | Go's `float64` has 53-bit mantissa |
| FFI to Go (`complex128` target) | BigComplex | Go's `complex128` uses two `float64` values |
| `number->string` | BigFloat | Finite decimal may not represent value exactly. Rational is *not* in this row: it prints as `N/D` and round-trips exactly. |
| `inexact` | Any exact type | Explicit user request to lose exactness |

The two FFI rows are **strict by default**: the converter raises `werr.ErrLossyConversion` rather than truncating silently, unless the engine was built with `wile.WithLossyConversionsAllowed()`. See [`tower.md`](tower.md) § "Conversion to Fixed-Precision Go Types" for the helper and primitive surface that reports the loss direction.

### Tier 4: IEEE 754 Special Values in Arbitrary-Precision Types

`values.BigFloat` and `values.BigComplex` support IEEE 754 Inf and NaN representation. The arithmetic dispatch tables retain an `isSpecialFloat` guard (`pkg/values/promotion.go` → `isSpecialFloat`, consumed by `makeArithmeticDispatch`) that short-circuits to `float64` / `complex128` arithmetic when a `Float` operand is Inf/NaN and the lattice LUB is `BigFloat`/`BigComplex`. The `LessThan` dispatch deliberately has no such guard.

The guard decides the result *kind*, so the lattice LUB is not what comes back:

| Operation | Result Type | Precision |
|-----------|------------|-----------|
| `Float(Inf/NaN) op BigFloat` | `Float` (guard overrides the `BigFloat` LUB) | No precision loss — the special value determines the result, so the extra mantissa carries nothing |
| `Float(Inf/NaN) op BigComplex` | `BigComplex` (guard computes in `complex128`, rewraps) | Imaginary part of the `BigComplex` operand is preserved rather than dropped to a `Complex` |
| `BigFloat(Inf) op BigFloat` | `BigFloat` | IEEE 754 rules apply |
| `BigComplex(Inf real) op BigComplex` | `BigComplex` | IEEE 754 rules apply to each component |

**No blanket precision loss is acceptable at this tier.** When the guard fires, the float64 short-circuit preserves IEEE 754 semantics (Inf/NaN propagation) and loses no information that was representable in the Float operand. See [`tower.md`](tower.md) § "IEEE 754 Semantic Uniformity".

**Go `math/big.Float` limitations:** Go's `big.Float` supports Inf (`SetInf`) but not NaN: a NaN-producing operation panics with `big.ErrNaN`. Wile's `values.BigFloat` extends beyond `big.Float` with an out-of-band `nan` flag (`NewBigFloatNaN`, `recoverNaN` in `pkg/values/big_float.go`). This shipped with issue #362; the guard was **updated** to preserve the BigComplex imaginary part rather than removed, and there is no open plan to remove it.

## Known Precision Bugs

None currently filed.

### Settled: exact→inexact targets `Float`, not `BigFloat`

`numberToInexact` and `makeInexact` (`pkg/parser/parser_number.go`, the `#i` reader prefix) convert BigInteger and Rational to `Float`, not to the 256-bit `BigFloat`. This was once filed here as a P1 bug on the ground that BigFloat preserves ~77 decimal digits against Float's ~15. It is **not** a bug, and the entry is retained only so the argument is not re-litigated:

- R7RS §6.2.6 sanctions the loss: `inexact` may return any inexact representation, and both types are inexact.
- `float64` is the system-wide inexact target. The runtime `exact->inexact` agrees, and the whole `inexact-accuracy` / `inexact-lossless?` family (`extensions/math/prim_conversion.go`) is defined *in terms of* the float64 boundary. Changing only the reader would split the two paths.
- BigFloat is sticky. Nothing demotes it per-op (see [`tower.md`](tower.md) § "Promotion Beyond Machine Types"), so silently minting one out of `#i` would put ordinary downstream arithmetic on the arbitrary-precision path indefinitely. That is the same mistake exact × `Float` → `BigFloat` promotion made before it was reverted.

A caller who wants the extra precision asks for it explicitly with a `#m` literal, which routes through the BigFloat rows of the promotion table and is preserved.

## Proposed: Precision Control Setting

**Not implemented.** No `PrecisionMode` type and no `WithPrecisionMode` option exist in the tree; the sketch below is a proposal. What *did* ship for Tier 3 is narrower and orthogonal: the strict-by-default float64 / complex128 conversion boundary described in [`tower.md`](tower.md) § "Conversion to Fixed-Precision Go Types", relaxed per engine by `wile.WithLossyConversionsAllowed()`.

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

Issue #362 shipped: BigFloat and BigComplex support Inf/NaN natively (see Tier 4). Under `NumericDomainMachine`, BigComplex would never be constructed — all complex arithmetic stays in `complex128`, and Inf/NaN is handled by Go's native complex128 type.

### Status

**Not implemented.** No `NumericDomain` type and no `WithNumericDomain` option exist in the tree. Desired feature, not yet a priority; no implementation work planned.

## Audit Checklist

Sites to verify conform to the tier model:

Sites are pinned as `file` + symbol, never `file:LINE` — a line number rots into a *wrong* answer rather than a dead link. When a concern has no single owning symbol (e.g. distributed logic), the file alone is cited with explanatory text.

- [x] `pkg/values/big_float.go` — Tier 4: BigFloat must support Inf and NaN (IEEE 754 uniformity). **DONE** (#362): `NewBigFloatNaN` plus the out-of-band `nan` flag; `recoverNaN` converts `big.ErrNaN` panics into BigFloat NaN.
- [x] `pkg/values/big_complex.go` — Tier 4: BigComplex must support Inf/NaN parts via BigFloat. **DONE** (#362): `(*BigComplex).IsNaN` delegates to each part.
- [x] ~~`pkg/values/promotion.go` `isSpecialFloat` guard — **REMOVE** after BigFloat Inf/NaN support (#362)~~ — **RETAINED, deliberately.** #362 updated the guard instead of deleting it, so the BigComplex LUB rewraps its `complex128` result as a BigComplex and keeps the operand's imaginary part. See Tier 4 above.
- [ ] `pkg/values/promotion.go` `NumberToFloat64` — Tier 3 helper for lossy `float64` conversion at machine/FFI boundaries; verify all precision-dropping call paths are intentional
- [ ] `pkg/values/promotion.go` `NumberToComplex128Lossy` — Tier 3 helper for lossy `complex128` conversion at machine/FFI boundaries; verify all precision-dropping call paths are intentional
- [x] ~~`pkg/values/big_complex.go` `toExactPart`~~ — **FIXED**. `toExactPart` delegates to `(*BigFloat).ToExact`, which calls `p.value.Rat(nil)` directly (`pkg/values/big_float.go`); no `.Float64()` roundtrip remains.
- [x] ~~`pkg/parser/parser_number.go` `numberToInexact` / `makeInexact` — **BUG** (Float instead of BigFloat)~~ — **NOT A BUG.** See "Settled: exact→inexact targets `Float`" above.
- [ ] `pkg/registry/helpers/value_conv.go` `ExtractReal` — Tier 3, verify callers use exactness bool correctly
- [ ] `pkg/registry/helpers/value_conv.go` `ToFloat64` / `ToFloat64Lossy` — Tier 3
- [ ] `pkg/registry/helpers/value_conv.go` `ToComplex128` — Tier 3
- [ ] `pkg/wile/ffi.go` argument conversion — Tier 3. No single `convertArg` function; per-argument conversion is distributed across `buildFFISpec` and the converters it selects (`pkg/wile/ffi_arg_converters.go`, `pkg/wile/ffi_ret_converters.go`).
- [ ] `extensions/math/prim_transcendental.go` — Tier 2, justified
- [ ] `pkg/values/big_complex.go` `(*BigComplex).Phase` — Tier 2, justified (atan2)
- [ ] `pkg/values/big_complex.go` `(*BigComplex).EqualTo` (BigComplex vs Complex) — Tier 3, justified (Complex is already float64)

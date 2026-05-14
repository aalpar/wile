# Numeric loss signals — design

**Date**: 2026-05-14
**Status**: Design draft — implementation deferred until the
  numeric-registry Phase 3 PRs (#750 + follow-ups) merge.
**Triggered by**: Crosscheck of PR #750 surfaced `Q-i` — FFI's
  `float64` path under Q-c=C3 (the conservative resolution) keeps
  rejecting `*BigFloat` and `*BigComplex` inputs with a *generic*
  error (`ErrNotAReal`). The current behavior is "loud but
  imprecise": every BigFloat → float64 errors, even when the
  BigFloat actually fits losslessly in float64. The design
  retracted Q-i=C1 (silent precision loss) precisely because it
  contradicted Wile's "fail loud at startup" discipline. This
  plan fills the gap on the other side: detect actual precision
  loss at the conversion boundary, error precisely when it
  happens, succeed silently when it doesn't.
**Priority**: Medium — the existing FFI behavior is correct
  (rejects rather than truncates), so this is an *improvement*
  rather than a bug-fix. Ships after Phase 3 closes.

## Motivation

Go's `math/big` package returns precision-loss signals on every
conversion path Wile uses, and Wile discards them universally:

| Source | Stdlib API | Returns | Wile discards via |
|--------|-----------|---------|---------------------|
| `*big.Float` | `Float64() (f float64, acc big.Accuracy)` | `Below` / `Exact` / `Above` | `f, _ := bigFloat.Float64()` — 12+ sites |
| `*big.Rat` | `Float64() (f float64, exact bool)` | `bool` (true = lossless) | `f, _ := rat.Float64()` — 6+ sites |
| `*big.Int` | `Float64() (f float64, acc big.Accuracy)` | same enum | `f, _ := bigInt.Float64()` — 6+ sites |
| `int64` | `n == int64(float64(n))` round-trip | inferred bool | **Used** at `ffi_arg_converters.go:50-53` (only loss-detecting site in the codebase) |
| float64 IEEE specials | `math.IsInf(f, 0)` / `math.IsNaN(f)` | bool | Used in `values/promotion.go:319` (`isSpecialFloat`) |

Verification: `git grep big.Accuracy` returns **zero hits** anywhere
in the codebase, including tests. The information is uniformly
available and uniformly thrown away.

The cost of this silent discarding is well-documented in this
plan series:

- The numeric-registry design (`plans/2026-05-14-numeric-registry-design.md`)
  champions "fail loud at startup" (line 281): *"validateNumericRegistry
  is a startup assertion: forget the spec, the program panics before
  serving traffic, not silently months later."*
- The errors-lens crosscheck (PR #750) flagged Q-c=C1 as a violation
  of that principle: a BigFloat carrying `2^100` would convert to
  `+Inf` and the `+Inf` flows downstream, producing wrong results
  far from the precision-loss site. Q-c was resolved C3 (conservative
  — keep rejecting) to avoid introducing the silent path.
- The Q-i=C3 resolution preserves correctness today but leaves the
  *better* behavior unrealized: a BigFloat that fits in float64
  should succeed at the FFI boundary. Today it errors.

This plan proposes the infrastructure to detect precision loss at
the conversion boundary, error precisely when it occurs, and
succeed silently when it doesn't. It also surfaces the signal to
Scheme via dedicated primitives so Scheme programs can ask "would
converting this number to inexact be lossless?" without learning
the answer the hard way.

## Goals

1. **Detection at the boundary.** Every conversion site that today
   discards `big.Accuracy` or the `*big.Rat` `exact` bool surfaces
   the signal to its caller, who can then act on it.
2. **Typed sentinel.** A `werr.ErrLossyConversion` sentinel so
   callers can `errors.Is` against precision-loss specifically (vs.
   the generic `ErrNotAReal` which today subsumes both "wrong type"
   and "right type but doesn't fit").
3. **Scheme-visible signal.** At least one Scheme primitive that
   answers "would `(exact->inexact n)` be lossless?" without
   actually performing the conversion. Optionally a second
   primitive that performs the conversion *and* returns the
   accuracy.
4. **FFI tightening.** The FFI `float64`-target converter accepts
   BigFloat/BigComplex inputs *when they fit* (precision-loss = no);
   continues to reject them with a precise sentinel when they don't.
   Today: rejects all BigFloat regardless of magnitude. Future:
   rejects only when the conversion would lose precision.

## Non-goals

- **Tracking accuracy through arithmetic.** The numeric dispatch
  tables (per-type `[numKinds]func` arrays) propagate `Number`
  values, not `(Number, big.Accuracy)` pairs. Threading accuracy
  through arithmetic would require restructuring every arithmetic
  closure, with corresponding hot-path implications. Out of scope.
- **Changing R7RS-mandated lossy behavior.** R7RS §6.2.6 specifies
  `(exact->inexact (* 10 (expt 10 100)))` returns `+inf.0`, not an
  error. This plan adds *companion* primitives that report loss
  signals; it does not modify `exact->inexact`.
- **`complex128` precision loss for arithmetic-of-Complex types.**
  The Complex case has its own structure (real + imaginary loss
  independent); deferred to a future plan unless demand emerges.
- **Subnormal/denormal precision edge cases beyond what `big.Accuracy`
  reports.** The stdlib's enum captures the relevant signal; we
  don't invent finer granularity.

## Proposed Go-side API

### New sentinel

```go
// werr/werr.go
var ErrLossyConversion = NewStaticError("lossy conversion")
```

Used by the new helpers when a conversion succeeds *mechanically*
but loses precision. Distinct from `ErrNotANumber` (wrong type
entirely) and `ErrNotAReal` (right family, wrong subset).

### New helpers in `values/`

```go
// values/conversion.go (new file, or extension of promotion.go)

// ToFloat64Lossless converts a Number to float64; returns
// ErrLossyConversion (wrapped) if any precision is lost. Returns
// ErrNotANumber for non-Number values.
//
// Lossless means: the returned float64, converted back to the
// original kind, would equal the original value. For *Integer,
// this is round-trip equality. For *big.Float and *big.Int, this
// is big.Accuracy == big.Exact. For *Rational, this is the
// exact bool returned by (*big.Rat).Float64. For *Float, always
// lossless (identity). For *Complex/*BigComplex with non-zero
// imaginary part, always lossy (the imaginary part is dropped).
//
// Callers that want to allow lossy conversions should call
// ToFloat64Lossy instead.
func ToFloat64Lossless(n Number) (float64, error) {
    // dispatch by Kind, route to per-kind helper
}

// ToFloat64Lossy always converts to float64 and reports whether
// the conversion was lossless. Never returns an error for in-family
// inputs (Number). The bool is true iff no precision was lost.
//
// Callers that want a hard error on loss should call
// ToFloat64Lossless.
func ToFloat64Lossy(n Number) (f float64, lossless bool) {
    // dispatch by Kind, route to per-kind helper
}

// Float64Accuracy combines the result and the three-valued accuracy
// for callers that need the direction of loss (Below/Exact/Above).
// For non-BigFloat sources, accuracy is synthesized: Below if the
// true value exceeds the float64 representation, Above if it falls
// short, Exact if lossless. *Complex/*BigComplex with non-zero
// imaginary parts: accuracy reports the real-part accuracy and
// always-non-zero imaginary contributes a Below|Above flag — see
// below.
type Float64Accuracy struct {
    Value      float64
    Accuracy   big.Accuracy   // Below / Exact / Above
    Real       bool           // false if imaginary part was dropped (Complex/BigComplex)
}

func ToFloat64WithAccuracy(n Number) (Float64Accuracy, error) {
    // The richer return; used by primitives that surface accuracy
    // to Scheme.
}
```

**Why three forms?**
- `ToFloat64Lossless` is the **FFI-callable** form. FFI sites want
  to error on loss; this returns a typed error.
- `ToFloat64Lossy` is the **explicit-lossy** form (e.g., R7RS
  `exact->inexact` which must succeed). Returns the bool for
  callers that want to log or surface the signal.
- `ToFloat64WithAccuracy` is the **diagnostic** form. Used by the
  Scheme primitive `inexact-with-accuracy` that returns the
  accuracy symbol.

All three share the same per-kind dispatch logic; the difference
is what they do with the lossless bit. Implementation pattern:
each `ToFloat64Lossy` does the work; `Lossless` and
`WithAccuracy` wrap it.

### Per-kind dispatch

```go
// values/integer.go
func (p *Integer) toFloat64Lossy() (float64, bool) {
    f := float64(p.Value)
    return f, p.Value == int64(f)  // round-trip check
}

// values/big_integer.go
func (p *BigInteger) toFloat64Lossy() (float64, bool) {
    f, acc := new(big.Float).SetInt(p.value).Float64()
    return f, acc == big.Exact
}

// values/big_float.go
func (p *BigFloat) toFloat64Lossy() (float64, bool) {
    f, acc := p.value.Float64()
    return f, acc == big.Exact
}

// values/rational.go
func (p *Rational) toFloat64Lossy() (float64, bool) {
    return p.value.Float64()  // stdlib already returns (float64, bool)
}

// values/float.go
func (p *Float) toFloat64Lossy() (float64, bool) {
    return p.Value, true  // identity, always lossless
}

// values/complex.go
func (p *Complex) toFloat64Lossy() (float64, bool) {
    return real(p.Value), imag(p.Value) == 0
}

// values/big_complex.go
func (p *BigComplex) toFloat64Lossy() (float64, bool) {
    realF, realAcc := toBigFloat(p.real).Float64()
    return realF, realAcc == big.Exact && p.imag.IsZero()
}
```

The methods are unexported and accessed via the registry (post
Phase 3 — see "Integration with numeric registry" below).

## Proposed Scheme-side API

### Primitives

```scheme
;; (wile numerics) extension — new library or extends (scheme inexact)
;; depending on where the numeric-registry phasing lands these.

(inexact-lossless? n)
  ;; Returns #t if (exact->inexact n) would be lossless, #f otherwise.
  ;; For (*Complex) and (*BigComplex) with non-zero imaginary part,
  ;; returns #f (the imaginary part would be silently dropped if
  ;; converted via real-part-only semantics).
  ;; Predicate: does not allocate the inexact representation.

(inexact-with-accuracy n)
  ;; Returns three values: inexact-n accuracy-sym real?
  ;;   inexact-n   the inexact representation
  ;;   accuracy    one of: 'below 'exact 'above
  ;;   real?       #t if the result represents the full value
  ;;               (#f if the imaginary part of a complex was dropped)
  ;; Implementation: routes through ToFloat64WithAccuracy on values/.
  ;; Wraps the float64 result back to a Scheme inexact (Float or BigFloat
  ;; depending on policy — see Q-1 below).
```

Accuracy symbol values (Scheme-side):
- `'below` — `float64 < true value` (truncated downward)
- `'exact` — `float64 == true value`
- `'above` — `float64 > true value` (truncated upward / overflowed to `+inf`)

These mirror `big.Accuracy` constants. The symbol names match Wile's
established singleton-symbol convention (`SymbolMutexNotOwned`, etc.).

### Example use

```scheme
(define big (expt 10 100))                           ; exact integer
(inexact-lossless? big)                              ; => #f
(inexact-with-accuracy big)                          ; => +inf.0, 'above, #t

(inexact-lossless? 1/3)                              ; => #f (1/3 is not float64-representable)
(inexact-with-accuracy 1/3)                          ; => 0.3333333333333333, 'below, #t

(inexact-lossless? 7)                                ; => #t
(inexact-with-accuracy 7)                            ; => 7.0, 'exact, #t

(inexact-lossless? 3+4i)                             ; => #f (imaginary dropped)
(inexact-with-accuracy 3+4i)                         ; => 3.0, 'exact, #f
```

### FFI integration

```go
// ffi_arg_converters.go (PR 2 of the loss-signals impl)
case reflect.Float64:
    targetType := t
    return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
        n, ok := v.(values.Number)
        if !ok {
            return reflect.Value{}, fmtArgError(name, pos, "number", v)
        }
        f, err := values.ToFloat64Lossless(n)
        if err != nil {
            return reflect.Value{}, werr.WrapForeignErrorf(
                err,  // ErrLossyConversion or ErrNotANumber
                "%s: argument %d: %T cannot fit float64", name, pos, v,
            )
        }
        return reflect.ValueOf(f).Convert(targetType), nil
    }, nil
```

**Behavior change vs. today:**
- Before: any `*BigFloat`, `*BigComplex`, `*Complex` rejected unconditionally with `ErrNotAReal`.
- After: `*BigFloat` accepted when it fits losslessly; rejected with `ErrLossyConversion` otherwise. `*Complex` accepted when imaginary part is zero AND real part fits losslessly. `*BigComplex` analogous.

This is a **widening** of accepted inputs — strictly more permissive
than today. No existing FFI caller that succeeds today will fail
under this change.

### Opt-out: allow-lossy-conversions option

For FFI callers that *want* the legacy silent-truncation behavior
(rare; mostly numerical/scientific code that knows it's already
inexact):

```go
err := eng.RegisterFunc("my-numerical-routine", fn,
    wile.WithLossyConversionsAllowed())
```

Implementation: a per-function flag on the FFI spec that selects
`ToFloat64Lossy` instead of `ToFloat64Lossless`. Default is the
strict path.

## Integration with the numeric registry

Phase 3 of values-SR delivers a `NumericTypeSpec` registry whose
`ToFloat64` field — under Q-i=C3 — covers only the 5 reducible
kinds (Integer, BigInteger, Float, BigFloat, Rational). BigComplex
and Complex have nil `ToFloat64` slots (or the field is omitted
for those kinds).

This plan extends the registry with a parallel `ToFloat64Lossy`
field that is **always populated** for all 7 kinds:

```go
type NumericTypeSpec struct {
    // ... existing fields ...

    // ToFloat64Lossy converts the value to float64 and reports
    // whether the conversion was lossless. Always non-nil for
    // every kind. Lossy conversions (BigFloat overflow, BigComplex
    // with non-zero imaginary part) return lossless=false; the
    // float64 is best-effort (real part for complex; saturated
    // ±Inf for overflow).
    ToFloat64Lossy func(Number) (float64, bool)
}
```

The new field is additive — doesn't disturb the C3 resolution of
`ToFloat64`. Consumers wanting the strict-or-error semantics call
`ToFloat64Lossless(n)` (the values/ helper); those wanting the
silent path call `Lookup(n.Kind()).ToFloat64Lossy(n)` directly.

`ToFloat64` (the C3 5-kind field) continues to exist for the cold
paths that don't care about precision — `helpers.ToFloat64` and
`extensions/math/prim_conversion.go` retain their current
behavior. The widened API (`ToFloat64Lossless`) is opt-in.

## Phasing

### Phase 1 — Go infrastructure (1 PR)

1. Add `werr.ErrLossyConversion` sentinel to `werr/werr.go`.
2. Add per-kind `toFloat64Lossy()` methods on each numeric type
   (7 files, ~5 LOC each).
3. Add `values.ToFloat64Lossless`, `values.ToFloat64Lossy`,
   `values.ToFloat64WithAccuracy` exported helpers in `values/`.
4. Register the `ToFloat64Lossy` field on every spec in the numeric
   registry (extends Phase 3 PR 1's per-type init blocks).
5. Tests: round-trip per kind, boundary cases (MaxFloat64,
   subnormals, 2^100), R7RS-required behavior preserved
   (`exact->inexact` still always succeeds).

Estimated: ~250 LOC across 9 files. No behavior change anywhere
outside the new helpers.

### Phase 2 — FFI integration (1 PR)

1. Replace `ffi_arg_converters.go:76-96` (current `reflect.Float64`
   case) with `ToFloat64Lossless` call.
2. Add `wile.WithLossyConversionsAllowed()` engine option.
3. Tests: widening behavior verified (BigFloat that fits now
   succeeds); narrowing behavior verified (BigFloat with `2^100`
   still errors but now with `ErrLossyConversion` instead of
   `ErrNotAReal`). Opt-in tested.

Estimated: ~80 LOC. One documented behavior change (the widening
above).

### Phase 3 — Scheme primitives (1 PR)

1. Implement `inexact-lossless?` and `inexact-with-accuracy` in
   `extensions/math/prim_conversion.go` (alongside `exact->inexact`).
2. Use the `WithSingleResult` / multiple-value-return pattern for
   `inexact-with-accuracy` (consult `prim_misc_test.go` for the
   established pattern).
3. Docstrings with R7RS-style entries (`Parameters:`, `Returns:`,
   `Category: Numbers — Conversion`, plus a `Keywords:` field for
   discovery).
4. Tests: every kind, every accuracy outcome, both primitives.
   Integration test calling the primitives from a Scheme program.

Estimated: ~150 LOC. No existing-primitive behavior change.

### Phase 4 (optional) — `complex128` parallel

If demand emerges, the same pattern extends to `complex128`
conversion (`reflect.Complex128` FFI path; potential Scheme
primitive `complex-lossless?`). Deferred; not implemented unless
a concrete use case appears.

## R7RS conformance check

- **`exact->inexact`** must remain lossy-but-successful per R7RS
  §6.2.6. Unchanged by this plan. The new primitives are *added*
  alongside.
- **`inexact->exact`** is unaffected (direction is from inexact to
  exact, where loss is well-defined — convert to nearest exact
  representation).
- The new primitives are not in R7RS-small. They go under Wile's
  documented extension namespace (`(wile numerics)` or similar);
  R7RS-strict programs that don't import them are unaffected.

## Open questions

**Q-1 — `inexact-with-accuracy` result type.** R7RS-large
`exact->inexact` returns `*Float` (single-precision float64). For
a BigFloat input that fits in float64 lossless, returning `*Float`
is correct. For one that doesn't fit, options:
  - **a (recommended)**: return `+inf.0` / `-inf.0` (`*Float`) with
    accuracy `'above` / `'below`. Matches `exact->inexact`'s lossy
    fallback. Result type uniformly `*Float`.
  - **b**: return a `*BigFloat` for the overflowing case so the
    full magnitude survives. Result type union (`*Float` or
    `*BigFloat`).

Option (a) is simpler and matches stdlib `Float64()` saturation.
Option (b) is more useful to callers but complicates the type
contract. Defer to user resolution at impl time.

**Q-2 — Primitive naming.** Three candidates:
  - `(inexact-lossless? n)` — matches `inexact?` predicate idiom.
  - `(exact->inexact-lossless? n)` — matches `exact->inexact` name.
  - `(inexact/lossless? n)` — slash-separated (per some SRFIs).

Recommend `(inexact-lossless? n)` for brevity; matches the
naming of `IsLossless` helpers on the Go side and keeps the
hyphen-naming convention.

**Q-3 — Library placement.** Three candidates:
  - `extensions/math/` — alongside `exact->inexact`. Already loaded
    by default in most profiles.
  - New `(wile numerics)` library — clean scope; new top-level.
  - `extensions/wile/numerics/` — namespaced under Wile-specific.

Recommend `extensions/math/` for minimum surface change; the
primitives are naturally adjacent to `exact->inexact`.

**Q-4 — FFI opt-in mechanism.** Three candidates:
  - **a (recommended)**: `wile.WithLossyConversionsAllowed()`
    engine option — applies globally to all FFI functions
    registered after the option is set.
  - **b**: per-function `wile.RegisterFuncWithOptions(name, fn,
    LossyAllowed)` — fine-grained, more API surface.
  - **c**: per-parameter — struct tag or function-signature flag
    (e.g., a custom `wile.LossyFloat64` named type). Most precise,
    largest API expansion.

Recommend (a) for v1 simplicity; (b) and (c) deferred.

**Q-5 — Should `helpers.ToFloat64` (in `registry/helpers/`)
also tighten?** Today rejects Complex/BigComplex with
`ErrNotAReal`. The widening would let it succeed for Complex with
zero imaginary part that fits losslessly. Probably yes — same
discipline — but it's a slightly larger blast radius (more
consumers). Resolve at Phase 2 design.

## Risks

| # | Risk                                                            | Mitigation                                                                  |
|---|-----------------------------------------------------------------|-----------------------------------------------------------------------------|
| R1 | FFI widening breaks a caller who relied on BigFloat rejection   | Behavior is **strictly more permissive** — no successful call becomes a failure. Conversely: any code that today catches the error and supplies a default will now skip that branch. Document in release notes. |
| R2 | `big.Accuracy` exposure leaks Go stdlib detail into Wile's public Go API | Restrict `big.Accuracy` to internal helpers; exported APIs use bool (`lossless`) or a Wile-typed enum if richer signal is needed publicly. |
| R3 | Subnormal / boundary precision edge cases produce surprising signals | `big.Accuracy` is the authoritative IEEE 754 signal; trust the stdlib. Tests cover boundaries. |
| R4 | Scheme primitive names clash with hypothetical future R7RS additions | Wile namespace primitives go under `(wile ...)` if needed; current proposal uses unadorned names but they're R7RS-large-compatible. |
| R5 | Implementation-effort drift: adding loss-signal infrastructure to per-kind dispatch tempts further "what about arithmetic loss?" expansion | Stay scoped: this plan covers *conversion* boundaries only. Arithmetic-internal loss is a separate plan (not yet drafted). |
| R6 | `ToFloat64WithAccuracy` is over-engineered if no Scheme caller needs the three-valued enum | Implement only `Lossless` + `Lossy` in Phase 1; defer `WithAccuracy` until Phase 3's primitive demand is real. |

## Done definition

Phase 1 done when:
- `werr.ErrLossyConversion` defined.
- `values.ToFloat64Lossless`, `ToFloat64Lossy`, `ToFloat64WithAccuracy`
  (or just first two in conservative Phase 1) exported.
- Registry's `ToFloat64Lossy` field populated for all 7 kinds.
- Per-kind `toFloat64Lossy()` methods tested round-trip.

Phase 2 done when:
- FFI `reflect.Float64` path consults `ToFloat64Lossless`.
- `wile.WithLossyConversionsAllowed()` engine option works.
- Behavior change documented in CHANGELOG.

Phase 3 done when:
- `inexact-lossless?` and `inexact-with-accuracy` primitives
  shipped, documented, tested.
- Integration test exercises both from Scheme.

## Cross-references

- `plans/2026-05-14-numeric-registry-design.md` — Phase 3 of
  values-SR. Q-i resolved C3 (conservative); this plan fills the
  precision-loss gap left by that resolution.
- `plans/2026-05-14-numeric-registry-impl.md` — extends the
  `NumericTypeSpec` shape with `ToFloat64Lossy` once Phase 3
  merges.
- `werr/werr.go` — sentinel registry; new `ErrLossyConversion` joins.
- `ffi_arg_converters.go:76-96` — site of the FFI tightening
  (Phase 2).
- `extensions/math/prim_conversion.go` — site of the new Scheme
  primitives (Phase 3).
- Go stdlib `math/big`:
  - `(*big.Float).Float64() (float64, big.Accuracy)`
  - `(*big.Rat).Float64() (float64, bool)`
  - `(*big.Int).Float64() (float64, big.Accuracy)` (via `new(big.Float).SetInt(...)`)
- R7RS §6.2.6 (exact/inexact conversion) — unchanged by this plan.

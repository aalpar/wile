# Numeric loss signals — design

**Date**: 2026-05-14 (initial draft); refined 2026-05-14 post-approval;
  converged with impl plan 2026-05-14 (BigFloat API hygiene added;
  `ToFloat64Lossy` dropped; `Complex128Result` struct introduced;
  `NumericTypeSpec` old fields replaced rather than augmented).
**Status**: **Complete — all three PRs merged.**
  - PR 1 (Go infrastructure) merged 2026-05-15 as #753 (`9d96a56d` +
    `dd243b9e` + Copilot fixups).
  - PR 2 (FFI tightening + helpers.ToFloat64 tightening) merged
    2026-05-15 as #754 (merge commit `45295bdb`).
  - PR 3 (four Scheme primitives + three-layer integration test +
    docs) merged 2026-05-15 as #755 (merge commit `a965d5ae`).

  All Q-1..Q-6 resolutions shipped as designed. One acceptance-table
  variation surfaced during PR 2: `helpers.ToFloat64` tightening
  broke `(atan y x)` for lossy operands (R7RS §6.2.6 inherently
  returns inexact, so silent loss is load-bearing there). Mitigated
  in PR 2 by an `atan2Operand` helper that goes through the
  lossy-allowed path. Cleanup deferred as a Tier 5 tech-debt entry
  (`TODO.md`: "Unify `atan2Operand` with `helpers.ToFloat64`").

  This document records the design rationale + acceptance contract.
  Code-stub detail lives in
  `2026-05-14-numeric-loss-signals-impl.md`. Numeric-registry
  Phase 3 prerequisite shipped at commit `082836d1` (PR #752).
**Refinement mandate**: User instruction "no information loss from
  Go `big` package to Scheme side" reshapes the design — every
  precision-loss signal that Go's stdlib surfaces (`big.Accuracy`,
  `(*big.Rat).Float64()` exact bool) must be retrievable by Scheme
  code through paraphrased primitives.
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

- The numeric-registry design (`memory/2026-05-14-numeric-registry-design.md`)
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

1. **No information loss from Go `big` package to Scheme.** Every
   precision-loss signal Go's `math/big` surfaces — the three-valued
   `big.Accuracy` enum (`Below`/`Exact`/`Above`), the `(*big.Rat).Float64()`
   exact bool, the `(*big.Int).Float64()` accuracy — must be
   retrievable by Scheme code through paraphrased primitives.
   `'below`/`'exact`/`'above` symbols (mirroring `big.Accuracy`) are
   the Scheme reflection.
2. **Detection at every boundary.** Every conversion site that
   today discards loss information surfaces the signal to its
   caller. Specifically: `*big.Float→float64` (12+ Wile sites
   today), `*big.Rat→float64` (6+ sites), `*big.Int→float64` (6+
   sites), `int64→float64` (one site — `ffi_arg_converters.go:50-53`).
3. **Typed sentinel.** A `werr.ErrLossyConversion` sentinel so
   callers can `errors.Is` against precision-loss specifically
   (vs. the generic `ErrNotAReal` which today subsumes "wrong
   type" and "right type but doesn't fit").
4. **Complex domain is first-class.** Per-component accuracy
   (`real-acc`, `imag-acc`) for complex→complex128 conversions —
   collapsing two independent precision events into one bool would
   itself be information loss. The complex domain has dedicated
   primitives and FFI paths.
5. **FFI tightening (both `float64` AND `complex128`).** The FFI
   converters accept BigFloat/BigComplex/Complex inputs when they
   fit; reject them with a precise sentinel when they don't.
   Today: rejects all BigFloat unconditionally with `ErrNotAReal`.
   Future: rejects only when conversion would lose precision; the
   error message names the direction of loss (Below/Above).
6. **Scheme primitives mirror Go APIs.** Each Go function that
   computes/returns accuracy has a Scheme counterpart that
   exposes the same information at the Scheme level.

## Non-goals

- **Tracking accuracy through arithmetic.** The numeric dispatch
  tables (per-type `[numKinds]func` arrays) propagate `Number`
  values, not `(Number, big.Accuracy)` pairs. Threading accuracy
  through arithmetic would require restructuring every arithmetic
  closure, with hot-path implications. Out of scope.
- **Changing R7RS-mandated lossy behavior.** R7RS §6.2.6 specifies
  `(exact->inexact (* 10 (expt 10 100)))` returns `+inf.0`, not an
  error. This plan adds *companion* primitives that report loss
  signals; it does not modify `exact->inexact`.
- **Subnormal/denormal precision edge cases beyond what
  `big.Accuracy` reports.** The stdlib's enum captures the relevant
  signal; we don't invent finer granularity.
- **Exposing `(*big.Float).Acc()` (the per-instance last-operation
  accuracy).** That's a different concept (operation history, not
  conversion result); out of scope unless demand emerges.
- **General-purpose `(conversion-lossless? from-kind to-kind)`
  primitive.** Loss signals here are scoped to conversion-to-float64
  and conversion-to-complex128 (the Go-tower-aligned boundaries).
  Arbitrary kind-to-kind loss is out of scope.

## Proposed Go-side API

### New sentinel

```go
// werr/werr.go
var ErrLossyConversion = NewStaticError("lossy conversion")
```

Used by helpers when a conversion succeeds *mechanically* but
loses precision. Distinct from `ErrNotANumber` (wrong type
entirely) and `ErrNotAReal` (right family, wrong subset).

### Accuracy semantics (paraphrased from Go stdlib)

The Wile reflection of `big.Accuracy` uses the same three values
with the same semantics — paraphrased only in name:

| `big.Accuracy` (Go) | Wile (Go API) | Wile (Scheme symbol) | Meaning                              |
|---------------------|---------------|----------------------|--------------------------------------|
| `big.Below`         | `big.Below`   | `'below`             | `result < true value` (rounded down) |
| `big.Exact`         | `big.Exact`   | `'exact`             | `result == true value` (lossless)    |
| `big.Above`         | `big.Above`   | `'above`             | `result > true value` (rounded up)   |

The Wile Go API uses `big.Accuracy` directly (no Wile-specific
wrapper enum) — the user mandate against information loss applies
both upward (to Scheme) and laterally (no needless aliasing in Go).

**For overflow:** `big.Float.Float64()` saturates to `±math.Inf(0)`
when the magnitude exceeds float64 range, and reports
`Above`/`Below` for the saturation direction (`+Inf` saturating a
finite positive value → result > value → `Above`; same logic for
negative). Wile preserves this contract.

**For NaN:** NaN propagates through `Float64()` and is reported as
`Exact` accuracy (the conversion was identity: NaN in, NaN out).
Callers that care about NaN check `math.IsNaN(f)` separately —
NaN is an IEEE 754 concept, not a precision-loss concept.

### Real-domain conversion helpers

```go
// values/conversion.go (new file)

// ToFloat64WithAccuracy converts a real Number to float64, returning
// the float64 result, the big.Accuracy of the conversion, and the
// real? bool (false if the input was complex with non-zero imaginary).
//
// For *Complex/*BigComplex with non-zero imaginary part, the
// returned float64 is the real-part-only conversion; the accuracy
// reports the real-part's conversion accuracy; real? is false to
// signal the dropped imaginary.
//
// For non-Number inputs, returns ErrNotANumber.
//
// This is the PRIMARY API — Lossless and the FFI converter wrap it.
func ToFloat64WithAccuracy(n Number) (f float64, acc big.Accuracy, real bool, err error)

// ToFloat64Lossless is a convenience wrapper: returns
// ErrLossyConversion if accuracy != Exact OR real == false.
// FFI-callable.
func ToFloat64Lossless(n Number) (float64, error)
```

**Note on `ToFloat64Lossy` (dropped during impl-plan refinement):**
An earlier draft proposed a third wrapper `ToFloat64Lossy(n) (float64, bool)`
that panicked on non-Number. It was dropped because the sole would-be
caller (the FFI Float64 converter under `WithLossyConversionsAllowed`)
already type-validates its input as `values.Number` *before* reaching
the helper — the panic path is dead. Lossy-allowed callers now use
`ToFloat64WithAccuracy` directly with discard:
`f, _, _, _ := values.ToFloat64WithAccuracy(n)`. One fewer exported
symbol, no asymmetric failure mode.

### Complex-domain conversion helpers (per-component accuracy)

The complex helpers use a **named result struct** rather than a
positional return. The two `big.Accuracy` slots (real and
imaginary) are the same Go type, distinguished by position only —
naming them as fields prevents a class of swap bugs the type
system cannot otherwise catch. See "Decision record: return shape"
below for the full rationale.

```go
// Complex128Result captures complex-domain conversion with
// per-component accuracy. Field-named so realAcc/imagAcc swaps
// are caught at compile time, not surfaced only as wrong output
// in the three-layer integration test.
type Complex128Result struct {
    // Value is the complex128 representation.
    Value complex128

    // RealAcc reports the conversion accuracy of the real
    // component (Below / Exact / Above per Go big.Accuracy).
    RealAcc big.Accuracy

    // ImagAcc reports the conversion accuracy of the imaginary
    // component. For real-only inputs (Integer/BigInteger/Float/
    // BigFloat/Rational), this is always big.Exact (the imaginary
    // part is exactly zero).
    ImagAcc big.Accuracy
}

// ToComplex128WithAccuracy converts a Number to complex128 with
// per-component accuracy.
//
// For non-Number inputs, returns ErrNotANumber.
func ToComplex128WithAccuracy(n Number) (Complex128Result, error)

// ToComplex128Lossless is the binary version: returns
// ErrLossyConversion if either component's accuracy is non-Exact.
// Returns the raw complex128 (strict-path consumers don't need
// the struct).
func ToComplex128Lossless(n Number) (complex128, error)
```

### Decision record: return shape — hybrid (positional + struct)

The two `WithAccuracy` helpers carry multi-component results. Three
shapes were evaluated. **The hybrid was chosen** after the
all-positional and all-struct shapes each failed one of the two
critical criteria. This subsection records the alternatives so a
future reader can revisit the choice without re-deriving the
analysis.

**Chosen — Hybrid (positional for float64, struct for complex128)**

```go
type Complex128Result struct {
    Value   complex128
    RealAcc big.Accuracy
    ImagAcc big.Accuracy
}

func ToFloat64WithAccuracy(n Number)    (float64, big.Accuracy, bool, error)
func ToComplex128WithAccuracy(n Number) (Complex128Result, error)
```

The rule applied: **use positional return when slot types
distinguish their roles; use a named struct when adjacent slots
share a type and could be silently swapped.**

- `ToFloat64WithAccuracy` slots: `float64`, `big.Accuracy`, `bool`,
  `error` — all four are distinct Go types. Position-swap requires
  swapping the float with the accuracy or the bool with the error;
  the compiler catches each. Positional is safe.
- `ToComplex128WithAccuracy` slots in the positional shape would
  be: `complex128`, `big.Accuracy`, `big.Accuracy`, `error`. The
  two `big.Accuracy` slots are interchangeable to the compiler. A
  `realAcc/imagAcc` swap at any call site compiles, type-checks,
  and is caught only at runtime by the three-layer integration
  test. Naming the slots as fields prevents the bug class at
  compile time.

**Alternative A — All-positional (rejected: swap risk on complex)**

```go
func ToFloat64WithAccuracy(n Number)    (float64,    big.Accuracy, bool, error)
func ToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy, error)
```

Strengths:
- Public API surface is minimal — no extra exported types.
- No translation step across the three consumer layers
  (per-kind closure → values/ helper → consumer); all three
  carry the same tuple shape.
- `f, _, _, _ := ToFloat64WithAccuracy(n)` is the idiomatic Go
  expression of "I want the value, throw the rest away." The FFI
  converter under `WithLossyConversionsAllowed` relies on this.

Weakness that ruled it out:
- Same-type adjacency on `(big.Accuracy, big.Accuracy)` in
  `ToComplex128WithAccuracy` admits silent `realAcc/imagAcc`
  swap bugs at any call site. Compiler doesn't catch it; unit
  tests that check both components together don't catch it; only
  the cross-layer integration test (PR 3) catches it. This matches
  the established `BigComplex` problem where constructors use
  `rel, iam` parameter naming as a hint the type system can't
  enforce.

**Alternative B — All-struct (rejected: needless friction on float)**

```go
type Float64Result struct {
    Value    float64
    Accuracy big.Accuracy
    IsReal   bool
}

type Complex128Result struct {
    Value   complex128
    RealAcc big.Accuracy
    ImagAcc big.Accuracy
}

func ToFloat64WithAccuracy(n Number)    (Float64Result, error)
func ToComplex128WithAccuracy(n Number) (Complex128Result, error)
```

Strengths:
- Field-level godoc (`res.IsReal` explained on the field, not in
  prose).
- Non-breaking extension (a future fifth field is opt-in for
  consumers).
- Methods attachable (`func (r Float64Result) Lossless() bool`
  centralizes the rule).
- `qt.Assert(res, qt.DeepEquals, Float64Result{...})` is one test
  assertion vs. three positional.

Weaknesses that ruled it out for `ToFloat64WithAccuracy`:
- Three distinct slot types (`float64`, `big.Accuracy`, `bool`)
  carry no swap risk; the struct provides no safety benefit here.
- Forces translation at every consumer of the float64 helper. The
  FFI lossy-allowed path loses its single-line discard idiom:
  `f, _, _, _ := fn()` becomes `res, _ := fn(); f := res.Value`.
- Per-kind closure → struct translation requires either an
  allocation per call (closure returns positional → public wraps)
  or a struct stored in the registry closure (changes the cold-
  path API for no internal benefit). Allocations on the FFI
  conversion path, while not arithmetic-hot, are higher-volume
  than the `ToComplex128WithAccuracy` path (most FFI registrations
  are real-valued).

**Why the hybrid wins**

The rule "positional when types disambiguate, struct when they
don't" applies the safety property only where it costs something
(swap risk) and the ergonomic property only where the API needs
it (discard idiom). Neither all-positional nor all-struct can
make this distinction; they apply one shape uniformly and pay
the corresponding price on the wrong helper.

The hybrid does add one cost: API consumers must remember two
shapes. The mitigation is that the asymmetry mirrors the actual
domain structure — real numbers have one accuracy signal; complex
numbers have two. The shape difference reflects a real semantic
difference, not an arbitrary design choice.

**Revisit trigger conditions**

Re-open this decision if any of:
- A second `WithAccuracy`-shaped helper with a single accuracy
  signal is added (e.g., for rationals, intervals, or matrix
  elements with one component). Re-evaluate whether the new helper
  should follow `ToFloat64WithAccuracy` (positional) or be
  promoted to a struct for consistency with `ToComplex128WithAccuracy`.
- A third or fourth multi-component helper is added (e.g., a
  quaternion or matrix conversion with N≥3 same-type accuracy
  slots). At that point the asymmetry-as-domain-structure argument
  weakens; consider a unified struct convention.
- The FFI converter is refactored such that it consumes the
  struct directly for both helpers (eliminating the discard-idiom
  advantage that motivates keeping `ToFloat64WithAccuracy`
  positional).
- A `realAcc/imagAcc` swap bug is reported despite the struct.
  Indicates the safety property failed; revisit whether the struct
  shape is sufficient or whether a stricter encoding (e.g.,
  newtype wrappers `type RealAccuracy big.Accuracy` + `type
  ImagAccuracy big.Accuracy`) is warranted.

Tracked at `TODO.md` under Tier 5 (Tech Debt) → "Loss-signals API
follow-ups (numeric-loss-signals impl)".

### Per-kind accuracy synthesis

`big.Accuracy` is natively returned by Go for `*big.Float.Float64()`
and `*big.Int.Float64()` (via `new(big.Float).SetInt(...).Float64()`).
For other types, we **synthesize** the three-valued accuracy by
recovering the direction explicitly. Synthesis rules (each is
deterministic; each is tested):

| Source        | Accuracy synthesis                                                                |
|---------------|-----------------------------------------------------------------------------------|
| `*Integer`    | `f := float64(n.Value)`; back-convert `i := int64(f)` (handle overflow). If `i == n.Value` → `Exact`. Else if `f < float64(n.Value)` (interpreting via `big.Int.Cmp` to avoid float-comparison pitfalls): → `Below`; else → `Above`. |
| `*BigInteger` | Native via `new(big.Float).SetInt(p.value).Float64()`. Use the returned `big.Accuracy` directly. |
| `*Rational`   | `f, exact := p.value.Float64()`. If `exact`, → `Exact`. Else round-trip: `r2 := new(big.Rat).SetFloat64(f)`; `cmp := r2.Cmp(p.value)`. `cmp == 0` shouldn't happen (we know `!exact`); `cmp < 0` (r2 < orig) → `Below`; `cmp > 0` → `Above`. |
| `*Float`      | Identity: always `Exact` (the Scheme `*Float` IS a float64).                      |
| `*BigFloat`   | Native via `p.value.Float64()`. Use the returned `big.Accuracy` directly.         |
| `*Complex`    | Real part: `real(p.Value)` (already a float64, identity → `Exact`). If `imag(p.Value) != 0`, real flag is false. |
| `*BigComplex` | Real part: native via `toBigFloat(p.real).Float64WithAccuracy()` (the new `*BigFloat` method introduced in PR 1; see impl plan's "BigFloat API hygiene" subsection). Use returned accuracy. If `!p.imag.IsZero()`, real flag is false. |

For complex-domain helpers, `imagAcc` follows the same rules
applied to the imaginary component.

### Per-kind dispatch implementations

Each numeric type contributes two package-level helper functions
(one per-kind closure each for the float64 and complex128 paths)
bound through `registerNumericSpec(...)` in the type's existing
`init()` block — same registry pattern PR #752 established.
The package-level `values.ToFloat64WithAccuracy` dispatches via the
numeric registry (extended in Phase 1). The complex helper returns
a `Complex128Result` struct (see "Decision record: return shape —
hybrid" above) — same-type adjacency on the two `big.Accuracy`
slots requires field naming to prevent silent realAcc/imagAcc swaps.

```go
// values/integer.go
func integerToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*Integer)
    f := float64(p.Value)
    back := int64(f)
    if back == p.Value {
        return f, big.Exact, true
    }
    // Direction-recovery: compare original int64 against back-converted.
    // Cannot use float comparison (loss is the question), so compare as int64.
    if back < p.Value {
        return f, big.Below, true
    }
    return f, big.Above, true
}

// values/big_integer.go
func bigIntegerToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigInteger)
    f, acc := new(big.Float).SetInt(p.value).Float64()
    return f, acc, true
}

// values/big_float.go
func bigFloatToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigFloat)
    if p.IsNaN() {
        return math.NaN(), big.Exact, true  // NaN propagates as Exact
    }
    f, acc := p.value.Float64()
    return f, acc, true
}

// values/rational.go
func rationalToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*Rational)
    f, exact := p.value.Float64()
    if exact {
        return f, big.Exact, true
    }
    // (*big.Rat).Float64 returns a bool, not direction. Recover via round-trip.
    back := new(big.Rat).SetFloat64(f)
    if back == nil {
        // f is NaN/Inf — Rational source cannot produce these, so unreachable.
        return f, big.Exact, true  // defensive
    }
    cmp := back.Cmp(p.value)
    if cmp < 0 {
        return f, big.Below, true
    }
    return f, big.Above, true
}

// values/float.go
func floatToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    return n.(*Float).Value, big.Exact, true  // identity
}

// values/complex.go
func complexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*Complex)
    return real(p.Value), big.Exact, imag(p.Value) == 0
}

// values/big_complex.go — uses the new (*BigFloat).Float64WithAccuracy()
// method (introduced by the BigFloat API hygiene fix in PR 1).
func bigComplexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigComplex)
    realF, realAcc := toBigFloat(p.real).Float64WithAccuracy()
    return realF, realAcc, p.imag.IsZero()
}
```

For the complex-domain helper, `toComplex128WithAccuracy` returns
the `Complex128Result` struct directly (per the hybrid return-shape
decision above) — per-component accuracy for the real and imaginary
parts is field-named to prevent realAcc/imagAcc swap bugs:

```go
// values/big_complex.go
func bigComplexToComplex128WithAccuracy(n Number) Complex128Result {
    p := n.(*BigComplex)
    realF, realAcc := toBigFloat(p.real).Float64WithAccuracy()
    imagF, imagAcc := toBigFloat(p.imag).Float64WithAccuracy()
    return Complex128Result{Value: complex(realF, imagF), RealAcc: realAcc, ImagAcc: imagAcc}
}

// values/integer.go (real-only kinds; imaginary part trivially exact)
func integerToComplex128WithAccuracy(n Number) Complex128Result {
    f, acc, _ := integerToFloat64WithAccuracy(n)
    return Complex128Result{Value: complex(f, 0), RealAcc: acc, ImagAcc: big.Exact}
}
// ... and analogous for BigInteger, Float, BigFloat, Rational, Complex
```

These are package-level functions and are accessed via the registry
(Phase 1 extends `NumericTypeSpec` to carry the function pointers).
The hot-path arithmetic dispatch tables in `promotion.go` are
unchanged — these loss-signal helpers live exclusively on the cold
path (FFI conversions and the four Scheme primitives in PR 3).

## Proposed Scheme-side API

### Primitives (lives in `extensions/math/prim_conversion.go` per Q-3)

```scheme
(inexact-lossless? n)
  ;; Returns #t if (exact->inexact n) would be fully lossless,
  ;; #f otherwise. Predicate; does not allocate the inexact
  ;; representation.
  ;;
  ;; For real n: #t iff the float64 representation is exact.
  ;; For complex n: #t iff BOTH the real and imaginary parts'
  ;;   conversions are exact. Non-zero imaginary on a converting-
  ;;   to-float64 path is NOT considered here (this primitive uses
  ;;   complex128 semantics for complex input).

(inexact-accuracy n)
  ;; Predicts the accuracy of (exact->inexact n) without performing
  ;; the conversion.
  ;;
  ;; For real n: returns a single symbol — 'below, 'exact, or 'above.
  ;; For complex n: returns two values — (values real-acc imag-acc),
  ;;   each one of 'below/'exact/'above.
  ;;
  ;; This is the most precise loss-prediction primitive — it returns
  ;; the same information Go's big.Accuracy carries.

(inexact-with-accuracy n)
  ;; Performs the conversion AND reports accuracy.
  ;;
  ;; For real n: returns (values inexact-n accuracy-sym).
  ;;   inexact-n is *Float (saturating to +inf.0 / -inf.0 for
  ;;   out-of-range BigFloat/Rational/BigInteger; NaN propagates
  ;;   as NaN). accuracy is 'below / 'exact / 'above per Go
  ;;   big.Accuracy semantics.
  ;;
  ;; For complex n: returns (values inexact-c real-acc imag-acc).
  ;;   inexact-c is *Complex. real-acc and imag-acc each one of
  ;;   'below/'exact/'above.

(complex-inexact-with-accuracy n)
  ;; Variant of inexact-with-accuracy that always uses complex128
  ;; semantics — accepts real inputs too (imag-acc trivially 'exact).
  ;; Returns 3 values uniformly: (values inexact-c real-acc imag-acc).
  ;;
  ;; Useful for code that wants polymorphism-by-value-count avoidance.
```

Accuracy symbol values (Scheme-side, paraphrasing `big.Accuracy`):

| Symbol  | Meaning                                          |
|---------|--------------------------------------------------|
| `'below` | result < true value (rounded down)              |
| `'exact` | result == true value (lossless)                 |
| `'above` | result > true value (rounded up / saturated up) |

These are global singleton symbols (`values.SymbolAccuracyBelow`,
`SymbolAccuracyExact`, `SymbolAccuracyAbove`) — same pattern as
`SymbolMutexNotOwned`, `SymbolThreadRunnable`, etc.

### Example use

```scheme
;; Real cases
(inexact-lossless? 7)                                ; => #t
(inexact-accuracy 7)                                 ; => 'exact
(inexact-with-accuracy 7)                            ; => 7.0, 'exact

(inexact-lossless? 1/3)                              ; => #f
(inexact-accuracy 1/3)                               ; => 'below  (0.333... < 1/3)
(inexact-with-accuracy 1/3)                          ; => 0.333..., 'below

(define big (expt 10 100))
(inexact-lossless? big)                              ; => #f
(inexact-accuracy big)                               ; => 'above  (+inf saturates upward)
(inexact-with-accuracy big)                          ; => +inf.0, 'above

(inexact-lossless? +nan.0)                           ; => #t  (NaN→NaN identity)
(inexact-accuracy +nan.0)                            ; => 'exact

;; Complex cases (multiple-value return)
(inexact-lossless? 3+4i)                             ; => #t  (both parts exact)
(inexact-accuracy 3+4i)                              ; => (values 'exact 'exact)
(inexact-with-accuracy 3+4i)                         ; => 3.0+4.0i, 'exact, 'exact

(inexact-lossless? (make-rectangular 1/3 1/7))       ; => #f
(inexact-accuracy (make-rectangular 1/3 1/7))        ; => (values 'below 'below)
(inexact-with-accuracy (make-rectangular big 1))     ; => +inf.0+1.0i, 'above, 'exact
```

### Why polymorphic return shape?

`inexact-accuracy` and `inexact-with-accuracy` return a different
number of values based on whether the input is real or complex.
This is a deliberate choice from Q-2 (the user-resolved option:
"Real-only triple + per-component complex"). The alternative —
forcing complex inputs to return a list `('below 'below)` instead
of two values — would be uniform-shape but less idiomatic in
Scheme.

Callers handle polymorphism via `call-with-values`:

```scheme
;; Real-only consumer:
(call-with-values
  (lambda () (inexact-with-accuracy n))
  (lambda (f acc) (printf "~a (~a)~n" f acc)))

;; Domain-aware consumer:
(call-with-values
  (lambda () (inexact-with-accuracy n))
  (case-lambda
    ((f acc)             (handle-real f acc))
    ((c real-acc imag-acc) (handle-complex c real-acc imag-acc))))
```

The `complex-inexact-with-accuracy` variant exists for callers
who want a uniform 3-value return regardless of input domain.

### FFI integration

Two converter paths tighten — `reflect.Float64` AND `reflect.Complex128`.
Both consult the loss-signals helpers.

```go
// ffi_arg_converters.go — Float64 path (PR 2)
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
                "%s: argument %d: %T cannot losslessly fit float64", name, pos, v,
            )
        }
        return reflect.ValueOf(f).Convert(targetType), nil
    }, nil

// ffi_arg_converters.go — Complex128 path (PR 2; new)
case reflect.Complex128:
    targetType := t
    return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
        n, ok := v.(values.Number)
        if !ok {
            return reflect.Value{}, fmtArgError(name, pos, "number", v)
        }
        c, err := values.ToComplex128Lossless(n)
        if err != nil {
            return reflect.Value{}, werr.WrapForeignErrorf(
                err,
                "%s: argument %d: %T cannot losslessly fit complex128", name, pos, v,
            )
        }
        return reflect.ValueOf(c).Convert(targetType), nil
    }, nil
```

The above stubs show the **strict path only**. The full PR 2 impl
plan threads a `lossyAllowed` flag through `buildFFISpec` to
`makeArgConverter`; when `lossyAllowed` is true the converter
substitutes `values.ToFloat64WithAccuracy` (and discards
`(acc, isReal, err)`) / `values.ToComplex128WithAccuracy` (and
projects `res.Value`) — same in-line discard idiom as the lossless
path, no new helper required.

**Behavior change vs. today (`float64` path):**
- Before: any `*BigFloat`, `*BigComplex`, `*Complex` rejected
  unconditionally with `ErrNotAReal`.
- After: `*BigFloat` accepted when it fits losslessly; rejected
  with `ErrLossyConversion` otherwise. `*Complex` accepted when
  imaginary part is zero AND real part fits losslessly.
  `*BigComplex` analogous. The rejection error now names the
  direction (Below/Above) so debugging is precise.

**Behavior change vs. today (`complex128` path):**
- Before: `reflect.Complex128` had no converter; Go functions
  taking `complex128` were unregisterable (verified — no case in
  `ffi_arg_converters.go` for it today).
- After: full support. `*BigComplex` accepted when both parts fit
  losslessly; rejected with `ErrLossyConversion` otherwise.

Both are **widenings** of accepted inputs — strictly more
permissive than today. No existing FFI caller that succeeds today
will fail under this change.

### Opt-in: WithLossyConversionsAllowed engine option

Per Q-4 resolution: engine-level option. Applies globally to FFI
functions registered after the option is set.

```go
eng := wile.NewEngine(ctx,
    wile.WithLossyConversionsAllowed(),
    wile.WithAllExtensions(),
)
err := eng.RegisterFunc("my-numerical-routine", fn)
// fn's float64/complex128 parameters now accept lossy conversions silently.
```

Implementation: a flag on `*Engine` consulted by the FFI converters.
When true, converters call `values.ToFloat64WithAccuracy` and
discard the accuracy / isReal / err slots
(`f, _, _, _ := values.ToFloat64WithAccuracy(n)`); when false (the
default), converters call `values.ToFloat64Lossless` which errors
with `ErrLossyConversion` on any loss. Silent truncation requires
an explicit opt-in.

Per-function and per-parameter granularity (Q-4 options b, c) are
deferred. The engine-level option is sufficient for v1.

### Tightening `helpers.ToFloat64` (per Q-5 resolution)

`registry/helpers/value_conv.go::ToFloat64` is the cross-package
conversion helper used by primitives like `exact->inexact`,
`number->string`, the `(wile algebra)` extension, etc. It today
returns `ErrNotAReal` for `*BigFloat` (along with the Complex
variants). Per Q-5 = yes, this helper tightens consistently with
the FFI:

- **`*BigFloat` accepted** (widening — losslessly converting to
  float64 when possible, returning `ErrLossyConversion` when not).
- **`*Complex` / `*BigComplex`** continue to error with
  `ErrNotAReal` (they aren't real; the discipline distinction
  between "wrong type" and "right type, wrong magnitude" matters).

This brings `helpers.ToFloat64` into alignment with the FFI's
post-tightening behavior. The `exact->inexact` primitive itself
remains R7RS-mandated lossy — it does not consume any of the new
loss-signal helpers; its implementation path is unchanged by this
plan (R7RS §6.2.6 requires it to succeed regardless of precision
loss, returning `±inf.0` on overflow).

## Integration with the numeric registry

Phase 3 of values-SR delivers a `NumericTypeSpec` registry (PR #752
merged 2026-05-13). The existing `toFloat64` and `toComplex128`
fields cover universal conversion across all 7 kinds with `(float64,
error)` / `complex128` returns respectively.

This plan **replaces** those two fields with loss-signal-aware
variants (the new fields carry strictly more information; the old
fields become redundant once internal callers in `promotion.go`
migrate). Both **always populated** for all 7 kinds:

```go
type NumericTypeSpec struct {
    // ... existing fields per the numeric-registry plan ...

    // toFloat64WithAccuracy REPLACES the previous `toFloat64
    // func(Number) (float64, error)` field. Returns the float64
    // result, the big.Accuracy of the conversion (Below/Exact/
    // Above), and a real bool (false if the input was complex
    // with non-zero imaginary part, where the float64 is
    // real-part-only). Always non-nil — internal callers in
    // promotion.go that previously consumed `toFloat64` now
    // synthesize ErrNotAReal from `!isReal` at the call site.
    toFloat64WithAccuracy func(Number) (float64, big.Accuracy, bool)

    // toComplex128WithAccuracy REPLACES the previous `toComplex128
    // func(Number) complex128` field. Returns a Complex128Result
    // struct (field-named to prevent realAcc/imagAcc swap bugs at
    // call sites — see "Decision record: return shape — hybrid"
    // above). Always non-nil. For real-only kinds, the result's
    // ImagAcc is trivially big.Exact.
    toComplex128WithAccuracy func(Number) Complex128Result
}

func (p *NumericTypeSpec) ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    return p.toFloat64WithAccuracy(n)
}

func (p *NumericTypeSpec) ToComplex128WithAccuracy(n Number) Complex128Result {
    return p.toComplex128WithAccuracy(n)
}
```

The old `ToFloat64` / `ToComplex128` getters from PR #752 are
**dropped**. The two production wrapper functions in `promotion.go`
(`NumberToFloat64`, `NumberToComplex128`) migrate to the new
helpers — the impl plan's PR 1 Step 5 details the per-site
migration. The full information content of the old getters is
preserved by `!isReal` (synthesizes the `ErrNotAReal` panic at the
call site) plus ignoring the accuracy slot on the IEEE 754 guard
short-circuit path.

`helpers.ToFloat64` migrates to the new helper per Q-5 in PR 2; its
behavior tightens to return `ErrLossyConversion` on precision loss
for BigFloat / BigInteger overflow / Rational with non-representable
denominators (previously silently truncated).

## Phasing

Three PRs, sequenced. PRs 2 and 3 depend on PR 1 merging first.
See `2026-05-14-numeric-loss-signals-impl.md` for per-step
detail, acceptance criteria, and test plans.

### Phase 1 — Go infrastructure (1 PR)

1. Add `werr.ErrLossyConversion` sentinel (`werr/werr.go`).
2. Add global accuracy symbols (`values.SymbolAccuracyBelow`,
   `SymbolAccuracyExact`, `SymbolAccuracyAbove`) plus the
   `BigAccuracyToSymbol(acc) *Symbol` helper.
3. **BigFloat API hygiene**: rename existing
   `(*BigFloat).Float64() float64` → `Float64Truncated()` and add
   new `Float64WithAccuracy() (float64, big.Accuracy)`. Migrate
   13 production call sites + test sites (full detail in impl
   plan's "BigFloat API hygiene" subsection). The rename is what
   lets the new BigComplex helpers below access accuracy through a
   named-and-symmetric API rather than the awkward
   `BigFloatValue().Float64()` chain through the stdlib accessor.
4. Add per-kind package-level helpers
   (`<type>ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool)`
   and `<type>ToComplex128WithAccuracy(n Number) Complex128Result`)
   for each of the 7 numeric types. Bind via `registerNumericSpec`
   in each type's existing `init()` block. ~20 LOC each including
   direction-recovery for Integer/Rational.
5. Add exported helpers:
   - `values.ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error)` — primary
   - `values.ToFloat64Lossless(n Number) (float64, error)` — wrapper
   - `values.Complex128Result` exported struct
     (`Value complex128`, `RealAcc`, `ImagAcc big.Accuracy`)
   - `values.ToComplex128WithAccuracy(n Number) (Complex128Result, error)` — primary
   - `values.ToComplex128Lossless(n Number) (complex128, error)` — wrapper

   `ToFloat64Lossy` was considered and dropped — see "Note on
   `ToFloat64Lossy`" above. Lossy-allowed callers use
   `ToFloat64WithAccuracy` directly with `_, _, _` discard.
6. **Replace** the `NumericTypeSpec` `toFloat64` and `toComplex128`
   fields with the `toFloat64WithAccuracy` and
   `toComplex128WithAccuracy` fields + getter methods. Register for
   all 7 kinds in their `init()` blocks. Migrate the two production
   call sites in `promotion.go` (`NumberToFloat64`,
   `NumberToComplex128`) to consume the new spec methods.
7. Tests: round-trip per kind, boundary cases (MaxFloat64,
   subnormals, 2^100, NaN, ±Inf, ±0, exact 1/3-style rationals
   that round Below/Above predictably). R7RS-required behavior
   preserved (`exact->inexact` still always succeeds).

Estimated: ~415 LOC / −25 across ~12 files (the BigFloat hygiene
fix added ~+15 LOC vs the original ~350 estimate; net larger
because the 13 call-site renames touch four additional files
outside `values/`). No behavior change anywhere outside the new
helpers and the renamed `Float64Truncated` (same body, new name).

### Phase 2 — FFI tightening + helpers.ToFloat64 migration (1 PR)

1. Migrate `ffi_arg_converters.go` `reflect.Float64` case:
   `ToFloat64Lossless` is the default; in lossy-allowed mode the
   converter calls `ToFloat64WithAccuracy` and discards the
   accuracy/isReal slots
   (`f, _, _, _ := values.ToFloat64WithAccuracy(n)`).
2. Add `reflect.Complex128` case (currently missing). Strict mode
   calls `ToComplex128Lossless`; lossy-allowed mode calls
   `ToComplex128WithAccuracy` and projects `res.Value`, discarding
   the per-component accuracies.
3. Add `wile.WithLossyConversionsAllowed()` engine option;
   thread the flag from `*Engine` to the FFI converter closures
   at registration time.
4. Migrate `registry/helpers/value_conv.go::ToFloat64` to consult
   `values.ToFloat64Lossless` for the 5 reducible kinds (per Q-5
   tightening); BigFloat newly accepted when it fits losslessly.
5. Tests covering:
   - **Widening**: `*BigFloat` that fits float64 now succeeds at
     the FFI boundary (was rejected).
   - **Narrowing precision**: `*BigFloat` with `2^100` magnitude
     errors with `ErrLossyConversion` (was `ErrNotAReal`); error
     message names the direction (`Above`).
   - **Complex128**: `reflect.Complex128` parameters now accept
     `*BigComplex` that fits; reject when either component is
     lossy.
   - **Opt-in**: `WithLossyConversionsAllowed()` engine option
     suppresses both error paths.

Estimated: ~150 LOC. Three documented behavior changes (the two
widenings above plus the `helpers.ToFloat64` widening).

### Phase 3 — Scheme primitives (1 PR)

1. Implement four primitives in
   `extensions/math/prim_conversion.go` (per Q-3 — alongside
   `exact->inexact`):
   - `inexact-lossless?` — predicate
   - `inexact-accuracy` — single-symbol return for real, two-value
     return for complex
   - `inexact-with-accuracy` — performs conversion, returns
     2 values (real) or 3 values (complex)
   - `complex-inexact-with-accuracy` — uniform 3-value return
2. Docstrings with R7RS-style entries (`Parameters:`, `Returns:`,
   `Category: Numbers — Conversion`, plus a `Keywords:` field for
   apropos discovery).
3. Tests: every kind × every accuracy outcome × both real and
   complex domain. Integration test exercising all 4 primitives
   from a Scheme program. Verify polymorphic return shape
   (1-value, 2-value, 3-value) per kind.

Estimated: ~250 LOC. No existing-primitive behavior change.

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

## Open questions — resolved (2026-05-14)

User accepted the recommended default for every question, plus
the no-information-loss mandate added these resolutions:

| Q   | Resolution | Implication |
|-----|------------|-------------|
| Q-1 | Saturate to ±Inf with accuracy `'above`/`'below` | `inexact-with-accuracy` returns uniformly `*Float`; saturation reported via the accuracy symbol. Matches Go `(*big.Float).Float64()` semantics. |
| Q-2 | Real-only triple + per-component complex | Four primitives: `inexact-lossless?`, `inexact-accuracy`, `inexact-with-accuracy`, `complex-inexact-with-accuracy`. Complex domain has per-component accuracy preserved. |
| Q-3 | `extensions/math/` (alongside `exact->inexact`) | Primitives ship in the math extension; loaded by default in most profiles; discoverable via apropos. |
| Q-4 | Engine-level `wile.WithLossyConversionsAllowed()` | One option applies globally. Per-function and per-parameter granularity deferred. |
| Q-5 | Yes — `helpers.ToFloat64` tightens to match FFI | `*BigFloat` accepted when fits losslessly; rejects with `ErrLossyConversion` when not. Consistent FFI / helpers behavior. |
| Q-6 (added post-crosscheck) | **NaN/Inf as `big.Exact` — documented, not changed** | Three-lens crosscheck (errors F6 + types C8 + tests T9) converged on the ambiguity that `(inexact-lossless? +nan.0)` returns `#t`. Mechanically correct — NaN→NaN is bit-pattern identity, `+Inf→+Inf` is identity, both return `big.Exact`. Semantically surprising — a caller reading `acc == Exact` may trust the value as if finite. **Resolution**: keep the mechanical contract (preserves Go stdlib semantics and avoids inventing a fourth accuracy value); document the contract prominently in every helper doc comment and in the Scheme primitive docstrings. Callers requiring finite-screening do so independently via `math.IsNaN(f)` / `math.IsInf` / R7RS `(finite? n)`. Trade-off explicit: simplicity preserved at the cost of one caveat. |

### Latent ambiguities resolved by the no-information-loss mandate

The refinement pass surfaced 8 latent ambiguities in the original
draft. Each is resolved here:

| # | Ambiguity | Resolution |
|---|-----------|------------|
| A1 | Accuracy direction wording was unclear ("Above if it falls short") | Replaced with explicit `big.Accuracy` semantics table (`Below`: result < value; `Above`: result > value). |
| A2 | `(*big.Rat).Float64()` returns bool only; direction unspecified | Direction recovered via round-trip `new(big.Rat).SetFloat64(f).Cmp(orig)`. |
| A3 | Integer round-trip direction unspecified | Direction recovered by comparing `back := int64(float64(n.Value))` with `n.Value` as int64 (avoids float-comparison pitfalls). |
| A4 | BigComplex case: which contributed to "lossy" — real-part accuracy or imag-dropped? | Both are independent. `toFloat64WithAccuracy` returns the *real-part accuracy* in the `acc` slot; the `real bool` flag (false if imag != 0) signals imag was dropped. Both must be `Exact`/`true` for fully-lossless. |
| A5 | NaN handling unspecified | NaN propagates as `big.Exact` (identity conversion). Callers check `math.IsNaN(f)` separately if they need to distinguish. |
| A6 | `Float(NaN)` → float64 considered "lossless"? | Yes — identity; accuracy `Exact`. (`*Float` IS a float64.) |
| A7 | `Float64Accuracy` struct vs multiple-value return | **Hybrid** — positional for `ToFloat64WithAccuracy` (slot types are distinct: `float64`/`big.Accuracy`/`bool`/`error` — no swap risk; preserves the discard idiom `f, _, _, _ := ...`); struct (`Complex128Result`) for `ToComplex128WithAccuracy` (same-type `big.Accuracy` slots adjacent; field naming prevents silent realAcc/imagAcc swap). See "Decision record: return shape — hybrid (positional + struct)" above. |
| A8 | Was `big.Accuracy` supposed to be hidden from public API? | Per the no-information-loss mandate: NO — `big.Accuracy` is the public type at the Go layer. R2 in the risk register has been flipped. |

## Risks

| # | Risk                                                            | Mitigation                                                                  |
|---|-----------------------------------------------------------------|-----------------------------------------------------------------------------|
| R1 | FFI behavior change breaks callers who relied on prior semantics | **Reframed** after verifying `ffi_arg_converters.go:76-96`: the FFI Float64 path change is **bidirectional** like helpers.ToFloat64 (R8). Widening: `*BigFloat` newly accepted when it fits losslessly (today rejected via `fmtArgError`/ErrTypeConversion). Tightening: `*BigInteger` overflow and `*Rational` non-representable today succeed silently with truncation; under strict mode they will newly error with `ErrLossyConversion`. Embedders depending on either prior behavior can opt into the legacy silent-truncate path via `WithLossyConversionsAllowed()`. CHANGELOG must list both directions. |
| R2 | ~~`big.Accuracy` exposure leaks stdlib detail~~ **FLIPPED**: the no-information-loss mandate REQUIRES exposing `big.Accuracy` publicly. The plan now treats this as a *goal*, not a risk. | n/a (resolved by the user mandate) |
| R3 | Subnormal / boundary precision edge cases produce surprising signals | `big.Accuracy` is the authoritative IEEE 754 signal; trust the stdlib. Tests cover boundaries explicitly. |
| R4 | Scheme primitive names clash with hypothetical future R7RS additions | Names are R7RS-large-compatible (`inexact-lossless?` parallels `inexact?`; `inexact-with-accuracy` is novel but unambiguous). If a future R7RS edition adds the same name with different semantics, namespace via `(wile numerics)` shadowing — deferred until needed. |
| R5 | Implementation-effort drift: adding loss-signal infrastructure to per-kind dispatch tempts further "what about arithmetic loss?" expansion | Stay scoped: this plan covers *conversion* boundaries only. Arithmetic-internal loss is a separate plan (not yet drafted). |
| R6 | ~~`ToFloat64WithAccuracy` over-engineered~~ **FLIPPED**: per no-information-loss, `WithAccuracy` is the PRIMARY API; `Lossless` is the only wrapper that ships. (The earlier `Lossy` wrapper was dropped during impl-plan refinement — its sole would-be caller already type-validates `n` as `Number` before reaching the helper, so the wrapper's panic path was dead. Lossy-allowed callers now use `ToFloat64WithAccuracy` directly with `f, _, _, _ := ...` discard.) | n/a (resolved by the user mandate) |
| R7 | Polymorphic return shape (1, 2, or 3 values from `inexact-with-accuracy`) confuses Scheme callers | Scheme `call-with-values` + `case-lambda` is the documented idiom. Document with examples. `complex-inexact-with-accuracy` exists as a uniform-3-value variant for callers who want monomorphic return. |
| R8 | `helpers.ToFloat64` tightening (Q-5) changes the cross-package contract — both widening AND breaking | **Reframed** after verifying current code (`registry/helpers/value_conv.go:72-90`): the change is **bidirectional**. Widening: `*BigFloat` is already accepted today (silently truncated); under Q-5, lossless BigFloat continues to succeed but lossy now errors. Tightening (the breaking part): three input classes that today succeed silently with lossy truncation will newly error — `*BigInteger` overflow, `*BigFloat` overflow, `*Rational` with non-power-of-2 denominator. Calls that succeeded silently for these inputs will now error with `ErrLossyConversion`. Audited consumers: `extensions/algebra`, `extensions/math`, `extensions/wile-tools`, and internal coverage tests — each must be reviewed for whether silent loss was load-bearing. CHANGELOG entry must enumerate all three behavior changes. |

## Acceptance test cases (the no-information-loss check)

These cases exercise the "no information loss from Go big to
Scheme" mandate. Each Go-stdlib behavior must have a Scheme path
that surfaces the same information.

### Real domain — `Float64()` accuracy mirror

| Input (Scheme)                              | Go semantics                                                  | Expected `(inexact-with-accuracy n)` |
|---------------------------------------------|---------------------------------------------------------------|--------------------------------------|
| `7`                                         | `int64(7)` → `7.0`; round-trips exactly                        | `(values 7.0 'exact)`                |
| `7.0`                                       | Identity                                                       | `(values 7.0 'exact)`                |
| `1/3`                                       | `(*big.Rat)(1/3).Float64()` → `(0.333..., exact=false)`; round-trip yields `r2 < 1/3` → `Below` | `(values 0.333... 'below)`           |
| `(expt 10 100)`                             | `(*big.Float)(10^100).Float64()` → `(+Inf, big.Above)`         | `(values +inf.0 'above)`             |
| `(- (expt 10 100))`                         | `(*big.Float)(-10^100).Float64()` → `(-Inf, big.Below)`        | `(values -inf.0 'below)`             |
| `+nan.0`                                    | Identity (NaN propagates per Q-6 contract)                     | `(values +nan.0 'exact)`             |
| `+inf.0`                                    | Identity (a finite-typed Float holding +Inf stays +Inf)        | `(values +inf.0 'exact)`             |
| `-inf.0`                                    | Identity                                                       | `(values -inf.0 'exact)`             |
| `1.5e308` (close to `math.MaxFloat64`)      | Round-trips exactly                                            | `(values 1.5e308 'exact)`            |
| `(expt 2 1024)` (finite BigInteger just over `math.MaxFloat64`) | Saturates to `+Inf`; accuracy `Above` (distinct from `+inf.0` input — the *finite* input *overflowed*) | `(values +inf.0 'above)`             |
| `(* 0 (expt 10 100))`                       | Exact zero; converts to `0.0`                                  | `(values 0.0 'exact)`                |
| `+nan.0+1.0i` (NaN real, finite imag)       | Real part identity; complex with non-zero imag → `isReal=#f`   | `(values +nan.0 'exact)` (real-only) — N.B. `inexact-with-accuracy` real-domain primitive returns this 2-value form for the real-domain coercion; the complex-domain primitive `complex-inexact-with-accuracy` returns `(values +nan.0+1.0i 'exact 'exact)` |

### Complex domain — per-component accuracy

| Input (Scheme)                              | Expected `(inexact-with-accuracy n)`                  |
|---------------------------------------------|-------------------------------------------------------|
| `3+4i`                                      | `(values 3.0+4.0i 'exact 'exact)`                     |
| `(make-rectangular 1/3 1/7)`                | `(values 0.333...+0.142...i 'below 'below)`           |
| `(make-rectangular (expt 10 100) 1)`        | `(values +inf.0+1.0i 'above 'exact)`                  |
| `(make-rectangular 1 (expt 10 100))`        | `(values 1.0+inf.0i 'exact 'above)`                   |

### Predicate

| Input                                       | `(inexact-lossless? n)` |
|---------------------------------------------|-------------------------|
| `7`                                         | `#t`                    |
| `1/3`                                       | `#f`                    |
| `(expt 10 100)`                             | `#f`                    |
| `3+4i`                                      | `#t`                    |
| `(make-rectangular 1/3 1)`                  | `#f`                    |
| `+nan.0`                                    | `#t` (per Q-6: NaN identity is mechanically "lossless"; semantically caller must screen with `nan?`) |
| `+inf.0`                                    | `#t` (per Q-6: same logic; caller screens with `infinite?`) |
| `+nan.0+1.0i`                               | `#t` (both components identity; the complex Real flag is `#t` since the imag is finite-and-zero-only-when-needed) — N.B. the predicate uses complex128 semantics, so `+nan.0+0.0i` is `#t` (no imag dropped) but `+nan.0+1.0i` is `#t` ONLY because both components are present in the complex128 (NaN real, 1.0 imag both representable). |

### FFI behavior

| Go function signature        | Scheme call                                     | Pre-Phase-2 result            | Post-Phase-2 result                          |
|------------------------------|-------------------------------------------------|-------------------------------|----------------------------------------------|
| `func(x float64) ...`        | called with `(* 1.5 1e10)` `*Float`             | succeeds                       | succeeds (no change)                         |
| `func(x float64) ...`        | called with `(/ 7 2)` `*Rational(7/2)`          | succeeds (was already accepted) | succeeds (no change — 7/2 = 3.5 is exact)    |
| `func(x float64) ...`        | called with `(* 2 (expt 10 100))` `*BigFloat`   | errors with `ErrNotAReal`     | errors with `ErrLossyConversion` (Above)      |
| `func(x float64) ...`        | called with a BigFloat that fits in float64     | errors with `ErrNotAReal`     | **succeeds** (widening)                       |
| `func(x complex128) ...`     | any input                                       | **registration fails** (no converter today) | succeeds with full per-component accuracy check |
| As above, with `WithLossyConversionsAllowed()` engine option | any input causing precision loss | n/a | **succeeds silently** (opt-in lossy path) |

## Done definition

Phase 1 done when:
- `werr.ErrLossyConversion` sentinel defined and listed in `werr/CLAUDE.md`.
- `values.SymbolAccuracyBelow`, `SymbolAccuracyExact`, `SymbolAccuracyAbove` defined.
- `values.ToFloat64WithAccuracy`, `ToFloat64Lossless` exported.
- `values.Complex128Result` struct exported with named `Value`,
  `RealAcc`, `ImagAcc` fields.
- `values.ToComplex128WithAccuracy`, `ToComplex128Lossless` exported.
- `(*BigFloat).Float64()` renamed to `Float64Truncated()`; new
  `Float64WithAccuracy()` method added; 13 production call sites
  migrated; grep confirms no remaining `(*BigFloat).Float64()` hits.
- `NumericTypeSpec` `toFloat64`/`toComplex128` fields **replaced**
  by `toFloat64WithAccuracy`/`toComplex128WithAccuracy`; per-kind
  helpers registered for all 7 kinds; `promotion.go`'s
  `NumberToFloat64`/`NumberToComplex128` migrated to consume the
  new spec methods.
- Round-trip + boundary test suite passes; the acceptance table
  above is the test-case source.

Phase 2 done when:
- FFI `reflect.Float64` path consults `ToFloat64Lossless`.
- FFI `reflect.Complex128` path added, consults `ToComplex128Lossless`.
- `wile.WithLossyConversionsAllowed()` engine option works.
- `registry/helpers/value_conv.go::ToFloat64` tightened
  (BigFloat widening per Q-5).
- Behavior changes documented in CHANGELOG.

Phase 3 done when:
- All four primitives (`inexact-lossless?`, `inexact-accuracy`,
  `inexact-with-accuracy`, `complex-inexact-with-accuracy`)
  shipped, documented, tested.
- Integration test exercises all four from Scheme.
- The acceptance-test table above passes when expressed as
  table-driven `RunSchemeCode` tests.

## Cross-references

- `memory/2026-05-14-numeric-registry-design.md` — Phase 3 of
  values-SR. Q-i resolved C3 (conservative); this plan fills the
  precision-loss gap left by that resolution.
- `memory/2026-05-14-numeric-registry-impl.md` — established the
  `NumericTypeSpec` shape (PR #752 merged 2026-05-13). This plan
  replaces the spec's `toFloat64`/`toComplex128` fields with
  loss-signal-aware variants; no `ToFloat64Lossy` involved (the
  earlier proposal was retracted — see "Note on `ToFloat64Lossy`"
  in the conversion-helpers section above).
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

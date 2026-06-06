# Numeric loss signals — implementation plan

**Date**: 2026-05-14 (initial); refined for impl detail 2026-05-14.
**Status**: **Complete — all three PRs merged.** See
  "Post-implementation outcome" at the end of this document for
  shipped-vs-planned deltas.

  | PR  | Branch                                          | Merge commit | GitHub PR |
  |-----|-------------------------------------------------|--------------|-----------|
  | 1   | `feat/values-sr-phase4-loss-signals-go`         | `9d96a56d` (head) + #753 merge | [#753](https://github.com/aalpar/wile/pull/753) |
  | 2   | `feat/values-sr-phase4-loss-signals-ffi`        | `45295bdb`   | [#754](https://github.com/aalpar/wile/pull/754) |
  | 3   | `feat/values-sr-phase4-loss-signals-scheme`     | `a965d5ae`   | [#755](https://github.com/aalpar/wile/pull/755) |

  Numeric-registry prerequisite shipped earlier in PR #752
  (commit `082836d1`, merged 2026-05-13).
**Design source**: `memory/2026-05-14-numeric-loss-signals-design.md`
  (refined 2026-05-14; resolutions: Q-1 saturate ±Inf, Q-2
  real-triple + per-component complex, Q-3 `extensions/math/`,
  Q-4 engine-level opt-in, Q-5 yes-tighten-helpers, Q-6 NaN/Inf
  documented as `big.Exact` identity).
**Branches** (matched established `feat/values-sr-phase<N>-<topic>`
convention — see PR #747 phase0, #748 phase1-mutex, #749
phase2-port-unification, #750 phase3-numeric-registry-design):
  - PR 1: `feat/values-sr-phase4-loss-signals-go` (merged + deleted)
  - PR 2: `feat/values-sr-phase4-loss-signals-ffi` (merged + deleted)
  - PR 3: `feat/values-sr-phase4-loss-signals-scheme` (merged + deleted)

All three branched from master after each preceding PR merged.
**Prior-art templates (cite these when writing code)**:
  - **Multi-value-return primitive**: `floor/`, `truncate/`,
    `exact-integer-sqrt` — all in `extensions/math/`. Each uses
    `mc.SetValues(v1, v2)` and omits `ReturnType` in
    `PrimitiveSpec`.
  - **Engine option pattern**: `WithContractEnforcement()` in
    `options.go:110-114` (boolean flag on `*engineConfig`).
  - **Per-kind dispatch via registry**: the post-merge numeric
    registry pattern — every type file's `init()` block calls
    `registerNumericSpec(KindXxx, NumericTypeSpec{...})`.
  - **FFI argument converter**: `ffi_arg_converters.go:42-96`
    — the current `reflect.Int64` and `reflect.Float64` cases
    show the closure-capture idiom.
  - **PrimitiveSpec with docstring + keywords**:
    `extensions/math/register.go:75-77` (the `expt` entry).

## Sequence overview

| PR | Scope                                                                                    | Bench gate | Est. delta   |
|----|------------------------------------------------------------------------------------------|------------|--------------|
| 1  | Go infrastructure: sentinel + accuracy symbols + per-kind helpers + values/ exports + registry extension + **`(*BigFloat).Float64()` → `Float64Truncated()` rename plus new `Float64WithAccuracy()` method (13 internal call-site migrations)** | yes — verify cold-path discipline | +415 / −25  |
| 2  | FFI tightening (Float64 + Complex128 paths) + `WithLossyConversionsAllowed` engine option + `helpers.ToFloat64` Q-5 **tightening** (was silently-truncates; now errors on loss) | yes — FFI conversion hot-ish | +180 / −50 |
| 3  | Four Scheme primitives in `extensions/math/prim_conversion.go` + docstrings + tests | no (cold-path primitives) | +280 / −0   |

Cumulative net: **≈ +805 LOC** (was ≈ +790 before the BigFloat
hygiene fix was folded into PR 1). Larger than typical because the
plan adds new public APIs at three layers (Go helpers, FFI
converters, Scheme primitives) with full no-information-loss
test coverage, plus the BigFloat API rename eliminates a recurring
landmine for future readers.

## PR 1 — Go infrastructure

### Goal

Lay the Go-side foundation. No behavior change outside the new
helpers; no FFI change; no Scheme primitives yet. Other PRs
consume what this PR exports.

### Concrete API contract (commit to before writing code)

**Four** exported functions in `values/` (the cross-package surface).
The earlier `ToFloat64Lossy` variant was dropped during refinement.
Returns use a **hybrid shape**: `ToFloat64WithAccuracy` returns
positional 4-tuple (slot types disambiguate roles); `ToComplex128WithAccuracy`
returns a `Complex128Result` struct (two `big.Accuracy` slots of
the same type would otherwise admit silent realAcc/imagAcc swap
bugs). Design rationale + rejected alternatives documented at
`memory/2026-05-14-numeric-loss-signals-design.md` §"Decision record:
return shape — hybrid (positional + struct)".

| Function | Signature | Behavior |
|----------|-----------|----------|
| `ToFloat64WithAccuracy` | `func(n Number) (float64, big.Accuracy, bool, error)` | Primary. Returns `(value, accuracy, isReal, err)`. The `err` slot is `ErrNotANumber` (wrapped) only for the defensive nil-Number case (the type system forbids passing a non-Number). Lossy-allowed callers (FFI under the engine flag) consume this and discard `(acc, isReal, err)`. |
| `ToFloat64Lossless` | `func(n Number) (float64, error)` | Wraps `WithAccuracy`. Returns `ErrLossyConversion` (wrapped, message names direction) if `acc != big.Exact OR !isReal`. **FFI strict-path consumer.** Returns the raw float64 since callers in strict mode don't need the accuracy slot. |
| `ToComplex128WithAccuracy` | `func(n Number) (Complex128Result, error)` | Primary for complex domain. Returns `Complex128Result{Value, RealAcc, ImagAcc}`. The `err` slot is `ErrNotANumber` for the defensive nil-Number case. For real-only inputs, `res.ImagAcc == big.Exact`. |
| `ToComplex128Lossless` | `func(n Number) (complex128, error)` | Wraps `WithAccuracy`. Returns `ErrLossyConversion` if either component's accuracy is non-Exact. Returns raw `complex128` in strict mode. |

Plus the exported result type:

```go
// Complex128Result captures complex-domain conversion with
// per-component accuracy. Field-named so RealAcc/ImagAcc swaps
// are caught at compile time.
type Complex128Result struct {
    Value   complex128   // complex128 representation
    RealAcc big.Accuracy // Below / Exact / Above for real component
    ImagAcc big.Accuracy // Below / Exact / Above for imaginary component
}
```

**API surface decisions**:

1. **No `ToFloat64Lossy` / `ToComplex128Lossy`.** The earlier
   refinement had a `ToFloat64Lossy(n) (float64, bool)` wrapper
   that panicked on non-Number. Dropped because:
   - The sole would-be caller (the FFI Float64 converter under
     `WithLossyConversionsAllowed`) already type-validates `n` via
     `v.(values.Number)` *before* reaching the helper — the panic
     path is dead.
   - There is no symmetric `ToComplex128Lossy` and adding one for
     parity would inflate the public API further without a real
     caller.
   - "Panic on non-Number" is an asymmetric failure mode versus
     the other helpers' wrapped-error returns; consolidating on
     `WithAccuracy + discard` removes the asymmetry.

   The FFI lossy-allowed path now reads
   `f, _, _, _ := values.ToFloat64WithAccuracy(n)` — same one-line
   shape, one fewer exported symbol, no panic site to maintain.

2. **Hybrid return shape — positional for float64, struct for
   complex128.** The choice is per-helper, applying the rule
   "positional when slot types disambiguate roles; struct when
   adjacent slots share a type."
   - `ToFloat64WithAccuracy`: four distinct slot types (`float64`,
     `big.Accuracy`, `bool`, `error`). No swap risk; positional
     preserves the FFI lossy-allowed discard idiom
     `f, _, _, _ := values.ToFloat64WithAccuracy(n)`.
   - `ToComplex128WithAccuracy`: two `big.Accuracy` slots (real,
     imag) of the same type. Positional would admit silent
     `RealAcc/ImagAcc` swap bugs at any call site. `Complex128Result`
     struct catches the swap at compile time.

   Alternatives considered and rejected:
   - **All-positional** (earlier iteration): cheaper API surface
     and consistent discard idiom across both helpers, but admits
     the realAcc/imagAcc swap bug class — caught only by the
     three-layer integration test, not by per-layer unit tests.
   - **All-struct** (intermediate iteration): consistent shape,
     field-level godoc, attached methods, but loses the discard
     idiom on the high-volume FFI float64 path and adds a
     translation step from the positional per-kind closure to
     the struct public API for every consumer of the float
     helper.

   Full decision record at design plan §"Decision record: return
   shape — hybrid (positional + struct)". Revisit triggers tracked
   at `TODO.md` § "Loss-signals API follow-ups".

3. **`isReal` (renamed from `real`)** — the boolean slot in the
   per-kind helper return tuple and in the values/ API. Renamed
   to avoid shadowing Go's predeclared `real(c)` complex-projection
   function (which is called from adjacent files like
   `values/complex.go:56,86`). Apply the rename throughout
   stubs, tests, and PR-3 primitive implementations.

4. **Rename existing `LookupNumericSpec` → `Lookup`.** PR-1
   renames the registry getter introduced by PR #752
   (`values/numeric_registry.go:146`) from `LookupNumericSpec` to
   `Lookup`. Verified call sites (all inside `values/`):
   `promotion.go:337,350` and `numeric_tower.go:106,142` plus 5
   test sites in `numeric_registry_test.go`. **No external
   consumers** outside the package. The rename is cheap, restores
   symmetry with the rest of the registry vocab (the registry is
   the only `Lookup` site in `values/`), and lets the new helpers
   in this plan read `spec := Lookup(n.Kind())` throughout.

### Acceptance criteria for the API contract

Every exported function in the table:
- Has a `// Doc` comment that names the return-value semantics
  and the no-information-loss contract explicitly.
- Returns errors only via `werr.WrapForeignErrorf(...)` — never
  `fmt.Errorf`, never bare sentinels.
- Has a corresponding row in `values/conversion_test.go`'s
  table-driven test for every kind × every accuracy outcome.

### Files added

- `values/conversion.go` — new file housing the package-level
  public helpers (the 4 functions in the API contract table
  above) plus the `Complex128Result` exported struct. ~160 LOC.
- `values/conversion_test.go` — table-driven tests covering the
  design's acceptance table (~250 LOC; see test-shape stub below).
- `values/symbols_accuracy.go` — three global accuracy symbols
  (`SymbolAccuracyBelow`, `SymbolAccuracyExact`,
  `SymbolAccuracyAbove`) + the `BigAccuracyToSymbol` converter.
  ~30 LOC. Justification for separate file: keeps the singleton
  symbols grouped (matches `*Symbol` singletons in other files
  like `mutex.go`'s `SymbolMutexNotOwned`).

### Files modified

| File                              | Change                                                                                                               |
|-----------------------------------|----------------------------------------------------------------------------------------------------------------------|
| `werr/werr.go`                    | New `ErrLossyConversion = NewStaticError("lossy conversion")` — alphabetically positioned among existing sentinels.  |
| `werr/CLAUDE.md`                  | Document the new sentinel in the inventory table (one row).                                                          |
| `values/numeric_registry.go`      | **Replace** `toFloat64`/`toComplex128` struct fields with `toFloat64WithAccuracy`/`toComplex128WithAccuracy` (see code stub below). Update `registerNumericSpec` validation to require the two replacement fields non-nil. **Rename `LookupNumericSpec` → `Lookup`** (4 production + 5 test call sites in `values/`; no external consumers). |
| `values/integer.go`               | **Replace** `integerToFloat64` with `integerToFloat64WithAccuracy`; add `integerToComplex128WithAccuracy`. Old `integerToComplex128` deleted (replaced by WithAccuracy variant). Bind both in `registerNumericSpec(KindInteger, …)`. |
| `values/big_integer.go`           | Same shape; uses `new(big.Float).SetInt(p.value).Float64()` for native accuracy.                                     |
| `values/float.go`                 | Same shape; identity → `big.Exact`; `(complex(p.Value, 0), Exact, Exact)` for complex helper.                        |
| `values/big_float.go`             | Same per-kind shape (consults `p.IsNaN()` first, returns `math.NaN(), big.Exact, true`; else `p.value.Float64()`). **Plus the BigFloat API hygiene refactor**: rename `(*BigFloat).Float64()` → `Float64Truncated()`, add `Float64WithAccuracy() (float64, big.Accuracy)`. See "BigFloat API hygiene" subsection. |
| `values/rational.go`              | Same shape; direction-recovery via `new(big.Rat).SetFloat64(f).Cmp(p.value)`.                                        |
| `values/complex.go`               | Same shape; real part is identity (Float component) → `Exact`; isReal flag = `imag(p.Value) == 0`.                     |
| `values/big_complex.go`           | Same per-kind shape (per-component accuracy for complex helper). **Plus 6 internal call sites** (`big_complex.go:175,183,487,488,532,533`) migrate `toBigFloat(...).Float64()` → `toBigFloat(...).Float64Truncated()`. |
| `values/promotion.go`             | **Migrate** `LookupNumericSpec(n.Kind()).ToFloat64(n)` call at line 337 to use new helper: read 4-tuple, synthesize `ErrNotAReal` when `!isReal`. Migrate `LookupNumericSpec(n.Kind()).ToComplex128(n)` at line 350 to use WithAccuracy variant (discard the two accuracy slots — these are hot-path closures that don't need them). |
| `values/numeric_tower.go`         | **Migrate** the `LookupNumericSpec(n.Kind()).IsAlwaysExact()` site at line 142 — no change (different field). Verify line 106 (`SimplifyDown`) needs no migration. Other call sites: review and migrate as required.                                                                                                                                                                                              |
| `values/numeric_kind.go`          | Update the ADDING-A-NEW-NUMERIC-TYPE guide comment to reflect the replaced spec fields (two new `WithAccuracy` fields replacing the old single-purpose ones).                                                                                                                                                                                                                                                            |
| `internal/parser/parser_number.go` | 4 call-site renames: `v.RealAsBigFloat().Float64()` / `.ImagAsBigFloat().Float64()` → `.Float64Truncated()` (lines 638, 639, 674, 675). Number printing fallback — truncation is the intended semantic. |
| `extensions/math/prim_complex.go`  | 4 call-site renames at lines 207, 208, 238, 239 (magnitude/angle paths). Feed into `math.Hypot` / `math.Atan2` — truncation acceptable. |
| `extensions/math/prim_rounding.go` | 1 call-site rename at line 50 (`v.Float64()` for floor/ceiling on BigFloat). |
| `extensions/math/prim_transcendental.go` | 2 call-site renames at lines 198, 199 (transcendental complex inputs). |

### Code stub: `werr/werr.go` addition

Insert (alphabetically positioned among existing `Err*` declarations):

```go
// ErrLossyConversion is returned when a numeric conversion succeeds
// mechanically but loses precision (e.g., *BigFloat to float64 where
// the magnitude exceeds float64 range, or *BigComplex with non-zero
// imaginary part converted to a real float64).
//
// Distinct from ErrNotANumber (wrong type entirely) and ErrNotAReal
// (right family, wrong subset).
var ErrLossyConversion = NewStaticError("lossy conversion")
```

### Code stub: `values/symbols_accuracy.go` (new file)

```go
package values

import "math/big"

// Accuracy singleton symbols — paraphrase big.Accuracy at the
// Scheme level. Returned by primitives like inexact-accuracy and
// inexact-with-accuracy.
//
// 'below — result < true value (rounded down)
// 'exact — result == true value (lossless)
// 'above — result > true value (rounded up)
var (
    SymbolAccuracyBelow = NewSymbol("below")
    SymbolAccuracyExact = NewSymbol("exact")
    SymbolAccuracyAbove = NewSymbol("above")
)

// BigAccuracyToSymbol maps a Go big.Accuracy to the corresponding
// Scheme singleton symbol. Used by primitives in extensions/math/
// that surface accuracy to Scheme.
func BigAccuracyToSymbol(acc big.Accuracy) *Symbol {
    switch acc {
    case big.Below:
        return SymbolAccuracyBelow
    case big.Above:
        return SymbolAccuracyAbove
    default:
        return SymbolAccuracyExact
    }
}
```

### Code stub: `values/numeric_registry.go` diff

**Replace** `toFloat64`/`toComplex128` fields with the loss-signal-aware
variants + update getters + update validation + rename `LookupNumericSpec`
→ `Lookup`. Single source of truth per kind — the WithAccuracy variants
carry strictly more information than the originals, so the originals
become redundant once internal callers migrate (see Step 5 below).

```go
type NumericTypeSpec struct {
    schemeName    string
    isAlwaysExact bool
    simplifyDown  func(Number) Number

    // toFloat64WithAccuracy REPLACES the previous `toFloat64
    // func(Number) (float64, error)` field. Per-kind dispatch
    // returns the float64 result, the big.Accuracy (Below/Exact/
    // Above), and an isReal flag (false iff input was Complex/
    // BigComplex with non-zero imaginary part — the imaginary
    // information is dropped). The dispatch tuple stays positional
    // at this internal level (cold path; no struct allocation).
    // The values/ public API forwards the same 4-tuple positionally
    // — no wrapping struct. Always non-nil — populated for every
    // kind. Internal callers in promotion.go and numeric_tower.go
    // that previously consumed `toFloat64` now consume this and
    // synthesize ErrNotAReal from `!isReal` at the call site.
    toFloat64WithAccuracy func(Number) (float64, big.Accuracy, bool)

    // toComplex128WithAccuracy REPLACES the previous `toComplex128
    // func(Number) complex128` field. Returns a Complex128Result
    // struct (field-named to prevent realAcc/imagAcc swap bugs at
    // call sites — see design plan §"Decision record: return shape").
    // The closure returns the struct directly rather than positional
    // tuple — eliminates a wrap step at the public API. Always
    // non-nil — populated for every kind. For real-only inputs,
    // res.ImagAcc is trivially big.Exact.
    toComplex128WithAccuracy func(Number) Complex128Result
}

// ToFloat64WithAccuracy is the public getter — dispatches via the
// registered closure for the kind. Equivalent to calling
// values.ToFloat64WithAccuracy(n) but skips the kind lookup since
// the caller already has the spec.
func (p *NumericTypeSpec) ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    return p.toFloat64WithAccuracy(n)
}

func (p *NumericTypeSpec) ToComplex128WithAccuracy(n Number) Complex128Result {
    return p.toComplex128WithAccuracy(n)
}

// The public `ToFloat64` and `ToComplex128` getters from PR #752 are
// DROPPED. Their internal callers (promotion.go:337,350 and
// numeric_tower.go:142) migrate to the WithAccuracy variants per
// Step 5. The full information content of the old getters is
// preserved by `!isReal` (ErrNotAReal trigger) + ignoring acc.

// registerNumericSpec's nil-check sequence is REWRITTEN to require
// the replacement fields non-nil:
//   if spec.toFloat64WithAccuracy == nil { panic(...) }
//   if spec.toComplex128WithAccuracy == nil { panic(...) }
// The previous `toFloat64`/`toComplex128` checks are removed.
// Panic messages follow the existing pattern with werr.WrapForeignErrorf.

// Lookup returns the NumericTypeSpec for the given kind. Renamed
// from `LookupNumericSpec` (PR #752) — the registry is the only
// Lookup site in values/, so the prefix was redundant. All call
// sites are inside the package (promotion.go, numeric_tower.go,
// numeric_registry_test.go); no external consumers. Bounds-checked:
// out-of-range kind panics with ErrNumericRegistry rather than
// producing a Go runtime "index out of range" panic.
func Lookup(kind NumericKind) *NumericTypeSpec {
    if int(kind) >= int(numKinds) {
        panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
            "Lookup: kind %d out of range [0,%d)", kind, numKinds))
    }
    return &numericRegistry[kind]
}
```

### Code stub: per-kind helpers (one file per type)

The seven type files each get the same shape. Concrete bodies:

**`values/integer.go`** (Kind 0; round-trip via int64-compare):

```go
// integerToFloat64WithAccuracy converts an *Integer to float64
// with direction-recovery. Handles the negative-overflow case
// correctly: for int64 values larger in magnitude than 2^53, the
// float64 representation rounds *toward zero*, so:
//   p.Value =  2^53 + 1  →  f =  2^53        →  back = 2^53  < p.Value  → Below
//   p.Value = -2^53 - 1  →  f = -2^53        →  back =-2^53  > p.Value  → Above
// The compare is in int64, NOT float (float comparison would itself
// suffer the rounding being measured).
func integerToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*Integer)
    f := float64(p.Value)
    back := int64(f)
    switch {
    case back == p.Value:
        return f, big.Exact, true
    case back < p.Value:
        return f, big.Below, true
    default:
        return f, big.Above, true
    }
}

func integerToComplex128WithAccuracy(n Number) Complex128Result {
    f, acc, _ := integerToFloat64WithAccuracy(n)
    return Complex128Result{Value: complex(f, 0), RealAcc: acc, ImagAcc: big.Exact}
}
```

**`values/big_integer.go`**:

```go
func bigIntegerToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigInteger)
    f, acc := new(big.Float).SetInt(p.value).Float64()
    return f, acc, true
}

func bigIntegerToComplex128WithAccuracy(n Number) Complex128Result {
    f, acc, _ := bigIntegerToFloat64WithAccuracy(n)
    return Complex128Result{Value: complex(f, 0), RealAcc: acc, ImagAcc: big.Exact}
}
```

**`values/float.go`** (identity):

```go
func floatToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    return n.(*Float).Value, big.Exact, true
}

func floatToComplex128WithAccuracy(n Number) Complex128Result {
    return Complex128Result{Value: complex(n.(*Float).Value, 0), RealAcc: big.Exact, ImagAcc: big.Exact}
}
```

**`values/big_float.go`** (NaN handling first):

```go
func bigFloatToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigFloat)
    if p.IsNaN() {
        return math.NaN(), big.Exact, true  // NaN→NaN identity
    }
    f, acc := p.value.Float64()
    return f, acc, true
}

func bigFloatToComplex128WithAccuracy(n Number) Complex128Result {
    f, acc, _ := bigFloatToFloat64WithAccuracy(n)
    return Complex128Result{Value: complex(f, 0), RealAcc: acc, ImagAcc: big.Exact}
}
```

**`values/rational.go`** (direction-recovery via round-trip):

```go
func rationalToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*Rational)
    f, exact := p.value.Float64()
    if exact {
        return f, big.Exact, true
    }
    // Recover direction: convert f back to big.Rat and compare
    // against the original. SetFloat64 returns nil for NaN/Inf; a
    // Rational cannot produce these from .Float64() unless the
    // numerator/denominator is itself non-finite, which Rational's
    // invariants prevent.
    back := new(big.Rat).SetFloat64(f)
    if back == nil {
        return f, big.Exact, true  // defensive; unreachable in practice
    }
    cmp := back.Cmp(p.value)
    if cmp < 0 {
        return f, big.Below, true
    }
    return f, big.Above, true
}

func rationalToComplex128WithAccuracy(n Number) Complex128Result {
    f, acc, _ := rationalToFloat64WithAccuracy(n)
    return Complex128Result{Value: complex(f, 0), RealAcc: acc, ImagAcc: big.Exact}
}
```

**`values/complex.go`**:

```go
func complexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*Complex)
    return real(p.Value), big.Exact, imag(p.Value) == 0
}

func complexToComplex128WithAccuracy(n Number) Complex128Result {
    return Complex128Result{Value: n.(*Complex).Value, RealAcc: big.Exact, ImagAcc: big.Exact}
}
```

**`values/big_complex.go`** (per-component for complex helper):

```go
// Uses the new (*BigFloat).Float64WithAccuracy() method introduced
// in this PR (see "BigFloat API hygiene" subsection below). The old
// (*BigFloat).Float64() returns-only-float64 wrapper is renamed to
// Float64Truncated to make its lossy semantics explicit in the name.
func bigComplexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigComplex)
    realF, realAcc := toBigFloat(p.real).Float64WithAccuracy()
    return realF, realAcc, p.imag.IsZero()
}

func bigComplexToComplex128WithAccuracy(n Number) Complex128Result {
    p := n.(*BigComplex)
    realF, realAcc := toBigFloat(p.real).Float64WithAccuracy()
    imagF, imagAcc := toBigFloat(p.imag).Float64WithAccuracy()
    return Complex128Result{Value: complex(realF, imagF), RealAcc: realAcc, ImagAcc: imagAcc}
}
```

### BigFloat API hygiene — rename `Float64()` → `Float64Truncated()`, add `Float64WithAccuracy()`

The current `(*BigFloat).Float64() float64` wrapper in `values/big_float.go:100`
silently discards the `big.Accuracy` that the underlying `(*big.Float).Float64()`
returns. The name `Float64()` does not warn readers that information is being
dropped — every caller has to read the body to know. This is the kind of trap
that produces incorrect code stubs at planning time (the original BigComplex
helper stub in an earlier draft of this plan called `toBigFloat(p.real).Float64()`
expecting a 2-tuple destructure).

This PR removes the trap by giving each semantic a name that warns or invites
its use:

```go
// values/big_float.go — replace the existing Float64() definition.

// Float64Truncated returns the value as float64, silently rounding when
// the magnitude exceeds float64 precision. Use only when downstream code
// inherently cannot use the accuracy bit (math.Sin/Cos inputs, FNV hash
// seeds, transcendental coercions).
//
// For loss-signal-aware conversion, use Float64WithAccuracy() instead.
//
// NaN handling: a BigFloat with the NaN flag set returns math.NaN().
func (p *BigFloat) Float64Truncated() float64 {
    if p.nan {
        return math.NaN()
    }
    f, _ := p.value.Float64()
    return f
}

// Float64WithAccuracy returns the value as float64 along with Go's
// big.Accuracy indicator (Below / Exact / Above). Mirrors the stdlib
// (*big.Float).Float64() signature directly; the NaN flag is surfaced
// as (math.NaN(), big.Exact) since NaN→NaN is bit-pattern identity.
//
// Use this whenever the caller can reasonably act on the accuracy bit
// (precision-aware conversions, audit trails, the new ToFloat64WithAccuracy
// public helper in values/conversion.go).
func (p *BigFloat) Float64WithAccuracy() (float64, big.Accuracy) {
    if p.nan {
        return math.NaN(), big.Exact
    }
    return p.value.Float64()
}
```

**Migration of existing callers** (verified 2026-05-14 — 13 production
sites, all internal to this workspace):

| File:line | Current call | New call |
|-----------|--------------|----------|
| `values/big_float.go:120` (HashCode) | `p.Float64()` | `p.Float64Truncated()` |
| `values/big_float.go:153` (`bigFloatToFloat64`) | `n.(*BigFloat).Float64()` | DROPPED — this whole helper is replaced by `bigFloatToFloat64WithAccuracy` in this PR |
| `values/big_float.go:159` (`bigFloatToComplex128`) | `n.(*BigFloat).Float64()` | DROPPED — replaced by `bigFloatToComplex128WithAccuracy` |
| `values/big_complex.go:175,183,487,488,532,533` | `toBigFloat(...).Float64()` | `toBigFloat(...).Float64Truncated()` |
| `internal/parser/parser_number.go:638,639,674,675` | `v.RealAsBigFloat().Float64()` | `v.RealAsBigFloat().Float64Truncated()` |
| `extensions/math/prim_complex.go:207,208,238,239` | `v.RealAsBigFloat().Float64()` | `v.RealAsBigFloat().Float64Truncated()` |
| `extensions/math/prim_rounding.go:50` | `v.Float64()` | `v.Float64Truncated()` |
| `extensions/math/prim_transcendental.go:198,199` | `v.RealAsBigFloat().Float64()` | `v.RealAsBigFloat().Float64Truncated()` |
| `registry/helpers/value_conv.go:38,82,109` (`n.BigFloatValue().Float64()`) | unchanged in PR 1 | replaced wholesale in PR 2 (Q-5 tightening) |

Test-file call sites are migrated mechanically in the same commit
(estimate ~6-10 sites; verify via `grep -rn "\.Float64()" values/ internal/ extensions/ registry/ --include='*_test.go' | grep -v 'big\.Float\|big\.Int\|big\.Rat'`).

**Why not also delete `Float64Truncated`** (force every call site
through `Float64WithAccuracy` + explicit discard)? The discard pattern
`f, _ := p.Float64WithAccuracy()` is two characters longer than `f := p.Float64Truncated()`,
and the *named* form is more self-documenting. Truncated callers are
deliberately discarding information; a name that says so is honest.
The two-method shape mirrors the new `ToFloat64Lossless` /
`ToFloat64WithAccuracy` pair the plan introduces at the package level.

**Acceptance addition for PR 1** (folds into the existing list):
- Grep `(*BigFloat).Float64()` returns zero production hits after migration.
- Each `Float64Truncated()` call site has been audited and the truncation
  is genuinely acceptable for that site (transcendental input, hash seed,
  printing, etc. — no site where the accuracy bit would matter).
- LOC delta: roughly +25 (two methods + comments) / −10 (deleted
  `Float64()` wrapper + the two `bigFloatTo*` helpers superseded by
  `WithAccuracy` variants). Net ≈ +15 to PR 1's previously estimated +400.

**Registration in each type's `init()` block** — add two fields to
the existing `registerNumericSpec` call:

```go
// Excerpt from values/integer.go's existing init() (post-PR-#750):
registerNumericSpec(KindInteger, NumericTypeSpec{
    // ... 5 existing field initializers ...
    toFloat64WithAccuracy:    integerToFloat64WithAccuracy,
    toComplex128WithAccuracy: integerToComplex128WithAccuracy,
})
```

### Code stub: `values/conversion.go` (new file)

```go
// Copyright 2026 Aaron Alpar
// ... license header per project convention ...

package values

import (
    "math/big"

    "github.com/aalpar/wile/werr"
)

// ToFloat64WithAccuracy is the primary loss-signal-aware conversion
// helper. ToFloat64Lossless wraps it.
//
// Returns 4-tuple positional:
//   - f:      the float64 representation, saturated to ±Inf for
//             overflow per Go (*big.Float).Float64() semantics
//   - acc:    Below/Exact/Above per Go big.Accuracy semantics:
//               Below: f < true value
//               Exact: f == true value
//               Above: f > true value
//   - isReal: false iff n was Complex/BigComplex with non-zero
//             imaginary part (the imaginary information is dropped
//             — caller should consult ToComplex128WithAccuracy
//             for full complex semantics)
//   - err:    ErrNotANumber (wrapped) on a defensive nil-Number
//             input. The signature is `n Number`, so a non-Number
//             value cannot be passed — the nil case is the only
//             reachable error path.
//
// No information loss from the Go big package is introduced by
// this helper — every signal Go's stdlib surfaces is exposed
// through the four positional slots.
//
// **NaN/Inf contract** (per design Q-6 resolution): NaN inputs
// return (NaN, Exact, true, nil) — NaN→NaN is bit-pattern identity
// in IEEE 754, so accuracy is Exact mechanically. Callers checking
// "is this a meaningful real number?" must screen finiteness
// independently via math.IsNaN(f) and math.IsInf.
// A *true* infinite input (*Float(math.Inf(1))) returns
// (+Inf, Exact, true, nil) — same identity logic. Finite values
// that overflow during conversion return (±Inf, Above|Below, true, nil).
//
// FFI lossy-allowed callers use this directly via the discard
// pattern `f, _, _, _ := ToFloat64WithAccuracy(n)`; FFI strict
// callers use ToFloat64Lossless.
func ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error) {
    if n == nil {
        return 0, big.Exact, true, werr.WrapForeignErrorf(
            werr.ErrNotANumber, "ToFloat64WithAccuracy: nil input")
    }
    spec := Lookup(n.Kind())
    f, acc, isReal := spec.ToFloat64WithAccuracy(n)
    return f, acc, isReal, nil
}

// ToFloat64Lossless is the FFI-strict convenience wrapper.
// Returns the raw float64 (callers in strict mode don't need the
// accuracy slot — they just want the value or an error). Returns
// ErrLossyConversion (wrapped, with direction info) if the
// conversion would lose precision OR drop the imaginary part.
func ToFloat64Lossless(n Number) (float64, error) {
    f, acc, isReal, err := ToFloat64WithAccuracy(n)
    if err != nil {
        return 0, err
    }
    if acc != big.Exact {
        return f, werr.WrapForeignErrorf(werr.ErrLossyConversion,
            "ToFloat64Lossless: %T rounded %s (lost precision)", n, acc)
    }
    if !isReal {
        return f, werr.WrapForeignErrorf(werr.ErrLossyConversion,
            "ToFloat64Lossless: %T has non-zero imaginary part dropped", n)
    }
    return f, nil
}

// Complex128Result captures complex-domain conversion with
// per-component accuracy. Field-named so RealAcc/ImagAcc swaps
// are caught at compile time, not surfaced only as wrong output.
// See design plan §"Decision record: return shape — hybrid".
type Complex128Result struct {
    Value   complex128
    RealAcc big.Accuracy
    ImagAcc big.Accuracy
}

// ToComplex128WithAccuracy is the primary complex-domain helper.
// Returns a Complex128Result struct (named fields prevent
// realAcc/imagAcc swap bugs at call sites; same-type adjacency
// would otherwise admit silent swaps the compiler can't catch).
//
// For real-only inputs (Integer/BigInteger/Float/BigFloat/Rational),
// res.ImagAcc is always big.Exact.
//
// For nil-Number defensive input, returns ErrNotANumber.
func ToComplex128WithAccuracy(n Number) (Complex128Result, error) {
    if n == nil {
        return Complex128Result{RealAcc: big.Exact, ImagAcc: big.Exact},
            werr.WrapForeignErrorf(werr.ErrNotANumber,
                "ToComplex128WithAccuracy: nil input")
    }
    spec := Lookup(n.Kind())
    return spec.ToComplex128WithAccuracy(n), nil
}

// ToComplex128Lossless returns the raw complex128, or
// ErrLossyConversion if either component's accuracy is non-Exact.
func ToComplex128Lossless(n Number) (complex128, error) {
    res, err := ToComplex128WithAccuracy(n)
    if err != nil {
        return 0, err
    }
    if res.RealAcc != big.Exact {
        return res.Value, werr.WrapForeignErrorf(werr.ErrLossyConversion,
            "ToComplex128Lossless: %T real part rounded %s (lost precision)", n, res.RealAcc)
    }
    if res.ImagAcc != big.Exact {
        return res.Value, werr.WrapForeignErrorf(werr.ErrLossyConversion,
            "ToComplex128Lossless: %T imaginary part rounded %s (lost precision)", n, res.ImagAcc)
    }
    return res.Value, nil
}
```

### Code stub: `values/conversion_test.go` (new file)

Table-driven; mirrors the design's acceptance table directly.

```go
package values

import (
    "math"
    "math/big"
    "testing"

    qt "github.com/frankban/quicktest"
)

func TestToFloat64WithAccuracy(t *testing.T) {
    cases := []struct {
        name       string
        input      Number
        wantValue  float64
        wantAcc    big.Accuracy
        wantIsReal bool
    }{
        // Real-domain — every kind × every accuracy outcome.
        // Float64 literals computed via Go expressions to avoid
        // hand-transcribed-ULP errors (e.g. `1.0/3.0` not `0.333...`).
        // ---- Integer ----
        {"integer-exact-zero",          NewInteger(0),                                        0.0,                    big.Exact, true},
        {"integer-exact-small",         NewInteger(7),                                        7.0,                    big.Exact, true},
        {"integer-exact-negative",      NewInteger(-42),                                      -42.0,                  big.Exact, true},
        {"integer-overflow-positive",   NewInteger(int64(1)<<53 + 1),                         float64(int64(1)<<53),  big.Below, true},
        {"integer-overflow-negative",   NewInteger(-(int64(1)<<53 + 1)),                      -float64(int64(1)<<53), big.Above, true},
        {"integer-minint64",            NewInteger(math.MinInt64),                            float64(math.MinInt64), big.Exact, true},

        // ---- BigInteger ----
        {"big-integer-exact",           NewBigIntegerFromInt64(1234),                          1234.0,                big.Exact, true},
        {"big-integer-overflow-pos",    NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(100), nil)),
                                                                                                math.Inf(1),          big.Above, true},
        {"big-integer-overflow-neg",    NewBigInteger(new(big.Int).Neg(new(big.Int).Exp(big.NewInt(10), big.NewInt(100), nil))),
                                                                                                math.Inf(-1),         big.Below, true},
        {"big-integer-near-mantissa-boundary", NewBigInteger(new(big.Int).Lsh(big.NewInt(1), 54)),
                                                                                                math.Ldexp(1, 54),    big.Exact, true},

        // ---- Float ----
        {"float-identity",              NewFloat(3.5),                                         3.5,                   big.Exact, true},
        {"float-zero",                  NewFloat(0.0),                                         0.0,                   big.Exact, true},
        {"float-negative-zero",         NewFloat(math.Copysign(0, -1)),                        math.Copysign(0, -1),  big.Exact, true},
        {"float-positive-inf",          NewFloat(math.Inf(1)),                                 math.Inf(1),           big.Exact, true},
        {"float-negative-inf",          NewFloat(math.Inf(-1)),                                math.Inf(-1),          big.Exact, true},
        {"float-nan",                   NewFloat(math.NaN()),                                  math.NaN(),            big.Exact, true},
        {"float-maxfloat64",            NewFloat(math.MaxFloat64),                             math.MaxFloat64,       big.Exact, true},
        {"float-smallest-subnormal",    NewFloat(math.SmallestNonzeroFloat64),                 math.SmallestNonzeroFloat64, big.Exact, true},

        // ---- BigFloat ----
        {"big-float-finite-exact",      NewBigFloatFromFloat64(2.5),                           2.5,                   big.Exact, true},
        {"big-float-overflow-positive", bigFloatFromString(t, "1e500"),                        math.Inf(1),           big.Above, true},
        {"big-float-overflow-negative", bigFloatFromString(t, "-1e500"),                       math.Inf(-1),          big.Below, true},
        {"big-float-underflow",         bigFloatFromString(t, "1e-400"),                       0.0,                   big.Below, true},
        {"big-float-nan",               NewBigFloatNaN(),                                      math.NaN(),            big.Exact, true},
        {"big-float-irrational-pi",     bigFloatFromString(t, "3.14159265358979323846"),       math.Pi,               big.Below, true}, // depends on rounding direction; verify

        // ---- Rational ----
        {"rational-exact-half",         NewRational(1, 2),                             0.5,                   big.Exact, true},
        {"rational-exact-quarter",      NewRational(1, 4),                             0.25,                  big.Exact, true},
        {"rational-onethird",           NewRational(1, 3),                             1.0/3.0,               big.Below, true},
        {"rational-twothirds",          NewRational(2, 3),                             2.0/3.0,               big.Above, true},

        // ---- Complex ----
        {"complex-real-zero-imag",      NewComplex(complex(3.0, 0)),                           3.0,                   big.Exact, true},
        {"complex-with-imag",           NewComplex(complex(3.0, 4.0)),                         3.0,                   big.Exact, false}, // imag dropped
        {"complex-nan-real",            NewComplex(complex(math.NaN(), 0)),                    math.NaN(),            big.Exact, true},

        // ---- BigComplex ----
        {"bigcomplex-exact-zero-imag",  NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(0)), 3.0,    big.Exact, true},
        {"bigcomplex-with-imag",        NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)), 3.0,    big.Exact, false},
        {"bigcomplex-real-overflow",    NewBigComplex(NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(500), nil)), NewBigIntegerFromInt64(0)),
                                                                                                math.Inf(1),          big.Above, true},
        {"bigcomplex-real-below-overflow", NewBigComplex(NewBigInteger(new(big.Int).Neg(new(big.Int).Exp(big.NewInt(10), big.NewInt(500), nil))), NewBigIntegerFromInt64(0)),
                                                                                                math.Inf(-1),         big.Below, true},
    }
    for _, tc := range cases {
        t.Run(tc.name, func(t *testing.T) {
            c := qt.New(t)
            f, acc, isReal, err := ToFloat64WithAccuracy(tc.input)
            c.Assert(err, qt.IsNil)
            if math.IsNaN(tc.wantValue) {
                c.Assert(math.IsNaN(f), qt.IsTrue)
            } else {
                c.Assert(f, qt.Equals, tc.wantValue)
            }
            c.Assert(acc, qt.Equals, tc.wantAcc)
            c.Assert(isReal, qt.Equals, tc.wantIsReal)
        })
    }
}

func TestToFloat64Lossless(t *testing.T) {
    losslessCases := []struct {
        name  string
        input Number
        want  float64
    }{
        {"integer", NewInteger(42), 42.0},
        {"rational-half", NewRational(1, 2), 0.5},
    }
    for _, tc := range losslessCases {
        t.Run(tc.name+"-lossless", func(t *testing.T) {
            c := qt.New(t)
            f, err := ToFloat64Lossless(tc.input)
            c.Assert(err, qt.IsNil)
            c.Assert(f, qt.Equals, tc.want)
        })
    }
    // Lossy cases — every one MUST resolve to a concrete fixture
    // before code is written. No `/* TODO */` placeholders survive.
    lossyCases := []struct {
        name  string
        input Number
    }{
        {"rational-onethird", NewRational(1, 3)},
        {"bigfloat-overflow-positive", bigFloatFromString(t, "1e500")},
        {"bigfloat-overflow-negative", bigFloatFromString(t, "-1e500")},
        {"bigfloat-irrational",        bigFloatFromString(t, "3.141592653589793238462643383279")},
        {"complex-with-imag",          NewComplex(complex(1, 1))},
        {"bigcomplex-with-imag",       NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(1))},
    }
    for _, tc := range lossyCases {
        t.Run(tc.name+"-errors", func(t *testing.T) {
            c := qt.New(t)
            _, err := ToFloat64Lossless(tc.input)
            c.Assert(err, qt.IsNotNil)
            c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue)
        })
    }
}

// bigFloatFromString is a test helper that constructs a *BigFloat
// from a Go literal string (uses big.Float.Parse). Lives in the
// values_test.go test helper section.
func bigFloatFromString(t *testing.T, s string) *BigFloat {
    bf, _, err := new(big.Float).SetPrec(256).Parse(s, 10)
    if err != nil {
        t.Fatalf("bigFloatFromString(%q): %v", s, err)
    }
    return &BigFloat{value: bf}
}

// Required additional tests — DO NOT skip with "follow the same
// shape." Each has a distinct contract that requires its own table:

// TestToComplex128WithAccuracy — 5-column table:
//   {name, input, wantValue, wantRealAcc, wantImagAcc}.
//   Assert via struct deep-equals:
//     res, err := ToComplex128WithAccuracy(input)
//     qt.Assert(t, err, qt.IsNil)
//     qt.Assert(t, res, qt.DeepEquals, Complex128Result{
//         Value: wantValue, RealAcc: wantRealAcc, ImagAcc: wantImagAcc})
//   Cover all 7 kinds × {Below, Exact, Above} for each component
//   independently. Imaginary-component Below/Above cases (e.g.,
//   BigComplex(0, 1/3) yields res.ImagAcc=Below) are the highest-
//   value rows.

// TestToComplex128Lossless — split tables analogous to the
// float64 version: lossless (asserts no error) + lossy (asserts
// errors.Is(err, werr.ErrLossyConversion)). Cover the two distinct
// loss paths separately: real-part-lossy, imag-part-lossy.

// TestErrNotANumber — non-Number inputs (NewString("hi"),
// TrueValue, EmptyList, nil) call all four public helpers; assert
// errors.Is(err, werr.ErrNotANumber). One test per helper × per
// non-Number-input row.
```

### Steps

1. **Add the `werr.ErrLossyConversion` sentinel** to
   `werr/werr.go`. Update `werr/CLAUDE.md`'s sentinel inventory
   if it enumerates them.

2. **Add global accuracy symbols** in `values/`:
   ```go
   var (
       SymbolAccuracyBelow = NewSymbol("below")
       SymbolAccuracyExact = NewSymbol("exact")
       SymbolAccuracyAbove = NewSymbol("above")
   )
   ```
   Plus a `BigAccuracyToSymbol(acc big.Accuracy) *Symbol` helper
   for converting Go's enum to the Scheme-facing singleton.

3. **Rewrite `NumericTypeSpec`**: replace the existing
   `toFloat64`/`toComplex128` fields (from PR #752) with
   `toFloat64WithAccuracy`/`toComplex128WithAccuracy`. Replace
   the two `*NumericTypeSpec.ToFloat64`/`ToComplex128` public
   getters with the `WithAccuracy` variants. Tighten
   `registerNumericSpec` validation to require the replacement
   fields non-nil for every kind. Drop the old field validators.

4. **Per-kind helpers + registration**. For each of the seven
   numeric type files: implement `<type>ToFloat64WithAccuracy` and
   `<type>ToComplex128WithAccuracy` as named package-level
   functions; reference them from `registerNumericSpec(...)` in
   the existing `init()`. **Delete** the prior
   `<type>ToFloat64`/`<type>ToComplex128` helpers from the same
   files (they bound to the now-removed struct fields). Per-kind
   details:
   - **Integer**: round-trip via `back := int64(float64(p.Value))`
     and compare as int64 (avoids float-comparison pitfalls).
   - **BigInteger**: delegate to `new(big.Float).SetInt(p.value).Float64()`.
   - **Float**: identity; always `big.Exact`.
   - **BigFloat**: handle `p.IsNaN()` first (return `math.NaN(),
     big.Exact, true` per design); else `p.value.Float64()`.
   - **Rational**: `f, exact := p.value.Float64()`; if `!exact`,
     direction-recover via `new(big.Rat).SetFloat64(f).Cmp(p.value)`.
   - **Complex**: real part is `real(p.Value)` (identity → Exact);
     isReal flag is `imag(p.Value) == 0`.
   - **BigComplex**: real part via `toBigFloat(p.real).Float64()`;
     isReal flag is `p.imag.IsZero()`. For the complex helper:
     per-component accuracy from both `real` and `imag`.

4b. **BigFloat API hygiene (rename + add method).** In
    `values/big_float.go`: rename the existing
    `(*BigFloat).Float64() float64` method to `Float64Truncated()`
    (semantics unchanged — same body, same NaN handling). Add the
    new `Float64WithAccuracy() (float64, big.Accuracy)` method
    (returns the stdlib tuple plus the NaN-flag identity case).
    Migrate the 13 production call sites listed in the
    "BigFloat API hygiene" subsection table — all become
    `.Float64Truncated()`. Run `make build && make test` after
    this step to confirm green before continuing. The new BigFloat
    helper in Step 4 uses `Float64WithAccuracy()` via the
    `bigFloatToFloat64WithAccuracy` body that reads `p.value`
    directly inside `values/`; no chain through the new method
    is required for the per-kind helper.

5. **Migrate internal consumers of the dropped fields.** PR #752
   exposed `*NumericTypeSpec.ToFloat64` (signature `(float64,
   error)`) and `*NumericTypeSpec.ToComplex128` (signature
   `complex128`). The two production wrapper functions consume
   these:
   - `values/promotion.go:336-343` — `NumberToFloat64(n Number) float64`
     currently calls `LookupNumericSpec(n.Kind()).ToFloat64(n)`
     and panics with `ErrNotAReal` on error. **New body**: call
     `Lookup(n.Kind()).ToFloat64WithAccuracy(n)`, panic when
     `!isReal` (synthesizing the same `ErrNotAReal` wrap as
     today), discard the accuracy slot. Note: the existing
     `WrapForeignErrorWithCause` chain loses its `err` cause
     since `isReal == false` is a bool, not an error. Acceptable
     — the panic-site context already names the function and
     concrete type.
   - `values/promotion.go:349-351` — `NumberToComplex128(n Number) complex128`
     currently returns `LookupNumericSpec(n.Kind()).ToComplex128(n)`.
     **New body**: `return Lookup(n.Kind()).ToComplex128WithAccuracy(n).Value`.
     Project the `Value` field — the existing function makes no
     precision claim, and this site is the IEEE-754 guard
     short-circuit. The struct allocation is one heap-or-stack
     per call on a path that fires only for Inf/NaN Float operands.
   Both replacements stay on the same path as today (the IEEE 754
   guard fires only for Inf/NaN Float operands), so per-call
   overhead from the extra return values is bounded. Verify with
   `make bench-gabriel` after the migration.

6. **Implement the public helpers** in `values/conversion.go`
   (4 functions + 1 result struct; `ToFloat64Lossy` dropped — see
   API surface decisions above; hybrid return shape per the design
   plan):
   - **`Complex128Result` struct** (`Value`, `RealAcc`, `ImagAcc`).
     Exported. Field-named to prevent realAcc/imagAcc swap bugs.
   - `ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error)` —
     positional 4-tuple. Dispatches via the registry. Returns
     `ErrNotANumber` on defensive nil-Number input (the type system
     rules out non-Number callers). **Primary API; lossy-allowed
     callers consume this directly and discard `acc`/`isReal`/`err`
     via `f, _, _, _ := ...`.**
   - `ToFloat64Lossless(n Number) (float64, error)` — wraps
     `WithAccuracy`; returns `ErrLossyConversion` (wrapped) if
     `acc != Exact || !isReal`.
   - `ToComplex128WithAccuracy(n Number) (Complex128Result, error)` —
     dispatches; returns `ErrNotANumber` on defensive nil-Number
     input. Consumers access `res.Value`, `res.RealAcc`, `res.ImagAcc`.
   - `ToComplex128Lossless(n Number) (complex128, error)` —
     wraps `WithAccuracy`; returns `ErrLossyConversion` if
     either component's accuracy is non-Exact.

7. **Validation tests** (`values/conversion_test.go`):
   - Table-driven tests over the design's acceptance table.
     `TestToFloat64WithAccuracy` rows assert the positional 4-tuple
     `(f, acc, isReal, err)` against expected.
     `TestToComplex128WithAccuracy` rows assert via
     `qt.DeepEquals` against a `Complex128Result{...}` literal —
     one assertion captures all three fields atomically.
   - Boundary cases: `math.MaxFloat64`, `math.SmallestNonzeroFloat64`,
     `math.MinInt64`, `(*big.Int).Exp(big.NewInt(10), big.NewInt(100), nil)`
     (overflows int64 / float64; check direction).
   - NaN propagation: `*Float(NaN)` round-trips as
     `(NaN, big.Exact, true, nil)`.
   - Rational direction: `1/3` → `'below`; `2/3` → `'above`
     (verified against actual stdlib behavior at test time).
   - Complex direction: `(make-rectangular 1/3 1/7)` returns
     `(c, Below, Below, nil)`.

8. **Registry validation extension**: add a unit test that drives
   `Lookup(k).ToFloat64WithAccuracy` and
   `Lookup(k).ToComplex128WithAccuracy` for each of the seven
   kinds, asserts non-panic and that the return shape is valid.

9. **Bench check**: run `make bench-gabriel` and confirm no hot
   path consults the new helpers (verified by grep). The helpers
   are exported but should only be called by future cold-path
   consumers (PR 2 FFI + PR 3 Scheme primitives). Also verify
   the `promotion.go` migration (Step 5) hasn't introduced
   regression — the IEEE 754 guard's short-circuit is on the
   arithmetic dispatch path.

10. **Lint + CI**: `make lint && make covercheck && make ci` all
    pass.

### Acceptance for PR 1

- `ErrLossyConversion` sentinel defined and grep-able.
- Accuracy symbols defined and grep-able.
- All seven numeric type files register the two new spec fields.
- `Lookup(k).ToFloat64WithAccuracy(n)` returns the expected
  shape for every kind × every representative input.
- The acceptance-table cases from the design pass as Go tests.
- No new code path called from hot-path arithmetic (verified by
  grep — none of `integer.go`'s `Add`/`Subtract`/`Multiply`/`Divide`
  paths call the new helpers).
- **`(*BigFloat).Float64()` no longer exists.** Grep
  `grep -rn '(\*BigFloat).Float64()\b\|\.Float64()' --include='*.go'`
  scoped to `values/ internal/ extensions/ registry/` returns hits
  only for `Float64Truncated()`, `Float64WithAccuracy()`, or
  stdlib `(*big.Float).Float64()` / `(*big.Rat).Float64()` /
  `(*big.Int).Float64()`. Each `Float64Truncated()` call site was
  audited and the truncation semantics are appropriate.
- `make bench-gabriel` geomean within ≤ 0.5% of master.

## PR 2 — FFI tightening + helpers migration

### Goal

Migrate FFI's `reflect.Float64` and `reflect.Complex128` cases to
the new helpers; add the `WithLossyConversionsAllowed()` engine
option; widen `registry/helpers/value_conv.go::ToFloat64` to
accept `*BigFloat`.

### Code stub: `options.go` — `WithLossyConversionsAllowed`

Add alongside `WithContractEnforcement` (the established
boolean-flag-on-engineConfig template):

```go
// WithLossyConversionsAllowed permits FFI converters to silently
// truncate when converting Scheme numerics to fixed-precision Go
// types (float64, complex128). When set, *BigFloat with magnitude
// exceeding float64 range converts to ±math.Inf(0) without error;
// *BigComplex with non-zero imaginary part converts via real-part-
// only or per-component truncation as applicable.
//
// Default (option not set): the FFI converter returns
// werr.ErrLossyConversion (wrapped, with direction info) when any
// precision loss would occur. This is the "fail loud" discipline
// — opt-in is required to suppress.
//
// The option is per-engine; the flag is captured into each FFI
// closure at RegisterFunc time, so calling
// WithLossyConversionsAllowed after some functions have already
// registered does NOT change their behavior.
func WithLossyConversionsAllowed() EngineOption {
    return func(cfg *engineConfig) {
        cfg.lossyConversionsAllowed = true
    }
}
```

Plus the `engineConfig` field (add alongside `contractEnforcement`):

```go
type engineConfig struct {
    // ... existing fields ...
    lossyConversionsAllowed bool
}
```

### Plumbing diagram — where the flag travels

```
NewEngine(ctx, WithLossyConversionsAllowed())
  → opt applies to engineConfig
    cfg.lossyConversionsAllowed = true
  → engine.config.lossyConversionsAllowed inherited
RegisterFunc(name, fn)
  → buildFFISpec(name, fn) reads p.config.lossyConversionsAllowed
    → captures into ffiSpec.lossyAllowed bool
  → spec.makeWrapper() reads spec.lossyAllowed
    → captures into wrapper closure (per-call)
On FFI call:
  → wrapper invokes makeArgConverter's closure
    → closure has captured lossyAllowed
    → if lossyAllowed: ToFloat64WithAccuracy(n) and discard
                       (acc, isReal) — silent truncation
    → else:            ToFloat64Lossless(n) — error on loss
```

`makeArgConverter` itself currently takes `(name, pos, t)` —
extend its signature to accept a per-spec `lossyAllowed bool`,
passed in by `buildFFISpec`. Alternative: store `lossyAllowed` on
`ffiSpec` and pass to `makeWrapper`, which then constructs
converters at-call (slower, no closure caching). **Use the
signature-extension approach** to preserve the per-call cost
profile.

### Code stub: extended `makeArgConverter` signature

`ffi_arg_converters.go` — change the function signature and
thread the flag through to the converter closures that care:

```go
// New signature (was: makeArgConverter(name string, pos int, t reflect.Type))
func makeArgConverter(name string, pos int, t reflect.Type, lossyAllowed bool) (argConverter, error) {
    // ... existing switch on t.Kind() ...

    case reflect.Float64:
        targetType := t
        return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
            n, ok := v.(values.Number)
            if !ok {
                return reflect.Value{}, fmtArgError(name, pos, "number", v)
            }
            var f float64
            if lossyAllowed {
                // Lossy-allowed: consume WithAccuracy, discard
                // (acc, isReal). Type-asserted to Number above, so
                // no error path reachable here.
                f, _, _, _ = values.ToFloat64WithAccuracy(n)
            } else {
                f2, err := values.ToFloat64Lossless(n)
                if err != nil {
                    return reflect.Value{}, werr.WrapForeignErrorf(
                        err,
                        "%s: argument %d: %T", name, pos, v,
                    )
                }
                f = f2
            }
            return reflect.ValueOf(f).Convert(targetType), nil
        }, nil

    case reflect.Complex128:
        targetType := t
        return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
            n, ok := v.(values.Number)
            if !ok {
                return reflect.Value{}, fmtArgError(name, pos, "number", v)
            }
            var c complex128
            if lossyAllowed {
                // Lossy-allowed: consume WithAccuracy struct, project
                // the Value field, discard per-component accuracies.
                // Type-asserted above; the error path is unreachable.
                res, _ := values.ToComplex128WithAccuracy(n)
                c = res.Value
            } else {
                c2, err := values.ToComplex128Lossless(n)
                if err != nil {
                    return reflect.Value{}, werr.WrapForeignErrorf(
                        err,
                        "%s: argument %d: %T", name, pos, v,
                    )
                }
                c = c2
            }
            return reflect.ValueOf(c).Convert(targetType), nil
        }, nil

    // ... other cases unchanged ...
}
```

**Why no `_ = lossless` discard pattern**: the previous draft used
`f, lossless = ToFloat64Lossy(n); _ = lossless` to satisfy the
"declared but not used" check. With `ToFloat64Lossy` dropped, the
pattern collapses to `f, _, _, _ = ToFloat64WithAccuracy(n)` — all
unused slots ignored at the call site directly. One fewer unused-
variable dance.

### Code stub: `buildFFISpec` threading

In `ffi.go`, where `buildFFISpec` calls `makeArgConverter` for
each parameter, pass the captured flag:

```go
func buildFFISpec(name string, fn any, lossyAllowed bool) (*ffiSpec, error) {
    // ... existing logic ...
    for i := 0; i < paramCount; i++ {
        conv, err := makeArgConverter(name, i, fnType.In(i), lossyAllowed)
        if err != nil {
            return nil, err
        }
        // ... existing logic ...
    }
}

// And in (p *Engine).RegisterFunc:
func (p *Engine) RegisterFunc(name string, fn any) error {
    spec, err := buildFFISpec(name, fn, p.config.lossyConversionsAllowed)
    // ... existing logic ...
}
```

Also update `makeCallbackArgConverter` (recursive inner converters
for `func` parameter types) to thread `lossyAllowed` if it builds
inner argument converters of its own. Audit at impl time.

### Code stub: `registry/helpers/value_conv.go::ToFloat64` (Q-5 — **tightening**, not widening)

**Critical framing correction**: an earlier draft of this plan
called Q-5 a "widening." Verified against the actual code (read
`registry/helpers/value_conv.go:72-90` on master): today's
`ToFloat64` accepts every real-numeric kind **including
`*BigFloat`** and silently truncates via the discarded
`big.Accuracy` from `(*big.Float).Float64()`:

```go
// CURRENT (master) — discards accuracy, silently truncates
case *values.BigFloat:
    f, _ := n.BigFloatValue().Float64()   // _ = lossless bit, discarded
    return f, nil
case *values.BigInteger:
    f, _ := new(big.Float).SetInt(n.BigInt()).Float64()  // same
case *values.Rational:
    f, _ := n.Rat().Float64()              // same
```

Q-5 **tightens** this: the same input now errors with
`ErrLossyConversion` when the conversion loses precision. The
behavior change is in the **error direction**, not the
acceptance direction:

| Input | Before Q-5 (master) | After Q-5 |
|-------|---------------------|-----------|
| `*Integer(42)` | succeeds: `42.0` | succeeds: `42.0` (unchanged) |
| `*BigFloat(1.5)` | succeeds: `1.5` (already exact in float64) | succeeds: `1.5` (unchanged) |
| `*BigFloat(2^100)` | **succeeds silently with `+Inf`** | errors with `ErrLossyConversion` (direction `Above`) |
| `*Rational(1/3)` | **succeeds silently with `0.333...`** | errors with `ErrLossyConversion` (direction `Below`) |
| `*Complex(3+0i)` | errors `ErrNotAReal` | errors `ErrNotAReal` (unchanged) |
| `*Complex(3+4i)` | errors `ErrNotAReal` | errors `ErrNotAReal` (unchanged) |

Three of the seven cases change from "silent truncation" to "loud
error on loss." This is consistent with the "fail loud at startup"
discipline; **the existing silent-truncation behavior was the bug
this plan is correcting.**

Replace the current 5-case switch with a delegation to the new
helper:

```go
// ToFloat64 converts a Scheme real number to a Go float64 with
// a lossless guarantee.
//
// Real inputs (*Integer, *BigInteger, *Float, *BigFloat, *Rational)
// that fit float64 exactly succeed; ones that don't fit return
// ErrLossyConversion (wrapped, message names direction Below/Above).
// Complex inputs (*Complex, *BigComplex) return ErrNotAReal —
// the helper is real-domain only.
//
// Behavior change vs. master: previously this function silently
// truncated *BigFloat, *BigInteger overflow, and *Rational with
// non-representable denominators. Now it errors. Embedders who
// relied on the silent path must either (a) opt into the
// permissive path via values.ToFloat64WithAccuracy and discard
// the accuracy, or (b) preserve the input in higher-precision
// types. See CHANGELOG for migration guidance.
func ToFloat64(v values.Value) (float64, error) {
    n, ok := v.(values.Number)
    if !ok {
        return 0, werr.WrapForeignErrorf(werr.ErrNotAReal,
            "expected a real number but got %T", v)
    }
    // Reject complex types explicitly — real-extraction is real-only.
    if _, isComplex := n.(values.ComplexNumber); isComplex {
        return 0, werr.WrapForeignErrorf(werr.ErrNotAReal,
            "expected a real number but got %T", v)
    }
    return values.ToFloat64Lossless(n)
}
```

(Note: the rejection switch is replaced by `values.ComplexNumber`
interface dispatch — matches `Hashable`/`Tuple`/`Indexable` precedent
in `values/` and avoids enumerating both `*Complex` and `*BigComplex`
by name.)

### Files modified

| File                                       | Change                                                                                                                                                       |
|--------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ffi.go` / engine.go                       | New `WithLossyConversionsAllowed()` engine option. The flag stored on `*Engine`; threaded into FFI spec at `RegisterFunc` time.                              |
| `ffi_arg_converters.go`                    | `reflect.Float64` case migrates: strict mode calls `ToFloat64Lossless`; lossy-allowed mode calls `ToFloat64WithAccuracy` and discards `acc`/`isReal`. New `reflect.Complex128` case added analogously. |
| `registry/helpers/value_conv.go`           | `ToFloat64` migrates to `values.ToFloat64Lossless`. **Tightening**: BigFloat/BigInteger/Rational that today silently truncate now error with `ErrLossyConversion` on loss. Same-precision inputs continue to succeed. |
| `ffi_test.go`                              | New tests cover (a) FFI Float64 strict-mode loss → error; (b) FFI Float64 lossy-allowed mode → silent truncation; (c) new Complex128 path; (d) `helpers.ToFloat64` tightening. |
| `CHANGELOG.md`                             | Document **three** behavior changes: FFI float64 precision-aware (previously rejected BigFloat with `ErrTypeConversion` via `fmtArgError` → now accepts when lossless, errors with `ErrLossyConversion` when not); FFI complex128 newly supported (no converter existed); `helpers.ToFloat64` tightened (previously silently truncated → now errors with `ErrLossyConversion`). Plus two additions: `WithLossyConversionsAllowed` option, `ErrLossyConversion` sentinel. |

### Steps

0. **Pre-PR-2 audit checklist (must run BEFORE writing code).**
   These greps surface callers whose error-handling must adapt:
   - `grep -rn 'errors.Is(.*ErrNotAReal)' --include='*.go'` — list
     every site that catches `ErrNotAReal` from `helpers.ToFloat64`.
     Audit each: does the caller distinguish "wrong type" from
     "silently truncated"? The tightening changes the latter path
     from success to `ErrLossyConversion` — callers that today
     never see an error path for big-precision inputs now will.
   - `grep -rn 'helpers.ToFloat64' --include='*.go'` — list every
     caller; audit error-handling at each. Three categories:
     (i) propagates error → no change required; (ii) catches
     `ErrNotAReal` and substitutes a default → must also catch
     `ErrLossyConversion`; (iii) ignores error → bug regardless.
   - `grep -rn 'BigFloat(...).RawValue()' --include='*.go'` —
     direct big-precision-bypass sites that may need migration.
   - `grep -rn 'reflect.Complex128' --include='*.go'` — verify
     no existing FFI test asserts that registration fails for a
     `complex128` parameter. (If any exist, they need updating to
     assert the new succeeds-behavior.)

1. **Add `wile.WithLossyConversionsAllowed()` engine option**.
   Plumb the flag to `*Engine` and into the per-function FFI
   spec at registration time (closure captures the value at
   `RegisterFunc` call).

2. **Migrate `reflect.Float64` path**. The canonical code stub is
   the "extended `makeArgConverter` signature" block above (lines
   ~774-805). The `p.lossyConversionsAllowed` direct-read shown
   in an earlier draft was wrong — `makeArgConverter` has no `p`
   receiver. The flag travels via the signature-extension
   approach: `makeArgConverter(name, pos, t, lossyAllowed)`,
   captured into the converter closure. See the plumbing diagram
   for the full chain.

3. **Add `reflect.Complex128` case** analogously. Canonical stub
   is the Complex128 block above. Strict mode calls
   `ToComplex128Lossless`; lossy-allowed mode calls
   `ToComplex128WithAccuracy`, then projects `res.Value` and
   discards `(res.RealAcc, res.ImagAcc)`.

4. **Migrate `helpers.ToFloat64`** (per Q-5 — **tightening**).
   Replace the current 5-case switch with a call to
   `values.ToFloat64Lossless`. The behavior change is:
   - **Same-precision inputs unchanged**: `*Integer(42)` →
     `42.0`; `*BigFloat(1.5)` → `1.5`. No regression.
   - **Lossy inputs now error**: `*BigFloat(2^100)` and
     `*Rational(1/3)` previously silently truncated; now return
     `ErrLossyConversion` (wrapped, with direction `Above`/`Below`
     in the message).
   - **Complex unchanged**: `*Complex`/`*BigComplex` continue to
     return `ErrNotAReal` (they're not real numbers).
   - **Audit before code**: `grep -rn 'errors.Is(.*ErrNotAReal)'`
     to find callers that may need updating to also handle the
     new `ErrLossyConversion` path. (Most won't — the typical
     pattern is "if not-a-real, error out"; they'll see the new
     error wrap as another error and let it propagate.)

5. **Tests** (`ffi_test.go` + `registry/helpers/value_conv_test.go`):
   - **FFI Float64 strict-mode loss table**:
     - `*BigFloat(1.5)` → succeeds (`1.5`)
     - `*BigFloat("1e500")` → errors `ErrLossyConversion`, direction `Above`
     - `*BigFloat("-1e500")` → errors `ErrLossyConversion`, direction `Below`
     - `*Rational(1, 3)` → errors `ErrLossyConversion`, direction `Below`
     - `*Integer(42)` → succeeds (`42.0`)
   - **FFI Float64 lossy-allowed-mode**: same inputs all succeed
     (`+Inf` / `-Inf` / `0.333...` / `42.0`); verify the silent
     truncation happens.
   - **Engine isolation**: two engines registered with the same
     function, one strict, one lossy-allowed — verify per-engine
     behavior independence.
   - **Complex128 path**:
     - `*BigComplex(3, 4)` → succeeds (`3+4i`)
     - `*BigComplex(10^500, 0)` → errors `ErrLossyConversion`, realAcc=Above (construct via `new(big.Int).Exp(big.NewInt(10), big.NewInt(500), nil)`)
     - `*BigComplex(0, 10^500)` → errors `ErrLossyConversion`, imagAcc=Above
   - **`helpers.ToFloat64` tightening regression**:
     - `*BigFloat("1e500")` previously silently returned `+Inf`;
       new test asserts it now errors with `ErrLossyConversion`.
     - `*Rational(1, 3)` previously silently returned
       `0.333...`; new test asserts it now errors.
     - Both verify via `errors.Is`.
   - **Opt-in**: engine with `WithLossyConversionsAllowed()` —
     `*BigFloat(2^100)` succeeds (silently truncated to `+Inf`).
   - **`helpers.ToFloat64` tightening**: `*BigFloat` /
     `*BigInteger` overflow / `*Rational(1/3)` previously
     silently truncated; now error with `ErrLossyConversion`.

6. **Update CHANGELOG.md** — **three behavior changes + two
   additions**. The behavior changes are user-visible and must be
   prominent. Concrete template:

   ```markdown
   ### Numeric conversion semantics — new loss-signal-aware rules

   This release introduces a strict-by-default discipline for
   numeric-to-fixed-precision conversions. Three sites previously
   silently truncated to `float64`; they now error on loss.
   Embedders relying on silent truncation must opt in via the
   new `WithLossyConversionsAllowed()` engine option.

   **Behavior changes (user-visible):**

   - **FFI `float64` parameter conversion is now precision-aware.**
     Three changes verified against current code
     (`ffi_arg_converters.go:76-96`):
     - **`*BigFloat` newly accepted.** Previously rejected via
       `fmtArgError` (wraps `werr.ErrTypeConversion`); now succeeds
       when the value fits losslessly in `float64`, errors with
       `werr.ErrLossyConversion` when it overflows or rounds.
     - **`*BigInteger` overflow newly errors.** Previously
       silently truncated to `±Inf` via `n.BigInt().Float64()`
       discarding the accuracy bit; now errors with
       `ErrLossyConversion` (direction `Above`/`Below`).
     - **`*Rational` non-representable newly errors.** Previously
       silently truncated via `n.Rat().Float64()` discarding the
       exact bool; now errors with `ErrLossyConversion`. Example:
       `(1/3)` passed to a `float64` parameter previously yielded
       `0.333...`; now errors.

     The error message names the direction of loss (`Above` /
     `Below`). Callers can `errors.Is(err, werr.ErrLossyConversion)`
     to catch precision-loss specifically and `errors.Is(err,
     werr.ErrTypeConversion)` to catch reflect.Kind mismatch.

     *Net effect*: some calls that errored before succeed now
     (lossless BigFloat); some calls that succeeded silently
     before now error (lossy BigInteger / Rational / now-loud
     BigFloat). Embedders relying on the previous silent path
     can recover it via `WithLossyConversionsAllowed()`.

   - **FFI `complex128` parameter conversion is now supported.**
     Previously, Go functions taking `complex128` parameters
     could not be registered (FFI had no converter for
     `reflect.Complex128`). Now `*Complex` and `*BigComplex`
     arguments convert with per-component precision tracking.

   - **Sentinel shift for complex args to Go `float64` parameters.**
     Previously, passing `*Complex` or `*BigComplex` (even with
     zero imaginary part) to a Go `float64` parameter errored with
     `werr.ErrTypeConversion` (the FFI's pre-tightening `default`
     branch via `fmtArgError`). After PR 2, the same call returns
     `werr.ErrLossyConversion` instead (via `ToFloat64Lossless`'s
     `!isReal` branch). Embedders matching on `errors.Is(err,
     ErrTypeConversion)` to catch "complex passed where real
     expected" must add `errors.Is(err, ErrLossyConversion)`.
     This is a deliberate consequence of unifying the type-check
     with the precision check; the new sentinel is more accurate
     (information is being dropped, not the wrong Go kind).

   - **`registry/helpers/value_conv.ToFloat64` tightened.**
     Previously **silently truncated** `*BigFloat`,
     `*BigInteger` overflowing float64, and `*Rational` with
     non-representable denominators (e.g., `1/3`). Now errors
     with `ErrLossyConversion` on loss. Same-precision inputs
     (`*Integer`, `*Float`, exact-power-of-2 `*Rational`, etc.)
     continue to succeed unchanged.

     *Migration*: code that previously consumed silently-
     truncated values (e.g., `float64(bigFloatValue.RawValue())`)
     should either (a) catch `ErrLossyConversion` and decide
     explicitly, or (b) use `values.ToFloat64WithAccuracy` and
     discard the accuracy slot to recover the silent-truncation
     behavior.

   **Additions:**

   - **New engine option `wile.WithLossyConversionsAllowed()`.**
     Opt-in flag suppressing `ErrLossyConversion` returns from
     the FFI converters; the converter calls
     `values.ToFloat64WithAccuracy` and discards the accuracy/
     isReal flags. Per-engine; captured at `RegisterFunc` time.

   - **New error sentinel `werr.ErrLossyConversion`.** Distinct
     from `ErrNotAReal` (real-vs-complex domain) and
     `ErrTypeConversion` (Go reflect.Kind mismatch). Callers can
     `errors.Is` against it specifically.

   - **New `values.ToFloat64WithAccuracy`,
     `values.ToFloat64Lossless`, `values.ToComplex128WithAccuracy`,
     `values.ToComplex128Lossless`** public helpers. See
     `values/conversion.go`. Surface Go's `big.Accuracy`
     three-valued enum directly.
   ```

7. **Bench check**: FFI conversion is borderline cold (called
   per FFI call, not per arithmetic op). Run `make bench-gabriel`
   + FFI-heavy benches; verify ≤ 0.5% geomean delta.

8. **Lint + CI**.

### Acceptance for PR 2

- FFI `reflect.Float64` and `reflect.Complex128` paths consult
  the new helpers.
- `wile.WithLossyConversionsAllowed()` option exists and works
  (verified by table case).
- `helpers.ToFloat64` **tightened**: previously-silent-truncation
  sites for `*BigFloat`, `*BigInteger` overflow, and `*Rational`
  with non-representable denominators now error with
  `ErrLossyConversion`. Verified by regression tests asserting
  these inputs error after migration.
- The acceptance-table FFI rows from the design pass as Go tests.
- CHANGELOG documents the three behavior changes + two additions
  (see Step 6 template); documentation deliverables (numeric
  tower, R7RS differences, embedding api-design, values/
  CLAUDE.md, werr/CLAUDE.md, extensions/math/CLAUDE.md) all
  landed in the same PR.
- No bench regression beyond noise.

## PR 3 — Scheme primitives

### Goal

Implement the four Scheme primitives. Cold-path; no bench gate.

### Reference template

Look at `extensions/math/` `floor/`, `truncate/`, and
`exact-integer-sqrt` for the established multi-value primitive
shape:
- `PrimitiveSpec` entry **omits `ReturnType`** when returning
  multiple values.
- The `Impl` function calls `mc.SetValues(v1, v2, ...)` instead of
  `mc.SetValue(v)`.
- The `Doc` field describes the multiple-value return shape
  explicitly ("Returns two values: ...").

### Code stub: PrimitiveSpec entries (`extensions/math/register.go`)

Add a new grouped section to `addPrimitives` — preserving the
existing organization-by-category:

```go
// Loss-signal primitives — surface Go big.Accuracy to Scheme.
// All in (extensions/math/prim_conversion.go) alongside
// exact->inexact, number->string.
r.AddPrimitives([]registry.PrimitiveSpec{
    {Name: "inexact-lossless?", ParamCount: 1, Impl: PrimInexactLosslessQ,
        Doc: "Returns #t if (exact->inexact n) would be lossless (every component exactly representable). " +
            "For complex N, both real and imaginary parts must be lossless.",
        ParamNames: []string{"n"}, Category: "math",
        Keywords:   []string{"precision", "lossless", "exact", "accuracy", "round-trip"},
        ParamTypes: []values.TypeConstraint{values.TypeNumber},
        ReturnType: values.TypeBoolean},

    // inexact-accuracy returns 1 symbol for real input, 2 for complex —
    // polymorphic return shape. ReturnType omitted (matches floor/ precedent).
    {Name: "inexact-accuracy", ParamCount: 1, Impl: PrimInexactAccuracy,
        Doc: "Predicts the accuracy of (exact->inexact n) without performing the conversion. " +
            "For real N, returns one of 'below 'exact 'above. " +
            "For complex N, returns two values: (values real-acc imag-acc).",
        ParamNames: []string{"n"}, Category: "math",
        Keywords:   []string{"precision", "accuracy", "below", "exact", "above"},
        ParamTypes: []values.TypeConstraint{values.TypeNumber}},

    // inexact-with-accuracy returns 2 values for real, 3 for complex.
    {Name: "inexact-with-accuracy", ParamCount: 1, Impl: PrimInexactWithAccuracy,
        Doc: "Returns (exact->inexact n) along with its accuracy. " +
            "For real N, returns two values: (values inexact-n accuracy-sym). " +
            "For complex N, returns three values: (values inexact-c real-acc imag-acc). " +
            "Accuracy symbols are 'below / 'exact / 'above.",
        ParamNames: []string{"n"}, Category: "math",
        Keywords:   []string{"precision", "convert", "inexact", "accuracy"},
        ParamTypes: []values.TypeConstraint{values.TypeNumber}},

    // complex-inexact-with-accuracy is the uniform 3-value variant.
    {Name: "complex-inexact-with-accuracy", ParamCount: 1, Impl: PrimComplexInexactWithAccuracy,
        Doc: "Returns the complex-domain inexact conversion of N with per-component accuracy. " +
            "Always returns three values: (values inexact-c real-acc imag-acc), " +
            "where real-acc and imag-acc are each one of 'below / 'exact / 'above. " +
            "For real input N, imag-acc is trivially 'exact.",
        ParamNames: []string{"n"}, Category: "math",
        Keywords:   []string{"complex", "precision", "convert", "inexact", "accuracy"},
        ParamTypes: []values.TypeConstraint{values.TypeNumber}},
}, registry.PhaseSetRuntime)
```

### Code stub: primitive implementations (`extensions/math/prim_conversion.go`)

Append to the existing file, alongside `PrimNumberToString`:

```go
// PrimInexactLosslessQ implements (inexact-lossless? n).
// Uses complex128 semantics: lossless iff BOTH real and imaginary
// component accuracies are big.Exact. (For real-only inputs, the
// imaginary accuracy is trivially big.Exact, so the predicate
// collapses to "real-part lossless".)
func PrimInexactLosslessQ(mc machine.CallContext) error {
    n, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber,
            "inexact-lossless?: expected a number but got %T", mc.Arg(0))
    }
    res, err := values.ToComplex128WithAccuracy(n)
    if err != nil {
        return err
    }
    lossless := res.RealAcc == big.Exact && res.ImagAcc == big.Exact
    mc.SetValue(values.BoolToBoolean(lossless))
    return nil
}

// PrimInexactAccuracy implements (inexact-accuracy n).
// Predicts conversion accuracy without performing the conversion.
// Real input: returns one symbol. Complex input: returns two
// symbols via mc.SetValues.
func PrimInexactAccuracy(mc machine.CallContext) error {
    n, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber,
            "inexact-accuracy: expected a number but got %T", mc.Arg(0))
    }
    // Domain dispatch via ComplexNumber interface — matches
    // Hashable/Tuple/Indexable precedent in values/. Avoids
    // enumerating *Complex and *BigComplex by name (would need
    // updating each time a new complex kind is added).
    if _, isComplex := n.(values.ComplexNumber); isComplex {
        res, err := values.ToComplex128WithAccuracy(n)
        if err != nil {
            return err
        }
        mc.SetValues(
            values.BigAccuracyToSymbol(res.RealAcc),
            values.BigAccuracyToSymbol(res.ImagAcc),
        )
        return nil
    }
    _, acc, _, err := values.ToFloat64WithAccuracy(n)
    if err != nil {
        return err
    }
    mc.SetValue(values.BigAccuracyToSymbol(acc))
    return nil
}

// PrimInexactWithAccuracy implements (inexact-with-accuracy n).
// Real input → (values inexact-n acc-sym). Complex input →
// (values inexact-c real-acc-sym imag-acc-sym).
func PrimInexactWithAccuracy(mc machine.CallContext) error {
    n, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber,
            "inexact-with-accuracy: expected a number but got %T", mc.Arg(0))
    }
    if _, isComplex := n.(values.ComplexNumber); isComplex {
        res, err := values.ToComplex128WithAccuracy(n)
        if err != nil {
            return err
        }
        mc.SetValues(
            values.NewComplex(res.Value),
            values.BigAccuracyToSymbol(res.RealAcc),
            values.BigAccuracyToSymbol(res.ImagAcc),
        )
        return nil
    }
    f, acc, _, err := values.ToFloat64WithAccuracy(n)
    if err != nil {
        return err
    }
    mc.SetValues(
        values.NewFloat(f),
        values.BigAccuracyToSymbol(acc),
    )
    return nil
}

// PrimComplexInexactWithAccuracy implements
// (complex-inexact-with-accuracy n) — uniform 3-value return
// regardless of input domain.
func PrimComplexInexactWithAccuracy(mc machine.CallContext) error {
    n, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber,
            "complex-inexact-with-accuracy: expected a number but got %T", mc.Arg(0))
    }
    res, err := values.ToComplex128WithAccuracy(n)
    if err != nil {
        return err
    }
    mc.SetValues(
        values.NewComplex(res.Value),
        values.BigAccuracyToSymbol(res.RealAcc),
        values.BigAccuracyToSymbol(res.ImagAcc),
    )
    return nil
}
```

### Code stub: tests (`extensions/math/prim_conversion_test.go`)

Use `testhelpers.SchemeCodeTestCase` for single-value primitives;
use `(call-with-values (lambda () ...) list)` to collect
multi-value returns into a list for `equal?` comparison (the
established pattern — `floor/`, `truncate/`, `exact-integer-sqrt`
all use this).

```go
func TestInexactLosslessQ(t *testing.T) {
    tcs := []testhelpers.SchemeCodeTestCase{
        {Name: "integer-lossless",       Code: `(inexact-lossless? 7)`,                Expected: values.TrueValue},
        {Name: "rational-half-lossless", Code: `(inexact-lossless? 1/2)`,              Expected: values.TrueValue},
        {Name: "rational-third-lossy",   Code: `(inexact-lossless? 1/3)`,              Expected: values.FalseValue},
        {Name: "bigint-overflow-lossy",  Code: `(inexact-lossless? (expt 10 100))`,    Expected: values.FalseValue},
        {Name: "nan-identity",           Code: `(inexact-lossless? +nan.0)`,           Expected: values.TrueValue},
        {Name: "complex-exact",          Code: `(inexact-lossless? 3+4i)`,             Expected: values.TrueValue},
        {Name: "complex-lossy-real",     Code: `(inexact-lossless? (make-rectangular 1/3 0))`, Expected: values.FalseValue},
        {Name: "bigcomplex-both-lossy",  Code: `(inexact-lossless? (make-rectangular (expt 10 100) (expt 10 100)))`, Expected: values.FalseValue},
    }
    for _, tc := range tcs {
        t.Run(tc.Name, func(t *testing.T) {
            result, err := testhelpers.RunSchemeCode(t, tc.Code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
        })
    }
}

func TestInexactAccuracy(t *testing.T) {
    // For real input — 1 value.
    realCases := []testhelpers.SchemeCodeTestCase{
        {Name: "integer-exact",        Code: `(inexact-accuracy 7)`,             Expected: values.NewSymbol("exact")},
        {Name: "rational-third-below", Code: `(inexact-accuracy 1/3)`,           Expected: values.NewSymbol("below")},
        {Name: "rational-twothirds-above", Code: `(inexact-accuracy 2/3)`,       Expected: values.NewSymbol("above")},
        {Name: "expt-10-100-above",    Code: `(inexact-accuracy (expt 10 100))`, Expected: values.NewSymbol("above")},
    }
    for _, tc := range realCases {
        t.Run(tc.Name, func(t *testing.T) {
            result, err := testhelpers.RunSchemeCode(t, tc.Code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
        })
    }
    // For complex input — 2 values; collect via call-with-values.
    complexCases := []testhelpers.SchemeCodeTestCase{
        {Name: "complex-exact-exact",
            Code:     `(call-with-values (lambda () (inexact-accuracy 3+4i)) list)`,
            Expected: values.List(values.NewSymbol("exact"), values.NewSymbol("exact"))},
        {Name: "complex-both-below",
            Code:     `(call-with-values (lambda () (inexact-accuracy (make-rectangular 1/3 1/7))) list)`,
            Expected: values.List(values.NewSymbol("below"), values.NewSymbol("below"))},
    }
    // ... iterate complexCases analogously.
}

// TestInexactWithAccuracy — covers real (2-value return) and
// complex (3-value return) cases. Uses (call-with-values ... list)
// to collect.
func TestInexactWithAccuracy(t *testing.T) {
    realCases := []testhelpers.SchemeCodeTestCase{
        {Name: "integer-7",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 7)) list)`,
            Expected: values.List(values.NewFloat(7.0), values.NewSymbol("exact"))},
        {Name: "rational-onethird",
            // Use Go-computed literal 1.0/3.0 rather than hand-written
            // 0.333... to avoid ULP-off transcription error.
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 1/3)) list)`,
            Expected: values.List(values.NewFloat(1.0/3.0), values.NewSymbol("below"))},
        {Name: "rational-twothirds",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 2/3)) list)`,
            Expected: values.List(values.NewFloat(2.0/3.0), values.NewSymbol("above"))},
        {Name: "bigint-overflow",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy (expt 10 100))) list)`,
            Expected: values.List(values.NewFloat(math.Inf(1)), values.NewSymbol("above"))},
    }
    complexCases := []testhelpers.SchemeCodeTestCase{
        {Name: "complex-exact-exact",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 3+4i)) list)`,
            Expected: values.List(values.NewComplex(complex(3, 4)), values.NewSymbol("exact"), values.NewSymbol("exact"))},
        {Name: "complex-onethird-oneseventh",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy (make-rectangular 1/3 1/7))) list)`,
            Expected: values.List(values.NewComplex(complex(1.0/3.0, 1.0/7.0)), values.NewSymbol("below"), values.NewSymbol("below"))},
    }
    runRealOrComplexTable(t, realCases, complexCases)
}

// TestPolymorphicReturnArity — explicit value-count assertion.
// The (call-with-values ... list) pattern above passes only when
// the list length AND contents match. A bug where the primitive
// always emits 2 values for complex input (the default-arm
// behavior) is caught by the list-content mismatch, but the
// failure mode is subtle. This separate test asserts arity
// directly via (call-with-values ... (lambda args (length args))).
//
// This is the regression-test class the crosscheck flagged
// explicitly: polymorphic return shape is the whole point of
// these primitives; arity must be asserted directly.
func TestPolymorphicReturnArity(t *testing.T) {
    arityCases := []testhelpers.SchemeCodeTestCase{
        // inexact-accuracy: 1 value for real, 2 for complex.
        {Name: "inexact-accuracy-real-1value",
            Code:     `(call-with-values (lambda () (inexact-accuracy 7))      (lambda args (length args)))`,
            Expected: values.NewInteger(1)},
        {Name: "inexact-accuracy-complex-2values",
            Code:     `(call-with-values (lambda () (inexact-accuracy 3+4i))   (lambda args (length args)))`,
            Expected: values.NewInteger(2)},

        // inexact-with-accuracy: 2 values for real, 3 for complex.
        {Name: "inexact-with-accuracy-real-2values",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 7)) (lambda args (length args)))`,
            Expected: values.NewInteger(2)},
        {Name: "inexact-with-accuracy-complex-3values",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 3+4i)) (lambda args (length args)))`,
            Expected: values.NewInteger(3)},

        // complex-inexact-with-accuracy: ALWAYS 3 values.
        {Name: "complex-inexact-with-accuracy-real-3values",
            Code:     `(call-with-values (lambda () (complex-inexact-with-accuracy 7))   (lambda args (length args)))`,
            Expected: values.NewInteger(3)},
        {Name: "complex-inexact-with-accuracy-complex-3values",
            Code:     `(call-with-values (lambda () (complex-inexact-with-accuracy 3+4i)) (lambda args (length args)))`,
            Expected: values.NewInteger(3)},

        // inexact-lossless?: always exactly 1 value (no polymorphism).
        {Name: "inexact-lossless-real-1value",
            Code:     `(call-with-values (lambda () (inexact-lossless? 7))       (lambda args (length args)))`,
            Expected: values.NewInteger(1)},
        {Name: "inexact-lossless-complex-1value",
            Code:     `(call-with-values (lambda () (inexact-lossless? 3+4i))   (lambda args (length args)))`,
            Expected: values.NewInteger(1)},
    }
    for _, tc := range arityCases {
        t.Run(tc.Name, func(t *testing.T) {
            result, err := testhelpers.RunSchemeCode(t, tc.Code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
        })
    }
}
```

### Library export

The math extension is registered via `registry.NewDescribedExtension`
in `extensions/math/register.go:26-28`. Adding primitives to the
extension's primitive list (via `r.AddPrimitives(...)` inside
`addPrimitives`) automatically makes them available when the
extension is loaded by an engine — no separate `.sld` library
file edit needed.

**Verified 2026-05-14**: there is no `(wile math)` library file in
`stdlib/lib/`. The only related library is
`stdlib/lib/scheme/inexact.sld`, which exports the R7RS `(scheme
inexact)` set; these four new primitives are *Wile extensions* and
deliberately do not join the R7RS surface. They are reachable
unconditionally from any engine that loads the math extension
(profile `Small` and above per `plans/2026-03-26-environment-profiles.md`).
No `.sld` edit is required for PR 3.

### Doc + apropos verification

After PR 3 lands, verify discoverability:

```scheme
;; In the REPL:
(doc inexact-lossless?)           ; renders the Doc string
(doc inexact-accuracy)
(apropos "lossless")              ; should list inexact-lossless?
(apropos "accuracy")              ; should list all four primitives
(apropos "precision")             ; same
```

If any apropos search misses a primitive, audit its `Keywords`
field.

### Files modified

| File                                              | Change                                                                                                                                       |
|---------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------|
| `extensions/math/prim_conversion.go`              | Add four primitives: `PrimInexactLosslessQ`, `PrimInexactAccuracy`, `PrimInexactWithAccuracy`, `PrimComplexInexactWithAccuracy`. Q-suffix convention verified against `extensions/math/prim_rounding.go:198,208,218` (`PrimFiniteQ`, `PrimInfiniteQ`, `PrimNanQ`). Wire in the extension's primitive registration. |
| `extensions/math/prim_conversion_test.go`         | Table-driven tests covering the design's acceptance table.                                                                                   |
| `extensions/math/CLAUDE.local.md`                 | Add the four primitives under a new "Numeric Conversion (loss-signal aware)" section. (Was previously assigned to PR 2; moved here so docs ship atomically with the primitives.) |
| `docs/extensions/architecture.md` (optional)      | Note the four primitives under the math extension entry, if the doc enumerates by primitive.                                                 |

### Steps

The canonical Go implementations are above in the "Code stub:
primitive implementations" section — full bodies for all four
Prim*. Steps 1–4 of this section deliberately point at the
canonical stub rather than re-stubbing here (a previous draft
re-stubbed with P-suffix names and wrong `*MachineContext`
pointer type — internal drift).

1. **Implement `PrimInexactLosslessQ`** per the canonical stub
   above. Uses `values.ToComplex128WithAccuracy` + tests both
   `res.RealAcc == big.Exact && res.ImagAcc == big.Exact`. Sets
   result via `mc.SetValue(values.BoolToBoolean(lossless))`.

2. **Implement `PrimInexactAccuracy`** per the canonical stub
   above. Type-switch on `values.ComplexNumber` (NOT
   `case *values.Complex, *values.BigComplex:`) — interface
   dispatch matches the `Hashable`/`Tuple`/`Indexable` precedent
   in `values/`. Real input: `mc.SetValue(symbol)`. Complex
   input: `mc.SetValues(realSym, imagSym)`.

3. **Implement `PrimInexactWithAccuracy`** per the canonical stub
   above. Real: `mc.SetValues(NewFloat(f), accSym)`. Complex:
   `mc.SetValues(NewComplex(c), realSym, imagSym)`.

4. **Implement `PrimComplexInexactWithAccuracy`** per the
   canonical stub above. Uniform 3-value via
   `mc.SetValues(NewComplex(c), realSym, imagSym)` regardless of
   input domain.

5. **Docstrings**: each primitive gets a structured docstring with:
   ```
   Parameters: n -- a number
   Returns: ...
   Category: Numbers — Conversion
   Keywords: precision, accuracy, conversion, lossless, inexact
   ```

6. **Wire-up**: register the four primitives in the math
   extension's primitive list. Verify `(apropos "lossless")` and
   `(apropos "accuracy")` discover them.

7. **Tests** (`prim_conversion_test.go`): table-driven covering
   every row of the design's acceptance table. Each row is one
   `t.Run` case with a multi-value-return assertion helper.

8. **Integration test**: a Scheme program that exercises all
   four primitives from `(import (wile math))` or wherever they
   end up exported.

9. **Lint + CI**.

### Three-layer integration test (PR 3 deliverable)

The three layers — Go helper, FFI converter, Scheme primitive —
all consume the same per-kind dispatch through the registry.
A bug at any layer that diverges from the others is the genuinely
consequential failure mode for this feature. PR 3 adds one
integration test (in `integration/` or wherever the project's
cross-layer tests live) that fixes the contract.

```go
// integration/loss_signals_three_layer_test.go (new file)

// TestLossSignalsThreeLayerAgreement asserts that the Go helper,
// the FFI converter, and the Scheme primitive all report the
// same accuracy outcome for the same numeric input. A bug at any
// single layer manifests as inter-layer disagreement that
// per-layer unit tests cannot catch.
//
// For each (input, expected) row:
//   Layer 1: values.ToFloat64WithAccuracy(input) → acc (positional)
//   Layer 2: FFI converter strict mode → succeeds iff acc == Exact
//              && isReal; otherwise ErrLossyConversion
//   Layer 3: (inexact-accuracy input) → same accuracy symbol
//
// All three must agree, on every row.
func TestLossSignalsThreeLayerAgreement(t *testing.T) {
    cases := []struct {
        name        string
        schemeInput string                 // Scheme expression producing the input
        goInput     func() values.Number   // equivalent Go construction
        wantAcc     big.Accuracy
        wantIsReal  bool
    }{
        {"integer-exact",     "7",            func() values.Number { return values.NewInteger(7) },             big.Exact, true},
        {"rational-onethird", "1/3",          func() values.Number { return values.NewRational(1, 3) }, big.Below, true},
        {"bigint-overflow",   "(expt 10 100)", func() values.Number {
            return values.NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(100), nil))
        }, big.Above, true},
        {"complex-with-imag", "3+4i",         func() values.Number { return values.NewComplex(complex(3, 4)) }, big.Exact, false},
    }

    eng, _ := wile.NewEngine(context.Background(), wile.WithAllExtensions())
    defer eng.Close()

    // Register a Go function that takes a float64; we'll use it to
    // probe the FFI converter behavior.
    var fnCalled bool
    _ = eng.RegisterFunc("test-float64-callback", func(x float64) {
        fnCalled = true
    })

    for _, tc := range cases {
        t.Run(tc.name, func(t *testing.T) {
            c := qt.New(t)

            // Layer 1: Go helper directly.
            _, acc, isReal, err := values.ToFloat64WithAccuracy(tc.goInput())
            c.Assert(err, qt.IsNil)
            c.Assert(acc, qt.Equals, tc.wantAcc)
            c.Assert(isReal, qt.Equals, tc.wantIsReal)

            // Layer 2: FFI converter (strict mode — no
            // WithLossyConversionsAllowed). Should error iff
            // Layer 1 reported loss.
            fnCalled = false
            _, ffiErr := eng.EvalMultiple(fmt.Sprintf(
                "(test-float64-callback %s)", tc.schemeInput))
            if tc.wantAcc == big.Exact && tc.wantIsReal {
                c.Assert(ffiErr, qt.IsNil)
                c.Assert(fnCalled, qt.IsTrue)
            } else {
                c.Assert(errors.Is(ffiErr, werr.ErrLossyConversion), qt.IsTrue)
                c.Assert(fnCalled, qt.IsFalse)
            }

            // Layer 3: Scheme primitive. For real input,
            // (inexact-accuracy x) returns 1 value matching Layer 1.
            // For complex with non-zero imag, it returns 2 values;
            // first should match the real-part accuracy.
            accCode := fmt.Sprintf("(inexact-accuracy %s)", tc.schemeInput)
            // Use Layer 1's IsReal flag to decide whether to expect
            // 1 vs 2 return values from the primitive.
            if tc.wantIsReal {
                accResult, err := testhelpers.RunSchemeCode(t, accCode)
                c.Assert(err, qt.IsNil)
                c.Assert(accResult, valuestest.SchemeEquals,
                    values.BigAccuracyToSymbol(tc.wantAcc))
            } else {
                // Complex: collect 2 values via call-with-values
                listCode := fmt.Sprintf(
                    "(call-with-values (lambda () %s) list)", accCode)
                accList, err := testhelpers.RunSchemeCode(t, listCode)
                c.Assert(err, qt.IsNil)
                // first element of list is the real-part accuracy
                firstSym, _ := accList.(values.Tuple).Car().(*values.Symbol)
                c.Assert(firstSym, valuestest.SchemeEquals,
                    values.BigAccuracyToSymbol(tc.wantAcc))
            }
        })
    }
}
```

This test is the *only* test that catches the bug class "three
layers disagree on the same input." Unit tests on each layer
can pass independently while the layers disagree — this test
fails the entire CI run on divergence.

### Acceptance for PR 3

- All four primitives implemented, registered, exported.
- Every row of the design's acceptance table passes as a Scheme
  test case.
- Docstrings render correctly via `(doc inexact-lossless?)` etc.
- `apropos` discovers all four under "Numbers — Conversion".
- **`TestPolymorphicReturnArity` passes** — asserts the value-
  count of each primitive matches the input domain (real vs
  complex).
- **`TestLossSignalsThreeLayerAgreement` passes** — verifies
  Go helper / FFI / Scheme primitive consistency for the same
  set of inputs.

## Risk register (impl-specific)

| # | Risk                                                                       | Mitigation                                                                  |
|---|----------------------------------------------------------------------------|-----------------------------------------------------------------------------|
| I1 | Multiple-value-return shape on Scheme primitives diverges from existing prim conventions | Audit `prim_misc_test.go` and the `WithSingleResult`/multi-value precedent BEFORE writing PR 3. Follow the existing pattern verbatim. |
| I2 | Direction-recovery for `*big.Rat` round-trip allocates a new `big.Rat` per call | Test-confirmed cold path. If a profile shows this on a hot path, cache the round-trip or switch to a direct big.Float compare. |
| I3 | ~~Order-of-init dependency~~ — **RESOLVED**: numeric-registry Phase 3 merged at `082836d1` (PR #752). The registry is in place; PR 1 extends `NumericTypeSpec` directly. | n/a |
| I4 | FFI `reflect.Complex128` is a *new* converter (no current code) — could surprise registrants who relied on previous "Go function with complex128 parameter is unregisterable" behavior | The change is additive. Add a CHANGELOG note. The pre-change failure mode was a *registration* error (Go panic / FFI build error) — no callers can have built around it; only "I tried it once and stopped" users are affected, and they're now unblocked. |
| I5 | Engine-level `WithLossyConversionsAllowed` interacts with multi-engine embedders (one engine strict, another lossy) | The flag is per-engine instance, set at construction. No global state. Document. |
| I6 | Saturation-to-±Inf may surprise users who expect an error on overflow rather than `+inf.0` | `inexact-with-accuracy` documents this; the `'above` / `'below` accuracy symbol IS the signal. R7RS itself allows the saturation (`exact->inexact (expt 10 100)` returns `+inf.0`); we just expose the *direction*. |
| I7 | `BigComplex` with NaN parts — what's the accuracy? | Per the design's NaN handling rule: NaN propagates as `Exact` (identity). A `*BigComplex(NaN, NaN)` returns `(complex(NaN,NaN), Exact, Exact)`. Tests cover this explicitly. |
| I8 | The `helpers.ToFloat64` Q-5 **tightening** breaks callers that today depend on silent truncation of `*BigFloat` / `*BigInteger` overflow / `*Rational`. Three sites change from "success with silently-truncated value" to "error with `ErrLossyConversion`." | Audit before PR 2 lands: grep `helpers.ToFloat64` for all callers; classify each by error-handling shape (propagates / catches-and-defaults / ignores). Document the tightening prominently in CHANGELOG (template above). For callers that need the legacy silent path, the explicit recovery is `values.ToFloat64WithAccuracy(n)` and discard the accuracy slot. |

## Cross-references

- `memory/2026-05-14-numeric-loss-signals-design.md` — design
  source (refined; all Q-1…Q-6 resolved).
- `memory/2026-05-14-numeric-registry-design.md` /
  `2026-05-14-numeric-registry-impl.md` — prerequisite plans
  (Phase 3 of values-SR; this plan branches from master after
  those close).
- `werr/werr.go` — sentinel registry; new
  `ErrLossyConversion` joins.
- `ffi_arg_converters.go` — sites of the FFI tightening (PR 2).
- `registry/helpers/value_conv.go` — site of the Q-5 tightening
  (PR 2).
- `extensions/math/prim_conversion.go` — site of the new Scheme
  primitives (PR 3).
- Go stdlib `math/big`:
  - `(*big.Float).Float64() (float64, big.Accuracy)`
  - `(*big.Rat).Float64() (float64, bool)`
  - `(*big.Int).Float64() (float64, big.Accuracy)` (via
    `new(big.Float).SetInt(...)`)
- R7RS §6.2.6 — `exact->inexact` semantics; this plan adds
  *companion* primitives, leaves `exact->inexact` unchanged.

## Documentation deliverables (PR 2 must update these)

Per user instruction "Documentation will need to be updated with
new rules for numeric rounding/handling", the Q-5 tightening
changes user-visible numeric semantics and must be reflected in
project documentation, not just the CHANGELOG. PR 2 updates:

| File | What to add / change |
|------|----------------------|
| `CHANGELOG.md` | Three behavior changes + two additions; concrete template in PR 2 Step 6 above. **Prominent** placement under a top-level "Numeric conversion semantics" subsection. |
| `docs/numeric/tower.md` | New section "Conversion to fixed-precision Go types". Document the three-valued `big.Accuracy` enum's Scheme-facing reflection (`'below`/`'exact`/`'above` symbols). Explicitly note the "fail loud on loss" discipline at the float64/complex128 boundary. Contrast with R7RS-mandated `(exact->inexact)` which stays lossy-but-successful. |
| `docs/reference/r7rs-differences.md` | Add an entry under "Wile-specific numeric primitives" describing the four new `inexact-*` primitives and their relationship to R7RS `exact->inexact`. Note that R7RS-strict programs that import only `(scheme base)` / `(scheme inexact)` are unaffected. |
| `docs/embedding/api-design.md` (if it documents FFI) | Document the `WithLossyConversionsAllowed()` engine option, the new `ErrLossyConversion` sentinel, and the FFI float64/complex128 behavior. Migration guidance for embedders depending on the previous silent-truncation behavior. |
| `values/CLAUDE.md` numeric section | Add the new helper functions (`ToFloat64WithAccuracy` etc.) and the `Accuracy` symbol constants to the package's exported-symbol inventory. |
| `werr/CLAUDE.md` | New `ErrLossyConversion` sentinel listed in the inventory. |
| `extensions/math/CLAUDE.local.md` | (Moved to PR 3 — lands atomically with the primitives it documents.) |

**Rule statement** (canonical wording to use across all of the
above, lifted from the design):

> Conversion of a Scheme numeric value to a fixed-precision Go
> type (`float64`, `complex128`) reports its accuracy via Go's
> `big.Accuracy` enum (Below / Exact / Above). The default-strict
> path errors on any loss with `werr.ErrLossyConversion`; the
> opt-in lossy-allowed path silently truncates. R7RS-mandated
> conversions (`exact->inexact`) continue to use the silently-
> truncating semantics R7RS requires.

PR 2 should land these doc updates in the same commit (or a
preceding commit on the same PR) as the code changes — not as a
follow-up — so the documented contract and the implemented
behavior land atomically.

## Done definition (whole plan)

- [x] PR 1 merged (#753): Go infrastructure exposed, registry
      extended, tests pass.
- [x] PR 2 merged (#754): FFI tightening + complex128 +
      helpers.ToFloat64 **tightening**; CHANGELOG entries posted;
      documentation deliverables landed (in PR 3 for the
      math-extension-local docs; in PR 2 for CHANGELOG and other
      project docs).
- [x] PR 3 merged (#755): four Scheme primitives shipped;
      acceptance table from design passes as Scheme tests;
      three-layer integration test pins inter-layer agreement.
- [ ] Parent design plan moves to "Completed Plans" in
      `plans/CLAUDE.md`. **Pending** — move both this impl plan
      and the design plan to `memory/` after the closeout sweep
      (the convention is move-on-done; keeping them here while
      this section is fresh).

## Post-implementation outcome

### Shipped vs planned — deltas

| Area | Planned | Shipped | Note |
|------|---------|---------|------|
| `LookupNumericSpec` → `Lookup` rename | yes, in PR 1 | **declined** — kept as `LookupNumericSpec` | The rename would have rippled into 4 production call sites (`values/conversion.go:81, 122` and the registry itself). The longer name is unambiguous in cross-package use (`values.LookupNumericSpec`); the shorter `Lookup` collided semantically with other registries. Verified in production: `grep LookupNumericSpec values/` returns 5 hits, all internal. |
| BigFloat hygiene rename (`Float64` → `Float64Truncated` + new `Float64WithAccuracy`) | yes, PR 1 | shipped as planned | 13 internal call-site migrations all landed. |
| `atan2Operand` helper | not planned | **added in PR 2** | The `helpers.ToFloat64` tightening surfaced a real R7RS regression: `(atan y x)` with lossy operands (e.g., `(atan 1/3 1)`) would now error, violating R7RS §6.2.6 ("any real arguments"). PR 2 mitigated with a small local helper that goes through the lossy-allowed path. The duplication with `helpers.ToFloat64` was caught by 3-lens crosscheck convergence and filed as a Tier 5 tech-debt entry (`TODO.md`: "Unify `atan2Operand` with `helpers.ToFloat64`"). |
| FFI `complex128` callback param + return | "deferred — `makeRetConverter` has no complex128 arm" | shipped as deferred | `makeRetConverter` still has no `complex128` arm; complex *returns* and complex *callback params* remain unsupported (`ffi_test.go::TestRegisterFuncUnsupportedTypes` retains those cases). |
| `runOne` test helper | not planned | **created then deleted** | PR 3 initially introduced a `runOne` test wrapper believing the package-level `eval` helper would trip a security linter on the substring `e`/`v`/`a`/`l` followed by paren. Crosscheck (3-lens convergence: code + errors + consistency) caught that the same file already used the package-level helper six times pre-existing — the workaround was based on a false premise. PR 3 fixup deleted `runOne` and migrated callers to the existing helper. |
| Three-layer integration test | mentioned in plan as "PR 3 deliverable"; sketched at impl-plan line ~1937 | shipped as `integration/loss_signals_three_layer_test.go` | 6 rows after the post-crosscheck `bigcomplex-mixed-lossy` addition. Per-subtest scoping of `fnCalled` adopted on the consistency lens's recommendation. |
| Discoverability test | not planned in original impl plan | **added in PR 3 fixup** | `TestLossSignalDiscoverability` exercises `(apropos "lossless")`, `(apropos "accuracy")`, and `(procedure-documentation ...)` from Scheme. 8 cases. Filed after the tests lens flagged that a typo in the `Keywords` slice would silently degrade discoverability. |

### LOC actuals vs estimates

| PR | Estimate | Actual (merge stat) |
|----|----------|----------------------|
| 1  | +415 / −25   | merged via two commits + Copilot fixups (see PR #753 final stat) |
| 2  | +180 / −50   | ≈ +750 / −85 final after Copilot + crosscheck fixups (see PR #754 final stat). Larger than estimated because of the new `atan2Operand` helper + ffi_loss_signals_test.go (new file, 259 lines) + the CHANGELOG block + the lint-fixup commit. |
| 3  | +280 / −0    | ≈ +735 / −0 final (see PR #755 final stat). Larger than estimated because of the integration test (`integration/loss_signals_three_layer_test.go`, ~180 lines), the docs (`docs/numeric/tower.md`, `docs/reference/r7rs-differences.md`, `values/CLAUDE.md`, `extensions/math/CLAUDE.local.md`), the discoverability test, and the IEEE-754-specials test rows added after crosscheck. |

### Bench gate results (PR 2)

Full A/B comparison ran 6 trials × 16 Gabriel benchmarks on master
baseline (`6127ab04`) vs PR-2 branch (`21067482`). Geomean ratio:
**1.0026 (+0.26%)** — within the 0.5% gate the plan called for.
Per-bench spread was 13–65% per trial, well above any signal from
the PR-2 changes — expected: the Gabriel suite is pure-Scheme
arithmetic and never crosses FFI or `helpers.ToFloat64`. PRs 1
and 3 were not benched (PR 1 was bench-gated separately during
the values/ structural reduction; PR 3 is cold-path Scheme
primitives — no bench gate).

### Review findings summary

PR 2 dual-review surfaced 1 Critical (tests-lens: atan2 migration
untested), 1 Critical (consistency-lens: duplicated test helpers),
and 8+ Notable findings — all addressed in a single `fix(...)`
commit per the workflow convention.

PR 3 dual-review surfaced 1 Critical (3-lens convergence on the
`runOne` duplication), 6 Notable Unambiguous, 3 user-approved
extras (apropos test, mixed-lossy integration row, IEEE-754
specials), and 5 declined findings (with rationale in the PR
resolution comment).

### Cross-references

- Plan parent: `2026-05-14-numeric-loss-signals-design.md`
- Prerequisite: `2026-05-14-numeric-registry-impl.md` (shipped PR
  #752 / commit `082836d1`)
- Tech-debt follow-up: `TODO.md` → "Unify `atan2Operand` with
  `helpers.ToFloat64`" (Tier 5, Low / S)
- User-visible documentation:
  - `docs/numeric/tower.md` §"Conversion to Fixed-Precision Go
    Types"
  - `docs/reference/r7rs-differences.md` §"Loss-Signal-Aware
    Numeric Conversion Primitives" + §"FFI Numeric Argument
    Precision"
  - `values/CLAUDE.md` §"Numeric Conversion Helpers"
  - `extensions/math/CLAUDE.local.md` §"Numeric Conversion
    (loss-signal aware)" (local-only, gitignored)
  - `CHANGELOG.md` `[Unreleased]` — three behavior changes + two
    additions + four-primitive table

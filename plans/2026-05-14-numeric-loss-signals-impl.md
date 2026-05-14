# Numeric loss signals — implementation plan

**Date**: 2026-05-14 (initial); refined for impl detail 2026-05-14.
**Status**: Plan ready to start — PR #750 (numeric-registry Phase 3)
  merged at `51b1176a`. Pre-Phase-3 hedging removed.
**Design source**: `plans/2026-05-14-numeric-loss-signals-design.md`
  (refined 2026-05-14; resolutions: Q-1 saturate ±Inf, Q-2
  real-triple + per-component complex, Q-3 `extensions/math/`,
  Q-4 engine-level opt-in, Q-5 yes-tighten-helpers).
**Branches**:
  - PR 1: `feat/numeric-loss-signals-go` (branch from master).
  - PRs 2 and 3 branch from master after each preceding PR
    merges.
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
    `extensions/math/register.go:75-79` (the `expt` entry).

## Sequence overview

| PR | Scope                                                                                    | Bench gate | Est. delta   |
|----|------------------------------------------------------------------------------------------|------------|--------------|
| 1  | Go infrastructure: sentinel + accuracy symbols + per-kind helpers + values/ exports + registry extension | yes — verify cold-path discipline | +400 / −20  |
| 2  | FFI tightening (Float64 + Complex128 paths) + `WithLossyConversionsAllowed` engine option + `helpers.ToFloat64` Q-5 **tightening** (was silently-truncates; now errors on loss) | yes — FFI conversion hot-ish | +180 / −50 |
| 3  | Four Scheme primitives in `extensions/math/prim_conversion.go` + docstrings + tests | no (cold-path primitives) | +280 / −0   |

Cumulative net: **≈ +790 LOC**. Larger than typical because the
plan adds new public APIs at three layers (Go helpers, FFI
converters, Scheme primitives) with full no-information-loss
test coverage.

## PR 1 — Go infrastructure

### Goal

Lay the Go-side foundation. No behavior change outside the new
helpers; no FFI change; no Scheme primitives yet. Other PRs
consume what this PR exports.

### Concrete API contract (commit to before writing code)

**Four** exported functions in `values/` (the cross-package surface).
The earlier `ToFloat64Lossy` variant was dropped during refinement
— see "API surface decisions" below.

| Function | Signature | Behavior |
|----------|-----------|----------|
| `ToFloat64WithAccuracy` | `func(n Number) (f float64, acc big.Accuracy, isReal bool, err error)` | Primary. Returns the float64, the `big.Accuracy` (Below/Exact/Above), and an `isReal` flag (false iff input was Complex/BigComplex with non-zero imaginary). Returns `ErrNotANumber` (wrapped) for non-Number. Non-nil for every `Number` input. Callers who want lossy-allowed semantics use this directly and discard the accuracy/isReal slots. |
| `ToFloat64Lossless` | `func(n Number) (float64, error)` | Wraps `WithAccuracy`. Returns `ErrLossyConversion` (wrapped, message names direction) if `acc != big.Exact OR !isReal`. Returns `ErrNotANumber` for non-Number. **FFI strict-path consumer.** |
| `ToComplex128WithAccuracy` | `func(n Number) (c complex128, realAcc, imagAcc big.Accuracy, err error)` | Primary for complex domain. Per-component accuracy. `ErrNotANumber` for non-Number. For real-only inputs, `imagAcc == big.Exact`. |
| `ToComplex128Lossless` | `func(n Number) (complex128, error)` | Wraps `WithAccuracy`. Returns `ErrLossyConversion` if either component's accuracy is non-Exact. |

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

2. **`isReal` (renamed from `real`)** — the boolean slot in the
   per-kind helper return tuple and in the values/ API. Renamed
   to avoid shadowing Go's predeclared `real(c)` complex-projection
   function (which is called from adjacent files like
   `values/complex.go:56,86`). Apply the rename throughout
   stubs, tests, and PR-3 primitive implementations.

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
  public helpers (the 5 functions in the API contract table
  above). ~150 LOC.
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
| `values/numeric_registry.go`      | Extend `NumericTypeSpec` (see code stub below). Update `registerNumericSpec` validation to require the two new fields non-nil. |
| `values/integer.go`               | Add `integerToFloat64WithAccuracy` + `integerToComplex128WithAccuracy` named helpers; bind in `registerNumericSpec(KindInteger, …)`. |
| `values/big_integer.go`           | Same shape; uses `new(big.Float).SetInt(p.value).Float64()` for native accuracy.                                     |
| `values/float.go`                 | Same shape; identity → `big.Exact`; `(complex(p.Value, 0), Exact, Exact)` for complex helper.                        |
| `values/big_float.go`             | Same shape; consults `p.IsNaN()` first (returns `math.NaN(), big.Exact, true`); else `p.value.Float64()`.            |
| `values/rational.go`              | Same shape; direction-recovery via `new(big.Rat).SetFloat64(f).Cmp(p.value)`.                                        |
| `values/complex.go`               | Same shape; real part is identity (Float component) → `Exact`; real flag = `imag(p.Value) == 0`.                     |
| `values/big_complex.go`           | Same shape; per-component accuracy for complex helper.                                                                |
| `values/numeric_kind.go`          | Update the ADDING-A-NEW-NUMERIC-TYPE guide comment to include the two new required spec fields.                       |

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

Extend the existing struct + add getters + update validation:

```go
type NumericTypeSpec struct {
    // ... existing 5 fields from PR #750: schemeName, isAlwaysExact,
    // simplifyDown, toFloat64, toComplex128 ...

    // toFloat64WithAccuracy is the primary loss-signal API for
    // float64 conversion. Returns the float64 result, the
    // big.Accuracy (Below/Exact/Above), and a real flag (false iff
    // input was Complex/BigComplex with non-zero imaginary).
    // Always non-nil — populated for every kind.
    toFloat64WithAccuracy func(Number) (float64, big.Accuracy, bool)

    // toComplex128WithAccuracy returns the complex128 result and
    // per-component accuracy (real part, imaginary part). Always
    // non-nil — populated for every kind. For real-only inputs,
    // imagAcc is trivially big.Exact.
    toComplex128WithAccuracy func(Number) (complex128, big.Accuracy, big.Accuracy)
}

// ToFloat64WithAccuracy is the public getter — dispatches via the
// registered closure for the kind. Equivalent to calling
// values.ToFloat64WithAccuracy(n) but skips the kind lookup since
// the caller already has the spec.
func (p *NumericTypeSpec) ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    return p.toFloat64WithAccuracy(n)
}

func (p *NumericTypeSpec) ToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    return p.toComplex128WithAccuracy(n)
}

// registerNumericSpec gains two new field validators (insert into
// the existing nil-check sequence):
//   if spec.toFloat64WithAccuracy == nil { panic(...) }
//   if spec.toComplex128WithAccuracy == nil { panic(...) }
// Panic messages follow the existing pattern with werr.WrapForeignErrorf.
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

func integerToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    f, acc, _ := integerToFloat64WithAccuracy(n)
    return complex(f, 0), acc, big.Exact
}
```

**`values/big_integer.go`**:

```go
func bigIntegerToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigInteger)
    f, acc := new(big.Float).SetInt(p.value).Float64()
    return f, acc, true
}

func bigIntegerToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    f, acc, _ := bigIntegerToFloat64WithAccuracy(n)
    return complex(f, 0), acc, big.Exact
}
```

**`values/float.go`** (identity):

```go
func floatToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    return n.(*Float).Value, big.Exact, true
}

func floatToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    return complex(n.(*Float).Value, 0), big.Exact, big.Exact
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

func bigFloatToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    f, acc, _ := bigFloatToFloat64WithAccuracy(n)
    return complex(f, 0), acc, big.Exact
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

func rationalToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    f, acc, _ := rationalToFloat64WithAccuracy(n)
    return complex(f, 0), acc, big.Exact
}
```

**`values/complex.go`**:

```go
func complexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*Complex)
    return real(p.Value), big.Exact, imag(p.Value) == 0
}

func complexToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    return n.(*Complex).Value, big.Exact, big.Exact
}
```

**`values/big_complex.go`** (per-component for complex helper):

```go
func bigComplexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
    p := n.(*BigComplex)
    realF, realAcc := toBigFloat(p.real).Float64()
    return realF, realAcc, p.imag.IsZero()
}

func bigComplexToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy) {
    p := n.(*BigComplex)
    realF, realAcc := toBigFloat(p.real).Float64()
    imagF, imagAcc := toBigFloat(p.imag).Float64()
    return complex(realF, imagF), realAcc, imagAcc
}
```

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
// Returns:
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
//   - err:    ErrNotANumber (wrapped) iff n is not a Number
//
// No information loss from the Go big package is introduced by
// this helper — every signal Go's stdlib surfaces is exposed
// through the (acc, isReal, err) tuple.
//
// FFI lossy-allowed callers use this directly and discard the
// accuracy/isReal slots; FFI strict callers use ToFloat64Lossless.
func ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error) {
    if n == nil {
        return 0, big.Exact, false, werr.WrapForeignErrorf(
            werr.ErrNotANumber, "ToFloat64WithAccuracy: nil input")
    }
    spec := Lookup(n.Kind())
    f, acc, isReal := spec.ToFloat64WithAccuracy(n)
    return f, acc, isReal, nil
}

// ToFloat64Lossless is the FFI-strict convenience wrapper.
// Returns ErrLossyConversion (wrapped, with direction info) if
// the conversion would lose precision OR drop the imaginary part.
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

// ToComplex128WithAccuracy is the primary complex-domain helper.
// Returns per-component accuracy — collapsing into one signal
// would be information loss for two-component values.
func ToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy, error) {
    if n == nil {
        return 0, big.Exact, big.Exact, werr.WrapForeignErrorf(
            werr.ErrNotANumber, "ToComplex128WithAccuracy: nil input")
    }
    spec := Lookup(n.Kind())
    c, realAcc, imagAcc := spec.ToComplex128WithAccuracy(n)
    return c, realAcc, imagAcc, nil
}

// ToComplex128Lossless returns ErrLossyConversion if either
// component's accuracy is non-Exact.
func ToComplex128Lossless(n Number) (complex128, error) {
    c, realAcc, imagAcc, err := ToComplex128WithAccuracy(n)
    if err != nil {
        return 0, err
    }
    if realAcc != big.Exact {
        return c, werr.WrapForeignErrorf(werr.ErrLossyConversion,
            "ToComplex128Lossless: %T real part lost precision (%s)", n, realAcc.String())
    }
    if imagAcc != big.Exact {
        return c, werr.WrapForeignErrorf(werr.ErrLossyConversion,
            "ToComplex128Lossless: %T imaginary part lost precision (%s)", n, imagAcc.String())
    }
    return c, nil
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
        name      string
        input     Number
        wantF     float64
        wantAcc   big.Accuracy
        wantReal  bool
    }{
        // Real-domain — every kind × every accuracy outcome
        {"integer-exact",      NewInteger(7),                                       7.0,              big.Exact, true},
        {"integer-overflow-positive", NewInteger(int64(1)<<53 + 1),                  float64(int64(1)<<53), big.Below, true},
        {"big-integer-exact",  NewBigIntegerFromInt64(1234),                        1234.0,           big.Exact, true},
        {"big-integer-overflow", NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(100), nil)), math.Inf(1), big.Above, true},
        {"float-identity",     NewFloat(3.5),                                       3.5,              big.Exact, true},
        {"float-nan",          NewFloat(math.NaN()),                                math.NaN(),       big.Exact, true},
        {"big-float-nan",      NewBigFloatNaN(),                                    math.NaN(),       big.Exact, true},
        {"rational-onethird",  NewRationalFromInts(1, 3),                           0.333333333333333, big.Below, true},
        {"rational-twothirds", NewRationalFromInts(2, 3),                           0.666666666666666, big.Above, true},
        {"rational-half",      NewRationalFromInts(1, 2),                           0.5,              big.Exact, true},
        {"complex-real-only",  NewComplex(complex(3.0, 0)),                         3.0,              big.Exact, true},
        {"complex-with-imag",  NewComplex(complex(3.0, 4.0)),                       3.0,              big.Exact, false}, // imag dropped
        {"bigcomplex-exact-real", NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(0)), 3.0, big.Exact, true},
        {"bigcomplex-with-imag", NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)), 3.0, big.Exact, false},
    }
    for _, tc := range cases {
        t.Run(tc.name, func(t *testing.T) {
            c := qt.New(t)
            f, acc, real, err := ToFloat64WithAccuracy(tc.input)
            c.Assert(err, qt.IsNil)
            if math.IsNaN(tc.wantF) {
                c.Assert(math.IsNaN(f), qt.IsTrue)
            } else {
                c.Assert(f, qt.Equals, tc.wantF)
            }
            c.Assert(acc, qt.Equals, tc.wantAcc)
            c.Assert(real, qt.Equals, tc.wantReal)
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
        {"rational-half", NewRationalFromInts(1, 2), 0.5},
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
        {"rational-onethird", NewRationalFromInts(1, 3)},
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
//   {name, input, wantC, wantRealAcc, wantImagAcc, wantErr}.
//   Cover all 7 kinds × {Below, Exact, Above} for each component
//   independently. Imaginary-component Below/Above cases (e.g.,
//   BigComplex(0, 1/3) yields imagAcc=Below) are the highest-
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

3. **Extend `NumericTypeSpec`**: add the two new function-pointer
   fields plus the two new getter methods. Tighten
   `registerNumericSpec` validation to require both non-nil for
   every kind.

4. **Per-kind helpers + registration**. For each of the seven
   numeric type files: implement `<type>ToFloat64WithAccuracy` and
   `<type>ToComplex128WithAccuracy` as named package-level
   functions; reference them from `registerNumericSpec(...)` in
   the existing `init()`. Per-kind details:
   - **Integer**: round-trip via `back := int64(float64(p.Value))`
     and compare as int64 (avoids float-comparison pitfalls).
   - **BigInteger**: delegate to `new(big.Float).SetInt(p.value).Float64()`.
   - **Float**: identity; always `big.Exact`.
   - **BigFloat**: handle `p.IsNaN()` first (return `math.NaN(),
     big.Exact, true` per design); else `p.value.Float64()`.
   - **Rational**: `f, exact := p.value.Float64()`; if `!exact`,
     direction-recover via `new(big.Rat).SetFloat64(f).Cmp(p.value)`.
   - **Complex**: real part is `real(p.Value)` (identity → Exact);
     real flag is `imag(p.Value) == 0`.
   - **BigComplex**: real part via `toBigFloat(p.real).Float64()`;
     real flag is `p.imag.IsZero()`. For the complex helper:
     per-component accuracy from both `real` and `imag`.

5. **Implement the public helpers** in `values/conversion.go`
   (4 functions; `ToFloat64Lossy` dropped — see API surface
   decisions above):
   - `ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error)` —
     dispatches via the registry. Returns `ErrNotANumber` if `n`
     isn't a `Number`. **Primary API; lossy-allowed callers
     consume this directly and discard `acc`/`isReal`.**
   - `ToFloat64Lossless(n Number) (float64, error)` — wraps
     `WithAccuracy`; returns `ErrLossyConversion` (wrapped) if
     `acc != Exact || !isReal`.
   - `ToComplex128WithAccuracy(n Number) (complex128, big.Accuracy, big.Accuracy, error)` —
     dispatches; returns `ErrNotANumber` for non-Number.
   - `ToComplex128Lossless(n Number) (complex128, error)` —
     wraps `WithAccuracy`; returns `ErrLossyConversion` if
     either accuracy is non-Exact.

6. **Validation tests** (`values/conversion_test.go`):
   - Table-driven tests over the design's acceptance table.
     Each row asserts `(f, acc, real, err)` matches expected.
   - Boundary cases: `math.MaxFloat64`, `math.SmallestNonzeroFloat64`,
     `math.MinInt64`, `(*big.Int).SetInt64(math.MaxInt64).Mul(...,
     big.NewInt(2))` (overflows int64; check direction).
   - NaN propagation: `*Float(NaN)` round-trips as
     `(NaN, big.Exact, true, nil)`.
   - Rational direction: `1/3` → `'below`; `2/3` → `'above`
     (verified against actual stdlib behavior at test time).
   - Complex direction: `(make-rectangular 1/3 1/7)` returns
     `(c, Below, Below, nil)`.

7. **Registry validation extension**: add a unit test that drives
   `Lookup(k).ToFloat64WithAccuracy` and
   `Lookup(k).ToComplex128WithAccuracy` for each of the seven
   kinds, asserts non-panic and that the return shape is valid.

8. **Bench check**: run `make bench-gabriel` and confirm no hot
   path consults the new helpers (verified by grep). The helpers
   are exported but should only be called by future cold-path
   consumers (PR 2 FFI + PR 3 Scheme primitives).

9. **Lint + CI**: `make lint && make covercheck && make ci` all
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
                // Lossy-allowed: consume WithAccuracy, discard per-
                // component accuracies. Type-asserted above; the
                // error path is unreachable.
                c, _, _, _ = values.ToComplex128WithAccuracy(n)
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
| `CHANGELOG.md`                             | Document **three** behavior changes: FFI float64 precision-aware (previously rejected BigFloat unconditionally → now accepts when lossless); FFI complex128 newly supported; `helpers.ToFloat64` tightened (previously silently truncated → now errors). Plus two additions: `WithLossyConversionsAllowed` option, `ErrLossyConversion` sentinel. |

### Steps

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
   is the Complex128 block above (lines ~807-836). Strict mode
   calls `ToComplex128Lossless`; lossy-allowed mode calls
   `ToComplex128WithAccuracy` and discards `(realAcc, imagAcc)`.

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
     - `*BigComplex("1e500", "0")` → errors `ErrLossyConversion`, realAcc=Above
     - `*BigComplex("0", "1e500")` → errors `ErrLossyConversion`, imagAcc=Above
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
     A `*BigFloat` argument that fits in `float64` continues to
     succeed; one that overflows or rounds now errors with
     `ErrLossyConversion` (new sentinel). The error message names
     the direction of loss (`Above` / `Below`). Previously the
     FFI rejected `*BigFloat` unconditionally with
     `ErrTypeConversion`.

     *Net effect*: FFI now accepts strictly more inputs (any
     BigFloat fitting in float64), and reports precise errors on
     the rest. No call that succeeded before fails now; some
     calls that errored before succeed now.

   - **FFI `complex128` parameter conversion is now supported.**
     Previously, Go functions taking `complex128` parameters
     could not be registered (FFI had no converter for
     `reflect.Complex128`). Now `*Complex` and `*BigComplex`
     arguments convert with per-component precision tracking.

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
     real flags. Per-engine; captured at `RegisterFunc` time.

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

7. **Pre-PR-2 audit checklist** (do this BEFORE writing code):
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
   - `grep -rn 'helpers.ToFloat64' --include='*.go'` — list every
     caller. Audit error-handling at each site.
   - `grep -rn 'reflect.Complex128' --include='*.go'` — verify
     no existing FFI test asserts that registration fails for a
     `complex128` parameter. (If any exist, they need updating to
     assert the new succeeds-behavior.)

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
    _, realAcc, imagAcc, err := values.ToComplex128WithAccuracy(n)
    if err != nil {
        return err
    }
    lossless := realAcc == big.Exact && imagAcc == big.Exact
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
    // Distinguish real-domain from complex-domain by checking the
    // type — Complex/BigComplex always use the 2-value path.
    switch n.(type) {
    case *values.Complex, *values.BigComplex:
        _, realAcc, imagAcc, err := values.ToComplex128WithAccuracy(n)
        if err != nil {
            return err
        }
        mc.SetValues(
            values.BigAccuracyToSymbol(realAcc),
            values.BigAccuracyToSymbol(imagAcc),
        )
        return nil
    default:
        _, acc, _, err := values.ToFloat64WithAccuracy(n)
        if err != nil {
            return err
        }
        mc.SetValue(values.BigAccuracyToSymbol(acc))
        return nil
    }
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
    switch n.(type) {
    case *values.Complex, *values.BigComplex:
        c, realAcc, imagAcc, err := values.ToComplex128WithAccuracy(n)
        if err != nil {
            return err
        }
        mc.SetValues(
            values.NewComplex(c),
            values.BigAccuracyToSymbol(realAcc),
            values.BigAccuracyToSymbol(imagAcc),
        )
        return nil
    default:
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
    c, realAcc, imagAcc, err := values.ToComplex128WithAccuracy(n)
    if err != nil {
        return err
    }
    mc.SetValues(
        values.NewComplex(c),
        values.BigAccuracyToSymbol(realAcc),
        values.BigAccuracyToSymbol(imagAcc),
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

// Analogous tests for inexact-with-accuracy (real returns 2-value
// list, complex returns 3-value list) and complex-inexact-with-accuracy
// (always 3-value).
func TestInexactWithAccuracy(t *testing.T) {
    realCases := []testhelpers.SchemeCodeTestCase{
        {Name: "integer-7",
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 7)) list)`,
            Expected: values.List(values.NewFloat(7.0), values.NewSymbol("exact"))},
        {Name: "rational-onethird",
            // Match the float64 value emitted by 1/3 conversion explicitly
            // — the exact float64 differs from 0.333... in the last bits.
            Code:     `(call-with-values (lambda () (inexact-with-accuracy 1/3)) list)`,
            Expected: values.List(values.NewFloat(1.0/3.0), values.NewSymbol("below"))},
        // ... boundary cases per design's acceptance table ...
    }
    // ... iterate ...
}
```

### Library export

The math extension is registered via `registry.NewDescribedExtension`
in `extensions/math/register.go:26-28`. Adding primitives to the
extension's primitive list (via `r.AddPrimitives(...)` inside
`addPrimitives`) automatically makes them available when the
extension is loaded by an engine — no separate `.sld` library
file edit needed for the runtime path.

For embedders using `(import (wile math))` or similar, verify the
library export through the existing math `.sld` (search:
`grep -r "scheme inexact\|wile math" stdlib/lib/`). If a Scheme-
level library file lists individual exports, append the four new
primitive names there. Otherwise, the primitives are available
unconditionally to any code that loads the math extension.

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
| `extensions/math/prim_conversion.go`              | Add four primitives: `PrimInexactLosslessP`, `PrimInexactAccuracy`, `PrimInexactWithAccuracy`, `PrimComplexInexactWithAccuracy`. Wire in the extension's primitive registration. |
| `extensions/math/prim_conversion_test.go`         | Table-driven tests covering the design's acceptance table.                                                                                   |
| `stdlib/lib/wile/math.sld` (or similar)           | Export the new primitives so `(import (wile math))` makes them available.                                                                    |
| `docs/extensions/architecture.md` (optional)      | Note the four primitives under the math extension entry, if the doc enumerates by primitive.                                                 |

### Steps

1. **Implement `PrimInexactLosslessP`** (predicate `inexact-lossless?`):
   ```go
   func PrimInexactLosslessP(mc *machine.MachineContext) error {
       n, ok := mc.Arg(0).(values.Number)
       if !ok {
           return werr.WrapForeignErrorf(werr.ErrNotANumber,
               "inexact-lossless?: argument is not a number")
       }
       // For real n, lossless iff accuracy is Exact.
       // For complex n with non-zero imag, lossless iff BOTH
       // per-component accuracies are Exact (using complex128
       // semantics).
       _, realAcc, imagAcc, err := values.ToComplex128WithAccuracy(n)
       if err != nil {
           return err
       }
       lossless := realAcc == big.Exact && imagAcc == big.Exact
       mc.SetValue(values.BoolToBoolean(lossless))
       return nil
   }
   ```

2. **Implement `PrimInexactAccuracy`** (`inexact-accuracy`):
   - Real input: returns single symbol via `mc.SetValue(symbol)`.
   - Complex input: returns two values via the multiple-value
     mechanism (`mc.SetValues(...)` or whatever the codebase
     pattern is — consult `prim_misc_test.go` for the existing
     multi-value primitive precedent).

3. **Implement `PrimInexactWithAccuracy`**:
   - Real: `(values f acc-sym)` — two values.
   - Complex: `(values c real-acc-sym imag-acc-sym)` — three values.
   - Wrap the float64/complex128 result back into a Scheme
     `*Float` / `*Complex` via existing constructors.

4. **Implement `PrimComplexInexactWithAccuracy`** (uniform
   3-value):
   - Always returns `(values c real-acc-sym imag-acc-sym)`.
   - Real inputs get `imag-acc = 'exact` trivially.

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

### Acceptance for PR 3

- All four primitives implemented, registered, exported.
- Every row of the design's acceptance table passes as a Scheme
  test case.
- Docstrings render correctly via `(doc inexact-lossless?)` etc.
- `apropos` discovers all four under "Numbers — Conversion".
- Integration test passes.

## Risk register (impl-specific)

| # | Risk                                                                       | Mitigation                                                                  |
|---|----------------------------------------------------------------------------|-----------------------------------------------------------------------------|
| I1 | Multiple-value-return shape on Scheme primitives diverges from existing prim conventions | Audit `prim_misc_test.go` and the `WithSingleResult`/multi-value precedent BEFORE writing PR 3. Follow the existing pattern verbatim. |
| I2 | Direction-recovery for `*big.Rat` round-trip allocates a new `big.Rat` per call | Test-confirmed cold path. If a profile shows this on a hot path, cache the round-trip or switch to a direct big.Float compare. |
| I3 | ~~Order-of-init dependency~~ — **RESOLVED**: numeric-registry Phase 3 merged at `51b1176a`. The registry is in place; PR 1 extends `NumericTypeSpec` directly. | n/a |
| I4 | FFI `reflect.Complex128` is a *new* converter (no current code) — could surprise registrants who relied on previous "Go function with complex128 parameter is unregisterable" behavior | The change is additive. Add a CHANGELOG note. The pre-change failure mode was a *registration* error (Go panic / FFI build error) — no callers can have built around it; only "I tried it once and stopped" users are affected, and they're now unblocked. |
| I5 | Engine-level `WithLossyConversionsAllowed` interacts with multi-engine embedders (one engine strict, another lossy) | The flag is per-engine instance, set at construction. No global state. Document. |
| I6 | Saturation-to-±Inf may surprise users who expect an error on overflow rather than `+inf.0` | `inexact-with-accuracy` documents this; the `'above` / `'below` accuracy symbol IS the signal. R7RS itself allows the saturation (`exact->inexact (expt 10 100)` returns `+inf.0`); we just expose the *direction*. |
| I7 | `BigComplex` with NaN parts — what's the accuracy? | Per the design's NaN handling rule: NaN propagates as `Exact` (identity). A `*BigComplex(NaN, NaN)` returns `(complex(NaN,NaN), Exact, Exact)`. Tests cover this explicitly. |
| I8 | The `helpers.ToFloat64` Q-5 **tightening** breaks callers that today depend on silent truncation of `*BigFloat` / `*BigInteger` overflow / `*Rational`. Three sites change from "success with silently-truncated value" to "error with `ErrLossyConversion`." | Audit before PR 2 lands: grep `helpers.ToFloat64` for all callers; classify each by error-handling shape (propagates / catches-and-defaults / ignores). Document the tightening prominently in CHANGELOG (template above). For callers that need the legacy silent path, the explicit recovery is `values.ToFloat64WithAccuracy(n)` and discard the accuracy slot. |

## Cross-references

- `plans/2026-05-14-numeric-loss-signals-design.md` — design
  source (refined; all Q-1…Q-5 resolved).
- `plans/2026-05-14-numeric-registry-design.md` /
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
| `extensions/math/CLAUDE.local.md` | Four new primitives added to the "Numeric Predicates" / "Numeric Conversion" sections. |

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

- [ ] PR 1 merged: Go infrastructure exposed, registry extended,
      tests pass.
- [ ] PR 2 merged: FFI tightening + complex128 + helpers.ToFloat64
      **tightening**; CHANGELOG entries posted; **documentation
      deliverables above all landed**.
- [ ] PR 3 merged: four Scheme primitives shipped; acceptance
      table from design passes as Scheme tests.
- [ ] Parent design plan moves to "Completed Plans" in
      `plans/CLAUDE.md`.

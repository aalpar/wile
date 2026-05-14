# Numeric loss signals — implementation plan

**Date**: 2026-05-14
**Status**: Plan locked in (all Q-1…Q-5 resolved post-approval);
  awaiting numeric-registry Phase 3 to merge before PR 1 starts.
**Design source**: `plans/2026-05-14-numeric-loss-signals-design.md`
  (refined 2026-05-14; resolutions: Q-1 saturate ±Inf, Q-2
  real-triple + per-component complex, Q-3 `extensions/math/`,
  Q-4 engine-level opt-in, Q-5 yes-tighten-helpers).
**Branches**:
  - PR 1: `feat/numeric-loss-signals-go` — branched from master
    after numeric-registry Phase 3 closes.
  - PRs 2 and 3 branch from master after each preceding PR
    merges.

## Sequence overview

| PR | Scope                                                                                    | Bench gate | Est. delta   |
|----|------------------------------------------------------------------------------------------|------------|--------------|
| 1  | Go infrastructure: sentinel + accuracy symbols + per-kind helpers + values/ exports + registry extension | yes — verify cold-path discipline | +400 / −20  |
| 2  | FFI tightening (Float64 + Complex128 paths) + `WithLossyConversionsAllowed` engine option + `helpers.ToFloat64` Q-5 widening | yes — FFI conversion hot-ish | +180 / −50 |
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

### Files added

- `werr/werr.go` — add `ErrLossyConversion` sentinel.
- `values/conversion.go` — new file housing the public helpers
  (`ToFloat64WithAccuracy`, `ToFloat64Lossless`,
  `ToFloat64Lossy`, `ToComplex128WithAccuracy`,
  `ToComplex128Lossless`). ~120 LOC.
- `values/conversion_test.go` — table-driven tests covering the
  acceptance table from the design doc (~200 LOC).
- `values/symbols_accuracy.go` (or extend an existing
  `values/symbols.go`) — global accuracy singleton symbols.

### Files modified

| File                              | Change                                                                                                               |
|-----------------------------------|----------------------------------------------------------------------------------------------------------------------|
| `werr/werr.go`                    | New `ErrLossyConversion = NewStaticError("lossy conversion")`.                                                       |
| `werr/CLAUDE.md`                  | Document the new sentinel in the inventory.                                                                          |
| `values/numeric_registry.go`      | Extend `NumericTypeSpec` with `toFloat64WithAccuracy` and `toComplex128WithAccuracy` function-pointer fields + corresponding `ToFloat64WithAccuracy` / `ToComplex128WithAccuracy` getter methods. Update `registerNumericSpec` validation to require these fields non-nil. |
| `values/integer.go`               | New `integerToFloat64WithAccuracy` + `integerToComplex128WithAccuracy` named helpers; register in `init()`.          |
| `values/big_integer.go`           | Same shape; uses `new(big.Float).SetInt(p.value).Float64()`.                                                         |
| `values/float.go`                 | Same shape; identity for `Float64`; `(complex(p.Value, 0), Exact, Exact)` for complex helper.                        |
| `values/big_float.go`             | Same shape; handles NaN flag explicitly before calling `.Float64()`.                                                  |
| `values/rational.go`              | Same shape; direction-recovery via `new(big.Rat).SetFloat64(f).Cmp(p.value)`.                                        |
| `values/complex.go`               | Same shape; real part is identity; imaginary handled in `toComplex128WithAccuracy`.                                  |
| `values/big_complex.go`           | Same shape; per-component accuracy for complex helper.                                                                |
| `values/numeric_kind.go`          | Update ADDING-A-NEW-NUMERIC-TYPE guide to include the new spec fields.                                                |

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

5. **Implement the public helpers** in `values/conversion.go`:
   - `ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error)` —
     dispatches via the registry. Returns `ErrNotANumber` if `n`
     isn't a `Number`.
   - `ToFloat64Lossless(n Number) (float64, error)` — wraps
     `WithAccuracy`; returns `ErrLossyConversion` (wrapped) if
     `acc != Exact || !real`.
   - `ToFloat64Lossy(n Number) (float64, bool)` — wraps
     `WithAccuracy`; returns `(f, acc == Exact && real)`. No
     error path; non-Number inputs panic (the caller is expected
     to have validated already — this is the Lossy-OK path).
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

### Files modified

| File                                       | Change                                                                                                                                                       |
|--------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ffi.go` / engine.go                       | New `WithLossyConversionsAllowed()` engine option. The flag stored on `*Engine`; threaded into FFI spec at `RegisterFunc` time.                              |
| `ffi_arg_converters.go`                    | `reflect.Float64` case migrates to `ToFloat64Lossless` (or `ToFloat64Lossy` when the engine flag is set). New `reflect.Complex128` case added analogously.   |
| `registry/helpers/value_conv.go`           | `ToFloat64` migrates to `values.ToFloat64Lossless` for the 5 reducible kinds. BigFloat newly accepted when fits; rejected with `ErrLossyConversion` otherwise. |
| `ffi_test.go`                              | New tests cover the widening + opt-in + complex128 path.                                                                                                     |
| `CHANGELOG.md`                             | Document the three behavior changes (BigFloat→float64 widening, BigComplex→complex128 widening, helpers.ToFloat64 widening).                                |

### Steps

1. **Add `wile.WithLossyConversionsAllowed()` engine option**.
   Plumb the flag to `*Engine` and into the per-function FFI
   spec at registration time (closure captures the value at
   `RegisterFunc` call).

2. **Migrate `reflect.Float64` path**:
   ```go
   case reflect.Float64:
       targetType := t
       lossyAllowed := p.lossyConversionsAllowed
       return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
           n, ok := v.(values.Number)
           if !ok {
               return reflect.Value{}, fmtArgError(name, pos, "number", v)
           }
           var f float64
           var err error
           if lossyAllowed {
               f, _ = values.ToFloat64Lossy(n)
           } else {
               f, err = values.ToFloat64Lossless(n)
           }
           if err != nil {
               return reflect.Value{}, werr.WrapForeignErrorf(
                   err, "%s: argument %d: cannot losslessly convert %T to float64",
                   name, pos, v,
               )
           }
           return reflect.ValueOf(f).Convert(targetType), nil
       }, nil
   ```

3. **Add `reflect.Complex128` case** analogously, consulting
   `ToComplex128Lossless` / `ToComplex128Lossy`.

4. **Migrate `helpers.ToFloat64`** (per Q-5). Replace the current
   5-case switch with a call to `values.ToFloat64Lossless`. The
   error shape changes from `ErrNotAReal` (broad) to
   `ErrLossyConversion` (precise) for `*BigFloat` that doesn't
   fit; `*Complex`/`*BigComplex` continue to return `ErrNotAReal`
   (they're not real).

5. **Tests** (`ffi_test.go`):
   - **Widening table**: `*BigFloat(1.5)` → succeeds as float64.
   - **Narrowing table**: `*BigFloat(2^100)` → errors with
     `ErrLossyConversion` (verify via `errors.Is`); message
     contains direction (`Above`).
   - **Complex128 widening**: `*BigComplex(3+4i)` → succeeds.
   - **Complex128 narrowing**: `*BigComplex(2^100+1i)` → errors
     with `ErrLossyConversion`.
   - **Opt-in**: engine with `WithLossyConversionsAllowed()` —
     `*BigFloat(2^100)` succeeds (silently truncated to `+Inf`).
   - **`helpers.ToFloat64` widening**: BigFloat that fits now
     succeeds; one that overflows errors with `ErrLossyConversion`.

6. **Update CHANGELOG.md** with three subsections:
   - "FFI float64 conversion now precision-aware" — describes
     the widening + the new typed error.
   - "FFI complex128 conversion now supported" — describes the
     new capability.
   - "`registry/helpers/value_conv.ToFloat64` widened" — for
     embedder migration awareness.

7. **Bench check**: FFI conversion is borderline cold (called
   per FFI call, not per arithmetic op). Run `make bench-gabriel`
   + FFI-heavy benches; verify ≤ 0.5% geomean delta.

8. **Lint + CI**.

### Acceptance for PR 2

- FFI `reflect.Float64` and `reflect.Complex128` paths consult
  the new helpers.
- `wile.WithLossyConversionsAllowed()` option exists and works
  (verified by table case).
- `helpers.ToFloat64` widened for `*BigFloat`.
- The acceptance-table FFI rows from the design pass as Go tests.
- CHANGELOG documents the three behavior changes.
- No bench regression beyond noise.

## PR 3 — Scheme primitives

### Goal

Implement the four Scheme primitives. Cold-path; no bench gate.

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
| I3 | Order-of-init dependency: PR 1 extends `NumericTypeSpec` *after* numeric-registry Phase 3 establishes the registry | This plan is explicitly sequenced AFTER numeric-registry Phase 3 closes; PR 1 branches from master post-merge. |
| I4 | FFI `reflect.Complex128` is a *new* converter (no current code) — could surprise registrants who relied on previous "Go function with complex128 parameter is unregisterable" behavior | The change is additive. Add a CHANGELOG note. The pre-change failure mode was a *registration* error (Go panic / FFI build error) — no callers can have built around it; only "I tried it once and stopped" users are affected, and they're now unblocked. |
| I5 | Engine-level `WithLossyConversionsAllowed` interacts with multi-engine embedders (one engine strict, another lossy) | The flag is per-engine instance, set at construction. No global state. Document. |
| I6 | Saturation-to-±Inf may surprise users who expect an error on overflow rather than `+inf.0` | `inexact-with-accuracy` documents this; the `'above` / `'below` accuracy symbol IS the signal. R7RS itself allows the saturation (`exact->inexact (expt 10 100)` returns `+inf.0`); we just expose the *direction*. |
| I7 | `BigComplex` with NaN parts — what's the accuracy? | Per the design's NaN handling rule: NaN propagates as `Exact` (identity). A `*BigComplex(NaN, NaN)` returns `(complex(NaN,NaN), Exact, Exact)`. Tests cover this explicitly. |
| I8 | The new `helpers.ToFloat64` widening (Q-5) might break a caller who relies on `ErrNotAReal` for `*BigFloat` specifically | Audit before PR 2 lands: grep `errors.Is(err, ErrNotAReal)` in callers of `helpers.ToFloat64`. Document in CHANGELOG. The change is strictly more permissive for the 5-reducible-kinds set (no successful call becomes a failure). |

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
- `registry/helpers/value_conv.go` — site of the Q-5 widening
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

## Done definition (whole plan)

- [ ] PR 1 merged: Go infrastructure exposed, registry extended,
      tests pass.
- [ ] PR 2 merged: FFI tightening + complex128 + helpers.ToFloat64
      widening; CHANGELOG entries posted.
- [ ] PR 3 merged: four Scheme primitives shipped; acceptance
      table from design passes as Scheme tests.
- [ ] Parent design plan moves to "Completed Plans" in
      `plans/CLAUDE.md`.

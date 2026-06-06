# Numeric registry — implementation plan (Phase 3, `values/` SR)

**Date**: 2026-05-14
**Status**: **Shipped (PR #752, merged).** All PRs landed; `values/numeric_registry.go` on master.
**Design source**: `memory/2026-05-14-numeric-registry-design.md`
  (Q-resolutions baked in: Q-a cold-path only; Q-b drop wile-goast
  scope; Q-c→Q-i resolved C3 — registry's `ToFloat64` covers only
  the 5 reducible kinds, FFI excluded; Q-d D1 leave `Eqv` as a
  switch; Q-e E2 three medium PRs; Q-f drop `IsInteger`/`IsRational`
  fields; Q-g `isAlwaysExact bool` instead of `Exactness Exactness`;
  Q-h unexport spec fields + getter methods.)
**Branches**:
  - PR 1 (this plan's first phase): branch
    `feat/values-sr-phase3-numeric-registry` — drops the `-design`
    suffix from the current docs branch
    (`feat/values-sr-phase3-numeric-registry-design`) to match the
    prior-phase convention (PR #748 `feat/values-sr-phase1-mutex`,
    PR #749 `feat/values-sr-phase2-port-unification` — topic
    suffix, not artifact suffix).
  - PRs 2 and 3 branch from master after each preceding PR merges.

## Sequence overview

| PR | Scope                                                                                      | Bench gate | Est. delta   |
|----|--------------------------------------------------------------------------------------------|------------|--------------|
| 1  | New `werr.ErrNumericRegistry` sentinel + `values/numeric_registry.go` (struct + 5 getter methods per Q-h + `registerNumericSpec` + `Lookup` + `ensureNumericRegistryInit`); 7 per-type `init()` registrations with named helper functions per kind; migrate `Simplify`, `ExactnessOf`, `NumberToFloat64`, `NumberToComplex128` to read from registry | yes | +210 / −40   |
| 2  | Migrate `registry/helpers/value_conv.go` (`ToFloat64`, `ToComplex128`, `ExtractReal`) only. **FFI excluded** per Q-i=C3 — handled by the loss-signals follow-up plan. | yes (cold-path, light) | +15 / −55 |
| 3  | Migrate `extensions/math/prim_conversion.go` + `prim_complex.go` (extraction/exactness only; `(integer? x)` and `(rational? x)` predicate sites stay on per-instance methods per Q-f); rewrite the `numeric_kind.go` ADDING-A-NEW-NUMERIC-TYPE guide | no (cold path only) | +30 / −180   |

Cumulative net: ≈ **−20 LOC** + the 10-site update obligation
collapses to **1 site** (the spec registration on the type file).
The unexported-fields-plus-getter-methods shape (Q-h) adds ~30 LOC
of method scaffolding over the original exported-fields design,
which is the bulk of the net delta change vs. the pre-crosscheck
estimate of ~−60 LOC. The encapsulation win is worth the LOC.

## PR 1 — Registry + values/ migration

### Goal

Introduce the `NumericTypeSpec` registry and migrate the two
`values/`-local cold-path consumers (`numeric_tower.go` and the
fallback helpers in `promotion.go`).

### Files added

- `werr/werr.go` — new `ErrNumericRegistry` sentinel
  (`NewStaticError("numeric registry violation")`). Added in
  the very first commit of PR 1.
- `values/numeric_registry.go` — registry type, accessor,
  validation hook (≈ 110 LOC: struct + 5 getter methods + register
  + Lookup + ensureInit).
- `values/numeric_registry_test.go` — completeness + per-kind
  spec invariants + behavioral-equivalence golden test
  (≈ 120 LOC).

### Files modified

| File                          | Change                                                                                            |
|-------------------------------|---------------------------------------------------------------------------------------------------|
| `werr/werr.go`                | Add `ErrNumericRegistry` sentinel (step 1 below).                                                 |
| `values/integer.go`           | Add per-kind `integerSimplifyDown`/`integerToFloat64`/`integerToComplex128` funcs + `registerNumericSpec(KindInteger, …)` in existing `init()`. |
| `values/big_integer.go`       | Same shape; `simplifyDown` demotes BigInteger fitting `int64` to `Integer`.                       |
| `values/float.go`             | Same shape; `simplifyDown` demotes whole-number `Float` to `Integer`.                             |
| `values/big_float.go`         | Same shape; `simplifyDown` demotes integral `BigFloat` to `BigInteger`. `toFloat64` accepts BigFloat per the 5-kind set (this is C3-aligned widening — BigFloat *is* one of the 5 reducible kinds; the C3 exclusion targets Complex/BigComplex). |
| `values/rational.go`          | Same shape; `simplifyDown` demotes integer-valued `Rational` to `BigInteger`. `toFloat64` uses `(*big.Rat).Float64()`; discards the bool (loss-signal handling deferred to `2026-05-14-numeric-loss-signals-design.md`). |
| `values/complex.go`           | Same shape; `simplifyDown` demotes zero-imaginary `Complex` to `Float`. `toFloat64` returns `werr.WrapForeignErrorf(werr.ErrNotAReal, …)` per Q-i=C3. |
| `values/big_complex.go`       | Same shape; `simplifyDown` demotes zero-imaginary `BigComplex` via existing `Simplify(v.Real())` cross-kind path. `toFloat64` returns `werr.WrapForeignErrorf(werr.ErrNotAReal, …)` per Q-i=C3. `isAlwaysExact: false` per Q-g; consumers consult `ExactnessOf(n)` for per-instance answer. |
| `values/numeric_tower.go`     | `Simplify`'s arms migrate to a thin wrapper over `Lookup(n.Kind()).SimplifyDown(n)` + the cross-kind `BigComplex` zero-imag shortcut. `ExactnessOf` reads `Lookup(n.Kind()).IsAlwaysExact()` plus the per-instance `BigComplex` fall-through. |
| `values/promotion.go`         | `NumberToFloat64` rewritten to `f, err := Lookup(n.Kind()).ToFloat64(n); if err != nil { panic … }`. `NumberToComplex128` rewritten to `Lookup(n.Kind()).ToComplex128(n)` (universal — no error path). |
| `values/numeric_kind.go`      | ADDING guide updated — but kept conservative; the full rewrite happens in PR 3. |

### Steps (tasks → atomic commits)

1. **Add the `werr.ErrNumericRegistry` sentinel.** One-line addition
   to `werr/werr.go` plus update to `werr/CLAUDE.md` if it
   enumerates sentinels. The sentinel is referenced by every panic
   site in the new registry code.

2. **Spec stub + accessors.** Create `values/numeric_registry.go`
   with the unexported-fields struct, 5 getter methods (per Q-h),
   `registerNumericSpec(kind, spec)` (per Q-h: kind passed
   positionally; no `Kind` field on the spec), `Lookup` wired
   through `ensureNumericRegistryInit`. No consumers yet. Add a
   compile-only test.

3. **Per-type registration.** For each of the seven type files,
   add three named helper functions (`<type>SimplifyDown`,
   `<type>ToFloat64`, `<type>ToComplex128`) and a
   `registerNumericSpec(Kind<type>, …)` call inside the existing
   `init()`. Bottom-of-chain kinds bind an identity `simplifyDown`
   — every spec field is non-nil (no nil sentinels per the
   crosscheck recommendation R5).

4. **Validation tests.** Expand `numeric_registry_test.go` per
   the crosscheck tests-lens Q1 recommendation:
   - `TestNumericRegistryAllKindsRegistered` — iterate
     `[0, numKinds)`; assert `schemeName != ""`,
     `simplifyDown != nil`, `toFloat64 != nil`,
     `toComplex128 != nil`.
   - `TestNumericRegistrySmoke` — for each kind, drive a
     representative exemplar through `SimplifyDown`,
     `ToFloat64`, `ToComplex128` via the public methods;
     assert "does not panic, returns non-zero where expected".
   - `TestEnsureNumericRegistryInitPanics` — construct a
     local zero-filled spec array, invoke a private validation
     entry point, assert it panics with `werr.ErrNumericRegistry`
     wrapped. (Tests the validator itself, not just live
     registration state.)
   - `TestRegisterNumericSpecDuplicateRejected` — register
     `KindInteger` twice; assert second registration panics with
     `werr.ErrNumericRegistry`. Repeat for a non-zero kind
     (`KindBigComplex`) to guard against `KindInteger == 0`
     special-case regressions.

5. **Migrate `Simplify`.** Replace the 7-arm switch with a thin
   wrapper:

   ```go
   func Simplify(n Number) Number {
       if bc, ok := n.(*BigComplex); ok && bc.Imag().IsZero() {
           return Simplify(bc.Real())  // cross-kind shortcut
       }
       if c, ok := n.(*Complex); ok && imag(c.Value) == 0 {
           return Simplify(NewFloat(real(c.Value)))  // cross-kind
       }
       return Lookup(n.Kind()).SimplifyDown(n)
   }
   ```

   The cross-kind BigComplex/Complex shortcuts stay in `Simplify`
   itself — they're *cross-row* moves that don't fit the per-kind
   spec. Per-kind same-row descent (`Rational → BigInteger →
   Integer`) lives in the spec's `simplifyDown` closures.

6. **Migrate `ExactnessOf`.** Replace with:

   ```go
   func ExactnessOf(n Number) Exactness {
       if bc, ok := n.(*BigComplex); ok {
           if bc.IsExact() {
               return Exact
           }
           return Inexact
       }
       if Lookup(n.Kind()).IsAlwaysExact() {
           return Exact
       }
       return Inexact
   }
   ```

7. **Migrate `NumberToFloat64`.** Replace switch with the
   registry lookup. The 7 spec entries each carry the same
   conversion they had inside the switch; the BigComplex/Complex
   arms now return `(0, ErrNotAReal)` rather than panicking — the
   wrapper panics if it gets a non-nil error (matches prior
   behavior of the function, which panicked on unsupported types).

8. **Migrate `NumberToComplex128`.** Replace switch with
   `Lookup(n.Kind()).ToComplex128(n)` — universal, no error path.

9. **Behavioral-equivalence golden test** (per crosscheck tests-lens
   C2). Add `TestSimplifyEquivalence`, `TestExactnessOfEquivalence`,
   `TestNumberToFloat64Equivalence`, `TestNumberToComplex128Equivalence`.
   Each drives a fixed-roster of 12 exemplars (including all
   boundary cases: `Float(3.0)`, `Float(3.5)`, `BigFloat(2.0)`,
   `Rational(6/2)`, `Rational(7/2)`, `Complex(3+0i)`,
   `Complex(3.5+0i)`, `BigComplex(BigInt(3), BigInt(0))`,
   `BigComplex(BigInt(3), BigInt(4))`) through both the
   post-migration registry path AND a captured pre-migration
   output table (golden file or inline `[]struct{ … }`). Asserts
   equality.

10. **Run benches.** `make bench-gabriel` and the math benches.
    Compare against master baseline (refresh first). Require
    ≤ 0.5% geomean delta. Spot-check that no individual benchmark
    regresses > 1%.

11. **Lint + ci.** `make lint && make covercheck && make ci` all
    pass before push.

### Acceptance for PR 1

- Registry has 7 spec entries; every spec is internally well-formed
  (validated by `registerNumericSpec` at registration time, plus
  the lazy `ensureNumericRegistryInit` validating global
  completeness).
- `Simplify`, `ExactnessOf`, `NumberToFloat64`,
  `NumberToComplex128` read from the registry; no per-kind
  switches remain except the documented cross-kind BigComplex/Complex
  shortcuts in `Simplify` and the per-instance `BigComplex.IsExact()`
  branch in `ExactnessOf`.
- Hot-path arithmetic dispatch closures (those generated by
  `makeArithmeticDispatch` and friends in `promotion.go:398-462`)
  introduce **no new indirect call** — the registry is read only
  by cold-path functions. Structural guarantee; verified by grep
  + `make bench-gabriel`.
- `make bench-gabriel` geomean within ≤ 0.5% of master.
- All existing tests green; new completeness, smoke,
  duplicate-rejection, validator-panic, and behavioral-equivalence
  tests green.

## PR 2 — registry/helpers migration (FFI excluded per Q-i=C3)

### Goal

Migrate the cross-package cold-path duplicators in
`registry/helpers/value_conv.go` for the 5 reducible kinds only.
`Eqv` stays as a switch per Q-d. **FFI excluded** per Q-i=C3 —
the FFI float64 path keeps its existing 5-case switch and
existing rejection behavior for BigFloat/BigComplex inputs.
Precision-loss detection at the FFI boundary is the subject of
follow-up plan `2026-05-14-numeric-loss-signals-design.md`.

### Files modified

| File                                 | Change                                                                                       |
|--------------------------------------|----------------------------------------------------------------------------------------------|
| `registry/helpers/value_conv.go`     | `ToFloat64`, `ToComplex128`, `ExtractReal` rewritten to read from registry **for the 5 reducible kinds**. BigComplex continues to error with `ErrNotAReal` from `ExtractReal` (`ToComplex128` is universal — covers all 7). |
| `ffi_arg_converters.go`              | **NO CHANGE** (per Q-i=C3). FFI float64 path stays as its current 5-case switch. Tightening to "accept-when-fits, error-when-lossy" is deferred to `2026-05-14-numeric-loss-signals-design.md` Phase 2. |
| `registry/helpers/equality.go`       | **NO CHANGE** (per Q-d). Documented as a deliberate exception — referenced from the design doc. |

### Steps

1. **`ToFloat64` migration** (`value_conv.go`). One-line lookup for
   the 5 reducible kinds; BigFloat is now accepted (was rejected
   today — minor widening, in the same direction as today's
   `Integer/BigInteger/Float/Rational` coverage but adding
   BigFloat); BigComplex/Complex continue to raise `ErrNotAReal`.
2. **`ToComplex128` migration** (`value_conv.go`). All 7 kinds —
   the spec's `ToComplex128` is universal. No behavior change.
3. **`ExtractReal` migration** (`value_conv.go`). Two registry
   reads: `ToFloat64` for the value (5-kind), `IsAlwaysExact()`
   for the bool (per Q-g). BigComplex/Complex continue to raise
   `ErrNotAReal` (real-extraction is real-only by definition).
4. **FFI** — **no change** this PR. The float64 path stays as the
   current 5-case switch. Q-i=C3 chose to defer FFI tightening to
   the loss-signals plan.
5. **Bench.** Lookup is one indexed array access, no regression
   expected on cold paths; verify.
6. **Lint + ci.**

### Acceptance for PR 2

- `value_conv.go` no longer holds numeric-kind switches for the
  five reducible kinds.
- `ffi_arg_converters.go` **unchanged**.
- `equality.go` `Eqv` is unchanged (and documented as deliberate
  in a comment that points at the design doc).
- No FFI behavior change in this PR. Loss-signal-aware FFI
  tightening lives in `2026-05-14-numeric-loss-signals-design.md`
  Phase 2.
- All existing FFI tests pass; no new FFI tests required.
- No bench regression beyond noise.

## PR 3 — extensions/math migration + ADDING guide rewrite

### Goal

Migrate the bulk of the duplicate switches and finalize the
ADDING-A-NEW-NUMERIC-TYPE guide.

### Files modified

| File                                          | Change                                                                                                 |
|-----------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `extensions/math/prim_conversion.go`          | `exact->inexact`, `inexact->exact` switches → registry consults `ToFloat64` (5-kind) and `IsAlwaysExact()`. **`integer?` / `rational?` predicate call sites are NOT migrated** (per Q-f: those are R7RS per-instance predicates; they continue to call `n.IsInteger()` / `n.IsRational()` on the `Number` interface directly). `number->string` currently delegates to `SchemeString()` — no migration needed. |
| `extensions/math/prim_complex.go`             | `make-rectangular`, `make-polar`, `magnitude`, `angle`, real-part/imag-part — switches that extract `complex128` collapse to `spec.ToComplex128(n)`. Switches that check exactness use `spec.IsAlwaysExact()` (plus per-instance `ExactnessOf` for BigComplex). Construction-side switches that build a kind-specific result stay as-is. |
| `values/numeric_kind.go`                      | Rewrite the ADDING-A-NEW-NUMERIC-TYPE guide. The 12-item list collapses to: (1) add `KindXxx`; (2) new type file with dispatch tables + `registerNumericSpec(KindXxx, …)`; (3) `promotionTable`/`promoter` rows in `promotion.go`; (4) `numeric_dispatch_test.go` roster. Items 6–12 disappear — they now read the spec automatically. |
| `BIBLIOGRAPHY.md` (optional)                  | Add a reference under "Numeric Type Registry" if the design notes are worth citing. Defer unless natural fit. |

### Steps

1. **`exact->inexact`** in `prim_conversion.go`. Each arm that
   *extracts* a float64 uses `spec.ToFloat64`. Arms that
   *construct* a kind-specific result (e.g., `Rational → BigFloat`
   simplification) stay as-is — they're per-kind code, not
   duplication.
2. **`number->string`** — verify no migration needed (delegates to
   `SchemeString()`).
3. **`prim_complex.go`** sweep. Each numeric switch is one of:
   (a) extracting a complex128 → use `spec.ToComplex128(n)`;
   (b) checking exactness → use `spec.IsAlwaysExact()` plus
   per-instance `ExactnessOf` for BigComplex;
   (c) constructing a kind-specific result → stays as-is.
   `(integer? x)` / `(rational? x)` style checks **stay on the
   value methods** per Q-f.
4. **ADDING guide rewrite.** Replace the 12-item list with the
   collapsed 4-item version. Update
   `memory/2026-05-13-values-structural-reduction.md` Finding 3's
   "leakage sites" annotation to reference this PR as the
   resolution.
5. **Lint + ci.** No bench gate (extensions are cold paths).

### Acceptance for PR 3

- `extensions/math/` no longer holds numeric-kind switches in the
  migrated primitives. Construction-side switches (which build new
  values per kind) may remain — those are not duplication, they're
  per-kind code.
- R7RS predicate semantics preserved: `(integer? 3.0)` returns
  `#t` after the migration. Verified by `prim_misc_test.go` and a
  new dedicated regression test if needed.
- `numeric_kind.go` guide is updated and points at the registry
  as the single source of truth.
- All math primitive tests pass.

## Risk register

| # | Risk                                                                       | Mitigation                                                                  |
|---|----------------------------------------------------------------------------|-----------------------------------------------------------------------------|
| R1 | Hot-path regression despite "cold-only" rule                              | Bench-gate PR 1 and PR 2; investigate any > 1% regression; revert if needed. |
| R2 | Init-order bug — consumer reads registry before all `init()` blocks run   | `Lookup` calls `ensureNumericRegistryInit` (once-guarded `sync.Once`); panics with `werr.ErrNumericRegistry` if any kind is missing. Same wiring pattern as `ensurePromotionInit` (`values/promotion.go:88`). |
| R3 | `BigComplex` exactness-per-instance edge case routes wrong                | Resolved by Q-g: spec field is `isAlwaysExact bool` (false for BigComplex). Per-instance lookups route through the existing `ExactnessOf(n)` function. Reinforced by `exactness_contagion_test.go`. |
| R7 | R7RS predicate regression on `(integer? 3.0)` if PR 3 migrates `n.IsInteger()` call sites to a kind-level spec field | Resolved by Q-f: `IsInteger`/`IsRational` are NOT spec fields. PR 3 leaves predicate call sites calling `n.IsInteger()` / `n.IsRational()` on the value methods. |
| R8 | External package mutates a spec via `Lookup` returning a pointer to package-global | Resolved by Q-h: spec fields are unexported. External callers can only use the getter methods. |
| R4 | ~~FFI behavior change~~ — **resolved by Q-i=C3.** No FFI change in this plan; the loss-signals plan handles precision-loss detection as a separate, opt-in enhancement.   | n/a |
| R5 | Cross-PR sequencing — PR 2 needs PR 1 merged; PR 3 needs PR 2 merged       | Open PRs sequentially; merge each before branching for the next. Branch from master post-merge each time. |
| R6 | Spec field drift between code and design doc                               | Single source of truth: the spec struct definition in `numeric_registry.go`. Design doc references the code at PR-merge time. |

## Bench-gate definition

For PR 1 and PR 2, the gate is:

- Run `make bench-gabriel` (16 canonical benches) on `master` first
  to refresh the baseline (mac M-series, performance mode, repeated
  3×).
- Run the same on the PR branch with the same conditions.
- Compute geomean ratio; **fail** the PR if geomean delta > 0.5%
  (regression) or any individual benchmark regresses > 1%.

The math bench suite (`bench-extended`) runs as a sanity check but
is not gating — its results are dominated by big-number arithmetic
which the registry doesn't touch.

## Done definition (whole Phase 3)

- [ ] PR 1 merged: registry exists, values/ cold paths migrated,
      no hot-path regression.
- [ ] PR 2 merged: cross-package cold paths migrated; FFI behavior
      delta documented and tested.
- [ ] PR 3 merged: math extension migrated; ADDING guide rewritten.
- [ ] Parent plan `memory/2026-05-13-values-structural-reduction.md`
      Finding 3 / Opportunity 3 marked complete; this plan moves
      to "Completed Plans" in `plans/CLAUDE.md`.

## Cross-references

- `memory/2026-05-14-numeric-registry-design.md` — design source.
- `memory/2026-05-13-values-structural-reduction.md` — parent plan
  (Tier A.1 of the SR roadmap).
- `plans/CLAUDE.md` — implementation completion workflow.
- `values/numeric_kind.go` — current ADDING guide that will be
  rewritten in PR 3.
- `values/promotion.go:398-665` — the centralized dispatch
  generators that stay untouched by this plan.

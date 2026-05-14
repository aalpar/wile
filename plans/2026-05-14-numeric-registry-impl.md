# Numeric registry — implementation plan (Phase 3, `values/` SR)

**Date**: 2026-05-14
**Status**: Plan locked in pending PR 1 start.
**Design source**: `plans/2026-05-14-numeric-registry-design.md`
  (Q-resolutions baked in: cold-path only; drop wile-goast scope;
  C1 universal ToFloat64; D1 leave Eqv as a switch; E2 three medium
  PRs.)
**Branch**: `feat/values-sr-phase3-numeric-registry-design` (this branch)
  will be reused for PR 1; subsequent PRs branch from master post-merge.

## Sequence overview

| PR | Scope                                                                                      | Bench gate | Est. delta   |
|----|--------------------------------------------------------------------------------------------|------------|--------------|
| 1  | New `values/numeric_registry.go`; 7 per-type `init()` registrations; migrate `Simplify`, `ExactnessOf`, `NumberToFloat64`, `NumberToComplex128` to read from registry | yes | +180 / −40   |
| 2  | Migrate `registry/helpers/value_conv.go` (`ToFloat64`, `ToComplex128`, `ExtractReal`) only. **FFI excluded** per Q-i=C3 — handled by the loss-signals follow-up plan. | yes (cold-path, light) | +15 / −55 |
| 3  | Migrate `extensions/math/prim_conversion.go` + `prim_complex.go`; rewrite the `numeric_kind.go` ADDING-A-NEW-NUMERIC-TYPE guide | no (cold path only) | +30 / −180   |

Cumulative net: ≈ **−60 LOC** + the 10-site update obligation
collapses to **1 site** (the spec field on the type file).

## PR 1 — Registry + values/ migration

### Goal

Introduce the `NumericTypeSpec` registry and migrate the two
`values/`-local cold-path consumers (`numeric_tower.go` and the
fallback helpers in `promotion.go`).

### Files added

- `values/numeric_registry.go` — registry type, accessor,
  validation hook (≈ 80 LOC).
- `values/numeric_registry_test.go` — completeness + per-kind spec
  invariants (≈ 70 LOC).

### Files modified

| File                          | Change                                                                                            |
|-------------------------------|---------------------------------------------------------------------------------------------------|
| `values/integer.go`           | Add `registerNumericSpec(…)` call in existing `init()`.                                           |
| `values/big_integer.go`       | Same.                                                                                             |
| `values/float.go`             | Same.                                                                                             |
| `values/big_float.go`         | Same. `ToFloat64` documented lossy.                                                               |
| `values/rational.go`          | Same.                                                                                             |
| `values/complex.go`           | Same. `ToFloat64` documented as "real part only".                                                 |
| `values/big_complex.go`       | Same. `ToFloat64` documented lossy + real-part-only.                                              |
| `values/numeric_tower.go`     | `Simplify` and `ExactnessOf` rewritten to consult registry. `BigComplex` exactness keeps its `IsExact()` check (per-instance). |
| `values/promotion.go`         | `NumberToFloat64` and `NumberToComplex128` rewritten to `Lookup(n.Kind()).ToFloat64(n)` / `.ToComplex128(n)`. |
| `values/numeric_kind.go`      | ADDING guide updated — but kept conservative; the full rewrite happens in PR 3. |

### Steps (tasks → atomic commits)

1. **Spec stub.** Create `numeric_registry.go` with type + accessor +
   `validateNumericRegistry` (panic-on-incomplete) + once-guarded
   `ensureNumericRegistryValid`. No consumers yet. Add a minimal
   test that asserts the file compiles standalone.
2. **Per-type registration.** Add `registerNumericSpec(…)` calls in
   each of the seven type files' existing `init()` blocks. Ensure
   `init()` ordering: spec registration runs *after* the dispatch
   tables, so `ensurePromotionInit()` and the registry are
   independently initialized.
3. **Cover-completeness test.** Add `TestNumericRegistryAllKindsRegistered`
   in `numeric_registry_test.go`: iterate `[0, numKinds)`, assert
   `numericRegistry[k].SchemeName != ""`. Run; expect green.
4. **Migrate `Simplify`.** Replace the 7-arm switch with the existing
   per-instance fast-paths plus a registry-driven generic descent
   (`spec.SimplifyDown`). Keep `BigComplex`'s zero-imaginary shortcut
   in place — it's a *cross-kind* simplification (BigComplex →
   Number) that doesn't fit the per-kind row shape. Verify all
   `numeric_tower_coverage_test.go` cases still pass.
5. **Migrate `ExactnessOf`.** Replace with `Lookup(n.Kind()).Exactness`,
   with a fall-through for `BigComplex` that calls `n.(*BigComplex).IsExact()`.
   Run `exactness_contagion_test.go`.
6. **Migrate `NumberToFloat64`.** Replace switch with
   `Lookup(n.Kind()).ToFloat64(n)`. The 7 spec entries each carry
   the same conversion they had inside the switch.
7. **Migrate `NumberToComplex128`.** Same shape as step 6.
8. **Run benches.** `make bench-gabriel` and the math benches.
   Compare against master baseline (refresh first to make sure
   we have a current reading). Require ≤ 0.5% geomean delta.
   If any benchmark regresses > 1%, investigate; do not paper
   over.
9. **Lint + ci.** `make lint && make covercheck && make ci` all
   pass before push.

### Acceptance for PR 1

- Registry has 7 spec entries, all populated, validated at startup.
- `Simplify`, `ExactnessOf`, `NumberToFloat64`, `NumberToComplex128`
  read from the registry; no per-kind switches remain in those
  functions.
- Hot-path arithmetic (Integer.Add, Float.Multiply, BigInteger.Compare,
  …) is byte-identical at the assembly level (no new function-pointer
  call introduced into the dispatch closures).
- `make bench-gabriel` geomean within ≤ 0.5% of master.
- All existing tests green; new completeness test green.

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
   reads: `ToFloat64` for the value (5-kind), `Exactness` (or
   `IsAlwaysExact` per Q-g) for the bool. BigComplex/Complex
   continue to raise `ErrNotAReal` (real-extraction is real-only
   by definition).
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
| `extensions/math/prim_conversion.go`          | `exact->inexact`, `number->string`, `inexact->exact` switches → registry consults. Some primitives use *type predicates* (`IsInteger`, `IsRational`); those read the spec's `IsInteger` / `IsRational` flags. |
| `extensions/math/prim_complex.go`             | `make-rectangular`, `make-polar`, `magnitude`, `angle`, real-part/imag-part — most cases collapse to registry lookups via the spec's `ToComplex128`. |
| `values/numeric_kind.go`                      | Rewrite the ADDING-A-NEW-NUMERIC-TYPE guide. The 12-item list collapses to: (1) add `KindXxx`; (2) new type file with dispatch tables + spec registration; (3) `promotionTable`/`promoter` rows in `promotion.go`; (4) `numeric_dispatch_test.go` roster. Items 6–12 disappear — they now read the spec automatically. |
| `BIBLIOGRAPHY.md` (optional)                  | Add a reference under "Numeric Type Registry" if the design notes are worth citing. Defer unless natural fit. |

### Steps

1. **`exact->inexact`** in `prim_conversion.go`. Identify which arm
   uses which spec field. Some arms create new values rather than
   reduce; those keep their type-specific code (e.g., `Rational →
   BigFloat` simplification path).
2. **`number->string`**. Currently delegates to `SchemeString()` on
   the value — likely no migration needed since `SchemeString` is
   per-type. Verify.
3. **`prim_complex.go`** sweep. Each numeric switch is either:
   (a) extracting a complex128 → use `spec.ToComplex128(n)`;
   (b) checking a type predicate → use `spec.IsInteger`,
   `spec.IsRational`, `spec.Exactness`;
   (c) constructing a kind-specific result → stays as-is.
4. **ADDING guide rewrite.** Replace the 12-item list with the
   collapsed 4-item version. Update `plans/2026-05-13-values-structural-reduction.md`
   Finding 3's "leakage sites" annotation to reference this PR as
   the resolution.
5. **Lint + ci.** No bench gate (extensions are cold paths).

### Acceptance for PR 3

- `extensions/math/` no longer holds numeric-kind switches in the
  migrated primitives. Construction-side switches (which build new
  values per kind) may remain — those are not duplication, they're
  per-kind code.
- `numeric_kind.go` guide is updated and points at the registry
  as the single source of truth.
- All math primitive tests pass.

## Risk register

| # | Risk                                                                       | Mitigation                                                                  |
|---|----------------------------------------------------------------------------|-----------------------------------------------------------------------------|
| R1 | Hot-path regression despite "cold-only" rule                              | Bench-gate PR 1 and PR 2; investigate any > 1% regression; revert if needed. |
| R2 | Init-order bug — consumer reads registry before all `init()` blocks run   | Lazy validation via `numericRegistryOnce`; consumers may call `ensureNumericRegistryValid` if needed. Same pattern as `ensurePromotionInit`. |
| R3 | `BigComplex` exactness-per-instance edge case routes wrong                | Registry's `Exactness` field for BigComplex holds `Inexact` (dominant case); `ExactnessOf` delegates to `n.(*BigComplex).IsExact()` for the per-instance answer. Documented in the design doc and reinforced by `exactness_contagion_test.go`. |
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
- [ ] Parent plan `plans/2026-05-13-values-structural-reduction.md`
      Finding 3 / Opportunity 3 marked complete; this plan moves
      to "Completed Plans" in `plans/CLAUDE.md`.

## Cross-references

- `plans/2026-05-14-numeric-registry-design.md` — design source.
- `plans/2026-05-13-values-structural-reduction.md` — parent plan
  (Tier A.1 of the SR roadmap).
- `plans/CLAUDE.md` — implementation completion workflow.
- `values/numeric_kind.go` — current ADDING guide that will be
  rewritten in PR 3.
- `values/promotion.go:398-665` — the centralized dispatch
  generators that stay untouched by this plan.

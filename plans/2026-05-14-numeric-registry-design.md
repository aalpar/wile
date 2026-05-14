# Numeric registry — Phase 3 of `values/` structural reduction

**Date**: 2026-05-14
**Status**: Design only — no implementation.
**Source plan**: `plans/2026-05-13-values-structural-reduction.md`
  (Opportunity 3 / Finding 3 — the 12-item ADDING-A-NEW-NUMERIC-TYPE
  guide collapses to one `NumericTypeSpec` record per kind.)
**Phase**: 3 of the values-SR sequence. Phases 0–2 closed via PRs
  #747, #748, #749. Phase 4 (`Datum()` cleanup) and Opportunity 1
  (`IsVoid` convention test) remain independent of this design.
**Priority**: High (Tier A.1 of the structural-reduction roadmap).

## Why this design pass exists

The parent plan recommends a design pass before any migration:

> **Phase 3 — Numeric registry (Opportunity 3)**: Design pass first —
> `plans/2026-05-??-numeric-registry-design.md`. Identify which
> hot-path dispatch lookups must stay direct vs. which can move to
> registry indirection.

This document is that pass. It does three things:

1. **Verifies** the parent plan's "12-site leakage" claim against the
   current code. Two of the twelve items turn out to be stale or
   wrong (item 9 wile-goast, item 10 `ffi.go:300`).
2. **Separates** the hot-path dispatch (already centralized through
   `promotion.go`'s generic generators) from the cold-path
   duplication (cross-package switches that re-encode the same
   per-kind facts). Recommends touching only the cold paths.
3. **Specifies** the `NumericTypeSpec` record shape, lazy-init
   pattern, and per-consumer migration order. Raises five open
   design questions for user resolution (Q-a … Q-e) before any
   implementation begins.

## Findings: verified leakage sites

The parent plan listed twelve sites. Walking each one against the
current code as of `master` (b3aaa693):

| # | Plan's site                                | Real location & shape                                  | Hot or cold | Verified? |
|---|--------------------------------------------|--------------------------------------------------------|-------------|-----------|
| 1 | `values/numeric_kind.go`                   | `values/numeric_kind.go:30-39` — 7 `Kind*` constants   | n/a         | ✅ exact   |
| 2 | `values/xxx.go` (per type)                 | `values/{integer,big_integer,float,big_float,rational,complex,big_complex}.go` — each holds 6 `[numKinds]func` dispatch tables + `Kind()` + `init()` populating them via the `makeXxxDispatch` generators | **hot**     | ✅ exact   |
| 3 | `values/promotion.go`                      | `values/promotion.go` — `promotionTable [7][7]Kind`, `promoter [7][7]func(Number) Number`, `NumberToFloat64` (lines 327-346), `NumberToComplex128` (lines 352-369) | mixed       | ✅ exact   |
| 4 | `values/numeric_tower.go`                  | `values/numeric_tower.go` — `Simplify` (94-124), `ExactnessOf` (145-158)                       | **cold**    | ✅ exact   |
| 5 | `values/numeric_dispatch_test.go`          | Roster-completeness test                                | n/a (test) | ✅ exact   |
| 6 | `registry/helpers/value_conv.go`           | `ToComplex128` (28-52), `ToFloat64` (72-90), `ExtractReal` (96-114) — three duplicate 7-case switches | **cold**    | ✅ exact   |
| 7 | `extensions/math/prim_conversion.go`       | `exact->inexact`, `number->string`, etc. — ~10 numeric switch sites across lines 54-298         | **cold**    | ✅ exact   |
| 8 | `extensions/math/prim_complex.go`          | `make-rectangular`, `make-polar`, `magnitude`, `angle`, real/imag-part — ~26 numeric switch sites (lines 66-265) | **cold**    | ✅ exact   |
| 9 | `wile-goast/goast/mapper.go` — `numberToAST` | **does not exist.** `goast/mapper.go` maps Go AST → Scheme; the only `values.New*` call across `wile-goast/` constructs `values.NewInteger` for AST-node IDs. There is no Big*, Rational, Complex, or Float construction anywhere outside test fixtures and historical plan files. | **n/a** | ❌ stale claim — no external repo migration required |
| 10 | `ffi.go` — `schemeToReflectValue (~line 300)` | **wrong file & wrong function name.** Current location is `ffi_arg_converters.go:41-96` (float64 and int64 cases handle `*Integer`, `*BigInteger`, `*Float`, `*Rational` — not BigFloat/BigComplex by design, since `reflect.Float64` cannot carry arbitrary precision). `ffi.go` is 268 lines and contains no `schemeToReflectValue` function. | **cold (registration-time + per-call closure)** | ⚠️ exists but in different file |
| 11 | `internal/parser/parser_number.go`         | Numeric literal parsers (`parseIntegerWithBase`, `parseRationalWithBase`, `parseBigIntegerWithBase`, `parseFloat`) | **cold** (parse-time) | ✅ exact |
| 12 | `registry/helpers/equality.go`             | `Eqv` (28-71) — 7-case switch with Integer↔BigInteger cross-comparison | **cold**    | ✅ exact   |

**Net verified leakage:** 10 real sites (the seven types' dispatch
files + 7 cold-path duplicators in `values/`, `registry/helpers/`,
`extensions/math/`, `ffi_arg_converters.go`, `internal/parser/`).
Items 9 and 10 (the external repo and the `ffi.go` site) are stale
in the parent plan and should be dropped from the migration scope.

## Hot path vs cold path

The single most important design constraint:

> **The hot path is already centralized through
> `makeArithmeticDispatch` and friends in `promotion.go:398-665`.
> The cold path is where the leakage lives.**

### Hot path (must not change)

Per-type arithmetic dispatch tables are populated at init time:

```go
// values/integer.go:165-180 (representative)
var integerAdd [numKinds]func(*Integer, Number) Number

func init() {
    integerAdd = makeAddDispatch(KindInteger, func(p *Integer, o Number) Number {
        return addInt64(p.Value, o.(*Integer).Value)
    })
    // ... 5 more similar dispatches
}

func (p *Integer) Add(o Number) Number {
    if same, ok := o.(*Integer); ok {
        return addInt64(p.Value, same.Value)  // same-type fast path
    }
    return integerAdd[o.Kind()](p, o)         // cross-type via captured closure
}
```

The closures inside `integerAdd[i]` already capture the promoters
`promSrc` and `promDst` from `promotionTable[i]` and `promoter[i][*]`
at construction time (see `makeArithmeticDispatch` lines 405-461).
There is no per-call registry lookup, no per-call switch — just one
`[numKinds]func` table index and an indirect call.

Forcing arithmetic to consult a `NumericTypeSpec` registry on every
call would mean: (a) replace the captured closure with a registry
lookup, (b) prevent escape-analysis from inlining the closure, (c)
add a bounds-check + map-access per arithmetic op. The numeric
benchmarks (Gabriel, fib, sum, math-extended) would almost certainly
regress.

**Design rule:** the new `NumericTypeSpec` registry is consulted in
*cold* code paths only. Hot-path dispatch tables stay direct.

### Cold path (where the registry helps)

The seven *duplicated* 7-case switches across packages:

| Switch                                          | Where (file:line)                                   |
|-------------------------------------------------|------------------------------------------------------|
| `Simplify` (which kind to demote toward)       | `values/numeric_tower.go:94-124`                     |
| `ExactnessOf`                                   | `values/numeric_tower.go:145-158`                    |
| `NumberToFloat64` (cold-path fallback)         | `values/promotion.go:327-346`                        |
| `NumberToComplex128` (cold-path fallback)      | `values/promotion.go:352-369`                        |
| `ToFloat64` (FFI-adjacent helper)              | `registry/helpers/value_conv.go:72-90`               |
| `ToComplex128`                                  | `registry/helpers/value_conv.go:28-52`               |
| `ExtractReal` (extractor + exactness)          | `registry/helpers/value_conv.go:96-114`              |
| `Eqv` (R7RS eqv? — Int↔BigInt cross-compare)   | `registry/helpers/equality.go:28-71`                 |
| `exact->inexact`, `number->string`, etc.        | `extensions/math/prim_conversion.go` (multiple)      |
| Numeric type-name reporting in errors           | scattered `fmt.Sprintf("%T", v)` sites               |

Each of these encodes the same closed-set enumeration of the seven
numeric kinds. Each must be updated in lockstep when a new numeric
kind is added; today nothing fails loudly if one is missed.

## Proposed design: `NumericTypeSpec` registry

### Shape

```go
// values/numeric_registry.go (new file)

// NumericTypeSpec is the single per-kind record describing the
// cross-package facts about a numeric type. Hot-path arithmetic
// dispatch does NOT consult this struct — it is read by cold paths
// only (type names, exactness, conversion helpers, R7RS predicates,
// FFI adapters, parser/printer hooks).
//
// Convention: every NumericKind has exactly one populated spec,
// validated at startup by validateNumericRegistry.
type NumericTypeSpec struct {
    Kind         NumericKind                   // self-reference for sanity checks
    SchemeName   string                        // R7RS-facing name (drives type errors)
    Exactness    Exactness                     // dominant exactness; BigComplex is per-instance — see comment below
    IsInteger    bool                          // satisfies R7RS integer? in the general case
    IsRational   bool                          // satisfies R7RS rational?
    SimplifyDown func(Number) Number           // one-step demotion (nil → no demotion path)
    ToFloat64    func(Number) float64          // lossy-allowed reduction; nil → not reducible (BigComplex)
    ToComplex128 func(Number) complex128       // always populated (every kind reduces to complex128)
}

var numericRegistry [numKinds]NumericTypeSpec

// Per-type init() functions register their spec via this helper.
func registerNumericSpec(spec NumericTypeSpec) {
    if spec.Kind >= numKinds {
        panic("numeric registry: out-of-range kind")
    }
    if numericRegistry[spec.Kind].Kind != 0 || spec.Kind == KindInteger && numericRegistry[spec.Kind].SchemeName != "" {
        panic("numeric registry: duplicate registration for kind")
    }
    numericRegistry[spec.Kind] = spec
}

// Lookup is the canonical accessor — panics if the kind is
// unregistered (registry completeness is a build-time invariant).
func Lookup(k NumericKind) *NumericTypeSpec {
    if k >= numKinds {
        panic("numeric registry: kind out of range")
    }
    return &numericRegistry[k]
}
```

**Exactness caveat:** `BigComplex` has per-instance exactness (its
parts may be exact `Rational`/`BigInteger` or inexact `BigFloat`).
The `Exactness` field is the *static* exactness; consumers that need
the per-instance answer must call `ExactnessOf(n)` which already
does the right thing for `BigComplex`. The registry's `Exactness`
holds `Inexact` for `BigComplex` (matching the dominant case);
`ExactnessOf` delegates to the per-instance check when the kind is
`BigComplex`. This is documented at the `Exactness` field.

### What deliberately stays out of the spec

Per the hot-path-vs-cold-path discipline above:

- **`PromotionResultKind`** stays in `promotion.go` — it is consulted
  inside dispatch closures at table-construction time.
- **`promoter[src][dst]`** stays in `promotion.go` — captured by
  dispatch closures.
- **Per-type same-type fast paths** (`addInt64`, `subInt64`, …) stay
  in the per-type file; the spec doesn't try to virtualize them.
- **R7RS literal parsing** (`internal/parser/parser_number.go`) stays
  in the parser — that file's job is recognizing source syntax, not
  reading registry data. Adding a new numeric type means adding a
  parser rule regardless of what the registry says.

### Init pattern

Each numeric type file gains an `init()` that registers its spec:

```go
// values/integer.go (excerpt)
func init() {
    // existing dispatch-table init...
    registerNumericSpec(NumericTypeSpec{
        Kind:         KindInteger,
        SchemeName:   "integer",
        Exactness:    Exact,
        IsInteger:    true,
        IsRational:   true,
        SimplifyDown: nil,  // Integer is the bottom of the exact-integer chain
        ToFloat64:    func(n Number) float64 { return float64(n.(*Integer).Value) },
        ToComplex128: func(n Number) complex128 {
            return complex(float64(n.(*Integer).Value), 0)
        },
    })
}
```

Validation runs once at first registry use:

```go
var numericRegistryOnce sync.Once

func ensureNumericRegistryValid() {
    numericRegistryOnce.Do(func() {
        for k := NumericKind(0); k < numKinds; k++ {
            if numericRegistry[k].SchemeName == "" {
                panic(werr.WrapForeignErrorf(
                    werr.ErrNumericRegistry,
                    "numeric registry: kind %d not registered", k))
            }
        }
    })
}
```

### Migration order (consumer-side)

Once the registry exists with no consumers, each cold-path site
migrates independently. Migration order minimizes churn and
maximizes early signal:

1. **`values/numeric_tower.go`**: rewrite `Simplify` and
   `ExactnessOf` to consult the registry. **No external API change.**
   Local-only first PR.
2. **`values/promotion.go`**: rewrite `NumberToFloat64` /
   `NumberToComplex128` to use `Lookup(n.Kind()).ToFloat64(n)` /
   `.ToComplex128(n)`. Still local to `values/`.
3. **`registry/helpers/value_conv.go`**: `ToFloat64`,
   `ToComplex128`, `ExtractReal` migrate. First cross-package PR.
4. **`registry/helpers/equality.go`**: `Eqv` — special since the
   Int↔BigInt cross-comparison is inherently a two-arg switch.
   Q-d (below) asks how to handle this.
5. **`extensions/math/prim_conversion.go` + `prim_complex.go`**:
   migrate the bulk of the duplicate switches. Largest LOC delta.
6. **`ffi_arg_converters.go`**: float64 path migrates to
   `Lookup(n.Kind()).ToFloat64`. The int64 path stays — it's
   exact-integer-only and doesn't fit the registry's float64 lens.

The parent plan estimated 3–4 PRs over 2 weeks. Six migration steps
fit comfortably in that envelope — most are small (under ~80 LOC
delta per consumer file).

## Trade-offs

**Wins:**
- Adding an 8th numeric type drops from 10 cold-path edits (plus
  parser + per-type-file mandatory work) to 1 spec registration
  inside the type file. The 12-item guide collapses accordingly.
- `validateNumericRegistry` is a startup assertion: forget the spec,
  the program panics before serving traffic, not silently months
  later.
- The duplicate switches stop drifting. Today nothing forces
  `ToFloat64` and `NumberToFloat64` to agree on the Rational
  rounding policy; with one source of truth they cannot diverge.

**Costs:**
- One indirection (`Lookup(n.Kind()).ToFloat64(n)`) per cold-path
  call. Cold paths by definition tolerate this.
- One new file (`values/numeric_registry.go`, ~80 LOC) plus seven
  init blocks (~15 LOC each across the seven type files). Net
  values/-side addition: ~185 LOC. The savings come on the consumer
  side: ~250 LOC across `registry/helpers/`, `extensions/math/`,
  `ffi_arg_converters.go`.
- Exactness has an instance-variant case (`BigComplex`) the registry
  cannot capture statically. Documented above; consumers needing
  the variant call `ExactnessOf` which routes through the
  per-instance check.

**Non-cost:**
- No external-repo coupling. The audit confirmed `wile-goast` does
  not consume numeric type information beyond `*values.Integer`; no
  PR there is needed.

## Open questions for user resolution

These shape the implementation. Each is real — there's a defensible
choice in either direction and the user's domain knowledge should
pick.

**Q-a — Hot-path scope.** The current design keeps `promotionTable`
and `promoter` in `promotion.go` (outside the registry). An
alternative would absorb them into the registry as `PromoteTo(kind)`
on the spec, unifying *all* per-kind data. The cost is a per-promote
function-pointer call instead of an inline `promoter[src][dst]`
array lookup. Promote runs inside dispatch closures at
table-construction time (not per-arithmetic-call), so the impact is
init-only — but it's the kind of change that needs a bench-gate.
**Recommendation: keep `promotionTable`/`promoter` out of the
registry** (the current design). Promote-related data is structurally
two-dimensional (`[src][dst]`) and doesn't fit the per-kind row
shape. Confirm or override?

**Q-b — Drop the wile-goast scope.** The parent plan listed
`wile-goast/goast/mapper.go — numberToAST` as a leakage site. The
audit shows no such function exists and `wile-goast` only ever
constructs `values.NewInteger`. **Recommendation: drop wile-goast
from the migration scope entirely.** Confirm?

**Q-c — FFI float64/int64 path.** `ffi_arg_converters.go:76-96`
handles `*Integer`, `*BigInteger`, `*Float`, `*Rational` for
float64-target conversions — but NOT `*BigFloat` or `*BigComplex`.
This is intentional: `reflect.Float64` can't carry arbitrary
precision. Options:
  - **C1**: registry's `ToFloat64` returns float64 for *all* seven
    kinds (including BigFloat/BigComplex with documented precision
    loss); FFI consults it; behavior matches current.
  - **C2**: registry's `ToFloat64` returns `(float64, bool)` where
    bool is "lossless"; FFI rejects lossy conversions with a clearer
    error than today.
  - **C3** (current code): registry's `ToFloat64` covers only the
    five reducible kinds; FFI keeps its 5-case switch.
  **Recommendation: C1 (match existing behavior; smallest delta).**
  Override if you'd prefer the more pedantic C2.

**Q-d — `Eqv` and cross-type integer comparison.** R7RS `eqv?`
returns `#t` for an `*Integer` and a `*BigInteger` with the same
numeric value (both being exact integers). This is an inherently
*two-argument* dispatch — neither operand's spec alone tells you
the comparison shape. Options:
  - **D1**: keep `Eqv` as a switch; don't migrate it. Lose ~5% of
    the duplicate-switch reduction but avoid an awkward
    cross-kind protocol.
  - **D2**: add `IsInteger bool` and a generic `compareAsExact` path
    on the registry; `Eqv` consults `IsInteger` on both operands.
    Cleaner if more numeric kinds satisfy `integer?` in future
    (none planned).
  **Recommendation: D1.** The cross-type two-arg dispatch resists
  the row-shape registry; switch is fine for one rarely-changing
  site.

**Q-e — Implementation phasing.** The migration order above defines
six PRs. The parent plan budgeted "3–4 PRs over 2 weeks." Options:
  - **E1**: Six small PRs (one per consumer site). Slowest but each
    is trivial to review.
  - **E2**: Three medium PRs — (a) registry + values/ migration,
    (b) registry/helpers + ffi, (c) extensions/math. Matches the
    parent plan's estimate.
  - **E3**: Two large PRs — (a) registry + all values/ + helpers,
    (b) extensions/math + ffi. Fastest but largest review surface.
  **Recommendation: E2 (matches parent estimate; balances review
  cost and bench-gating frequency).**

## Phasing (post Q-resolution)

Assuming default recommendations (E2):

| PR | Scope                                                          | Bench gate? | Est. delta |
|----|----------------------------------------------------------------|-------------|------------|
| 1  | New `values/numeric_registry.go` + 7 init blocks + `Simplify` and `ExactnessOf` migration in `values/numeric_tower.go` + cold-path `NumberToFloat64`/`NumberToComplex128` in `promotion.go` | yes — verify hot-path unchanged | +180/−40 LOC |
| 2  | `registry/helpers/{value_conv.go,equality.go}` migration (Eqv per Q-d) + `ffi_arg_converters.go` float64 path | yes — FFI is borderline | +20/−70 LOC |
| 3  | `extensions/math/{prim_conversion.go,prim_complex.go}` migration; ADDING-A-NEW-NUMERIC-TYPE guide rewritten to read "register a NumericTypeSpec; done." | no (cold path only) | +30/−180 LOC |

Cumulative: ~+230/−290 LOC net = **~60 LOC reduction** in the values
subsystem, plus the elimination of the 10-site update obligation.

## Risks

| # | Risk                                                              | Mitigation                                                  |
|---|-------------------------------------------------------------------|-------------------------------------------------------------|
| R1 | Hot path regresses despite "registry is cold-only" rule          | Bench-gate every PR with `make bench-gabriel` + math suite; require ≤ 0.5% geomean delta |
| R2 | Init-order bug: a consumer reads the registry before all spec init() blocks have run | Lazy-init via `numericRegistryOnce`; consumers call `ensureNumericRegistryValid` (analogous to `ensurePromotionInit`) |
| R3 | `BigComplex` exactness-per-instance edge case routes wrong       | Document explicitly; consumers route through `ExactnessOf` not raw `Lookup(k).Exactness` for that kind |
| R4 | `Eqv` migration regresses R7RS exactness semantics               | Keep `Eqv` as a switch per Q-d unless user overrides |
| R5 | Stale plan claims (item 9, item 10) cause a future contributor to add EXTERNAL repo migration work that isn't needed | This design doc supersedes those claims; the ADDING-A-NEW-NUMERIC-TYPE guide will be rewritten to reflect verified reality |

## Open questions — resolved (2026-05-14)

User accepted the recommended default for every question.

| Q  | Resolution                                                | Implication                                                |
|----|-----------------------------------------------------------|------------------------------------------------------------|
| Qa | Cold-path only                                            | `promotionTable`/`promoter` stay in `promotion.go`. Registry has no `PromoteTo`. |
| Qb | (implicit) Drop wile-goast scope                          | No cross-repo PR. ADDING guide loses external-repo item.   |
| Qc | C1 — universal `ToFloat64` with documented precision loss | Every kind's spec has a non-nil `ToFloat64`; BigFloat/BigComplex documented as lossy at the field. |
| Qd | D1 — leave `Eqv` as a switch                              | Migration scope omits `registry/helpers/equality.go`. PR 2 covers `value_conv.go` + FFI only. |
| Qe | E2 — three medium PRs                                     | Phasing locked in (see Phasing table above). |

All Q-resolutions are now baked into the design above and the
implementation plan at `2026-05-14-numeric-registry-impl.md`.

## Done definition for the design pass

- [x] All twelve leakage sites in the parent plan are verified or
      retracted against `master`.
- [x] The hot-path-vs-cold-path discipline is documented with file:
      line citations.
- [x] The `NumericTypeSpec` shape is specified with field
      semantics.
- [x] Five open questions (Q-a … Q-e) are surfaced with
      recommendations.
- [x] User resolves Q-a … Q-e.
- [x] An implementation plan (`2026-05-14-numeric-registry-impl.md`)
      is drafted from this design after Q-resolution.

## Cross-references

- `plans/2026-05-13-values-structural-reduction.md` — Phase 3
  parent (Opportunity 3 / Finding 3).
- `plans/CLAUDE.md` — implementation completion workflow; this
  design phase precedes the impl plan.
- `values/numeric_kind.go:8-27` — the current ADDING guide that
  will be rewritten after migration.
- `values/promotion.go:398-665` — the centralized dispatch
  generators that the registry deliberately does NOT touch.
- `BIBLIOGRAPHY.md` — "Numeric Promotion Lattice", "Exactness as
  Abstract Interpretation".

# Cross-package finding: dispatch axes hand-unrolled instead of data

**Date**: 2026-05-08
**Type**: Synthesis of structural-reduction findings
**Source**: Analyses of `values/` (2026-05-08) and `registry/` (2026-05-08)
**Status**: Findings consolidated — implementation deferred to per-package plans
**Related**: `plans/2026-05-07-structural-reduction-roadmap.md` (gates these)

## Why this document exists

Two `/structural-reduction` analyses surfaced what at first looked like
unrelated findings. On closer reading, three of them — `values/` Finding 1
(numeric dispatch tables), `registry/` Finding 1 (two `Phase` types),
`registry/` Finding 2 (four phase loops in `Apply`) — are instances of the
same structural defect at different scales.

Naming the pattern lets us:

1. Plan implementation in the right order (the three instances may share
   underlying assumptions that should be unified once, not three times).
2. Predict where the pattern recurs — most likely in `environment/`
   (Tier A.2, not yet analyzed) and possibly `internal/`.
3. Recognize the shape on first sight in future analyses, instead of
   re-deriving the diagnosis each time.

## The pattern

> **A multi-axis dispatch problem encoded by replicating structure along one
> axis instead of treating that axis as data.**

In category-theoretic terms (Bird & de Moor 1997, *Algebra of Programming*;
Milewski, *Category Theory for Programmers*), a parametric family
`{F(c) : c ∈ C}` of operations indexed by some axis `C` can be encoded two
ways:

| Form | What it looks like | When it's wrong |
|---|---|---|
| **Materialized once** (data) | `dispatch[c]` — table or map indexed by `c` | rarely, when `c` is genuinely fixed at compile time and N is small (≤3) |
| **Materialized N times** (code) | one named entity per `c`, e.g. `<C₁>Op`, `<C₂>Op`, ... | when `C` has 4+ inhabitants, when adding to `C` requires multi-site edits, or when consistency across `c` is invariant |

The "materialized N times" form has three predictable costs:

1. **Multiplicative coupling on changes**: adding to the axis (one new
   numeric type, one new phase) requires N edits across the codebase.
   Compile-time guarantees of completeness are weak — Go's compiler does
   not enforce that all axis-points have a dispatch entry; the failure
   mode is a runtime panic or a silently missing case.
2. **Drift between the N copies**: 7 functions that *should* implement the
   same operation can silently diverge. Fixes applied to one are easily
   forgotten in the others. Detection requires careful manual review or a
   coverage test (and even then: only that all entries exist, not that they
   agree).
3. **The axis becomes invisible**: the parameterizing dimension is encoded
   in identifier names, file positions, and comments — everywhere except
   the type system. New code can't iterate over the axis, transform it, or
   reason about it programmatically.

The fix is mechanical once the pattern is named: **make the axis a value,
not a code position.**

## Three instances

### Instance A — `values/`: numeric dispatch tables (41 vars, 7 files)

**Where**: `values/integer.go:165-207`, `big_integer.go:124-129`,
`float.go:76-81`, `big_float.go:130-135`, `rational.go:105-110`,
`complex.go:69-74`, `big_complex.go:152-156` (5 vars there — no
`bigComplexLessThan`).

**Axes**: `Op = {Add, Subtract, Multiply, Divide, LessThan, Compare}` (6),
`SrcKind = NumericKind` (7), `DstKind = NumericKind` (7).
Total cube: 6 × 7 × 7 = 294 entries.

**Hand-unrolled form**: For each (Op, SrcKind), one
`var <type><Op> [numKinds]func(*<Type>, Number) <result>` package var,
populated at `init()` time by a parallel `make<Op>Dispatch` family. So 41
vars (with the BigComplex/LessThan asymmetry) × ~7 closures each = 287
entries materialized as code. Methods on each numeric type do
`<type><Op>[o.Kind()](p, o)`.

**Materialized-once form**: A single per-Op 2D table or a single 3D table:

```go
var addDispatch [numKinds][numKinds]func(Number, Number) Number
// ... 5 more, populated centrally in promotion.go ...

func (p *Integer) Add(o Number) Number {
    return addDispatch[KindInteger][o.Kind()](p, o)
}
```

The receiver type `*Integer` becomes an implementation detail of the
closure, not a fact encoded in N parallel symbol names.

**Symptoms**:
- 12-point "ADDING A NEW NUMERIC TYPE" guide in `numeric_kind.go:8-25`
  (one item references an EXTERNAL repo, `wile-goast`).
- `LessThan` is fully derivable from `Compare` but each type implements both,
  with parallel dispatch tables (~7 tables of pure redundancy).

### Instance B — `registry/`: two `Phase` types with conflicting values

**Where**: `registry/phase.go:20-29` vs `environment/phase_registry.go:26-31`.

**Axis**: phase (compile / expand / runtime / template).

**Hand-unrolled form**: Two parallel encodings of the same axis, in two
packages:

| Constant | `registry.Phase` | `environment` (raw int) |
|---|---|---|
| `PhaseRuntime` | `1` (bit 0) | `0` |
| `PhaseExpand` | `2` (bit 1) | `1` |
| `PhaseCompile` | `4` (bit 2) | `2` |
| `PhaseTemplate` | absent | `-1` |

`registry/`'s form is a bit-flag for set semantics (composition via `|`).
`environment/`'s form is a sequential index for map keys
(`map[int]*EnvironmentFrame`). The public API at `wile/options.go:36-37`
re-exports the `environment` constants — so embedders writing extension
code see one set of values, while the registry uses another.

**Materialized-once form**: One `Phase` type, one set of values, one set
of operations. Either a typed enum used as both index and bit-flag carrier,
or a `Phase` enum + `PhaseSet` bitset distinguished by name. The map
becomes `map[Phase]*EnvironmentFrame`; bit-set composition operates over
the same `Phase` values via a thin `PhaseSet` newtype.

**Symptoms**:
- `PhaseTemplate` is missing from the registry — extensions cannot
  register template-phase bindings, an asymmetry no comment explains.
- Conversion between the two encodings is hand-coded at the call boundary
  (e.g., `apply.go` checks `Phases.HasExpand()` then calls `env.Expand()`
  which uses the integer `PhaseExpand = 1`).
- `wile.PhaseRuntime = 0` (env value) vs `registry.PhaseRuntime = 1` —
  identical name, different value, both compile, both reachable from
  embedder code.

### Instance C — `registry/`: four phase loops in `Apply`, two near-duplicate registrars

**Where**: `registry/apply.go:58-94` (4 phase loops),
`apply.go:123-179` (`registerRuntimePrimitive` and
`registerExpandTimePrimitive`).

**Axis**: phase (the same axis as Instance B, just at a different
encoding level).

**Hand-unrolled form**: Three loops differing only in the phase predicate
and the registration function, plus two helpers differing only in
`env` vs `env.Expand()` and the error message string:

```go
for _, reg := range p.primitives {
    if reg.Phases.HasRuntime() { ... registerRuntimePrimitive ... }
}
for _, reg := range p.primitives {
    if reg.Phases.HasExpand() { ... registerExpandTimePrimitive ... }
}
// ... etc ...
```

**Materialized-once form**:

```go
type phaseAction struct {
    phase  Phase
    target func(*environment.EnvironmentFrame) *environment.EnvironmentFrame
}
var applyOrder = []phaseAction{
    {PhaseCompile, (*environment.EnvironmentFrame).Compile},
    {PhaseRuntime, identity},
    {PhaseExpand,  (*environment.EnvironmentFrame).Expand},
}

for _, action := range applyOrder {
    for _, reg := range p.primitives {
        if !reg.Phases.Has(action.phase) { continue }
        // single shared registration body, parameterized by action.target
    }
}
```

**Symptoms**: The "Apply Order" comment in `registry/CLAUDE.md` documents
the order; that order is currently encoded in code position rather than as
data. Adding a phase requires duplicating both the loop and the helper.

## Why these three are the same problem

All three share these structural traits:

1. **Axis with small but nontrivial cardinality** (3 or 6 inhabitants in
   our cases — small enough to feel manageable, large enough that
   replication compounds).
2. **Replication along that axis is the encoding choice** — not a
   performance optimization, not a unique-per-axis-point semantic
   requirement, just an artifact of how the code grew.
3. **Adding to the axis costs N edits** at multi-site coordinates.
4. **Compile-time enforcement of completeness is absent** — coverage is
   verified by runtime assertions, comments, or vigilance.

When all four traits hold, the materialized-once form pays for itself.
When any is absent (e.g., the axis is genuinely small and stable, or
each axis-point has irreducibly distinct semantics), replication is
acceptable. **None of the three instances above has any trait absent.**

## Implementation order

These do not have to ship together, but they have a partial dependency:

```
            ┌────────────────────────────────────────┐
            │ Instance B: unify Phase types          │
            │ (registry.Phase ↔ environment.Phase)   │
            └─────────────────┬──────────────────────┘
                              │ once Phase is one type with one set of
                              │ values, the apply-loops collapse cleanly
                              ▼
            ┌────────────────────────────────────────┐
            │ Instance C: phase-keyed dispatch       │
            │ table for `Apply`                      │
            └────────────────────────────────────────┘

            Independent ──────────────────────────────
            ┌────────────────────────────────────────┐
            │ Instance A: numeric dispatch registry  │
            │ (no shared types with B/C)             │
            └────────────────────────────────────────┘
```

**Recommended order**:

1. **Instance B first** (`Phase` unification). Highest blast radius
   (touches embedder API), but mechanically smallest. Should land on its
   own PR.
2. **Instance C second**, on top of B. Becomes a much smaller change once
   `Phase` is one type — the apply-loops collapse into a literal slice of
   actions.
3. **Instance A independently**. Larger refactor, no shared types with
   B/C. Can run in parallel with B/C, or after, depending on reviewer
   bandwidth.

The Tier-5 TODO entries `plans/2026-05-06-machine-structural-reduction.md`
and `plans/2026-05-07-internal-structural-reduction.md` should consume
the Phase-unified API once Instance B lands. Neither plan currently
depends on Instance A or Instance C; the gating roadmap
(`plans/2026-05-07-structural-reduction-roadmap.md`) sequences `values/`
(this Instance A) before `internal/` Phase 7 because the
`SyntaxPair`/`SyntaxEmptyList` migration cites `values/` precedent — that
precedent is closed (per analysis).

## Predicted further instances

The pattern is general enough that more instances likely exist. Top
candidates to check during the next analysis pass:

- **`environment/`** (Tier A.2, not yet analyzed): the
  `PhaseRegistry.envs map[int]*EnvironmentFrame` is already in the
  materialized-once form, but **construction** of phase environments
  (`createPhaseEnv`, the four binding-type helpers) likely has parallel
  patterns. The `Binding`/`BindingMeta`/`BindingType` triple is also a
  candidate (axis = binding kind).
- **`internal/validate/`**: validators per special form. If each form has
  its own `validate<Form>` function with structural overlap, the same
  pattern applies (axis = form name). Already noted in
  `plans/2026-05-07-internal-structural-reduction.md` Findings 2–5.
- **`registry/core/`** primitive groups: if registration helpers are
  duplicated per category (arithmetic vs lists vs strings), the axis is
  category. Lower-priority because `registry/core/` is targeted with the
  `scheme-conformance` lens, not structural-reduction.

## Done criteria for each instance

For each instance, "done" means:

- [ ] The axis is named in code with one type definition. No second
      package declares constants at conflicting values for the same
      conceptual position.
- [ ] Coverage of the axis is enforced by the type system, by a
      compile-time generator, or by a startup assertion that walks the
      axis exhaustively (no runtime "did I forget that case?").
- [ ] Adding an axis-point requires changes at one site (or one site per
      axis-axis intersection, e.g. one new row + one new column in a 2D
      table — but never N parallel files).
- [ ] The `ADDING A NEW X` guide comment for that axis (if any) drops
      its item count by at least one.

A future analysis pass should be able to point to a single registration
table and say "this is the axis"; if it has to point to a comment or a
collection of identifiers, the refactor is incomplete.

## Cross-references

- `plans/2026-05-07-structural-reduction-roadmap.md` — the planning-only
  document that gated these analyses.
- `plans/2026-05-06-machine-structural-reduction.md` — `machine/` plan;
  awaits Phase unification before its Phase 7 (sub-record extraction)
  can be sequenced confidently.
- `plans/2026-05-07-internal-structural-reduction.md` — `internal/` plan;
  Findings 2–5 are smaller-scale instances of the same pattern (helper
  extraction across hand-unrolled validators).
- `TODO.md` Tier 5 — the existing dispatch-tables and Phase items live
  here; they should reference back to this synthesis.

## Footnote on theory

The phrase "axis as data" is the operational form of two intersecting
ideas:

- **Defunctionalization** (Reynolds 1972, "Definitional Interpreters for
  Higher-Order Programming Languages"): replace a family of higher-order
  functions with a single function that consumes a tag identifying which
  member of the family to apply. The tag is the axis.
- **Functor materialization** (Milewski, *Category Theory for
  Programmers*; Bird & de Moor 1997, *Algebra of Programming*): a
  parametric family `{F(c)}` is the action of a functor on objects; if
  the family laws hold (uniformity across `c`), one materialization
  suffices.

Both name the same advice: **encode the parameter, don't replicate the
structure.** This document operationalizes that advice for three
specific findings in this codebase.

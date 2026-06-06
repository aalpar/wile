# `registry/` package structural reduction

**Date**: 2026-05-18 (analysis); revised 2026-05-19 (Phase 0 closeout +
tech-lead verification pass)
**Source**: `/structural-reduction ./registry` analysis (Tier A.3 of the
roadmap — `plans/2026-05-07-structural-reduction-roadmap.md`)
**Status**: **Phases 0-3 shipped** (commits `47b9b0c6`, `924ebe43`,
`62f053ad`, and the head of `feat/registry-sr-phase3`). Tier A.3 analysis
side closed. Phases 4-6 deferred per recommended phasing.
**Priority**: **Medium-High** (Tier 5 tech debt; the last Tier A target
before moving to Tier B per the values-SR plan's closing summary).

## Phase status (2026-05-19)

| Phase | Findings              | Status            | Where                                       |
|-------|-----------------------|-------------------|---------------------------------------------|
| 0     | 4 + 5 + 1-Step1 + 9   | ✅ Shipped         | commit `47b9b0c6` (also ride-along: lock-   |
|       |                       |                   | internalize on deepCopy, ADDING-A-NEW-      |
|       |                       |                   | CATEGORY guide, test contract coverage,     |
|       |                       |                   | doc header casing)                          |
| 1     | 2                     | ✅ Shipped         | commit `924ebe43` on `feat/registry-sr-     |
|       |                       |                   | phase1`                                     |
| 2     | 3 (Opportunity 2)     | ✅ Shipped         | commit `62f053ad` on `feat/registry-sr-     |
|       |                       |                   | phase2`                                     |
| 3     | 7 (Opportunity 4)     | ✅ Shipped         | `feat/registry-sr-phase3`                    |
| 4     | 1 Step 2              | ⏸ Deferred         | gated on 7th-category trigger               |
| 5     | 6 (ArgShape)          | ⏸ Deferred         | gated on extension-contracts Phase 2+       |
| 6     | 8, 9 unification      | ⏸ Deferred         | gated on iter.Seq2 refactor / 7th variant   |

The body of this document still describes findings in their
*pre-Phase-0* form for historical fidelity. Phase 0 findings are
annotated **✅ SHIPPED** inline; their line-number citations
(`registry.go:328-347` etc.) reflect the pre-Phase-0 file layout. The
post-Phase-0 layout differs: `Clone` is now ~3 lines, `deepCopy` is
the shared helper around line 343 of the current `registry.go`,
filter methods are 5-line uses of `slices.DeleteFunc`.

## Why this scope

`registry/` is the contract surface for every primitive in Wile —
~397 entries (per `TODO.md`'s status header) across `registry/core/`
(49 prod files; 126 test files in the same directory) and 12
extensions. Twenty packages import it. The roadmap
(`2026-05-07-…`) ranks it
A.3 because, unlike `values/` (most-depended-on) and `environment/`
(binding-resolution algebra), `registry/` carries the *API shape of
extension authoring*: how primitives are declared, how phases are
expressed, how documentation flows, how capabilities are advertised.
Improvements compound across every present and future extension.

The cross-package "dispatch axis as data" plan
(`plans/2026-05-08-dispatch-axis-as-data.md`) predicted further
instances inside `registry/` after Phase unification shipped (PR #728).
This analysis confirms two — the parallel-slices-as-categories pattern
in `Registry`, and the capability-detection cascade for `Extension` —
and adds seven more findings the cross-package plan did not predict.

## Scope analyzed

```
Package layout (production files only; test files excluded):
  registry/             8 prod files,  ~1460 LOC  (registry, apply, phase,
                                                   search, contract,
                                                   extension, builder, doc)
  registry/helpers/    12 prod files,  ~1820 LOC  (args, char, equality,
                                                   integer, list, numeric,
                                                   sequence, string, type,
                                                   value_conv, variadic,
                                                   doc)
  registry/testhelpers/ 4 prod files,   ~481 LOC  (helpers, pipeline_helpers,
                                                   env_helpers, doc)
  registry/core/       49 prod files,  ~6952 LOC  (registration + impl;
                                                   consumed by exactly two
                                                   packages — wile root
                                                   and internal/bootstrap)
```

(An earlier draft of this plan reported `158 files / ~45000 LOC` for
`registry/core/`; that figure conflated production with the 126
co-located test files. The corrected prod-only counts above do not
change any structural conclusion — the findings are driven by
*method/type* counts, not by file/LOC volume.)

The `registry/core/` mass is by design — it's the catalog of primitives,
not the abstraction. The structural findings target the three smaller
packages (`registry/`, `registry/helpers/`, `registry/testhelpers/`),
since those carry the contract surface that `registry/core/` and every
extension consume.

## Dependency map

```
                       ┌──────────────────┐
                       │      werr         │  Ca=33  (I not measured)
                       └────────┬─────────┘
                                │ sentinels + WrapForeignErrorf
                                ▼
                       ┌──────────────────┐
                       │     values        │  Ca=32  (I not measured)
                       └────────┬─────────┘
                                │ Value, Number, Tuple, TypeConstraint
                                ▼
   ┌────────────────────────────────────────────────────────────────┐
   │                        machine + machine/compilation             │
   │   (machine: ForeignFunction, CallContext, etc.)                  │
   │   (compilation: LibraryRegistry, LibraryExportIndex)             │
   └────────────────────┬───────────────────────────────────────────┘
                        │ ForeignFunction, NewForeignClosure,
                        │ LibraryRegistry  (← over-broad surface,
                        │                     Finding 7 — ISP, not SDP;
                        │                     direction is correct)
                        ▼
                       ┌──────────────────┐    ┌──────────────────┐
                       │ environment       │    │   docparse        │
                       │  Phase, frames    │    │  ParseDocstring   │
                       └────────┬─────────┘    └────────┬─────────┘
                                │ Phase, EnvironmentFrame, BindingType
                                ▼                       ▼
   ┌────────────────────────────────────────────────────────────────┐
   │                          registry                                │
   │   Ca=20   Ce=6   I = 6/26 ≈ 0.23   (8 files, ~1460 LOC)          │
   │                                                                   │
   │     Types:                          Submodules:                   │
   │       Registry          (state)       registry/helpers   (Ca=14)  │
   │       PrimitiveSpec     (contract)    registry/testhelpers(test)  │
   │       PrimitiveRegistration           registry/core      (Ca=2)   │
   │       BindingSpec                                                 │
   │       DocEntry           ── Finding 2: structurally identical     │
   │       GlobalValue            to BindingSpec                       │
   │       InitFunc                                                    │
   │       Extension         (interface)                               │
   │       LibraryNamer      (opt iface)── Finding 3: detected via     │
   │       Describer         (opt iface)   type-assert dance at        │
   │       Closeable         (opt iface)   engine.go:287-307           │
   │       PhaseSet          (bitset; PR #728 already tightened)       │
   └────────────────────┬───────────────────────────────────────────┘
                        │ Registry, PrimitiveSpec, Extension, PhaseSet
                        ▼
        wile (root), repl/, cmd/wile/, internal/bootstrap, internal/testutil,
        internal/extensions/{io,namespace,all,envvars,iotest},
        extensions/{math,files,system,process,threads,gointerop,
        introspection,charsets,eval}, registry/core
        (20 importing packages)
```

The graph is a clean DAG. No SDP violation — registry/ depends only on
packages below it in the layering documented in `CLAUDE.md`
(`werr → values → ... → machine + security → registry`). One
**non-narrow** dependency edge — `registry/search.go` →
`machine/compilation` — is Finding 7. The direction is fine; the issue
is that registry takes a dependency on the whole `LibraryRegistry`
surface to use 2-3 read methods (ISP violation, not SDP violation).

Stability summary: registry/ is moderately stable. Twenty dependents
means breaking changes here propagate widely, but `Ce=6` means the
package itself is well-insulated from churn below it. I values for
neighboring packages were not measured for this plan and are intentionally
not asserted.

## Findings

### Finding 1 — `Registry` holds 6 parallel slices with hand-unrolled lifecycle

> **Status**: Step 1 (`deepCopy()` extraction) ✅ shipped in commit
> `47b9b0c6` (Phase 0). The 6-slice copy block now lives in one helper
> shared by `Clone`, `filterPrimitives`, `WithoutBindings`, and the
> lock has been internalized so callers don't carry the precondition.
> Step 2 (generic `registrationCategory[T]`) deferred — see
> Recommended Phasing → Phase 4. Pre-Phase-0 line numbers below.

**Principle**: Composability (hand-unrolled loops over data)
**Where**: `registry/registry.go:73-94` (struct + ctor),
`:328-347` (Clone), `:441-471` (filterPrimitives),
`:477-507` (WithoutBindings); `apply.go:49-117` (Apply)

**Theory**: This is the **categories-as-code, not-as-data** anti-pattern
formalized in Bird & de Moor (*Algebra of Programming*, 1997, Ch. 3 —
*Datatypes*). When a struct holds N parallel collections, each requiring
the same lifecycle operations (add, copy, count, filter), the N×op
matrix is implicitly multiplied across every method that touches the
struct. The natural representation is to *defunctionalize* the category
axis — make "which category" a value, not a position in source code.

**Current state**: `Registry` has 6 fields:

```go
type Registry struct {
    mu           sync.RWMutex
    primitives   []PrimitiveRegistration
    bindingSpecs []BindingSpec
    docs         []DocEntry
    initFuncs    []InitFunc
    macroSources []string
    globalValues []GlobalValue
}
```

Cost of adding a 7th category (e.g., `aliasSpecs`):

  - Add field + initialize in `NewRegistry()` (one block edit)
  - Add `AddX` / `AddXs` methods              (registry.go:97-220)
  - Add `XCount` / `Xs` accessor methods      (registry.go:223-326)
  - Branch in `Clone`                          (registry.go:328-347)
  - Branch in `filterPrimitives`               (registry.go:441-471)
  - Branch in `WithoutBindings`                (registry.go:477-507)
  - Step in `Apply`                            (apply.go:49-117)

Seven coordinated edit sites, three of which (Clone / filterPrimitives /
WithoutBindings) duplicate the same 6-slice deep-copy block. This is
the same shape `internal/extensions/io/state.go` had before its
rewrite, and the same shape `values/` ports had before PR #749.

**Problem**: The repetition is *structural* — every category requires
the same operations — but expressed as *parallel code*. The 6-slice
copy block (12 statements) appears three times, for a total of 36
maintenance points where they should be one. Adding the 7th category
requires touching all three sites; missing one creates silent state
corruption (the new field stays at zero-value in the clone).

**Type precision**: The struct's representable state space is the
Cartesian product of 6 slices = ∞⁶ (representable). The semantically
valid state space is the same — there's no impossible combination of
slice contents — so type-precision is not the issue here. The issue
is **operational tightness**: the same operation on 6 collections is
spelled 6 times.

**Proposed direction**: Two-step.

*Step 1 (small, immediate)*: Extract a private `(p *Registry).deepCopy()
*Registry` method that performs the 6-slice clone exactly once.
`Clone`, `filterPrimitives`, and `WithoutBindings` each call it, then
mutate only the slice they need to change. Eliminates ~30 lines of
duplication. No API change.

*Step 2 (larger, optional)*: Introduce a generic
`registrationCategory[T any]` with `items []T`, `add(T)`,
`addMany([]T)`, `count() int`, `snapshot() []T`, `clone()
registrationCategory[T]`. Compose `Registry` as 6 typed fields of this
category. Adding the 7th category becomes "add one field"; all the
Add/Get/Clone operations factor through the category. Go's generics
don't permit a heterogeneous map of `registrationCategory[T]` values,
so the struct stays a struct — but the per-field code shrinks.

**Impact**:
- Step 1: ~30 LOC removed; the 6-slice copy lives in one helper that
  the 3 callers share. (Forgetting to update the helper is still
  possible — Step 1 *consolidates* the drift surface, it doesn't
  *eliminate* it.)
- Step 2: ~80 LOC removed; per-category code paid once.

---

### Finding 2 — `DocEntry` and `BindingSpec` are structurally identical, and four doc paths exist

**Principle**: State Tightness (value aliases / common subexpression
elimination on types) + Composability (multiple-paths-for-one-concept)
**Where**: `registry/registry.go:60-69` (type defs);
`apply.go:184-190` (the smoking-gun cast); `:206-220` (4th doc path)

**Theory**: When two types have identical field shape and one can be
*cast* to the other (Go allows `T(v)` only when the underlying structs
match exactly), they are **value-equivalent** in the type-theoretic
sense (Pierce, *TAPL* §11.6 — *Equivalence of named types*). The
multiple type identities are then **accidental nominal distinctions**,
not real semantic distinctions. By the **Common Subexpression
Elimination** rule from compiler theory applied to types, equivalent
types should be merged unless the distinction does load-bearing work.

**Current state**: Four mechanisms attach documentation to a name:

| # | Mechanism                          | Carries                           |
|---|------------------------------------|-----------------------------------|
| 1 | `PrimitiveSpec.Doc`                | Doc on a real primitive           |
| 2 | `BindingSpec.Doc`                  | Doc on a compile-time binding     |
| 3 | `DocEntry` (via `AddDocumentation`)| Doc only, no binding lifecycle    |
| 4 | `AddDocOnlyPrimitive` (PhaseSet=0) | Doc-only "primitive" entry        |

And:

```go
// registry/registry.go
type BindingSpec struct {
    Name string
    Doc  string
}
type DocEntry struct {
    Name string
    Doc  string
}
```

The smoking gun is `apply.go:188`:

```go
for _, spec := range p.bindingSpecs {
    if spec.Doc != "" {
        allDocs = append(allDocs, DocEntry(spec))  // ← direct cast
    }
}
```

Go permits this conversion only when the structs are structurally
identical. The cast exists because `ApplyDocs` walks "all things that
carry a doc string indexed by name" — and the two categories *are* the
same shape, distinguished only by which slice they live in.

**Problem**: Four registration entry points, two structurally
identical types, one direct cast in the merge code. Every
doc-handling site has to walk multiple sources and check provenance.
The redundancy is the parallel data, not the operations:

  - `SearchDoc` walks primitives + binding specs + doc entries + env
    + libraries + unloaded exports (6 sources)
  - `NonPrimitiveDocs` walks binding specs + doc entries to build
    `[]DocSearchResult` (2 sources for the same conceptual category)
  - `ApplyDocs` walks doc entries + binding specs (2 sources, then
    merges via cast)

Note: `AddDocOnlyPrimitive` does **not** create a new data slice —
it appends to the existing `primitives` slice with `PhaseSet=0`,
so the Apply path silently ignores it for binding installation
(`apply.go:69-76` and `:81-98` both gate on phase bits being set).
That is, the four mechanisms above all carry doc strings; they share
three underlying slice categories (`primitives`, `bindingSpecs`,
`docs`). The redundancy is in the *registration API*, not in the
storage shape.

**Proposed direction**: Collapse to a canonical representation.

*Path A (minimal, recommended)*: Make `DocEntry` an alias of
`BindingSpec` and document that `AddDocumentation(name, doc)` is
sugar for `AddBindingSpecs([]BindingSpec{{Name: name, Doc: doc}})`
with a "doc-only" flag. The `docs` slice on `Registry` becomes
redundant — every doc lives on a `BindingSpec`. Walk-once
implementations replace the merge-then-walk pattern.

*Path B (typed cleanly)*: Introduce a `DocOnly bool` on `BindingSpec`
to distinguish "create a binding" from "only carry doc text." The
`docs` slice collapses into `bindingSpecs`. `AddDocOnlyPrimitive` is
also expressible via this flag.

Both paths kill 1 type, 1 slice, and 1 redundant Add method. Path A is
slightly less explicit but preserves binary compatibility for
embedders who might inspect `Registry.Docs()`.

**Impact**: ~50 LOC removed; doc-handling code paths collapse from 4
to 2 (primitive doc + binding-or-standalone doc); the parallel
"merge in ApplyDocs" pattern goes away.

---

### Finding 3 — Extension capability interfaces detected via type-assertion cascade

**Principle**: Composability (capability-slot vs capability-interface
duality) + State Tightness (boolean blindness)
**Where**: `registry/extension.go:25-45` (the three optional
interfaces); `engine.go:287-307` (the detection cascade);
`registry/extension.go:47-83` (`ExtensionFunc` — half-implements the
slot pattern)

**Theory**: When an interface is "optional" (callers must type-assert
to discover support), the type system has stopped helping. The choice
between **capability interfaces** (Go's typical idiom) and **capability
slots** (one struct with optional fields populated by options) is the
duality between **product types** (struct with all slots, some nil)
and **sum-over-interfaces** (multiple smaller types, each implementing
a subset).

Harper (*Practical Foundations for Programming Languages* §11.1)
identifies the trade-off: interfaces are open (any new type can
implement) but make discovery O(N) type assertions; slots are closed
(the struct's shape is fixed) but discovery is O(1) field access and
the surface is uniform. For *internal-author* extensions, slots win on
ergonomics. For *external-author* extensions, interfaces win on
extensibility. The right answer for a public API often combines both.

**Current state**:

```go
// registry/extension.go
type Extension interface {       // required
    Name() string
    AddToRegistry(r *Registry) error
}
type LibraryNamer interface {    // optional
    LibraryName() []string
}
type Describer interface {       // optional
    Description() string
}
type Closeable interface {       // optional
    Close() error
}
```

Detected at `engine.go:287-307` via three type-assertion blocks:

```go
n, ok := ext.(registry.LibraryNamer)    // each block:
if ok { namer = n }                       //   declare, assert, branch,
                                          //   assign-or-skip
d, ok := ext.(registry.Describer)
if ok { describer = d }

c, ok := ext.(registry.Closeable)
if ok { closers = append(closers, c) }
```

The `ExtensionFunc` adapter already partly implements the slot pattern
— it has fields for `name` and `description` populated by
`NewDescribedExtension`. But it has no slot for `LibraryName` or
`CloseFn`. An extension author who wants to set both must implement a
*custom type* — the adapter doesn't compose all four capabilities.

**Problem**:
- Three type assertions per extension at engine boot (one-time cost,
  but representative of the design)
- `ExtensionFunc` is asymmetric: half-slots, half-interface — authors
  who want library names or close hooks must abandon the adapter
- Adding a fifth optional capability (e.g., a docstring loader, an
  init hook with engine context) requires both a new interface *and*
  a new detection block

**Proposed direction**: Extend `ExtensionFunc` with all four optional
capabilities via constructor options. Keep the three interfaces public
for backward-compatible custom implementations.

```go
type ExtensionFunc struct {
    name          string
    addToRegistry func(*Registry) error
    description   string                  // ← already there
    libraryName   []string                // ← new slot
    closeFn       func() error            // ← new slot
}

func NewExtension(name string, fn func(*Registry) error, opts ...Option) Extension
func WithDescription(s string) Option
func WithLibraryName(parts ...string) Option
func WithClose(fn func() error) Option
```

`ExtensionFunc` implements all three optional interfaces — the
type-assertion cascade in `engine.go` still works for custom types,
and the struct-path covers ~all real extensions today.

**Impact**:
- Removes the friction of "must I write a custom Extension type?"
  for ~80% of new extensions
- Symmetric: all four optional capabilities are first-class
- One detection cascade still exists in engine.go but is the only one
  (callers don't repeat the pattern)
- Reduces author cognitive load: one constructor, one option list

Same structural pattern as `values/` port unification (PR #749) —
capability slots over capability interfaces — but a much smaller
surface and a different cost ledger. Port unification touched 9
hot-path types serving I/O on every read/write call. Extension
capability detection runs *once at engine boot* across ~10
extensions; the win here is API ergonomics for new extension authors,
not VM performance.

---

### Finding 4 — Telescoping `Add*` constructors

> **Status**: ✅ Shipped in commit `47b9b0c6` (Phase 0). `AddPrimitive`,
> `AddBinding`, `AddBindings` are now 1-line forwarders to
> `AddPrimitives` / `AddBindingSpecs`. Validation + mutex live in one
> site per category.

**Principle**: Composability (functions doing too few things; thin
wrappers)
**Where**: `registry/registry.go:97-166`:
- `AddPrimitive` / `AddPrimitives` (two entry points)
- `AddBinding` / `AddBindings` / `AddBindingSpecs` (three entry points)

**Theory**: Telescoping constructors (Bloch, *Effective Java* §2 —
*Builder*) are the symptom of "too many arities of the same
operation." When the only difference between two methods is "takes a
T" vs "takes a []T," the singular form is sugar that doesn't earn its
keep — it duplicates the underlying state machine (mutex lock,
validation, append) for the saving of one `[]{...}` literal at the
call site. PR #739's `match.NewMatcher*` collapse (4 ctors → 1 + N
options) is the same shape.

**Current state**: Five `Add` entry points where two suffice:

```go
func (p *Registry) AddPrimitive(spec PrimitiveSpec, phases PhaseSet)
func (p *Registry) AddPrimitives(specs []PrimitiveSpec, phases PhaseSet)
func (p *Registry) AddBinding(name string)
func (p *Registry) AddBindings(names []string)
func (p *Registry) AddBindingSpecs(specs []BindingSpec)
```

`AddPrimitive` and `AddPrimitives` each do: validate → lock → append →
unlock. The singular variant has no behavior the plural lacks.

`AddBinding`, `AddBindings`, `AddBindingSpecs` are three flavors of
"register N name-or-name-with-doc records." Two of them differ only in
"do you have a Doc string?" — which a single `AddBindingSpecs` covers
already (`BindingSpec{Name: name}` has empty Doc).

**Problem**: Each pair has two lock/unlock sites, two validation
paths, two places to forget if a change is made. The singular variants
are doc sugar that can be safely expressed as one-liners forwarding to
the plural.

**Proposed direction**: Singular variants become 1-line forwarders.

```go
func (p *Registry) AddPrimitive(spec PrimitiveSpec, phases PhaseSet) {
    p.AddPrimitives([]PrimitiveSpec{spec}, phases)
}
func (p *Registry) AddBinding(name string) {
    p.AddBindings([]string{name})
}
func (p *Registry) AddBindings(names []string) {
    specs := make([]BindingSpec, len(names))
    for i, n := range names {
        specs[i] = BindingSpec{Name: n}
    }
    p.AddBindingSpecs(specs)
}
```

Three methods reduced to forwarders; the *actual* state-changing
methods (`AddPrimitives`, `AddBindingSpecs`) become the single source
of truth.

**Impact**: ~30 LOC removed; mutex-and-validation lifecycle factored
to one site per category. Same shape as the PR #739 telescoping-ctor
collapse, applied to a different package.

---

### Finding 5 — `Clone`, `filterPrimitives`, `WithoutBindings` repeat the 6-slice copy block

> **Status**: ✅ Shipped in commit `47b9b0c6` (Phase 0), as part of
> Finding 1 Step 1. The per-callsite 6-slice copy is now factored
> into the shared `deepCopy()` helper.

> **Relationship to Finding 1**: Finding 5 is the *call-site evidence*
> for Finding 1's Step 1 (extract `deepCopy()`). The fix is one PR
> that addresses both. They are listed separately so each can be
> evaluated on its own merits, but counting "9 findings" overstates the
> distinct count — effectively 8.

**Principle**: Composability (hand-unrolled loops differing only in data)
**Where**: `registry/registry.go:328-347` (Clone), `:441-471`
(filterPrimitives), `:477-507` (WithoutBindings)

**Theory**: Same as Finding 1 (category-as-code), but at the operation
layer rather than the field layer. Three sites do the same 6-slice
copy with one slice substituted; the substitution is *the algorithm
they implement*, the copy is *not*. Separating them is the
**fusion-of-replicated-prefix** transformation from program calculation
(Bird & de Moor, Ch. 7 — *Recursion patterns*).

**Current state**: Three methods that produce a new `Registry`. Each
manually performs the same 12-statement clone (6 `make`, 6 `copy`)
with one slice substituted by a filtered version. ~30 lines of dead
duplication; growing to 35 lines with the 7th category from Finding 1.

**Proposed direction**: Extract `(p *Registry).deepCopy() *Registry`
that clones all 6 slices and returns the new registry. Each filter
method calls it, then overwrites the slice it cares about:

```go
func (p *Registry) Without(names ...string) *Registry {
    q := p.deepCopy()
    exclude := setOf(names)
    q.primitives = slices.DeleteFunc(q.primitives,
        func(r PrimitiveRegistration) bool { return exclude[r.Spec.Name] })
    return q
}
```

(Note: this is a textbook **Filter as a fold** — pulling the 6-slice
clone into one shared step and parameterizing the discriminator. See
Bird, *Functional Pearls*: *The countdown problem*.)

**Impact**: ~30 LOC removed; the per-callsite 6-slice copy is
factored into one shared helper. This fix and Step 1 of Finding 1
are the same edit.

**Sequence with Finding 1**: Step 1 of Finding 1 (the `deepCopy`
extraction) *is* this fix. Treat as one PR; this finding is the
motivating call-site evidence.

---

### Finding 6 — `PrimitiveSpec` carries implicit invariants between three fields

**Principle**: State Tightness (make illegal states unrepresentable)
**Where**: `registry/registry.go:27-43` (struct), `:125-143`
(validateParamTypes)

**Theory**: When a struct has N fields with cross-field invariants
checked at runtime, the type's **representation invariant** (Liskov &
Guttag, *Program Development in Java* §5.6) is encoded in code rather
than types. The invariant runs every time the type is constructed but
provides no compile-time guarantee. Minsky's "Effective ML" rule:
**make illegal states unrepresentable** — encode the invariant in the
type so the compiler rejects the bad combinations.

**Current state**: `PrimitiveSpec` has three correlated fields:

```go
ParamCount int                       // 0..∞ representable
IsVariadic bool                      // 2 states
ParamTypes []values.TypeConstraint   // nil or len in [1, ParamCount]
```

Cross-field invariants enforced by `validateParamTypes`:

```
if len(ParamTypes) == 0           → any state legal (unannotated)
if !IsVariadic                    → len(ParamTypes) == ParamCount
if IsVariadic                     → 1 ≤ len(ParamTypes) ≤ ParamCount
```

State space: `int × bool × []TypeConstraint` = ∞ × 2 × ∞ = ∞.
Semantically valid subset: a tiny fraction — for each `ParamCount=N`,
only `len(ParamTypes) ∈ {0, [1..N] for variadic, {0, N} for
non-variadic}`. The invariant is **runtime-checked rather than
type-checked**: every production-path registration funnels through
`AddPrimitive` / `AddPrimitives` → `validateParamTypes` → panic on
violation, so the *operational* risk today is near zero. The
*structural* risk is that a future path constructing a `PrimitiveSpec`
without going through these registers (test fixtures, JSON
deserialization, generative testing) can produce an invalid spec the
type system accepts.

**Problem**: The invariant lives in code, not types. Adding new
construction paths (or refactoring the existing ones) requires
remembering to call the validator. The type system gives no help.

**Proposed direction**: Replace the three correlated fields with a
typed arity descriptor that makes the relationship explicit:

```go
type ArgShape struct {
    Fixed []values.TypeConstraint  // typed positions; len = fixed arity
    Rest  values.TypeConstraint    // nil if not variadic; otherwise rest element type
}

type PrimitiveSpec struct {
    Name       string
    Args       ArgShape           // ← replaces ParamCount, IsVariadic, ParamTypes
    Impl       machine.ForeignFunction
    Doc        string
    ...
}
```

The fixed-vs-variadic distinction becomes `Args.Rest == nil` (boolean
blindness retained but in a smaller surface); the
`len(Fixed) == ParamCount` invariant evaporates because there's no
longer a separate `ParamCount` to disagree with `Fixed`.

A migration helper preserves the old constructor signature so
extensions don't all change at once:

```go
func PrimSpec(name string, fixed int, variadic bool,
    types []values.TypeConstraint, impl machine.ForeignFunction) PrimitiveSpec
```

**Cost**: Breaking change to `PrimitiveSpec` literals — touches every
primitive registration site. **Precise site count uncertain**: a grep
for the `PrimitiveSpec{` literal opening finds **71 occurrences**
across `registry/core/` + `extensions/` + `internal/extensions/`;
TODO.md reports **397 registered primitives** total. The true edit
count lies between these two figures (many primitives are constructed
in bulk slice-of-specs blocks where one literal opens a slice of N
specs). A precise count must be produced before Phase 5 is scheduled —
the cost estimate determines whether to bundle with extension-contracts
Phase 2 (worth it if the figures are close) or schedule independently
(better if the count is significantly higher than 71). Conversion is
mechanical regardless: the literals are already mostly named-field
style; the change is search-and-replace.

**Recommended timing**: **Defer.** Two reasons:

1. Extension contracts Phase 2 (per
   `plans/2026-03-26-extension-contracts-phase2-design.md`) is
   *partially shipped* — Phase 1 infrastructure done + 172 core
   primitives annotated; ~228 extension primitives still pending.
   Phase 2's remaining work re-touches the spec literals in every
   extension package. Piggybacking the `ArgShape` migration on the
   same file-touch pass amortizes diff coverage even though the two
   edits are independently mechanical (one *adds annotations to
   existing fields*, the other *restructures the fields*).
2. The operational risk today is low because every production
   construction path is funneled through `validateParamTypes`. The
   migration earns its keep when the type-vs-runtime gap is paid
   down alongside other ParamTypes work, not on its own.

**Impact**: Invariant moves from runtime-checked to type-checked; the
`validateParamTypes` panic site disappears.

---

### Finding 7 — `registry/search.go` takes an over-broad dependency on `machine/compilation`

**Principle**: Dependency Minimization (narrowest possible dependency —
Interface Segregation Principle)
**Where**: `registry/search.go:24` (import), `:29-38`
(ExtractLibraryRegistry), `:61` (SearchDoc signature),
`:233-293` (searchLibraries, searchUnloadedExports)

**Theory**: Per the **Interface Segregation Principle** (Martin,
*Clean Architecture* Ch. 14), depending on an interface with N methods
when you use M < N of them couples you to N-M methods you don't need.
Every method on the depended-upon surface is a channel for the
dependency package to change in ways that ripple into yours. Pierce
(*TAPL* §15) frames this as the **width-subtyping** trade-off: a wider
type accepts fewer values, but a *needed* type that's wider than
necessary admits more change.

**Note on direction**: registry → machine/compilation is the
*correct* direction in CLAUDE.md's layering
(`... → machine/ + security/ → registry/`). This is not an SDP
violation; the issue is solely about surface width.

`SearchDoc` accepts a `*compilation.LibraryRegistry`, but uses only
two methods: `All()` and `Lookup()`. The full type is a much larger
surface (registration, loading, import-set semantics). By ISP, the
dependency should be on the methods used, not the whole type.

**Current state**:

```go
// registry/search.go
import "github.com/aalpar/wile/machine/compilation"

func ExtractLibraryRegistry(env *environment.EnvironmentFrame) *compilation.LibraryRegistry {
    ...
    lr, ok := env.LibraryRegistry().(*compilation.LibraryRegistry)
    ...
}

func SearchDoc(reg *Registry, env *environment.EnvironmentFrame,
    libReg *compilation.LibraryRegistry,                 // ← concrete type
    exportIndex *compilation.LibraryExportIndex,         // ← concrete type
    pattern string) []DocSearchResult
```

Only two methods are used on `*compilation.LibraryRegistry`:
- `lib.Name.SchemeString()` (the Name field)
- `lib.Description`

And on `*compilation.LibraryExportIndex`:
- `idx.Entries()`

The actual `LibraryRegistry` surface includes registration, lookup by
import set, phase-keyed binding installation, etc. — none of which
`SearchDoc` touches.

**Problem**: registry/ takes a dependency on the *entire* compilation
package to use 2-3 read methods. Compilation churn (new fields, new
methods, signature changes) ripples into registry's compile graph.
Tests for registry can't be written without setting up a compilation
pipeline.

**Proposed direction**: Define a narrow searcher interface in
registry/:

```go
type LibrarySearcher interface {
    All() []LibrarySummary
    Lookup(name LibraryName) *LibrarySummary
}

type LibraryExportSearcher interface {
    Entries() []LibraryExportSummary
}

// in compilation/, LibraryRegistry already satisfies LibrarySearcher
```

(`LibrarySummary` / `LibraryName` / `LibraryExportSummary` are small
DTOs in registry/ that compilation's types convert to. Or — simpler —
the interfaces are over the *fields* needed, not new DTOs.)

`SearchDoc` accepts the interfaces. `ExtractLibraryRegistry`
disappears (the type assertion is the caller's problem, or
`env.LibraryRegistry()` returns the interface directly).

**Impact**:
- Ce drops from 6 to 5 (the only `machine/compilation` import in
  registry/ goes away)
- registry/search.go is independently testable with a stub searcher
- compilation can refactor freely without rippling into registry/

**Cost**: One new interface in registry/; one call-site change in
engine.go where SearchDoc is invoked.

---

### Finding 8 — `validateFixed` and `validateVariadic` are sibling functions with parallel structure

**Principle**: Composability (similar functions with parameterizable difference)
**Where**: `registry/contract.go:65-120`

**Current state**: `BuildValidator` returns one of two closures based
on `IsVariadic`. The two helpers share:
- The per-position type-check loop
- The `Check(arg) → wrap on failure` shape
- The `nil constraint = skip` rule

They differ in:
- Variadic clamps `min(i, lastIdx)` for fixed slots past the end of
  `types`
- Variadic walks the rest-list cdr-chain after the fixed slots

**Problem**: Lower-priority duplication (~50 LOC each, ~30 LOC shared
shape). The duplication is real but the two paths have different
arity-handling that obscures the commonality.

**Proposed direction**: Convert both paths to iterate a unified
"sequence of (index, value) pairs" — for fixed args, the sequence is
just `mc.Arg(0..ParamCount-1)`; for variadic, it's the fixed args
followed by the rest-list iter. Go 1.23 `iter.Seq2` makes this clean.

```go
func argSeq(mc machine.CallContext, paramCount int, variadic bool) iter.Seq2[int, values.Value]
func validateArgs(seq iter.Seq2[int, values.Value], types []TypeConstraint, name string) error
```

**Impact**: ~30 LOC removed; the two paths become one. Lower priority
than Findings 1-7 — defer unless an iter.Seq2 refactor is happening
elsewhere.

---

### Finding 9 — Numeric helper family has 6 fold variants

> **Status**: Documentation half ✅ shipped in commit `47b9b0c6`
> (Phase 0). A `# Fold-Shape Family` section now lives in
> `registry/helpers/doc.go` naming the six variants and the
> protocol × accumulator × side-channel axes. Code consolidation
> remains a **no-op** until a 7th protocol-fundamentally-different
> variant motivates it — see "Revisit trigger sharpening" below.

**Principle**: Composability (parametric family that could share a skeleton)
**Where**: `registry/helpers/numeric.go` (NumericFoldVariadic,
NumericFoldWithFirst, NumericChainCompare, NumericChainCompareReal,
NumericExtremum); `registry/helpers/integer.go` (IntegerFold)

**Theory**: These six functions are all instances of `fold (⊕) ε [...]`
(Bird & de Moor, Ch. 3 — *Catamorphisms*). They differ along three
axes — (a) variadic protocol (rest-at-0 vs first+rest-at-1),
(b) accumulator type (Number, comparison-state, extremum-tracking),
(c) per-element side effects (NaN detection, exactness contagion).

**Current state**: Six well-named, well-tested variants. Each is
~40-80 LOC. They share the proper-list-validation tail, the type-check
shape, and the ForEach pattern. They have small fast paths for the
common 2-arg case.

**Problem (mild)**: Adding a 7th variant means writing another
fold-shaped function from the same template. The shared skeleton isn't
extracted.

**Proposed direction**: **No-op for now.** The family-relationship is
real but the trade-off favors readability — a generic
`Fold[Acc any](mc, name, protocol, initial, step)` forces callers to
spell out the accumulator type and step closure, losing the named-
variant documentation effect. Cost-benefit doesn't justify the change
at 6 variants.

**Revisit trigger**: A 7th variant is added (e.g., for arithmetic
with overflow tracking, or for set-fold operations). At that point,
extract the skeleton.

**Documentation action now**: Add a paragraph to
`registry/helpers/doc.go` (or the top comment of `numeric.go`) naming
the family — all six (five in `numeric.go`, one in `integer.go`) are
fold-shaped variants over Scheme numbers; differences are protocol ×
accumulator × side-channel. This is structural-reduction-lite — make
the relationship visible without forcing a refactor.

**Revisit trigger sharpening**: A 7th variant that fits the existing
template (another numeric fold) doesn't motivate consolidation — the
template absorbs it. A 7th variant with a *fundamentally different
protocol* (e.g., an async fold; an early-termination fold with a
return-value-producing sentinel) is the trigger.

---

## Opportunities (sort-package style)

### Opportunity 1 — `registrationCategory[T any]` for the 6-slice Registry

**Replaces**: 6 fields × 4 operations (Add, count, snapshot, clone) =
24 manually-implemented combinations in `registry.go`.

**Core operation**: Per-category "append-many, snapshot copy, clone".

**Algebraic structure**: A *monoid* under append (identity: empty
slice; associative). Each `registrationCategory[T]` is `(slice T, ⊕,
ε) = (slice, append, [])`. Six homogeneous monoids composing into one
struct.

**Proposed shape**:

```go
type registrationCategory[T any] struct {
    items []T
}

func (c *registrationCategory[T]) add(item T)
func (c *registrationCategory[T]) addMany(items []T)
func (c *registrationCategory[T]) count() int
func (c *registrationCategory[T]) snapshot() []T
func (c *registrationCategory[T]) clone() registrationCategory[T]
func (c *registrationCategory[T]) filter(keep func(T) bool) registrationCategory[T]

type Registry struct {
    mu           sync.RWMutex
    primitives   registrationCategory[PrimitiveRegistration]
    bindingSpecs registrationCategory[BindingSpec]
    initFuncs    registrationCategory[InitFunc]
    macroSources registrationCategory[string]
    globalValues registrationCategory[GlobalValue]
    // docs subsumed by bindingSpecs per Finding 2
}
```

**Reuse sites**: Inside `registry/` only — this analysis didn't survey
other packages for the same shape. The abstraction earns its keep on
the in-package use alone (six categories → one factored type). If
other packages later exhibit a similar parallel-slices pattern,
they can adopt; speculative adoption isn't part of the case here.

**Caveat**: Go's generics don't permit `map[CategoryName]
registrationCategory[any]` — the heterogeneous-collection problem.
Each field stays separately typed; the win is per-category
operations defined once.

---

### Opportunity 2 — `*Extension` struct with capability slots

**Replaces**: 4 interfaces (Extension + 3 optional) + 3 type-assertion
blocks in `engine.go:287-307`.

**Core operation**: Construct an extension carrying a name, a
register-function, and 0+ optional capabilities (description, library
name, close-fn).

**Algebraic structure**: An *applicative-style builder*. The
constructor `NewExtension(name, fn, opts...)` is a fold over options;
each `Option = func(*ExtensionFunc)` is the per-step transformation.
Same monoid pattern as middleware composition.

**Proposed shape**:

```go
type ExtensionFunc struct {
    name          string
    addToRegistry func(*Registry) error
    description   string
    libraryName   []string
    closeFn       func() error
}

type Option func(*ExtensionFunc)

func NewExtension(name string, fn func(*Registry) error, opts ...Option) Extension
func WithDescription(s string) Option
func WithLibraryName(parts ...string) Option
func WithClose(fn func() error) Option

// Existing optional interfaces stay public; the struct implements them all
func (p *ExtensionFunc) LibraryName() []string { return p.libraryName }
func (p *ExtensionFunc) Description() string   { return p.description }
func (p *ExtensionFunc) Close() error          { ... }
```

**Reuse sites**: Every extension that wants more than just `Name` +
`AddToRegistry`. Today that's at least the I/O extension (close
hooks), library-named extensions (math, files, etc.), and any future
extension with cleanup needs.

**Direct parallel**: Same shape as `values/` port unification (PR
#749). Capability slots over capability interfaces.

---

### Opportunity 3 — `BindingSpec` as the canonical "named-doc" record

**Replaces**: `BindingSpec` + `DocEntry` + standalone `docs` slice +
`AddDocOnlyPrimitive` doc path.

**Core operation**: "Attach a doc string (and optionally other
metadata) to a name."

**Algebraic structure**: A *singleton dictionary* under merge — each
record is a `(name, doc)` pair; merging is "later writer wins" on the
same name. The merge is the operation `ApplyDocs` already performs.

**Proposed shape**: Path A — alias + flag.

```go
type BindingSpec struct {
    Name    string
    Doc     string
    DocOnly bool        // ← true for AddDocumentation entries
}

// DocEntry kept as an alias for embedder back-compat (Go type aliases
// preserve []DocEntry ≡ []BindingSpec for return values).
type DocEntry = BindingSpec
```

`AddDocumentation(name, doc)` becomes a thin wrapper that appends
`BindingSpec{Name: name, Doc: doc, DocOnly: true}`. The `docs` slice
on Registry goes away. `ApplyDocs` walks `bindingSpecs` once instead
of merging two sources.

**Disposition of `AddDocOnlyPrimitive`**: Subsumed by the same
mechanism. Today it registers a PrimitiveRegistration with
`Phases: 0` — a doc-bearing record that the Apply path correctly
ignores for binding installation (`apply.go:69-76` only handles
`Phases.Has(PhaseCompile/PhaseRuntime/PhaseExpand)`). After Path A,
the same call becomes `AddBindingSpecs([]BindingSpec{{Name: spec.Name,
Doc: spec.Doc, DocOnly: true}})`. The "name-conflict-skip" semantic
(today: skip if a primitive with the same name exists) moves into the
binding-spec append helper.

**Doc-path count after Path A**: 4 → 2.
- (1) `PrimitiveSpec.Doc` — primitive doc (unchanged)
- (2) `BindingSpec` — covers `AddBinding(Spec)s`, `AddDocumentation`,
  and `AddDocOnlyPrimitive`.

**Reuse sites**: All doc-handling sites in registry/ (`SearchDoc`,
`NonPrimitiveDocs`, `ApplyDocs`). REPL's `,doc` command becomes
slightly simpler.

---

### Opportunity 4 — `LibrarySearcher` interface to narrow the registry → compilation dependency

**Replaces**: Direct import of `machine/compilation` from
`registry/search.go`.

**Core operation**: Enumerate libraries (loaded or indexed) for
substring matching on name/description/exports.

**Algebraic structure**: An *enumerator* — a thin read-only window over
the underlying registry. ISP applied: narrow the dependency to the
methods used.

**Proposed shape**:

```go
// registry/search.go
type LibrarySearcher interface {
    All() []LibrarySummary
    Lookup(LibraryName) *LibrarySummary  // returns nil if not present
}

type LibraryExportSearcher interface {
    Entries() []LibraryExportSummary
}

// registry/search.go: small DTOs
type LibrarySummary struct {
    Name        LibraryName
    Description string
}
type LibraryExportSummary struct {
    Name        LibraryName
    Description string
    Exports     []string
}

// SearchDoc accepts the interfaces
func SearchDoc(reg *Registry, env *environment.EnvironmentFrame,
    libReg LibrarySearcher,
    exportIndex LibraryExportSearcher,
    pattern string) []DocSearchResult
```

`compilation.LibraryRegistry` already satisfies `LibrarySearcher`
(rename `All() []*Library` to `All() []LibrarySummary` if the field
shape doesn't already match, or add a small adapter).

**Reuse sites**: Tests for `SearchDoc` no longer need a compilation
pipeline. Future search consumers (e.g., MCP server tools) can swap
in alternate searchers.

---

## What's already done well (preserve)

The following structural choices are *already correct* and should not
be touched:

- **`PhaseSet` design** — bitset with `init()` assertion catching
  drift from `environment.Phase`, comprehensive ADDING-A-NEW-PHASE
  guide, defensive `Has`/`With` rejecting unrepresentable phases.
  PR #728 already collapsed the two-conflicting-Phase-types finding
  from `2026-05-08-dispatch-axis-as-data.md`. Don't touch.

- **`apply.go` `phaseTargets` loop** — Instance C of the
  dispatch-axis-as-data finding (`registerRuntimePrimitive` and
  `registerExpandPrimitive` collapsed to `registerPhasePrimitive`
  with a `phaseTargets` slice). Already shipped; don't undo.

- **`registry/helpers/` layering** — Depends only on `machine`,
  `values`, `werr`, and stdlib. No upward edges. Don't add any.

- **Defensive copy discipline** — Every accessor returns a deep copy.
  Mutation discipline is clean; if Findings 1 + 5 are implemented,
  preserve this.

- **`Builder` pattern + composition** — `RegistryBuilder.AddToRegistry`
  is a clean monoid (`func(*Registry) error` is the operation;
  no-op is the identity; composition is associative). Already correct.

- **`registry/core/` registration-vs-implementation split** —
  `xxx.go` defines specs, `prim_xxx.go` implements them. Clean
  separation of concerns; matches the package's role as
  primitive catalog.

- **Table-driven test discipline** — Mandated in `registry/CLAUDE.md`
  and followed throughout. Don't relax.

---

## Closing summary

**State-space summary**: registry/ has 7 main types
(`Registry`, `PrimitiveSpec`, `PrimitiveRegistration`, `BindingSpec`,
`DocEntry`, `GlobalValue`, `Extension`-as-interface). State precision
issues:

- `Registry`: representable ∞⁶, valid ∞⁶ — operational tightness, not
  state tightness (Finding 1, 5)
- `PrimitiveSpec`: state precision ≈ 0% — three correlated fields
  with runtime-only enforcement (Finding 6)
- `BindingSpec` ↔ `DocEntry`: structurally identical → effective
  type count is 6, not 7 (Finding 2)
- `Extension` + 3 optional interfaces: capability-boolean blindness
  (Finding 3)

**Dependency count**: 6 direct dependencies on registry/, 1 of which
(`machine/compilation`) could be eliminated via Finding 7. Measured
instability: registry/ I = 6/26 ≈ 0.23; registry/helpers/ I = 3/17
≈ 0.18. Both stable. testhelpers/ is test-only and not part of the
production dependency graph.

**Top 3 highest-impact changes** (ranked by states-eliminated +
dependencies-removed + reuse-sites-gained):

1. **Findings 1 + 5 combined** — `deepCopy` extraction (Step 1) and
   optionally `registrationCategory[T]` (Step 2). Step 1: ~30 LOC,
   1 PR, zero API change. Effect: the 6-slice copy becomes one
   consolidation point shared by the 3 callers, rather than a copy
   pattern duplicated 3 times. (The consolidation point still exists
   as one drift surface; the win is "edit one place" rather than
   "drift gone.") Step 2: ~80 LOC, 1 PR, architectural; pay only when
   the 7th category arrives.

2. **Finding 2** — Collapse `DocEntry` into `BindingSpec`; eliminate
   the `docs` slice; rewrite `ApplyDocs` to walk one source. ~50 LOC
   removed; doc paths 4→2.

3. **Finding 3** — `*Extension` struct with capability slots
   (`WithDescription` / `WithLibraryName` / `WithClose`). ~60 LOC
   added + ~30 LOC removed in `engine.go`. Eliminates the
   type-assertion cascade for the common path. Direct parallel to
   PR #749's port unification.

Lower-priority but worth scheduling:

4. **Finding 7** — `LibrarySearcher` interface; drop
   `machine/compilation` import from registry/. Ce 6→5.
5. **Finding 4** — Telescoping ctor collapse. ~30 LOC.

Deferred:

6. **Finding 6** — `ArgShape` tightening. Defer until extension
   contracts Phase 2+ opens (TODO.md Tier 2).
7. **Finding 8** — `validateFixed`/`validateVariadic` unification.
   Defer unless an iter.Seq2 refactor passes through.
8. **Finding 9** — Numeric helper unification. **No-op**; revisit
   at 7th variant.

---

## Recommended phasing

**Phase 0 — Quick wins (single PR)** ✅ **SHIPPED in commit `47b9b0c6`**:
- Finding 4: Telescoping `Add*` ctor collapse (singular forwarders). ✓
- Finding 5 / Finding 1 Step 1: Extract `deepCopy()`; rewrite
  `Clone`, `filterPrimitives`, `WithoutBindings` in terms of it. ✓
- Finding 9 (documentation half): Add family-relationship paragraph
  to `registry/helpers/doc.go`. ✓
- Ride-along (added post-crosscheck):
  - Internalize `p.mu.RLock` inside `deepCopy()` (4-lens crosscheck
    convergence) ✓
  - `ADDING A NEW REGISTRY CATEGORY` guide comment above the Registry
    struct (matches 6 in-tree precedents) ✓
  - Extend `TestRegistry_Without{,Category,Bindings}` to cover the
    docstring contract for the 4 non-filtered fields ✓
  - `# Fold-Shape Family` header casing ✓
- Actual delta: +96 / −66 (+VERSION auto-bump).
- 1 commit on `feat/registry-sr-phase0`.

**Phase 1 — Doc unification (Finding 2)**:
- Design pass first — decide Path A (alias) vs Path B (`DocOnly`
  flag); recommend A for embedder back-compat.
- Collapse `DocEntry` into `BindingSpec` (or alias it).
- Rewrite `ApplyDocs`, `SearchDoc`, `NonPrimitiveDocs` to walk one
  source.
- Decide fate of `AddDocOnlyPrimitive` — keep as sugar or merge.
- Estimated: ~50 LOC removed, 1 PR.

**Phase 2 — Extension capability slots (Finding 3 / Opportunity 2)**:
- Add option-based constructor `NewExtension(name, fn, opts...)`.
- Add `WithLibraryName`, `WithClose` (keep existing
  `NewDescribedExtension` as forwarder).
- `*ExtensionFunc` implements all three optional interfaces.
- engine.go's type-assertion cascade becomes the back-compat path,
  reads the slots directly for the common case.
- Migrate `internal/extensions/io` (the only Closeable today) and a
  couple Describer/LibraryNamer implementors to the new style as
  worked examples.
- Estimated: ~60 LOC added (new options), ~30 LOC removed
  (engine.go simplification + migrated extensions). 1 PR.

**Phase 2 implementation outcome (shipped on
`feat/registry-sr-phase2`)**:
- Pre-impl-audit finding: the plan's claim "the only Closeable today"
  was stale. No production extension implements `Closeable`; only
  `plans/2026-05-14-stderr-flush-on-exit.md` (design-locked, not yet
  implemented) would introduce one. The slot is still useful
  prospectively — the stderr-flush plan can use `WithClose` instead
  of needing a custom struct.
- No production `LibraryNamer` implementor exists either (only the
  `mockLibraryNamerExtension` in `engine_library_test.go`). The
  `WithLibraryName` option earns its keep via the same prospective
  argument.
- Worked-example migrations were therefore *not* applied — there
  were no consumers to migrate. `NewDescribedExtension` remains the
  call site for all ~14 extensions; it now forwards through the
  options API.
- Semantic relaxation in `engine.go`: previously a custom Extension
  implementing `LibraryNamer` but returning an empty slice produced
  an "invalid library name" error; now it falls back to the
  `(wile <name>)` default. This unifies the "did not implement" and
  "implemented but returned the zero value" paths — consistent with
  the slot mental model. No tests asserted the prior defensive-error
  behavior.
- Actual delta: +123 / −31 across `registry/extension.go`,
  `registry/registry_test.go`, `engine.go`.

**Phase 3 — LibrarySearcher interface (Finding 7 / Opportunity 4)**:
- Define `LibrarySearcher`, `LibraryExportSearcher`,
  `LibrarySummary`, `LibraryExportSummary` in `registry/`.
- Update `SearchDoc` signature to accept interfaces.
- Add adapter methods on `compilation.LibraryRegistry` /
  `LibraryExportIndex` if field shapes don't already match.
- Delete `ExtractLibraryRegistry`.
- Drop `machine/compilation` import from registry/.
- **No bench-gate required.** `SearchDoc`'s only callers are
  `repl/registry_doc_provider.go:121` (REPL `,doc`/`,apropos`) and
  `registry/core/prim_reflection.go:411` (Scheme-level `(apropos)`
  / `(doc)`). Both are human-interactive cadence — interface
  indirection at this call site is invisible to users.
- Estimated: ~80 LOC delta (new types + adapter), 1 PR.

**Phase 3 implementation outcome (shipped on
`feat/registry-sr-phase3`)**:
- `registry/search.go` no longer imports `machine/compilation`. New
  registry-side types: `LibraryDoc`, `LibraryExportDoc` (plain DTOs),
  `LibrarySearcher`, `LibraryExportSearcher` (one-method interfaces:
  `AllLibraries()` / `AllLibraryExports()`).
- `SearchDoc` now accepts `LibrarySearcher` / `LibraryExportSearcher`.
  `ExtractLibraryRegistry` deleted.
- Plan-accuracy correction: the plan claimed "`compilation.LibraryRegistry`
  already satisfies `LibrarySearcher`." It cannot. If the interface
  methods return registry-defined DTOs, `compilation` would have to
  import `registry` to satisfy the interface — a dependency cycle
  (`registry → compilation` already exists). The adapter is therefore
  mandatory and lives in `registry/core` (a package that imports both):
  `libraryRegistrySearcher` and `libraryExportIndexSearcher` in
  `prim_reflection.go`, with their own 100%-covered internal test.
- DTOs deal in `string` library names, not a re-created `LibraryName`
  type — `SearchDoc` only ever needs `LibraryName.SchemeString()`.
- Behavior-mechanism change in `searchUnloadedExports`: the
  "skip already-loaded library" check moved from per-entry
  `*LibraryRegistry.Lookup` to a name-set built from
  `LibrarySearcher.AllLibraries()`. Equivalent — the canonical
  Scheme-form string is an injective key for library names. Covered
  by a new test (`TestSearchDoc_UnloadedSkipsLoadedLibrary`).
- Testability win realized: `registry/search_test.go` no longer imports
  `machine/compilation`; library/export cases use in-memory stub
  searchers.
- Actual delta: +311 / −81 across `registry/search.go` (+63/−38),
  `registry/search_test.go` (+83/−42), `registry/core/prim_reflection.go`
  (+55/−1), and `registry/core/prim_reflection_internal_test.go`
  (+110, new). The plan's "~80 LOC delta" estimate did not account for
  the adapter test file or the stub-searcher rewrite of the existing
  search tests.

**Phase 4 — `registrationCategory[T]` (Finding 1 Step 2; optional)**:
- Only if the 7th category arrives, or if the win is otherwise
  motivated.
- Big refactor for ~80 LOC savings + drift-surface 7→1 per category.
- Estimated: 1 PR; bench-gated (the indirection through the
  generic type should be zero-cost but verify).

**Phase 5 — `ArgShape` tightening (Finding 6; deferred)**:
- Couple with extension contracts Phase 2+ work (TODO.md Tier 2).
- Out of scope for this plan.

**Phase 6 — Numeric / contract helper consolidation (Findings 8, 9;
deferred)**: revisit when triggered.

---

## Cross-references

- `plans/2026-05-07-structural-reduction-roadmap.md` — Tier A.3
  (this plan closes the analysis side of A.3).
- `plans/2026-05-08-dispatch-axis-as-data.md` — Phase unification
  (PR #728) addressed Instance B; this plan picks up the remaining
  registry-package instances under the same lens.
- `memory/2026-05-13-values-structural-reduction.md` — Tier A.1
  template + format precedent. Phase 2 (port unification) is the
  direct parallel for Finding 3.
- `memory/2026-05-09-environment-structural-reduction.md` — Tier A.2
  precedent.
- `plans/2026-05-06-machine-structural-reduction.md` — Tier A.0.
- `memory/MEMORY.md` — Architecture Quick Reference.
- `TODO.md` Tier 2 — Extension contracts Phase 2+ (gates Finding 6).
- `TODO.md` Tier 5 — Structural reduction roadmap status.

**After Phases 0-3 ship, Tier A.3 is closed.** Phases 4 (architectural
`registrationCategory[T]`), 5 (`ArgShape` tightening, gated on
extension-contracts Phase 2+), and 6 (numeric/contract helper
consolidation) are explicitly out of scope for this plan's
closeout — they ride on later triggers (7th category, extension
contracts ship, 7th numeric variant). With A.3 closed and A.1 (values/)
+ A.2 (environment/) + A.0 (machine, internal) already shipped, the
roadmap moves to Tier B (`wile/` root API design, `repl/`) under
appropriate lenses.

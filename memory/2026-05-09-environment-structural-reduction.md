# `environment/` package structural reduction

**Date**: 2026-05-09
**Source**: `/structural-reduction ./environment` analysis (Tier A.2 of the
roadmap)
**Status**: Phases 1–9 implemented in PR #730 (branch
`feat/env-structural-reduction`). Phase 10 (LocalIndex allocation audit)
deferred — benchmark-gated per the recommended phasing.
**Priority**: Medium-High (Tier 5 tech debt; closes Tier A of the
`/structural-reduction` roadmap together with `values/` and `registry/`)

## Scope analyzed

`environment/` (11 production files, 2719 LOC; 13 test files):

- `binding.go` (221 LOC) — `Binding` + `BindingMeta`
- `binding_id.go` (49 LOC) — `BindingID` (frame+slot stable identity)
- `binding_type.go` (30 LOC) — `BindingType` enum
- `doc.go` (45 LOC) — package doc
- `environment_frame.go` (825 LOC) — `EnvironmentFrame` (lexical scope node)
- `file_resolver.go` (61 LOC) — `FileResolver`/`LibrarySearcher`/`PathTracker`
  interfaces (defined here to break compilation/sourceload cycles)
- `global_environment_frame.go` (319 LOC) — `GlobalEnvironmentFrame`
- `local_environment_frame.go` (259 LOC) — `LocalEnvironmentFrame`
- `local_index.go` (70 LOC) — `LocalIndex [2]int`
- `namespace.go` (718 LOC) — `Namespace` (per-VM root)
- `phase_registry.go` (167 LOC) — `Phase` + `PhaseRegistry`

External fan-in: 14 packages (`Ca=14`–`16` depending on counting method).
External fan-out: 4 packages (`internal/syntax`, `values`, `werr`,
`security`); none removable.

## Dependency map

```
                     ┌────────────────────────┐
                     │   internal/syntax       │  Scope, SourceContext
                     │   I≈0.18  Ca=14         │
                     └────────────┬───────────┘
                                  │
                     ┌────────────▼───────────┐
                     │       values            │  Value interface
                     │   I≈0.05  Ca=80+        │
                     └────────────┬───────────┘
                                  │
   security ──┐                   │
             ┌▼───────────────────▼───────────────────────┐
   werr ────►│             environment                     │
             │   I≈0.4  Ca=14  Ce=4                         │
             └─────────────────┬───────────────────────────┘
                               │  imported by 14 packages
                               ▼
   wile (root) ◄── machine ◄── machine/compilation ◄── internal/validate
   repl       ◄── extensions ◄── registry ◄── internal/bootstrap …

Internal SCC (intra-package strongly-connected component):

       ┌─────────────────────────────────────────────────────┐
       │                                                     ▼
   ┌───────────┐  .runtime  ┌──────────────────┐  .global   ┌──────────────────────┐
   │ Namespace │ ─────────► │ EnvironmentFrame │ ─────────► │ GlobalEnvironmentFrame│
   │ (14       │ ◄────────  │   (parent chain) │ ◄───────── │   (.namespace DEAD)  │
   │  fields)  │ .namespace │                  │  (orphan   │                      │
   └───────────┘            └────────┬─────────┘   field)   └──────────────────────┘
        ▲                            │ .local                          ▲
        │ .owner                     ▼                                 │
   ┌───────────────┐         ┌──────────────────────┐                  │
   │ PhaseRegistry │         │ LocalEnvironmentFrame │                 │
   │ envs[Phase]   │ ───────►│  (no back-ptrs)       │                 │
   └───────────────┘ .envs[0]└──────────────────────┘                  │
                                                                        │
   GlobalEnvironmentFrame.namespace ─────────── DEAD ────────────────── ┘
                                                aliases EnvironmentFrame.namespace
```

**Observations on the graph**

- The internal SCC `{Namespace, EnvironmentFrame, GlobalEnvironmentFrame}`
  is forced by R7RS first-class environments (the `(environment ...)` form
  returns a `Namespace`, while `eval` accepts an `EnvironmentFrame` walked
  from a `Namespace`). The cycle is irreducible at the language level — but
  one of its three edges (`GlobalEnvironmentFrame.namespace`) is dead and
  removable (Finding 1).
- No external cycles. Imports flow strictly downward to
  `internal/syntax` and `values`. Both `werr` and `security` are leaves.
- `FileResolver` / `LibrarySearcher` / `PathTracker` interfaces defined here
  (not where their implementations live) is a deliberate **Dependency
  Inversion** — necessary to keep `environment/` free of imports from
  `machine/compilation/`. Documented in `file_resolver.go:25-30`. Worth
  preserving.
- Package instability `I ≈ 0.4` (4 imports out of 10 likely). With `Ca=14`,
  the SDP balance is correct: many depend on it, it depends on little.

## Findings

### Finding 1 — `GlobalEnvironmentFrame.namespace` is a dead field

**Principle**: State Tightness (representation invariant); Dependency
minimization (cycle reduction)
**Where**: `global_environment_frame.go:88` (declaration);
`phase_registry.go:134`, `namespace.go:571` (writes);
`global_environment_frame.go:114` (propagation by `Copy`); 0 reads anywhere
in the workspace.
**Theory**: A field written but never read is a **phantom dependency** that
couples writers to nonexistent readers (Liskov & Guttag,
*Program Development in Java*, §5: representation invariants must constrain
*reachable* state, not merely *allocated* state). Equivalently, this is a
**value-alias** with `EnvironmentFrame.namespace` — both fields point to
the same `*Namespace` at every program point, so by common-subexpression
elimination they are one variable with two names.
**Current state**: Verified by grep — `GlobalEnvironmentFrame.namespace`
is set in `phase_registry.createPhaseEnv` and `Namespace.NewSchemeReportNamespace`
and propagated by `GlobalEnvironmentFrame.Copy`. There are no readers
anywhere in the workspace.
**Problem**: 8 bytes per global frame plus an unenforced aliasing
obligation: `Namespace.runtime.global.namespace == Namespace.runtime.namespace == Namespace`.
Nothing prevents these from drifting if a new constructor forgets to set
both. The dead field also adds an edge to the intra-package SCC (see the
diagram above).
**Proposed direction**:
1. Delete the `namespace *Namespace` field from `GlobalEnvironmentFrame`.
2. Remove `global.namespace = p.owner` at `phase_registry.go:134`.
3. Remove `copiedGlobal.namespace = q` at `namespace.go:571`.
4. Drop the field from `Copy` at `global_environment_frame.go:114`.
5. Update `newGlobalEnvironmentFrameForNamespace` to stop setting it.
6. Update the diagram in `environment_frame.go:55-62` to remove the
   `namespace ──── *Namespace` line on the `GlobalEnvironmentFrame` box.
**Impact**: One field deleted, one edge of the intra-package SCC
eliminated, one unenforced aliasing invariant retired.
**Estimated size**: XS (mechanical deletion + tests; only one defensive
test reads the field for verification, if any).

### Finding 2 — Dead `EqualTo`/`SchemeString`/`IsVoid` cluster on four env types

**Principle**: Composability — abstractions must pay rent
**Where**: `binding.go:178-201` (`SchemeString`/`IsVoid`/`EqualTo`),
`local_environment_frame.go:160-188`, `global_environment_frame.go:269-319`,
`environment_frame.go:790-820`. Only `Namespace` carries the
`var _ values.Value = (*Namespace)(nil)` check at `namespace.go:38`.
**Theory**: Wadler parametricity (Wadler, "Theorems for Free!", ICFP 1989)
— an interface conformance that is never witnessed at a call site adds no
semantic constraint, only an implementation tax. Verified by grep:
`Binding.EqualTo`, `LocalEnvironmentFrame.EqualTo`, `GlobalEnvironmentFrame.EqualTo`,
`EnvironmentFrame.EqualTo` have **zero external callers**. The
`EnvironmentFrame.EqualTo` method recursively walks the parent chain *and*
global keys (line 819) — a structural equality semantically wrong for the
type's role: under R7RS §6.12 environments compare by `eq?`, not by
binding-set equality. The dead method is a latent correctness landmine.
**Current state**: ~100 LOC of `EqualTo`/`SchemeString`/`IsVoid` methods
across four types, none called from outside `environment/` and most not
called at all. Only `Namespace`, `LocalIndex`, and `GlobalIndex` actually
flow through Scheme value plumbing.
**Problem**: Dead code with cognitive cost; `EnvironmentFrame.EqualTo` is
also semantically incorrect — a future caller trusting it would get the
wrong answer. The four types informally implement `values.Value` without
the compile-time check, so a future refactor of the `values.Value`
interface might silently break them.
**Proposed direction**:
1. Delete `EqualTo`, `IsVoid`, `SchemeString` from `Binding`,
   `LocalEnvironmentFrame`, `GlobalEnvironmentFrame`, `EnvironmentFrame`.
2. If any debugging needs a string form, keep an internal Go-side
   `String() string`. None of the `SchemeString` strings (e.g.,
   `"#<binding>"`) are observed by Scheme programs since none of these
   types reach `display`/`write`.
3. Retain `EqualTo`/`IsVoid`/`SchemeString` on `Namespace`, `LocalIndex`,
   `GlobalIndex` (these *are* used as Scheme values).
**Impact**: ~100 LOC removed. The package's *de facto* `values.Value`
implementations shrink from 5 types to 3 — matching the actual consumer
set. Removes one semantic correctness landmine
(`EnvironmentFrame.EqualTo`).
**Estimated size**: S (mechanical deletion; touches 4 files).

### Finding 3 — `Binding.SetBindingType` is unused

**Principle**: Composability — return values nobody uses, parameters
nobody passes
**Where**: `binding.go:90`
**Theory**: Inverse of Wadler parametricity — a function whose effect
cannot be observed contributes no constraint to the system. Verified by
grep: declared in `binding.go`, never called from anywhere in the
workspace (including the package's own tests).
**Current state**: 4 LOC; one accessor with no consumers.
**Proposed direction**: Delete.
**Impact**: 4 LOC; reduces mutator surface on `Binding` from 7 setters to
6 (further reduced by Finding 4 below).
**Estimated size**: XS.

### Finding 4 — `BindingMeta` accessor proliferation (10 boilerplate methods)

**Principle**: Composability — hand-unrolled patterns; State tightness —
optional product type
**Where**: `binding.go:96-175` (10 methods sharing the same lazy-init
shape over 5 metadata fields)
**Theory**: `BindingMeta { Scopes, Source, Doc, Imported, Constant }` is
an optional product type — most bindings populate 1–2 fields, never all 5.
Each setter duplicates the same 4-line pattern verbatim:
```go
// repeated 5×
func (p *Binding) SetX(v T) {
    if p.meta == nil { p.meta = &BindingMeta{} }
    p.meta.X = v
}
```
This is a **hand-unrolled loop** over field names: the irreducible
operation is "ensure meta exists, then assign field X." The split between
hot-path fields (`value`, `bindingType`) and cold metadata is justified
(comment at `binding.go:23` documents the 32 vs 56 byte difference). The
*accessor proliferation* is not.
**Current state**: 10 methods (5 getters + 5 setters), ~60 LOC of
mechanical accessors. External usage is sparse:
- `IsImported` / `SetImported`: 4 sites in `machine/compilation/`
- `IsConstant` / `SetConstant`: 1 site in `machine/compilation/`
- `Doc` / `SetDoc`: 2 setter sites (`engine.go:479`, `registry/apply.go:201`)
- Everything else internal.
**Problem**: Every accessor is a place a future bug can hide (forget the
lazy init; return wrong default; etc.). Adding a new metadata field
requires editing 3 sites (struct, getter, setter). The pattern is
identical across the 5 fields.
**Proposed direction** — pick one based on call-site preference:
- (a) **Expose `Meta()` directly**:
  ```go
  func (p *Binding) Meta() *BindingMeta             // nil if absent
  func (p *Binding) EnsureMeta() *BindingMeta       // lazily allocates
  ```
  Callers do `b.EnsureMeta().Imported = true`. Eliminates the 10
  accessors.
- (b) **Generic helper**:
  ```go
  func setMetaField[T any](b *Binding, set func(*BindingMeta, T), v T) {
      if b.meta == nil { b.meta = &BindingMeta{} }
      set(b.meta, v)
  }
  ```
  Less ergonomic for Go callers but more uniform.
Preference: (a). Go idiom favors direct field access on small records.
**Impact**: ~50 LOC removed; one place to add a new metadata field
(struct field only) instead of three.
**Estimated size**: S (call-site update is mechanical:
~7 sites in `machine/compilation/`, 2 in root).

### Finding 5 — "Delegate to root" pattern hand-unrolled 7 times in `Namespace`

**Principle**: Composability — same verbs, not same nouns
**Where**: `namespace.go` lines 213, 223, 263, 322, 335, 380, 396
(`FileResolver`, `SetFileResolver`, `LoadPathStack`, `RegisterLibraryScope`,
`LookupLibraryEnv`, `ExportIndex`, `SetExportIndex`)
**Theory**: Each method opens with the same shape:
```go
if p.parent != nil { return p.parent.X(...) }
// then operate on receiver
```
This is the **transition from enumeration to induction** — the pattern is
identical, only the operation varies. By **functor composition**, the
right primitive is "find the root, then run the operation there." In Go
terms: a `root() *Namespace` helper plus direct field access.
**Current state**: 7 methods carrying the same parent-chain prologue.
Inconsistency hazard: `Authorizer()`, `Registry()`, `LibraryRegistry()`
*do not* delegate (they're snapshotted at child construction in
`NewChildNamespace` — `namespace.go:537-548`). The reader cannot tell
from the API which model applies. CLAUDE.local.md:262-264 documents
exactly the kind of bug this asymmetry can hide ("LoadPathStack is
per-VM, not per-environment"; "envMap is capability state — derived
namespaces must not silently widen capability").
**Problem**: Two failure modes have already cost real bugs:
1. Forgetting to add the prologue when adding a new field — silent
   capability widening (child operates on its own nil field instead of
   inheriting parent authority).
2. Inconsistency between snapshot fields and delegate fields — readers
   guess the wrong policy when adding new code.
**Proposed direction**:
1. Extract:
   ```go
   func (p *Namespace) root() *Namespace {
       for p.parent != nil { p = p.parent }
       return p
   }
   ```
2. Rewrite the 7 methods as `p.root().field` direct accesses, dropping
   the recursion.
3. Audit the snapshot-at-construction fields and document them as
   "captured" vs "delegated" explicitly in a new doc-comment block above
   the `Namespace` struct. The implicit policy is the bug source.
**Impact**: ~30 LOC removed; the delegation policy becomes a single
function; new fields' authors face a binary choice (snapshot or delegate)
instead of guessing the existing pattern.
**Estimated size**: S (mechanical replacement + 1 new doc block).

### Finding 6 — Maximal-resolution reducer duplicated in `GetBinding` and `GetLocalIndex`

**Principle**: Composability — multi-purpose abstractions; fold over the
parent chain
**Where**: `environment_frame.go:451-477` (`GetBinding` scoped path),
`environment_frame.go:545-565` (`GetLocalIndex` scoped path)
**Theory**: Both methods perform the same fold: walk all scope-compatible
candidates, track the best by `scopeCount`, with early-exit when a perfect
match is found (`scopeCount == len(scopes)`). This is a **monoid** with
identity "no candidate yet" and binary op "take the one with larger
`scopeCount`, prefer the new one when tied" — plus a kill switch for
perfect matches. Per Bird & de Moor (*Algebra of Programming*, 1997),
folds are the canonical primitive for this shape.
**Current state**: Two near-identical 30-line blocks differing only in
the `T` of the candidate (`*Binding` vs `*LocalIndex`).
```go
type candidate struct { item T; scopeCount int }
var best candidate
visit(func(...) {
    sc := len(binding.Scopes())
    if sc > 0 && sc == len(scopes) { best = ...; return true }
    if best.item == nil || sc > best.scopeCount { best = ... }
    return nil
})
```
The Flatt-resolution semantics (most-specific binding wins) are encoded
twice. Future changes to the rule (e.g., handling tied scope counts
differently, or adding a tie-breaker) must be applied in both places.
**Problem**: Two implementations of one rule. CLAUDE.md "Refactoring":
*"Inconsistency = incomplete thinking."*
**Proposed direction**: Extract a generic helper (Go 1.18+ generics
already used in the workspace):
```go
type bestOf[T any] struct {
    item       T
    scopeCount int
    has        bool
}
func (b *bestOf[T]) consider(item T, scopeCount, target int) (done bool) {
    if scopeCount > 0 && scopeCount == target {
        *b = bestOf[T]{item, scopeCount, true}
        return true
    }
    if !b.has || scopeCount > b.scopeCount {
        *b = bestOf[T]{item, scopeCount, true}
    }
    return false
}
```
Plug into both `GetBinding` (`T = *Binding`) and `GetLocalIndex`
(`T = *LocalIndex`). One definition, two consumers.
**Impact**: ~40 LOC removed; one place to change Flatt-resolution
semantics; opens the door to a third caller (cross-phase, cross-library
lookups) using the same primitive.
**Estimated size**: S.

### Finding 7 — Five Namespace constructors plus four internal helpers

**Principle**: Composability — factor out common structure
**Where**: `namespace.go:136` (`NewNamespace`), `:537` (`NewChildNamespace`),
`:558` (`NewSchemeReportNamespace`), `:584` (`NewChildRuntime`),
`:610` (`Derive`), `:620` (`DeriveWith`); helpers at `:675`, `:685`,
`:697`, `:711`. `environment_frame.go:116` (deprecated
`NewNamespaceFrame`).
**Theory**: 5 named public variants for what is essentially
`New(parentOrNil, copyParentBindings, registry, authorizer)`.
| Constructor              | parent | bindings   | registry/auth         | returns                 |
|--------------------------|--------|------------|-----------------------|-------------------------|
| `NewNamespace`           | no     | n/a        | n/a                   | fresh `*Namespace`      |
| `NewChildNamespace`      | yes    | empty      | shared ptr            | child `*Namespace`      |
| `NewSchemeReportNamespace` | yes  | snapshot   | shared ptr            | child `*Namespace`      |
| `NewChildRuntime`        | n/a    | empty      | n/a (shares Ns)       | `*EnvironmentFrame`     |
| `Derive`                 | yes    | empty      | shared ptr            | child `*Namespace`      |
| `DeriveWith`             | yes    | empty      | overridable           | child `*Namespace`      |
`Derive` and `NewChildNamespace` are equivalent up to no-op; `DeriveWith`
is `NewChildNamespace` plus optional overrides. This is **product-type
variant proliferation**.
**Current state**: 5 public constructors + 4 internal helpers. Two
existing TODOs at `namespace.go:535-536` flag exactly this:
*"review whether libraryRegistry should be copied here"* and
*"review for optimization/refactoring opportunities."*
**Problem**: Adding a new state field means deciding "what should each
constructor do with it?" — 5 decisions, not 1. Recently added `envMap`
was added to `NewChildNamespace` and `NewSchemeReportNamespace` but
*omitted from `Derive` and `DeriveWith`* directly — flow works only
because they call `NewChildNamespace` internally. If a future
`DeriveWith` short-circuit ever stops calling `NewChildNamespace`,
capability inheritance breaks silently.
**Proposed direction**: Reduce to two primary constructors plus one
specialty:
1. `NewNamespace(opts ...NamespaceOption)` — fresh root.
2. `(p *Namespace).NewChild(opts ...NamespaceOption)` — derived child;
   absorbs `NewChildNamespace`, `Derive`, `DeriveWith`.
3. Keep `NewSchemeReportNamespace` (or rename to `(p).Snapshot()`) — the
   "snapshot parent's globals at fork time" semantics is genuinely
   distinct.
4. Keep `NewChildRuntime` but rename to `(p).NewSharedRuntime()` to
   make the "shares Namespace" property visible. It returns
   `*EnvironmentFrame`, not `*Namespace`, so it's a different abstraction
   despite the family name.
5. Resolve and delete the two existing TODOs at `namespace.go:535-536`.
**Impact**: 5 public constructors → 3; one consistent option mechanism
for inheritance overrides; resolves both TODOs; eliminates the latent
"new field forgotten in one variant" bug class.
**Estimated size**: S–M (call-site sweep is mechanical but spans
~5 packages).

### Finding 8 — `EnvironmentFrame` is a wide delegation surface to `Namespace`

**Principle**: Dependency minimization — Stable Dependencies Principle,
Interface Segregation
**Where**: `environment_frame.go:306-348` —
`FileResolver`, `SetFileResolver`, `LibraryRegistry`,
`SetLibraryRegistry`, `LoadPathStack`
**Theory**: Each method is a 3-line pass-through to `p.namespace.X(...)`
with a nil guard. Callers heavily use these (~15 sites in
`machine/compilation/`, `extensions/`, `registry/`). They could call
`p.Namespace().X()` directly — the wrappers add no semantics, only an
extra method on `EnvironmentFrame`. By **Interface Segregation** (Martin,
*Clean Architecture*, ch. 11), `EnvironmentFrame`'s API surface should
reflect its *primary role* (lexical scope + binding lookup), not
"everything Namespace can do, replayed here."
**Current state**: `EnvironmentFrame` exposes 5 cross-cutting
capabilities (file resolver, library registry, load path) by passthrough.
`Namespace` exposes the same 5. Callers cannot tell which is "right" to
call from the type signature alone.
**Problem**: This is **API-surface boolean blindness** — the type of
`*EnvironmentFrame` claims it can do file resolution and library
registration, but it actually delegates. Readers can't tell from the type
where state lives. Worsens the symptom Finding 5 already addresses
(snapshot vs delegate confusion).
**Proposed direction** — two paths:
- (a) **Demote** the 5 methods. Mark them deprecated; route existing
  callers to `frame.Namespace().X()`. Removes the appearance that
  `EnvironmentFrame` owns this state.
- (b) **Keep as ergonomic shorthand**, document the delegation
  prominently, and explicitly mark "owned by `Namespace`." Callers
  benefit from `p.env.FileResolver()` being shorter than
  `p.env.Namespace().FileResolver()`.
Preference: (b) for now (low cost, real ergonomics) — but pair with
Finding 5's documentation upgrade so the reader can see the policy
without reading source.
**Impact**: Either ~5 fewer methods on `EnvironmentFrame` (a) or
sharper documentation (b). Either way, the ownership model becomes
explicit.
**Estimated size**: XS (b) or S (a).

### Finding 9 — `BindingType` carries a transient `Unknown` state

**Principle**: State Tightness — make illegal states unrepresentable
**Where**: `binding_type.go:21-30` (declaration);
`local_environment_frame.go:44, 130` (transient use)
**Theory**: `BindingTypeUnknown = 0` exists *only* to be the zero-value
of pre-allocated slots in `NewLocalEnvironment`. Once the slot is bound,
the type becomes Variable/Syntax/Primitive; `Unknown` is never observed
by `GetBinding`. The state space of *observed* binding types is
`{Variable, Syntax, Primitive}` (3) but the type permits 4. **Type
precision = 3/4 = 75%.** Per Yaron Minsky, "Effective ML" — `Unknown`
is internal scaffolding that has leaked into the public type.
**Current state**: `BindingType` is exported; all 4 constants exported;
`Unknown` is checked nowhere outside the env package itself.
**Proposed direction** — pick one:
- (a) Keep `Unknown`; document it as
  "internal pre-allocation marker; never observed externally." Cheap. No
  behavior change.
- (b) Remove `Unknown`; pre-allocated slots use a separate sentinel
  (e.g., a nil-binding pointer marker on `LocalEnvironmentFrame.bindings[i]`).
  Tighter type, more invasive change.
Preference: (a). The cost-benefit doesn't justify (b) unless touching
`LocalEnvironmentFrame.bindings` for other reasons.
**Impact**: Documentation clarity (a) or one impossible state eliminated
(b).
**Estimated size**: XS (a) or M (b).

### Finding 10 — `LocalIndex *[2]int` allocation is partially worked around

**Principle**: State Tightness — type precision; Composability —
sort-package abstraction already discovered
**Where**: `local_index.go:32` (`type LocalIndex [2]int`); hot-path
alternatives at `environment_frame.go:620, 653`
(`GetLocalBindingBySlotDepth`, `SetLocalValueBySlotDepth`)
**Theory**: `[2]int` on 64-bit = 16 bytes. Slot indexes are bounded by
frame size (typically <100); depth is bounded by lexical nesting
(typically <20). Practical entropy: `log₂(100) + log₂(20) ≈ 11 bits`.
The packing isn't the problem — it's that `NewLocalIndex` returns
`*LocalIndex` (heap-allocated 16 bytes) and the methods use pointer
receivers. The hot path already worked around this by exposing
`GetLocalBindingBySlotDepth(slot, depth int)` — the **sort-package
abstraction** discovered after the fact: the irreducible operation is
"(slot, depth) → binding" without packaging.
**Current state**: Two parallel APIs:
- `*LocalIndex`-based (general use): ~40 sites in `machine/`,
  `internal/validate/`.
- `slot, depth int`-based (hot path): 2 methods in `environment_frame.go`.
The unboxed path covers `OpLoadLocal`/`OpStoreLocal` dispatch;
non-hot-path callers still allocate.
**Proposed direction**: Audit the 40 occurrences of
`environment.NewLocalIndex` outside `environment/`. Migrate
non-hot-path callers to `slot, depth` where straightforward. If most
can, demote `*LocalIndex` to a debug-print convenience and remove
`NewLocalIndex` from the public API. A reasonable interim: keep
`LocalIndex` as a value type (drop the pointer receivers); the
allocation pressure comes from the `*LocalIndex` indirection, not the
struct itself.
**Impact**: Reduces allocations on a previously-instrumented path;
collapses two parallel APIs into one. Magnitude depends on call-site
distribution — measure first.
**Estimated size**: M (call-site sweep, benchmark verification).

## Opportunities (sort-package style)

### Opportunity 1: `bestOf[T]` — Flatt-style maximal-resolution reducer

- **Replaces**: 2 candidate-tracking blocks in `GetBinding` and
  `GetLocalIndex` (Finding 6). Possibly a third in any future scope-aware
  lookup.
- **Core operation**: Walk a sequence of `(item, scopeCount)` pairs,
  return the one with the largest `scopeCount`, with early-exit when
  `scopeCount == len(targetScopes)` (perfect match).
- **Algebraic structure**: A **fold** with an early-termination
  predicate. Identity = "no candidate yet"; binary op = "take the one
  with larger `scopeCount`, prefer the new one when tied." Associative
  on the (max scopeCount, last-wins) tuple — a left-leaning **monoid**
  with a kill switch.
- **Proposed shape**: see Finding 6.
- **Reuse sites**: `GetBinding` scoped path; `GetLocalIndex` scoped path;
  `GetGlobalIndexFromLibraryScopes`, `GetGlobalIndexAcrossPhases` (if
  they ever need maximal selection); future cross-phase or cross-library
  scope-aware lookups.

### Opportunity 2: `Namespace.root()` — terminate the parent chain once

- **Replaces**: 7 methods that prepend
  `if p.parent != nil { return p.parent.X(...) }` (Finding 5).
- **Core operation**: Walk parent pointers to the root namespace.
- **Algebraic structure**: An **idempotent fixed-point**:
  `root(root(p)) = root(p)`. Equivalently, finding the canonical
  representative of an equivalence class under "shares-syntax-interning-with."
- **Proposed shape**:
  ```go
  func (p *Namespace) root() *Namespace {
      for p.parent != nil { p = p.parent }
      return p
  }
  ```
  Then: `p.fileResolver` → `p.root().fileResolver`;
  `p.SetX(v)` → `p.root().x = v`. Centralizes the "root owns this state"
  policy.
- **Reuse sites**: All 7 current delegators; future fields that need
  root-canonical access (e.g., metrics, request IDs).

### Opportunity 3: `Namespace` field policy table — capture vs. delegate

- **Replaces**: Implicit knowledge that `Authorizer` / `Registry` /
  `LibraryRegistry` / `envMap` are *captured* at child construction
  while `FileResolver` / `LoadPathStack` / `ExportIndex` are *delegated*
  to root.
- **Core operation**: Document and enforce the inheritance model per
  field. Each field is either a **snapshot** (immutable copy at fork
  point) or a **lens into root** (live read). These have different
  equational properties: snapshots commute with later mutations,
  delegations don't.
- **Proposed shape**: At minimum, add a comment block at the top of
  `Namespace` documenting each field's inheritance policy. If you go
  further, a dual-table doc (`Captured: [Registry, Authorizer, …]`
  vs `Delegated: [FileResolver, LoadPathStack, …]`) catches future
  "I added a field and forgot to inherit it" bugs at code-review time.
- **Reuse sites**: Future fields gain a clear policy decision instead
  of pattern-matching on existing precedent.

### Opportunity 4: Unified `NewNamespace`/`NewChild` constructors

- **Replaces**: 5 public Namespace constructors (Finding 7).
- **Core operation**: Construct a namespace with optional inheritance
  overrides.
- **Algebraic structure**: Optional product of fields, expressed as a
  closure-pipeline rather than position-tuple chain. Same idiom Wile
  already uses for `wile.Engine` (`WithProfile`, `WithSandbox`,
  `WithSourceFS`, etc.) and for which Finding 6 of
  `2026-05-07-internal-structural-reduction.md` recommended `match.Matcher`.
- **Proposed shape**:
  ```go
  type NamespaceOption func(*Namespace)

  func WithRegistry(r any) NamespaceOption
  func WithAuthorizer(a security.Authorizer) NamespaceOption
  func WithSnapshotOfBindings() NamespaceOption  // formerly NewSchemeReportNamespace

  func NewNamespace(opts ...NamespaceOption) *Namespace
  func (p *Namespace) NewChild(opts ...NamespaceOption) *Namespace
  ```
  `NewChildRuntime` stays separate because it returns
  `*EnvironmentFrame`; rename to `(p).NewSharedRuntime()` for clarity.
- **Reuse sites**: 5 public constructors collapse to 2 + 1 specialty.
  Adding a new inheritance dimension becomes one new option function
  instead of editing 5 constructors.

## What's already done well (preserve)

Several pieces of architecture are textbook good and should be preserved
or imitated when addressing the findings:

1. **`FileResolver` / `LibrarySearcher` / `PathTracker` interfaces
   defined in `environment/`** despite their implementations living in
   `machine/compilation/resolver/` (`file_resolver.go:25-30`). This is
   proper **Dependency Inversion**: the abstract interface lives with
   the consumer (`Namespace`), the concrete impls live with the
   producer. Breaks what would otherwise be a circular import.

2. **`Phase` typed enum with named constants** (`phase_registry.go:46-69`).
   Avoids the "magic number" trap that would have arisen from
   `int8`-keyed phase access. The `String()` and `Compare()` methods
   make the enum first-class. The companion `registry.PhaseSet` bitset
   for primitive registration is a nice second-tier abstraction over
   the same value space.

3. **`PhaseRegistry` thread-safety with double-checked locking**
   (`phase_registry.go:104-127`). RLock fast path; Lock slow path with
   re-check. Textbook execution of a concurrent-init pattern that
   appears throughout the codebase (also seen in
   `Namespace.InternSyntax` at `namespace.go:159-178`).

4. **`LocalEnvironmentFrame`'s copy-on-write keys map**
   (`local_environment_frame.go:33, 87-89, 105-126, 192-208`). Marked
   `keysShared bool`; `EnsureLocalBinding` clones before mutating.
   Avoids the cost of cloning maps for the (common) case of
   non-mutating copies. Documented as an optimization, not folklore.

5. **Pre-allocation + reuse contract on `EnvironmentFrame` for the
   pool** (`environment_frame.go:203-238`). `ResetForPool`,
   `PreAllocateBindings`, `LocalBindingsSlice` work together to keep
   pooled frames' backing arrays live across cycles, eliminating the
   `make([]Binding, n)` that dominated allocation profiles. Comments
   document *why* the dance is needed.

6. **`BindingID` as the local analog of `GlobalIndex`**
   (`binding_id.go:25-49`). `(*LocalEnvironmentFrame, slot)` provides
   stable identity even when the underlying `[]Binding` slice is
   reallocated by `append`. Used by validate/ for capture and escape
   analysis. The asymmetry with `LocalIndex [2]int` (which is *not*
   stable across slice growth — see local_environment_frame.go:80-85) is
   intentional and documented.

7. **The `Namespace` doc-comment ASCII diagram for parent vs. shared
   semantics** (`namespace.go:412-528`). The two side-by-side diagrams
   for `NewChildRuntime` vs `NewChildNamespace` make a subtle
   distinction visible. Imitate this presentation when documenting
   future inheritance variants.

## Closing summary

**State-space**: Of the high-traffic types examined,
- `LocalEnvironmentFrame` and `EnvironmentFrame` track their semantic
  state tightly (no dead branches in production after Finding 1).
- `Binding` is the worst offender: 5 optional metadata fields × 10
  accessors give a wide surface for a structurally simple type.
  Finding 4 collapses this.
- `BindingType` has 75% precision (3 valid out of 4 representable).
  Finding 9 documents or removes the unused `Unknown` state.
- `Namespace` has 14 fields with a *split* inheritance model
  (snapshot vs delegate). Type precision is fine; the *policy* is
  implicit. Findings 5 + 7 + Opportunity 3 make the policy explicit.

**Dependency count**: 4 external imports (`internal/syntax`, `values`,
`werr`, `security`); none removable. Internal SCC of size 3 — one of
its three edges (`GlobalEnvironmentFrame.namespace`) is dead and
removable (Finding 1). Instability: package `I ≈ 0.4` with `Ca=14`
matches the SDP balance.

**Top 3 highest-impact changes**:

1. **Delete `GlobalEnvironmentFrame.namespace`** (Finding 1) — removes
   a dead field, breaks one edge in the internal SCC, eliminates an
   unenforced aliasing invariant. Tiny diff, high signal-to-noise.
2. **Delete the four dead `EqualTo`/`SchemeString`/`IsVoid` clusters**
   (Finding 2) — drops ~100 LOC of dead methods, cleanly separates
   "Scheme value types" (`Namespace`, `LocalIndex`, `GlobalIndex`)
   from "internal data types" (the rest). Removes one semantic
   correctness landmine.
3. **Extract `Namespace.root()`** (Finding 5 / Opportunity 2) and
   document field inheritance policy (Opportunity 3) — collapses a
   hand-unrolled pattern, makes the snapshot-vs-delegate choice
   explicit, resolves the existing TODOs at `namespace.go:535-536`.

## Recommended phasing

Sequence from highest impact-per-effort to lowest:

| Phase | Finding(s)        | Size  | Gating                                              |
|-------|-------------------|-------|-----------------------------------------------------|
| 1     | 1                 | XS    | None (dead-field delete)                            |
| 2     | 3                 | XS    | None (dead-method delete)                           |
| 3     | 9 (option a)      | XS    | None (doc-only)                                     |
| 4     | 2                 | S     | None (mechanical deletion of 4 dead clusters)       |
| 5     | 5 + Opp 2 + Opp 3 | S     | None (extract `root()`, doc field policy)           |
| 6     | 6 + Opp 1         | S     | None (extract `bestOf[T]` reducer)                  |
| 7     | 4                 | S     | None (or after Finding 2 to avoid touching same file twice) |
| 8     | 7 + Opp 4         | S–M   | After Finding 5 (so `root()` exists for new code)   |
| 9     | 8 (option b)      | XS    | After Finding 5 (documentation upgrade is unified)  |
| 10    | 10                | M     | None (benchmark-gated; defer if no allocation win)  |

Phases 1–4 are independent dead-code/dead-state cleanup; do them in any
order. Phase 5 unblocks Phase 8's option-function refactor. Phase 10 is
the only one with measurement risk; treat as opportunistic.

## Cross-references

- `plans/2026-05-07-structural-reduction-roadmap.md` — closes Tier A.2.
  Tier A.1 (`values/`) and A.3 (`registry/`) analyses already complete
  (`plans/2026-05-08-dispatch-axis-as-data.md`); Tier A is now done.
  The `machine/` and `internal/` implementation plans can now reference
  this analysis when sequencing their Phase 7's
  (`MachineContext` sub-record extraction depends on whether
  `EnvironmentFrame` is reshaped).
- `plans/2026-05-06-machine-structural-reduction.md` — Finding 7 (named
  sub-records for `MachineContext`) interacts with how
  `EnvironmentFrame` is held. Sequence after Phase 8 of this plan to
  absorb constructor consolidation.
- `plans/2026-05-07-internal-structural-reduction.md` — Finding 6
  (option-function constructor for `match.Matcher`) is the *same*
  pattern as Opportunity 4 here (option-function constructor for
  `Namespace`). Apply once, reuse the pattern; both pieces of code
  align on Wile's existing `wile.Engine` idiom.
- `plans/2026-05-08-dispatch-axis-as-data.md` — cross-package
  defunctionalization findings. `environment/` does not exhibit this
  pattern (the `Phase` enum + `PhaseRegistry.envs map[Phase]…` already
  treats the dispatch axis as data — see "What's already done well",
  item 2).
- `environment/CLAUDE.local.md` — architectural reference; Finding 5
  draws on the documented capability-state policy for `LoadPathStack`
  and `envMap`.
- `namespace.go:535-536` — two existing TODOs that Finding 7 / Opportunity
  4 resolve.
- TODO.md Tier 5 "FCA-Derived" — the sibling
  `LocalEnvironmentFrame pointer ambiguity` item touches the same type
  family; sequencing is independent.

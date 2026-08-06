# Environment System

This document describes Wile's environment system, which manages variable bindings, syntax interning, and the phase hierarchy for macro expansion.

---

## Overview

The environment system has four key types organized in a hierarchy:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                            Namespace                                    │
│  (Per-VM instance: owns syntax interning, phases, libraries)            │
│                                                                         │
│  syntaxInterns ──── map[Value]SyntaxValue (thread-safe, per-instance)   │
│  phases ─────────── *PhaseRegistry                                      │
│  libraryRegistry ── LibrarySearcher (*compilation.LibraryRegistry)      │
│  runtime ────────── *EnvironmentFrame (the ROOT view: phase 0, mutable) │
│  sealedWriteRoot ── *EnvironmentFrame (phase-0 SEALED-WRITE view)       │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ owns
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         EnvironmentFrame                                │
│  (A VIEW over the owner's one store, not a node in a parent graph)      │
│                                                                         │
│  parent ─────────── *EnvironmentFrame (LEXICAL parent only; nil at any  │
│                      structural root — every phase/sealed-write view)   │
│  local ──────────── LocalEnvironmentFrame (value; keys==nil → none)     │
│  global ─────────── *GlobalEnvironmentFrame (the owner's ONE store)     │
│  phaseLevel ─────── Phase (-1=template, 0=runtime, 1=expand, 2=compile) │
│  rank ───────────── writeRank (mutable | sealed — which tier this       │
│                      view's writes land in)                             │
│  phases ─────────── *PhaseRegistry (shared reference)                   │
│  namespace ──────── *Namespace (back-reference)                         │
└─────────────────────────────────────────────────────────────────────────┘
          │                                    │
          │ contains                           │ contains
          ▼                                    ▼
┌───────────────────────────┐    ┌────────────────────────────────────────┐
│  LocalEnvironmentFrame    │    │      GlobalEnvironmentFrame            │
│  (Single scope bindings)  │    │  (the owner's whole binding store)     │
│                           │    │                                        │
│  keys ── map[Symbol][]int │    │  keys ── map[Symbol][]slotRef          │
│  bindings ── []Binding    │    │  bindings ──── []*Binding              │
│                           │    │  slotRef: {slot, phase PhaseKey,       │
│                           │    │            sealed bool}                │
└───────────────────────────┘    └────────────────────────────────────────┘
```

Every phase or sealed-write **view** is a *structural* root (`parent == nil`):
none of them chains to another view. `EnvironmentFrame.parent` is exclusively
the *lexical* chain — `lambda`/`let`/`letrec` nesting — which is why the type's
own doc comment calls it "a VIEW over the owner's one store, not a node in a
parent graph." What used to be inter-frame inheritance (a phase-1 frame's
parent pointing at a sealed base) is now two `slotRef` fields, `phase` and
`sealed`, living in the STORE's own key map. See [Sealed Coordinates and the
Ranked Probe](#invariants) below.

---

## Namespace

The `Namespace` is the root of the environment hierarchy. An engine has exactly one
*root* Namespace, and every first-class environment a program constructs is another
one: `(environment …)`, `(null-environment)`, `(make-namespace)`,
`(scheme-report-environment)`, and `(environment '(wile <profile>))` each mint a
child. A Namespace owns:

- **Syntax interning table**: Caches syntax objects for consistent identity across macro expansion
- **Phase registry**: O(1) access to any phase view, plus the sealed-write views registration targets
- **One binding store**: every phase's bindings, at every registration rank, in one scope-keyed `GlobalEnvironmentFrame`
- **Library registry**, **module-instance cache**, **primitive registry**, **authorizer**

### Namespace Kinds

Four constructors, and the differences between them are not cosmetic:

| Constructor | Store's sealed tier | Runtime view | Scheme surface |
|---|---|---|---|
| `NewNamespace()` | empty until bootstrap applies the registry | own | the engine root |
| `NewChildNamespace()` | own store, **stays empty** | own, empty | `(environment …)`, `(null-environment)`, `(make-namespace)`, `namespace-derive` |
| `NewSchemeReportNamespace()` | own store, a **copy** of the parent's | own, a copy of the parent's runtime | `(scheme-report-environment)` |
| `NewChildRuntime()` | own store, own sealed tier | is itself | not first-class — library loading only |

The empty sealed tier under `NewChildNamespace` is what splits the two
`(environment …)` forms apart. An import-spec environment copies the imported
bindings into the child's **mutable runtime**, so they are ordinary user bindings;
a profile environment routes a curated registry apply through the child's
**sealed-write view** (`NewProfileEnvironment`, `pkg/internal/bootstrap/bootstrap.go`),
landing those bindings in the sealed tier of the child's own store, so the
same names are immutable there:

```scheme
(namespace-undefine! (environment '(scheme base)) 'car)  ; succeeds
(namespace-undefine! (environment '(wile console)) 'car)
;; => namespace-undefine!: cannot undefine sealed binding "car"
```

### What a Child Inherits

`Namespace` documents a five-way field policy (see the type's doc comment, which
is the authority — new fields must pick one):

| Policy | Fields | Effect on a child |
|---|---|---|
| Per-VM | `Name`, `parent`, `phases`, `runtime`, `moduleInstances`, `syntaxInterns` | child gets its own; `syntaxInterns` is nil and `InternSyntax` delegates to the parent |
| Captured at construction | `libraryRegistry`, `libraryEnvFactory`, `registry`, `authorizer`, `envMap` | child copies the parent's pointer at fork time; a later `parent.SetRegistry(other)` does **not** reach it, but mutation *through* the shared pointer does |
| Delegated to root | `fileResolver`, `loadPathStack`, `scopeRegistry`, `immutableLiterals`, `immutableTopLevel` | child stores nothing; reads walk the parent chain |
| Pointer-shared (`*EngineServices`) | `ioState`, `formRegistry`, `inlineThreshold`, `maxExpandDepth`, `exportIndex` | one struct for the whole namespace tree |
| Owned outright | `sealedWriteRoot`, `inlineHOFTemplates`, `effectiveRegistry`, `extensionState` | child builds its own; unrelated to the parent's |

Capture, not delegation, is the policy for capability state: a reassignment on the
parent must not silently widen an existing child.

### Creating a Namespace

```go
// Basic creation
ns := environment.NewNamespace()
env := ns.Runtime()  // Get the runtime (phase 0) environment

// Convenience wrapper (deprecated; delegates to NewNamespace().Runtime())
env := environment.NewNamespaceFrame()
```

### Syntax Interning

Syntax interning is **per-Namespace**, ensuring consistent syntax object identity during macro expansion:

```go
ns := environment.NewNamespace()

// Syntax objects are interned for consistent identity
interned := ns.InternSyntax(key, syntaxValue)
```

**Note:** Symbol `eq?` identity does not use interning. Symbols are compared by their string key via `helpers.EqIdentity`, satisfying R7RS §6.5 ("Two symbols are identical if and only if their names are spelled the same way") without per-instance interning.

---

## Phase Hierarchy

`Phase` is an `int8`, and `PhaseRegistry.GetOrCreate` mints a frame for **any**
value in `[-128, 127]` on first access. The four named constants are the phases
that have names; they are not the set of phases that exist.

| Phase | Constant | What actually lands there |
|-------|----------|---------------------------|
| -1 | `PhaseTemplate` | `(import (for-template …))` / `(for-meta -1 …)` bindings. Nothing in Wile *reads* phase -1, so these bindings are installed and inert |
| 0 | `PhaseRuntime` | Normal program execution; user `define`s; runtime primitives |
| 1 | `PhaseExpand` | `define-syntax` transformers, `begin-for-syntax` / `define-for-syntax` bodies, `(import (for-syntax …))` |
| 2 | `PhaseCompile` | Registry compile-time bindings (`AddBinding`, `PhaseSetCompile`); `(for-meta 2 …)` |
| 3 … 127 | *(none)* | Created on demand by the macro tower: a transformer body or a nested `begin-for-syntax` at phase *N* runs its own compile-time code at *N+1* |

Phases 3 and up are not hypothetical, and the tower is observable from Scheme.
Under `--strict=no-bindings` (nothing ambient, so every name must be imported at
a stated phase):

```scheme
;; program A
(import (for-meta 2 (scheme base)))                  ; car bound at phase 2
(begin-for-syntax (begin-for-syntax (car '(1 2))))   ; body runs at phase 2 => ok

;; program B, a separate file: the imports do not accumulate
(import (for-syntax (scheme base)))                  ; car bound at phase 1
(begin-for-syntax (begin-for-syntax (car '(1 2))))   ; body runs at phase 2
;; => no such local or global binding "car"
```

The second failure is the hermeticity property, not a missing feature: a phase-*N*
read is a candidate only against slots at exactly phase *N* or the ambient
coordinate — never at any other exact phase — so a binding installed at phase
*N* is invisible at *N+1* (and at *N-1*) by key disjointness in the store, not by
a missing parent link. Phase shifts compose
additively, so `(for-syntax (for-syntax lib))` is the same as `(for-meta 2 lib)`,
and a shift that leaves `int8` is rejected (`for-meta: phase 200 out of range
[-128, 127]`).

`GetGlobalIndexAcrossPhases` (`environment/environment_frame.go`, the R7RS §4.3
macro-generating-macro carve-out for free template identifiers) and
`findLibraryBinding` (`machine/compilation/library_bindings.go`, which decides
what a library can export) both derive their probe set from
`EnvironmentFrame.PresentPhases()`: the non-negative phases the owner's OWN
registry has actually instantiated, ascending, `PhaseTemplate` excluded.
Neither is hard-wired to `{0, 1, 2}` — a name a library binds at phase 3 or
above is exportable.

Everything else that walks phases is driven by `Store().LiveSlots()` (every
live slot at every phase and rank, in one map walk over the owner's store):
`BoundNamesAcrossPhases`, and `,apropos` through it.

`registry.PhaseSet`, the *registration* vocabulary, is narrower still: a `uint8`
bitset covering phases 0..7 only. `PhaseTemplate` and any tower phase ≥ 8 are
unrepresentable in it by construction (`With` panics, `Has` returns false). See
[extensions/architecture.md](../extensions/architecture.md#phases).

### Accessing Phase Environments

```go
env := environment.NewNamespaceFrame()

// Absolute phase access (creates the frame on first call)
runtime := env.AtPhase(environment.PhaseRuntime)   // same as env.Runtime()
expand  := env.AtPhase(environment.PhaseExpand)    // same as env.Expand()
compile := env.AtPhase(environment.PhaseCompile)   // same as env.Compile()
tower   := env.AtPhase(7)                          // legal; created on demand

// Relative phase access: what the macro tower is built on. Climbing sites
// must use these, not Expand(), or every phase collapses into phase 1.
next := env.NextPhase()                   // frame at env.PhaseLevel()+1
next, err := env.NextPhaseChecked(base)   // same, int8 ceiling as an error
```

`NextPhase()` at `phaseLevel 0` equals `Expand()`, which is why top-level
expansion is byte-for-byte unchanged by the tower (the *level-0 identity*). The
climbing sites are enumerated in
[compiler/macro-system.md](../compiler/macro-system.md#phase-tower-relative-phase-accessors).

Each phase has its own `GlobalEnvironmentFrame` for bindings but shares the `Namespace` for interning.

---

## Creating Child Environments

### For Lexical Scopes (lambda, let, etc.)

Use `NewEnvironmentFrameWithParent` to create child environments that inherit from a parent:

```go
// Create a top-level environment
topEnv := environment.NewNamespaceFrame()

// Create a child with local bindings (e.g., for lambda parameters)
localEnv := environment.NewLocalEnvironment(2)  // 2 parameters
childEnv := environment.NewEnvironmentFrameWithParent(localEnv, topEnv)

// The child shares:
// - Same GlobalEnvironmentFrame as parent
// - Same PhaseRegistry
// - Same Namespace (for interning)
```

### For Libraries

Libraries need isolated bindings but must share the `Namespace` for syntax identity:

```go
// WRONG: Creates new Namespace, breaks syntax identity
// libEnv := environment.NewNamespaceFrame()

// CORRECT: Share Namespace with caller
callerTopLevel := callerEnv.Namespace()
libEnv := callerTopLevel.NewChildRuntime()
```

The `NewChildRuntime` method creates an environment that:
- Has its own `GlobalEnvironmentFrame` (isolated bindings)
- Has its own `PhaseRegistry` (isolated phase hierarchy)
- Shares the `Namespace` (syntax interning)

---

## Library Environment Factory

The `LibraryEnvFactory` field on `Namespace` creates environments for R7RS libraries. It must share the caller's `Namespace`:

```go
// In pkg/internal/bootstrap/bootstrap.go — NewLibraryEnvironmentFrame
func NewLibraryEnvironmentFrame(ctx context.Context, callerEnv *environment.EnvironmentFrame, _ []string) (*environment.EnvironmentFrame, error) {
    // Create a new environment that shares the caller's Namespace
    libEnv := callerEnv.Namespace().NewChildRuntime()

    // Initialize with shared sequence (primitives, macros, etc.)
    // ...

    return libEnv, nil
}

// In main.go or engine setup
env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)
```

---

## Common Patterns

### Setting Up a Complete Runtime

```go
import (
    "context"
    "github.com/aalpar/wile/pkg/environment"
    "github.com/aalpar/wile/pkg/internal/bootstrap"
    "github.com/aalpar/wile/pkg/machine/compilation"
)

func setupRuntime(ctx context.Context) (*environment.EnvironmentFrame, error) {
    // Create complete environment with primitives and macros
    env, err := bootstrap.NewNamespaceFrame(ctx)
    if err != nil {
        return nil, err
    }

    // Set up library loading
    registry := compilation.NewLibraryRegistry()
    env.SetLibraryRegistry(registry)

    // Configure library environment factory (shares Namespace)
    env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

    return env, nil
}
```

### Creating Test Environments

```go
func TestSomething(t *testing.T) {
    // Simple environment for unit tests
    env := environment.NewNamespaceFrame()

    // Or with full primitives
    env, err := bootstrap.NewNamespaceFrame(context.TODO())
    if err != nil {
        t.Fatal(err)
    }

    // For tests involving libraries
    env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)
}
```

### Accessing Bindings

```go
env := environment.NewNamespaceFrame()

// Create a global binding and give it a value. Creation takes a plain
// []*syntax.Scope: a nil slice is the ambient (empty) scope set — what a
// user-written top-level define carries. A macro-introduced binder passes its
// own scope set, which keys a distinct slot under the same name.
// DefineOwnGlobal pairs the create and the write under one key; see Invariant 5.
sym := values.NewSymbol("foo")
err := env.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, values.NewInteger(42))
if err != nil {
    return err
}

// Look up a binding. The query is a syntax.ScopeSet, not a slice:
// AllScopes() is the wildcard, ScopesOf(ref.Scopes()) is hygiene-correct
// resolution, EmptyScopes() is the ambient reference.
binding := env.GetBinding(sym, syntax.AllScopes())
if binding != nil {
    value := binding.Value()
}
```

---

## Invariants

These invariants must be maintained:

1. **Every EnvironmentFrame has a Namespace**
   - Use `NewNamespaceFrame()` or `NewEnvironmentFrameWithParent()` with a valid parent
   - Never call `NewEnvironmentFrameWithParent(local, nil)` - it will panic

2. **Syntax interning requires Namespace**
   - `InternSyntax()` delegates to parent if this is a child environment
   - Always create environments properly

3. **Libraries share Namespace with caller**
   - `Namespace.LibraryEnvFactory()` must use caller's Namespace
   - Failure breaks syntax identity across library boundaries

4. **Phase environments share Namespace**
   - All phases use the same interning tables
   - Expand-phase macros can reference runtime symbols correctly

5. **Global bindings are scope-keyed, and creation is stricter than lookup**
   - A name owns a *list* of slots, one per distinct binder scope set, so a
     user-written `x` and a macro-introduced `x` are different variables
   - Lookup uses maximal subset match (`bindingScopes ⊆ useScopes`, largest wins)
     within the highest-ranked tier the ranked probe finds a candidate in (see
     Invariant 6), which is what preserves sealed-tier shadowing
   - Creation uses **exact scope-set equality**, not subset compatibility.
     Reusing lookup's predicate here would let a macro-introduced `{m}` binder
     clobber a user's `{}` binding, since `ScopesCompatible({}, {m})` is true
   - A resolution can also come back **ambiguous**: two incomparable scope sets
     tied for the maximal match. That is a third answer, not "unresolved";
     `GetBinding` / `GetLocalIndex` panic with a wrapped `werr.ErrAmbiguousBinding`
     rather than break the tie by order (Racket's "ambiguous binding")
   - The query side is the `syntax.ScopeSet` type (`pkg/values/scope_set.go`), not
     a `[]*Scope` whose nil once meant "match any" at some entry points and "the
     empty set" at others. `AllScopes()` is the wildcard, `EmptyScopes()` the
     ambient reference, `ScopesOf(s)` a specific set; the zero value is the empty
     set, so a forgotten initialization can never silently widen a resolution.
     `GetGlobalIndex` has no query parameter at all: it is the wildcard form by
     construction, as is any index built by `NewGlobalIndex` from a bare symbol
   - Creation is **not** converted: it still takes a plain `[]*syntax.Scope`.
     `CreateGlobalBinding` (via `MaybeCreateOwnGlobalBinding`) reads nil as the
     exact empty set; `LocalEnvironmentFrame.MaybeCreateLocalBinding` still reads
     nil as "match any", the last surviving instance of the old overload
   - Pairing a create with a write through a wildcard index therefore still lands
     the value on a different binding than the one just created;
     `DefineOwnGlobal` exists so that pairing cannot be written by hand

6. **One store per owner; sealed vs. mutable is a slot coordinate, not a frame**
   - **Phase environments are isolated; the one thing they all reach is the
     ambient set built at VM startup.** There is no hierarchy of phases and no
     phase frame ever resolves into the phase below it. But there is also no
     *frame* to inherit from any more: a `Namespace` and a `NewChildRuntime`
     library env each own exactly ONE `GlobalEnvironmentFrame` (the "store"),
     and both the ordinary phase views and the sealed-write views
     (`EnvironmentFrame.AtPhase` / `SealedWriteViewAt`) are thin views over that
     same store, distinguished only by which coordinates their reads probe and
     their writes stamp. What used to be "does frame A's parent chain reach
     frame B" is now "does slot X's `(PhaseKey, sealed)` pair make it a
     candidate for this read".
   - **Every slot carries a coordinate.** `PhaseKey` is an exact phase
     (`ExactPhase(n)`) or the ambient wildcard (`AnyPhase()`); `sealed` is a bool.
     `(ANY, mutable)` is refused at the write API (`CreateGlobalBindingAt`
     panics) — nothing populates it, so the ranked probe below never has to
     consider it.
   - **A read is a ranked probe over three tiers**, highest wins
     (`resolveRankedLocked` / `probeRankedLocked`, `global_environment_frame.go`):

     | Tier | Coordinate | What lands here |
     |------|------------|------------------|
     | T1 | `(exact phase N, mutable)` | ordinary `define`s at phase N — user code, `define-for-syntax` bodies |
     | T2 | `(exact phase N, sealed)` | registry fixtures that must stay OFF the T1 tier for one phase only — the phase-1 sealed-write view (bootstrap macros, special-form expanders) |
     | T3 | `(ANY, sealed)` | the ambient startup set — Go primitives, sealed stdlib procedures, optimizer `Stable` anchors, all written through the phase-0 sealed-write view |

     A slot at any OTHER exact phase is **not a candidate at all** — that is
     phase hermeticity, expressed as key disjointness rather than a missing
     parent link. Within the winning tier, maximal scope cardinality ranks as
     usual; an incomparable equal-cardinality tie panics with a wrapped
     `werr.ErrAmbiguousBinding`.
   - **Write coordinates come from the writing VIEW, not from an argument**
     (`EnvironmentFrame.writeCoordinates`): a sealed write at phase 0 derives
     `(ANY, sealed)` — the ambient set every other phase's T3 reaches — and every
     other write (mutable at any phase, or sealed at any phase above 0) derives
     `(ExactPhase(view's phase), sealed)`. This is why the phase-0 sealed-write
     view and the phase-1 one differ in REACH even though both carry
     `rank == writeRankSealed`: only the phase-0 one's writes are ambient.
   - **`sealedAxis` names which phases own a sealed-write view at all**
     (`sealed_write_view.go`): `{PhaseRuntime, PhaseExpand}`, in construction
     order. Every owner's `PhaseRegistry` mints every row (`newPhaseRegistry`);
     owners differ only in what gets applied *through* those views, never in
     which phases have one. Phase 2 and above have no sealed-write view at all,
     so a `define-syntax` inside a transformer body climbs off the sealed axis
     into the ordinary mutable phase-2 view.
   - **`SealedWriteViewAt(phase)` is what registration writes through.** It
     returns the owner's cached sealed-write view for `phase` when the axis has
     a row there, else falls back to the receiver's own ordinary view at that
     phase (`unsealedTargetAt`) — which is what leaves a library's registry
     expand-phase primitives exactly where `registry.Apply`'s `phaseTargets`
     put them (the mutable expand tier, never sealed; that placement was never a
     property of a frame kind).
   - **`AtPhase`'s climb from a sealed-write view stays sealed** wherever the
     target phase has a sealed-write view of its own, and falls through to the
     ordinary phase view everywhere else. Bootstrap macros compile with
     `env == the sealed-write ROOT VIEW` (phase 0), so their `define-syntax`
     writes climb via `NextPhase()`/`AtPhase` into the phase-1 sealed-write view;
     special-form expanders register there directly via
     `SealedWriteViewAt(PhaseExpand)`. Above phase 1, the climb lands on the
     ordinary mutable view — there is nothing left to stay sealed onto.
   - **Hermeticity is a property of every owner of a store, library environments
     included.** A `NewChildRuntime` library env is a full OWNER: its own store,
     its own `PhaseRegistry`, its own sealed-write views over every `sealedAxis`
     row — never the namespace's. It shares only the caller's `Namespace`
     pointer, for syntax interning. A library body's phase separation therefore
     matches the top level's exactly, just addressed into a different store.
   - **A top-level `(define-syntax foo …)` shadows, it does not overwrite.** It
     lands in the mutable expand view (`AtPhase(PhaseExpand)` from the mutable
     runtime) at coordinate `(1, mutable)` — a different SLOT from the pinned
     bootstrap `foo` at `(1, sealed)`, both in the same store.
   - **Why the phase-1 sealed-write view is distinct from the phase-0 one, not a
     reuse.** A compile-time handler (a bootstrap macro, `BindingTypeSyntax`, or
     a special-form expander, `BindingTypePrimitive`) written at the phase-0
     `(ANY, sealed)` coordinate would be reachable by **runtime value
     resolution** — every phase-0 read's T3 tier is exactly that ambient set.
     That is a phase confusion: a dialect that removes a form
     (`Dialect.Forms().Remove`) would then leak the form's
     `#<primitive-expander:…>` into the value world instead of the name being
     unbound. Landing it at `(1, sealed)` instead keeps it off every phase-0
     probe entirely — it is a candidate only for a phase-1 read's T2 tier.
   - **Enumeration must span the whole store, not one view.** `LiveSlots()` /
     `SealedSlots()` (`GlobalEnvironmentFrame`) snapshot every live slot at any
     phase and rank in ONE map walk; `BoundNamesAcrossPhases` and `,apropos` use
     these rather than iterating per-phase views, or primitive / bootstrap-macro
     names would silently vanish from introspection.

---

## Load-Path Stack

A load stack enables relative path resolution for `load`, `include`, and `import` by tracking which files are currently being loaded. It is stored on `Namespace` (per-VM, not per-thread). The stack is behind the `PathTracker` interface so `environment/` does not depend on `machine/compilation/`:

```
┌────────────────────────────────────────────────────────────────────────┐
│  PathTracker interface (environment/file_resolver.go)                  │
│    Push(absPath) / Pop() / Current() / CurrentDir() / Depth()          │
│                                                                        │
│  Concrete impl: *LoadStack                                             │
│    (machine/compilation/sourceload/load_stack.go)                      │
│    paths []string    ← LIFO stack of resolver-supplied paths           │
│    mu    sync.RWMutex ← thread-safe access                             │
└────────────────────────────────────────────────────────────────────────┘
```

The engine wires in the concrete implementation via `ns.SetLoadPathStack(sourceload.NewLoadStack())` at startup (see `engine.go`).

### Resolution Strategy

Filename resolution goes through the `FileResolver` interface (`environment/file_resolver.go`). Concrete implementations live in `machine/compilation/resolver/` (`os_file_resolver.go`, `fs_file_resolver.go`, `embed_file_resolver.go`, `chain_file_resolver.go`), backed by `sourceload.Finder` for file search. The load stack's current directory is consulted as the relative base:

```
1. Absolute path     → use as-is (authorizer-gated)
2. Stack-relative    → path relative to stack.CurrentDir()
3. Fallback dirs     → library registry paths, SCHEME_INCLUDE_PATH, CWD
4. Filesystem root   → "."
```

Stack-relative takes precedence over fallback directories. Error messages list all searched paths.

### Integration Points

All three file-loading operations push/pop the stack:

| Operation | Location | Phase |
|-----------|----------|-------|
| `load` | `extensions/eval/prim_eval.go` | Runtime |
| `include` | `machine/compilation/compile_time_continuation_include.go` | Compile-time |
| `import` (library loading) | `machine/compilation/library_loader.go` | Compile-time |

This enables correct nested resolution: `(load "a.scm")` containing `(load "b.scm")` resolves `b.scm` relative to `a.scm`'s directory.

### Scheme Primitives

| Primitive | Returns | Notes |
|-----------|---------|-------|
| `(current-load-path)` | string or `#f` | Absolute path of file being loaded |
| `(current-load-directory)` | string or `#f` | Directory of file being loaded |
| `(current-load-depth)` | integer | Nesting depth (0 in REPL) |

### Go Embedder API

```go
// Recommended: automatic push/pop via defer
err := engine.WithLoadPath("/app/scripts/main.scm", func() error {
    _, err := engine.EvalMultiple(ctx, `(load "helper.scm")`) // resolves relative to /app/scripts/
    return err
})

// Direct access
engine.PushLoadPath("/app/scripts/main.scm")
defer engine.PopLoadPath()

// Query
engine.CurrentLoadPath()       // "" if none
engine.CurrentLoadDirectory()  // "" if none
```

### Design: Per-VM, Not Per-Thread

The stack lives on `Namespace`, shared across all child environments via delegation. This is intentional: library loading must resolve paths relative to the importing file even when the library runs in its own isolated environment.

**Concurrency caveat**: Concurrent `(load ...)` from multiple SRFI-18 threads can corrupt LIFO ordering. Single-threaded loading (the common case) is fully correct.

---

## Thread Safety

- `Namespace.InternSyntax()` - Thread-safe (uses RWMutex)
- `PhaseRegistry.GetOrCreate()` - Thread-safe (uses RWMutex)
- `PathTracker` / concrete `LoadStack` - Thread-safe for individual operations (uses RWMutex); LIFO ordering only guaranteed single-threaded
- `GlobalEnvironmentFrame` keys/slots - Thread-safe (RWMutex; `CreateGlobalBindingAt` takes the write lock for its check-then-write)
- Global `Binding` value and metadata - Thread-safe: a global binding's value and its `*BindingMeta` live in an `atomicCell`, read lock-free and published by store / copy-on-write CAS (`Binding.UpdateMeta`). Never write a global's meta field in place
- Local `Binding` operations - Not thread-safe (locals are frame-private, single-threaded compilation assumed)

---

## References

- R7RS §6.5: Symbols - Symbol identity requirements
- Flatt 2016: "Binding as Sets of Scopes" - Hygiene model
- `pkg/environment/` - Implementation
- `pkg/internal/bootstrap/bootstrap.go` - Runtime initialization (`NewNamespaceFrame`, `NewLibraryEnvironmentFrame`)

# Environment Relationship Diagram

Visual map of all environment types, their relationships, and how they flow through the compilation/expansion/execution pipeline.

See [system.md](system.md) for detailed API documentation.

---

## Ownership Hierarchy

```
┌───────────────────────────────────────────────────────────────────────────────┐
│                       Engine (pkg/wile/engine.go)                             │
│  namespace ──→ *environment.Namespace                                         │
│  env ────────→ runtime EnvironmentFrame (phase 0, mutable user global)        │
│  registry ───→ *registry.PrimitiveRegistry (Go-side primitive registration)   │
└───────────────────────────────────────────────────────────────────────────────┘
                    │
                    │ owns
                    ▼
┌───────────────────────────────────────────────────────────────────────────────┐
│                         Namespace (root, one per VM)                          │
│                                                                               │
│  syntaxInterns ─── map[Value]SyntaxValue ← thread-safe, per-instance          │
│  libraryRegistry ─ LibrarySearcher       ← *compilation.LibraryRegistry       │
│  phases ────────── *PhaseRegistry        ← phase views + sealed-write views   │
│  runtime ───────── *EnvironmentFrame     ← the ROOT VIEW (mutable phase 0)    │
│  sealedWriteRoot ─ *EnvironmentFrame     ← phase-0 SEALED-WRITE view, same    │
│                                            store as runtime                   │
│  parent ────────── nil (root)                                                 │
└───────────────────────────────────────────────────────────────────────────────┘
                    │
                    │ owns
                    ▼
┌───────────────────────────────────────────────────────────────────────────────┐
│     PhaseRegistry (one per owner: a Namespace, or a NewChildRuntime env)      │
│                                                                               │
│  ONE store per owner: runtime.global == every view's global below.            │
│                                                                               │
│  envs (mutable-rank views,        sealedViews (sealed-rank views, one per     │
│  any int8 phase, lazy):           sealedAxis row):                            │
│    0 → runtime   (mutable writes)   0 → sealed-write root                     │
│    1 → expand    (mutable writes)     (writes land (phase 0, sealed))         │
│    2 → phase 2   (mutable writes)   1 → sealed-write expand                   │
│   -1 → template                       (writes land (phase 1, sealed))         │
│    N → tower phase, on demand                                                 │
│                                                                               │
│  sealedAxis = {PhaseRuntime, PhaseExpand} — every owner mints both rows;      │
│  owners differ only in what gets applied THROUGH them.                        │
└───────────────────────────────────────────────────────────────────────────────┘
```

A `NewChildNamespace()` child mints the same pair — its own store, its own
`PhaseRegistry` with its own `sealedViews` — never the parent's. Differences from
the root are ownership-policy, not shape:

| Field | Child Namespace |
|---|---|
| `parent` | root Namespace (interning delegation) |
| `syntaxInterns` | nil (`InternSyntax` delegates up) |
| `phases` | own `PhaseRegistry`, own store, sealed tier starts **empty** |
| `runtime` | own `EnvironmentFrame`, empty |
| captured | `libraryRegistry`, `libraryEnvFactory`, `registry`, `authorizer`, `envMap` — parent's pointer at fork time |
| `services` | shared `*EngineServices` (same pointer as parent) |

---

## Phase Environments

Every phase VIEW, whether `runtime`, `expand`, a tower phase (phase ≥ 2), or a
sealed-write view, shares the SAME `*GlobalEnvironmentFrame` (`global`); they
differ only in the phase their reads probe at and the `(phase, sealed)`
coordinate their writes stamp (`pkg/environment/global_environment_frame.go`,
`environment_frame.go`). There is no parent chain to a "sealed base" any more.

```
┌──────────────────────────────────────────────────────────────────────────┐
│  ONE store: every phase's bindings, every rank, in one scope-keyed map.  │
└──────────────────────────────────────────────────────────────────────────┘
```

| View | Reads (ranked probe at) | Write lands at |
|---|---|---|
| `runtime` (`Runtime()`) | phase 0 | (0, mutable) |
| `expand` (`Expand()`) | phase 1 | (1, mutable) |
| `tower` (`AtPhase(n)`, n ≥ 2) | phase n | (n, mutable) |
| sealed-write root | phase 0 | (0, sealed) |
| sealed-write expand | phase 1 | (1, sealed) |

Every ordinary read (`GetBinding`, `GetGlobalIndexWithScopes`) runs the same
probe (`resolveRankedLocked`): `tierExactMutable`, then `tierExactImported`, then
`tierExactSealed` at the view's phase, then the bulk rows declared at that phase
on a miss. The `sealed` flag affects only writes and
`AtPhase`'s upward climb.

There is no phase-blind tier. A phase-*N* read is a candidate only for slots at
exactly phase *N* — never any OTHER phase. That disjointness IS hermeticity: a
phase-1 read cannot see a phase-0 user define, and vice versa. What makes a base
name visible from a phase that holds no slot of its own for it is a declared BULK
ROW, consulted only when the per-symbol probe misses
(`pkg/environment/bulk_source.go`). By default the Engine declares the whole base
at phase 0 and only the macro-vocabulary subset at every phase ≥ 1 (`installInitialImports`,
`pkg/wile/engine.go`), so a phase-1 body reaches `car` but not `cadr` without an
`(import (for-syntax …))`. Primitives registered for the expand phase also hold
their own `(1, sealed)` slots (`registry.Apply`).

The split between the mutable tier and the sealed ones is what makes a top-level
`define-syntax` shadow a bootstrap macro in the mutable expand view rather than
overwrite it in place — they are different SLOTS at different coordinates in the
same map.

The set of views is open-ended, not five fixed ones. `PhaseRegistry.GetOrCreate`
mints an ordinary view for any `int8` phase on first access, and
`EnvironmentFrame.NextPhase()` climbs one rung per nested compile-time form, so
a doubly-nested `begin-for-syntax` or a transformer body that itself defines
macros reaches phase 2, 3, and beyond. Above phase 1 there is no sealed-write
view (`sealedAxis = {PhaseRuntime, PhaseExpand}`), so the climb from a
sealed-write view lands on the ordinary mutable view at that phase — never a
phase→phase parent edge, because there is no parent edge at all
(`TestPhaseRegistry_ExpandPhaseIsHermetic` guards the disjointness).

**A library environment has the same shape.** A `NewChildRuntime` env is a full
OWNER — its own store, its own `PhaseRegistry`, its own `sealedViews` over every
`sealedAxis` row — never the namespace's. See [Library
Environments](#library-environments).

The sealed-write views are **not** `PhaseRegistry.envs` entries; they live in
the registry's separate `sealedViews` map. Every ordinary numbered phase view
IS an `envs` entry, created lazily.

All phase views share:
- The same `*GlobalEnvironmentFrame` (the one store)
- The same `*PhaseRegistry` (back-pointer)
- The same `*Namespace` (for interning)

---

## Lexical Scope Chain (Runtime Execution)

The compiler builds this chain with `NewEnvironmentFrameWithParent`. At run time
a closure call builds its parameter frame with `InitApplyFrameWithParent`, and only
an unmerged `let` (`OpPushEnv`) adds a frame of its own; a `let` in a procedure
body takes its slots from the parameter frame (see
[frame-allocation.md](frame-allocation.md)).

```
┌───────────────────┐    ┌───────────────────┐    ┌───────────────────┐
│ Runtime (phase 0) │◄───│ Lambda body       │◄───│ Inner let         │
│ EnvFrame          │    │ EnvFrame          │    │ EnvFrame          │
│                   │    │                   │    │                   │
│ parent: nil       │    │ parent: ──────────┘    │ parent: ──────────┘
│ local: nil        │    │ local: params     │    │ local: let-vars   │
│ global: ──────┐   │    │ global: ──────┐   │    │ global: ──────┐   │
└───────────────┼───┘    └───────────────┼───┘    └───────────────┼───┘
                │                        │                        │
                └────────────────────────┴────────────────────────┘
                              SHARED GlobalEnvironmentFrame
                              (every frame of the owner shares it, at every phase)
```

Child frames inherit `global`, `phaseLevel`, `phases`, and `namespace` from the parent; `local` and `parent` are their own, and `sealed` is always false (a lexical child is never a registration target).

---

## Library Environments

Created by `Namespace.NewChildRuntime()`. A library env is a full OWNER — its
own store, its own `PhaseRegistry` — sharing only the caller's `Namespace`
pointer, for syntax interning identity.

```
┌──────────────────────────────────────────────────────────┐
│                      Root Namespace                      │
│                                                          │
│  syntaxInterns: {...}                                    │
│  phases: rootPhases  → owns the ROOT store               │
│  runtime: rootEnv                                        │
└──────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────┐
│      Library env (rootNamespace.NewChildRuntime())       │
│                                                          │
│  namespace: ─────► SAME pointer as root (interning)      │
│  global: OWN store  → isolated bindings                  │
│  phases: ownPhases  → owns its OWN store, own            │
│                       sealedViews (not the root's)       │
│  parent: nil        → structural root, phaseLevel 0      │
└──────────────────────────────────────────────────────────┘
```

Inside the library's own store, phase separation is the same ranked probe as a
namespace's — over the library's OWN coordinates, never the root's:

| View | Reads (ranked probe at) | Write lands at |
|---|---|---|
| `libRT` (`Runtime()`) | phase 0 | (0, mutable) |
| `libExp` (`Expand()`) | phase 1 | (1, mutable) |
| `libTower` (`AtPhase(n)`, n ≥ 2) | phase n | (n, mutable) |
| library sealed-write root | phase 0 | (0, sealed) |
| library sealed-write expand | phase 1 | (1, sealed) |

So the isolation a library env provides is both *lateral* (its store is not the
engine's, so nothing reaches the engine's bindings at any phase) and *vertical*
(inside a library, phase 1 does not see phase 0). The registry apply —
primitives, bootstrap procedures, syntax compilers — writes through the
library's sealed-write views, and the library's own `define`s land in its
`(0, mutable)` tier; that split is what lets a `begin-for-syntax` body reach
`car` (the registry's expand-phase copy at `(1, sealed)`) while missing the
library's runtime defines. The Engine's library env factory also installs the
dialect's initial-import rows into the library store, as it does for the
namespace.

Until 2026-08-05 a library env was a single flat frame with `parent: nil`, and
its phase-1/phase-2 frames parented to its own phase-0 frame — the one
phase→phase parent edge in the tree, from back when hermeticity was topology
rather than key disjointness. Neither shape, flat or fold, was ever the
mechanism of an observable phase leak that existed independently; see the doc
comment on `EnvironmentFrame.GetGlobalIndexFromLibraryScopes`
(`environment_frame.go`).

---

## NewChildRuntime vs NewChildNamespace

Both create isolated bindings with shared interning. They differ in what they return and how `Namespace()` resolves.

```
NewChildRuntime:                NewChildNamespace:

  Namespace (shared)    Parent NS         Child NS
  +------------------+            +----------+      +----------+
  | runtime: envP    |            | runtime: |      | runtime: |
  +------------------+            | envP     |      | envC     |
          │                       +----------+      +----------+
          │                                            │
     ┌────┴────┐                                       ▼
     ▼         ▼                           EnvironmentFrame (envC)
   envP      envC ◄── new child            +----------------------+
   (parent   (has own Global-              | namespace: child NS  |
    frame)    EnvFrame, but                +----------------------+
              namespace points
              to shared NS)

  envC.Namespace() == parent    envC.Namespace() == child
  ns.Runtime() returns envP       child.Runtime() returns envC  ✓
```

| | `NewChildRuntime()` | `NewChildNamespace()` |
|---|---|---|
| **Returns** | `*EnvironmentFrame` | `*Namespace` |
| **Use case** | Library loading (internal) | `(environment)`, `(null-environment)` (first-class) |
| **`Namespace()`** | Parent's Namespace | The child Namespace itself |
| **`Runtime()`** | N/A | Returns child's own frame |

---

## Compilation / Expansion Pipeline

How environments flow through the pipeline stages.

```
CompileTimeContinuation          ExpanderTimeContinuation
┌────────────────────────┐       ┌────────────────────────┐
│ env: EnvironmentFrame  │       │ env: EnvironmentFrame  │
│   phase N (0 at top)   │       │   phase N (0 at top)   │
│                        │       │                        │
│ Uses:                  │       │ Uses:                  │
│  env ──── runtime vars │       │  env ───── arm 1 macro │
│  keywords ─ (0,sealed) │       │  env.NextPhase() arm 2 │
│  env.Expand() ──── P1  │       │  SealedBindingAt arm2b │
│  env.NextPhase() P N+1 │       │  libEnv.Expand() arm 3 │
└────────────────────────┘       └────────────────────────┘
          │                                  │
          │ define-syntax                    │ let-syntax / letrec-syntax
          │ compiles transformer             │ creates local expand scope
          ▼                                  ▼
┌────────────────────────┐     ┌───────────────────────────────┐
│ env.NextPhase()        │     │ NewEnvironmentFrameWithParent │
│ (phase N+1 EnvFrame)   │     │ (localExpandEnv, p.env)       │
│                        │     │                               │
│ Stores macro bindings: │     │ parent: enclosing env         │
│  BindingTypeSyntax     │     │ local: macro bindings         │
│  with hygiene scopes   │     │ (NOT p.env.Expand()!)         │
└────────────────────────┘     └───────────────────────────────┘
```

**`NextPhase()`, not `Expand()`, on the climbing paths.** `define-syntax` storage
and the expander's arm-2 macro lookup are both relative to the expanding frame's
own `phaseLevel`, so a macro defined inside a transformer body lands and resolves
at its climbed phase. At `phaseLevel 0` `NextPhase() == Expand()`, which is why
top-level behavior is unchanged. The remaining absolute readers fall in two groups:

- **Registry fixtures, absolute by design.** `LookupPrimitiveExpander` reads
  `env.Expand()` and accepts only `BindingTypePrimitive`, so it names no user
  macro that could climb. The expander's arm 2b reads the owner's sealed phase-1
  tier (`SealedBindingAt(…, PhaseExpand)`) from phase 2 and above, or when arm 2
  answered a `BindingTypePrimitive` binding, so bootstrap macros stay reachable
  up the tower. Syntax compilers and auxiliary keywords are `(0, sealed)` slots;
  a higher phase reaches `else`/`=>` and the macro-writing names only through
  the dialect's macro-vocabulary bulk row.
- **One site that pins phase 1 regardless of the defining frame's level.**
  `CompileMeta` (the `meta` form) compiles its body against `p.env.Expand()`:
  correct at the top level, collapsed to phase 1 inside a transformer body.
  `er-macro-transformer` no longer does this; `CompileERMacroTransformerExpr`
  (`compile_transformer_forms.go`) records `p.env` as the definition-site env.

**Note:** `let-syntax` environments chain through the enclosing expander's env (`p.env`), not through `env.Expand()`. This preserves nested lexical scoping of macros — inner macros can reference outer macros through the parent chain.

---

## VM Execution (MachineContext)

The VM holds the current environment and mutates it via opcodes.

```
MachineContext
┌──────────────────────────────────────┐
│ vmState.env ── current EnvFrame      │ ← mutated by opcodes
│                                      │
│ expansion.expanderCtx ── ExpanderCtx │ ← impl: compilation.ExpanderContext
│   .env ─── expand-time EnvFrame      │ ← used during macro expansion
│   .introScope ── hygiene scope       │
│   .useSiteScope ─ hygiene scope      │
│                                      │
│ Opcodes that change env:             │
│   Apply → fresh parameter frame      │  (closure call)
│   OpPushEnv → extend with locals     │  (let/letrec)
│   OpPopEnv → restore parent          │
└──────────────────────────────────────┘
```

---

## Summary of All Environment Kinds

| Environment | Created by | Bindings | Interning | Phases | Use |
|---|---|---|---|---|---|
| Root Namespace | `NewNamespace()` | Own store | Own tables | Own registry | VM instance |
| Child Namespace | `NewChildNamespace()` | Own store, sealed tier starts **empty** | Delegates to parent | Own registry | `(environment …)`, `(null-environment)`, `(make-namespace)`, `namespace-derive` |
| Profile Namespace | `bootstrap.NewProfileEnvironment()` | Own store; a curated registry apply fills the child's sealed tier | Delegates to parent | Own registry | `(environment '(wile <profile> [<strictness>]))` |
| Report Namespace | `NewSchemeReportNamespace()` | Own store, **copied** from the parent's | Delegates to parent | Own registry | `(scheme-report-environment)` |
| Runtime frame | `ns.Runtime()` | (0, mutable) tier of the store — the ROOT VIEW | Via Namespace | Shared | Normal execution |
| Sealed-write root view | `ns.Runtime().SealedWriteViewAt(PhaseRuntime)` | writes land (0, sealed) — the startup set | Via Namespace | Shared | Primitives, sealed stdlib, `Stable` anchors |
| Sealed-write expand view | `ns.Runtime().SealedWriteViewAt(PhaseExpand)` | writes land (1, sealed) | Via Namespace | Shared | Bootstrap macros, special-form expanders, expand-phase primitive copies |
| Expand frame | `env.Expand()` / `AtPhase(1)` | (1, mutable) tier | Via Namespace | Shared | Macro bindings |
| Tower frame | `env.NextPhase()` / `AtPhase(n)` | (n, mutable) tier | Via Namespace | Shared | Nested compile-time forms at phase ≥ 2; `(for-meta 2 …)` imports |
| Template frame | `AtPhase(-1)` | (-1, mutable) tier | Via Namespace | Shared | `(for-template …)` import target; no reader |
| Lexical child | `NewEnvironmentFrameWithParent()` | Own local, shared global | Via Namespace | Shared | `lambda`, `let`, `letrec` |
| Library env | `ns.NewChildRuntime()` | Own global + phases | Via shared Namespace | Own registry | `(import ...)` |
| let-syntax env | `NewEnvironmentFrameWithParent(local, p.env)` | Own local macros | Via Namespace | Shared | `let-syntax`, `letrec-syntax` |

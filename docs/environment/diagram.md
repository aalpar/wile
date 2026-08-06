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
│  loadPathStack ─── PathTracker           ← interface; impl *LoadStack         │
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
│    0 → runtime   (T1 writes)        0 → sealed-write root                     │
│    1 → expand    (T1 writes)          (writes land AMBIENT (ANY,sealed))      │
│    2 → compile   (T1 writes)        1 → sealed-write expand                   │
│   -1 → template                        (writes land (exact 1, sealed))        │
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
| captured | `libraryRegistry`, `registry`, `authorizer`, `envMap` — parent's pointer at fork time |
| `services` | shared `*EngineServices` (same pointer as parent) |

---

## Phase Environments

Every phase VIEW — `runtime`, `expand`, `compile`, a tower phase, and each
sealed-write view — shares the SAME `*GlobalEnvironmentFrame` (`global`); they
differ only in which `(PhaseKey, sealed)` coordinates their reads probe and
their writes stamp (`pkg/environment/global_environment_frame.go`,
`environment_frame.go`). There is no parent chain to a "sealed base" any more.

```
┌──────────────────────────────────────────────────────────────────────────┐
│  ONE store: every phase's bindings, every rank, in one scope-keyed map.  │
└──────────────────────────────────────────────────────────────────────────┘
```

| View | Coordinate | Read tier | Write lands at |
|---|---|---|---|
| `runtime` (`Runtime()`) | (0, mutable) | T1 | (0, mutable) |
| `expand` (`Expand()`) | (1, mutable) | T1 | (1, mutable) |
| `compile` (`Compile()`) | (2, mutable) | T1 | (2, mutable) |
| sealed-write root | (0, sealed) at construction | T2 at phase 0 | **(ANY, sealed)** — ambient |
| sealed-write expand | (1, sealed) at construction | T2 at phase 1 | (1, sealed) — exact |

Every read also considers the ambient `(ANY, sealed)` tier (T3) regardless of
which exact phase it targets — that row is what makes primitives and sealed
stdlib procedures visible from every phase.

A phase-*N* read is a candidate only for slots at exactly phase *N* (tier T1
mutable, T2 sealed) or the ambient `(ANY, sealed)` coordinate (tier T3) — never
any OTHER exact phase. That disjointness IS hermeticity: a phase-1 read cannot
see a phase-0 user define, and vice versa, while both still reach the ambient
startup set. The split into two sealed tiers (T2 exact, T3 ambient) is what
makes a top-level `define-syntax` shadow a bootstrap macro in the mutable
expand view rather than overwrite it in place — they are different SLOTS at
different coordinates in the same map.

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

Created by `lambda`, `let`, `letrec`, etc. via `NewEnvironmentFrameWithParent`.

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
                              (all frames at same phase share it)
```

Child frames inherit `global`, `phases`, and `namespace` from the parent. Only `local` and `parent` differ.

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

| View | Coordinate | Read tier |
|---|---|---|
| `libRT` (`Runtime()`) | (0, mutable) | T1 |
| `libExp` (`Expand()`) | (1, mutable) | T1 |
| `libCmp` (`Compile()`) | (2, mutable) | T1 |
| library sealed-write root | writes (ANY, sealed) | T3 at any phase |
| library sealed-write expand | writes (1, sealed) | T2 at phase 1 |

So the isolation a library env provides is both *lateral* (its store is not the
engine's, so nothing reaches the engine's bindings at any phase) and *vertical*
(inside a library, phase 1 does not see phase 0). The registry apply —
primitives, bootstrap procedures, syntax compilers — writes through the
library's sealed-write views, and the library's own `define`s land in its
`(0, mutable)` tier; that split is what lets a `begin-for-syntax` body reach
`car` while missing the library's runtime defines.

Until 2026-08-05 a library env was a single flat frame with `parent: nil`, and
its phase-1/phase-2 frames parented to its own phase-0 frame — the one
phase→phase parent edge in the tree, from back when hermeticity was topology
rather than key disjointness. Neither shape, flat or fold, was ever the
mechanism of an observable phase leak that existed independently; see
`docs/environment/system.md` on `GetGlobalIndexFromLibraryScopes`.

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
│  env.Compile() ─── P2  │       │  env.NextPhase() arm 2 │
│  env.Expand() ──── P1  │       │  libEnv.Expand() arm 3 │
│  env.NextPhase() P N+1 │       │                        │
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

- **Registry fixtures, absolute by design.** `LookupSyntaxCompiler` reads
  `env.Compile()` and `LookupPrimitiveExpander` reads `env.Expand()`; both resolve
  through the sealed axis, and neither names a user macro that could climb.
- **Two sites that pin phase 1 regardless of the defining frame's level.**
  `CompileMeta` (the `meta` form) compiles its body against `p.env.Expand()`, and
  `er-macro-transformer` stores `env.Expand()` as the transformer's definition-site
  env (`compile_er_macro.go`). Both are correct at the top level and collapse to
  phase 1 inside a transformer body.

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
│   OpMakeClosure → new child env      │  (lambda captures env)
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
| Sealed-write root view | `ns.Runtime().SealedWriteViewAt(PhaseRuntime)` | writes land (ANY, sealed) — the ambient tier | Via Namespace | Shared | Primitives, sealed stdlib, `Stable` anchors |
| Sealed-write expand view | `ns.Runtime().SealedWriteViewAt(PhaseExpand)` | writes land (1, sealed) | Via Namespace | Shared | Bootstrap macros, special-form expanders |
| Expand frame | `env.Expand()` / `AtPhase(1)` | (1, mutable) tier | Via Namespace | Shared | Macro bindings |
| Compile frame | `env.Compile()` / `AtPhase(2)` | Phase 2 global | Via Namespace | Shared | Special-form names as valueless bindings; `(for-meta 2 …)` imports |
| Tower frame | `env.NextPhase()` / `AtPhase(n)` | Phase *n* global | Via Namespace | Shared | Nested compile-time forms at phase ≥ 3 |
| Template frame | `AtPhase(-1)` | Phase -1 global | Via Namespace | Shared | `(for-template …)` import target; no reader |
| Lexical child | `NewEnvironmentFrameWithParent()` | Own local, shared global | Via Namespace | Shared | `lambda`, `let`, `letrec` |
| Library env | `ns.NewChildRuntime()` | Own global + phases | Via shared Namespace | Own registry | `(import ...)` |
| let-syntax env | `NewEnvironmentFrameWithParent(local, p.env)` | Own local macros | Via Namespace | Shared | `let-syntax`, `letrec-syntax` |

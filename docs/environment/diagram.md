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
│                     Namespace (root, one per VM)                              │
│                                                                               │
│  syntaxInterns ─── map[Value]SyntaxValue ← thread-safe, per-instance          │
│  loadPathStack ─── PathTracker           ← interface; impl *LoadStack         │
│  libraryRegistry ─ LibrarySearcher       ← *compilation.LibraryRegistry       │
│  phases ────────── *PhaseRegistry        ← owns phase→env mapping             │
│  runtime ───────── *EnvironmentFrame     ← mutable phase 0 (user defines)     │
│  sealedBase ────── *EnvironmentFrame     ← immutable; parent of runtime       │
│  sealedExpandBase  *EnvironmentFrame     ← immutable phase 1 (macros)         │
│  parent ────────── nil (root)                                                 │
└───────────────────────────────────────────────────────────────────────────────┘
         │                                      │
         │ phases registry                      │ NewChildNamespace()
         │ (entries lazily created)             │ (for R7RS (environment),
         ▼                                      │  (null-environment))
┌─────────────────────────────┐                 ▼
│     PhaseRegistry           │    ┌──────────────────────────────────────────┐
│                             │    │   Child Namespace                        │
│  envs:                      │    │                                          │
│    0 → runtime EnvFrame ────┼──→ │  parent ──→ root Namespace               │
│    1 → expand EnvFrame      │    │  syntaxInterns ── nil (delegates up)     │
│    2 → compile EnvFrame     │    │  phases ────── own PhaseRegistry         │
│   -1 → template EnvFrame    │    │  runtime ───── own EnvironmentFrame      │
│   ...                       │    │  sealedBase ── own (fresh, never shared) │
│                             │    │  libraryReg ── captured (shared ptr)     │
│  owner → Namespace          │    └──────────────────────────────────────────┘
│                             │
└─────────────────────────────┘
```

---

## Phase Environments

Each phase has its own `GlobalEnvironmentFrame` (isolated bindings) but shares the `PhaseRegistry` and `Namespace`. Phase frames parent to the **sealed base**, not to the mutable runtime frame (`createPhaseEnv` / `phaseParent` in `pkg/environment/phase_registry.go`).

```
sealedBase          (phase 0, parent nil)  ← Go primitives, sealed stdlib,
│                                            optimizer Stable anchors
├── runtime          (phase 0)              ← mutable user global; Namespace.Runtime()
├── sealedExpandBase (phase 1)              ← bootstrap macros, special-form expanders
│   └── expand       (phase 1)              ← user define-syntax lands here
├── compile          (phase 2)
└── template         (phase -1)
```

**Key:** phases parent to the sealed base, never to each other and never to the mutable runtime. That cut is the hermeticity property: a phase-1 or phase-2 lookup cannot see phase-0 user defines or imports, but still reaches the shared frozen base. Phase 1 reparents one level further, onto `sealedExpandBase`, so a top-level `define-syntax` shadows a bootstrap macro in the mutable child rather than overwriting it in place.

`sealedBase` and `sealedExpandBase` are **not** `PhaseRegistry` entries; they are reached only through the parent chain. `runtime`, `expand`, `compile`, and `template` are the registry entries, created lazily.

All phase environments share:
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
│ parent: sealedBase│    │ parent: ──────────┘    │ parent: ──────────┘
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

Created by `Namespace.NewChildRuntime()`. Shares syntax interning but has isolated bindings and phases.

```
Root Namespace                          Library environment
┌───────────────────────┐               ┌──────────────────────┐
│  syntaxInterns: {...} │◄──────────────┤  namespace: ─────────┤ (same pointer!)
│  phases: rootPhases   │  shared NS    │  global: OWN         │ (isolated bindings)
│  runtime: rootEnv     │               │  phases: ownPhases   │ (isolated phases)
│  sealedBase: rootBase │               │  parent: nil         │ (flat island: no
└───────────────────────┘               │  phaseLevel: 0       │  sealed base)
      │                                 └──────────────────────┘
      │ rootPhases                              │ ownPhases
      ▼                                         ▼
┌───────────┐                             ┌───────────┐
│ 0:runtime │                             │ 0:libRT   │  ← library's own runtime
│ 1:expand  │                             │ 1:libExp  │  ← library's own expand
│ 2:compile │                             │ 2:libCmp  │  ← library's own compile
└───────────┘                             └───────────┘
```

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
┌───────────────────────┐       ┌────────────────────────┐
│ env: EnvironmentFrame │       │ env: EnvironmentFrame  │
│      (phase 0)        │       │      (phase 0)         │
│                       │       │                        │
│ Uses:                 │       │ Uses:                  │
│  env ─── runtime vars │       │  env ──── runtime vars │
│  env.Expand() ── P1   │       │  env.Expand() ─── P1   │
│  env.Compile() ─ P2   │       │                        │
└───────────────────────┘       └────────────────────────┘
          │                               │
          │ define-syntax                  │ let-syntax / letrec-syntax
          │ compiles transformer           │ creates local expand scope
          ▼                                ▼
┌────────────────────────┐     ┌───────────────────────────────┐
│ env.Expand()           │     │ NewEnvironmentFrameWithParent │
│ (phase 1 EnvFrame)     │     │ (localExpandEnv, p.env)       │
│                        │     │                               │
│ Stores macro bindings: │     │ parent: enclosing env         │
│  BindingTypeSyntax     │     │ local: macro bindings         │
│  with hygiene scopes   │     │ (NOT p.env.Expand()!)         │
└────────────────────────┘     └───────────────────────────────┘
```

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
| Root Namespace | `NewNamespace()` | Own | Own tables | Own registry | VM instance |
| Child Namespace | `NewChildNamespace()` | Own | Delegates to parent | Own registry | `(environment)`, `(null-environment)` |
| Runtime frame | `ns.Runtime()` | Phase 0 global (mutable) | Via Namespace | Shared | Normal execution |
| Sealed base | `ns.SealedBase()` | Phase 0 global (immutable) | Via Namespace | Shared | Primitives, sealed stdlib; parent of every phase frame |
| Sealed expand base | `ns.SealedExpandBase()` | Phase 1 global (immutable) | Via Namespace | Shared | Bootstrap macros, special-form expanders |
| Expand frame | `env.Expand()` / `AtPhase(1)` | Phase 1 global | Via Namespace | Shared | Macro bindings |
| Compile frame | `env.Compile()` / `AtPhase(2)` | Phase 2 global | Via Namespace | Shared | Syntax compilers |
| Lexical child | `NewEnvironmentFrameWithParent()` | Own local, shared global | Via Namespace | Shared | `lambda`, `let`, `letrec` |
| Library env | `ns.NewChildRuntime()` | Own global + phases | Via shared Namespace | Own registry | `(import ...)` |
| let-syntax env | `NewEnvironmentFrameWithParent(local, p.env)` | Own local macros | Via Namespace | Shared | `let-syntax`, `letrec-syntax` |

# Environment Relationship Diagram

Visual map of all environment types, their relationships, and how they flow through the compilation/expansion/execution pipeline.

See [system.md](system.md) for detailed API documentation.

---

## Ownership Hierarchy

```
┌───────────────────────────────────────────────────────────────────────────────┐
│                         Engine (engine.go)                                    │
│  namespace ──→ Namespace                                                    │
│  env ────────→ runtime EnvironmentFrame (phase 0)                             │
│  registry ──→ Registry (Go-side primitive registration)                       │
└───────────────────────────────────────────────────────────────────────────────┘
                    │
                    │ owns
                    ▼
┌───────────────────────────────────────────────────────────────────────────────┐
│              Namespace (root, one per VM)                           │
│                                                                               │
│  syntaxInterns ─── map[Value]SyntaxValue ← thread-safe, per-instance           │
│  loadPathStack ─── PathTracker (interface, typically *sourceload.LoadStack) │
│  libraryRegistry ─ any                   ← *machine.LibraryRegistry           │
│  phases ────────── *PhaseRegistry        ← owns phase→env mapping             │
│  runtime ──────── *EnvironmentFrame      ← the phase 0 env                    │
│  parent ────────── nil (root)                                                 │
└───────────────────────────────────────────────────────────────────────────────┘
         │                                      │
         │ phases registry                      │ NewChildNamespace()
         │ (lazily created)                     │ (for R7RS (environment),
         ▼                                      │  (null-environment))
┌─────────────────────────────┐                 ▼
│     PhaseRegistry           │    ┌──────────────────────────────────────────┐
│                             │    │   Child Namespace              │
│  envs:                      │    │                                          │
│    0 → runtime EnvFrame ────┼──→ │  parent ──→ root Namespace     │
│    1 → expand EnvFrame      │    │  syntaxInterns ── nil (delegates up)     │
│    2 → compile EnvFrame     │    │  syntaxInterns ── nil (delegates up)     │
│   -1 → template EnvFrame    │    │  phases ────── own PhaseRegistry         │
│   ...                       │    │  runtime ───── own EnvironmentFrame      │
│                             │    │  libraryReg ── inherited (shared ptr)    │
│  owner → Namespace           │    └──────────────────────────────────────────┘
│                             │
└─────────────────────────────┘
```

---

## Phase Environments

Each phase has its own `GlobalEnvironmentFrame` (isolated bindings) but shares the `PhaseRegistry`, `Namespace`, and parents to the runtime frame.

```
Phase -1 (Template)     Phase 0 (Runtime)     Phase 1 (Expand)     Phase 2 (Compile)
┌──────────────┐       ┌──────────────┐       ┌──────────────┐    ┌──────────────┐
│ EnvFrame     │       │ EnvFrame     │       │ EnvFrame     │    │ EnvFrame     │
│              │       │              │       │              │    │              │
│ parent: ─────┼──┐    │ parent: nil  │  ┌──→ │ parent: ─────┼─┐  │ parent: ─────┼─┐
│ global: own  │  │    │ global: own  │  │    │ global: own  │ │  │ global: own  │ │
│ local: nil   │  │    │ local: nil   │  │    │ local: nil   │ │  │ local: nil   │ │
│ phaseLevel:-1│  │    │ phaseLevel:0 │  │    │ phaseLevel:1 │ │  │ phaseLevel:2 │ │
└──────────────┘  │    └──────────────┘  │    └──────────────┘ │  └──────────────┘ │
                  │           ▲          │           ▲         │          ▲        │
                  └───────────┘          └───────────┘         └──────────┘        │
                   parent→runtime         parent→runtime        parent→runtime     │
                                                                                   │
                  ┌────────────────────────────────────────────────────────────────┘
                  └──→ runtime (parent→runtime)
```

**Key:** Phase environments parent to runtime, not to each other. The phase hierarchy is flat — each phase has its own `GlobalEnvironmentFrame` but falls back to runtime globals through the parent pointer.

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

Created by `Namespace.NewChildRuntime()`. Shares syntax interning but has isolated bindings and phases.

```
Root Namespace                Library environment
┌──────────────────────┐               ┌──────────────────────┐
│  syntaxInterns: {...} │◄──────────────│  namespace: ─────────┤ (same pointer!)
│  syntaxInterns: {...} │  shared NS    │  global: OWN         │ (isolated bindings)
│  phases: rootPhases   │               │  phases: ownPhases   │ (isolated phases)
│  runtime: rootEnv     │               │  parent: nil         │
└──────────────────────┘               │  phaseLevel: 0       │
      │                                 └──────────────────────┘
      │ rootPhases                              │ ownPhases
      ▼                                         ▼
┌──────────┐                              ┌──────────┐
│ 0:runtime │                              │ 0:libRT  │  ← library's own runtime
│ 1:expand  │                              │ 1:libExp │  ← library's own expand
│ 2:compile │                              │ 2:libCmp │  ← library's own compile
└──────────┘                              └──────────┘
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
│ expanderCtx ── ExpanderContext       │
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
| Runtime frame | `ns.Runtime()` | Phase 0 global | Via Namespace | Shared | Normal execution |
| Expand frame | `env.Expand()` / `AtPhase(1)` | Phase 1 global | Via Namespace | Shared | Macro bindings |
| Compile frame | `env.Compile()` / `AtPhase(2)` | Phase 2 global | Via Namespace | Shared | Syntax compilers |
| Lexical child | `NewEnvironmentFrameWithParent()` | Own local, shared global | Via Namespace | Shared | `lambda`, `let`, `letrec` |
| Library env | `ns.NewChildRuntime()` | Own global + phases | Via shared Namespace | Own registry | `(import ...)` |
| let-syntax env | `NewEnvironmentFrameWithParent(local, p.env)` | Own local macros | Via Namespace | Shared | `let-syntax`, `letrec-syntax` |

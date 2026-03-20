# Namespace Migration

**Status:** Complete (PR #544, merged 2026-03-20)
**Date:** 2026-03-20

## Motivation

Wile's `TopLevelEnvironment` is already a namespace: it owns bindings, phases, library
registry, syntax interning, and the load-path stack. But the name doesn't reflect this,
and the capabilities are split across three types (`TopLevelEnvironment`, `Engine`,
`context.valueContext`) rather than unified on one.

This plan renames `TopLevelEnvironment` to `Namespace`, moves ownership of the registry
and authorizer onto it, adds module instance tracking, and exposes a Scheme-level API
for mutable namespace operations. The result is a single type that serves both Go
embedders and Scheme code.

### Design Constraints

- **Zero runtime regression.** Namespace resolution happens at compile time. Generated
  bytecode uses the same `GlobalIndex` direct-dispatch as today. The VM dispatch loop
  is untouched.
- **Backward-compatible Go API.** `NewEngine(WithExtension(...))` continues to work
  by creating an implicit namespace. No existing embedder code breaks.
- **Preserve invariants.** Every `EnvironmentFrame` has a `Namespace` (was
  `TopLevelEnvironment`). Syntax interning delegates up the parent chain. Libraries
  share the caller's `Namespace` for interning.

---

## Core Type: `Namespace`

`TopLevelEnvironment` becomes `Namespace`. Existing fields stay; new fields are added.

```go
// environment/namespace.go (renamed from top_level_environment.go)
type Namespace struct {
    name            string
    parent          *Namespace                     // syntax interning delegation (existing)
    syntaxInterns   map[values.Value]SyntaxValue    // existing
    phases          *PhaseRegistry                  // existing
    libraryRegistry any                             // existing
    loadPathStack   *LoadPathStack                  // existing
    runtime         *EnvironmentFrame               // existing

    // NEW
    moduleInstances map[string]*ModuleInstance       // keyed by resolved library path
    registry        *registry.Registry              // was on Engine
    authorizer      security.Authorizer             // was on context.valueContext
}

type ModuleInstance struct {
    env     *EnvironmentFrame
    exports map[string]*GlobalIndex
}
```

### What Moves

| Field | From | To | Why |
|-------|------|----|-----|
| `registry` | `Engine` | `Namespace` | Namespace owns "what bindings exist" |
| `authorizer` | `context.valueContext` | `Namespace` | Namespace owns "what operations are permitted" |

### What Stays

- `EnvironmentFrame`, `GlobalEnvironmentFrame`, `LocalEnvironmentFrame` — unchanged
- Parent chain for syntax interning delegation — unchanged
- `PhaseRegistry` — unchanged
- Compiler, expander, VM dispatch loop — unchanged
- Bytecode format — unchanged

---

## Go Embedding API

### Namespace Construction

```go
ns := wile.NewNamespace(ctx,
    wile.WithExtension(files.Extension),
    wile.WithExtension(math.Extension),
    wile.WithAuthorizer(security.ReadOnly()),
)
engine, _ := wile.NewEngine(ctx, wile.WithNamespace(ns))
```

### Backward Compatibility

`NewEngine` without `WithNamespace` creates a default namespace internally. All
existing option functions work on `NewEngine` directly.

```go
// This still works — creates namespace implicitly
engine, _ := wile.NewEngine(ctx,
    wile.WithExtension(math.Extension),
)
```

### Namespace Derivation

```go
base := wile.NewNamespace(ctx,
    wile.WithExtension(files.Extension),
    wile.WithExtension(math.Extension),
)

// Child: shared syntax interning, isolated bindings, restricted registry
sandbox := base.Derive(
    wile.WithoutExtension(files.Extension),
)

eng1, _ := wile.NewEngine(ctx, wile.WithNamespace(base))
eng2, _ := wile.NewEngine(ctx, wile.WithNamespace(sandbox))
```

`Derive` = shared syntax interning, isolated bindings (existing `NewChildTopLevelEnvironment`
path, renamed). `NewNamespace` = fully isolated.

### Copy Policy

The registry is **immutable after construction**. `buildRegistry` populates it,
`Apply` writes its bindings into the namespace's environment, and no code path
mutates it after that. This determines how `Derive` handles it:

| Derive form | Registry behavior | Why |
|-------------|-------------------|-----|
| `ns.Derive()` | Pointer share — parent and child reference the same `*Registry` | Immutable object; sharing is safe, zero allocation cost |
| `ns.Derive(WithoutExtension(...))` | Clone via `Registry.Without()` — new `*Registry` with primitives removed | Restriction requires a new object; parent's registry is unchanged |

The same policy applies to the authorizer: shared by pointer on `Derive()`,
overridden on `Derive(WithAuthorizer(...))`. Authorizers are stateless interfaces
(a single `Authorize(AccessRequest) error` method) — sharing is safe.

`NewNamespace` always builds a fresh registry from scratch. No sharing with any
other namespace.

### Engine API Changes

| Current | New | Notes |
|---------|-----|-------|
| `Engine.env` (private) | `Engine.Namespace()` (public) | First-class concept |
| `Engine.Define(name, val)` | Stays | Delegates to `ns.Define()` |
| `Engine.Get(name)` | Stays | Delegates to `ns.Get()` |
| `Engine.Eval(ctx, code)` | Stays | Uses engine's namespace |
| — | `Engine.EvalIn(ctx, code, ns)` | Eval in a different namespace |

### Namespace Methods

```go
ns.Define(name string, val values.Value) error
ns.Get(name string) (values.Value, error)
ns.BoundNames() []string
ns.IsBound(name string) bool
ns.Undefine(name string) error
ns.Derive(opts ...NamespaceOption) *Namespace
ns.Require(ctx context.Context, lib string) error
ns.AttachModule(path string, target *Namespace) error
```

---

## Scheme API

### Parameter

```scheme
(current-namespace)                       ; returns active namespace
(parameterize ([current-namespace ns])
  (eval '(define x 42)))                  ; defines x in ns
```

`eval` gains a 1-arg form: `(eval expr)` uses `(current-namespace)`.
The 2-arg form `(eval expr ns)` still works.

### Constructors

```scheme
(make-namespace)                          ; empty (kernel only)
(make-namespace '(scheme base))           ; pre-loaded
(make-namespace '(scheme base) '(scheme write))

(namespace-derive ns)                     ; shared interning, isolated bindings
(namespace-derive ns '(scheme write))     ; child + additional libs
```

### Binding Operations

```scheme
(namespace-define! ns 'x 42)
(namespace-ref ns 'x)                     ; error if unbound
(namespace-ref ns 'x default)             ; with default
(namespace-bound? ns 'x)
(namespace-undefine! ns 'x)
(namespace-bound-names ns)
```

### Dynamic Module Loading

```scheme
(namespace-require ns '(scheme write))
(namespace-require ns '(srfi 1))
```

### Predicates

```scheme
(namespace? obj)
(namespace-name ns)
```

### R7RS Compatibility

| R7RS / current Wile | Maps to | Notes |
|----------------------|---------|-------|
| `(environment '(scheme base))` | `(make-namespace '(scheme base))` | Returns namespace |
| `(interaction-environment)` | `(current-namespace)` | Live reference |
| `(scheme-report-environment 5)` | Snapshot copy | Frozen, not mutable |
| `(null-environment 5)` | `(make-namespace)` | Empty |
| `(eval expr env)` | `(eval expr ns)` | Namespaces are environments |
| `(environment-ref env sym)` | `(namespace-ref ns sym)` | Alias kept |
| `(environment-bound? env sym)` | `(namespace-bound? ns sym)` | Alias kept |
| `(environment-bound-names env)` | `(namespace-bound-names ns)` | Alias kept |
| `(environment? obj)` | `(namespace? obj)` | Both names work |

### Not Included

- `#%app` / `#%datum` / `#%top` interposition (Tier 4 — performance-negative)
- Automatic source-location marks (Tier 4)
- Inspectors / custodians (Tier 4)

---

## Internal Migration

### Phase 1: Mechanical Rename

Pure rename, zero behavior change. ~143 downstream files.

| Before | After |
|--------|-------|
| `TopLevelEnvironment` | `Namespace` |
| `NewTopLevelEnvironment()` | `NewNamespace()` |
| `NewTopLevelEnvironmentFrame()` | `NewNamespaceFrame()` |
| `NewChildTopLevelEnvironment()` | Kept internally, exposed as `Derive()` |
| `NewChildRuntime()` | Stays (internal to `Namespace`) |
| `env.TopLevelEnv()` | `env.Namespace()` |
| `env.topLevel` | `env.namespace` |

### Phase 2: Move Registry from Engine to Namespace

- Add `registry` field to `Namespace`
- Move `buildRegistry()` and `applyBaseEnvironment()` from `Engine` to `Namespace`
- `NewNamespace(ctx, opts...)` accepts `WithExtension`, `WithRegistry`, `WithoutCore`
- `NewEngine` without `WithNamespace` creates a default namespace from its options
- Add `Namespace.Derive(opts...)`

### Phase 3: Move Authorizer from context to Namespace

- Add `authorizer` field to `Namespace`
- Gate sites change from `security.Check(ctx, ...)` to
  `security.Check(mc.Namespace().Authorizer(), ...)`
- Remove `context.WithValue` security plumbing
- `Derive` inherits parent's authorizer; `WithAuthorizer` overrides

Closes TODO.md item: "Security context — Authorizer rides on `context.valueContext`."

### Phase 4: Module Instance Tracking

- Add `moduleInstances` field to `Namespace`
- Library loader caches instances after initialization
- Subsequent loads reuse cached instance (copy exports, skip init)
- `Derive` starts with empty instance table
- `AttachModule` copies instance from one namespace to another
- Add `GlobalEnvironmentFrame.DeleteBinding()` for `namespace-undefine!`

### Phase 5: Scheme API

- New extension `internal/extensions/namespace/`
- All Scheme primitives from the API section above
- `current-namespace` parameter
- `eval` 1-arg form
- `environment-*` aliases in `extensions/introspection/`
- Test file: `test/scheme/namespace-test.scm`

---

## Execution: PR Sequence

Each PR is independently mergeable. Build stays green after each.

```
PR 1 (rename)           ~143 files, zero behavior change
  ↓
PR 2 (registry move)    Engine → Namespace, backward compat
  ↓
PR 3 (authorizer move)  context → Namespace, closes TODO item
  ↓
PR 4 (module instances)  caching, Derive isolation, AttachModule
  ↓
PR 5 (Scheme API)        primitives, current-namespace, aliases, tests
```

PR 2 and PR 3 are independent of each other (both depend on PR 1, not on each other).
PR 4 depends on PR 2. PR 5 depends on all previous.

---

## References

- `docs/dev/ENVIRONMENT_SYSTEM.md` — current environment architecture
- `docs/learn/racket-namespaces.md` — Racket namespace concepts
- `docs/learn/racket-low-level-primitives.md` — feasibility analysis (§3 Binding Manipulation)
- `plans/ENVIRONMENT-CLEANUP.md` — prior cleanup (complete)
- `plans/ARCHITECTURE.md` — dialect system proposal (related, orthogonal)
- `plans/SECURITY.md` — authorizer framework (phases 1-6 complete)

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
│  runtime ────────── *EnvironmentFrame (phase 0, mutable user global)    │
│  sealedBase ─────── *EnvironmentFrame (phase 0, immutable, parent nil)  │
│  sealedExpandBase ─ *EnvironmentFrame (phase 1, immutable)              │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ owns
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         EnvironmentFrame                                │
│  (Lexical scope node: links local/global bindings, parent chain)        │
│                                                                         │
│  parent ─────────── *EnvironmentFrame (lexical parent, nil at top)      │
│  local ──────────── LocalEnvironmentFrame (value; keys==nil → none)     │
│  global ─────────── *GlobalEnvironmentFrame (define bindings)           │
│  phaseLevel ─────── Phase (-1=template, 0=runtime, 1=expand, 2=compile) │
│  phases ─────────── *PhaseRegistry (shared reference)                   │
│  namespace ──────── *Namespace (back-reference)                         │
└─────────────────────────────────────────────────────────────────────────┘
          │                                    │
          │ contains                           │ contains
          ▼                                    ▼
┌───────────────────────────┐    ┌────────────────────────────────────────┐
│  LocalEnvironmentFrame    │    │      GlobalEnvironmentFrame            │
│  (Single scope bindings)  │    │  (Phase-wide global bindings)          │
│                           │    │                                        │
│  keys ── map[Symbol][]int │    │  keys ────── map[Symbol][]int          │
│  bindings ── []Binding    │    │  bindings ──── []*Binding              │
└───────────────────────────┘    └────────────────────────────────────────┘
```

---

## Namespace

The `Namespace` is the root of the environment hierarchy. Each Wile VM instance has exactly one Namespace which owns:

- **Syntax interning table**: Caches syntax objects for consistent identity across macro expansion
- **Phase registry**: O(1) access to any phase environment
- **Library registry**: Tracks loaded R7RS libraries

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

Wile supports multiple phases for macro expansion:

| Phase | Constant | Purpose |
|-------|----------|---------|
| -1 | `PhaseTemplate` | Template phase (for-template, future) |
| 0 | `PhaseRuntime` | Normal program execution |
| 1 | `PhaseExpand` | Macro expansion (for-syntax) |
| 2 | `PhaseCompile` | Compile-time (for-meta 2) |

### Accessing Phase Environments

```go
env := environment.NewNamespaceFrame()

// Direct phase access
runtime := env.AtPhase(0)   // Same as env.Runtime()
expand := env.AtPhase(1)    // Same as env.Expand()
compile := env.AtPhase(2)   // Same as env.Compile()

// Convenience methods
env.Runtime()   // Phase 0
env.Expand()    // Phase 1
env.Compile()   // Phase 2
```

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
   - Lookup uses maximal subset match (`bindingScopes ⊆ useScopes`, largest wins);
     first frame up the parent chain that yields a match wins, which is what
     preserves sealed-base shadowing
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

6. **Sealed base (phase 0) and sealed expand base (phase 1) are per-namespace
   immutable frames, reached only via the parent chain**
   - Each `Namespace` owns two sealed frames that are **not** `PhaseRegistry`
     entries: `sealedBase` (phase 0: Go primitives + sealed stdlib procedures +
     optimizer `Stable` anchors) and `sealedExpandBase` (phase 1: bootstrap
     macros + special-form primitive expanders). The mutable phase frames parent
     to them:

     ```
     phase 0:  runtime          → sealedBase          → nil
                                  (prims + sealed procs)
     phase 1:  expand-child      → sealedExpandBase    → sealedBase → nil
               (user define-syntax   (bootstrap macros,
                lands here)           special-form expanders)
     phase 2+: compile           → sealedBase          → nil
     ```

   - `createPhaseEnv` parents each phase frame to that phase's seal via
     `phaseParent`, and only for a namespace that owns one (`IsNamespaceRuntime`).
     A flat `NewChildRuntime` library frame owns no seal and stays a flat island
     (library isolation). A phase with no seal parents straight to `sealedBase`,
     preserving hermeticity and the climbing-tower invariant that higher phases
     never introduce a phase→phase parent edge.
   - **Why `sealedExpandBase` is distinct from `sealedBase`, not a reuse.** A
     compile-time handler (a bootstrap macro, `BindingTypeSyntax`, or a
     special-form expander, `BindingTypePrimitive`) placed on the phase-0 value
     frame is reachable by **runtime value resolution** (the runtime frame
     parents to `sealedBase`). That is a phase confusion: a dialect that removes
     a form (`Dialect.Forms().Remove`) would then leak the form's
     `#<primitive-expander:…>` into the value world instead of the name being
     unbound. The phase-1 `sealedExpandBase` is on the expand chain only, so it is
     invisible to phase-0 value resolution.
   - **A top-level `(define-syntax foo …)` shadows, it does not overwrite.** It
     lands in the mutable expand child (`AtPhase(PhaseExpand)` from the mutable
     runtime), a different frame from the pinned bootstrap `foo` in
     `sealedExpandBase`. Bootstrap macros compile with `env == sealedBase`, so
     their `define-syntax` writes climb into `sealedExpandBase` via `AtPhase`;
     expanders register there directly via
     `SealedTargetAt(PhaseExpand, SealKindHandler)`.
   - **The seal is keyed by `(phase, kind)`, and `sealedAxis` is the only place
     that decides.** Phase 0 seals both kinds; phase 1 seals handlers only, which
     is why registry expand-phase primitives stay in the mutable expand child
     while special-form expanders do not. Phase 2 and above have no seal, so a
     `define-syntax` inside a transformer body climbs off the sealed axis.
     `SealedAt` reports the pair's seal and whether there is one; a row that
     declares a seal the namespace never built panics (`mustSeal`) rather than
     degrading to the mutable frame. The parent link is kind-independent, so
     `phaseParent` asks `sealedFrameAt`, not `SealedAt`.
   - **Enumeration must span every seal.** Because no sealed frame is a
     `PhaseRegistry` entry, name/doc walks (`BoundNamesAcrossPhases`, `,apropos`)
     iterate `SealedFrames()`, or primitive / bootstrap-macro names silently
     vanish from introspection.

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
- `GlobalEnvironmentFrame` keys/slots - Thread-safe (RWMutex; `CreateGlobalBinding` takes the write lock for its check-then-write)
- Global `Binding` value and metadata - Thread-safe: a global binding's value and its `*BindingMeta` live in an `atomicCell`, read lock-free and published by store / copy-on-write CAS (`Binding.UpdateMeta`). Never write a global's meta field in place
- Local `Binding` operations - Not thread-safe (locals are frame-private, single-threaded compilation assumed)

---

## References

- R7RS §6.5: Symbols - Symbol identity requirements
- Flatt 2016: "Binding as Sets of Scopes" - Hygiene model
- `pkg/environment/` - Implementation
- `pkg/internal/bootstrap/bootstrap.go` - Runtime initialization (`NewNamespaceFrame`, `NewLibraryEnvironmentFrame`)

# Environment System

This document describes Wile's environment system, which manages variable bindings, syntax interning, and the phase hierarchy for macro expansion.

---

## Overview

The environment system has four key types organized in a hierarchy:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        Namespace                              │
│  (Per-VM instance: owns syntax interning, phases, libraries)           │
│                                                                         │
│  syntaxInterns ──── map[Value]SyntaxValue (thread-safe, per-instance)  │
│  phases ─────────── *PhaseRegistry                                     │
│  libraryRegistry ── any (*machine.LibraryRegistry)                     │
│  runtime ────────── *EnvironmentFrame (phase 0)                        │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ owns
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         EnvironmentFrame                                │
│  (Lexical scope node: links local/global bindings, parent chain)       │
│                                                                         │
│  parent ─────────── *EnvironmentFrame (lexical parent, nil at top)     │
│  local ──────────── LocalEnvironmentFrame (value; keys==nil → none)    │
│  global ─────────── *GlobalEnvironmentFrame (define bindings)          │
│  phaseLevel ─────── int (0=runtime, 1=expand, 2=compile)               │
│  phases ─────────── *PhaseRegistry (shared reference)                  │
│  namespace ───────── *Namespace (back-reference)                        │
└─────────────────────────────────────────────────────────────────────────┘
          │                                    │
          │ contains                           │ contains
          ▼                                    ▼
┌───────────────────────────┐    ┌────────────────────────────────────────┐
│  LocalEnvironmentFrame    │    │      GlobalEnvironmentFrame            │
│  (Single scope bindings)  │    │  (Phase-wide global bindings)          │
│                           │    │                                        │
│  keys ─── map[Symbol]int  │    │  keys ──────── map[Symbol]int          │
│  bindings ── []*Binding   │    │  bindings ──── []*Binding              │
└───────────────────────────┘    │  namespace ──── *Namespace             │
                                 └────────────────────────────────────────┘
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

// Convenience wrapper (creates Namespace internally)
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
// In internal/bootstrap/environment_tiny.go
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
    "github.com/aalpar/wile/environment"
    "github.com/aalpar/wile/internal/bootstrap"
    "github.com/aalpar/wile/machine"
)

func setupRuntime(ctx context.Context) (*environment.EnvironmentFrame, error) {
    // Create complete environment with primitives and macros
    env, err := bootstrap.NewNamespaceFrameTiny(ctx)
    if err != nil {
        return nil, err
    }

    // Set up library loading
    registry := machine.NewLibraryRegistry()
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
    env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
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

// Create a global binding
sym := values.NewSymbol("foo")
gi, created := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
if created {
    env.SetOwnGlobalValue(gi, values.NewInteger(42))
}

// Look up a binding
binding := env.GetBinding(sym)
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
1. Absolute path     → use as-is (if exists)
2. Stack-relative    → path relative to stack.CurrentDir()
3. Fallback dirs     → SCHEME_INCLUDE_PATH, CWD
```

Stack-relative takes precedence over fallback directories. Error messages list all searched paths.

### Integration Points

All three file-loading operations push/pop the stack:

| Operation | Location | Phase |
|-----------|----------|-------|
| `load` | `internal/extensions/eval/prim_eval.go` | Runtime |
| `include` | `machine/compile_time_continuation_include.go` | Compile-time |
| `import` (library loading) | `machine/library_loader.go` | Compile-time |

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
- Binding operations - Not thread-safe (single-threaded compilation assumed)

---

## References

- R7RS §6.5: Symbols - Symbol identity requirements
- Flatt 2016: "Binding as Sets of Scopes" - Hygiene model
- `environment/` - Implementation
- `internal/bootstrap/environment_tiny.go` - Runtime initialization

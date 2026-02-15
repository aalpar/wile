# Environment System

This document describes Wile's environment system, which manages variable bindings, symbol interning, and the phase hierarchy for macro expansion.

---

## Overview

The environment system has four key types organized in a hierarchy:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        TopLevelEnvironment                              │
│  (Per-VM instance: owns symbol/syntax interning, phases, libraries)    │
│                                                                         │
│  symbolInterns ──── map[Symbol]*Symbol (thread-safe, per-instance)     │
│  syntaxInterns ──── map[Value]SyntaxValue (thread-safe)                │
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
│  local ──────────── *LocalEnvironmentFrame (lambda params, let vars)   │
│  global ─────────── *GlobalEnvironmentFrame (define bindings)          │
│  phaseLevel ─────── int (0=runtime, 1=expand, 2=compile)               │
│  phases ─────────── *PhaseRegistry (shared reference)                  │
│  topLevel ───────── *TopLevelEnvironment (back-reference)              │
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
└───────────────────────────┘    │  topLevel ──── *TopLevelEnvironment    │
                                 └────────────────────────────────────────┘
```

---

## TopLevelEnvironment

The `TopLevelEnvironment` is the root of the environment hierarchy. Each Wile VM instance has exactly one TopLevelEnvironment which owns:

- **Symbol interning table**: Ensures `(eq? 'foo 'foo)` is `#t` per R7RS §6.5
- **Syntax interning table**: Caches syntax objects for hygiene
- **Phase registry**: O(1) access to any phase environment
- **Library registry**: Tracks loaded R7RS libraries

### Creating a TopLevelEnvironment

```go
// Basic creation
topLevel := environment.NewTopLevelEnvironment()
env := topLevel.Runtime()  // Get the runtime (phase 0) environment

// Convenience wrapper (creates TopLevelEnvironment internally)
env := environment.NewTopLevelEnvironmentFrame()
```

### Symbol Interning

Symbol interning is **per-TopLevelEnvironment**, ensuring symbol identity works correctly:

```go
topLevel := environment.NewTopLevelEnvironment()

// These will be eq? because they're interned in the same TopLevelEnvironment
sym1 := topLevel.InternSymbol(values.NewSymbol("foo"))
sym2 := topLevel.InternSymbol(values.NewSymbol("foo"))
// sym1 == sym2 (pointer equality)

// Symbols from different TopLevelEnvironments are NOT eq?
otherTopLevel := environment.NewTopLevelEnvironment()
sym3 := otherTopLevel.InternSymbol(values.NewSymbol("foo"))
// sym1 != sym3 (different instances)
```

This design enables:
- Multiple isolated Wile VMs in one Go process
- Clean VM teardown without affecting other instances
- R7RS §6.5 compliance: "Two symbols are identical if and only if their names are spelled the same way"

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
env := environment.NewTopLevelEnvironmentFrame()

// Direct phase access
runtime := env.AtPhase(0)   // Same as env.Runtime()
expand := env.AtPhase(1)    // Same as env.Expand()
compile := env.AtPhase(2)   // Same as env.Compile()

// Convenience methods
env.Runtime()   // Phase 0
env.Expand()    // Phase 1
env.Compile()   // Phase 2
```

Each phase has its own `GlobalEnvironmentFrame` for bindings but shares the `TopLevelEnvironment` for interning.

---

## Creating Child Environments

### For Lexical Scopes (lambda, let, etc.)

Use `NewEnvironmentFrameWithParent` to create child environments that inherit from a parent:

```go
// Create a top-level environment
topEnv := environment.NewTopLevelEnvironmentFrame()

// Create a child with local bindings (e.g., for lambda parameters)
localEnv := environment.NewLocalEnvironment(2)  // 2 parameters
childEnv := environment.NewEnvironmentFrameWithParent(localEnv, topEnv)

// The child shares:
// - Same GlobalEnvironmentFrame as parent
// - Same PhaseRegistry
// - Same TopLevelEnvironment (for interning)
```

### For Libraries

Libraries need isolated bindings but must share the `TopLevelEnvironment` for symbol identity:

```go
// WRONG: Creates new TopLevelEnvironment, breaks symbol eq?
// libEnv := environment.NewTopLevelEnvironmentFrame()

// CORRECT: Share TopLevelEnvironment with caller
callerTopLevel := callerEnv.TopLevelEnv()
libEnv := callerTopLevel.NewChildRuntime()
```

The `NewChildRuntime` method creates an environment that:
- Has its own `GlobalEnvironmentFrame` (isolated bindings)
- Has its own `PhaseRegistry` (isolated phase hierarchy)
- Shares the `TopLevelEnvironment` (symbol/syntax interning)

---

## Library Environment Factory

The `machine.LibraryEnvFactory` function creates environments for R7RS libraries. It must share the caller's `TopLevelEnvironment`:

```go
// In internal/bootstrap/environment_tiny.go
func NewLibraryEnvironmentFrame(ctx context.Context, callerEnv *environment.EnvironmentFrame) (*environment.EnvironmentFrame, error) {
    // Get caller's TopLevelEnvironment
    callerTopLevel := callerEnv.TopLevelEnv()
    if callerTopLevel == nil {
        return nil, errors.New("caller has no TopLevelEnvironment")
    }

    // Create child sharing the TopLevelEnvironment
    libEnv := callerTopLevel.NewChildRuntime()

    // Register primitives, macros, etc.
    // ...

    return libEnv, nil
}

// In main.go or engine setup
machine.LibraryEnvFactory = bootstrap.NewLibraryEnvironmentFrame
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
    env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(ctx)
    if err != nil {
        return nil, err
    }

    // Set up library loading
    registry := machine.NewLibraryRegistry()
    env.SetLibraryRegistry(registry)

    // Configure library environment factory (shares TopLevelEnvironment)
    machine.LibraryEnvFactory = bootstrap.NewLibraryEnvironmentFrame

    return env, nil
}
```

### Creating Test Environments

```go
func TestSomething(t *testing.T) {
    // Simple environment for unit tests
    env := environment.NewTopLevelEnvironmentFrame()

    // Or with full primitives
    env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
    if err != nil {
        t.Fatal(err)
    }

    // For tests involving libraries
    machine.LibraryEnvFactory = bootstrap.NewLibraryEnvironmentFrame
    defer func() { machine.LibraryEnvFactory = nil }()
}
```

### Accessing Bindings

```go
env := environment.NewTopLevelEnvironmentFrame()

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

1. **Every EnvironmentFrame has a TopLevelEnvironment**
   - Use `NewTopLevelEnvironmentFrame()` or `NewEnvironmentFrameWithParent()` with a valid parent
   - Never call `NewEnvironmentFrameWithParent(local, nil)` - it will panic

2. **Symbol interning requires TopLevelEnvironment**
   - `InternSymbol()` panics if `topLevel` is nil
   - Always create environments properly

3. **Libraries share TopLevelEnvironment with caller**
   - `LibraryEnvFactory` must use caller's TopLevelEnvironment
   - Failure breaks `(eq? 'foo (string->symbol "foo"))` per R7RS §6.5

4. **Phase environments share TopLevelEnvironment**
   - All phases use the same interning tables
   - Expand-phase macros can reference runtime symbols correctly

---

## Migration from Legacy Environments

The following patterns are deprecated and will panic:

```go
// DEPRECATED: Creates environment without TopLevelEnvironment
env := environment.NewEnvironmentFrame(nil, nil)

// DEPRECATED: Creates GlobalEnvironmentFrame without TopLevelEnvironment
genv := environment.NewGlobalEnvironmentFrame()
env := environment.NewEnvironmentFrame(nil, genv)

// DEPRECATED: Parent cannot be nil
env := environment.NewEnvironmentFrameWithParent(local, nil)
```

Replace with:

```go
// CORRECT: Creates environment with proper TopLevelEnvironment
env := environment.NewTopLevelEnvironmentFrame()

// CORRECT: Create child with parent
childEnv := environment.NewEnvironmentFrameWithParent(local, parentEnv)
```

---

## Load-Path Stack

The `LoadPathStack` enables relative path resolution for `load`, `include`, and `import` by tracking which files are currently being loaded. It is stored on `TopLevelEnvironment` (per-VM, not per-thread).

```
┌─────────────────────────────────────────────────────────┐
│  LoadPathStack (environment/load_path_stack.go)         │
│                                                         │
│  paths []string    ← LIFO stack of absolute file paths  │
│  mu    sync.Mutex  ← thread-safe access                 │
│                                                         │
│  Push(absPath) → Pop() → Current() → CurrentDir()      │
└─────────────────────────────────────────────────────────┘
```

### Resolution Strategy

`ResolveFile` (`environment/resolve.go`) resolves filenames using a 3-tier strategy:

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
    _, err := engine.Eval(ctx, `(load "helper.scm")`) // resolves relative to /app/scripts/
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

The stack lives on `TopLevelEnvironment`, shared across all child environments via delegation. This is intentional: library loading must resolve paths relative to the importing file even when the library runs in its own isolated environment.

**Concurrency caveat**: Concurrent `(load ...)` from multiple SRFI-18 threads can corrupt LIFO ordering. Single-threaded loading (the common case) is fully correct.

---

## Thread Safety

- `TopLevelEnvironment.InternSymbol()` - Thread-safe (uses RWMutex)
- `TopLevelEnvironment.InternSyntax()` - Thread-safe (uses RWMutex)
- `PhaseRegistry.GetOrCreate()` - Thread-safe (uses RWMutex)
- `LoadPathStack` - Thread-safe for individual operations (uses Mutex); LIFO ordering only guaranteed single-threaded
- Binding operations - Not thread-safe (single-threaded compilation assumed)

---

## References

- R7RS §6.5: Symbols - Symbol identity requirements
- Flatt 2016: "Binding as Sets of Scopes" - Hygiene model
- `environment/` - Implementation
- `internal/bootstrap/environment_tiny.go` - Runtime initialization

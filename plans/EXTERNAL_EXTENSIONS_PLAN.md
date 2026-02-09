# External Extensions Plan

**Status:** PROPOSED — Not yet implemented

> **Cross-reference**: See also `PLUGIN_ARCHITECTURE_PROPOSAL.md` for the broader architectural design (Registry, Extension interface, REPL separation, package organization). This document focuses specifically on the `EnvironmentAccess` interface needed to decouple extensions from internal packages.

This document describes how to make Wile's extension system publicly consumable so extensions can live in separate repositories.

## Problem Statement

Extensions currently live in `internal/extensions/`, which prevents external linking due to Go's `internal` package visibility rules. This blocks:

1. **Modular adoption** — Users who only need I/O primitives must import all extensions
2. **External libraries** — Third-party extensions (Kubernetes API, database drivers) can't be implemented
3. **Ecosystem growth** — Community extensions can't `go get` against Wile's public API

## Current Architecture

```
wile/                           # Public: Engine, WithExtension()
├── registry/                   # Public: Extension interface, PrimitiveSpec, Registry
├── machine/                    # Public: ForeignFunction, MachineContext
├── values/                     # Public: All value types
├── environment/                # Public package, but extensions access via internal
└── internal/
    └── extensions/             # BLOCKED: Extension implementations
        ├── io/
        ├── math/
        ├── eval/
        └── ...
```

**What's already public and stable:**
- `registry.Extension` interface
- `registry.PrimitiveSpec` for defining primitives
- `registry.Registry` methods: `AddPrimitive`, `AddPrimitives`, `AddInitFunc`, `AddMacroSource`
- `machine.ForeignFunction` type signature
- `machine.MachineContext` full API
- `values.*` all value types and error constructors
- `wile.WithExtension()` option

**Blocked by current design:**
- `registry.ApplyContext.Environment()` returns `*environment.EnvironmentFrame`
- Extensions import `environment` directly for constants and methods

## Audit: What Extensions Actually Need

Extensions use only **14 distinct operations** from the environment package:

### Symbol Interning
| Operation | Used By |
|-----------|---------|
| `InternSymbol` | io, eval, system, registry/core |

### Global Binding Management
| Operation | Used By |
|-----------|---------|
| `MaybeCreateOwnGlobalBinding` | io, registry/apply |
| `SetOwnGlobalValue` | io, registry/apply |
| `NewGlobalIndex` | registry/apply |

### Phase Access
| Operation | Used By |
|-----------|---------|
| `Runtime()` | eval, registry/apply |
| `Expand()` | eval, registry/apply |
| `Compile()` | registry/apply |

### Scope/Frame Navigation
| Operation | Used By |
|-----------|---------|
| `TopLevel()` | eval, all, registry/core |
| `TopLevelEnv()` | eval, registry/core |

### Hygienic Lookup
| Operation | Used By |
|-----------|---------|
| `GetBindingWithScopes` | eval, registry/core |

### First-Class Environments
| Operation | Used By |
|-----------|---------|
| `NewChildTopLevelEnvironment` | eval |

### Constants
| Constant | Used By |
|----------|---------|
| `BindingTypeVariable` | io, registry/apply |
| `BindingTypePrimitive` | registry/apply |

## Proposed Solution: Widen ApplyContext Interface

Instead of exposing the entire `environment` package internals, expand `registry.ApplyContext` to provide only the operations extensions need through a clean interface.

### New Interface Design

```go
// registry/apply_context.go

// ApplyContext provides access to the environment during extension initialization.
// This is the public API that external extensions should depend on.
type ApplyContext interface {
    // Context returns the context.Context for this registration.
    Context() context.Context

    // Env returns the environment accessor for this phase.
    Env() EnvironmentAccess
}

// EnvironmentAccess provides the subset of environment operations
// that extensions need. This isolates extensions from internal
// environment implementation details.
type EnvironmentAccess interface {
    // InternSymbol interns a symbol, ensuring pointer equality for
    // symbols with the same name. Required for R7RS §6.5 compliance.
    InternSymbol(sym *values.Symbol) *values.Symbol

    // DefineVariable creates or updates a global variable binding.
    // This is the primary way extensions register runtime values.
    DefineVariable(name string, value values.Value) error

    // DefineParameter creates a global parameter binding.
    // Used for current-input-port, current-output-port, etc.
    DefineParameter(name string, param Parameter) error

    // Runtime returns the runtime phase environment accessor.
    Runtime() EnvironmentAccess

    // Expand returns the expand phase environment accessor.
    Expand() EnvironmentAccess

    // Compile returns the compile phase environment accessor.
    Compile() EnvironmentAccess
}

// Parameter represents a dynamically-scoped parameter object.
// This mirrors machine.Parameter but is defined here for the public API.
type Parameter interface {
    values.Value
    Get() values.Value
    Set(values.Value)
}
```

### What This Enables

**External extension code:**
```go
// github.com/aalpar/wile-kubernetes

package kubernetes

import (
    "github.com/aalpar/wile/registry"
    "github.com/aalpar/wile/machine"
    "github.com/aalpar/wile/values"
)

var Extension = registry.NewExtension("kubernetes", addToRegistry)

func addToRegistry(r *registry.Registry) error {
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"k8s-list-pods", 1, false, listPods},
        {"k8s-get-pod", 2, false, getPod},
    }, registry.PhaseRuntime)

    r.AddInitFunc(func(ctx registry.ApplyContext) error {
        // Register default namespace parameter
        param := newNamespaceParam("default")
        return ctx.Env().DefineParameter("current-k8s-namespace", param)
    })

    return nil
}

func listPods(ctx context.Context, mc *machine.MachineContext) error {
    namespace := mc.Arg(0)
    // ... implementation ...
    mc.SetValue(result)
    return nil
}
```

**User application code:**
```go
package main

import (
    "github.com/aalpar/wile"
    kubernetes "github.com/aalpar/wile-kubernetes"
)

func main() {
    engine, _ := wile.NewEngine(
        wile.WithExtension(kubernetes.Extension),
    )
    engine.EvalString(`(k8s-list-pods "kube-system")`)
}
```

## Migration Strategy

### Phase 1: Design the Public Interface

1. **Define `EnvironmentAccess` interface** in `registry/apply_context.go`
2. **Define `Parameter` interface** for parameter objects
3. **Keep `ApplyContext.Environment()` temporarily** for backwards compatibility
4. **Add `ApplyContext.Env()` returning `EnvironmentAccess`**

### Phase 2: Implement the Adapter

Create an adapter that implements `EnvironmentAccess` by wrapping `*environment.EnvironmentFrame`:

```go
// registry/env_access.go

type envAccessAdapter struct {
    frame *environment.EnvironmentFrame
}

func (a *envAccessAdapter) InternSymbol(sym *values.Symbol) *values.Symbol {
    return a.frame.InternSymbol(sym)
}

func (a *envAccessAdapter) DefineVariable(name string, value values.Value) error {
    sym := a.frame.InternSymbol(values.NewSymbol(name))
    idx, _ := a.frame.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
    return a.frame.SetOwnGlobalValue(idx, value)
}

func (a *envAccessAdapter) DefineParameter(name string, param Parameter) error {
    return a.DefineVariable(name, param.(values.Value))
}

func (a *envAccessAdapter) Runtime() EnvironmentAccess {
    return &envAccessAdapter{frame: a.frame.Runtime()}
}

func (a *envAccessAdapter) Expand() EnvironmentAccess {
    return &envAccessAdapter{frame: a.frame.Expand()}
}

func (a *envAccessAdapter) Compile() EnvironmentAccess {
    return &envAccessAdapter{frame: a.frame.Compile()}
}
```

### Phase 3: Migrate Internal Extensions

Update existing extensions to use the new interface:

**Before (io/register.go):**
```go
func registerPortParameters(ctx registry.ApplyContext) {
    env := ctx.Environment()
    sym := env.InternSymbol(values.NewSymbol("current-input-port"))
    idx, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
    env.SetOwnGlobalValue(idx, GetCurrentInputPortParam())
}
```

**After:**
```go
func registerPortParameters(ctx registry.ApplyContext) error {
    return ctx.Env().DefineParameter("current-input-port", GetCurrentInputPortParam())
}
```

### Phase 4: Handle Advanced Use Cases

Some extensions need more than basic variable definition:

#### Hygienic Lookup (eval, registry/core)

Add to `EnvironmentAccess`:
```go
// LookupWithScopes finds a binding using scope-aware matching.
// Used for hygienic macro expansion. Returns nil if not found.
LookupWithScopes(sym *values.Symbol, scopes []*syntax.Scope) Binding
```

This requires exposing `syntax.Scope` publicly, or creating an opaque `ScopeSet` type.

#### First-Class Environments (eval)

Add to `EnvironmentAccess`:
```go
// NewChildEnvironment creates an isolated child environment that
// shares symbol interning but has independent bindings.
// Used for (environment) and (null-environment) primitives.
NewChildEnvironment() EnvironmentAccess
```

#### TopLevel Capture (all, registry/core)

Extensions that capture environments in closures need:
```go
// TopLevel returns the top-level (outermost) environment.
TopLevel() EnvironmentAccess

// Frame returns the underlying frame for use in closures.
// This is an escape hatch for advanced use cases.
Frame() interface{}
```

### Phase 5: Deprecate Direct Environment Access

1. Mark `ApplyContext.Environment()` as deprecated
2. Update documentation to use `Env()` interface
3. Remove `Environment()` in next major version

### Phase 6: Extract Extensions to Separate Repos

Once the public API is stable:

| Current Location | New Repository |
|------------------|----------------|
| `internal/extensions/io` | `github.com/aalpar/wile-io` |
| `internal/extensions/math` | `github.com/aalpar/wile-math` |
| `internal/extensions/eval` | `github.com/aalpar/wile-eval` |
| `internal/extensions/files` | `github.com/aalpar/wile-files` |
| `internal/extensions/system` | `github.com/aalpar/wile-system` |
| `internal/extensions/exceptions` | (merge into core or wile-control) |
| `internal/extensions/all` | `github.com/aalpar/wile-all` (meta-package) |

The `wile-all` package would import all standard extensions:
```go
package all

import (
    wileio "github.com/aalpar/wile-io"
    wilemath "github.com/aalpar/wile-math"
    // ...
)

var Extensions = []registry.Extension{
    wileio.Extension,
    wilemath.Extension,
    // ...
}
```

## Backwards Compatibility

### For Internal Code

- `ApplyContext.Environment()` continues to work during migration
- Extensions can be migrated incrementally
- Tests continue to pass throughout

### For External Adopters

- Public API is additive (new interface, not breaking changes)
- Version the API: `registry/v1` or semver on the module

### Versioning Strategy

```
v0.x.x  - Current: internal extensions only
v1.0.0  - Public EnvironmentAccess interface, extensions still internal
v1.1.0  - Extensions extracted to separate repos, deprecated Environment()
v2.0.0  - Remove Environment(), EnvironmentAccess is the only API
```

## Open Questions

1. **Scope exposure**: Should `syntax.Scope` be public, or should we create an opaque `ScopeSet` type for hygienic lookup?

2. **Parameter type**: Should `registry.Parameter` be an interface or a concrete type? Interface is more flexible but adds complexity.

3. **Error handling**: Should `DefineVariable` return an error, or panic on failure? Current code ignores the bool return from `MaybeCreateOwnGlobalBinding`.

4. **Naming**: `Env()` vs `Environment()` vs `Access()` for the new method?

5. **Extension dependencies**: Should extensions be able to depend on other extensions? (e.g., kubernetes extension depends on io extension for port operations)

## File Changes Summary

| File | Change |
|------|--------|
| `registry/apply_context.go` | New file: `EnvironmentAccess` interface |
| `registry/env_access.go` | New file: adapter implementation |
| `registry/apply.go` | Add `Env()` method to `applyContext` |
| `internal/extensions/io/register.go` | Migrate to `Env().DefineParameter()` |
| `internal/extensions/eval/prim_eval.go` | Migrate to `EnvironmentAccess` |
| `internal/extensions/system/prim_system.go` | Migrate to `Env().InternSymbol()` |
| `internal/extensions/all/prim_all.go` | Migrate to `Env().TopLevel()` |
| `registry/core/prim_syntax.go` | Migrate hygienic lookup |
| `registry/core/prim_strings.go` | Migrate symbol interning |
| `registry/core/prim_control.go` | Migrate TopLevel capture |

## Success Criteria

1. All existing tests pass
2. New `EnvironmentAccess` interface covers all extension use cases
3. At least one extension successfully extracted to external repo
4. External extension can be imported via `go get` and used with `WithExtension()`
5. Documentation explains how to write external extensions

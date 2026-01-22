# CLAUDE.md

Package `registry` provides a plugin architecture for registering Scheme primitives.

## Purpose

- Central registry for primitive registration
- Phase-aware registration (runtime, expand-time, compile-time)
- Extension interface for modular primitive packages
- Builder pattern for composing multiple extensions

## Key Types

| Type | Purpose |
|------|---------|
| `Registry` | Central registry holding primitives, bindings, init funcs |
| `PrimitiveSpec` | Defines a primitive (name, param count, variadic, impl) |
| `Phase` | Bit flags for runtime/expand/compile phases |
| `Extension` | Interface for loadable extensions |
| `RegistryBuilder` | Collects registration functions |
| `ApplyContext` | Context passed to init functions |

## Key Files

| File | Purpose |
|------|---------|
| `registry.go` | Registry type and primitive registration |
| `apply.go` | Apply registry to environment |
| `builder.go` | RegistryBuilder type |
| `phase.go` | Phase constants and methods |
| `extension.go` | Extension interface |

## Usage

```go
// Create a registry
reg := registry.NewRegistry()

// Add primitives
reg.AddPrimitives([]registry.PrimitiveSpec{
    {"car", 1, false, primitives.PrimCar},
    {"cdr", 1, false, primitives.PrimCdr},
}, registry.PhaseRuntime|registry.PhaseExpand)

// Add compile-time bindings
reg.AddBindings([]string{"if", "lambda", "define"})

// Apply to environment
err := reg.Apply(ctx, env)
```

## Extension Pattern

```go
// In extensions/myext/register.go
var Extension = registry.NewExtension("myext", AddToRegistry)
var Builder = registry.NewRegistryBuilder(addPrimitives)
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
    r.AddPrimitives(specs, registry.PhaseRuntime)
    return nil
}
```

## Phase Registration

| Phase | Environment | Purpose |
|-------|-------------|---------|
| `PhaseRuntime` | TopLevel | Available at program runtime |
| `PhaseExpand` | Expand | Available during macro expansion |
| `PhaseCompile` | Compile | Binding only, no runtime value |

## Gotchas

- **Thread safety**: Registry uses mutex for concurrent registration
- **Apply order**: Compile bindings → Runtime → Expand → Init funcs
- **Init functions**: Use `ApplyContext` to access environment
- **Macro sources**: Loaded separately by the Engine after Apply

## Testing

Uses quicktest with table-driven tests for Registry, Builder, and Phase types.

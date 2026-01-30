# Embedding API Design

This document describes the design of Wile's Go embedding API, provided by the `wile` package.

## Overview

The `wile` package exposes a high-level API for embedding the Scheme interpreter in Go programs. It wraps the internal compilation pipeline (parser, expander, compiler, VM) behind an `Engine` type that manages initialization, evaluation, and Go/Scheme interop.

```
┌──────────────────────────────────────────────────────┐
│  Go Application                                      │
│                                                      │
│  engine, _ := wile.NewEngine()                       │
│  result, _ := engine.Eval(ctx, "(+ 1 2)")            │
│                                                      │
├──────────────────────────────────────────────────────┤
│  wile.Engine                                         │
│  - Eval / EvalMultiple / Compile / Run / Call         │
│  - Define / Get / RegisterPrimitive                  │
│  - Value wrapping/unwrapping boundary                │
├──────────────────────────────────────────────────────┤
│  Internal Pipeline                                   │
│  Parser → Expander → Compiler → VM                   │
└──────────────────────────────────────────────────────┘
```

## Engine Lifecycle

### Initialization

`NewEngine` performs a complete initialization sequence:

1. Apply functional options to build configuration
2. Create a registry (default: core primitives via `core.AddToRegistry`)
3. Apply any extensions to the registry
4. Create a per-instance `TopLevelEnvironment` for symbol interning
5. Create the runtime `EnvironmentFrame` from the top-level environment
6. Apply registry bindings to the environment
7. Register syntax compilers and primitive expanders
8. Load bootstrap macros from the registry

If any step fails (including bootstrap macro loading), the engine is not returned.

### Per-Instance Isolation

Each `Engine` has its own `TopLevelEnvironment` and symbol table. This means:

- Multiple engines can coexist in the same process
- Symbols from different engines are not `eq?` to each other
- Each engine has independent variable bindings

## Evaluation Methods

| Method | Parses | Purpose |
|--------|--------|---------|
| `Eval(ctx, code)` | First expression only | Single expression evaluation |
| `EvalMultiple(ctx, code)` | All expressions | Multiple expressions, returns last result |
| `Compile(code)` | First expression only | Compile without executing |
| `Run(ctx, compiled)` | N/A | Execute pre-compiled code |

### Compile/Run Separation

`Compile` returns an opaque `CompiledCode` value containing the bytecode template and environment. This enables:

- Compiling once and running multiple times
- Caching compiled expressions
- Separating compilation cost from execution

### Internal Pipeline

```
engine.Eval(ctx, "(+ 1 2 3)")
    │
    ├─ parser.NewParser().ReadSyntax()
    │  └─ Returns syntax.SyntaxValue
    │
    ├─ machine.NewExpanderTimeContinuation().ExpandExpression()
    │  └─ Macro expansion
    │
    ├─ machine.NewCompiletimeContinuation().CompileExpression()
    │  └─ Bytecode compilation
    │
    └─ machine.NewMachineContext().Run()
       └─ VM execution → result
```

## Value Boundary

All values crossing the API boundary are wrapped behind a public `Value` interface:

```go
type Value interface {
    SchemeString() string   // Scheme representation (#t, "hello", 42, etc.)
    IsVoid() bool           // True for the void value
    Internal() values.Value // Escape hatch to raw internal value
}
```

The interface includes an unexported method to prevent external implementations. Values are wrapped/unwrapped at the API boundary by internal functions.

### Constructors

| Constructor | Creates |
|---|---|
| `NewInteger(n int64)` | Exact integer |
| `NewFloat(f float64)` | Inexact real |
| `NewString(s string)` | String |
| `NewSymbol(s string)` | Symbol |
| `NewBoolean(b bool)` | `#t` / `#f` |
| `NewList(vals ...Value)` | Proper list |

### Constants

| Constant | Value |
|---|---|
| `Null` | Empty list `'()` |
| `Void` | Void value |
| `True` | `#t` |
| `False` | `#f` |

## Go/Scheme Interop

### Defining Values

`Define(name, value)` binds a Go-constructed value in the top-level environment. `Get(name)` retrieves it.

### Registering Go Functions

`RegisterPrimitive` exposes a Go function as a Scheme procedure:

```go
type PrimitiveSpec struct {
    Name       string          // Scheme-visible name
    ParamCount int             // Fixed parameter count
    IsVariadic bool            // Accepts variable arguments
    Impl       ForeignFunction // Go implementation
}
```

The `ForeignFunction` receives a `MachineContext` and unwrapped arguments. It sets the return value via `mc.SetValue()`.

### Calling Scheme from Go

`Call(ctx, proc, args...)` invokes a Scheme procedure from Go. It creates a sub-context, applies the closure, and runs it to completion.

**Limitation**: `Call` only supports `MachineClosure` (compiled Scheme functions). Foreign closures or other callable types will fail.

## Extensions

Engine behavior can be customized via functional options:

| Option | Purpose |
|---|---|
| `WithRegistry(r)` | Use a custom registry (skips automatic core registration) |
| `WithExtension(ext)` | Add a single extension |
| `WithExtensions(exts...)` | Add multiple extensions |

Extensions implement `registry.Extension` and register primitives, macros, and compile-time definitions via `AddToRegistry`.

## Design Decisions

**Value wrapping**: The public `Value` interface hides internal types to maintain API stability. The `Internal()` escape hatch is available for advanced use cases that need direct access.

**Per-instance symbol interning**: Avoids global state and allows concurrent independent engines.

**Registry freezing**: Primitives must be registered before or during engine creation. This simplifies the runtime model — the set of available primitives is fixed once the engine is initialized.

**No continuation escape handling**: The `Engine` uses plain `Run()` internally. The `repl` package uses `RunWithEscapeHandling` for full R7RS continuation escape support. This is a deliberate simplification for the embedding case.

## File Reference

| File | Purpose |
|------|---------|
| `wile/engine.go` | Engine type, evaluation methods, initialization |
| `wile/value.go` | Value interface, constructors, wrapping |
| `wile/options.go` | Functional options for engine configuration |
| `wile/compiled.go` | CompiledCode type |
| `wile/doc.go` | Package documentation |

# Embedding API Design

This document describes the design of Wile's Go embedding API, provided by the `wile` package.

## Overview

The `wile` package exposes a high-level API for embedding the Scheme interpreter in Go programs. It wraps the internal compilation pipeline (parser, expander, compiler, VM) behind an `Engine` type that manages initialization, evaluation, and Go/Scheme interop.

```
┌──────────────────────────────────────────────────────┐
│  Go Application                                      │
│                                                      │
│  engine, _ := wile.NewEngine(ctx)                    │
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
4. Create a per-instance `TopLevelEnvironment` for syntax interning and phase management
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
| `EmptyList` | Empty list `'()` |
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

`Call` supports any `values.Callable`: `MachineClosure`, `ForeignClosure`, `CaseLambdaClosure`, and `Parameter`. It rejects `ComposableContinuation` (which cannot be re-entered via a simple sub-context).

## Extensions

Engine behavior can be customized via functional options:

| Option | Purpose |
|---|---|
| `WithExtension(ext)` | Add a single extension |
| `WithExtensions(exts...)` | Add multiple extensions |
| `WithSafeExtensions()` | Add the safe extension set (see Sandboxing below) |
| `WithoutCore()` | Skip core primitives — bare engine with only explicit extensions |
| `WithRegistry(r)` | Use a custom registry (skips automatic core registration) |
| `WithAuthorizer(auth)` | Set fine-grained runtime authorization policy |

Extensions implement `registry.Extension` and register primitives, macros, and compile-time definitions via `AddToRegistry`.

### Sandboxing

Wile provides two independent sandboxing layers.

**Layer 1: Extension-based (compile-time).** Primitives not in the registry don't exist — there's no runtime check to bypass (Rees, "A Security Kernel Based on the Lambda Calculus", 1996; Miller, "Robust Composition", 2006). `WithSafeExtensions()` adds only extensions with no ambient authority: io (in-memory ports, no filesystem), math, introspection, and the safe subset of all (records, promises, strings, characters). Privileged extensions (files, eval, system) and context-dependent extensions (gointerop, threads) are excluded. `WithoutCore()` goes further — it produces an engine with zero primitives. Library environments inherit the engine's registry, so restrictions propagate transitively to loaded libraries.

**Layer 2: Fine-grained authorization (runtime).** The `security.Authorizer` interface gates privileged operations at runtime using a K8s-style resource+action vocabulary (`file`, `code`, `env`, `process` x `read`, `write`, `delete`, `stat`, `load`, `exit`). Set via `WithAuthorizer(auth)`. Gate sites include file I/O, system calls, `eval`/`load`, `include`, and library loading. Without an authorizer, all operations are allowed (open by default). Built-in authorizers: `DenyAll()`, `ReadOnly()`, `FilesystemRoot(path)`, `All(authorizers...)`.

The two layers complement each other: layer 1 removes entire categories of capability at zero runtime cost; layer 2 fine-tunes what remains. See [`docs/SANDBOXING.md`](../SANDBOXING.md) for the full security model.

## Design Decisions

**Value wrapping**: The public `Value` interface hides internal types to maintain API stability. The `Internal()` escape hatch is available for advanced use cases that need direct access.

**Per-instance syntax interning**: Avoids global state and allows concurrent independent engines. Symbols are compared by string key (`helpers.EqIdentity`), not pointer identity.

**Registry freezing**: Primitives must be registered before or during engine creation. This simplifies the runtime model — the set of available primitives is fixed once the engine is initialized.

**No continuation escape handling**: The `Engine` uses plain `Run()` internally. The `repl` package uses `RunWithEscapeHandling` for full R7RS continuation escape support. This is a deliberate simplification for the embedding case.

## File Reference

| File | Purpose |
|------|---------|
| `engine.go` | Engine type, evaluation methods, initialization |
| `value.go` | Value interface, constructors, wrapping |
| `options.go` | Functional options for engine configuration |
| `compiled.go` | CompiledCode type |
| `doc.go` | Package documentation |

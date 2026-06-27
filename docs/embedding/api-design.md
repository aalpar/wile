# Embedding API Design

This document describes the design of Wile's Go embedding API, provided by the `wile` package.

## Overview

The `wile` package exposes a high-level API for embedding the Scheme interpreter in Go programs. It wraps the internal compilation pipeline (parser, expander, compiler, VM) behind an `Engine` type that manages initialization, evaluation, and Go/Scheme interop.

```
┌──────────────────────────────────────────────────────┐
│  Go Application                                      │
│                                                      │
│  engine, _ := wile.NewEngine(ctx)                    │
│  expr, _ := engine.Parse(ctx, "(+ 1 2)")             │
│  result, _ := engine.Eval(ctx, expr)                 │
│                                                      │
├──────────────────────────────────────────────────────┤
│  wile.Engine                                         │
│  - Parse / Eval / EvalIn / EvalMultiple / Compile    │
│  - Run / Call / Define / Get / RegisterPrimitive     │
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
4. Create a per-instance `Namespace` for syntax interning and phase management
5. Create the runtime `EnvironmentFrame` from the top-level environment
6. Apply registry bindings to the environment
7. Register syntax compilers and primitive expanders
8. Load bootstrap macros from the registry

If any step fails (including bootstrap macro loading), the engine is not returned.

### Per-Instance Isolation

Each `Engine` has its own `Namespace` and symbol table. This means:

- Multiple engines can coexist in the same process
- Symbols from different engines are not `eq?` to each other
- Each engine has independent variable bindings

## Evaluation Methods

| Method | Input | Purpose |
|--------|-------|---------|
| `Parse(ctx, code)` | `string` | Parse exactly one expression to `*Expression`; errors if empty, malformed, or multi-expression |
| `ParseWithSource(ctx, code, source)` | `string` + source name | Parse with source file attribution |
| `ReadExpression(ctx, r)` | `io.Reader` | Read the first complete expression from a reader; ignores trailing input. Pairs with `IsIncompleteInput` for REPL input handling |
| `MustParse(ctx, code)` | `string` | Parse or panic |
| `MustParseWithSource(ctx, code, source)` | `string` + source name | MustParse with source attribution |
| `Eval(ctx, expr)` | `*Expression` | Evaluate a single parsed expression |
| `EvalMultiple(ctx, code)` | `string` | Parse and evaluate all expressions, return last result |
| `EvalMultipleWithSource(ctx, code, source)` | `string` + source name | EvalMultiple with source attribution |
| `Compile(ctx, expr)` | `*Expression` | Compile a parsed expression without executing |
| `Run(ctx, compiled)` | `*CompiledCode` | Execute pre-compiled code |
| `Call(ctx, proc, args...)` | `Value` + args | Call a Scheme procedure from Go |
| `EvalIn(ctx, expr, ns)` | `*Expression` + `*Namespace` | Evaluate in an alternate namespace |

### Compile/Run Separation

`Compile` returns an opaque `CompiledCode` value containing the bytecode template and environment. This enables:

- Compiling once and running multiple times
- Caching compiled expressions
- Separating compilation cost from execution

### Internal Pipeline

```
engine.Parse(ctx, "(+ 1 2 3)")       ── string → *Expression
    │
engine.Eval(ctx, expr)               ── *Expression → result
    │
    ├─ ExpandExpression()
    │  └─ Macro expansion
    │
    ├─ CompileExpression()
    │  └─ Bytecode compilation
    │
    └─ MachineContext.Run()
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
| `NewInteger(n int64)` | Exact integer (fixnum) |
| `NewBigInteger(n *big.Int)`, `NewBigIntegerFromInt64`, `NewBigIntegerFromString(s, base)` | Exact integer (bignum) |
| `NewRational(num, denom int64)`, `NewRationalFromBigInt` | Exact rational |
| `NewFloat(f float64)` | Inexact real (machine float) |
| `NewBigFloat(f *big.Float)`, `NewBigFloatFromFloat64`, `NewBigFloatFromString` | Inexact real (arbitrary precision) |
| `NewComplex(v complex128)`, `NewComplexFromParts(re, im float64)` | Complex number |
| `NewString(s string)` | String |
| `NewSymbol(s string)` | Symbol |
| `NewBoolean(b bool)` | `#t` / `#f` |
| `NewVector(vals ...Value)` | Vector |
| `NewList(vals ...Value)` | Proper list |
| `WrapValue(v values.Value)` | Escape hatch — wrap a pre-constructed internal value |

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
    // Optional fields:
    Doc        string          // brief description
    ParamNames []string        // parameter names (for documentation)
    Category   string          // grouping category (for topic browsing)
    ParamTypes []values.TypeConstraint // per-parameter type contract
    ReturnType values.TypeConstraint   // return type
    Keywords   []string        // searchable tags
}
```

The `ForeignFunction` receives a `CallContext` (interface implemented by `MachineContext`). Arguments are accessed positionally via `mc.Arg(i)` (and `mc.Arg(n-1)` for the variadic rest tail when `IsVariadic` is set). The return value is set via `mc.SetValue()`. Primitives that need full VM access (sub-contexts, continuations, exception handling) type-assert to `*MachineContext`.

### Calling Scheme from Go

`Call(ctx, proc, args...)` invokes a Scheme procedure from Go. It creates a sub-context, applies the closure, and runs it to completion.

`Call` supports any `values.Callable`: `MachineClosure`, `ForeignClosure`, `CaseLambdaClosure`, and `Parameter`. It rejects `ComposableContinuation` (which cannot be re-entered via a simple sub-context).

## Extensions

Engine behavior can be customized via functional options:

| Option | Purpose |
|---|---|
| `WithExtension(ext)` | Add a single extension |
| `WithExtensions(exts...)` | Add multiple extensions |
| `WithProfile(p)` | Apply a named profile bundle (`Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink`) |
| `WithoutCore()` | Skip core primitives — bare engine with only explicit extensions |
| `WithRegistry(r)` | Use a custom registry (skips automatic core registration) |
| `WithAuthorizer(auth)` | Set fine-grained runtime authorization policy |
| `WithSandbox(opts...)` | Layer a restrictive authorizer (read-allowed, write/delete denied, env-prefix filtered). Takes optional `SandboxEnvPrefix(prefix)`; default prefix is `"WILE_"`. See "Option ordering" below |
| `WithStrictNamespace()` | Bind only the core surface at the top level; the profile's extension primitives stay importable but are not pre-bound. See "Strict namespace" below |
| `WithEnv(k, v)`, `WithEnvMap(m)` | Install a virtual environment-variable map |
| `WithSourceFS(fsys)` | Add a virtual `fs.FS` layer to the source resolver chain |
| `WithSourceOS()` | Add OS filesystem to the source resolver chain |
| `WithLibraryPaths(paths...)` | Set R7RS library search paths |
| `WithNamespace(ns)` | Use a pre-built namespace |
| `WithContractEnforcement()` | Enable runtime enforcement of `PrimitiveSpec.ParamTypes`/`ReturnType` contracts |
| `WithMaxCallDepth(n)` | Cap the continuation chain depth (default `DefaultMaxCallDepth`) |
| `WithMaxStackSize(n)` | Cap the eval stack size. Opt-in: when not set (or set to `0`), the stack is unlimited |
| `WithInlineThreshold(n)` | Tune the procedure inliner's size budget |
| `WithImportObserver(obs)` | Observe library imports (called on each `(import ...)`) |
| `WithCoverage(c)` | Attach a `*coverage.Collector` to record per-line Scheme execution |

**Option ordering.** `WithAuthorizer` *assigns* the authorizer; `WithSandbox` *composes* via `security.All(...)` only if an authorizer is already set. Therefore `WithSandbox` must appear **after** `WithProfile`/`WithAuthorizer`. A later `WithAuthorizer(...)` silently overwrites any sandbox installed earlier — there is no diagnostic. Place authorizer-assigning options first and `WithSandbox` last.

Extensions implement `registry.Extension` and register primitives, macros, and compile-time definitions via `AddToRegistry`.

### Sandboxing

Wile provides two independent sandboxing layers.

**Layer 1: Extension-based (compile-time).** Primitives not in the registry don't exist — there's no runtime check to bypass (Rees, "A Security Kernel Based on the Lambda Calculus", 1996; Miller, "Robust Composition", 2006). `WithProfile(Console)` selects a curated bundle (io with in-memory ports, files, math, the safe subset of `all`, and envvars) plus a matching `ConsoleAuthorizer` that restricts file ops to `/tmp` and denies code/process. `WithProfile(Tiny)` registers no extensions beyond core; `WithProfile(KitchenSink)` registers every extension and matches the CLI. `WithoutCore()` goes further — it produces an engine with zero primitives. Library environments inherit the engine's registry, so restrictions propagate transitively to loaded libraries.

**Layer 2: Fine-grained authorization (runtime).** The `security.Authorizer` interface gates privileged operations at runtime using a K8s-style resource+action vocabulary (resources: `file`, `code`, `env`, `process`; actions: `read`, `write`, `delete`, `stat`, `load`, `exit`, `exec`, `exec-shell`). Set via `WithAuthorizer(auth)`. Gate sites include file I/O, system calls, `eval`/`load`, `include`, and library loading. Without an authorizer, all operations are allowed (open by default). Built-in authorizers: `DenyAll()`, `ReadOnly()`, `ReadOnlyWithLoad()`, `FilesystemRoot(path)`, `ConsoleAuthorizer()`, `ConsoleWithLoadAuthorizer()`, `SandboxAuthorizer(envPrefix)`, `All(authorizers...)`. Profiles bundle a matching authorizer; `WithSandbox` adds `SandboxAuthorizer` on top via `All(...)`.

The two layers complement each other: layer 1 removes entire categories of capability at zero runtime cost; layer 2 fine-tunes what remains. See [`security/sandboxing.md`](../security/sandboxing.md) for the full security model.

### Strict namespace

By default Wile is batteries-included: a profile's extension primitives are
pre-bound at the top level, so `(display x)` and `(+ 1 2)` work with no `import`.
This is the "feels native to Go" scripting ergonomic, mirroring Racket's `racket`
vs `racket/base`.

`WithStrictNamespace()` opts into an R7RS-strict *visible* surface: only the core
primitives (and the `define`/`import`/syntax machinery) are bound at the top
level. The profile's extension primitives stay **registered** — reachable via
`(import …)` — but are not pre-bound. The bare surface equals a `Tiny` engine's,
while the full profile registry still backs library loading, so libraries layer
on top of a bare baseline:

```go
eng, _ := wile.NewEngine(ctx,
    wile.WithProfile(wile.Small), wile.WithStrictNamespace(),
    wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())

eng.EvalMultiple(ctx, "(car '(1 2))")                       // 1   — core visible
eng.EvalMultiple(ctx, "(display 1)")                        // error: no binding "display"
eng.EvalMultiple(ctx, "(import (scheme r5rs)) (exact->inexact 1/2)") // 0.5 — layered on top
```

**Security is unchanged.** The profile (the registered extension set) remains the
capability boundary — strict mode never widens what is reachable, it only
withholds it from the top level until imported. `WithProfile(Small) +
WithStrictNamespace()` exposes exactly the `Small` surface, just bare until
imported. The option is orthogonal to `WithProfile`/`WithSandbox`/`WithAuthorizer`
and composes order-independently with them. Off by default (the batteries-included
top level is preserved for compatibility and the REPL/CLI experience).

A bare top level is a valid import target: import installs resolved bindings into
the mutable user-global frame, not the sealed base, so layering libraries on a
strict engine works exactly as on a non-strict one.

## Virtual Filesystem

`WithSourceFS(fsys fs.FS)` adds a virtual filesystem layer to the source file resolver chain. Multiple calls add layers searched in call order. `WithSourceOS()` appends the OS filesystem to the chain. When no resolver options are used, the engine defaults to the OS filesystem. Once any resolver option is used, only the explicitly configured resolvers are active.

```go
//go:embed scheme
var schemeFS embed.FS

engine, err := wile.NewEngine(ctx,
    wile.WithSourceFS(schemeFS),  // searched first
    wile.WithSourceOS(),          // OS filesystem searched last
    wile.WithLibraryPaths("./stdlib/lib"),
)
```

Library search paths from `WithLibraryPaths` become relative paths within each FS layer. Bootstrap macros are unaffected — they always load from the embedded bootstrap filesystem.

Internally, each `WithSourceFS` creates an `FSFileResolver` that resolves files within its `fs.FS` using load-path-stack directory, then library search paths, then FS root. Multiple resolvers are composed into a `ChainFileResolver` that tries each in order, falling through on file-not-found. Absolute paths are rejected by `FSFileResolver`. Security authorization (`WithAuthorizer`) is still enforced.

## Design Decisions

**Value wrapping**: The public `Value` interface hides internal types to maintain API stability. The `Internal()` escape hatch is available for advanced use cases that need direct access.

**Per-instance syntax interning**: Avoids global state and allows concurrent independent engines. Symbols are compared by string key (`helpers.EqIdentity`), not pointer identity.

**Registry freezing**: Primitives must be registered before or during engine creation. This simplifies the runtime model — the set of available primitives is fixed once the engine is initialized.

**Continuation escape handling enabled**: Both the compiled-code path (`Engine.Run` / `Engine.Eval`, via `runCompiled` in `engine.go`) and the foreign-call path (`Engine.Call`, via `callCallable`) use `MachineContext.RunWithEscapeHandling`. It installs `DefaultPromptTag` as a top-level prompt and catches `ErrPromptAbort` aborts to that tag, restoring to the prompt frame and resuming execution so the abort payload becomes the returned value (normal return, `err == nil`). Only aborts for tags that have no matching prompt escape as runtime errors. Embedders get consistent R7RS escape semantics regardless of entry point.

## File Reference

| File | Purpose |
|------|---------|
| `engine.go` | Engine type, evaluation methods, initialization |
| `expression.go` | `Expression` type, `Parse`, `ParseWithSource`, `MustParse`, `MustParseWithSource`, `ReadExpression` |
| `value.go` | `Value` interface, constructors, wrapping |
| `options.go` | Functional options for engine configuration |
| `profile.go` | `Profile` type and `WithProfile` |
| `sandbox.go` | `WithSandbox`, `SandboxOption`, `SandboxEnvPrefix` |
| `debugger.go` | `Debugger` type (breakpoints, stepping) |
| `compiled.go` | `CompiledCode` type |
| `error.go` | `CompilationError`, `RuntimeError`, `IsIncompleteInput` |
| `doc.go` | Package documentation |

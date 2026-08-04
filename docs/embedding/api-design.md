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
│  - Parse / Eval / EvalMultiple / EvalProgram         │
│  - Compile / Run / Call / Define / RegisterFunc      │
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

`Close()` releases resources held by extensions implementing `registry.Closeable`; individual closer errors are joined. A second call returns `ErrEngineClosed`.

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
| `ReadExpressions(ctx, r)` | `io.Reader` | Read every expression from a reader to EOF |
| `MustParse(ctx, code)` | `string` | Parse or panic |
| `MustParseWithSource(ctx, code, source)` | `string` + source name | MustParse with source attribution |
| `Eval(ctx, expr)` | `*Expression` | Evaluate a single parsed expression |
| `EvalMultiple(ctx, code)` | `string` | Compile and run each top-level form independently, return last result. Forward references between separate `define`s do not resolve |
| `EvalMultipleWithSource(ctx, code, source)` | `string` + source name | EvalMultiple with source attribution |
| `EvalProgram(ctx, code, source)` | `string` + source name | Whole-program semantics: every top-level form is spliced into one `(begin ...)` and compiled as a unit, so top-level `define`s are mutually visible. The recommended entry point for a script or file |
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
    InvokesProcedure bool      // Impl may call back into a Scheme procedure
}
```

The `ForeignFunction` receives a `CallContext` (interface implemented by `MachineContext`). Arguments are accessed positionally via `mc.Arg(i)` (and `mc.Arg(n-1)` for the variadic rest tail when `IsVariadic` is set). The return value is set via `mc.SetValue()`. Primitives that need full VM access (sub-contexts, continuations, exception handling) type-assert to `*MachineContext`.

`InvokesProcedure` is a soundness commitment, not documentation: the default `false` tells the frame-reclaim classifier the primitive is capture-safe. Any `Impl` that reaches `ApplyCallable` or runs a sub-context MUST set it to `true`. A static guard in `pkg/wile` fails CI when the annotation is missing.

### Registering Go Functions by Signature

`RegisterFunc(name, fn)` (and `RegisterFuncs(map[string]any)`) registers a plain Go function without writing a `ForeignFunction`. Argument and return converters are computed once at registration time from the reflected signature. Supported: `int64`, `int`, `float64`, `complex128`, `string`, `bool`, `[]byte`, typed slices, maps, structs with exported fields, `func(...)` callbacks, `Value`, and `error` as the last return. A leading `context.Context` parameter receives the VM's context and does not count toward the Scheme arity. Callbacks must be invoked synchronously during the call; they capture VM state that is not goroutine-safe. Numeric conversions that would lose precision fail with `werr.ErrLossyConversion` unless `WithLossyConversionsAllowed()` is set.

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
| `WithoutAmbientBindings()` | Bind *nothing* at the top level — one step past `WithStrictNamespace()`. Only the core special forms remain (they are phase handlers, not bindings); everything else, `car` included, must be imported. See "Strict namespace" below |
| `WithEnv(k, v)`, `WithEnvMap(m)` | Install a virtual environment-variable map |
| `WithSourceFS(fsys)` | Add a virtual `fs.FS` layer to the source resolver chain |
| `WithSourceOS()` | Add OS filesystem to the source resolver chain |
| `WithLibraryPaths(paths...)` | Enable the R7RS library system (`define-library`/`import`) and set its search paths. Without it, `(import ...)` raises a configuration error |
| `WithNamespace(ns)` | Use a pre-built namespace |
| `WithDialect(d)` | Fork the forms registry per engine so a dialect can install, replace, or remove special forms (`DefaultDialect` is R7RS; `NoMutation` also ships). Incompatible with `WithNamespace` |
| `WithMutableTopLevel()` | Opt out of the immutable-top-level default and take strict R7RS redefinable/`set!`-able top-level bindings |
| `WithImmutableTopLevel()` | Explicit, redundant selector for the default. Retained for source compatibility |
| `WithContractEnforcement()` | Enable runtime enforcement of `PrimitiveSpec.ParamTypes`/`ReturnType` contracts |
| `WithLossyConversionsAllowed()` | Let FFI converters truncate Scheme numerics into fixed-precision Go types instead of failing with `werr.ErrLossyConversion` |
| `WithMaxCallDepth(n)` | Cap the continuation chain depth (default `DefaultMaxCallDepth`) |
| `WithMaxStackSize(n)` | Cap the eval stack size. Opt-in: when not set (or set to `0`), the stack is unlimited |
| `WithMaxParseDepth(n)` | Cap parser nesting depth (default `parser.DefaultMaxParseDepth`, 10000); yields `ErrParseDepthExceeded` instead of a fatal Go stack overflow |
| `WithMaxExpandDepth(n)` | Cap expander recursion depth (default 50000), bounding programmatically-built syntax that never passed through the parser |
| `WithInlineThreshold(n)` | Tune the procedure inliner's size budget |
| `WithImportObserver(obs)` | Observe library imports (called on each `(import ...)`) |
| `WithCoverage(c)` | Attach a `*coverage.Collector` to record per-line Scheme execution |

**Immutable top level is the default.** A top-level `define` that is defined once and never `set!` within its compilation unit becomes rebind-stable, and a later `set!` of it is rejected with `ErrImmutableBinding`. This is a documented deviation from R7RS §4.1.6/§5.3 that unlocks the frame-reclamation optimizer; `WithMutableTopLevel()` opts out and forfeits it. Enforcement is scoped to the engine's own user runtime global: redefining a sealed primitive or stdlib name is a child-frame shadow, and user-loaded libraries stay mutable.

**Option ordering.** Authorizer selection is resolved once at construction (`resolveAuthorizer` in `options.go`) from three separate fields, so it is order-independent: an explicit `WithAuthorizer` (even `nil`) overrides a profile's authorizer, and a `WithSandbox` layer is always intersected on top via `security.All(...)`. Multiple `WithSandbox` calls accumulate, so restrictions only tighten. `WithEnv`/`WithEnvMap` is the exception that still depends on order: `WithProfile(Console*)` fills in an empty env map only when none is set, so a later `WithEnvMap(nil)` re-nils it and opens the sandbox.

**`WithNamespace` drops the security options.** Every option consumed by namespace construction is silently ignored on that path, because `NewEngine` skips the bootstrap step that would apply them: the registry/extension/core options, `WithStrictNamespace`, `WithAuthorizer`, `WithSandbox`, a profile's built-in authorizer, `WithEnv`/`WithEnvMap`, and `WithImmutableTopLevel`/`WithMutableTopLevel`. `WithNamespace(ns)` plus `WithSandbox()` yields no sandbox and no error. Apply those options to `NewNamespace` and pass its result here. `WithDialect` is the one such option `NewEngine` rejects outright instead of dropping.

Extensions implement `registry.Extension` and register primitives, macros, and compile-time definitions via `AddToRegistry`.

### Sandboxing

Wile provides two independent sandboxing layers.

**Layer 1: Extension-based (compile-time).** Primitives not in the registry don't exist — there's no runtime check to bypass (Rees, "A Security Kernel Based on the Lambda Calculus", 1996; Miller, "Robust Composition", 2006). `WithProfile(Console)` selects a curated bundle (io with in-memory ports, files, math, the safe subset of `all`, charsets, and envvars) plus a matching `ConsoleAuthorizer` that restricts file ops to `/tmp` and denies code/process. `WithProfile(Tiny)` registers no extensions beyond core; `WithProfile(KitchenSink)` registers every extension and matches the CLI. `WithoutCore()` goes further — it produces an engine with zero primitives. Library environments inherit the engine's registry, so restrictions propagate transitively to loaded libraries.

**Layer 2: Fine-grained authorization (runtime).** The `security.Authorizer` interface gates privileged operations at runtime using a K8s-style resource+action vocabulary (resources: `file`, `code`, `env`, `process`; actions: `read`, `write`, `delete`, `stat`, `load`, `eval`, `exit`, `exec`, `exec-shell`). Set via `WithAuthorizer(auth)`. Gate sites include file I/O, system calls, `eval`/`load`, `include`, and library loading. Without an authorizer, all operations are allowed (open by default). Built-in authorizers: `DenyAll()`, `ReadOnly()`, `ReadOnlyWithLoad()`, `FilesystemRoot(path)`, `ConsoleAuthorizer()`, `ConsoleWithLoadAuthorizer()`, `SandboxAuthorizer(envPrefix)`, `All(authorizers...)`. Profiles bundle a matching authorizer; `WithSandbox` adds `SandboxAuthorizer` on top via `All(...)`.

The two layers complement each other: layer 1 removes entire categories of capability at zero runtime cost; layer 2 fine-tunes what remains. See [`security/sandboxing.md`](../security/sandboxing.md) for the full security model.

### Strict namespace

By default Wile is batteries-included: a profile's extension primitives are
pre-bound at the top level, so `(display x)` and `(+ 1 2)` work with no `import`.
This is the "feels native to Go" scripting ergonomic, mirroring Racket's `racket`
vs `racket/base`.

Two options narrow that surface, and they form a ladder. Both combine by **max**,
so applying several in any order always lands on the narrowest — the one direction
this family must never reverse.

| Level | Visible top level | Option |
|---|---|---|
| 0 | the whole registry (default) | none |
| 1 | core primitives + core bootstrap macros | `WithStrictNamespace()` |
| 2 | nothing | `WithoutAmbientBindings()` |

What is **registered** is identical at all three, so nothing is withheld
permanently: a program reaches the whole profile by importing it. These options
buy *explicitness* — proof that every dependency is declared in source — not
confinement. The profile and the authorizer are the capability boundary.

#### Level 1 — core-only

`WithStrictNamespace()` binds only the core primitives (and the
`define`/`import`/syntax machinery) at the top level. The profile's extension
primitives stay **registered** — reachable via `(import …)` — but are not
pre-bound. The visible surface equals a `Tiny` engine's, while the full profile
registry still backs library loading, so libraries layer on top of a core-only
baseline:

```go
eng, _ := wile.NewEngine(ctx,
    wile.WithProfile(wile.Small), wile.WithStrictNamespace(),
    wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())

eng.EvalMultiple(ctx, "(car '(1 2))")                       // 1   — core visible
eng.EvalMultiple(ctx, "(display 1)")                        // error: no binding "display"
eng.EvalMultiple(ctx, "(import (scheme r5rs)) (exact->inexact 1/2)") // 0.5 — layered on top
```

#### Level 2 — nothing pre-bound

`WithoutAmbientBindings()` binds the visible top level from an **empty** registry.
Not even `car` survives:

```go
eng, _ := wile.NewEngine(ctx,
    wile.WithProfile(wile.Small), wile.WithoutAmbientBindings(),
    wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())

eng.EvalMultiple(ctx, "(let ((x 1)) x)")                 // 1 — the floor, see below
eng.EvalMultiple(ctx, "(car '(1 2))")                    // error: no binding "car"
eng.EvalMultiple(ctx, "(import (scheme base)) (car '(1 2))") // 1
```

**The floor is not the empty set, and it is not R7RS-strict.** Core special forms
are *phase handlers*: registered by the compiler, held in frames that ordinary
value resolution never consults, and never sourced from a registry — so
withholding the registry cannot withhold them. The partition is three-way:

| | Members | Why |
|---|---|---|
| **Usable** | `lambda` `if` `quote` `define` `begin` `set!` `let` `let*` `letrec` `letrec*` named `let` `define-syntax` `let-syntax` `letrec-syntax` `syntax-rules` `cond-expand` `case-lambda` `define-library` `import` | phase handler whose codegen emits no call |
| **Resolves, unusable** | `quasiquote` *with* an `unquote` (emits `list`), `unless` (needs `not`), `guard` (needs `call-with-exit`) | phase handler, but its expansion calls a primitive nothing bound |
| **Unresolved** | `cond` `case` `when` `and` `or` `do` `define-record-type` `let-values` `let*-values` `define-values` `delay` `parameterize`, and every primitive | bootstrap macro or primitive; both come from the registry |

A constant quasiquote template such as `` `(1 2) `` works; add a comma and it
fails. `when` and `unless` land on **opposite** sides — `when` is a bootstrap
macro, `unless` is a phase handler that expands through `not`.

So the option is strict for *procedures and derived syntax*. Reaching a usable
R7RS surface takes one `(import (scheme base))`, which restores the derived syntax
and the procedures alike.

**Two costs, both real:**

- *Per-import latency.* A library environment is engine-sized, so imports are not
  cheap: `(import (scheme base))` measures **9.38 ms**, against **3.93 ms /
  1.20 MB heap** to build a whole `Small` engine — one import is ~2.4× an engine's
  startup. An eight-library R7RS preamble costs **58.57 ms / 7.93 MB heap**.
  Per-library spread is narrow (6.0–9.4 ms), so this is per-import overhead, not
  library size. A level-2 engine is therefore slower to reach a usable state than
  a `Small` engine is at rest. (In-process, warm, macOS/arm64, 2026-08-04.)
- *Profile bound.* Usable on `Small` and `KitchenSink` only. `Tiny` cannot import
  `(scheme base)` (64 of its exports are unregistered there) and
  `Console`/`ConsoleWithLoad` are denied `code:load` on the stdlib path. Both
  failures pre-date the option and reproduce without it — but at level 0 or 1
  those profiles still hand the program an ambient surface, and at level 2 there
  is none to fall back to.

`WithSandbox()` denies `code:load` for the same reason, so **level 2 plus a
sandbox is confinement**: the program is left on the phase-handler floor with no
import route off it. The two options are individually orthogonal — one picks the
visible surface, the other the authorizer — but do not reach for the pair
expecting the "explicitness, not confinement" reading above to still hold.

#### Both levels

**Security is unchanged.** The profile (the registered extension set) remains the
capability boundary — a strict level never widens what is reachable, it only
withholds it from the top level until imported. `WithProfile(Small) +
WithStrictNamespace()` exposes exactly the `Small` surface, just withheld until
imported. Both options are orthogonal to
`WithProfile`/`WithSandbox`/`WithAuthorizer` and compose order-independently with
them. Off by default (the batteries-included top level is preserved for
compatibility and the REPL/CLI experience).

A narrowed top level is a valid import target: import installs resolved bindings into
the mutable user-global frame, not the sealed base, so layering libraries on a
strict engine works exactly as on a non-strict one.

**Scope.** The visible surface is carved when the namespace is built, so strictness
must be set at namespace-creation time. Like `WithRegistry`/`WithExtension`/
`WithoutCore`, neither option has any effect on the `WithNamespace` path —
a pre-built namespace is authoritative for its own top level, so bake strictness
in at `NewNamespace`. Both are also incompatible with `WithRegistry`/`WithoutCore`
(which supply a custom or coreless registry): the strict levels derive the visible
surface from the default core registry, not from a caller-supplied one, so that
combination is rejected at construction with `ErrEngineInit`. `WithoutCore()` is
the one to keep separate in your head: it empties **both** the visible surface and
the registry that backs library environments, where `WithoutAmbientBindings()`
empties only the former.

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
| `expression.go` | `Expression` type, `Parse`, `ParseWithSource`, `MustParse`, `MustParseWithSource`, `ReadExpression`, `ReadExpressions` |
| `value.go`, `value_helpers.go` | `Value` interface, constructors, wrapping |
| `options.go` | Functional options for engine configuration, `resolveAuthorizer` |
| `profile.go` | `Profile` type and `WithProfile` |
| `sandbox.go` | `WithSandbox`, `SandboxOption`, `SandboxEnvPrefix` |
| `dialect.go`, `dialect_nomutation.go` | `Dialect` interface, `WithDialect`, `DefaultDialect`, `NoMutation` |
| `primitive.go` | `PrimitiveSpec`, `ForeignFunction`, `CallContext` re-exports |
| `ffi.go`, `ffi_*.go` | `RegisterFunc`/`RegisterFuncs` and the reflection-based converters |
| `stdlib.go` | `StdLibFS` (re-export of `stdlib.LibFS`) |
| `library_info.go` | `LibraryName`, `LibraryInfo`, library introspection |
| `disassemble.go` | `DisassembleValue`, `FormLabel` |
| `debugger.go` | `Debugger` type (breakpoints, stepping) |
| `compiled.go` | `CompiledCode` type |
| `error.go` | `CompilationError`, `RuntimeError`, `IsIncompleteInput` |
| `doc.go` | Package documentation |

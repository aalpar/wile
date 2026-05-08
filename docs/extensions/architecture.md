# Extension System

Wile's extension system lets Go code add Scheme primitives, macros, and global
values to the interpreter. Extensions are modular, composable, and — when the
R7RS library system is enabled — automatically importable from Scheme via
`(import ...)`.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        Go embedder                              │
│                                                                 │
│  engine, _ := wile.NewEngine(ctx,                               │
│      wile.WithExtension(math.Extension),                        │
│      wile.WithExtension(myExt),                                 │
│      wile.WithLibraryPaths(),          ← enables (import ...)   │
│  )                                                              │
└──────────────────────────────┬──────────────────────────────────┘
                               │
               ┌───────────────▼───────────────┐
               │         Registry              │
               │                               │
               │  core primitives   (~80)      │
               │  + math extension  (+30)      │
               │  + myExt           (+N)       │
               │  + bindings, macros, globals  │
               └───────────────┬───────────────┘
                               │ Apply()
               ┌───────────────▼───────────────┐
               │     EnvironmentFrame          │
               │                               │
               │  Phase 0 (runtime)  ← procs   │
               │  Phase 1 (expand)   ← macros  │
               │  Phase 2 (compile)  ← syntax  │
               └───────────────────────────────┘
```

### Lifecycle

1. **Registration** — Extensions call `Registry.AddPrimitives`, `AddBindings`,
   `AddMacroSource`, `AddGlobalValue` during `NewEngine`.
2. **Application** — `Registry.Apply` materializes everything into a live
   environment, following a strict phase order.
3. **Library creation** — If `WithLibraryPaths` was called, each extension's
   runtime primitives are wrapped in a synthetic `CompiledLibrary` and
   registered in the `LibraryRegistry`.
4. **Evaluation** — Scheme code can use extension primitives directly (they're
   in the top-level environment) or import them selectively via `(import ...)`.

---

## Creating an Extension

### Minimal Extension

```go
package myext

import (
    "github.com/aalpar/wile/machine"
    "github.com/aalpar/wile/registry"
    "github.com/aalpar/wile/values"
    "github.com/aalpar/wile/werr"
)

// Extension is the package's entry point.
var Extension = registry.NewDescribedExtension("myext",
    "Brief description of what this extension provides.",
    AddToRegistry)

// Builder composes registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry is the combined registration function.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
    r.AddPrimitives([]registry.PrimitiveSpec{
        {
            Name:       "double",
            ParamCount: 1,
            Impl:       primDouble,
            Doc:        "Returns twice the argument.",
            ParamNames: []string{"n"},
            Category:   "myext",
        },
    }, registry.PhaseSetRuntime)
    return nil
}

func primDouble(mc machine.CallContext) error {
    n, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber, "double: expected number")
    }
    mc.SetValue(n.Add(n))
    return nil
}
```

### Loading the Extension

```go
ctx := context.Background()
engine, err := wile.NewEngine(ctx,
    wile.WithExtension(myext.Extension),
)
// double is now available in the top-level environment
result, _ := engine.Eval(ctx, engine.MustParse(ctx, "(double 21)")) // 42
```

### Multiple Extensions

```go
engine, err := wile.NewEngine(ctx,
    wile.WithExtensions(
        math.Extension,
        process.Extension,
        system.Extension,
        myext.Extension,
    ),
)
```

---

## Extension Interface

Every extension implements the `registry.Extension` interface:

```go
type Extension interface {
    Name() string                    // "math", "system", etc.
    AddToRegistry(r *Registry) error // Registers primitives with the registry
}
```

### `ExtensionFunc` Adapter

For simple extensions, `registry.NewDescribedExtension` wraps a function:

```go
var Extension = registry.NewDescribedExtension("myext",
    "Brief description of this extension.",
    func(r *registry.Registry) error {
        r.AddPrimitive(spec, registry.PhaseSetRuntime)
        return nil
    })
```

### Optional Interfaces

Extensions can implement additional interfaces for extra behavior:

| Interface | Method | Purpose |
|-----------|--------|---------|
| `Describer` | `Description() string` | Human-readable description shown in `,doc` and `,libraries` |
| `LibraryNamer` | `LibraryName() []string` | Custom R7RS library name (default: `(wile <name>)`) |
| `Closeable` | `Close() error` | Resource cleanup when `Engine.Close()` is called |

```go
// Custom library name: (myorg utils) instead of (wile custom)
type myExtension struct{}

func (e *myExtension) Name() string {
	return "custom"
}

func (e *myExtension) AddToRegistry(r *registry.Registry) error {
	// ... register primitives ...
	return nil
}

func (e *myExtension) LibraryName() []string {
	return []string{"myorg", "utils"}
}

func (e *myExtension) Close() error {
	// cleanup
	return nil
}
```

---

## Registry API

The `registry.Registry` is the central store for all registration data.
Extensions interact with it during the registration phase.

### Registering Primitives

```go
// Single primitive
r.AddPrimitive(registry.PrimitiveSpec{
    Name:       "sqrt",
    ParamCount: 1,
    Impl:       primSqrt,
    Doc:        "Returns the square root of z.",
    ParamNames: []string{"z"},
    Category:   "math",
}, registry.PhaseSetRuntime)

// Batch registration
r.AddPrimitives([]registry.PrimitiveSpec{
    {Name: "sin", ParamCount: 1, Impl: primSin},
    {Name: "cos", ParamCount: 1, Impl: primCos},
}, registry.PhaseSetRuntime)
```

### PrimitiveSpec Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `Name` | `string` | yes | Scheme-visible name (`"sqrt"`, `"string->number"`) |
| `ParamCount` | `int` | yes | Fixed parameter count (for variadic, `Arg(ParamCount-1)` holds the rest list) |
| `IsVariadic` | `bool` | no | Accepts variable arguments; last `Arg` slot is the rest list |
| `Impl` | `machine.ForeignFunction` | yes | Go implementation function |
| `Doc` | `string` | no | Brief description |
| `ParamNames` | `[]string` | no | Parameter names for documentation |
| `Category` | `string` | no | Grouping category |
| `ParamTypes` | `[]values.TypeConstraint` | no | Per-parameter type contract. For variadic, last slot annotates rest-list element type |
| `ReturnType` | `values.TypeConstraint` | no | Return-type declaration (nil = unspecified) |
| `Keywords` | `[]string` | no | Searchable tags for `apropos` discovery |

### Registering Other Items

```go
// Compile-time bindings (no runtime value — for auxiliary syntax like else, =>)
r.AddBinding("my-special-form")
r.AddBindings([]string{"else", "=>", "_"})

// Bootstrap macro source (Scheme code evaluated at engine startup)
r.AddMacroSource(`
    (define-syntax my-when
      (syntax-rules ()
        ((my-when test body ...)
         (if test (begin body ...)))))
`)

// Global values (parameters, promises, or any Value)
r.AddGlobalValue("current-input-port", portParam)

// Initialization callbacks (run after Apply, environment is fully populated)
r.AddInitFunc(func() error {
    // post-registration setup
    return nil
})
```

### Application Order

`Registry.Apply()` processes registrations in this fixed order:

```
1. Compile-time bindings     (AddBindings)
2. Compile-only primitives   (PhaseCompile without PhaseRuntime)
3. Runtime primitives        (PhaseRuntime → ForeignClosure at phase 0)
4. Expand-time primitives    (PhaseExpand → ForeignClosure at phase 1)
5. Global values             (AddGlobalValue)
6. Init functions            (AddInitFunc)
```

Macro sources are loaded separately by the engine after `Apply()`.

---

## Phases

Phases control when a primitive is available. They are bit flags that compose
with `|`.

| Phase | Bit | Environment | Purpose |
|-------|-----|-------------|---------|
| `PhaseRuntime` | `1` | Top-level (phase 0) | Normal runtime evaluation |
| `PhaseExpand` | `2` | Expand (phase 1) | Available during macro expansion |
| `PhaseCompile` | `4` | Compile (phase 2) | Binding-only, no runtime value |

Most extension primitives use `PhaseRuntime` only. Primitives needed during
`syntax-rules` expansion use `PhaseRuntime | PhaseExpand`. Compile-time
bindings (auxiliary syntax keywords) use `PhaseCompile` or `AddBinding`.

```go
// Available at runtime only
r.AddPrimitive(spec, registry.PhaseSetRuntime)

// Available at both runtime and during macro expansion
r.AddPrimitive(spec, registry.PhaseSetRuntime|registry.PhaseSetExpand)

// Compile-time binding only (e.g., auxiliary syntax)
r.AddBinding("else")
```

---

## RegistryBuilder: Composing Registration Functions

Large extensions can split registration into logical groups using
`RegistryBuilder`:

```go
var Builder = registry.NewRegistryBuilder(
    addTranscendental,  // exp, log, sin, cos, ...
    addRounding,        // floor, ceiling, truncate, round
    addDivision,        // floor/, truncate/, ...
    addComplex,         // make-rectangular, real-part, ...
)

var AddToRegistry = Builder.AddToRegistry
var Extension = registry.NewDescribedExtension("math",
    "Extended math: trigonometry, logarithms, bitwise operations.",
    AddToRegistry)
```

Each function receives the same `*Registry` and can independently register its
primitives. The builder runs them in order, stopping on the first error.

---

## Writing Primitive Implementations

A primitive implementation has the type `machine.ForeignFunction`:

```go
// machine.ForeignFunction
func(mc machine.CallContext) error
```

### Accessing Arguments

```go
func primAdd(mc machine.CallContext) error {
    a, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber, "add: first argument")
    }
    b, ok := mc.Arg(1).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber, "add: second argument")
    }
    // Number.Add panics on unknown types; the VM recovers panics
    mc.SetValue(a.Add(b))
    return nil
}
```

### Variadic Arguments

For variadic primitives (`IsVariadic: true`), the last argument index holds
a proper list of the excess arguments:

```go
// (my-sum x ...) — ParamCount: 1, IsVariadic: true
func primMySum(mc machine.CallContext) error {
    rest := mc.Arg(0) // proper list of all arguments
    // Walk the list...
}
```

### Return Values

- **Single value**: `mc.SetValue(result)`
- **Void**: `mc.SetValue(values.Void)`
- **Multiple values**: Use `mc.SetValues()` or return values through
  continuation-based protocols

### Error Handling

Use project error types, not bare `errors.New` or `fmt.Errorf`:

```go
// Type check errors
return werr.WrapForeignErrorf(werr.ErrTypeConversion,
    "sqrt: expected number, got %s", v.SchemeString())

// Wrong argument count
return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
    "my-fn: expected 2 arguments, got %d", n)
```

---

## RegisterFunc: Go Functions as Scheme Primitives

For simpler Go↔Scheme bridging, `Engine.RegisterFunc` uses reflection to
automatically marshal arguments and return values:

```go
engine.RegisterFunc("double", func(x int64) int64 {
    return x * 2
})

engine.RegisterFunc("greet", func(name string) string {
    return "Hello, " + name
})
```

### Supported Types

| Go type | Scheme type |
|---------|-------------|
| `int64`, `int` | integer |
| `float64` | inexact real |
| `string` | string |
| `bool` | `#t` / `#f` |
| `[]byte` | bytevector |
| `[]T` | proper list (elements converted recursively) |
| `map[K]V` | hashtable (K must be string, int64, int, or bool) |
| `struct` | alist `((FieldName . value) ...)` |
| `func(...)` | callback (invokes Scheme lambda via VM sub-context) |
| `wile.Value` | any Scheme value (pass-through) |
| `context.Context` | auto-forwarded (first param only, invisible to Scheme) |
| `error` | last return only — returned as Go error |

### Batch Registration

```go
engine.RegisterFuncs(map[string]any{
    "double":  func(x int64) int64 { return x * 2 },
    "greet":   func(name string) string { return "Hello, " + name },
    "is-even": func(x int64) bool { return x%2 == 0 },
})
```

Note: Go map iteration order is unspecified. If multiple functions are invalid,
which one fails first is non-deterministic. Functions registered before the
failure remain registered.

### Callbacks

Go functions can accept Scheme procedures as callback parameters:

```go
engine.RegisterFunc("apply-twice", func(f func(int64) int64, x int64) int64 {
    return f(f(x))
})
```

```scheme
(apply-twice (lambda (x) (* x 2)) 5)  ; → 20
```

Callbacks must be invoked synchronously during the registered function's
execution. Storing a callback or calling it from another goroutine is unsafe.

### Context Forwarding

If the first Go parameter is `context.Context`, the VM's context is forwarded
automatically. It does not count toward the Scheme parameter count:

```go
engine.RegisterFunc("fetch", func(ctx context.Context, url string) (string, error) {
    // ctx carries the VM's context (deadline, cancellation)
    return httpGet(ctx, url)
})
```

```scheme
(fetch "https://example.com")  ; 1 Scheme argument, not 2
```

---

## Built-in Extensions

### Public Extensions (`extensions/`)

These are importable by external Go code:

| Package | Library Name | Primitives |
|---------|-------------|------------|
| `extensions/math` | `(wile math)` | `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sqrt`, `expt`, `floor`, `ceiling`, `truncate`, `round`, `floor/`, `floor-quotient`, `floor-remainder`, `truncate/`, `truncate-quotient`, `truncate-remainder`, `finite?`, `infinite?`, `nan?`, `numerator`, `denominator`, `rationalize`, `exact-integer-sqrt`, `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`, `number->string`, `string->number` (35 primitives) |
| `extensions/system` | `(wile system)` | `command-line`, `exit`, `emergency-exit`, `current-second`, `current-jiffy`, `jiffies-per-second` (6 primitives) |
| `extensions/files` | `(wile files)` | `open-input-file`, `open-output-file`, `open-binary-input-file`, `open-binary-output-file`, `file-exists?`, `delete-file`, `call-with-input-file`, `call-with-output-file`, `create-directory`, `delete-directory`, `directory-files`, `current-directory`, `set-current-directory!` (13 primitives) |
| `extensions/process` | `(wile process)` | `system`, `process-spawn`, `process-stdout`, `process-stderr`, `process-stdin`, `process-wait`, `process-kill`, `process?` (8 primitives) |
| `extensions/threads` | `(wile threads)` | SRFI-18 threading: `make-thread`, `thread-start!`, `thread-join!`, `thread-yield!`, `make-mutex`, `mutex-lock!`, `make-condition-variable`, `condition-variable-signal!`, etc. |
| `extensions/gointerop` | `(wile gointerop)` | Go concurrency: `make-channel`, `channel-send!`, `channel-receive`, `make-wait-group`, `make-rw-mutex`, `make-once`, `make-atomic`, `atomic-compare-and-swap!`, etc. |
| `extensions/introspection` | `(wile introspection)` | `environment?`, `interaction-environment`, `environment-bound-names`, `environment-ref`, `environment-bound?`, `features`, `available-libraries`, `disassemble` (8 primitives) |
| `extensions/eval` | `(wile eval)` | `eval`, `load`, `current-load-path`, `current-load-directory`, `current-load-depth`, `scheme-report-environment`, `null-environment`, `environment`, `expand`, `expand-once`, `compile`, `syntax-local-value`, `syntax-local-value/immediate`, `make-compile-time-value`, `syntax-local-introduce`, `syntax-local-identifier-as-binding` (16 primitives) |

### Internal Extensions (`internal/extensions/`)

Not importable by external code:

| Package | Purpose |
|---------|---------|
| `internal/extensions/io` | R7RS I/O: `read`, `write`, `display`, port operations |
| `internal/extensions/envvars` | Environment-variable primitives: `get-environment-variable`, `get-environment-variables` (sandbox-aware) |
| `internal/extensions/namespace` | Namespace introspection and management |
| `internal/extensions/all` | Records, promises, exceptions, strings, characters, and other R7RS primitives |

---

## Engine Options Reference

| Option | Description |
|--------|-------------|
| `WithExtension(ext)` | Add a single extension |
| `WithExtensions(ext...)` | Add multiple extensions |
| `WithProfile(p)` | Apply a named profile bundle: `Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink` |
| `WithoutCore()` | Skip core primitives — creates a bare engine with only explicitly added extensions |
| `WithLibraryPaths(paths...)` | Enable R7RS library system with optional search paths |
| `WithRegistry(reg)` | Use a custom registry (skips core primitives) |
| `WithMaxCallDepth(n)` | Set maximum VM recursion depth |
| `WithAuthorizer(auth)` | Set fine-grained runtime authorization policy (see [`sandboxing.md`](../security/sandboxing.md)) |
| `WithSandbox()` | Compose the sandbox env-prefix wrapper with the current authorizer |
| `WithEnv(k, v)`, `WithEnvMap(m)` | Install a virtual environment-variable map |

`WithProfile(KitchenSink)` matches the CLI's full extension set; `WithProfile(Console)` is the safe-by-default bundle (io with in-memory ports, files restricted to `/tmp`, math, the safe subset of `all`, and envvars) plus a matching `ConsoleAuthorizer`. See [`sandboxing.md`](../security/sandboxing.md) for the full profile table.

---

## Thread Safety

- `Registry` is thread-safe for concurrent registration (uses `sync.RWMutex`).
- `Engine` is **not** safe for concurrent use. Each goroutine needs its own
  engine, or external synchronization.
- SRFI-18 threads within a single engine are safe — the VM handles coordination
  internally.

---

## Naming Conventions

| Convention | Example | Meaning |
|------------|---------|---------|
| Trailing `?` | `pair?`, `finite?` | Predicate (returns boolean) |
| Trailing `!` | `set-car!`, `string-fill!` | Mutator (side effect) |
| `->` | `number->string` | Type conversion |
| `%` prefix | `%make-lazy-promise` | Internal/private |

---

## Package Dependencies

Extensions should depend only on public packages:

```
extensions/myext
  ├── github.com/aalpar/wile/registry       ← Extension, Registry, PrimitiveSpec
  ├── github.com/aalpar/wile/machine        ← MachineContext, ForeignFunction
  ├── github.com/aalpar/wile/values         ← Value types, error types
  └── github.com/aalpar/wile/registry/helpers  ← Type conversion helpers
```

No circular dependencies between extensions. Each is independently importable.

# Wile Plugin Architecture Proposal

## Overview

This proposal outlines a plugin architecture for Wile that enables embedding the Scheme interpreter in Go applications with customizable functionality. The design separates a minimal core from optional extensions, allowing users to compose exactly the capabilities they need.

```
┌─────────────────────────────────────────────────────────────────┐
│                     User Applications                            │
│  ┌──────────┐  ┌──────────────┐  ┌────────────────────────────┐ │
│  │ Standard │  │ Game Engine  │  │ Configuration DSL          │ │
│  │   REPL   │  │  Scripting   │  │ (no I/O, custom functions) │ │
│  └────┬─────┘  └──────┬───────┘  └─────────────┬──────────────┘ │
└───────┼───────────────┼────────────────────────┼────────────────┘
        │               │                        │
        ▼               ▼                        ▼
┌─────────────────────────────────────────────────────────────────┐
│                      wile.Engine (Public API)                    │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ NewEngine(options) / Eval(code) / Compile() / Run()        │ │
│  │ RegisterPrimitive() / Define() / Call() / Get()            │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
        │
        ├─────────────────────────────────────┐
        ▼                                     ▼
┌─────────────────────────┐   ┌───────────────────────────────────┐
│   registry/core         │   │         Optional Extensions        │
│   (Always Included)     │   │  ┌─────┐ ┌───────┐ ┌───────────┐  │
│  ─────────────────────  │   │  │ io  │ │threads│ │ gointerop │  │
│  • Type predicates      │   │  └─────┘ └───────┘ └───────────┘  │
│  • Lists/pairs          │   │  ┌───────┐ ┌────────┐ ┌────────┐  │
│  • Basic arithmetic     │   │  │system │ │ math   │ │ user   │  │
│  • Equality             │   │  │       │ │(trans.)│ │defined │  │
│  • Control (apply,call/cc)  │  └───────┘ └────────┘ └────────┘  │
│  • Vectors/strings      │   └───────────────────────────────────┘
│  • Bootstrap macros     │
└─────────────────────────┘
        │
        ▼
┌─────────────────────────────────────────────────────────────────┐
│                        Wile Core                                 │
│  ┌─────────┐ ┌────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐  │
│  │ machine │ │ parser │ │ validate │ │  syntax  │ │  values  │  │
│  └─────────┘ └────────┘ └──────────┘ └──────────┘ └──────────┘  │
│  ┌─────────────┐ ┌───────┐ ┌───────┐ ┌───────────────────────┐  │
│  │ environment │ │ forms │ │ match │ │      tokenizer        │  │
│  └─────────────┘ └───────┘ └───────┘ └───────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

## Design Goals

1. **Embeddable**: Wile can be embedded in Go applications with a clean, stable API
2. **Minimal Core**: Only primitives required for the Scheme language itself are mandatory (~85)
3. **Optional Extensions**: I/O, threading, system calls, math transcendentals are opt-in
4. **Independent REPL**: The REPL is just another consumer of the engine API
5. **User Extensibility**: Easy to add custom primitives without modifying Wile
6. **No I/O by Default**: Safe for sandboxed environments
7. **Backward Compatible**: Existing code continues to work unchanged

## Current Architecture

Today, all 350+ primitives are registered in `runtime/environment_tiny.go`:

```go
var runtimePrimitives = []PrimitiveSpec{
    {"car", 1, false, primitives.PrimCar},
    {"cdr", 1, false, primitives.PrimCdr},
    // ... 350+ more
}

func NewTopLevelEnvironmentFrameTiny(ctx context.Context) (*environment.TopLevelEnvironmentFrame, error) {
    env := environment.NewTopLevelEnvironmentFrame()
    primitives.InitState()
    registerCompileTimePrimitives(env, compileTimePrimitives)
    registerRuntimePrimitives(env, runtimePrimitives)
    registerExpandTimePrimitives(env, expandTimePrimitives)
    // ...
}
```

### Limitations

1. **Monolithic**: All primitives compiled into every binary
2. **Centralized**: Adding primitives requires editing `environment_tiny.go`
3. **No External Extensions**: Third parties cannot add primitives without forking
4. **No Embedding API**: Must use internal packages directly

## Proposed Architecture

### Three-Layer Design

1. **Public API Layer** (`wile/`) - Clean embedding interface
2. **Registry Layer** (`registry/`) - Primitive registration infrastructure
3. **Extension Layer** (`extensions/*`) - Optional functionality packages

### Core Components

#### 1. Registry (`registry/registry.go`)

```go
package registry

import (
    "sync"

    "wile/machine"
)

// PrimitiveSpec defines a primitive to be registered.
type PrimitiveSpec struct {
    Name       string
    ParamCount int
    IsVariadic bool
    Impl       machine.ForeignFunction
}

// Phase indicates when a primitive is available.
type Phase int

const (
    PhaseRuntime Phase = 1 << iota
    PhaseExpand
    PhaseCompile
)

// Registry is the central registry for primitives.
type Registry struct {
    mu           sync.RWMutex
    primitives   []PrimitiveRegistration
    bindings     []string        // Compile-time only bindings
    initFuncs    []InitFunc
    macroSources []string
}

// NewRegistry creates a new empty registry.
func NewRegistry() *Registry {
    q := &Registry{
        primitives:   make([]PrimitiveRegistration, 0, 128),
        bindings:     make([]string, 0, 16),
        initFuncs:    make([]InitFunc, 0, 8),
        macroSources: make([]string, 0, 4),
    }
    return q
}

// AddPrimitive registers a primitive with the given phases.
func (r *Registry) AddPrimitive(spec PrimitiveSpec, phases Phase) {
    r.mu.Lock()
    defer r.mu.Unlock()
    r.primitives = append(r.primitives, PrimitiveRegistration{
        Spec:   spec,
        Phases: phases,
    })
}

// AddPrimitives registers multiple primitives with the given phases.
func (r *Registry) AddPrimitives(specs []PrimitiveSpec, phases Phase) {
    r.mu.Lock()
    defer r.mu.Unlock()
    for _, spec := range specs {
        r.primitives = append(r.primitives, PrimitiveRegistration{
            Spec:   spec,
            Phases: phases,
        })
    }
}

// Apply registers all primitives and runs init functions on an environment.
func (r *Registry) Apply(ctx context.Context, env *environment.TopLevelEnvironmentFrame) error {
    // ... registration logic
}
```

#### 2. Extension Interface (`registry/extension.go`)

```go
package registry

// Extension represents a loadable extension that adds primitives to a registry.
type Extension interface {
    // Name returns the extension name for logging/debugging.
    Name() string
    // AddToRegistry registers primitives with the registry.
    AddToRegistry(r *Registry) error
}

// NewExtension creates an Extension from a name and function.
func NewExtension(name string, fn func(*Registry) error) Extension {
    return &ExtensionFunc{name: name, fn: fn}
}
```

#### 3. RegistryBuilder (`registry/builder.go`)

```go
package registry

// RegistryBuilder collects functions that add primitives to a registry.
type RegistryBuilder []func(*Registry) error

// NewRegistryBuilder creates a builder with the given registration functions.
func NewRegistryBuilder(funcs ...func(*Registry) error) RegistryBuilder {
    var q RegistryBuilder
    q.Register(funcs...)
    return q
}

// AddToRegistry applies all registration functions to the registry.
func (b RegistryBuilder) AddToRegistry(r *Registry) error {
    for _, f := range b {
        if err := f(r); err != nil {
            return err
        }
    }
    return nil
}
```

#### 4. Public Engine API (`wile/engine.go`)

```go
// Package wile provides the public API for embedding the Wile Scheme interpreter.
package wile

// Engine is the main entry point for embedding Wile.
type Engine struct {
    env      *environment.TopLevelEnvironmentFrame
    registry *registry.Registry
}

// NewEngine creates a new Wile engine.
// By default, only core primitives are included.
// Use WithExtension to add optional extensions.
func NewEngine(opts ...EngineOption) (*Engine, error)

// Eval parses, compiles, and executes Scheme code, returning the result.
func (e *Engine) Eval(ctx context.Context, code string) (Value, error)

// Compile parses and compiles code without executing.
func (e *Engine) Compile(code string) (*CompiledCode, error)

// Run executes previously compiled code.
func (e *Engine) Run(ctx context.Context, cc *CompiledCode) (Value, error)

// Define binds a value to a name in the top-level environment.
func (e *Engine) Define(name string, value Value) error

// Get retrieves a value by name from the environment.
func (e *Engine) Get(name string) (Value, bool)

// RegisterPrimitive adds a Go function as a Scheme primitive.
func (e *Engine) RegisterPrimitive(spec PrimitiveSpec) error

// Call invokes a Scheme procedure with arguments.
func (e *Engine) Call(ctx context.Context, proc Value, args ...Value) (Value, error)
```

### Core vs Extension Primitives

#### Core Primitives (~85 primitives, always included)

These are required for bootstrap macros and basic Scheme semantics:

| Category | Count | Primitives |
|----------|-------|------------|
| Type Predicates | 19 | `null?`, `pair?`, `boolean?`, `number?`, `integer?`, `real?`, `rational?`, `complex?`, `exact?`, `inexact?`, `exact-integer?`, `symbol?`, `string?`, `char?`, `vector?`, `bytevector?`, `procedure?`, `list?`, `void?` |
| Boolean | 1 | `not` |
| Equality | 3 | `eq?`, `eqv?`, `equal?` |
| Pairs/Lists | 20 | `cons`, `car`, `cdr`, `set-car!`, `set-cdr!`, `list`, `make-list`, `append`, `reverse`, `length`, `list-ref`, `list-set!`, `list-tail`, `memq`, `memv`, `member`, `assq`, `assv`, `assoc`, `list?` |
| CxR Accessors | 28 | `caar`..`cddddr` (all 2-4 level combinations) |
| Arithmetic | 18 | `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `zero?`, `positive?`, `negative?`, `odd?`, `even?`, `abs`, `min`, `max`, `quotient`, `remainder`, `modulo`, `gcd`, `lcm` |
| Numeric Conversion | 2 | `exact`, `inexact` |
| Control | 6 | `apply`, `call/cc`, `call-with-current-continuation`, `values`, `call-with-values`, `dynamic-wind` |
| Vectors | 8 | `make-vector`, `vector`, `vector?`, `vector-length`, `vector-ref`, `vector-set!`, `vector->list`, `list->vector` |
| Strings | 10 | `make-string`, `string`, `string?`, `string-length`, `string-ref`, `string-set!`, `string->list`, `list->string`, `symbol->string`, `string->symbol` |
| Characters | 3 | `char?`, `char->integer`, `integer->char` |
| Bytevectors | 6 | `bytevector?`, `make-bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!`, `bytevector` |
| Syntax | 4 | `identifier?`, `syntax->datum`, `datum->syntax`, `generate-temporaries` |
| Parameters | 2 | `make-parameter`, `parameter?` |

#### Extension Primitives

| Extension | Count | Contents |
|-----------|-------|----------|
| `io` | ~25 | `read`, `write`, `display`, `newline`, port operations, string/bytevector ports |
| `files` | ~10 | `open-input-file`, `open-output-file`, `file-exists?`, `delete-file`, `call-with-*-file` |
| `system` | ~10 | `command-line`, `exit`, `emergency-exit`, `get-environment-variable`, `current-second`, `current-jiffy`, `features` |
| `math` | ~15 | `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `log`, `sqrt`, `expt`, `floor`, `ceiling`, `round`, `truncate` |
| `threads` | ~25 | SRFI-18: `make-thread`, `thread-start!`, `mutex-*`, `condition-variable-*` |
| `gointerop` | ~35 | `make-channel`, `channel-send!`, `atomic-*`, `rw-mutex-*`, `make-wait-group` |
| `records` | ~10 | `make-record-type`, `record-constructor`, `record-predicate`, `record-accessor`, `record-modifier` |
| `exceptions` | ~8 | `with-exception-handler`, `raise`, `raise-continuable`, `error`, `error-object?`, `error-object-message` |
| `eval` | ~6 | `eval`, `load`, `environment`, `interaction-environment`, `scheme-report-environment` |
| `syntax` | ~5 | `expand`, `expand-once`, `compile`, `syntax-local-value` |

### Extension Package Pattern

#### Example: Core Primitives (`registry/core/register.go`)

```go
// Package core provides the core primitives required for Scheme to function.
package core

import (
    "wile/registry"
)

// Extension is the core extension containing required primitives.
var Extension = registry.NewExtension("core", AddToRegistry)

// Builder aggregates all core registration functions.
var Builder = registry.NewRegistryBuilder(
    addSpecialForms,
    addPredicates,
    addEquality,
    addPairs,
    addLists,
    addArithmetic,
    addControl,
    addVectors,
    addStrings,
    addCharacters,
    addBytevectors,
    addSyntax,
    addParameters,
    addBootstrapMacros,
)

// AddToRegistry registers all core primitives.
var AddToRegistry = Builder.AddToRegistry
```

#### Example: I/O Extension (`extensions/io/register.go`)

```go
// Package io provides I/O primitives for reading and writing.
package io

import (
    "wile/registry"
    "wile/runtime/primitives"
)

// Extension is the I/O extension.
var Extension = registry.NewExtension("io", AddToRegistry)

var Builder = registry.NewRegistryBuilder(addPrimitives, addInit)
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"read", 0, true, primitives.PrimRead},
        {"write", 1, true, primitives.PrimWrite},
        {"display", 1, true, primitives.PrimDisplay},
        {"newline", 0, true, primitives.PrimNewline},
        // ... more I/O primitives
    }, registry.PhaseRuntime)
    return nil
}

func addInit(r *registry.Registry) error {
    r.AddInitFunc(initPortState)
    return nil
}
```

#### Example: All Extensions (`extensions/all/register.go`)

```go
// Package all provides a convenience extension that includes all standard extensions.
package all

import (
    "wile/extensions/io"
    "wile/extensions/files"
    "wile/extensions/system"
    "wile/extensions/math"
    "wile/extensions/threads"
    "wile/extensions/gointerop"
    "wile/registry"
)

// Extension includes all standard extensions.
var Extension = registry.NewExtension("all", AddToRegistry)

var Builder = registry.RegistryBuilder{
    io.AddToRegistry,
    files.AddToRegistry,
    system.AddToRegistry,
    math.AddToRegistry,
    threads.AddToRegistry,
    gointerop.AddToRegistry,
}

var AddToRegistry = Builder.AddToRegistry
```

#### Example: Third-Party Extension (`mycompany/wile-extensions/http/register.go`)

```go
package http

import (
    "context"
    "net/http"

    "github.com/aalpar/wile/go/machine"
    "github.com/aalpar/wile/go/registry"
    "github.com/aalpar/wile/go/values"
)

var Extension = registry.NewExtension("http", AddToRegistry)

var Builder = registry.NewRegistryBuilder(addPrimitives)
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"http-get", 1, true, PrimHttpGet},
        {"http-post", 2, true, PrimHttpPost},
        {"json-parse", 1, false, PrimJsonParse},
    }, registry.PhaseRuntime)
    return nil
}

func PrimHttpGet(ctx context.Context, mc *machine.MachineContext) error {
    url, ok := mc.Arg(0).(*values.String)
    if !ok {
        return values.WrapForeignErrorf(values.ErrWrongType, "http-get: expected string")
    }
    // ... implementation
}
```

### Independent REPL Package

The REPL is a separate package that consumes the `wile.Engine` API:

```go
// Package repl provides an interactive Read-Eval-Print Loop for Wile.
package repl

import (
    "wile"
)

// REPL is an interactive Scheme REPL.
type REPL struct {
    engine *wile.Engine
    // ...
}

// New creates a new REPL with the given engine.
func New(engine *wile.Engine, opts ...Option) (*REPL, error)

// Run starts the REPL loop.
func (r *REPL) Run(ctx context.Context) error
```

This design means:
- The REPL is not required for embedding Wile
- Users can implement custom REPLs or other frontends
- The REPL uses only the public `wile.Engine` API

## Package Organization

```
go/
├── wile/                       # Public embedding API
│   ├── engine.go               # Engine type, main API
│   ├── options.go              # EngineOption configuration
│   ├── value.go                # Value wrapper type
│   ├── compiled.go             # CompiledCode type
│   └── primitive.go            # PrimitiveSpec re-export
├── registry/
│   ├── registry.go             # Registry type
│   ├── builder.go              # RegistryBuilder type
│   ├── phase.go                # Phase constants
│   ├── register.go             # Apply functions
│   ├── extension.go            # Extension interface
│   └── core/                   # Core primitives (always included)
│       ├── register.go         # Core extension entry point
│       ├── predicates.go       # Type predicates
│       ├── equality.go         # eq?, eqv?, equal?
│       ├── lists.go            # List operations
│       ├── pairs.go            # Pair operations, CxR
│       ├── arithmetic.go       # Basic arithmetic
│       ├── control.go          # apply, call/cc, values
│       ├── vectors.go          # Vector operations
│       ├── strings.go          # String operations
│       ├── characters.go       # Character operations
│       ├── bytevectors.go      # Bytevector operations
│       ├── syntax.go           # Syntax primitives
│       ├── parameters.go       # Parameters
│       ├── specialforms.go     # Compile-time bindings
│       └── bootstrap.go        # Bootstrap macro source
├── extensions/                 # Optional extensions
│   ├── io/
│   │   └── register.go
│   ├── files/
│   │   └── register.go
│   ├── system/
│   │   └── register.go
│   ├── math/
│   │   └── register.go
│   ├── threads/
│   │   └── register.go
│   ├── gointerop/
│   │   └── register.go
│   ├── records/
│   │   └── register.go
│   ├── exceptions/
│   │   └── register.go
│   ├── eval/
│   │   └── register.go
│   ├── syntax/
│   │   └── register.go
│   └── all/
│       └── register.go         # Convenience: all extensions
├── repl/                       # Independent REPL
│   ├── repl.go
│   ├── readline.go
│   ├── debug.go
│   └── options.go
├── runtime/
│   └── primitives/             # Primitive implementations (unchanged)
│       ├── prim_add.go
│       ├── prim_car.go
│       └── ...
└── cmd/
    └── main.go                 # Uses wile.Engine + repl
```

## Dependency Rules

| Package | Can Import | Cannot Import |
|---------|------------|---------------|
| `wile/` | `registry/`, `machine/`, `environment/`, `values/` | `repl/`, `cmd/`, `extensions/*` |
| `registry/` | `machine/`, `environment/`, `values/` | `wile/`, `repl/`, `extensions/*` |
| `repl/` | `wile/` | `registry/`, `machine/`, `extensions/*` |
| `extensions/*` | `registry/`, `machine/`, `values/`, `runtime/primitives/` | `wile/`, `repl/` |
| `machine/` | `environment/`, `values/`, `syntax/`, `parser/` | `registry/`, `wile/`, `extensions/*` |

## Migration Strategy

### Phase 1: Create Registry Infrastructure

1. Create `registry/registry.go` with `Registry` type
2. Create `registry/builder.go` with `RegistryBuilder` type
3. Create `registry/extension.go` with `Extension` interface
4. Create `registry/phase.go` with phase constants
5. Add tests for registry components

### Phase 2: Create Core Primitives Package

1. Create `registry/core/` with all required primitive registrations
2. Extract primitive categories into separate files
3. Add bootstrap macro source
4. Test that core creates working environment

### Phase 3: Create Public API Package

1. Create `wile/engine.go` with `Engine` type
2. Create `wile/value.go` with `Value` wrapper
3. Create `wile/options.go` with configuration
4. Add comprehensive tests

### Phase 4: Extract Extension Packages

1. Create `extensions/io/` for I/O primitives
2. Create `extensions/files/` for file operations
3. Continue for all extension categories
4. Create `extensions/all/` convenience package

### Phase 5: Create REPL Package

1. Move REPL logic from `cmd/` to `repl/`
2. Update to use `wile.Engine` API
3. Keep debug command support

### Phase 6: Update Entry Points

1. Modify `cmd/main.go` to use `wile.Engine` + `repl`
2. Add `--minimal` flag for core-only mode
3. Update `runtime/environment_tiny.go` to use registry

### Phase 7: Documentation and Examples

1. Document embedding API
2. Provide extension development guide
3. Add example projects

## Comparison with Kubernetes Pattern

| Kubernetes | Wile |
|------------|------|
| `runtime.Scheme` | `registry.Registry` |
| `runtime.SchemeBuilder` | `registry.RegistryBuilder` |
| `schema.GroupVersionKind` | `Phase` (Runtime/Expand/Compile) |
| `AddKnownTypes()` | `AddPrimitives()` |
| `AddToScheme` function | `AddToRegistry` function |
| `init()` registration | `init()` registration (optional) |
| Type registration | Primitive registration |

## Usage Examples

### Minimal Embedding (Core Only)

```go
engine, _ := wile.NewEngine()
result, _ := engine.Eval(ctx, "(+ 1 2 3)")
fmt.Println(result) // 6
```

### With I/O Support

```go
engine, _ := wile.NewEngine(
    wile.WithExtension(io.Extension),
)
engine.Eval(ctx, `(display "Hello, World!")`)
```

### Full Featured

```go
engine, _ := wile.NewEngine(
    wile.WithExtension(all.Extension),
)
```

### Custom Primitives

```go
engine, _ := wile.NewEngine()
engine.RegisterPrimitive(wile.PrimitiveSpec{
    Name:       "get-player-health",
    ParamCount: 1,
    Impl:       myHealthFunc,
})
engine.Eval(ctx, "(get-player-health player)")
```

### Game Engine Scripting

```go
// Create minimal engine without I/O for sandboxing
engine, _ := wile.NewEngine()

// Add game-specific primitives
engine.RegisterPrimitive(wile.PrimitiveSpec{Name: "spawn-enemy", ...})
engine.RegisterPrimitive(wile.PrimitiveSpec{Name: "play-sound", ...})
engine.RegisterPrimitive(wile.PrimitiveSpec{Name: "get-player-pos", ...})

// Run user scripts safely
engine.Eval(ctx, userScript)
```

### Custom REPL

```go
engine, _ := wile.NewEngine(wile.WithExtension(all.Extension))
r, _ := repl.New(engine, repl.WithPrompt("scheme> "))
r.Run(ctx)
```

### Configuration DSL

```go
// Create engine with only core + custom config primitives
engine, _ := wile.NewEngine()
engine.RegisterPrimitive(wile.PrimitiveSpec{Name: "set-option", ...})
engine.RegisterPrimitive(wile.PrimitiveSpec{Name: "enable-feature", ...})

// Load config file - no I/O means users can't access filesystem
configCode, _ := os.ReadFile("config.scm")
engine.Eval(ctx, string(configCode))
```

### Using Go Values

```go
engine, _ := wile.NewEngine()

// Define Go values in Scheme
engine.Define("pi", wile.NewFloat(3.14159))
engine.Define("app-name", wile.NewString("MyApp"))

// Retrieve values
val, _ := engine.Get("some-result")
fmt.Println(val.SchemeString())

// Call Scheme procedures from Go
proc, _ := engine.Get("my-procedure")
result, _ := engine.Call(ctx, proc, wile.NewInteger(42))
```

## Benefits

1. **Embeddable**: Clean API for embedding in Go applications
2. **Composable**: Projects include exactly the primitives they need
3. **Safe**: No I/O by default enables sandboxed execution
4. **Extensible**: Third parties can add primitives without forking
5. **Testable**: Registries can be constructed for specific test scenarios
6. **Binary Size**: Minimal builds exclude unused functionality
7. **Familiar Pattern**: Go developers recognize Kubernetes-style registration

## Considerations

### Thread Safety

The `Registry` type uses `sync.RWMutex` for thread-safe registration. All registrations should complete before `Apply()` is called.

### Registration Order

Registration order matters for primitives with dependencies. Use `AddInitFunc` for initialization that must run after all primitives are registered.

### Error Handling

Registration errors are returned by `AddToRegistry`. In `init()` contexts, these should `panic()` to fail fast on misconfiguration.

### Bootstrap Macros

The `AddMacroSource` method allows extensions to provide Scheme macro definitions that are loaded after primitives are registered. Core macros (`and`, `or`, `let`, `cond`, etc.) are included in `registry/core`.

## Conclusion

This architecture provides a clean, embeddable way to use Wile in Go applications. It separates core language functionality from optional features, allowing users to compose exactly the capabilities they need. The design follows proven patterns from the Kubernetes ecosystem while adapting them to Wile's specific requirements around phase-aware primitive registration and macro expansion.

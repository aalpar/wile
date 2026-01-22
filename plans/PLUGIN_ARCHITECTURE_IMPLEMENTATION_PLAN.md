# Plugin Architecture Implementation Plan

This document provides a detailed, step-by-step implementation plan for the plugin architecture described in `PLUGIN_ARCHITECTURE_PROPOSAL.md`, revised to support embedding Wile as a library with optional extensions and an independent REPL.

## Architecture Overview

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

1. **Embeddable**: Wile can be embedded in Go applications with a clean API
2. **Minimal Core**: Only primitives required for the language itself are mandatory
3. **Optional Extensions**: I/O, threading, system calls are opt-in
4. **Independent REPL**: The REPL is just another consumer of the engine
5. **User Extensibility**: Easy to add custom primitives
6. **No I/O by Default**: Safe for sandboxed environments

---

## Dependency Analysis

### Package Dependency Graph

```
                         cmd/main.go
                              │
                    ┌─────────┼─────────┐
                    ▼         ▼         ▼
                  repl/     wile/    extensions/*
                    │         │         │
                    └────┬────┘         │
                         ▼              │
                      registry/ ◄───────┘
                         │
                         ▼
                      machine/
                         │
              ┌──────────┼──────────┐
              ▼          ▼          ▼
           parser/   validate/   environment/
              │          │          │
              ▼          ▼          ▼
          tokenizer/  forms/     syntax/
                         │          │
                         ▼          ▼
                              values/
```

### Key Import Rules

| Package | Can Import | Cannot Import |
|---------|------------|---------------|
| `wile/` | `registry/`, `machine/`, `environment/`, `values/` | `repl/`, `cmd/`, `extensions/*` |
| `registry/` | `machine/`, `environment/`, `values/` | `wile/`, `repl/`, `extensions/*` |
| `repl/` | `wile/` | `registry/`, `machine/`, `extensions/*` |
| `extensions/*` | `registry/`, `machine/`, `values/`, `runtime/primitives/` | `wile/`, `repl/` |
| `machine/` | `environment/`, `values/`, `syntax/`, `parser/` | `registry/`, `wile/`, `extensions/*` |

### Existing Cycle-Breaking Pattern

The codebase uses dependency injection for `machine ↔ runtime`:

```go
// machine/library_loader.go:43
var LibraryEnvFactory func(ctx context.Context) (*environment.EnvironmentFrame, error)

// Set at initialization in cmd/main.go
machine.LibraryEnvFactory = runtime.NewTopLevelEnvironmentFrameTiny
```

This pattern will be extended for the new architecture.

---

## Core vs Extension Primitives

### Core Primitives (~85 primitives)

These are **always included** because bootstrap macros and basic Scheme semantics require them:

| Category | Count | Primitives |
|----------|-------|------------|
| Type Predicates | 15 | `null?`, `pair?`, `boolean?`, `number?`, `integer?`, `real?`, `rational?`, `complex?`, `exact?`, `inexact?`, `symbol?`, `string?`, `char?`, `vector?`, `procedure?`, `list?` |
| Boolean | 1 | `not` |
| Equality | 3 | `eq?`, `eqv?`, `equal?` |
| Pairs/Lists | 20 | `cons`, `car`, `cdr`, `set-car!`, `set-cdr!`, `list`, `make-list`, `append`, `reverse`, `length`, `list-ref`, `list-set!`, `list-tail`, `memq`, `memv`, `member`, `assq`, `assv`, `assoc`, `list?` |
| CxR Accessors | 28 | `caar`..`cddddr` (all 2-4 level combinations) |
| Arithmetic | 15 | `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `zero?`, `positive?`, `negative?`, `odd?`, `even?`, `abs` |
| Numeric Conversion | 4 | `exact`, `inexact`, `exact?`, `inexact?` |
| Control | 6 | `apply`, `call/cc`, `call-with-current-continuation`, `values`, `call-with-values`, `dynamic-wind` |
| Vectors | 8 | `make-vector`, `vector`, `vector?`, `vector-length`, `vector-ref`, `vector-set!`, `vector->list`, `list->vector` |
| Strings | 10 | `make-string`, `string`, `string?`, `string-length`, `string-ref`, `string-set!`, `string->list`, `list->string`, `symbol->string`, `string->symbol` |
| Characters | 3 | `char?`, `char->integer`, `integer->char` |
| Bytevectors | 6 | `bytevector?`, `make-bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!`, `bytevector` |
| Syntax | 4 | `identifier?`, `syntax->datum`, `datum->syntax`, `generate-temporaries` |
| Parameters | 2 | `make-parameter`, `parameter?` |

### Extension Primitives

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
| `syntax-extended` | ~5 | `expand`, `expand-once`, `compile`, `syntax-local-value` |

---

## Implementation Phases

### Phase 1: Create Registry Package

#### 1.1 Directory Structure

```
go/registry/
├── registry.go         # Registry type
├── builder.go          # RegistryBuilder type
├── phase.go            # Phase constants
├── register.go         # Apply and helper functions
├── extension.go        # Extension interface
├── registry_test.go    # Tests
├── builder_test.go     # Tests
└── CLAUDE.md           # Documentation
```

#### 1.2 File: `go/registry/phase.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package registry

// Phase indicates when a primitive is available.
type Phase int

const (
    // PhaseRuntime indicates the primitive is available at runtime.
    PhaseRuntime Phase = 1 << iota
    // PhaseExpand indicates the primitive is available during macro expansion.
    PhaseExpand
    // PhaseCompile indicates the primitive is a compile-time binding (no value).
    PhaseCompile
)

// HasRuntime returns true if the phase includes runtime.
func (p Phase) HasRuntime() bool { return p&PhaseRuntime != 0 }

// HasExpand returns true if the phase includes expand time.
func (p Phase) HasExpand() bool { return p&PhaseExpand != 0 }

// HasCompile returns true if the phase includes compile time.
func (p Phase) HasCompile() bool { return p&PhaseCompile != 0 }

// String returns a string representation of the phase.
func (p Phase) String() string {
    var parts []string
    if p.HasRuntime() {
        parts = append(parts, "runtime")
    }
    if p.HasExpand() {
        parts = append(parts, "expand")
    }
    if p.HasCompile() {
        parts = append(parts, "compile")
    }
    if len(parts) == 0 {
        return "none"
    }
    return strings.Join(parts, "|")
}
```

#### 1.3 File: `go/registry/extension.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package registry

// Extension represents a loadable extension that adds primitives to a registry.
type Extension interface {
    // Name returns the extension name for logging/debugging.
    Name() string
    // AddToRegistry registers primitives with the registry.
    AddToRegistry(r *Registry) error
}

// ExtensionFunc adapts a function to the Extension interface.
type ExtensionFunc struct {
    name string
    fn   func(*Registry) error
}

// NewExtension creates an Extension from a name and function.
func NewExtension(name string, fn func(*Registry) error) Extension {
    return &ExtensionFunc{name: name, fn: fn}
}

func (e *ExtensionFunc) Name() string                    { return e.name }
func (e *ExtensionFunc) AddToRegistry(r *Registry) error { return e.fn(r) }
```

#### 1.4 File: `go/registry/registry.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

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

// PrimitiveRegistration holds a primitive and its phases.
type PrimitiveRegistration struct {
    Spec   PrimitiveSpec
    Phases Phase
}

// InitFunc is called after primitives are registered.
type InitFunc func(ApplyContext) error

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

// AddBinding registers a compile-time only binding (no runtime value).
func (r *Registry) AddBinding(name string) {
    r.mu.Lock()
    defer r.mu.Unlock()
    r.bindings = append(r.bindings, name)
}

// AddBindings registers multiple compile-time only bindings.
func (r *Registry) AddBindings(names []string) {
    r.mu.Lock()
    defer r.mu.Unlock()
    r.bindings = append(r.bindings, names...)
}

// AddInitFunc registers an initialization function.
func (r *Registry) AddInitFunc(f InitFunc) {
    r.mu.Lock()
    defer r.mu.Unlock()
    r.initFuncs = append(r.initFuncs, f)
}

// AddMacroSource adds Scheme source code for bootstrap macros.
func (r *Registry) AddMacroSource(source string) {
    r.mu.Lock()
    defer r.mu.Unlock()
    r.macroSources = append(r.macroSources, source)
}

// PrimitiveCount returns the number of registered primitives.
func (r *Registry) PrimitiveCount() int {
    r.mu.RLock()
    defer r.mu.RUnlock()
    return len(r.primitives)
}

// BindingCount returns the number of compile-time bindings.
func (r *Registry) BindingCount() int {
    r.mu.RLock()
    defer r.mu.RUnlock()
    return len(r.bindings)
}

// MacroSources returns copies of macro source strings.
func (r *Registry) MacroSources() []string {
    r.mu.RLock()
    defer r.mu.RUnlock()
    result := make([]string, len(r.macroSources))
    copy(result, r.macroSources)
    return result
}

// Clone creates a copy of the registry.
func (r *Registry) Clone() *Registry {
    r.mu.RLock()
    defer r.mu.RUnlock()

    q := &Registry{
        primitives:   make([]PrimitiveRegistration, len(r.primitives)),
        bindings:     make([]string, len(r.bindings)),
        initFuncs:    make([]InitFunc, len(r.initFuncs)),
        macroSources: make([]string, len(r.macroSources)),
    }
    copy(q.primitives, r.primitives)
    copy(q.bindings, r.bindings)
    copy(q.initFuncs, r.initFuncs)
    copy(q.macroSources, r.macroSources)
    return q
}
```

#### 1.5 File: `go/registry/register.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package registry

import (
    "context"

    "wile/environment"
    "wile/machine"
    "wile/values"
)

// ApplyContext provides context during registry application.
type ApplyContext interface {
    Context() context.Context
    Environment() *environment.TopLevelEnvironmentFrame
}

type applyContext struct {
    ctx context.Context
    env *environment.TopLevelEnvironmentFrame
}

func (a *applyContext) Context() context.Context                            { return a.ctx }
func (a *applyContext) Environment() *environment.TopLevelEnvironmentFrame { return a.env }

// Apply registers all primitives and runs init functions on an environment.
func (r *Registry) Apply(ctx context.Context, env *environment.TopLevelEnvironmentFrame) error {
    r.mu.RLock()
    defer r.mu.RUnlock()

    // Register compile-time bindings first
    for _, name := range r.bindings {
        err := registerCompileTimeBinding(env, name)
        if err != nil {
            return err
        }
    }

    // Register compile-time primitives (bindings only, no values)
    for _, reg := range r.primitives {
        if reg.Phases.HasCompile() && !reg.Phases.HasRuntime() {
            err := registerCompileTimeBinding(env, reg.Spec.Name)
            if err != nil {
                return err
            }
        }
    }

    // Register runtime primitives
    for _, reg := range r.primitives {
        if reg.Phases.HasRuntime() {
            err := registerRuntimePrimitive(env, reg.Spec)
            if err != nil {
                return err
            }
        }
    }

    // Register expand-time primitives
    for _, reg := range r.primitives {
        if reg.Phases.HasExpand() {
            err := registerExpandTimePrimitive(env, reg.Spec)
            if err != nil {
                return err
            }
        }
    }

    // Run initialization functions
    actx := &applyContext{ctx: ctx, env: env}
    for _, f := range r.initFuncs {
        err := f(actx)
        if err != nil {
            return err
        }
    }

    return nil
}

func registerCompileTimeBinding(env *environment.TopLevelEnvironmentFrame, name string) error {
    compileEnv := env.Compile()
    sym := compileEnv.InternSymbol(values.NewSymbol(name))
    compileEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive)
    return nil
}

func registerRuntimePrimitive(env *environment.TopLevelEnvironmentFrame, spec PrimitiveSpec) error {
    sym := env.InternSymbol(values.NewSymbol(spec.Name))
    env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

    closure := machine.NewForeignClosure(
        env.EnvironmentFrame(),
        spec.ParamCount,
        spec.IsVariadic,
        spec.Impl,
    )

    return env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
}

func registerExpandTimePrimitive(env *environment.TopLevelEnvironmentFrame, spec PrimitiveSpec) error {
    expandEnv := env.Expand()
    sym := expandEnv.InternSymbol(values.NewSymbol(spec.Name))
    expandEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

    closure := machine.NewForeignClosure(
        expandEnv,
        spec.ParamCount,
        spec.IsVariadic,
        spec.Impl,
    )

    return expandEnv.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
}
```

#### 1.6 File: `go/registry/builder.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package registry

// RegistryBuilder collects functions that add primitives to a registry.
type RegistryBuilder []func(*Registry) error

// NewRegistryBuilder creates a builder with the given registration functions.
func NewRegistryBuilder(funcs ...func(*Registry) error) RegistryBuilder {
    var q RegistryBuilder
    q.Register(funcs...)
    return q
}

// Register adds registration functions to the builder.
func (b *RegistryBuilder) Register(funcs ...func(*Registry) error) {
    *b = append(*b, funcs...)
}

// AddToRegistry applies all registration functions to the registry.
func (b RegistryBuilder) AddToRegistry(r *Registry) error {
    for _, f := range b {
        err := f(r)
        if err != nil {
            return err
        }
    }
    return nil
}

// Build creates a new registry and applies all registration functions.
func (b RegistryBuilder) Build() (*Registry, error) {
    r := NewRegistry()
    err := b.AddToRegistry(r)
    if err != nil {
        return nil, err
    }
    return r, nil
}
```

---

### Phase 2: Create Core Primitives Package

#### 2.1 Directory Structure

```
go/registry/core/
├── register.go         # Main registration, exports CoreExtension
├── predicates.go       # Type predicates
├── equality.go         # eq?, eqv?, equal?
├── lists.go            # List operations
├── pairs.go            # Pair operations, CxR accessors
├── arithmetic.go       # Basic arithmetic
├── control.go          # apply, call/cc, values
├── vectors.go          # Vector operations
├── strings.go          # String operations
├── characters.go       # Character operations
├── bytevectors.go      # Bytevector operations
├── syntax.go           # Syntax primitives for macros
├── parameters.go       # make-parameter, parameter?
├── specialforms.go     # Compile-time bindings
├── bootstrap.go        # Bootstrap macro source
└── CLAUDE.md           # Documentation
```

#### 2.2 File: `go/registry/core/register.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

// Package core provides the core primitives required for Scheme to function.
// These primitives are always included and cannot be omitted.
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

#### 2.3 File: `go/registry/core/predicates.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package core

import (
    "wile/registry"
    "wile/runtime/primitives"
)

func addPredicates(r *registry.Registry) error {
    // Type predicates available at both runtime and expand time
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"null?", 1, false, primitives.PrimNullQ},
        {"pair?", 1, false, primitives.PrimPairQ},
        {"boolean?", 1, false, primitives.PrimBooleanQ},
        {"number?", 1, false, primitives.PrimNumberQ},
        {"integer?", 1, false, primitives.PrimIntegerQ},
        {"real?", 1, false, primitives.PrimRealQ},
        {"rational?", 1, false, primitives.PrimRationalQ},
        {"complex?", 1, false, primitives.PrimComplexQ},
        {"exact?", 1, false, primitives.PrimExactQ},
        {"inexact?", 1, false, primitives.PrimInexactQ},
        {"exact-integer?", 1, false, primitives.PrimExactIntegerQ},
        {"symbol?", 1, false, primitives.PrimSymbolQ},
        {"string?", 1, false, primitives.PrimStringQ},
        {"char?", 1, false, primitives.PrimCharQ},
        {"vector?", 1, false, primitives.PrimVectorQ},
        {"bytevector?", 1, false, primitives.PrimBytevectorQ},
        {"procedure?", 1, false, primitives.PrimProcedureQ},
        {"list?", 1, false, primitives.PrimListQ},
        {"void?", 1, false, primitives.PrimVoidQ},
    }, registry.PhaseRuntime|registry.PhaseExpand)

    // Numeric predicates
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"zero?", 1, false, primitives.PrimZeroQ},
        {"positive?", 1, false, primitives.PrimPositiveQ},
        {"negative?", 1, false, primitives.PrimNegativeQ},
        {"odd?", 1, false, primitives.PrimOddQ},
        {"even?", 1, false, primitives.PrimEvenQ},
    }, registry.PhaseRuntime|registry.PhaseExpand)

    return nil
}
```

#### 2.4 File: `go/registry/core/arithmetic.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package core

import (
    "wile/registry"
    "wile/runtime/primitives"
)

func addArithmetic(r *registry.Registry) error {
    // Basic arithmetic (required for macros and basic computation)
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"+", 0, true, primitives.PrimAdd},
        {"-", 1, true, primitives.PrimSubtract},
        {"*", 0, true, primitives.PrimMultiply},
        {"/", 1, true, primitives.PrimDivide},
    }, registry.PhaseRuntime|registry.PhaseExpand)

    // Comparisons
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"=", 2, true, primitives.PrimNumEq},
        {"<", 2, true, primitives.PrimNumLt},
        {">", 2, true, primitives.PrimNumGt},
        {"<=", 2, true, primitives.PrimNumLte},
        {">=", 2, true, primitives.PrimNumGte},
    }, registry.PhaseRuntime|registry.PhaseExpand)

    // Basic numeric operations
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"abs", 1, false, primitives.PrimAbs},
        {"min", 1, true, primitives.PrimMin},
        {"max", 1, true, primitives.PrimMax},
        {"quotient", 2, false, primitives.PrimQuotient},
        {"remainder", 2, false, primitives.PrimRemainder},
        {"modulo", 2, false, primitives.PrimModulo},
        {"gcd", 0, true, primitives.PrimGcd},
        {"lcm", 0, true, primitives.PrimLcm},
    }, registry.PhaseRuntime|registry.PhaseExpand)

    // Exactness conversion
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"exact", 1, false, primitives.PrimExact},
        {"inexact", 1, false, primitives.PrimInexact},
    }, registry.PhaseRuntime|registry.PhaseExpand)

    return nil
}
```

#### 2.5 File: `go/registry/core/specialforms.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package core

import (
    "wile/registry"
)

// compileTimeBindings are names that exist only at compile time.
// These are handled specially by the compiler and should NOT have
// their arguments expanded by the macro expander.
var compileTimeBindings = []string{
    "if",
    "lambda",
    "quote",
    "define",
    "define-syntax",
    "set!",
    "begin",
    "include",
    "quasiquote",
    "unquote",
    "unquote-splicing",
    "cond-expand",
    "define-for-syntax",
    "begin-for-syntax",
    "eval-when",
    "syntax-case",
    "syntax",
}

func addSpecialForms(r *registry.Registry) error {
    r.AddBindings(compileTimeBindings)
    return nil
}
```

#### 2.6 File: `go/registry/core/bootstrap.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package core

import (
    "wile/registry"
)

// bootstrapMacroSource contains essential derived expression forms.
// These macros are required for standard Scheme to work.
const bootstrapMacroSource = `
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...)))
        tag)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...) body1 body2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...)
     (letrec "generate_temp_names" (var1 ...) () ((var1 init1) ...) body ...))
    ((letrec "generate_temp_names" () (temp1 ...) ((var1 init1) ...) body ...)
     (let ((var1 (if #f #f)) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1) ...
         body ...)))
    ((letrec "generate_temp_names" (x y ...) (temp ...) ((var1 init1) ...) body ...)
     (letrec "generate_temp_names" (y ...) (newtemp temp ...) ((var1 init1) ...) body ...))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp (result temp) (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (%make-lazy-promise (lambda () expression)))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (%make-lazy-promise (lambda () expression)))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body ...)
     (begin body ...))
    ((parameterize ((param value) rest ...) body ...)
     (call-with-parameterize param value
       (lambda () (parameterize (rest ...) body ...))))))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     (guard-aux (var clause ...) e1 e2 ...))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux (var (else result1 result2 ...)) e1 e2 ...)
     (with-exception-handler
       (lambda (var) result1 result2 ...)
       (lambda () e1 e2 ...)))
    ((guard-aux (var (test => result) clause ...) e1 e2 ...)
     (with-exception-handler
       (lambda (var)
         (let ((temp test))
           (if temp
               (result temp)
               (guard-aux (var clause ...) (raise-continuable var)))))
       (lambda () e1 e2 ...)))
    ((guard-aux (var (test) clause ...) e1 e2 ...)
     (with-exception-handler
       (lambda (var)
         (if test
             test
             (guard-aux (var clause ...) (raise-continuable var))))
       (lambda () e1 e2 ...)))
    ((guard-aux (var (test result1 result2 ...) clause ...) e1 e2 ...)
     (with-exception-handler
       (lambda (var)
         (if test
             (begin result1 result2 ...)
             (guard-aux (var clause ...) (raise-continuable var))))
       (lambda () e1 e2 ...)))
    ((guard-aux (var) e1 e2 ...)
     (with-exception-handler
       (lambda (var) (raise-continuable var))
       (lambda () e1 e2 ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...) (test expr ...) command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin (if #f #f) expr ...)
               (begin
                 command ...
                 (loop (do "step" var step ...) ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))
`

func addBootstrapMacros(r *registry.Registry) error {
    r.AddMacroSource(bootstrapMacroSource)
    return nil
}
```

---

### Phase 3: Create Public API Package

#### 3.1 Directory Structure

```
go/wile/
├── engine.go           # Engine type, main API
├── options.go          # EngineOption, configuration
├── value.go            # Value wrapper type
├── compiled.go         # CompiledCode type
├── primitive.go        # PrimitiveSpec re-export
├── error.go            # Public error types
├── doc.go              # Package documentation
└── CLAUDE.md           # Developer documentation
```

#### 3.2 File: `go/wile/engine.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

// Package wile provides the public API for embedding the Wile Scheme interpreter.
//
// Basic usage:
//
//     engine, err := wile.NewEngine()
//     if err != nil {
//         log.Fatal(err)
//     }
//     result, err := engine.Eval(ctx, "(+ 1 2 3)")
//     fmt.Println(result) // 6
//
// With extensions:
//
//     engine, err := wile.NewEngine(
//         wile.WithExtension(io.Extension),
//         wile.WithExtension(system.Extension),
//     )
package wile

import (
    "bufio"
    "context"
    "strings"

    "wile/environment"
    "wile/machine"
    "wile/parser"
    "wile/registry"
    "wile/registry/core"
    "wile/values"
)

// Engine is the main entry point for embedding Wile.
type Engine struct {
    env      *environment.TopLevelEnvironmentFrame
    registry *registry.Registry
}

// NewEngine creates a new Wile engine.
// By default, only core primitives are included.
// Use WithExtension to add optional extensions.
func NewEngine(opts ...EngineOption) (*Engine, error) {
    cfg := &engineConfig{
        registry: nil,
    }
    for _, opt := range opts {
        opt(cfg)
    }

    // Build registry
    reg := cfg.registry
    if reg == nil {
        reg = registry.NewRegistry()
        err := core.AddToRegistry(reg)
        if err != nil {
            return nil, err
        }
    }

    // Add any additional extensions
    for _, ext := range cfg.extensions {
        err := ext.AddToRegistry(reg)
        if err != nil {
            return nil, err
        }
    }

    // Create environment
    env := environment.NewTopLevelEnvironmentFrame()

    // Apply registry
    ctx := context.Background()
    err := reg.Apply(ctx, env)
    if err != nil {
        return nil, err
    }

    // Register syntax compilers and primitive expanders
    machine.RegisterSyntaxCompilers(env.EnvironmentFrame())
    machine.RegisterPrimitiveExpanders(env.EnvironmentFrame())

    // Load bootstrap macros
    err = loadBootstrapMacros(ctx, env, reg.MacroSources())
    if err != nil {
        return nil, err
    }

    q := &Engine{
        env:      env,
        registry: reg,
    }
    return q, nil
}

// Eval parses, compiles, and executes Scheme code, returning the result.
func (e *Engine) Eval(ctx context.Context, code string) (Value, error) {
    compiled, err := e.Compile(code)
    if err != nil {
        return nil, err
    }
    return e.Run(ctx, compiled)
}

// EvalMultiple evaluates multiple expressions, returning the last result.
func (e *Engine) EvalMultiple(ctx context.Context, code string) (Value, error) {
    reader := strings.NewReader(code)
    p := parser.NewParser(e.env.EnvironmentFrame(), true, reader)

    var lastResult Value
    for {
        stx, err := p.ReadSyntax(ctx)
        if err != nil {
            if isEOF(err) {
                break
            }
            return nil, err
        }

        compiled, err := e.compileExpr(stx)
        if err != nil {
            return nil, err
        }

        result, err := e.runCompiled(ctx, compiled)
        if err != nil {
            return nil, err
        }
        lastResult = result
    }

    return lastResult, nil
}

// Compile parses and compiles code without executing.
func (e *Engine) Compile(code string) (*CompiledCode, error) {
    reader := strings.NewReader(code)
    p := parser.NewParser(e.env.EnvironmentFrame(), true, reader)

    stx, err := p.ReadSyntax(context.Background())
    if err != nil {
        return nil, err
    }

    return e.compileExpr(stx)
}

// Run executes previously compiled code.
func (e *Engine) Run(ctx context.Context, cc *CompiledCode) (Value, error) {
    return e.runCompiled(ctx, cc)
}

// Define binds a value to a name in the top-level environment.
func (e *Engine) Define(name string, value Value) error {
    sym := e.env.InternSymbol(values.NewSymbol(name))
    e.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
    return e.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), unwrapValue(value))
}

// Get retrieves a value by name from the environment.
func (e *Engine) Get(name string) (Value, bool) {
    sym := e.env.InternSymbol(values.NewSymbol(name))
    idx := environment.NewGlobalIndex(sym)
    val, err := e.env.GetGlobalValue(idx)
    if err != nil {
        return nil, false
    }
    return wrapValue(val), true
}

// RegisterPrimitive adds a Go function as a Scheme primitive.
func (e *Engine) RegisterPrimitive(spec PrimitiveSpec) error {
    sym := e.env.InternSymbol(values.NewSymbol(spec.Name))
    e.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

    closure := machine.NewForeignClosure(
        e.env.EnvironmentFrame(),
        spec.ParamCount,
        spec.IsVariadic,
        spec.Impl,
    )

    return e.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
}

// Call invokes a Scheme procedure with arguments.
func (e *Engine) Call(ctx context.Context, proc Value, args ...Value) (Value, error) {
    closure, ok := unwrapValue(proc).(*machine.MachineClosure)
    if !ok {
        return nil, &Error{Message: "not a procedure"}
    }

    unwrappedArgs := make([]values.Value, len(args))
    for i, arg := range args {
        unwrappedArgs[i] = unwrapValue(arg)
    }

    mc, err := machine.NewMachineContextForCall(ctx, e.env.EnvironmentFrame(), closure, unwrappedArgs...)
    if err != nil {
        return nil, err
    }

    err = mc.Run()
    if err != nil {
        return nil, err
    }

    return wrapValue(mc.GetValue()), nil
}

// Environment returns the underlying environment for advanced use.
func (e *Engine) Environment() *environment.TopLevelEnvironmentFrame {
    return e.env
}

// internal helpers

func (e *Engine) compileExpr(stx interface{}) (*CompiledCode, error) {
    tpl := machine.NewNativeTemplate(0, 0, false)

    ectx := machine.NewExpandTimeCallContext()
    expanded, err := machine.NewExpanderTimeContinuation(e.env.EnvironmentFrame()).ExpandExpression(ectx, stx)
    if err != nil {
        return nil, err
    }

    cctx := machine.NewCompileTimeCallContext(false, true, e.env.EnvironmentFrame())
    err = machine.NewCompiletimeContinuation(tpl, e.env.EnvironmentFrame()).CompileExpression(cctx, expanded)
    if err != nil {
        return nil, err
    }

    return &CompiledCode{template: tpl, env: e.env}, nil
}

func (e *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
    cont := machine.NewMachineContinuation(nil, cc.template, cc.env.EnvironmentFrame())
    mc := machine.NewMachineContext(ctx, cont)
    err := mc.Run()
    if err != nil {
        return nil, err
    }
    return wrapValue(mc.GetValue()), nil
}

func loadBootstrapMacros(ctx context.Context, env *environment.TopLevelEnvironmentFrame, sources []string) error {
    for _, source := range sources {
        reader := bufio.NewReader(strings.NewReader(source))
        p := parser.NewParser(env.EnvironmentFrame(), true, reader)

        for {
            stx, err := p.ReadSyntax(ctx)
            if err != nil {
                if isEOF(err) {
                    break
                }
                return err
            }

            tpl := machine.NewNativeTemplate(0, 0, false)
            ectx := machine.NewExpandTimeCallContext()
            expanded, err := machine.NewExpanderTimeContinuation(env.EnvironmentFrame()).ExpandExpression(ectx, stx)
            if err != nil {
                return err
            }

            cctx := machine.NewCompileTimeCallContext(false, true, env.EnvironmentFrame())
            err = machine.NewCompiletimeContinuation(tpl, env.EnvironmentFrame()).CompileExpression(cctx, expanded)
            if err != nil {
                return err
            }

            cont := machine.NewMachineContinuation(nil, tpl, env.EnvironmentFrame())
            mc := machine.NewMachineContext(ctx, cont)
            err = mc.Run()
            if err != nil {
                return err
            }
        }
    }
    return nil
}
```

#### 3.3 File: `go/wile/options.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package wile

import (
    "wile/registry"
)

type engineConfig struct {
    registry   *registry.Registry
    extensions []registry.Extension
}

// EngineOption configures an Engine.
type EngineOption func(*engineConfig)

// WithRegistry uses a custom registry instead of the default.
// When set, core primitives are NOT automatically added.
func WithRegistry(r *registry.Registry) EngineOption {
    return func(cfg *engineConfig) {
        cfg.registry = r
    }
}

// WithExtension adds an extension to the engine.
func WithExtension(ext registry.Extension) EngineOption {
    return func(cfg *engineConfig) {
        cfg.extensions = append(cfg.extensions, ext)
    }
}

// WithExtensions adds multiple extensions to the engine.
func WithExtensions(exts ...registry.Extension) EngineOption {
    return func(cfg *engineConfig) {
        cfg.extensions = append(cfg.extensions, exts...)
    }
}
```

#### 3.4 File: `go/wile/value.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package wile

import (
    "wile/values"
)

// Value represents a Scheme value in the public API.
type Value interface {
    // SchemeString returns the Scheme representation.
    SchemeString() string
    // Type returns the Scheme type name.
    Type() string
    // IsVoid returns true if this is the void value.
    IsVoid() bool
    // internal returns the underlying values.Value
    internal() values.Value
}

type wrappedValue struct {
    v values.Value
}

func (w *wrappedValue) SchemeString() string { return w.v.SchemeString() }
func (w *wrappedValue) Type() string         { return w.v.TypeName() }
func (w *wrappedValue) IsVoid() bool         { return w.v == values.VoidValue }
func (w *wrappedValue) internal() values.Value { return w.v }

func wrapValue(v values.Value) Value {
    if v == nil {
        return nil
    }
    return &wrappedValue{v: v}
}

func unwrapValue(v Value) values.Value {
    if v == nil {
        return nil
    }
    return v.internal()
}

// Helper constructors for creating Scheme values from Go values

// NewInteger creates a Scheme integer.
func NewInteger(n int64) Value {
    return wrapValue(values.NewInteger(n))
}

// NewFloat creates a Scheme inexact real.
func NewFloat(f float64) Value {
    return wrapValue(values.NewFlonum(f))
}

// NewString creates a Scheme string.
func NewString(s string) Value {
    return wrapValue(values.NewString(s))
}

// NewSymbol creates a Scheme symbol.
func NewSymbol(s string) Value {
    return wrapValue(values.NewSymbol(s))
}

// NewBoolean creates a Scheme boolean.
func NewBoolean(b bool) Value {
    if b {
        return wrapValue(values.TrueValue)
    }
    return wrapValue(values.FalseValue)
}

// NewList creates a Scheme list from values.
func NewList(vals ...Value) Value {
    if len(vals) == 0 {
        return wrapValue(values.EmptyList)
    }
    unwrapped := make([]values.Value, len(vals))
    for i, v := range vals {
        unwrapped[i] = unwrapValue(v)
    }
    return wrapValue(values.NewListFromSlice(unwrapped))
}

// Null is the empty list.
var Null Value = wrapValue(values.EmptyList)

// Void is the void value.
var Void Value = wrapValue(values.VoidValue)

// True is the #t value.
var True Value = wrapValue(values.TrueValue)

// False is the #f value.
var False Value = wrapValue(values.FalseValue)
```

---

### Phase 4: Create Extension Packages

#### 4.1 Directory Structure

```
go/extensions/
├── io/
│   └── register.go
├── files/
│   └── register.go
├── system/
│   └── register.go
├── math/
│   └── register.go
├── threads/
│   └── register.go
├── gointerop/
│   └── register.go
├── records/
│   └── register.go
├── exceptions/
│   └── register.go
├── eval/
│   └── register.go
├── syntax/
│   └── register.go
└── all/
    └── register.go     # Convenience: all extensions
```

#### 4.2 File: `go/extensions/io/register.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

// Package io provides I/O primitives for reading and writing.
package io

import (
    "wile/environment"
    "wile/machine"
    "wile/registry"
    "wile/runtime/primitives"
    "wile/values"
)

// Extension is the I/O extension.
var Extension = registry.NewExtension("io", AddToRegistry)

var Builder = registry.NewRegistryBuilder(addPrimitives, addInit)
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
    // Reading
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"read", 0, true, primitives.PrimRead},
        {"read-char", 0, true, primitives.PrimReadChar},
        {"peek-char", 0, true, primitives.PrimPeekChar},
        {"read-line", 0, true, primitives.PrimReadLine},
        {"read-syntax", 0, true, primitives.PrimReadSyntax},
        {"read-token", 0, true, primitives.PrimReadToken},
        {"char-ready?", 0, true, primitives.PrimCharReadyQ},
    }, registry.PhaseRuntime)

    // Writing
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"write", 1, true, primitives.PrimWrite},
        {"write-char", 1, true, primitives.PrimWriteChar},
        {"display", 1, true, primitives.PrimDisplay},
        {"newline", 0, true, primitives.PrimNewline},
        {"write-simple", 1, true, primitives.PrimWriteSimple},
        {"write-shared", 1, true, primitives.PrimWriteShared},
        {"flush-output-port", 0, true, primitives.PrimFlushOutputPort},
    }, registry.PhaseRuntime)

    // String/bytevector ports
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"open-input-string", 1, false, primitives.PrimOpenInputString},
        {"open-output-string", 0, false, primitives.PrimOpenOutputString},
        {"get-output-string", 1, false, primitives.PrimGetOutputString},
        {"open-input-bytevector", 1, false, primitives.PrimOpenInputBytevector},
        {"open-output-bytevector", 0, false, primitives.PrimOpenOutputBytevector},
        {"get-output-bytevector", 1, false, primitives.PrimGetOutputBytevector},
    }, registry.PhaseRuntime)

    // Port predicates
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"port?", 1, false, primitives.PrimPortQ},
        {"input-port?", 1, false, primitives.PrimInputPortQ},
        {"output-port?", 1, false, primitives.PrimOutputPortQ},
        {"textual-port?", 1, false, primitives.PrimTextualPortQ},
        {"binary-port?", 1, false, primitives.PrimBinaryPortQ},
        {"input-port-open?", 1, false, primitives.PrimInputPortOpenQ},
        {"output-port-open?", 1, false, primitives.PrimOutputPortOpenQ},
    }, registry.PhaseRuntime)

    // EOF
    r.AddPrimitives([]registry.PrimitiveSpec{
        {"eof-object", 0, false, primitives.PrimEofObject},
        {"eof-object?", 1, false, primitives.PrimEofObjectQ},
    }, registry.PhaseRuntime)

    return nil
}

func addInit(r *registry.Registry) error {
    r.AddInitFunc(initPortState)
    return nil
}

func initPortState(ctx registry.ApplyContext) error {
    env := ctx.Environment()

    // Initialize primitives state
    primitives.InitState()

    // Register port parameters
    registerParameter(env, "current-input-port", primitives.GetCurrentInputPortParam())
    registerParameter(env, "current-output-port", primitives.GetCurrentOutputPortParam())
    registerParameter(env, "current-error-port", primitives.GetCurrentErrorPortParam())

    return nil
}

func registerParameter(env *environment.TopLevelEnvironmentFrame, name string, param *machine.Parameter) {
    sym := env.InternSymbol(values.NewSymbol(name))
    env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
    env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), param)
}
```

#### 4.3 File: `go/extensions/all/register.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

// Package all provides a convenience extension that includes all standard extensions.
package all

import (
    "wile/extensions/eval"
    "wile/extensions/exceptions"
    "wile/extensions/files"
    "wile/extensions/gointerop"
    "wile/extensions/io"
    "wile/extensions/math"
    "wile/extensions/records"
    "wile/extensions/syntax"
    "wile/extensions/system"
    "wile/extensions/threads"
    "wile/registry"
)

// Extension includes all standard extensions.
var Extension = registry.NewExtension("all", AddToRegistry)

var Builder = registry.RegistryBuilder{
    io.AddToRegistry,
    files.AddToRegistry,
    system.AddToRegistry,
    math.AddToRegistry,
    exceptions.AddToRegistry,
    eval.AddToRegistry,
    syntax.AddToRegistry,
    records.AddToRegistry,
    threads.AddToRegistry,
    gointerop.AddToRegistry,
}

var AddToRegistry = Builder.AddToRegistry
```

---

### Phase 5: Create REPL Package

#### 5.1 Directory Structure

```
go/repl/
├── repl.go             # Main REPL loop
├── readline.go         # Readline integration
├── debug.go            # Debug commands
├── multiline.go        # Multi-line input handling
├── options.go          # REPL options
└── CLAUDE.md           # Documentation
```

#### 5.2 File: `go/repl/repl.go`

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

// Package repl provides an interactive Read-Eval-Print Loop for Wile.
package repl

import (
    "context"
    "fmt"
    "io"
    "os"
    "strings"

    "wile"

    "github.com/ergochat/readline"
)

// REPL is an interactive Scheme REPL.
type REPL struct {
    engine    *wile.Engine
    rl        *readline.Instance
    prompt    string
    contPrompt string
    debugCtx  *DebugContext
}

// Option configures a REPL.
type Option func(*REPL)

// WithPrompt sets the primary prompt.
func WithPrompt(p string) Option {
    return func(r *REPL) { r.prompt = p }
}

// WithContinuationPrompt sets the continuation prompt.
func WithContinuationPrompt(p string) Option {
    return func(r *REPL) { r.contPrompt = p }
}

// New creates a new REPL with the given engine.
func New(engine *wile.Engine, opts ...Option) (*REPL, error) {
    r := &REPL{
        engine:     engine,
        prompt:     "> ",
        contPrompt: "  ",
        debugCtx:   NewDebugContext(),
    }
    for _, opt := range opts {
        opt(r)
    }

    rl, err := readline.NewFromConfig(&readline.Config{
        Prompt:          r.prompt,
        InterruptPrompt: "^C",
        EOFPrompt:       "",
        HistoryFile:     getHistoryFile(),
        HistoryLimit:    1000,
    })
    if err != nil {
        return nil, err
    }
    r.rl = rl

    return r, nil
}

// Run starts the REPL loop.
func (r *REPL) Run(ctx context.Context) error {
    defer r.rl.Close()

    var inputBuffer strings.Builder

    for {
        line, err := r.rl.ReadLine()
        if err != nil {
            if err == readline.ErrInterrupt {
                inputBuffer.Reset()
                r.rl.SetPrompt(r.prompt)
                continue
            }
            if err == io.EOF {
                fmt.Println()
                return nil
            }
            return err
        }

        // Check for debug commands
        trimmed := strings.TrimSpace(line)
        if strings.HasPrefix(trimmed, ",") && inputBuffer.Len() == 0 {
            r.debugCtx.HandleCommand(trimmed)
            continue
        }

        // Accumulate input
        if inputBuffer.Len() > 0 {
            inputBuffer.WriteString("\n")
        }
        inputBuffer.WriteString(line)

        // Try to evaluate
        input := inputBuffer.String()
        result, err := r.engine.Eval(ctx, input)
        if err != nil {
            if isIncompleteInput(err) {
                r.rl.SetPrompt(r.contPrompt)
                continue
            }
            fmt.Fprintf(os.Stderr, "Exception: %v\n", err)
            inputBuffer.Reset()
            r.rl.SetPrompt(r.prompt)
            continue
        }

        inputBuffer.Reset()
        r.rl.SetPrompt(r.prompt)

        if result != nil && !result.IsVoid() {
            fmt.Println(result.SchemeString())
        }
    }
}

// Close releases REPL resources.
func (r *REPL) Close() error {
    return r.rl.Close()
}

func getHistoryFile() string {
    home, err := os.UserHomeDir()
    if err != nil {
        return ""
    }
    return home + "/.wile_history"
}

func isIncompleteInput(err error) bool {
    if err == nil {
        return false
    }
    errStr := err.Error()
    return strings.Contains(errStr, "unexpected EOF") ||
        strings.Contains(errStr, "unterminated") ||
        strings.Contains(errStr, "unclosed")
}
```

---

### Phase 6: Update cmd/main.go

```go
// Copyright 2025 Aaron Alpar
// Licensed under the Apache License, Version 2.0

package main

import (
    "context"
    "fmt"
    "os"

    "wile"
    "wile/extensions/all"
    "wile/machine"
    "wile/repl"

    "github.com/jessevdk/go-flags"
)

type Options struct {
    File        string `short:"f" long:"file" description:"Scheme file to run"`
    LibraryPath string `short:"L" long:"library-path" description:"Library search path"`
    Minimal     bool   `long:"minimal" description:"Use minimal runtime (core only)"`
    Version     bool   `short:"v" long:"version" description:"Print version"`
}

var opts Options

func main() {
    parser := flags.NewParser(&opts, flags.Default)
    args, err := parser.Parse()
    if err != nil {
        os.Exit(1)
    }

    if opts.Version {
        fmt.Printf("Wile Scheme %s\n", BuildVersion)
        os.Exit(0)
    }

    if opts.File == "" && len(args) > 0 {
        opts.File = args[0]
    }

    // Create engine with appropriate extensions
    var engine *wile.Engine
    if opts.Minimal {
        engine, err = wile.NewEngine()
    } else {
        engine, err = wile.NewEngine(
            wile.WithExtension(all.Extension),
        )
    }
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }

    // Set up library loader
    machine.LibraryEnvFactory = func(ctx context.Context) (*environment.EnvironmentFrame, error) {
        e, err := wile.NewEngine(wile.WithExtension(all.Extension))
        if err != nil {
            return nil, err
        }
        return e.Environment().EnvironmentFrame(), nil
    }

    ctx := context.Background()

    if opts.File != "" {
        runFile(ctx, engine, opts.File)
        return
    }

    // Interactive REPL
    r, err := repl.New(engine)
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }
    r.Run(ctx)
}

func runFile(ctx context.Context, engine *wile.Engine, filename string) {
    content, err := os.ReadFile(filename)
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }

    _, err = engine.EvalMultiple(ctx, string(content))
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }
}
```

---

## Files Summary

### New Files

| Path | Lines (est.) | Purpose |
|------|--------------|---------|
| `go/registry/registry.go` | 120 | Registry type |
| `go/registry/builder.go` | 40 | RegistryBuilder |
| `go/registry/phase.go` | 35 | Phase constants |
| `go/registry/register.go` | 80 | Apply functions |
| `go/registry/extension.go` | 25 | Extension interface |
| `go/registry/core/register.go` | 30 | Core extension |
| `go/registry/core/predicates.go` | 50 | Type predicates |
| `go/registry/core/equality.go` | 20 | eq?, eqv?, equal? |
| `go/registry/core/lists.go` | 60 | List operations |
| `go/registry/core/pairs.go` | 80 | CxR accessors |
| `go/registry/core/arithmetic.go` | 50 | Basic arithmetic |
| `go/registry/core/control.go` | 30 | apply, call/cc |
| `go/registry/core/vectors.go` | 30 | Vector ops |
| `go/registry/core/strings.go` | 40 | String ops |
| `go/registry/core/characters.go` | 20 | Char ops |
| `go/registry/core/bytevectors.go` | 25 | Bytevector ops |
| `go/registry/core/syntax.go` | 25 | Syntax primitives |
| `go/registry/core/parameters.go` | 15 | Parameters |
| `go/registry/core/specialforms.go` | 25 | Compile-time bindings |
| `go/registry/core/bootstrap.go` | 150 | Bootstrap macros |
| `go/wile/engine.go` | 200 | Engine type |
| `go/wile/options.go` | 40 | Options |
| `go/wile/value.go` | 100 | Value wrapper |
| `go/wile/compiled.go` | 20 | CompiledCode |
| `go/wile/primitive.go` | 20 | PrimitiveSpec |
| `go/wile/error.go` | 30 | Error types |
| `go/extensions/io/register.go` | 100 | I/O extension |
| `go/extensions/files/register.go` | 60 | Files extension |
| `go/extensions/system/register.go` | 50 | System extension |
| `go/extensions/math/register.go` | 50 | Math extension |
| `go/extensions/threads/register.go` | 80 | Threads extension |
| `go/extensions/gointerop/register.go` | 100 | Go interop |
| `go/extensions/records/register.go` | 40 | Records |
| `go/extensions/exceptions/register.go` | 40 | Exceptions |
| `go/extensions/eval/register.go` | 30 | Eval |
| `go/extensions/syntax/register.go` | 30 | Syntax |
| `go/extensions/all/register.go` | 30 | All extensions |
| `go/repl/repl.go` | 150 | REPL |
| `go/repl/debug.go` | 100 | Debug commands |

**Total new files**: ~40 files, ~2200 lines

### Modified Files

| Path | Changes |
|------|---------|
| `go/runtime/environment_tiny.go` | Remove to ~50 lines (just re-exports) |
| `go/cmd/main.go` | Rewrite to use wile.Engine + repl |

### Unchanged Files

- All `go/runtime/primitives/*.go` (implementation files)
- All core packages: `machine/`, `parser/`, `environment/`, `values/`, `syntax/`, etc.

---

## Verification Checklist

- [ ] `go build ./...` succeeds
- [ ] `go test ./...` passes
- [ ] `make lint` passes
- [ ] `wile.NewEngine()` creates working minimal engine
- [ ] `wile.NewEngine(wile.WithExtension(all.Extension))` creates full engine
- [ ] REPL works: `go run ./cmd`
- [ ] File execution works: `go run ./cmd -f test.scm`
- [ ] Minimal mode works: `go run ./cmd --minimal`
- [ ] Custom primitives can be registered
- [ ] Third-party extension compiles and works
- [ ] Bootstrap macros (and, or, let, cond) work
- [ ] No circular import errors

---

## Usage Examples

### Minimal Embedding

```go
engine, _ := wile.NewEngine()
result, _ := engine.Eval(ctx, "(+ 1 2 3)")
fmt.Println(result) // 6
```

### With I/O

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
```

### Custom REPL

```go
engine, _ := wile.NewEngine(wile.WithExtension(all.Extension))
r, _ := repl.New(engine, repl.WithPrompt("scheme> "))
r.Run(ctx)
```
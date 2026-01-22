# CLAUDE.md

Package `wile` provides the public API for embedding the Wile Scheme interpreter.

## Purpose

- Clean embedding interface for Go applications
- Manages environment creation and initialization
- Provides Eval/Compile/Run workflow
- Supports custom primitives and extensions

## Key Types

| Type | Purpose |
|------|---------|
| `Engine` | Main entry point, manages environment and registry |
| `Value` | Public interface for Scheme values |
| `CompiledCode` | Pre-compiled code ready for execution |
| `PrimitiveSpec` | Defines a primitive (re-export from registry) |
| `EngineOption` | Functional options for Engine configuration |

## Key Files

| File | Purpose |
|------|---------|
| `engine.go` | Engine type and main API |
| `options.go` | EngineOption configuration |
| `value.go` | Value interface and constructors |
| `compiled.go` | CompiledCode type |
| `primitive.go` | PrimitiveSpec re-export |
| `error.go` | Error types |
| `doc.go` | Package documentation |

## Usage

```go
// Minimal embedding
engine, _ := wile.NewEngine()
result, _ := engine.Eval(ctx, "(+ 1 2 3)")
fmt.Println(result.SchemeString()) // "6"

// With extensions
engine, _ := wile.NewEngine(
    wile.WithExtension(io.Extension),
)

// Custom primitives
engine.RegisterPrimitive(wile.PrimitiveSpec{
    Name:       "my-func",
    ParamCount: 1,
    Impl:       myFunc,
})

// Define values
engine.Define("pi", wile.NewFloat(3.14159))

// Get values
val, ok := engine.Get("result")

// Call procedures
proc, _ := engine.Get("my-proc")
result, _ := engine.Call(ctx, proc, wile.NewInteger(42))
```

## Engine Methods

| Method | Purpose |
|--------|---------|
| `NewEngine(opts...)` | Create new engine with options |
| `Eval(ctx, code)` | Parse, compile, and execute code |
| `EvalMultiple(ctx, code)` | Evaluate multiple expressions |
| `Compile(code)` | Compile without executing |
| `Run(ctx, compiled)` | Execute compiled code |
| `Define(name, value)` | Bind value to name |
| `Get(name)` | Retrieve value by name |
| `RegisterPrimitive(spec)` | Add Go function as primitive |
| `Call(ctx, proc, args...)` | Invoke Scheme procedure |
| `Environment()` | Get underlying environment |

## Value Constructors

| Constructor | Creates |
|-------------|---------|
| `NewInteger(n)` | Exact integer |
| `NewFloat(f)` | Inexact real |
| `NewString(s)` | String |
| `NewSymbol(s)` | Symbol |
| `NewBoolean(b)` | Boolean |
| `NewList(vals...)` | List |
| `Null` | Empty list |
| `Void` | Void value |
| `True` / `False` | Boolean constants |

## Gotchas

- **Core always included**: Unless WithRegistry is used, core primitives are automatic
- **Extensions added after core**: Extension primitives registered after core
- **Bootstrap macros**: Loaded after all primitives are registered
- **Value wrapping**: Values are wrapped; use internal() for raw access
- **Environment access**: Use Environment() for advanced operations

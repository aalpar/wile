# Wile

A R7RS Scheme interpreter/compiler in Go with hygienic macros.

The name is a play on "scheme" (as in "wiles" - cunning stratagems) and a nod to Wile E. Coyote, the cartoon schemer.

## Overview

Wile compiles Scheme source code to bytecode and executes it on a stack-based virtual machine. It implements R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model.

## Background

Wile was originally a Lisp interpreter (compiler and VM) used for scripting block-based storage systems (databases, pipelines, search, etc.). It's been recently expanded to the Scheme R7RS standard in the hopes that it will be of use to someone who wants to use Lisp/Scheme in Go.

### Why Another Scheme Implementation?

The world isn't really in need of another Scheme implementation - there are plenty out there. The primary use of this implementation is for embedding into Go. Go was seen as a good candidate for embedding Scheme because it's already got garbage collection (saving the need to implement garbage collection for Scheme), and Go's use in many server-side applications - such as web-servers and database servers.

### Use of AI

Anthropic's Claude Code was used to help document, fill out the primitive library, and diagnose bugs. The `CLAUDE.md` file is committed to help others get started.

## Features

- **Bytecode compilation** - Scheme code compiles to an efficient bytecode representation
- **Stack-based VM** - Execution uses a stack machine with proper tail-call optimization
- **Hygienic macros** - `syntax-rules` with the "sets of scopes" model (Flatt 2016)
- **First-class syntax objects** - Source location and scope information preserved through compilation
- **Derived expressions as macros** - `let`, `cond`, `and`, `or` defined using `define-syntax`

## Build

```bash
# Build everything
make

# Run tests
make test
```

## Usage

```bash
# Start REPL
./dist/scheme

# Run a Scheme file
./dist/scheme --file example.scm
./dist/scheme -f example.scm
./dist/scheme example.scm

# Print version
./dist/scheme --version
```

## Example

```scheme
;; Define a macro
(define-syntax let1
  (syntax-rules ()
    ((let1 ((name val) ...) body)
     ((lambda (name ...) body) val ...))))

;; Use the macro
(let1 ((x 1) (y 2))
  (+ x y))
;; => 3
```

## Architecture

```
Source → Tokenizer → Parser → Expander → Compiler → VM
```

1. **Tokenizer** - Lexical analysis
2. **Parser** - Builds syntax tree with source information
3. **Expander** - Macro expansion using `syntax-rules` transformers
4. **Compiler** - Generates bytecode operations
5. **VM** - Executes bytecode with stack-based evaluation

### Key Components

| Package | Purpose |
|---------|---------|
| `machine/` | Virtual machine, compiler, macro expander |
| `environment/` | Variable binding and scope management |
| `values/` | Scheme value types (numbers, pairs, etc.) |
| `registry/` | Extension registration and primitives |
| `internal/syntax/` | First-class syntax objects with hygiene |
| `internal/match/` | Pattern matching engine for macros |
| `internal/parser/` | Scheme parser |
| `internal/tokenizer/` | Lexer |

## Hygiene Model

Wile uses the "sets of scopes" approach from Flatt's 2016 paper. Each identifier carries a set of scopes, and variable resolution checks that the binding's scopes are a subset of the use site's scopes:

```
bindingScopes ⊆ useScopes
```

This prevents unintended variable capture in macros:

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))    ; tmp gets macro's scope
       (set! x y)
       (set! y tmp)))))

(let ((tmp 5) (a 1) (b 2))  ; this tmp has different scope
  (swap! a b)
  tmp)  ; => 5, not captured by macro's tmp
```

## Embedding in Go

Wile provides a public API for embedding Scheme in Go programs via the `wile` package.

### Basic Usage

```go
import "github.com/aalpar/wile"

// Create an engine
engine, err := wile.NewEngine()
if err != nil {
    log.Fatal(err)
}

// Evaluate a single expression
result, err := engine.Eval(ctx, "(+ 1 2 3)")
fmt.Println(result.SchemeString()) // "6"

// Evaluate multiple expressions (returns last result)
result, err = engine.EvalMultiple(ctx, `
  (define x 10)
  (define y 20)
  (+ x y)
`)
```

### Compile Once, Run Many Times

```go
compiled, err := engine.Compile("(+ x 1)")
result, err := engine.Run(ctx, compiled)
```

### Bridging Go and Scheme

Define Go values in Scheme's environment:

```go
engine.Define("my-var", wile.NewInteger(100))
val, ok := engine.Get("my-var")
```

Register a Go function as a Scheme primitive:

```go
import "github.com/aalpar/wile/values"

engine.RegisterPrimitive(wile.PrimitiveSpec{
    Name:       "go-add",
    ParamCount: 2,
    Impl: func(ctx context.Context, mc *wile.MachineContext) error {
        a := mc.Arg(0).(*values.Integer).Value
        b := mc.Arg(1).(*values.Integer).Value
        mc.SetValue(values.NewInteger(a + b))
        return nil
    },
})
// Now callable from Scheme: (go-add 3 4) => 7
```

Call a Scheme procedure from Go:

```go
proc, _ := engine.Get("my-scheme-function")
result, err := engine.Call(ctx, proc, wile.NewInteger(42))
```

### Value Constructors

| Constructor | Creates |
|---|---|
| `wile.NewInteger(n)` | Exact integer |
| `wile.NewFloat(f)` | Inexact real |
| `wile.NewString(s)` | String |
| `wile.NewSymbol(s)` | Symbol |
| `wile.NewBoolean(b)` | `#t` / `#f` |
| `wile.NewList(vals...)` | Proper list |
| `wile.Null` | Empty list `'()` |
| `wile.Void` | Void value |

## Documentation

- `PRIMITIVES.md` - Complete reference of supported types and primitives
- `docs/design/DESIGN.md` - Detailed macro system design
- `docs/design/EMBEDDING.md` - Embedding API design
- `BIBLIOGRAPHY.md` - Academic references
- `TODO.md` - Implementation status and pending tasks

## References

- [Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/) - Flatt (2016)
- [R7RS Scheme](https://small.r7rs.org/) - Language specification

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.


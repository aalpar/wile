# Wile

[![CI](https://github.com/aalpar/wile/actions/workflows/ci.yml/badge.svg)](https://github.com/aalpar/wile/actions/workflows/ci.yml)

A R7RS Scheme interpreter/compiler in Go with hygienic macros.

The name is a play on "scheme" (as in "wiles" - cunning stratagems) and a nod to Wile E. Coyote, the cartoon schemer.

## Overview

Wile compiles Scheme source code to bytecode and executes it on a stack-based virtual machine. It implements R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model (Flatt 2016).

Wile is designed as a Scheme scripting layer that feels native to Go. It provides what Go intentionally lacks -- hygienic macros, first-class continuations, symbolic computation -- without requiring CGo, a C toolchain, or cross-compilation headaches. Add it with `go get` and it just works.

## Background

Wile was originally a Lisp interpreter (compiler and VM) used for scripting block-based storage systems (databases, pipelines, search, etc.). It's been recently expanded to the Scheme R7RS standard in the hopes that it will be of use to someone who wants to use Lisp/Scheme in Go.

### Why Another Scheme Implementation?

Existing Scheme-in-Go implementations are typically toys or subsets. Embedding a production Scheme like Chibi-Scheme or S7 requires CGo, which means slow builds, broken cross-compilation, and platform-specific toolchain pain. Wile is pure Go: Scheme values are Go heap objects collected by Go's GC, so there's no custom allocator to maintain and the GC improves for free with each Go release.

### Use of AI

Anthropic's Claude Code was used to help document, fill out the primitive library, and diagnose bugs. The `CLAUDE.md` file is committed to help others get started.

## Features

- **R7RS-small compliance** - Standard libraries, numeric tower, continuations, tail calls
- **Bytecode compilation** - Scheme code compiles to an efficient bytecode representation
- **Stack-based VM** - Execution uses a stack machine with proper tail-call optimization
- **Hygienic macros** - `syntax-rules` with the "sets of scopes" model (Flatt 2016)
- **First-class continuations** - `call/cc` and `dynamic-wind` with delimited continuation support
- **Full numeric tower** - Integers, rationals, floats, complex numbers with exact/inexact distinction
- **Arbitrary precision** - `BigInteger` with automatic overflow promotion
- **Library system** - `define-library`, `import`, `export` with configurable search paths
- **Pure Go** - No CGo, no C dependencies, works with `go get`
- **Go embedding API** - Clean API for evaluating Scheme from Go and registering Go functions as primitives

## Installation

Requires Go 1.23 or later.

### As a library

```bash
go get github.com/aalpar/wile@latest
```

### As a standalone interpreter

Download a prebuilt binary from [Releases](https://github.com/aalpar/wile/releases), or build from source:

```bash
git clone https://github.com/aalpar/wile.git
cd wile
make build
```

The binary is built to `./dist/{os}/{arch}/scheme`.

## Usage

```bash
# Start REPL
./dist/darwin/arm64/scheme

# Run a Scheme file
./dist/darwin/arm64/scheme --file example.scm
./dist/darwin/arm64/scheme -f example.scm
./dist/darwin/arm64/scheme example.scm

# With library search path
./dist/darwin/arm64/scheme -L /path/to/libs example.scm

# Print version
./dist/darwin/arm64/scheme --version
```

Replace `darwin/arm64` with your platform (e.g., `linux/amd64`).

The `SCHEME_LIBRARY_PATH` environment variable provides additional library search paths (colon-separated).

## Example

```scheme
;; Hygienic macros
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))

(let ((a 1) (b 2))
  (swap! a b)
  (list a b))
;; => (2 1)

;; First-class continuations
(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x) (exit x)))
              '(54 0 37 -3 245 19))
    #t))
;; => -3
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

### Engine Options

| Option | Description |
|---|---|
| `wile.WithRegistry(r)` | Use a custom registry instead of the default core primitives |
| `wile.WithExtension(ext)` | Add a single extension |
| `wile.WithExtensions(exts...)` | Add multiple extensions |

## R7RS Standard Libraries

The following R7RS libraries are available via `(import ...)`:

| Library | Description |
|---|---|
| `(scheme base)` | Core language: arithmetic, pairs, lists, strings, vectors, control |
| `(scheme case-lambda)` | `case-lambda` form |
| `(scheme char)` | Character predicates and case conversion |
| `(scheme complex)` | Complex number operations |
| `(scheme cxr)` | Compositions of `car` and `cdr` |
| `(scheme eval)` | `eval` and `environment` |
| `(scheme file)` | File I/O (`open-input-file`, `file-exists?`, etc.) |
| `(scheme inexact)` | Inexact math (`sin`, `cos`, `exp`, `log`, `sqrt`, etc.) |
| `(scheme lazy)` | Promises (`delay`, `force`, `make-promise`) |
| `(scheme load)` | `load` |
| `(scheme read)` | `read` |
| `(scheme write)` | `write`, `display` |
| `(scheme repl)` | `interaction-environment` |
| `(scheme process-context)` | `command-line`, `exit`, `get-environment-variable` |
| `(scheme time)` | `current-second`, `current-jiffy`, `jiffies-per-second` |
| `(scheme r5rs)` | R5RS compatibility |

## Architecture

```
Source → Tokenizer → Parser → Expander → Compiler → VM
```

1. **Tokenizer** - Lexical analysis with comprehensive R7RS token support
2. **Parser** - Builds syntax tree with source location tracking
3. **Expander** - Macro expansion using `syntax-rules` transformers with scope sets
4. **Compiler** - Generates bytecode operations
5. **VM** - Executes bytecode with stack-based evaluation

### Key Components

| Package | Purpose |
|---------|---------|
| `.` (root) | Public embedding API (`wile` package) |
| `machine/` | Virtual machine, compiler, macro expander |
| `values/` | Scheme value types (numbers, pairs, ports, etc.) |
| `environment/` | Variable binding, scope chains, phase hierarchy |
| `registry/` | Extension registration and primitives |
| `internal/syntax/` | First-class syntax objects with scope sets for hygiene |
| `internal/match/` | Pattern matching engine for `syntax-rules` macros |
| `internal/parser/` | Scheme parser with source location tracking |
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

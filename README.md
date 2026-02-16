# Wile

[![CI](https://github.com/aalpar/wile/actions/workflows/ci.yml/badge.svg)](https://github.com/aalpar/wile/actions/workflows/ci.yml)
[![Go Reference](https://pkg.go.dev/badge/github.com/aalpar/wile.svg)](https://pkg.go.dev/github.com/aalpar/wile)

**R7RS Scheme in pure Go. No CGo, no C toolchain, no cross-compilation pain.**

Full hygienic macros, first-class continuations, and numeric tower. `go get` and it just works.

### Embed Scheme in 4 lines of Go

```go
engine, _ := wile.NewEngine(ctx)
engine.Define("width", wile.NewInteger(800))
engine.Define("height", wile.NewInteger(600))
result, _ := engine.Eval(ctx, "(* width height)")  // => 480000
```

## Why Wile?

**Embedding a Lisp in Go has always required tradeoffs:**

| Approach | Problem |
|----------|---------|
| Chibi-Scheme, S7 via CGo | Slow builds, broken cross-compilation, platform toolchain pain |
| Lisp subsets (Zygomys, etc.) | No R7RS compliance, limited ecosystem |
| Lua via go-lua | Not a Lisp, no macros, different semantics |
| JavaScript via goja | Heavy runtime, no hygiene, async complexity |

**Wile solves this:** Full R7RS Scheme in pure Go. Scheme values are Go heap objects, collected by Go's GC. No custom allocator, no FFI tax, no surprises.

| Feature | Wile | Chibi/S7 (CGo) | Goja (JS) | Starlark | Lua |
|---------|------|----------------|-----------|----------|-----|
| Pure Go | ✓ | ✗ | ✓ | ✓ | ✗ |
| Hygienic macros | ✓ | ✓ | ✗ | ✗ | ✗ |
| R7RS compliance | ✓ | ✓ | N/A | N/A | N/A |
| First-class continuations | ✓ | ✓ | ✗ | ✗ | ✗ |
| Cross-compilation | ✓ | ✗ | ✓ | ✓ | ✗ |
| Go GC integration | ✓ | ✗ | ✓ | ✓ | ✗ |

## Embedding in Go

Wile provides a public API for embedding Scheme in Go programs via the `wile` package.

### Basic Usage

```go
import "github.com/aalpar/wile"

// Create an engine
engine, err := wile.NewEngine(ctx)
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
compiled, err := engine.Compile(ctx, "(+ x 1)")
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
| `wile.NewBigInteger(n)` | Exact arbitrary-precision integer (`*big.Int`) |
| `wile.NewFloat(f)` | Inexact real |
| `wile.NewBigFloat(f)` | Inexact arbitrary-precision float (`*big.Float`) |
| `wile.NewRational(num, den)` | Exact rational |
| `wile.NewComplex(v)` | Complex number (`complex128`) |
| `wile.NewString(s)` | String |
| `wile.NewSymbol(s)` | Symbol |
| `wile.NewBoolean(b)` | `#t` / `#f` |
| `wile.True` / `wile.False` | Boolean constants |
| `wile.NewVector(vals...)` | Vector |
| `wile.NewList(vals...)` | Proper list |
| `wile.Null` | Empty list `'()` |
| `wile.Void` | Void value |

Additional constructors (e.g., `NewRationalFromBigInt`, `NewComplexFromParts`) are available in the `values` package.

### Engine Options

| Option | Description |
|---|---|
| `wile.WithRegistry(r)` | Use a custom registry instead of the default core primitives |
| `wile.WithExtension(ext)` | Add a single extension |
| `wile.WithExtensions(exts...)` | Add multiple extensions |

## Quick Start

```bash
# Install as a command-line tool
go install github.com/aalpar/wile/cmd/scheme@latest

# Or download a prebuilt binary from releases
# https://github.com/aalpar/wile/releases

# Run the REPL
scheme

# Try an example
scheme --file examples/basics/hello.scm

# See all examples
ls examples/
```

**Explore**:
- [**72 Examples**](examples/) — Basics, macros, concurrency, numeric tower, and more
- [**Gabriel Benchmarks**](examples/benchmarks/) — 21 Scheme benchmarks for performance testing
- [**Schelog**](examples/logic/schelog/) — Full Prolog-style logic programming embedded in Scheme
- [**Embedding Guide**](examples/embedding/) — How to use Wile from Go

## Key Features in Action

**Logic Programming** — Full Prolog embedded in Scheme

```scheme
(load "examples/logic/schelog/schelog.scm")

(%rel (append xs ys zs)
  ((append () ?ys ?ys))
  ((append (?x . ?xs) ?ys (?x . ?zs))
   (append ?xs ?ys ?zs)))

(%which (zs)
  (append '(1 2) '(3 4) zs))
;; ⇒ ((zs 1 2 3 4))
```

*See [examples/logic/schelog/](examples/logic/schelog/) for a self-contained Prolog implementation in Scheme.*

**Numeric Tower** — Exact rationals, complex numbers, and arbitrary precision

```scheme
(/ 1 3)              ; ⇒ 1/3 (exact rational, not 0.333...)
(* 1/3 3)            ; ⇒ 1 (exact)
(make-rectangular 0 1) ; ⇒ 0+1i (exact complex)
(expt 2 1000)        ; ⇒ 10715086071862673209484250490...
```

**Hygienic Macros** — Build DSLs without variable capture

```scheme
(load "examples/macros/state-machine.scm")

(define-state-machine traffic-light
  (states: red yellow green)
  (initial: red)
  (transitions:
   (red -> green)
   (green -> yellow)
   (yellow -> red)))
```

**Go-Native Concurrency** — Goroutines and channels from Scheme

```scheme
(let ((ch (make-channel)))
  (thread-start!
   (make-thread
    (lambda () (channel-send ch 42))))
  (channel-receive ch))  ; ⇒ 42
```

**First-Class Continuations** — Non-local control flow

```scheme
;; Early return
(call/cc (lambda (return)
  (for-each (lambda (x)
              (if (negative? x)
                  (return x)))
            '(1 2 -3 4))
  'not-found))  ; ⇒ -3

;; See examples/control/ for generators, coroutines, and backtracking
```

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
scheme

# Run a Scheme file
scheme example.scm
scheme --file example.scm
scheme -f example.scm

# With library search path
scheme -L /path/to/libs example.scm

# Enter REPL after loading file
scheme -f example.scm -i

# Print version
scheme --version
```

The `SCHEME_LIBRARY_PATH` environment variable provides additional library search paths (colon-separated).

### REPL Debugger

The REPL includes an integrated debugger. Commands start with `,`:

| Command | Description |
|---------|-------------|
| `,break FILE:LINE` | Set breakpoint |
| `,delete ID` | Delete breakpoint |
| `,list` | List breakpoints |
| `,step` | Step into next expression |
| `,next` | Step over (same frame) |
| `,finish` | Step out (return from function) |
| `,continue` | Continue execution |
| `,backtrace` | Show stack trace |
| `,where` | Show current source location |

## R7RS Standard Libraries

| Library | Description |
|---|---|
| `(scheme base)` | Core language: arithmetic, pairs, lists, strings, vectors, control |
| `(scheme case-lambda)` | `case-lambda` form |
| `(scheme char)` | Character predicates and case conversion |
| `(scheme complex)` | Complex number operations |
| `(scheme cxr)` | Compositions of `car` and `cdr` |
| `(scheme eval)` | `eval` and `environment` |
| `(scheme file)` | File I/O |
| `(scheme inexact)` | Transcendental functions (`sin`, `cos`, `exp`, `log`, `sqrt`, etc.) |
| `(scheme lazy)` | Promises (`delay`, `force`, `make-promise`) |
| `(scheme load)` | `load` |
| `(scheme read)` | `read` |
| `(scheme write)` | `write`, `display` |
| `(scheme repl)` | `interaction-environment` |
| `(scheme process-context)` | `command-line`, `exit`, `get-environment-variable` |
| `(scheme time)` | `current-second`, `current-jiffy`, `jiffies-per-second` |
| `(scheme r5rs)` | R5RS compatibility |

### Additional Libraries

| Library | Description |
|---|---|
| `(srfi 18)` | Multithreading (threads, mutexes, condition variables) |
| `(chibi test)` | Minimal test framework (for R7RS test compatibility) |

## Architecture

```
Source → Tokenizer → Parser → Expander → Compiler → VM
```

1. **Tokenizer** — Lexical analysis with comprehensive R7RS token support
2. **Parser** — Builds syntax tree with source location tracking
3. **Expander** — Macro expansion using `syntax-rules`/`syntax-case` with scope sets
4. **Compiler** — Generates bytecode operations
5. **VM** — Executes bytecode with stack-based evaluation

### Package Structure

| Package | Purpose |
|---------|---------|
| `wile` (root) | Public embedding API |
| `machine/` | Virtual machine, compiler, macro expander |
| `values/` | Scheme value types (numbers, pairs, ports, threads, etc.) |
| `environment/` | Variable binding, scope chains, phase hierarchy |
| `registry/` | Extension registration and primitives |
| `registry/core/` | Essential primitives and bootstrap macros |
| `registry/helpers/` | Shared utilities for primitive implementations |
| `runtime/` | Compile/Run API for embedding |
| `internal/syntax/` | First-class syntax objects with scope sets |
| `internal/match/` | Pattern matching engine for macros |
| `internal/parser/` | Scheme parser |
| `internal/tokenizer/` | Lexer |
| `internal/validate/` | Syntax validation |
| `internal/forms/` | Compiled form definitions |
| `internal/schemeutil/` | Scheme utility functions |
| `internal/repl/` | Interactive REPL with debugger |
| `internal/bootstrap/` | Environment initialization |
| `internal/extensions/` | Extension packages (io, files, math, threads, etc.) |

### API Stability

The following packages form the public API and follow [Go module versioning](https://go.dev/doc/modules/version-numbers):

- **`wile` (root)** — `Engine`, `RegisterFunc`, `Eval`/`Compile`/`Run`, error types
- **`values`** — Scheme value types, `Value` interface, numeric tower
- **`registry`** — `Registry`, `Extension`, `PrimitiveSpec`, phase constants

All other packages (`machine/`, `environment/`, `internal/`) are implementation details and may change without notice. The `machine` package is technically importable but is not covered by compatibility guarantees.

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

## Types

### Numeric Tower

| Type | Description | Example |
|------|-------------|---------|
| Integer | Exact 64-bit signed | `42`, `-17` |
| BigInteger | Exact arbitrary precision | `#z12345678901234567890` |
| Rational | Exact fraction | `3/4`, `-1/2` |
| Float | Inexact IEEE 754 double | `3.14`, `1e10` |
| BigFloat | Inexact arbitrary precision | `#m3.14159265358979323846` |
| Complex | Complex number | `1+2i`, `3@1.57` (polar) |

### Concurrency Types

| Type | Description |
|------|-------------|
| Thread | SRFI-18 thread |
| Mutex | SRFI-18 mutex |
| Condition Variable | SRFI-18 condition variable |
| Channel | Go channel wrapper |
| WaitGroup | Go sync.WaitGroup wrapper |
| RWMutex | Go sync.RWMutex wrapper |
| Atomic | Thread-safe mutable value |

## Documentation

| Document | Description |
|----------|-------------|
| `PRIMITIVES.md` | Complete reference of types and primitives |
| `docs/design/DESIGN.md` | Macro system design |
| `docs/design/EMBEDDING.md` | Embedding API design |
| `docs/design/DELIMITED_CONTINUATIONS.md` | Delimited continuation implementation |
| `docs/dev/NUMERIC_TOWER.md` | Numeric tower architecture |
| `docs/dev/ENVIRONMENT_SYSTEM.md` | Environment system architecture |
| `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` | Documented differences from R7RS |
| `BIBLIOGRAPHY.md` | Academic references |
| `CHANGELOG.md` | Release history |

## References

- [Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/) — Flatt (2016)
- [R7RS Scheme](https://small.r7rs.org/) — Language specification
- [SRFI-18](https://srfi.schemers.org/srfi-18/) — Multithreading

## Contributing

Wile welcomes contributions! We're actively looking for help with:

- **Documentation** — Examples, guides, tutorials
- **Standard library** — R7RS-small features, SRFI implementations
- **Test coverage** — Improving test coverage across packages
- **Performance** — Allocation reduction, targeted optimizations
- **Tooling** — REPL improvements, debugging tools, IDE integration

**Get started:**
- Browse [issues labeled `good-first-issue`](https://github.com/aalpar/wile/labels/good-first-issue)
- Check [help wanted](https://github.com/aalpar/wile/labels/help-wanted) for high-priority items
- Read [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines and workflow

## License

This project is licensed under the Apache License 2.0 — see the [LICENSE](LICENSE) file for details.

# wile

A Scheme interpreter/compiler in Go with hygienic macros.

## Overview

wile compiles Scheme source code to bytecode and executes it on a stack-based virtual machine. It implements R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model.

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

# Build from go directory
cd go && make build

# Run tests
cd go && make test
```

## Usage

```bash
# Start REPL
./go/cmd/scheme

# Run a Scheme file
./go/cmd/scheme -file example.scm
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
| `syntax/` | First-class syntax objects with hygiene |
| `values/` | Scheme value types (numbers, pairs, etc.) |
| `match/` | Pattern matching engine for macros |
| `parser/` | Scheme parser |
| `tokenizer/` | Lexer |

## Hygiene Model

wile uses the "sets of scopes" approach from Flatt's 2016 paper. Each identifier carries a set of scopes, and variable resolution checks that the binding's scopes are a subset of the use site's scopes:

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

- `CLAUDE.md` - Development guide and architecture overview
- `go/DESIGN.md` - Detailed macro system design
- `BIBLIOGRAPHY.md` - Academic references
- `go/TODO.md` - Implementation status and pending tasks

## References

- [Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/) - Flatt (2016)
- [R7RS Scheme](https://small.r7rs.org/) - Language specification

## License

MIT
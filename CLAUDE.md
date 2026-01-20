# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Wile is a Scheme interpreter/compiler in Go with hygienic macros. It compiles Scheme to bytecode and executes it on a stack-based virtual machine, implementing R7RS-style `syntax-rules` macros with a "sets of scopes" hygiene model (Flatt 2016).

## Build Commands

All commands run from the `go/` directory:

```bash
make build      # Build the scheme binary to ../dist/scheme
make test       # Run all tests (go test -v ./...)
make lint       # Run golangci-lint
make fix        # Run golangci-lint with --fix
make cover      # Run tests with coverage
make format     # Format code with golangci-lint
make tidy       # Tidy go.mod
```

Run a single test:
```bash
cd go && go test -v -run TestName ./package/...
```

Run the REPL:
```bash
./dist/scheme
```

Run a Scheme file:
```bash
./dist/scheme -file example.scm
```

## Architecture

```
Source → Tokenizer → Parser → Expander → Compiler → VM
```

| Package | Purpose |
|---------|---------|
| `machine/` | VM, compiler, macro expander (largest package) |
| `values/` | Scheme value types (69 types: numbers, pairs, ports, etc.) |
| `syntax/` | First-class syntax objects with scope sets for hygiene |
| `environment/` | Variable binding, scope chains, phase hierarchy |
| `match/` | Pattern matching engine for `syntax-rules` macros |
| `parser/` | Scheme parser with source location tracking |
| `tokenizer/` | Lexer with comprehensive R7RS token support |
| `validate/` | Form validation before compilation |
| `define_syntax/` | Macro definition handling |
| `runtime/` | Top-level environment with standard library bindings |
| `forms/` | Form specifications (lambda, if, define, etc.) |
| `cmd/` | REPL and file execution entry point |

**Key entry point**: `go/cmd/main.go` - REPL with readline support, file execution, debug commands.

**Standard library**: `lib/scheme/` contains R7RS library definitions (base.sld, r5rs.sld, etc.).

**Per-package docs**: Each Go package has its own `CLAUDE.md` with detailed type/function documentation and gotchas.

## Coding Conventions

See `CODING_STYLE.md` for the complete style guide. Key points:

**Receivers**: Always single-letter - `p` for standard, `c` for compiler-related types.

**Return variable**: Use `q` for values that will be returned.

**Constructors**: Use `New*` prefix, intermediate variable `q`:
```go
func NewInteger(v int64) *Integer {
    q := &Integer{Value: v}
    return q
}
```

**No if-assignments**: Never combine assignment and comparison:
```go
// Correct
err := doSomething()
if err != nil { ... }

// Avoid
if err := doSomething(); err != nil { ... }
```

**Errors**: Use `values.NewForeignError()`, `values.WrapForeignErrorf()`, or static errors from values package.

**Tests**: Table-driven with `quicktest` and `SchemeEquals` checker. Pattern: `Test{Type}_{Method}`.

**Imports**: Internal packages first, then standard library.

## References

- `TODO.md` - Pending tasks, missing R7RS features, future extensions (multithreading, POSIX API, Go FFI)
- `CODING_STYLE.md` - Comprehensive style guide
- `PRIMITIVES.md` - Complete primitives reference
- `go/DESIGN.md` - Macro system design details
- `BIBLIOGRAPHY.md` - Academic references (Flatt 2016, R7RS)
- `plans/TESTING_PLAN.md` - Comprehensive primitive unit test implementation plan
- `plans/R7RS_SEMANTIC_DIFFERENCES.md` - Documented differences between implementation and R7RS specification
- `plans/R7RS_CONFORMANCE_PLAN.md` - R7RS conformance roadmap

## R7RS Conformance

This project aims to implement R7RS-small. Key resources:

| Source | URL |
|--------|-----|
| R7RS-small PDF | https://small.r7rs.org/attachment/r7rs.pdf |
| R7RS Corrected (HTML) | https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html |
| R7RS-large Wiki (in progress) | https://codeberg.org/scheme/r7rs/wiki |

**Testing policy**: Tests that conform to R7RS must not be removed or reverted. If a test fails but correctly reflects R7RS behavior, the implementation must be fixed—not the test. See `go/runtime/primitives/CLAUDE.md` for detailed type requirements.

### R7RS Specification Comments

Functions implementing R7RS-specified behavior must include comments citing the relevant specification section. This ensures traceability and helps maintain conformance.

**Format**: Use `R7RS §X.Y.Z` notation in doc comments.

**Example**:
```go
// Add returns the sum of this integer and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
func (p *Integer) Add(o Number) Number {
```

**When to include R7RS citations**:
- Type definitions for Scheme value types (Integer, Pair, etc.)
- Arithmetic and comparison operations
- Type predicates and conversions (exact, inexact, integer?, etc.)
- Primitive procedure implementations
- Exactness preservation/contagion behavior
- Any behavior specified by R7RS sections 4-6

**Key R7RS sections**:
| Section | Topic |
|---------|-------|
| §4.1-4.3 | Expressions, syntax |
| §5.1-5.5 | Program structure, definitions |
| §6.1 | Equivalence predicates |
| §6.2 | Numbers (tower, exactness, operations) |
| §6.3 | Booleans, pairs, lists, symbols, characters, strings, vectors |
| §6.4 | Bytevectors |
| §6.5 | Control features |
| §6.6 | Exceptions |
| §6.7-6.13 | Environments, I/O, system interface |

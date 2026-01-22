# CLAUDE.md

Package `core` provides the core primitives required for Scheme to function.

## Purpose

- Registers ~100 essential primitives always included in any Wile engine
- Provides compile-time bindings for special forms (if, lambda, define, etc.)
- Contains bootstrap macro definitions (and, or, let, cond, etc.)
- Contains primitive implementations in `prim_*.go` files

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Main entry point, Builder and Extension exports |
| `specialforms.go` | Compile-time bindings (if, lambda, quote, define, etc.) |
| `predicates.go` | Type predicate registrations |
| `equality.go` | eq?, eqv?, equal?, not registrations |
| `pairs.go` | cons, car, cdr, CxR accessor registrations |
| `lists.go` | list, append, reverse, memq, assq, etc. registrations |
| `arithmetic.go` | +, -, *, /, comparisons, abs, min, max registrations |
| `control.go` | apply, call/cc, dynamic-wind, values registrations |
| `vectors.go` | Vector operation registrations |
| `strings.go` | String operation registrations |
| `characters.go` | Character operation registrations |
| `bytevectors.go` | Bytevector operation registrations |
| `syntax.go` | Syntax operation registrations |
| `parameters.go` | make-parameter, parameter? registrations |
| `bootstrap.go` | Bootstrap macro source code |
| `prim_*.go` | Primitive implementations (one per category) |

## Usage

```go
import "wile/registry/core"

// Use the Extension
engine, _ := wile.NewEngine()  // Core is included by default

// Or use the Builder directly
reg := registry.NewRegistry()
core.AddToRegistry(reg)
```

## Primitive Categories

| Category | Count | Phase |
|----------|-------|-------|
| Type Predicates | 24 | Runtime + Expand |
| Boolean | 1 | Runtime + Expand |
| Equality | 3 | Runtime + Expand |
| Pairs/CxR | 33 | Runtime + Expand |
| Lists | 14 | Runtime + Expand |
| Arithmetic | 18 | Runtime + Expand |
| Control | 8 | Runtime |
| Vectors | 7 | Runtime + Expand |
| Strings | 15 | Runtime + Expand |
| Characters | 7 | Runtime + Expand |
| Bytevectors | 10 | Runtime + Expand |
| Syntax | 6 | Runtime + Expand |
| Parameters | 2 | Runtime |

## Testing

This package contains the comprehensive test suite for all Scheme primitives (~90 test files). Tests cover:
- Core primitives (arithmetic, lists, vectors, strings, etc.)
- Extension primitives (I/O, files, math, eval, exceptions, threads, channels)
- R7RS compliance, edge cases, and error conditions

Test files use `package core_test` and test helpers in `test_helpers_test.go`.

Key test files:
| Test File | Tests For |
|-----------|-----------|
| `prim_arithmetic_test.go` | `+`, `-`, `*`, `/` and related |
| `prim_list_test.go` | List/pair operations |
| `prim_string_test.go` | String operations |
| `prim_io_test.go` | read, write, display, ports |
| `prim_exception_test.go` | Exception handling |
| `prim_thread_test.go` | SRFI-18 threading |
| `prim_channel_test.go` | Go channels |

## Gotchas

- **Expand-time subset**: Many primitives available at expand-time for macro fenders
- **Bootstrap order**: Macros depend on primitives being registered first
- **No I/O**: Core does not include any I/O primitives (use io extension)
- **No exceptions**: Exception handling in extensions/exceptions

# CLAUDE.md

Package `core` provides the core primitives required for Scheme to function.

## Purpose

- Registers ~85 essential primitives always included in any Wile engine
- Provides compile-time bindings for special forms (if, lambda, define, etc.)
- Contains bootstrap macro definitions (and, or, let, cond, etc.)

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Main entry point, Builder and Extension exports |
| `specialforms.go` | Compile-time bindings (if, lambda, quote, define, etc.) |
| `predicates.go` | Type predicates (null?, pair?, number?, etc.) |
| `equality.go` | eq?, eqv?, equal?, not |
| `pairs.go` | cons, car, cdr, CxR accessors |
| `lists.go` | list, append, reverse, memq, assq, etc. |
| `arithmetic.go` | +, -, *, /, comparisons, abs, min, max |
| `control.go` | apply, call/cc, dynamic-wind, values |
| `vectors.go` | make-vector, vector-ref, vector-set!, etc. |
| `strings.go` | string operations and conversions |
| `characters.go` | char->integer, integer->char, comparisons |
| `bytevectors.go` | Bytevector operations, UTF-8 conversion |
| `syntax.go` | identifier?, syntax->datum, datum->syntax |
| `parameters.go` | make-parameter, parameter? |
| `bootstrap.go` | Bootstrap macro source code |

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
| Bytevectors | 9 | Runtime + Expand |
| Syntax | 6 | Runtime + Expand |
| Parameters | 2 | Runtime |

## Gotchas

- **Expand-time subset**: Many primitives available at expand-time for macro fenders
- **Bootstrap order**: Macros depend on primitives being registered first
- **No I/O**: Core does not include any I/O primitives (use io extension)
- **No exceptions**: Exception handling in extensions/exceptions

# CLAUDE.md

Package `runtime` initializes the top-level Scheme environment with primitives.

## Purpose

- Creates and bootstraps the complete top-level environment
- Registers 200+ Scheme built-in functions as foreign closures
- Loads bootstrap macros (and, or, let, let*, letrec, cond, when, unless, etc.)
- Manages I/O port parameters (stdin, stdout, stderr)
- Maintains three-phase environment: TopLevel → Expand → Compile

## Key Files

| File | Purpose |
|------|---------|
| `environment_tiny.go` | Main initialization, primitive registration, bootstrap macros |
| `primitives/` | 240+ primitive implementations |
| `primitives/state.go` | Global I/O port state, tokenizer/parser caches |

## Initialization Flow

1. `NewTopLevelEnvironmentFrameTiny()` creates environment
2. `registerCompileTimePrimitives()` - if, lambda, quote, define, etc.
3. `registerRuntimePrimitives()` - 200+ executable primitives
4. `registerExpandTimePrimitives()` - Subset for macro expansion
5. `loadBootstrapMacros()` - Parse, expand, compile, execute macro definitions
6. `registerPortParameters()` - Bind port parameter objects

## Primitive Categories

- Arithmetic, numeric predicates, comparisons
- List/pair operations, CxR accessors
- String/character operations
- Vectors, bytevectors, hashtables
- I/O (read, write, ports)
- Higher-order (apply, map, for-each)
- Continuations, exceptions, multiple values
- SRFI-18 threads, mutexes, condition variables
- Go channels, WaitGroup, RWMutex, Once, Atomic

## Gotchas

- **Variadic convention**: Rest args passed as Pair in last parameter
- **Three-phase hierarchy**: TopLevel/Expand/Compile with different primitive subsets
- **Port parameters special**: Created as Parameter objects, not regular values
- **Bootstrap order matters**: Macros loaded after all primitives registered
- **Weak references**: Tokenizer/parser caches use weak pointers
- **Expand-time subset**: Only safe primitives available during macro expansion

## Testing

Uses quicktest with `evalScheme()` helper for full parse→expand→compile→execute cycle.

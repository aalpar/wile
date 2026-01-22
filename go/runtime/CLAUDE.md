# CLAUDE.md

Package `runtime` initializes the top-level Scheme environment using the registry pattern.

## Purpose

- Creates and bootstraps the complete top-level environment
- Uses the registry pattern to compose core primitives and extensions
- Loads bootstrap macros (and, or, let, let*, letrec, cond, when, unless, guard, etc.)
- Maintains three-phase environment: TopLevel → Expand → Compile

## Key Files

| File | Purpose |
|------|---------|
| `environment_tiny.go` | Main initialization using registry pattern |

## Initialization Flow

`NewTopLevelEnvironmentFrameTiny()` uses the registry pattern:

1. Creates a `registry.Registry` with core primitives (`core.AddToRegistry`)
2. Adds all extensions (io, files, math, eval, exceptions, threads, gointerop, all, system)
3. Creates new top-level environment frame
4. Applies registry to environment (`reg.Apply`)
5. Registers syntax compilers in compile environment
6. Registers primitive expanders in expand environment
7. Loads bootstrap macros from registry's `MacroSources()`

## Extensions

| Extension | Purpose |
|-----------|---------|
| `io` | Port I/O (read, write, display, ports) |
| `files` | File I/O (open-input-file, open-output-file, etc.) |
| `math` | Transcendental functions (sin, cos, exp, log, etc.) |
| `eval` | eval, load, environments |
| `exceptions` | with-exception-handler, raise, error |
| `threads` | SRFI-18 threads, mutexes, condition variables |
| `gointerop` | Go channels, WaitGroup, RWMutex, Once, Atomic |
| `all` | Records, promises, extra string/char operations |
| `system` | System interface (command-line, features, etc.) |

## Primitive Categories

Core primitives (from `registry/core/`):
- Arithmetic, numeric predicates, comparisons
- List/pair operations, CxR accessors
- String/character operations
- Vectors, bytevectors
- Higher-order (apply, map, for-each)
- Continuations, multiple values
- Parameters (make-parameter)
- Syntax operations

## Gotchas

- **Registry pattern**: Uses `registry.Registry` to compose primitives, same as `wile.Engine`
- **Three-phase hierarchy**: TopLevel/Expand/Compile with different primitive subsets
- **Bootstrap order matters**: Macros loaded after all primitives registered
- **Variadic convention**: Rest args passed as Pair in last parameter

## Primitive Registration

Primitives are registered via the registry pattern in extension packages:

```go
r.AddPrimitives([]registry.PrimitiveSpec{
    {"char=?", 2, true, PrimCharEqVariadic},
}, registry.PhaseRuntime|registry.PhaseExpand)
```

| ParamCount | IsVariadic | Behavior |
|------------|------------|----------|
| 0 | true | All args as Pair in `mc.Arg(0)` |
| 1 | true | First arg direct, rest as Pair in `mc.Arg(0)` |
| 2 | true | First arg in `mc.Arg(0)`, rest as Pair in `mc.Arg(1)` |
| N | false | Exactly N args, each in `mc.Arg(0)` through `mc.Arg(N-1)` |

## Testing

Comprehensive primitive tests are located in `registry/core/`. Uses quicktest with helpers for full parse→expand→compile→execute cycle.

## References

See `BIBLIOGRAPHY.md` at project root for R7RS specification, SRFI-18 (threading), and other standards implemented by this package.

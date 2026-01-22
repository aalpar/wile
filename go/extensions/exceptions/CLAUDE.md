# CLAUDE.md

Package `exceptions` provides exception handling primitives.

## Purpose

- Exception raising and handling
- Error object creation and inspection

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration |
| `prim_exceptions.go` | Exception primitive implementations |

## Primitives (Runtime only)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `with-exception-handler` | 2 | Install handler and run thunk |
| `raise` | 1 | Raise non-continuable exception |
| `raise-continuable` | 1 | Raise continuable exception |
| `error` | 2+ | Create and raise error object |
| `error-object?` | 1 | Check if value is an error object |
| `error-object-message` | 1 | Get error message |
| `error-object-irritants` | 1 | Get error irritants list |

## Usage

```go
import "wile/extensions/exceptions"

// Use with registry
reg := registry.NewRegistry()
exceptions.AddToRegistry(reg)
```

## Gotchas

- **Continuable exceptions**: Handler return value becomes raise-continuable result
- **Non-continuable**: raise aborts if handler returns normally
- **guard macro**: Bootstrap macro in core uses these primitives

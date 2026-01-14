# CLAUDE.md

Package `primitives` implements all R7RS Scheme built-in procedures.

## Purpose

224+ primitive implementations as Go foreign functions, organized by:
- One file per primitive: `prim_<name>.go`
- Helper modules for shared patterns
- Comprehensive test coverage

## Key Files

| File | Purpose |
|------|---------|
| `state.go` | Global I/O port state, weak-referenced caches |
| `numeric_fold.go` | Variadic arithmetic helpers (+, -, *, /) |
| `numeric_compare.go` | Comparison chain helpers (=, <, >, <=, >=) |
| `char_compare.go` | Character comparison predicates |
| `call_with_file.go` | File I/O wrapper with cleanup |
| `eqv.go` | Equality semantics for memv/assv |

## Primitive Implementation Pattern

```go
func PrimXxx(ctx context.Context, mc *machine.MachineContext) error {
    arg := mc.Arg(0)
    typed, ok := arg.(*values.SomeType)
    if !ok {
        return values.WrapForeignErrorf(values.ErrSomeError, "...")
    }
    // ... work ...
    mc.SetValue(result)
    return nil
}
```

## Registration

Primitives listed in `runtime/environment_tiny.go`:
```go
var runtimePrimitives = []PrimitiveSpec{
    {Name: "+", ParamCount: 0, IsVariadic: true, Impl: primitives.PrimAdd},
    // ...
}
```

## Gotchas

- **Variadic args as Pair**: Rest arguments passed as linked list in last parameter
- **Identity elements**: `(+)` returns 0, `(*)` returns 1
- **Append complexity**: Uses vector intermediate for O(n) despite linked lists
- **Continuation escape**: `call/cc` copies continuation, uses sentinel error
- **Arguments via mc.Arg()**: Not via environment frame bindings
- **Weak caching**: Tokenizers/parsers cached per port with weak pointers

## Testing

Uses quicktest with `runProgram()` and `runSchemeCode()` helpers. Table-driven tests cover operations, edge cases, and error conditions.

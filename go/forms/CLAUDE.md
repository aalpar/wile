# CLAUDE.md

Package `forms` provides a unified registry for special form handlers.

## Purpose

Central dispatch table mapping keyword names (if, define, lambda, etc.) to validation and compilation functions. Decouples the validate and machine packages.

## Key Types

```go
type ValidatorFunc func(ctx context.Context, pair any, result any) any
type CompilerFunc func(ctc any, ctctx any, expr any) error

type FormSpec struct {
    Name     string
    Validate ValidatorFunc  // Optional
    Compile  CompilerFunc   // Optional
}
```

## Registration Functions

- `Register(spec)` - Complete replacement of existing entry
- `RegisterValidator(name, fn)` - Partial: updates/creates Validate field
- `RegisterCompiler(name, fn)` - Partial: updates/creates Compile field
- `Lookup(name)` - Runtime lookup by form name
- `Names()` - List all registered form names

## Usage Flow

1. **Validation**: `validate` package calls `forms.Lookup(name).Validate()`
2. **Compilation**: `machine` package calls `forms.Lookup(name).Compile()`

## Gotchas

- **Type erasure via `any`**: No compile-time type safety; runtime assertions in wrappers
- **Partial registration**: Can register validator without compiler (and vice versa)
- **Two-tier dispatch**: Form-name lookup first, then type-based fallback
- **Global namespace**: Single registry, registration in `init()` functions
- **No unregistration**: Form names live forever once registered

## Testing

No direct unit tests (too thin). Tested implicitly through validate and machine package tests.

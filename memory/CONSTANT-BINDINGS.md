# Constant Bindings: Immutability Identification and Enforcement

## Goal

Identify bindings whose values are known at compile time and cannot be mutated,
and enforce immutability of imported bindings per R7RS 5.2. This lays the
foundation for a follow-up constant folding pass that inlines known values as
template literals.

## Non-Goals

- Constant folding / inlining (follow-up work)
- REPL exemption for imported binding mutation (not implementing)
- User-declared `define-constant` syntax (future)
- `identifier-syntax` support (future)

## Background

R7RS 5.2 states: "it is an error to import the same identifier more than once
with different bindings, or to redefine or mutate an imported binding with a
definition or with set!."

Wile currently has no mechanism to track whether a binding came from an import.
Imported bindings become indistinguishable from locally-defined ones after
`CopyLibraryBindingsToEnvAtPhase` copies the value into a fresh binding.

The existing `Mutable` flag on `ValidatedLetBinding` tracks whether a let-bound
variable is targeted by `set!`, but this is a compile-time analysis on local
bindings only -- it does not apply to global or imported bindings.

## Design

### Data Model

Two new fields on `BindingMeta` (`environment/binding.go`):

```go
type BindingMeta struct {
    Scopes   []*syntax.Scope
    Source   *syntax.SourceContext
    Doc      string
    Imported bool  // binding came from a library import
    Constant bool  // value is known at compile time
}
```

**`Imported`** -- set when the binding is installed by `CopyLibraryBindingsToEnvAtPhase`.
Permanent property of the binding. The compiler rejects `set!` on these bindings.

**`Constant`** -- set when the binding is both immutable and its value is statically
known. For imported bindings, this is true when the library binding has a resolved
value at import time (the common case -- the library has already been evaluated).
A follow-up can use this flag to drive constant folding and inlining.

The two flags are separate because `Imported` is about provenance and enforcement,
while `Constant` is about optimization eligibility. An imported binding is always
`Imported` but only `Constant` if its value is statically known. A local `define`
with a literal value and no `set!` could be `Constant` without being `Imported`.

Accessors follow the existing `SetDoc`/`Doc` pattern, lazily allocating `BindingMeta`:

- `IsImported() bool`
- `SetImported(bool)`
- `IsConstant() bool`
- `SetConstant(bool)`

### Error Infrastructure

One new sentinel in `werr/werr.go`:

```go
var ErrImmutableBinding = NewStaticError("cannot mutate immutable binding")
```

Named `ErrImmutableBinding` rather than `ErrImportedBinding` so the same sentinel
can serve future immutability sources (user-declared constants, sealed bindings).

### Enforcement Site

`CompileValidatedSetBang` (`machine/compilation/compile_validated.go`), after
resolving the binding:

```go
binding := p.env.GetBinding(sym, symbolScopes)
if binding == nil { ... }

if binding.IsImported() {
    return werr.WrapForeignErrorf(
        werr.ErrImmutableBinding,
        "set!: cannot mutate imported binding %q",
        sym.Key,
    )
}
```

Compile-time error, unconditional (no REPL exemption).

### Import Site

`CopyLibraryBindingsToEnvAtPhase` (`machine/compilation/library_bindings.go`),
after creating the binding and setting its value:

```go
binding.SetImported(true)
if binding.Value() != nil {
    binding.SetConstant(true)
}
```

### Shadowing

Local `define` that shadows an import creates a fresh binding via
`MaybeCreateOwnGlobalBinding`. The new binding has no `BindingMeta` (or a fresh
one without `Imported`), so `set!` on the shadow works correctly. No special
handling needed -- the existing scope-aware resolution finds the right binding.

## Files Changed

| File | Change |
|------|--------|
| `environment/binding.go` | Add `Imported`, `Constant` to `BindingMeta`; add accessors |
| `werr/werr.go` | Add `ErrImmutableBinding` sentinel |
| `machine/compilation/compile_validated.go` | Check `IsImported()` in `CompileValidatedSetBang` |
| `machine/compilation/library_bindings.go` | Set `Imported` and `Constant` in `CopyLibraryBindingsToEnvAtPhase` |
| `environment/binding_test.go` | Test new accessors |
| `machine/compilation/compile_validated_test.go` or new test file | Integration tests |

## Test Plan

| Case | Input | Expected |
|------|-------|----------|
| Reject set! on import | `(import (scheme base)) (set! cons 42)` | Compile-time error: `ErrImmutableBinding` |
| Allow set! on local | `(define x 1) (set! x 2)` | Success |
| Shadow then set! | `(import (scheme base)) (define cons 42) (set! cons 99)` | Success (fresh binding) |
| Constant flag set | Import a binding, inspect `IsConstant()` | `true` for resolved values |
| Constant flag unset on local | `(define x 1)`, inspect `IsConstant()` | `false` (no analysis yet for top-level defines) |

## Future Work

- **Constant folding**: use `Constant` flag + `binding.Value()` to replace
  `OpLoadGlobal` with `OpPushLiteral` in the compiler
- **Top-level define analysis**: extend `Constant` to cover `(define x <literal>)`
  bindings that are never `set!`'d (analogous to existing `Mutable` analysis for
  let-bindings, but for globals)
- **User-declared constants**: `define-constant` via `identifier-syntax` or a
  dedicated form
- **Cross-module inlining**: use `Constant` values from imported libraries to
  inline across module boundaries

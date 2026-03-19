# Reflection Primitives Design

**Status:** Complete — all 5 primitives implemented

## Summary

Five runtime primitives for inspecting procedure metadata. All operate on
`values.Callable` and return plain Scheme data — no new types.

## Primitives

### procedure-arity

`(procedure-arity proc) → integer | (min . #f) | list`

Returns the arity of a procedure:
- Fixed arity: integer (e.g., `2`)
- Variadic: `(min . #f)` pair (e.g., `(1 . #f)` for `(lambda (x . rest) ...)`)
- case-lambda: list of the above (e.g., `(1 (2 . #f))`)

| Type | Result |
|------|--------|
| MachineClosure (fixed) | `template.ParameterCount()` |
| MachineClosure (variadic) | `(template.ParameterCount()-1 . #f)` |
| ForeignClosure (fixed) | `paramCount` |
| ForeignClosure (variadic) | `(paramCount-1 . #f)` |
| CaseLambdaClosure | list of clause arities |
| Parameter | `(0 . #f)` |
| ComposableContinuation | `1` |

### procedure-name

`(procedure-name proc) → string | #f`

Returns the name of a procedure, or `#f` if anonymous.

| Type | Result |
|------|--------|
| MachineClosure | `template.Name()` or `#f` if empty |
| ForeignClosure | `name` field (new) or `#f` |
| CaseLambdaClosure | first clause's template name or `#f` |
| Parameter | `#f` |
| ComposableContinuation | `#f` |

**Requires**: Adding a `name string` field to `ForeignClosure`, populated from
`PrimitiveSpec.Name` during registry application.

### procedure-source-location

`(procedure-source-location proc) → (file line column) | #f`

Returns source location as a 3-element list, or `#f` if unavailable.

| Type | Result |
|------|--------|
| MachineClosure | from template's source table (first non-nil entry) |
| ForeignClosure | `#f` |
| CaseLambdaClosure | first clause's source location |
| Parameter | `#f` |
| ComposableContinuation | `#f` |

### procedure-bound-symbols

`(procedure-bound-symbols proc) → list | #f`

Returns the list of symbols bound in the closure's captured environment.
Only meaningful for closures with environments.

| Type | Result |
|------|--------|
| MachineClosure | symbol list from `env.LocalEnvironment().Keys()` |
| ForeignClosure | `#f` |
| CaseLambdaClosure | first clause's bound symbols |
| Parameter | `#f` |
| ComposableContinuation | `#f` |

### procedure-type

`(procedure-type proc) → symbol`

Returns a symbol classifying the procedure type.

| Type | Result |
|------|--------|
| MachineClosure | `lambda` |
| ForeignClosure | `foreign` |
| CaseLambdaClosure | `case-lambda` |
| Parameter | `parameter` |
| ComposableContinuation | `continuation` |

## Implementation

All five primitives live in `registry/core/`. Each is a 1-argument
runtime-only primitive registered via `addReflection`.

### Files

| File | Contents |
|------|----------|
| `reflection.go` | `addReflection` registration function |
| `prim_reflection.go` | `PrimProcedureArity`, `PrimProcedureName`, `PrimProcedureSourceLocation`, `PrimProcedureBoundSymbols`, `PrimProcedureType` |
| `prim_reflection_test.go` | Table-driven tests for all five primitives |

### Infrastructure Change

Add `name string` field to `machine.ForeignClosure` and a `Name() string`
accessor. Update `NewForeignClosure` and `NewVMForeignClosure` to accept
an optional name parameter, or add `SetName(string)`. The registry's
`Apply()` method already has `PrimitiveSpec.Name` available — thread it
through during closure creation.

### Registration

Add `addReflection` to the `Builder` in `register.go`, after `addControl`.
All five primitives registered at `PhaseRuntime` only (reflection at
expand-time is not useful).

## Error Handling

All five primitives raise an error if the argument is not a procedure
(`values.Callable`). Use `werr.ErrNotAProcedure` (or add if missing)
wrapped with `WrapForeignErrorf`.

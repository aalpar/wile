# Error Diagnostics Design

**Status:** All layers complete
**Date:** 2026-04-16

> **Completed:** Layer 1 (ErrorContext via continuation marks), Layer 2 (NativeError enrichment with source/stackTrace).
> **Incomplete:** Layer 3 — compiler error source location wrapping for Phases 2-4 (compile_syntax_rules.go, Phase 3 import/library files, Phase 4 expander files: ~100+ unwrapped sites).
**Depends on:** `2026-04-14-error-stack-traces-design.md` (Phase 1 complete)

## Problem

Runtime error context (source location, stack trace, continuation marks) is captured
by `ErrExceptionEscape` at every `raise` site but never reaches Scheme code. Exception
handlers receive only `e.Condition` -- the diagnostic context is stripped by the dispatch
mechanism. Compiler errors (Phases 2-4) still lack source locations.

## Design

Two-layer approach: continuation marks as transport (Layer 1), `NativeError` enrichment
as persistence (Layer 2), plus mechanical compiler error migration (Layer 3).

### Layer 1: ErrorContext via Continuation Mark

#### ErrorContext type

New opaque Scheme value carrying diagnostic context from a raise site:

```go
// machine/error_context.go
type ErrorContext struct {
    source     *syntax.SourceContext
    stackTrace StackTrace
    marks      *ContinuationMarkSet
}
```

Implements `values.Value`. Not constructible from Scheme -- only created by the
exception dispatch mechanism.

#### Mark key

A private `ContinuationMarkKey` (`errorContextKey`) created at package init.
Not accessible by name from user code -- only through `current-error-context`.

#### Raise integration

When `ErrExceptionEscape` reaches the exception handler dispatch (in
`OperationForeignFunctionCall` or equivalent), before calling the Scheme handler:

1. Build `ErrorContext` from `ErrExceptionEscape` fields (source, stackTrace)
   plus `CollectContinuationMarks(DefaultPromptTag)` for the marks snapshot
2. `mc.SetMark(errorContextKey, errorContext)`
3. Call handler(condition)

The mark lives on the handler's frame. Nested raises shadow correctly --
`current-error-context` always returns the innermost.

#### Scheme primitives

| Primitive | Returns | Notes |
|-----------|---------|-------|
| `(error-context? obj)` | boolean | Type predicate |
| `(error-context-source ctx)` | string or `#f` | `"file:line:col"` |
| `(error-context-stack-trace ctx)` | list of alists | `((name . "f") (file . "foo.scm") (line . 10) (column . 5))` |
| `(error-context-continuation-marks ctx)` | continuation-mark-set | Marks snapshot from raise site |
| `(current-error-context)` | error-context or `#f` | Reads `%error-context` from immediate frame |

Stack trace returned as structured data, not formatted string.

### Layer 2: NativeError Enrichment

At exception dispatch time, if `e.Condition` is a `*NativeError`, copy source and
stack trace from `ErrExceptionEscape` into it:

```go
// In exception dispatch, before calling handler:
if ne, ok := e.Condition.(*values.NativeError); ok {
    ne.SetSource(e.Source)
    ne.SetStackTrace(e.StackTrace)
}
```

New fields on `NativeError`:

```go
type NativeError struct {
    message    *String
    irritants  Value
    kind       NativeErrorKind
    err        error
    source     *syntax.SourceContext  // nil until raised
    stackTrace StackTrace             // nil until raised
}
```

New Scheme accessors:

| Primitive | Returns | Notes |
|-----------|---------|-------|
| `(error-object-source e)` | string or `#f` | `#f` if never raised |
| `(error-object-stack-trace e)` | list of alists or `()` | Empty if never raised |

Enrichment is idempotent -- re-raising overwrites with current context.

### Layer 3: Compiler Error Source Locations

Finish Phases 2-4 from `2026-04-14-error-stack-traces-design.md`:

| Phase | Scope | Sites |
|-------|-------|-------|
| 2 | Syntax/macro compilation | ~60 |
| 3 | Library/import | ~90 |
| 4 | Expander | ~70 |

Mechanical: wrap each `WrapForeignErrorf` call with `p.wrapCompilationError()`.
Independent of Layers 1-2 (compiler errors are Go-side `*CompilationError`, not
Scheme exceptions).

## Implementation Order

1. `ErrorContext` type (machine/error_context.go)
2. `errorContextKey` mark key (machine/error_context.go)
3. Exception dispatch sets mark before calling handler
4. `current-error-context` + accessor primitives
5. `NativeError` source/stackTrace fields + setters
6. Exception dispatch enriches NativeError
7. `error-object-source` / `error-object-stack-trace` primitives
8. Compiler error Phases 2-4

Steps 1-4 are Layer 1. Steps 5-7 are Layer 2. Step 8 is Layer 3.

## Out of Scope

| Item | Reason |
|------|--------|
| Cross-boundary stack traces | High complexity, independent follow-on |
| Debugger VM integration | Separate workstream (see `DEBUGGER.md`) |
| Local variable inspection | Requires code-info metadata, belongs with runtime debugging |

## Testing

Layer 1:
- `raise` inside `with-exception-handler`: verify `current-error-context` returns
  non-`#f`, source and stack trace are populated
- Nested raises: verify innermost context wins
- Non-error raise (`(raise 42)`): verify context still available via mark

Layer 2:
- `guard` catches `NativeError`: verify `error-object-source` returns location string
- `error-object-source` on un-raised NativeError: verify `#f`
- Re-raise: verify context updates

Layer 3:
- Compile undefined binding: verify `CompilationError.Source` is non-empty
- One representative test per phase (syntax, library, expander)

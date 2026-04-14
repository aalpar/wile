# Error Stack Traces Design

**Status:** Design
**Date:** 2026-04-14
**TODO Item:** Tier 1 — Exceptions and error stack traces

## Problem

Compilation errors lack source locations. When the compiler encounters an undefined
binding, type error, or syntax error, the error message contains the *what* ("no such
local or global binding 'foo'") but not the *where* (file:line:col). The compiler
tracks source context via `pushSource`/`popSource`, but this info isn't propagated
into errors.

Runtime errors already have good source+trace support via `ErrExceptionEscape` →
`RuntimeError`.

## Scope

Three sub-problems, ordered by dependency:

### P1: Compiler errors with source context

**What:** The 12 `WrapForeignErrorf` sites in `compile_time_continuation.go` (and the
~332 sites across 43 other files in `compilation/`) don't include the source location
from `currentSource()`.

**Approach:** Add a `wrapCompileError` method to `CompileTimeContinuation` that wraps
errors with the current source context. Define a `SourcedError` type in `compilation/`
that carries `*syntax.SourceContext` alongside the error.

```go
// compilation/sourced_error.go
type SourcedError struct {
    Source *syntax.SourceContext
    Err    error
}

func (p *SourcedError) Error() string { ... }
func (p *SourcedError) Unwrap() error { return p.Err }
```

```go
// On CompileTimeContinuation:
func (p *CompileTimeContinuation) wrapCompileError(err error) error {
    src := p.currentSource()
    if src == nil {
        return err
    }
    return &SourcedError{Source: src, Err: err}
}
```

**Migration:** Start with `compile_time_continuation.go` (12 sites), then
`compile_validated.go` (5), `compile_let.go` (4), `compile_closure.go` (2).
The expander files have their own error paths and can be done in a follow-up.

**Phasing:**
- Phase 1: Core compiler files (compile_time_continuation.go, compile_validated.go,
  compile_let.go, compile_closure.go) — ~23 sites
- Phase 2: Syntax/macro compilation (compile_syntax_rules.go, compile_syntax_case.go,
  compile_define_syntax.go, etc.) — ~60 sites
- Phase 3: Library/import (compile_library_forms.go, import_set_datum.go,
  library_loader.go, etc.) — ~90 sites
- Phase 4: Expander (expander_*.go) — ~70 sites

Phase 1 covers the errors embedders see most often (undefined bindings, arity
mismatches, type errors in core forms).

### P2: CompilationError.Source field

**What:** Add `Source string` to `CompilationError` in the public API. Populate it
from `SourcedError` in the cause chain.

**Approach:**
```go
// error.go
type CompilationError struct {
    Message string
    Source  string // formatted "file:line:col", empty if unavailable
    Cause   error
}
```

In `engine.go`, the compilation error wrapping sites extract source:
```go
func wrapCompilationError(msg string, cause error) *CompilationError {
    ce := &CompilationError{Message: msg, Cause: cause}
    var se *compilation.SourcedError
    if errors.As(cause, &se) && se.Source != nil {
        ce.Source = fmt.Sprintf("%s:%d:%d",
            se.Source.File,
            se.Source.Start.Line(),
            se.Source.Start.Column())
    }
    return ce
}
```

### P3: Cross-boundary stack traces (deferred)

**What:** When a Go primitive calls Scheme via sub-context, the sub-context's
stack trace starts from nil — the trace doesn't bridge back to the parent context.

**Complexity:** High — requires passing parent context info through sub-context
creation and augmenting `CaptureStackTrace` to walk `parentMC` chains.

**Recommendation:** Defer to a separate design. P1+P2 provide immediate value.

## Implementation Order

1. `SourcedError` type + `wrapCompileError` method
2. Phase 1 migration (core compiler, ~23 sites)
3. `CompilationError.Source` field + extraction
4. Tests: verify source locations appear in `CompilationError` for common errors
5. Phase 2-4 migration (follow-up PRs)

## Testing

```go
func TestCompilationErrorHasSource(t *testing.T) {
    eng, _ := wile.NewEngine(ctx)
    _, err := eng.Eval(ctx, "(undefined-var)")
    var ce *wile.CompilationError
    if errors.As(err, &ce) {
        // ce.Source should be non-empty: "<mcp-eval>:1:1" or similar
    }
}
```

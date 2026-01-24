# Plan: Fix let-syntax Hygiene for Local Bindings

## Status: COMPLETED ✓

All test cases pass. The fix was implemented on 2026-01-23.

## Problem Statement

Macros defined with `let-syntax` that reference local variables don't correctly capture those bindings:

```scheme
(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m))))
; Returns 'inner (WRONG), should return 'outer
```

The macro template `x` should refer to the `x` bound by the outer `let`, not the inner `let`.

## Root Cause

The bug was in `go/match/syntax_adapter.go` in `valueToSyntaxWithOrigin`. When creating syntax for free identifiers with local binding resolution, the code used:

```go
srcCtx.WithScopes(localScopes)
```

This **prepends** `localScopes` to `srcCtx`'s existing scopes instead of **replacing** them. This meant that a free identifier in a macro template was getting BOTH:
- Definition-site scopes (from where the macro was defined)
- Use-site scopes (from where the macro was invoked)

The use-site scopes caused the identifier to match inner bindings that it shouldn't match.

### Debugging Evidence

When tracing the lookup for the macro template's `x`:
- Reference had scopes `[A, B, C, A]` when it should have only had `[A, I]` (definition-site scopes)
- The extra scopes `[B, C]` were use-site scopes being combined incorrectly

## Solution Implemented

### The Fix

Changed `valueToSyntaxWithOrigin` to create a new `SourceContext` with ONLY the definition-site scopes, explicitly discarding any use-site scopes from `srcCtx`:

```go
if lsp, ok := resolution.(localScopesProvider); ok {
    if localScopes := lsp.GetLocalScopes(); localScopes != nil {
        // Local binding - use ONLY definition-site scopes, NOT use-site scopes
        var scopedCtx *syntax.SourceContext
        if srcCtx != nil {
            scopedCtx = &syntax.SourceContext{
                Text:   srcCtx.Text,
                File:   srcCtx.File,
                Start:  srcCtx.Start,
                End:    srcCtx.End,
                Origin: srcCtx.Origin,
                Scopes: localScopes, // Use ONLY definition-site scopes
            }
        } else {
            scopedCtx = &syntax.SourceContext{Scopes: localScopes}
        }
        sym := syntax.NewSyntaxSymbol(v.Key, scopedCtx)
        return sym
    }
}
```

### Additional Implementation: `with-binding-scope`

To support proper binding scope propagation, we also implemented the `with-binding-scope` primitive expander:

**Semantics:**
```scheme
(with-binding-scope (id ...)
  body)
```

1. Creates fresh scope S
2. Adds S to the entire body (which contains both binding sites and references)
3. Returns the scoped body (the `with-binding-scope` form disappears)

**Usage in `let` macro:**
```scheme
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body ...)
     (with-binding-scope (name ...)
       ((lambda (name ...) (begin body ...)) val ...)))
    ((let tag ((name val) ...) body ...)
     (with-binding-scope (tag name ...)
       (letrec ((tag (lambda (name ...) body ...)))
         (tag val ...))))))
```

## Files Changed

| File | Change |
|------|--------|
| `go/match/syntax_adapter.go` | Fixed `valueToSyntaxWithOrigin` to use only definition-site scopes for local bindings |
| `go/machine/primitive_expanders_registry.go` | Registered `with-binding-scope` primitive expander |
| `go/machine/expander_time_continuation.go` | Implemented `expandWithBindingScope` |
| `go/registry/core/bootstrap.go` | Updated `let` and `let-values` to use `with-binding-scope` |
| `go/machine/compile_syntax_rules.go` | Added `FreeIdResolution` type, `collectFreeIdentifiersWithEllipsis` |
| `go/environment/environment_frame.go` | Added `GetLocalIndexWithScopes`, `GetBindingWithScopes` |
| `go/machine/compile_time_continuation.go` | Updated `CompileSymbol` to use scope-aware lookup |

## Test Results

All test cases pass:

| Test | Expected | Result |
|------|----------|--------|
| Test 1: Basic let-syntax hygiene | `'outer` | ✓ PASS |
| Test 2: Nested let shadowing | `3` | ✓ PASS |
| Test 3: Global macro hygiene | `'outer-global` | ✓ PASS |
| Test 4: Bootstrap macros (parameterize) | `2` | ✓ PASS |
| Full test suite (`make test`) | All pass | ✓ PASS |

### Test Code

```scheme
;; Test 1: Basic let-syntax hygiene
(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m))))
; Expected: 'outer

;; Test 2: Nested let with same name
(let ((x 1))
  (let ((x 2))
    (let ((x 3))
      x)))
; Expected: 3 (innermost, as normal)

;; Test 3: Global macro hygiene (regression)
(define outer-x 'outer-global)
(define-syntax m-global (syntax-rules () ((m-global) outer-x)))
(let ((outer-x 'inner)) (m-global))
; Expected: 'outer-global

;; Test 4: Bootstrap macros (regression)
(let ((p (make-parameter 1)))
  (parameterize ((p 2)) (p)))
; Expected: 2
```

---

## Flatt's Hygiene Model (Background)

Per "Binding as Sets of Scopes" (Flatt 2016):

| Scope Type | When Added | Purpose |
|------------|------------|---------|
| **Intro scope** | Macro expansion | Distinguish macro-introduced identifiers |
| **Use-site scope** | Macro invocation | Track where macro was called |
| **Binding scope** | Binding forms | Distinguish nested bindings of same name |

### Resolution Algorithm

1. Each binding form (lambda, let) introduces a fresh **scope**
2. **Bindings** are tagged with scopes from their definition site
3. **References** are tagged with scopes from their reference site
4. Resolution: `bindingScopes ⊆ referenceScopes`

### How the Fix Works

For the failing test case:
```
outer let adds scope S1 → outer x binding has {S1}
inner let adds scope S2 → inner x binding has {S1, S2}
macro's x reference has {S1} from definition site

ScopesMatch({S1}, {S1}) = true → outer x matches
ScopesMatch({S1}, {S1, S2}) = false → inner x doesn't match
                                       (because {S1,S2} ⊄ {S1})
```

---

## Future Work

For future hygiene enhancements, see **`HYGIENE_DEBUGGING_DESIGN.md`** which documents a debugging-focused approach emphasizing:

- **Scope provenance tracking**: Every scope knows why it exists and where it came from
- **Debugging primitives**: `identifier-scopes`, `scope-info`, `binding-info`
- **Enhanced error messages**: Explain resolution failures with scope context

This approach prioritizes introspection over manipulation, providing tools to understand what's happening when macros misbehave rather than full Racket-style scope manipulation APIs.

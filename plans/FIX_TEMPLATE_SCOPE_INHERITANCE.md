# Fix: Template Identifiers Inheriting Input Scopes

## Problem Summary

Macro template identifiers incorrectly inherit scopes from the input form's `SourceContext`, causing hygiene violations where template code matches user bindings it shouldn't.

**Test case that fails:**
```scheme
(let ((if even?))
  (or #f (if 7)))
;; Expected: #f (template's `if` uses special form)
;; Actual: Error (template's `if` matches user's `if` binding)
```

## Root Cause

In `match/syntax_adapter.go:valueToSyntaxWithOrigin`, the `useSiteCtx` parameter is intended for source location tracking (file, line, column for error messages), but `SourceContext` also carries **Scopes**. When creating new template identifiers, the code passes `srcCtx` (which may be `useSiteCtx`) directly to `NewSyntaxSymbol`, causing template identifiers to inherit the input's scopes.

**Problematic code (lines 386-391):**
```go
// Not a free identifier - create symbol with intro scope
sym := syntax.NewSyntaxSymbol(v.Key, srcCtx)  // srcCtx has input's scopes!
if introScope != nil {
    sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
}
return sym
```

**Why this causes the bug:**
1. User's `(or #f (if 7))` inside `(let ((if even?)) ...)` has let-scope S on all identifiers
2. `or` macro expands; `useSiteCtx` = input's context which has scope S in its `Scopes` field
3. Template's `if` (from template `(if x x ...)`) is created with `srcCtx` → inherits scope S
4. Template's `if` now has `{intro-scope, let-scope}` instead of just `{intro-scope}`
5. Scope matching: `{let-scope}` ⊆ `{intro-scope, let-scope}` → incorrectly matches user's `if` binding

## Solution

Strip scopes from `srcCtx` when creating **new template identifiers**. Template identifiers should only get:
1. The explicitly-added intro scope
2. Location info for error messages (File, Start, End, Origin)

**NOT:**
- Input's scopes (would pollute template with use-site scopes)

### Scope of Changes

**Primary fix:** `match/syntax_adapter.go:valueToSyntaxWithOrigin`

The fix affects the `*values.Symbol` case (lines 310-391) for non-free identifiers. Pattern variable substitutions and free identifiers already have correct handling.

## Implementation Plan

### Step 1: Add Helper Method to SourceContext

Add `WithoutScopes()` method to `syntax/source_context.go` for clarity and reuse:

```go
// WithoutScopes returns a new SourceContext with scopes cleared.
// Used when creating template identifiers that should not inherit
// use-site scopes during macro expansion.
func (p *SourceContext) WithoutScopes() *SourceContext {
    if p == nil {
        return nil
    }
    return &SourceContext{
        Text:   p.Text,
        File:   p.File,
        Start:  p.Start,
        End:    p.End,
        Origin: p.Origin,
        // Scopes intentionally omitted
    }
}
```

### Step 2: Fix Template Symbol Creation

Modify `match/syntax_adapter.go:valueToSyntaxWithOrigin` around line 386:

**Before:**
```go
// Not a free identifier - create symbol with intro scope
sym := syntax.NewSyntaxSymbol(v.Key, srcCtx)
if introScope != nil {
    sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
}
return sym
```

**After:**
```go
// Not a free identifier - create symbol with intro scope
// Strip scopes from srcCtx: template identifiers should not inherit
// use-site scopes, only the explicitly-added intro scope (Flatt 2016)
templateCtx := srcCtx
if srcCtx != nil && len(srcCtx.Scopes) > 0 {
    templateCtx = srcCtx.WithoutScopes()
}
sym := syntax.NewSyntaxSymbol(v.Key, templateCtx)
if introScope != nil {
    sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
}
return sym
```

### Step 3: Consider Other Literal Types

Review whether other template literals (Integer, Float, String, Boolean, Character) should also have scopes stripped. Currently (lines 393-410):

```go
case *values.Integer:
    return syntax.NewSyntaxObject(v, srcCtx)
// ... similar for Float, String, Boolean, Character
```

**Analysis:** These are self-evaluating literals, not identifiers. They don't participate in scope-based binding resolution. However, for consistency and to prevent any future issues, we could strip scopes here too. **Recommendation:** Leave as-is for now since literals don't use scopes for binding resolution. If issues arise, this can be addressed separately.

### Step 4: Remove Debug Statements

Remove the debug `fmt.Printf` statements added during investigation from:
- `validate/validate.go` (lines 77-79, 106-108, 114-116, 124-126, 130-133, 154-156, 157-160, 165-167)

## Files to Modify

| File | Change |
|------|--------|
| `syntax/source_context.go` | Add `WithoutScopes()` method |
| `match/syntax_adapter.go` | Strip scopes when creating template symbols |
| `validate/validate.go` | Remove debug statements |

## Verification

### Test Cases

**1. Simple shadowing (already works):**
```scheme
(let ((if even?)) (if 7))
;; Expected: #f
```

**2. Shadowing through macro (the bug):**
```scheme
(let ((if even?)) (or #f (if 7)))
;; Expected: #f
```

**3. Full R7RS conformance test:**
```scheme
(letrec-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1))
               (if temp temp (my-or e2 ...)))))))
  (let ((x #f) (y 7) (temp 8) (let odd?) (if even?))
    (my-or x (let temp) (if y) y)))
;; Expected: 7
```

**4. Hygiene preserved (template identifiers still hygienic):**
```scheme
(let ((temp 1))
  (or #f temp))
;; Expected: 1 (user's temp, not macro's temp)
```

**5. Free identifiers still work:**
```scheme
(or #f #t)
;; Expected: #t (or's template `if` uses special form)
```

### Run Full Test Suite

```bash
cd go && make test
```

### Run Specific Hygiene Tests

```bash
cd go && go test -v -run Hygiene ./machine/...
cd go && go test -v -run Scope ./match/...
```

## Risk Assessment

**Low risk.** The change is targeted:
- Only affects creation of NEW template identifiers
- Pattern variable substitutions unchanged (they preserve original scopes)
- Free identifiers unchanged (they have special handling)
- Intro scope still added (hygiene for macro-introduced identifiers preserved)

**Potential regression areas:**
- Macros that intentionally capture user bindings (rare, non-hygienic)
- Cross-library macros (should still work via `ResolvedBinding`)

## References

- Flatt 2016: "Binding as Sets of Scopes" - Section 3.2 describes intro scopes
- R7RS §4.3: Macros
- R7RS §4.2.2: Local variable bindings shadow special forms
- `plans/SCOPE_DEBUGGING_NOTES.md` - Full debugging analysis

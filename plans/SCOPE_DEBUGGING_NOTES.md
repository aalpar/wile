# Scope Debugging Notes

This document captures findings from debugging R7RS conformance bug #1: `letrec-syntax` expansion failure where local variables don't properly shadow special forms.

## The Bug

**Test case:**
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
```

**Expected:** `7` - The `(if y)` should call `even?` (the local variable `if`), not the `if` special form.

**Actual:** Error - macro template's `if` incorrectly matches user's local binding.

## Architecture Overview

### Scope Flow for `let`

1. User writes: `(let ((if even?)) (or #f (if 7)))`

2. `let` is a **macro** defined in `registry/core/bootstrap.go`:
   ```scheme
   (define-syntax let
     (syntax-rules ()
       ((let ((name val) ...) body ...)
        (with-binding-scope (name ...)
          ((lambda (name ...) (begin body ...)) val ...)))))
   ```

3. `let` macro expands to `with-binding-scope` form

4. `with-binding-scope` is a **primitive expander** in `machine/expander_time_continuation.go:294`:
   ```go
   func (p *ExpanderTimeContinuation) expandWithBindingScope(...) {
       // Create a fresh binding scope
       bindingScope := syntax.NewScope(nil)

       // Add the scope to the entire body
       scopedBody := body.AddScope(bindingScope)

       // Continue expanding the scoped body
       return p.ExpandExpression(ectx, scopedBody)
   }
   ```

5. Key insight: **Scope is added BEFORE expansion continues**. This means:
   - The body `((lambda (if) ...) even?)` gets scope S added
   - THEN the body is expanded (which triggers nested macro calls like `or`)

### Macro Expansion Scope Handling

When a macro (like `or`) is invoked during expansion:

1. **Transform operation** (`machine/operation_syntax_rules_transform.go:65`):
   - Retrieves compiled clauses from value register
   - Gets input form (which has accumulated scopes from outer forms)
   - Creates fresh intro scope for this invocation
   - Expands template with pattern variable substitutions

2. **Intro scope addition** (`addScopeToSyntaxSkipFreeIds`):
   - Adds intro scope to ALL template identifiers EXCEPT free identifiers
   - Free identifiers (like `if`, `let`, `lambda`) don't get intro scope
   - Pattern variable substitutions preserve their original scopes

### The Problem

Debug output showed:
```
DEBUG hasLocalVariableBinding: sym=if, bindingType=1, bindingScopes=[0x14000282610], refScopes=[0x14000282790 0x14000282610]
DEBUG hasLocalVariableBinding: sym=if, scopesMatch=true
```

**Both** `if` references (user's `(if 7)` AND macro template's `(if temp temp ...)`) have the same scopes:
- `0x14000282610` = let binding scope (S)
- `0x14000282790` = macro intro scope

The macro template's `if` should NOT have scope S (the let scope), because:
- The `or` macro was defined BEFORE the `let` created scope S
- Template identifiers come from macro definition time
- Only pattern variable substitutions should carry use-site scopes

## Key Files

| File | Purpose |
|------|---------|
| `registry/core/bootstrap.go` | `let` macro definition using `with-binding-scope` |
| `machine/expander_time_continuation.go:294` | `expandWithBindingScope` implementation |
| `machine/operation_syntax_rules_transform.go:65` | Macro transform operation |
| `machine/operation_syntax_rules_transform.go:198` | `addScopeToSyntaxSkipFreeIds` |
| `syntax/scope_utils.go` | `ScopesMatch` (subset check) |
| `validate/validate.go` | `hasLocalVariableBinding` (shadowing check) |

## Key Data Structures

### Scope (`syntax/scope.go`)
```go
type Scope struct {
    ID       int64
    Origin   *OriginInfo  // For debugging: which macro created this scope
}
```

### SourceContext (`syntax/source_context.go`)
```go
type SourceContext struct {
    Text   string
    File   string
    Start  Position
    End    Position
    Scopes []*Scope      // THE SCOPE SET - key for hygiene
    Origin *OriginInfo   // Macro expansion chain
}
```

### Environment Binding (`environment/binding.go`)
```go
type Binding struct {
    bindingType BindingType  // Variable, Macro, Syntax
    scopes      []*Scope     // Scopes at binding site
    value       Value        // Runtime value
}
```

### LocalIndex (`environment/local_index.go`)
```go
type LocalIndex struct {
    frameDepth int  // How many frames up
    slotIndex  int  // Index within that frame
}
```

## Pointers to Track When Debugging

When adding debug statements, track these pointers:

1. **Scope pointers** (`*syntax.Scope`):
   - Each `NewScope()` creates a unique pointer
   - Compare pointer addresses to see if same scope object
   - Format: `fmt.Printf("scope=%p", scope)`

2. **Environment frame pointers** (`*environment.EnvironmentFrame`):
   - `env` parameter passed through expansion/validation
   - Format: `fmt.Printf("env=%p", env)`

3. **LocalIndex values**:
   - Return value from `env.GetLocalIndex(sym)`
   - Format: `fmt.Printf("li=%d:%d", li.FrameDepth(), li.SlotIndex())`

4. **Binding pointers and scopes**:
   - `binding := env.GetLocalBinding(li)`
   - Format: `fmt.Printf("binding=%p, scopes=%v", binding, binding.Scopes())`

5. **SyntaxSymbol scopes**:
   - `sym.Scopes()` returns the reference's scope set
   - Format: `fmt.Printf("refScopes=%v", sym.Scopes())`

## Debug Statement Examples

```go
// In hasLocalVariableBinding
if sym.Key == "if" {
    fmt.Printf("DEBUG hasLocalVariableBinding:\n")
    fmt.Printf("  sym=%s\n", sym.Key)
    fmt.Printf("  env=%p\n", env)
    li := env.GetLocalIndex(sym)
    fmt.Printf("  li=%v\n", li)
    if li != nil {
        binding := env.GetLocalBinding(li)
        fmt.Printf("  binding=%p\n", binding)
        fmt.Printf("  bindingType=%v\n", binding.BindingType())
        fmt.Printf("  bindingScopes=%v (len=%d)\n", binding.Scopes(), len(binding.Scopes()))
        for i, s := range binding.Scopes() {
            fmt.Printf("    bindingScope[%d]=%p\n", i, s)
        }
    }
    fmt.Printf("  refScopes=%v (len=%d)\n", scopes, len(scopes))
    for i, s := range scopes {
        fmt.Printf("    refScope[%d]=%p\n", i, s)
    }
}
```

```go
// In expandWithBindingScope
fmt.Printf("DEBUG expandWithBindingScope:\n")
fmt.Printf("  creating bindingScope=%p\n", bindingScope)
fmt.Printf("  body before AddScope: %s\n", body.SchemeString())
fmt.Printf("  body after AddScope: %s\n", scopedBody.SchemeString())
```

```go
// In OperationSyntaxRulesTransform.Apply
fmt.Printf("DEBUG SyntaxRulesTransform:\n")
fmt.Printf("  macroName=%s\n", macroName)
fmt.Printf("  introScope=%p\n", introScope)
fmt.Printf("  input scopes:\n")
// Walk input and print scope pointers for symbols
```

## Hygiene Model (Flatt 2016)

**Binding matches reference if:** `bindingScopes ⊆ referenceScopes`

This is implemented in `syntax/scope_utils.go:ScopesMatch`:
```go
func ScopesMatch(useScopes, bindingScopes []*Scope) bool {
    // bindingScopes ⊆ useScopes
    for _, bindScope := range bindingScopes {
        found := false
        for _, useScope := range useScopes {
            if bindScope == useScope {
                found = true
                break
            }
        }
        if !found {
            return false
        }
    }
    return true
}
```

## Root Cause Identified

**Location:** `match/syntax_adapter.go:265-391` in `valueToSyntaxWithOrigin`

The bug is in how template identifiers inherit scopes from `useSiteCtx`:

```go
// Lines 277-283:
var srcCtx *syntax.SourceContext
if useSiteCtx != nil {
    srcCtx = useSiteCtx  // useSiteCtx comes from the INPUT's SourceContext
} else if templateStx != nil {
    srcCtx = templateStx.SourceContext()
}

// Lines 386-391 - For NEW template identifiers:
sym := syntax.NewSyntaxSymbol(v.Key, srcCtx)  // Inherits useSiteCtx's SCOPES!
if introScope != nil {
    sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
}
return sym
```

**The problem:**
1. `useSiteCtx` is the input form's `SourceContext` (for error location tracking)
2. `SourceContext` contains both location info (File, Line, Column) AND **Scopes**
3. `NewSyntaxSymbol(key, srcCtx)` uses the ENTIRE `SourceContext`, including scopes
4. So template identifiers like `if` inherit the input's scopes (the let scope) through `useSiteCtx`

**Why this causes the bug:**
- User's `(or #f (if 7))` inside `(let ((if even?)) ...)` has let scope S on all identifiers
- When `or` macro expands, `useSiteCtx` = input's context which has scope S
- Template's `if` (from `(if x x ...)`) gets created with `useSiteCtx` → inherits scope S
- Template's `if` now has `{intro-scope, let-scope}` instead of just `{intro-scope}`
- Scope matching finds `{let-scope}` ⊆ `{intro-scope, let-scope}` → matches user's binding!

**The fix:**
When creating new template identifiers, use `useSiteCtx` for LOCATION info only, stripping its scopes. Template identifiers should only get:
1. The intro scope (added explicitly)
2. NOT the use-site's scopes

```go
// Proposed fix in valueToSyntaxWithOrigin around line 386:
// Create srcCtx WITHOUT scopes for template identifiers
var templateCtx *syntax.SourceContext
if srcCtx != nil {
    templateCtx = &syntax.SourceContext{
        Text:   srcCtx.Text,
        File:   srcCtx.File,
        Start:  srcCtx.Start,
        End:    srcCtx.End,
        Origin: srcCtx.Origin,
        // Scopes intentionally omitted - template identifiers shouldn't inherit input scopes
    }
}
sym := syntax.NewSyntaxSymbol(v.Key, templateCtx)
if introScope != nil {
    sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
}
return sym
```

## Next Steps

**Root cause found.** The fix is in `match/syntax_adapter.go:valueToSyntaxWithOrigin`.

1. **Implement the fix:**
   - Modify `valueToSyntaxWithOrigin` to strip scopes from `srcCtx` when creating new template identifiers
   - Only the intro scope should be added to template identifiers
   - Pattern variable substitutions should preserve their original scopes (already correct)
   - Free identifiers already have special handling (already correct)

2. **Test the fix:**
   ```scheme
   ;; Simple case
   (let ((if even?)) (if 7))  ; Should return #f

   ;; Macro case
   (let ((if even?)) (or #f (if 7)))  ; Should return #f

   ;; Full conformance test
   (letrec-syntax
     ((my-or (syntax-rules () ...)))
     (let ((if even?))
       (my-or x (if y) y)))  ; Should return 7
   ```

3. **Verify no regressions:**
   - Run full test suite
   - Check existing hygiene tests still pass

## Related Files Changed

During this debugging session, these files were modified:

- `validate/validate.go` - Added `hasLocalVariableBinding` function
- `validate/validate_lambda.go` - Added `createLambdaValidationEnv`
- `validate/register.go` - Updated validator signatures to include env
- `forms/form_spec.go` - Updated `ValidatorFunc` signature
- Various `validate_*.go` files - Added env parameter

## References

- Flatt 2016: "Binding as Sets of Scopes" - The hygiene model
- R7RS §4.2.2: Local variable bindings shadow special forms
- R7RS §4.3: Macros

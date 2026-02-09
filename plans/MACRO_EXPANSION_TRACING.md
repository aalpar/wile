# Macro Expansion Tracing Plan

**Status:** PLANNED — Design in progress, implementation not started

> **Cross-reference**: See also `HYGIENE_DEBUGGING_DESIGN.md` for complementary work on scope provenance (`ScopeReason`, `formName`, `location`) and debugging primitives (`identifier-scopes`, `scope-info`, `binding-info`).

## Goal

Enable tracing of macro-generated code back to:
1. **Which macro** generated it (already exists: `OriginInfo.Identifier`)
2. **Which invocation** (unique application ID)
3. **Template source** (definition-site location)

## Current State

`OriginInfo` in `syntax/source_context.go` currently tracks:
```go
type OriginInfo struct {
    Identifier string         // ✓ Macro name
    Location   *SourceContext // ✓ Use-site (where macro was called)
    Parent     *OriginInfo    // ✓ Chain for nested macros
}
```

**Missing:**
- Intro scope reference - to trace back to the invocation
- `TemplateLocation` - where the template was defined

## Design

### 1. Extend `OriginInfo`

**File:** `syntax/source_context.go`

```go
type OriginInfo struct {
    IntroScope *Scope      // Intro scope for this invocation (IS the identity)
    MacroName  string      // Human-readable name (for error messages)
    UseSite    SyntaxValue // The macro invocation form (e.g., (my-macro x y))
    Template   SyntaxValue // The template that was expanded
    Parent     *OriginInfo // Chain for nested macros
}
```

Using `SyntaxValue` instead of `*SourceContext`:
- Source location still available via `.SourceContext()`
- Preserves actual syntax structure (the code, not just location)
- Can inspect macro call arguments from `UseSite`
- Can see template structure that produced expansion
- Better error messages - can print original forms

The `IntroScope` pointer serves as the unique identifier. You can:
- Get numeric ID via `IntroScope.ID()`
- Compare scope pointers directly for identity
- Match against scopes on identifiers to see if introduced by this invocation

### 2. Expose Scope ID

**File:** `syntax/syntax_value.go`

Add method to `Scope`:
```go
func (p *Scope) ID() uint64 {
    if s == nil {
        return 0
    }
    return s.id
}
```

### 3. Template Already Stored in Clause

**File:** `machine/compile_syntax_rules.go`

The `template` field already exists in `SyntaxRulesClause`:
```go
type SyntaxRulesClause struct {
    template     syntax.SyntaxValue  // The template to expand on match (already exists!)
    // ... other fields ...
}
```

No changes needed here - the template syntax is already available.

### 4. Populate OriginInfo During Expansion

**File:** `machine/operation_syntax_rules_transform.go`

Reorder to create intro scope before origin (~line 122-136):
```go
// Create intro scope FIRST (moved up from line 136)
introScope := syntax.NewScope()

// Now create origin with the intro scope
origin = &syntax.OriginInfo{
    IntroScope: introScope,
    MacroName:  macroName,
    UseSite:    input,           // The macro invocation form
    Template:   clause.template, // The template being expanded
    Parent:     parentOrigin,
}
```

Note: `input` is the macro invocation syntax (already available), `clause.template` is already stored.

### 5. Add `syntax-origin` Primitive

**File:** `extensions/eval/prim_eval.go` (or new file)

Add primitive to inspect origin info from Scheme:

```scheme
(syntax-origin stx) → origin-info or #f
```

Returns an association list with:
- `macro-name` - human-readable macro name
- `scope-id` - unique invocation ID (from intro scope)
- `use-site` - the macro invocation form (syntax object)
- `template` - the template that was expanded (syntax object)
- `parent` - parent origin alist or #f

Example return value:
```scheme
((macro-name . "my-macro")
 (scope-id . 42)
 (use-site . #'(my-macro x y))      ; actual syntax object
 (template . #'(+ x 1))             ; actual template syntax
 (parent . #f))
```

The syntax objects preserve full structure - you can further inspect them with `syntax->datum`, `syntax-source`, etc.

**File:** `extensions/eval/register.go`

Register the new primitive.

## Files to Modify

| File | Changes |
|------|---------|
| `syntax/source_context.go` | Extend `OriginInfo` with new fields |
| `syntax/syntax_value.go` | Add `Scope.ID()` method |
| `machine/operation_syntax_rules_transform.go` | Use new fields when creating `OriginInfo` |
| `extensions/eval/prim_eval.go` | Add `PrimSyntaxOrigin` function |
| `extensions/eval/register.go` | Register `syntax-origin` primitive |
| `syntax/coverage_test.go` | Update tests for new `OriginInfo` fields |

## Verification

1. **Build:** `go build ./...`
2. **Tests:** `go test ./...`
3. **Manual verification in REPL:**
   ```scheme
   (define-syntax my-mac
     (syntax-rules ()
       ((my-mac x) (+ x 1))))

   ;; Expand and inspect origin
   (define expanded (expand '(my-mac 5)))
   (define origin (syntax-origin expanded))

   ;; Check fields
   (assq 'macro-name origin)   ; => (macro-name . "my-mac")
   (assq 'scope-id origin)     ; => (scope-id . <some-number>)
   (assq 'use-site origin)     ; => (use-site . #'(my-mac 5))
   (assq 'template origin)     ; => (template . #'(+ x 1))
   ```
4. **Verify unique scope IDs per invocation:**
   ```scheme
   (define o1 (syntax-origin (expand '(my-mac 1))))
   (define o2 (syntax-origin (expand '(my-mac 2))))
   (cdr (assq 'scope-id o1))  ; different from
   (cdr (assq 'scope-id o2))  ; this one
   ```
5. **Verify scope matching:**
   ```scheme
   ;; Identifiers introduced by macro should have the intro scope
   ;; from origin in their scope set
   ```

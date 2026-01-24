# Hygiene Debugging Design

## Status: PLANNED

This document describes a debugging-focused approach to hygiene tooling, emphasizing introspection over manipulation.

## Design Philosophy

### Observation

Most macro authors don't need to *manipulate* scopes—they need to *understand* what's happening when things break. Racket's full API (`make-syntax-introducer`, `syntax-local-introduce`, `datum->syntax`, etc.) provides power but adds complexity that most users never need.

### Approach

1. **Minimal manipulation API**: `with-binding-scope` handles the common case (already implemented)
2. **Rich debugging API**: Tools to inspect scopes, understand resolution, diagnose failures
3. **Provenance tracking**: Every scope knows why it exists and where it came from

### Contrast with Racket

| Racket | This Design |
|--------|-------------|
| Scopes are anonymous objects | Scopes have identity, reason, location |
| Debugging via `identifier-binding` (opaque) | Structured introspection primitives |
| Full manipulation API | Minimal manipulation, rich debugging |
| Learn by trial and error | Errors explain what went wrong |

## Current State

### What Exists

**`OriginInfo`** (in `syntax/source_context.go`) tracks macro expansion provenance:
```go
type OriginInfo struct {
    Identifier string         // Macro name that caused expansion
    Location   *SourceContext // Where the macro was invoked
    Parent     *OriginInfo    // Chain for nested macros
}
```

This answers: "This syntax came from expanding macro X at location Y."

**`Scope`** (in `syntax/syntax_value.go`) currently lacks provenance:
```go
type Scope struct {
    keys   map[values.Symbol]ScopeID
    parent *Scope
}
```

### What's Missing

Scopes don't track:
- Why they were created (binding form? macro intro?)
- Where they were created (source location)
- What form created them (let? lambda? which macro?)

This makes debugging difficult: when resolution fails, users can't understand why.

---

## Scope Provenance Design

### Enhanced Scope Type

```go
// syntax/syntax_value.go

type ScopeReason int

const (
    ScopeReasonBindingForm ScopeReason = iota  // let, lambda, define, letrec
    ScopeReasonMacroIntro                       // intro scope from macro expansion
    ScopeReasonModuleTop                        // top-level module scope
    ScopeReasonPhase                            // phase separation scope
)

type Scope struct {
    // New: Provenance fields
    id       uint64          // Unique identifier for debugging/display
    reason   ScopeReason     // Why this scope exists
    formName string          // "let", "lambda", "my-macro", etc.
    location *SourceContext  // Where this scope was created

    // Existing fields
    keys   map[values.Symbol]ScopeID
    parent *Scope
}

var scopeCounter uint64

func nextScopeID() uint64 {
    return atomic.AddUint64(&scopeCounter, 1)
}

// NewScope creates a scope without provenance (for backwards compatibility)
func NewScope(parent *Scope) *Scope {
    return &Scope{
        id:     nextScopeID(),
        keys:   make(map[values.Symbol]ScopeID),
        parent: parent,
    }
}

// NewScopeWithProvenance creates a scope with full debugging information
func NewScopeWithProvenance(reason ScopeReason, formName string, loc *SourceContext) *Scope {
    return &Scope{
        id:       nextScopeID(),
        reason:   reason,
        formName: formName,
        location: loc,
        keys:     make(map[values.Symbol]ScopeID),
    }
}
```

### Human-Readable Display

```go
func (s *Scope) String() string {
    if s == nil {
        return "#<scope:nil>"
    }
    if s.location != nil && s.location.File != "" {
        return fmt.Sprintf("#<scope:%d %s:%s:%d>",
            s.id, s.reasonString(), s.formName, s.location.Start.Line)
    }
    if s.formName != "" {
        return fmt.Sprintf("#<scope:%d %s:%s>", s.id, s.reasonString(), s.formName)
    }
    return fmt.Sprintf("#<scope:%d>", s.id)
}

func (s *Scope) reasonString() string {
    switch s.reason {
    case ScopeReasonBindingForm:
        return "bind"
    case ScopeReasonMacroIntro:
        return "intro"
    case ScopeReasonModuleTop:
        return "module"
    case ScopeReasonPhase:
        return "phase"
    default:
        return "?"
    }
}
```

Example output:
```
#<scope:42 bind:let:foo.scm:10>
#<scope:17 intro:when:foo.scm:15>
#<scope:3 module:my-lib>
```

---

## Scope Creation Sites

### Binding Forms (`with-binding-scope`)

In `machine/expander_time_continuation.go`:

```go
func (p *ExpanderTimeContinuation) expandWithBindingScope(
    ectx ExpandTimeCallContext,
    _ *syntax.SyntaxSymbol,
    expr syntax.SyntaxValue,
) (syntax.SyntaxValue, error) {
    // ... parse form ...

    // Extract source location from the form for provenance
    loc := extractSourceContext(expr)

    // Create scope with provenance
    bindingScope := syntax.NewScopeWithProvenance(
        syntax.ScopeReasonBindingForm,
        "let",  // TODO: could extract actual form name from expansion context
        loc,
    )

    scopedBody := body.AddScope(bindingScope)
    return p.ExpandExpression(ectx, scopedBody)
}
```

### Macro Expansion (Intro Scope)

In `machine/operation_syntax_rules_transform.go` (or equivalent):

```go
// When expanding a macro invocation
introScope := syntax.NewScopeWithProvenance(
    syntax.ScopeReasonMacroIntro,
    macroName,      // e.g., "when", "let", "my-macro"
    useSiteContext, // where the macro was invoked
)

// Add intro scope to expanded output
expanded = expanded.AddScope(introScope)
```

### Module/Top-Level

```go
// When creating a new module environment
moduleScope := syntax.NewScopeWithProvenance(
    syntax.ScopeReasonModuleTop,
    moduleName,  // e.g., "(scheme base)", "my-lib"
    nil,         // no specific source location
)
```

---

## Debugging Primitives

### `identifier-scopes`

**Purpose**: Get the scopes attached to an identifier.

**Signature**: `(identifier-scopes id) → (scope ...)`

**Returns**: List of scope objects attached to the identifier.

```scheme
> (identifier-scopes #'x)
(#<scope:42 bind:let:foo.scm:10> #<scope:17 intro:when:foo.scm:15>)
```

**Implementation**:

```go
func primIdentifierScopes(args []values.Value) (values.Value, error) {
    if len(args) != 1 {
        return nil, values.NewArityError("identifier-scopes", 1, len(args))
    }

    id, ok := args[0].(*syntax.SyntaxSymbol)
    if !ok {
        return nil, values.NewForeignError("identifier-scopes: expected identifier")
    }

    scopes := id.Scopes()
    if len(scopes) == 0 {
        return values.EmptyList, nil
    }

    // Convert to list of scope values
    result := values.EmptyList
    for i := len(scopes) - 1; i >= 0; i-- {
        result = values.Cons(scopes[i], result)
    }
    return result, nil
}
```

**Registration**: `PhaseExpand | PhaseRuntime`

---

### `scope-info`

**Purpose**: Get provenance information about a scope.

**Signature**: `(scope-info scope) → alist`

**Returns**: Association list with scope details.

```scheme
> (scope-info (car (identifier-scopes #'x)))
((id . 42)
 (reason . binding-form)
 (form . "let")
 (file . "foo.scm")
 (line . 10)
 (column . 5))
```

**Implementation**:

```go
func primScopeInfo(args []values.Value) (values.Value, error) {
    if len(args) != 1 {
        return nil, values.NewArityError("scope-info", 1, len(args))
    }

    scope, ok := args[0].(*syntax.Scope)
    if !ok {
        return nil, values.NewForeignError("scope-info: expected scope")
    }

    // Build alist
    result := values.EmptyList

    // ID
    result = values.Cons(
        values.Cons(values.NewSymbol("id"), values.NewInteger(int64(scope.ID()))),
        result,
    )

    // Reason
    var reasonSym string
    switch scope.Reason() {
    case syntax.ScopeReasonBindingForm:
        reasonSym = "binding-form"
    case syntax.ScopeReasonMacroIntro:
        reasonSym = "macro-intro"
    case syntax.ScopeReasonModuleTop:
        reasonSym = "module-top"
    case syntax.ScopeReasonPhase:
        reasonSym = "phase"
    default:
        reasonSym = "unknown"
    }
    result = values.Cons(
        values.Cons(values.NewSymbol("reason"), values.NewSymbol(reasonSym)),
        result,
    )

    // Form name
    if scope.FormName() != "" {
        result = values.Cons(
            values.Cons(values.NewSymbol("form"), values.NewString(scope.FormName())),
            result,
        )
    }

    // Location
    if loc := scope.Location(); loc != nil {
        if loc.File != "" {
            result = values.Cons(
                values.Cons(values.NewSymbol("file"), values.NewString(loc.File)),
                result,
            )
        }
        result = values.Cons(
            values.Cons(values.NewSymbol("line"), values.NewInteger(int64(loc.Start.Line))),
            result,
        )
        result = values.Cons(
            values.Cons(values.NewSymbol("column"), values.NewInteger(int64(loc.Start.Column))),
            result,
        )
    }

    return result, nil
}
```

**Registration**: `PhaseExpand | PhaseRuntime`

---

### `binding-info`

**Purpose**: Explain what an identifier resolves to and why.

**Signature**: `(binding-info id [env]) → alist or #f`

**Returns**: Association list with binding details, or `#f` if unbound.

```scheme
> (binding-info #'x)
((status . bound)
 (kind . local)
 (index . (0 2))
 (defined-at . "foo.scm:5:3")
 (binding-scopes . (#<scope:42>))
 (reference-scopes . (#<scope:42> #<scope:17> #<scope:99>))
 (match-reason . "binding scopes ⊆ reference scopes"))

> (binding-info #'undefined-var)
#f

> (binding-info #'x)  ; when ambiguous
((status . ambiguous)
 (candidates . (((defined-at . "foo.scm:10") (scopes . (#<scope:42>)))
                ((defined-at . "foo.scm:18") (scopes . (#<scope:42> #<scope:55>)))))
 (reference-scopes . (#<scope:42> #<scope:17> #<scope:55>))
 (reason . "multiple bindings match; neither is more specific"))
```

**Implementation Sketch**:

```go
func primBindingInfo(env *environment.EnvironmentFrame, args []values.Value) (values.Value, error) {
    if len(args) < 1 || len(args) > 2 {
        return nil, values.NewArityError("binding-info", "1-2", len(args))
    }

    id, ok := args[0].(*syntax.SyntaxSymbol)
    if !ok {
        return nil, values.NewForeignError("binding-info: expected identifier")
    }

    // Use provided env or current
    lookupEnv := env
    if len(args) == 2 {
        // Extract environment from second arg if provided
    }

    sym := lookupEnv.InternSymbol(id.Sym)
    refScopes := id.Scopes()

    // Try to find binding with detailed info
    binding, bindingScopes, ambiguous := lookupEnv.GetBindingWithScopesDetailed(sym, refScopes)

    if ambiguous != nil {
        // Return ambiguity info
        return buildAmbiguousInfo(ambiguous, refScopes), nil
    }

    if binding == nil {
        return values.False, nil
    }

    // Return successful binding info
    return buildBindingInfo(binding, bindingScopes, refScopes), nil
}
```

**Registration**: `PhaseExpand` (needs environment access)

---

### `scope?`

**Purpose**: Type predicate for scope objects.

**Signature**: `(scope? obj) → boolean`

```go
func primScopeP(args []values.Value) (values.Value, error) {
    if len(args) != 1 {
        return nil, values.NewArityError("scope?", 1, len(args))
    }
    _, ok := args[0].(*syntax.Scope)
    return values.NewBoolean(ok), nil
}
```

---

### `scope=?`

**Purpose**: Compare scope identity.

**Signature**: `(scope=? scope1 scope2) → boolean`

```go
func primScopeEqP(args []values.Value) (values.Value, error) {
    if len(args) != 2 {
        return nil, values.NewArityError("scope=?", 2, len(args))
    }
    s1, ok1 := args[0].(*syntax.Scope)
    s2, ok2 := args[1].(*syntax.Scope)
    if !ok1 || !ok2 {
        return nil, values.NewForeignError("scope=?: expected scopes")
    }
    return values.NewBoolean(s1 == s2), nil  // pointer identity
}
```

---

## Error Messages

### Resolution Failure

When `CompileSymbol` fails to resolve an identifier, provide context:

```
Error: unbound identifier 'x' at foo.scm:20:5

Reference has scopes:
  #<scope:42 bind:let:foo.scm:10>
  #<scope:17 intro:when:foo.scm:15>

No binding for 'x' has a scope set that is a subset of these scopes.

Hint: If 'x' was intended to come from a macro, check that the macro
correctly preserves the binding's scopes in its template.
```

### Ambiguous Binding

When multiple bindings match:

```
Error: ambiguous binding for 'x' at foo.scm:20:5

Reference has scopes:
  #<scope:42 bind:let:foo.scm:10>
  #<scope:55 bind:let:foo.scm:18>
  #<scope:17 intro:when:foo.scm:15>

Candidate bindings:
  1. 'x' at foo.scm:10 with scopes {#<scope:42>}
     Matches because {42} ⊆ {42, 55, 17}

  2. 'x' at foo.scm:18 with scopes {#<scope:42>, #<scope:55>}
     Matches because {42, 55} ⊆ {42, 55, 17}

Neither binding's scopes are a subset of the other's, so neither is
more specific. This typically happens with nested binding forms in macros.
```

---

## Implementation Plan

### Phase 1: Scope Provenance (~100 LOC)

| File | Change |
|------|--------|
| `syntax/syntax_value.go` | Add `ScopeReason`, provenance fields to `Scope`, `NewScopeWithProvenance`, `String()` |
| `syntax/scope_reason.go` | New file with `ScopeReason` type and constants |

### Phase 2: Update Scope Creation Sites (~50 LOC)

| File | Change |
|------|--------|
| `machine/expander_time_continuation.go` | Use `NewScopeWithProvenance` in `expandWithBindingScope` |
| `machine/operation_syntax_rules_transform.go` | Use `NewScopeWithProvenance` for intro scope |

### Phase 3: Debugging Primitives (~200 LOC)

| File | Change |
|------|--------|
| `registry/core/prim_hygiene_debug.go` | New file with `identifier-scopes`, `scope-info`, `binding-info`, `scope?`, `scope=?` |
| `registry/core/hygiene_debug.go` | Registration |

### Phase 4: Enhanced Error Messages (~100 LOC)

| File | Change |
|------|--------|
| `machine/compile_time_continuation.go` | Enhanced error in `CompileSymbol` |
| `environment/environment_frame.go` | Add `GetBindingWithScopesDetailed` for ambiguity detection |

**Total**: ~450 LOC

---

## What This Design Omits (Intentionally)

These are power tools for advanced macro authors. They can be added later if needed:

| Primitive | Purpose | Why Omitted |
|-----------|---------|-------------|
| `make-syntax-introducer` | Create scope-flipping procedure | Rarely needed; `with-binding-scope` covers common case |
| `syntax-local-introduce` | Flip current macro's intro scope | For intentional hygiene breaking |
| `datum->syntax` | Programmatic syntax construction | Power tool for `syntax-case` macros |
| `syntax->datum` | Strip syntax to datum | Useful but not essential |
| `local-expand` | Partial expansion | Very advanced |
| `syntax-local-value` | Get compile-time binding | For macro introspection |

If these are needed later, they can be added without changing the core provenance design.

---

## Testing

### Unit Tests

```scheme
;; Test identifier-scopes returns list
(test-assert "identifier-scopes returns list"
  (list? (identifier-scopes #'x)))

;; Test scope-info returns alist with expected keys
(let ((info (scope-info (car (identifier-scopes #'x)))))
  (test-assert "scope-info has id" (assq 'id info))
  (test-assert "scope-info has reason" (assq 'reason info)))

;; Test scope? predicate
(test-assert "scope? on scope" (scope? (car (identifier-scopes #'x))))
(test-assert "scope? on non-scope" (not (scope? 42)))

;; Test scope=? identity
(let ((scopes (identifier-scopes #'x)))
  (test-assert "scope=? reflexive" (scope=? (car scopes) (car scopes))))
```

### Integration Tests

```scheme
;; Test that binding-info works for local bindings
(let ((x 1))
  (let ((info (binding-info #'x)))
    (test-equal "binding-info status" 'bound (cdr (assq 'status info)))
    (test-equal "binding-info kind" 'local (cdr (assq 'kind info)))))

;; Test that binding-info returns #f for unbound
(test-equal "binding-info unbound" #f (binding-info #'not-bound-anywhere))
```

---

## References

- Flatt, M. (2016). "Binding as Sets of Scopes" — Core theory
- `syntax/source_context.go` — Existing `OriginInfo` for macro expansion provenance
- `machine/expander_time_continuation.go` — `with-binding-scope` implementation

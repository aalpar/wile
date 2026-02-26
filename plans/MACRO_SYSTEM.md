# Macro System Plans

---

# `er-macro-transformer` (Explicit Renaming Macros)

**Status:** Proposed

## Goal

Add `er-macro-transformer` to Wile's macro system, providing a procedural
macro facility that operates on raw s-expressions with opt-in hygiene via
`rename` and `compare` closures.

```scheme
(define-syntax my-or
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        `(let ((,(rename 'tmp) ,a))
           (if ,(rename 'tmp) ,(rename 'tmp) ,b))))))
```

## API Contract

```
(er-macro-transformer <lambda-expr>)
```

- `<lambda-expr>` must be a `(lambda (form rename compare) body...)` with
  exactly 3 parameters
- **form**: the complete macro invocation as a raw s-expression (unwrapped
  from syntax objects), e.g. `(my-or a b)` as a plain list of symbols
- **rename**: `(rename sym) -> symbol` — returns a symbol that resolves to
  the binding at the macro *definition* site. Calling `rename` on the same
  symbol within one invocation always returns the same (eq?) renamed symbol.
  This is how ER macros achieve hygiene.
- **compare**: `(compare id1 id2) -> boolean` — returns `#t` if the two
  identifiers refer to the same binding (used for literal matching, like
  checking if an identifier is `else` or `=>`)
- **return**: a raw s-expression that is re-wrapped into syntax objects
  with the use-site context, then recursively expanded

## Architecture

### Key Insight

The existing `expandMacroInvocation` invokes *any* `MachineClosure` with
a single argument (the input form as syntax). For ER macros, we need to:
1. **Unwrap** the input form before passing it
2. **Create** rename/compare closures (environment-capturing Go operations)
3. **Pass 3 arguments** instead of 1
4. **Re-wrap** the result back to syntax

This is handled by detecting ER transformers in `expandMacroInvocation`
via a marker type.

### Scope-Sets Integration

ER macros map cleanly onto Wile's Flatt scope-sets model:

| ER concept | Scope-sets equivalent |
|---|---|
| `rename` returns "hygienic" symbol | Create `SyntaxSymbol` with definition-site scopes |
| `compare` checks "same binding" | Resolve both symbols via `GetBindingWithScopes`, check equality |
| Un-renamed symbol | Symbol with no special scopes (use-site resolution) |

## Implementation Phases

### Phase 1: Transformer Recognition + Compilation

**File: `machine/compile_transformer.go`**

Add `"er-macro-transformer"` case to `compileTransformerToMachineClosure`:

```go
case "er-macro-transformer":
    return compileERMacroTransformer(ctx, env, transformerPair)
```

**New file: `machine/compile_er_macro.go`**

`compileERMacroTransformer(ctx, env, expr)`:
1. Extract the lambda expression from `(er-macro-transformer <lambda>)`
2. Validate it has exactly 3 parameters
3. Compile and evaluate the lambda via `compileAndEvalLambdaTransformer`
   (reuse existing lambda compilation — the lambda is a normal 3-arg closure)
4. Wrap the resulting `MachineClosure` in an `*ERMacroTransformer` marker struct:

```go
// ERMacroTransformer wraps a 3-arg MachineClosure to identify it as an
// explicit-renaming transformer in expandMacroInvocation.
type ERMacroTransformer struct {
    closure *MachineClosure
    defEnv  *environment.EnvironmentFrame // definition-site environment
}
```

Store the `*ERMacroTransformer` as the binding value (it satisfies
`values.Value` via embedding or a simple wrapper). The `defEnv` captures the
environment at `define-syntax` time — needed by `rename` to resolve
definition-site bindings.

### Phase 2: ER Rename/Compare Operations

**New file: `machine/operation_er_macro.go`**

Two lightweight VM operations, each wrapped into a 1-param `MachineClosure`
at invocation time:

#### `OperationERRename`

Captures: `defEnv *environment.EnvironmentFrame`, `cache map[string]*syntax.SyntaxSymbol`

```
rename(sym) -> syntax-symbol
```

1. Get argument (a `*values.Symbol` or `*syntax.SyntaxSymbol`)
2. Check cache — if already renamed in this invocation, return cached symbol
3. Look up `sym` in `defEnv` to find its definition-site binding
4. Create a `SyntaxSymbol` with the definition-site scopes:
   - If found in expand env -> use the binding's scopes
   - If found in runtime env -> use the binding's scopes
   - If not found -> create symbol with empty scopes (top-level)
5. Set `ResolvedBinding` on the new `SyntaxSymbol` to the `GlobalIndex`
   (same mechanism as `FreeIdResolution` in syntax-rules)
6. Cache and return

The rename cache ensures `(eq? (rename 'x) (rename 'x))` is `#t` within
one macro invocation. This is required by the ER macro contract.

#### `OperationERCompare`

Captures: `useEnv *environment.EnvironmentFrame`

```
compare(id1, id2) -> boolean
```

1. Get both arguments (symbols or syntax symbols)
2. For each, resolve to a binding:
   - If `SyntaxSymbol` with scopes, use `GetBindingWithScopes`
   - If plain `Symbol`, use `GetBinding`
3. Return `#t` if both resolve to the same binding (pointer equality on
   `*Binding` or `*GlobalIndex`), or both resolve to no binding and have
   the same symbol name

### Phase 3: Expander Integration

**File: `machine/expander_time_continuation.go`**

Modify `expandMacroInvocation` to detect `*ERMacroTransformer`:

```go
func (p *ExpanderTimeContinuation) expandMacroInvocation(...) {
    // Check if it's an ER transformer
    if erTransformer, ok := bnd.Value().(*ERMacroTransformer); ok {
        return p.expandERMacroInvocation(ctx, sym, expr, erTransformer)
    }
    // ... existing MachineClosure path ...
}
```

New method `expandERMacroInvocation`:

1. Build the input form: `(sym . expr)` as syntax, then `UnwrapAll()` to
   get raw s-expression
2. Create `rename` closure:
   - Fresh `OperationERRename` with `erTransformer.defEnv` and empty cache
   - Wrap in a 1-param `MachineClosure`
3. Create `compare` closure:
   - Fresh `OperationERCompare` with `p.env` (use-site)
   - Wrap in a 2-param `MachineClosure`
4. Create `MachineContext` from `erTransformer.closure`
5. Apply with 3 arguments: `(form, rename, compare)`
6. Run the VM
7. Get the result (a raw s-expression)
8. Re-wrap with `datumToSyntax(result, useSiteSourceContext)`
   - Use the use-site source context so error messages point to the macro call
   - Renamed symbols are already `SyntaxSymbol` with definition-site scopes;
     `datumToSyntax` passes through existing `SyntaxValue`s unchanged
9. Recursively expand: `p.ExpandExpression(ctx, wrapped)`

### Phase 4: `let-syntax` / `letrec-syntax` Support

**File: `machine/expander_time_continuation.go`**

`expandLetSyntaxImpl` currently only accepts `syntax-rules`. Extend it to
also accept `er-macro-transformer` (and `lambda`) by routing through
`compileTransformerToMachineClosure` instead of hardcoding `CompileSyntaxRules`.

This is a refactoring of the transformer compilation logic in `expandLetSyntaxImpl` (around line 372).

### Phase 5: Primitive Expander Registration

**File: `machine/primitive_expanders_registry.go`**

Register `"er-macro-transformer"` as a pass-through primitive expander
(like `syntax-case`, `quasiquote`) so the expander doesn't try to expand
it as a procedure call. It should return unchanged since it's only
meaningful inside `define-syntax`.

### Phase 6: Tests

**New file: `machine/compile_er_macro_test.go`**

1. **Basic ER macro** — `my-or` from the example above
2. **Hygiene via rename** — macro introduces a `tmp` variable that doesn't
   capture user's `tmp`
3. **Compare for literals** — macro that checks for `else` keyword
4. **Rename caching** — `(eq? (rename 'x) (rename 'x))` returns `#t`
5. **Nested ER macros** — ER macro expanding to another ER macro call
6. **ER inside let-syntax** — local ER macro definitions
7. **ER mixed with syntax-rules** — ER macro calling a syntax-rules macro
8. **Un-renamed symbols** — verify they resolve at use site (intentional
   hygiene breaking for anaphoric macros)

**Integration test: `integration/er_macro_test.go`**

End-to-end tests via `Engine.Eval`:
- Chibi `optional.sld` compatibility (the library already has an
  `er-macro-transformer` branch in its `cond-expand`)
- Real-world patterns: `swap!`, `aif` (anaphoric if), `my-cond`

## Files Changed

| File | Change |
|---|---|
| `machine/compile_transformer.go` | Add `er-macro-transformer` case |
| `machine/compile_er_macro.go` | **New** — compilation logic |
| `machine/operation_er_macro.go` | **New** — rename/compare operations |
| `machine/expander_time_continuation.go` | ER detection + `expandERMacroInvocation` |
| `machine/primitive_expanders_registry.go` | Register `er-macro-transformer` |
| `machine/compile_er_macro_test.go` | **New** — unit tests |
| `integration/er_macro_test.go` | **New** — integration tests |

## Design Decisions

**Why marker type instead of bytecode flag?** The `*ERMacroTransformer`
wrapper is explicit and type-safe. It avoids coupling to bytecode layout
and makes the detection in `expandMacroInvocation` a simple type switch.

**Why Go-implemented rename/compare instead of Scheme?** The rename and
compare operations need direct access to environment frames and scope
sets. Implementing them in Go avoids a complex bootstrapping problem and
keeps the critical path simple.

**Why not modify `expandMacroInvocation`'s calling convention?** We add
a new method `expandERMacroInvocation` rather than adding conditionals
to the existing one. This keeps the syntax-rules/lambda path untouched.

**Re-wrapping uses `datumToSyntax` with use-site context.** This means
un-renamed symbols get the use-site's source context (no scopes), and
renamed symbols (already `SyntaxSymbol`) pass through with their
definition-site scopes intact. This is correct: un-renamed symbols
should resolve at the use site.

## Open Questions

1. **Should `rename` accept syntax symbols or only plain symbols?**
   Chibi accepts both. Recommended: accept both, extract the key if
   syntax symbol.

2. **What about `syntax->datum` on renamed symbols?** A renamed symbol
   is a `SyntaxSymbol`. `syntax->datum` would strip it to a plain symbol.
   This matches expected behavior.

3. **Should `er-macro-transformer` be available at phase 0 (as a runtime
   value)?** No — it should only be valid inside `define-syntax`. Same
   restriction as `syntax-rules`.

---

# Hygiene Debugging Design

**Status:** PLANNED — Not started

> **Cross-reference**: See Macro Expansion Tracing (below) for complementary `OriginInfo` extensions.

## Philosophy

Most macro authors need to *understand* what's happening when things break, not *manipulate* scopes. Minimal manipulation API (`with-binding-scope` — already implemented) + rich debugging API.

## Scope Provenance

### Enhanced Scope Type

Add provenance fields to `Scope` in `internal/syntax/syntax_value.go` (currently has `id uint64` and `IsRebinding bool`):

| Field | Type | Purpose |
|-------|------|---------|
| `id` | `uint64` | Unique identifier (atomic counter) |
| `reason` | `ScopeReason` | Why this scope exists |
| `formName` | `string` | "let", "lambda", "my-macro", etc. |
| `location` | `*SourceContext` | Where this scope was created |

**`ScopeReason` enum**: `ScopeReasonBindingForm`, `ScopeReasonMacroIntro`, `ScopeReasonModuleTop`, `ScopeReasonPhase`

**Display format**: `#<scope:42 bind:let:foo.scm:10>`, `#<scope:17 intro:when:foo.scm:15>`

### Scope Creation Sites

| Site | File | Reason |
|------|------|--------|
| `expandWithBindingScope` | `machine/expander_time_continuation.go` | `ScopeReasonBindingForm` |
| Macro expansion (intro scope) | `machine/operation_syntax_rules_transform.go` | `ScopeReasonMacroIntro` |
| Module/top-level | Various | `ScopeReasonModuleTop` |

## Debugging Primitives

| Primitive | Signature | Purpose |
|-----------|-----------|---------|
| `identifier-scopes` | `(identifier-scopes id) -> (scope ...)` | Get scopes attached to an identifier |
| `scope-info` | `(scope-info scope) -> alist` | Get provenance info (id, reason, form, file, line) |
| `binding-info` | `(binding-info id [env]) -> alist or #f` | Explain what an identifier resolves to and why |
| `scope?` | `(scope? obj) -> boolean` | Type predicate |
| `scope=?` | `(scope=? s1 s2) -> boolean` | Compare scope identity (pointer equality) |

## Enhanced Error Messages

**Resolution failure** will show: reference scopes, why no binding matched, hint about macro hygiene.

**Ambiguous binding** will show: reference scopes, candidate bindings with their scopes, explanation of why neither is more specific.

## Implementation Phases

| Phase | Description | ~LOC |
|-------|-------------|------|
| 1 | Scope provenance (ScopeReason, fields, NewScopeWithProvenance, String()) | ~100 |
| 2 | Update scope creation sites | ~50 |
| 3 | Debugging primitives (5 primitives) | ~200 |
| 4 | Enhanced error messages in CompileSymbol | ~100 |

**Total**: ~450 LOC

## Intentionally Omitted

`make-syntax-introducer`, `syntax-local-introduce`, `datum->syntax`, `local-expand`, `syntax-local-value` — power tools for advanced macro authors. Can be added later without changing the core provenance design.

## Files to Modify

| File | Change |
|------|--------|
| `internal/syntax/syntax_value.go` | Add provenance fields to Scope |
| `internal/syntax/scope_reason.go` | New — ScopeReason type and constants |
| `machine/expander_time_continuation.go` | Use NewScopeWithProvenance |
| `machine/operation_syntax_rules_transform.go` | Use NewScopeWithProvenance for intro scope |
| `registry/core/prim_hygiene_debug.go` | New — 5 debugging primitives |
| `machine/compile_time_continuation.go` | Enhanced error in CompileSymbol |
| `environment/environment_frame.go` | Add GetBindingWithScopesDetailed |

---

# Macro Expansion Tracing Plan

**Status:** PLANNED — Not started

> **Cross-reference**: See Hygiene Debugging Design (above) for complementary scope provenance work.

## Goal

Enable tracing of macro-generated code back to which macro generated it, which invocation (unique ID), and the template source.

## Current State

`OriginInfo` in `internal/syntax/source_context.go` tracks: macro name (`Identifier`), application ID (`ApplicationID`), use-site location (`Location`), template location (`TemplateLocation`), nesting chain (`Parent`). These fields were added in prior work. Remaining: intro scope reference, use-site syntax form, template syntax form.

## Design

### Extended `OriginInfo`

| Field | Type | Purpose |
|-------|------|---------|
| `IntroScope` | `*Scope` | Unique invocation identity (scope pointer = ID) |
| `MacroName` | `string` | Human-readable name for error messages |
| `UseSite` | `SyntaxValue` | The macro invocation form |
| `Template` | `SyntaxValue` | The template that was expanded |
| `Parent` | `*OriginInfo` | Chain for nested macros |

Using `SyntaxValue` instead of `*SourceContext` preserves actual syntax structure — can inspect arguments, print original forms, get location via `.SourceContext()`.

### `syntax-origin` Primitive

```scheme
(syntax-origin stx) -> alist or #f
;; Returns: ((macro-name . "my-macro") (scope-id . 42)
;;           (use-site . #'(my-macro x y)) (template . #'(+ x 1))
;;           (parent . #f))
```

## Files to Modify

| File | Change |
|------|--------|
| `internal/syntax/source_context.go` | Extend `OriginInfo` with new fields |
| `machine/operation_syntax_rules_transform.go` | Create intro scope before origin, populate new fields |
| `internal/extensions/eval/prim_eval.go` | Add `PrimSyntaxOrigin` |
| `internal/extensions/eval/register.go` | Register `syntax-origin` |
| `internal/syntax/coverage_test.go` | Update tests |

## Notes

`clause.template` already exists in `SyntaxRulesClause` — no changes needed in `compile_syntax_rules.go`. The reordering in `operation_syntax_rules_transform.go` (create intro scope before origin, ~line 122-136) is the only non-trivial code change.

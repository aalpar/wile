# Macro System Plans

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

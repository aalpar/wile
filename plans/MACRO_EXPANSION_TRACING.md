# Macro Expansion Tracing Plan

**Status:** PLANNED — Not started

> **Cross-reference**: See `HYGIENE_DEBUGGING_DESIGN.md` for complementary scope provenance work.

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
(syntax-origin stx) → alist or #f
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

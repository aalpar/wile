# Structured Docstring Metadata — Design

**Date**: 2026-04-06
**Status**: Approved
**Motivation**: Scheme-defined procedures (`map`, `for-each`, `assoc`, etc.) have Guile-style
docstrings but lack the structured metadata (parameter types, return types, categories) that
Go-implemented primitives carry via `PrimitiveSpec`. This makes them second-class citizens in
`,doc`, `,apropos`, and `,topics`. The goal is parity: same documentation tools, same output
format, regardless of implementation language.

## Approach

Parse structured metadata from sections within the existing Guile-style docstring. Extends
the `CODING_STYLE.md` convention with three new section headers (`Parameters:`, `Returns:`,
`Category:`) alongside the existing `Examples:` and `See also:`.

## Docstring Format

A single Scheme string literal (the leading-string-literal convention from PR #579). Sections
may appear in any order. Text before the first recognized section header is prose description.

### Section Headers

| Section | Format | Required? |
|---------|--------|-----------|
| `Parameters:` | Indented `name : type` lines | No |
| `Returns:` | Single type name on same line | No |
| `Category:` | Single category name on same line | No |
| `Examples:` | Indented code lines (existing convention) | No |
| `See also:` | Comma-separated `` `name' `` refs (existing convention) | No |

### Full Example

```scheme
(define (map f lst)
  "Apply F to each element of LST, returning a list of results.\nWith multiple lists, F receives one element from each list per\ncall. Stops at the shortest list.\n\nParameters:\n  f : procedure\n  lst : list\nReturns: list\nCategory: lists\n\nExamples:\n  (map + '(1 2) '(3 4))  => (4 6)\n\nSee also: `for-each', `vector-map', `string-map'."
  ...)
```

### Minimal Example

```scheme
(define (not x)
  "Return #t if X is #f, #f otherwise.\n\nCategory: predicates"
  (if x #f #t))
```

## Type Vocabulary

The `Parameters:` section uses type names that map to `ValueType` constants in `values/`.
Unrecognized type names map to `TypeAny` (graceful degradation).

| Docstring name | ValueType constant |
|----------------|--------------------|
| `any` | `TypeAny` |
| `boolean` | `TypeBoolean` |
| `number` | `TypeNumber` |
| `complex` | `TypeComplex` |
| `real` | `TypeReal` |
| `rational` | `TypeRational` |
| `integer` | `TypeInteger` |
| `exact-integer` | `TypeExactInteger` |
| `string` | `TypeString` |
| `char` | `TypeCharacter` |
| `symbol` | `TypeSymbol` |
| `pair` | `TypePair` |
| `list` | `TypeList` |
| `vector` | `TypeVector` |
| `bytevector` | `TypeByteVector` |
| `hashtable` | `TypeHashtable` |
| `procedure` | `TypeProcedure` |
| `port` | `TypePort` |
| `input-port` | `TypeInputPort` |
| `output-port` | `TypeOutputPort` |

## Parser Design

New package `internal/docparse/` with a single entry point:

```go
func ParseDocstring(raw string) DocInfo
```

`DocInfo` reuses the existing `repl.DocInfo` type (or a shared equivalent):

```go
type DocInfo struct {
    Doc        string             // prose description (before first section)
    ParamNames []string
    ParamTypes []values.ValueType
    ReturnType values.ValueType
    Category   string
}
```

### Algorithm

1. Split string on `\n`
2. Scan for section headers (line starts with a known keyword followed by `:`)
3. Everything before the first header → prose description (trimmed)
4. Each header starts a section; subsequent indented lines belong to it
5. A new header or non-indented non-blank line ends the previous section
6. `Parameters:` lines parsed as `name : type` — split on ` : `, look up type
7. `Returns:` and `Category:` — take trimmed rest of header line
8. `Examples:` and `See also:` — preserved in prose (already handled by existing rendering)

## Integration Points

### 1. `,doc` REPL command (`internal/repl/meta.go`)

`formatBindingDoc` changes: when the binding holds a `MachineClosure` or `CaseLambdaClosure`
with a non-empty `Doc()`, call `ParseDocstring`. If the result has any structured metadata
(ParamNames, ReturnType, or Category non-empty), use `formatPrimitiveDoc` (the same renderer
Go primitives use). Otherwise, fall through to the existing raw-text display.

### 2. `,apropos` and `,topics`

At bootstrap completion, walk all runtime bindings, parse docstrings with `Category:`,
and register synthetic entries in the registry. This makes Scheme-defined procedures
visible to `RegistryDocProvider.Search()` and `RegistryDocProvider.Categories()`.

### 3. MCP `doc`/`apropos` tools

These use the same `DocSearchProvider` interface. Registry population at bootstrap time
means the MCP tools get structured data for free.

## Bootstrap Registration Flow

```
Engine startup sequence:
  1. Registry.Apply(env)                  — Go primitives bound
  2. Bootstrap macros loaded              — define-syntax forms
  3. Bootstrap procedures loaded          — define forms
  4. ApplyDocs(env)                       — special form/macro docs (existing)
  5. RegisterSchemeDocstrings(env, reg)    — NEW
```

`RegisterSchemeDocstrings` does:

1. Iterate all runtime bindings in the top-level environment
2. For each `MachineClosure` or `CaseLambdaClosure` with non-empty `Doc()`
3. Call `ParseDocstring` on the doc text
4. If the result has structured metadata, register a synthetic `PrimitiveSpec` entry
5. Skip bindings already registered as Go primitives

Only procedures with structured docstrings become visible to `apropos`/`topics`.
Adding `Category:` is the opt-in signal.

## Changes by Component

| Component | Changes? | What |
|-----------|----------|------|
| `CODING_STYLE.md` | Yes | Add `Parameters:`, `Returns:`, `Category:` to docstring conventions |
| New `internal/docparse/` | Yes | `ParseDocstring` + type vocabulary table |
| `internal/repl/meta.go` | Yes | `formatBindingDoc` uses `ParseDocstring` for closures |
| `registry/registry.go` | Yes | Method to register synthetic doc entries |
| Engine startup | Yes | `RegisterSchemeDocstrings` after bootstrap |
| Bootstrap `.scm` files | Yes | Add structured sections to ~30 core procedure docstrings |
| `formatPrimitiveDoc` | No | Already renders the right output |
| `DocProvider`/`DocSearchProvider` | No | Interface unchanged |
| `NativeTemplate.doc` / compiler | No | Unchanged |
| `procedure-documentation` | No | Returns raw docstring (metadata parsing is presentation) |

## Phasing

### Phase 1: Parser + Integration Plumbing

- `internal/docparse/` package with `ParseDocstring` and type vocabulary
- Modify `formatBindingDoc` to use parsed metadata
- `RegisterSchemeDocstrings` in engine startup
- Tests: parser unit tests, integration test for `,doc` parity

### Phase 2: Annotate Bootstrap Procedures (~30)

- `map`, `for-each`, `vector-map`, `vector-for-each`, `string-map`, `string-for-each`
- `member`, `assoc`
- `not`, `zero?`, `positive?`, `negative?`, `list?`, `exact-integer?`
- `boolean=?`, `symbol=?`, `square`
- CxR accessors (mechanical — just add `Category: pairs`)

### Phase 3: Annotate Stdlib Library Procedures (~270)

Same phasing as the existing `2026-03-27-scheme-library-docstrings-design.md` plan.

## Out of Scope

- `define-syntax` macro metadata (no compiler mechanism for macro docstrings yet)
- Runtime type enforcement from docstring declarations (contracts are a separate system)
- Docstring generation tooling
- Changes to `procedure-documentation` return value (stays raw string)

# Apropos & Topic Browsing Design

## Problem

No way to search for procedures by keyword or browse by category. Docstrings are
only visible one-at-a-time via `,doc`. The `PrimitiveSpec.Category` field (25
categories, ~280 primitives) is never exposed to users.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Search scope | Name + doc text + category | Pays off docstring investment |
| Apropos REPL format | `name [category] one-line-doc` | Orients without reading each doc |
| Apropos primitive return | List of symbols | Composable; printing is separate |
| Topic browsing | Two primitives (`topics`, `topic`) | Mirrors REPL commands cleanly |

## New Commands & Primitives

### REPL Commands

| Command | Args | Description |
|---------|------|-------------|
| `,apropos <pattern>` | substring (case-insensitive) | Search names, docs, categories |
| `,topics` | none | List all category names with counts |
| `,topic <category>` | category name | List all bindings in category |

### Scheme Primitives

| Primitive | Signature | Returns |
|-----------|-----------|---------|
| `(apropos pattern)` | `(apropos string)` → list of symbols | Symbols whose name, doc, or category matches |
| `(doc-topics)` | `(doc-topics)` → list of strings | Category name strings, sorted |
| `(doc-topic name)` | `(doc-topic string)` → list of symbols | Symbols in the named category, sorted |

All three primitives go in `registry/core/reflection.go`, category `"reflection"`.

## Architecture

### Search Infrastructure

A new `DocSearchProvider` interface extends `DocProvider` with search capabilities:

```go
// DocSearchResult holds one search hit.
type DocSearchResult struct {
    Name     string
    Doc      string
    Category string
}

// DocSearchProvider extends DocProvider with search and category browsing.
type DocSearchProvider interface {
    DocProvider
    Search(pattern string) []DocSearchResult
    Categories() []string
    ByCategory(category string) []DocSearchResult
}
```

`RegistryDocProvider` implements `DocSearchProvider` — it already has the
`*registry.Registry`, which has `Primitives()` and `PrimitivesByCategory()`.

### Why extend DocProvider rather than adding standalone functions?

The `MetaCommandHandler` already holds a `DocProvider`. Extending the interface
lets `,apropos` and `,topic` use the same field without adding new constructor
parameters. The Scheme primitives access the same data through the registry
directly (they don't go through `DocProvider`).

### Search algorithm

Case-insensitive substring match against three fields per primitive:
1. `Spec.Name`
2. `Spec.Doc`
3. `Spec.Category`

If any field contains the pattern, the entry is a hit. Results sorted by name.

For environment bindings (macros, special forms, user defines): walk phase
environments, match binding name and `Binding.Doc()`. These have no category
field, so category-based topic browsing only covers registry primitives.

### REPL Output Format

**`,apropos "string"`:**
```
string-append    [strings]     Concatenate strings
string-copy      [strings]     Returns a copy of string
string-ref       [strings]     Return character at index k
number->string   [arithmetic]  Convert number to string representation
```

Column widths: name left-aligned to longest match, category in brackets, doc
truncated to terminal width.

**`,topics`:**
```
Categories:
  arithmetic     (26)
  bytevectors    (10)
  characters     (13)
  ...
```

**`,topic arithmetic`:**
```
arithmetic (26 procedures):
  +              Returns the sum of its arguments
  -              Returns the difference of its arguments
  ...
```

### Scheme Primitive Data Flow

The Scheme primitives need access to the registry. Two paths:

1. **Registry on Namespace** — `env.Namespace().Registry()` returns `any`,
   type-assert to `*registry.Registry`. Already stored there.
2. **Direct registry access** — primitives receive `MachineContext`, which has
   `Env()` → `Namespace()` → `Registry()`.

Path 1 is the right one: it uses existing infrastructure with no new fields.

### Binding Search (apropos beyond primitives)

Apropos also searches phase environment bindings (special forms, macros, user
defines). The walk:

```
namespace.Phases().Phases() → for each phase:
    phaseEnv.Global().Keys() → for each symbol:
        binding := phaseEnv.Global().GetOwnGlobalBinding(gi)
        match name against pattern
        match binding.Doc() against pattern
        match callableDoc(binding.Value()) against pattern
```

This catches `if`, `define-syntax`, user-defined macros, etc. Results merge
with registry results, deduplicated by name (registry wins on conflict since
it has richer metadata).

## Non-Goals

- Regex patterns (substring is sufficient; regex can be added later)
- "See also" cross-references (no data source for these yet)
- Fuzzy/typo-tolerant matching
- Library-level documentation browsing

## Out of Scope for This Design

- Modifying `PrimitiveSpec` or adding new metadata fields
- Changes to the `environment` package
- Changes to the compiler or validator

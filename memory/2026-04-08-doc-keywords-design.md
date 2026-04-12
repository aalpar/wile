# Documentation Keywords

**Status:** Complete
**Current-state doc:** `plans/DOCUMENTATION-SEARCH.md`

Add a `Keywords` field to the documentation system so `apropos` and `,apropos`
can match on curated tags rather than relying on name/prose/category substring hits.

## Motivation

`(apropos "sort")` today returns nothing because no primitive is named "sort" and
no docstring happens to contain the word. A `Keywords: sort, ordering, comparison`
annotation on `list-sort` (or future sorting primitives) would surface it.

## Docstring Format

New metadata header, same convention as `Category:`:

```
Keywords: sort, ordering, comparison
```

Comma-separated, single line. Extracted from prose by `docparse.ParseDocstring`
and stored as `[]string`. Trimmed, lowercased at search time (not at parse time).

## Touch Points

| # | File | Change |
|---|------|--------|
| 1 | `docparse/docparse.go` | Add `Keywords []string` to `DocInfo`. Recognize `Keywords:` as metadata header. Split on `,`, trim whitespace. Add to `HasStructuredMetadata`. |
| 2 | `docparse/docparse_test.go` | Parse tests: keywords present, absent, single keyword, whitespace variants. |
| 3 | `registry/registry.go` | Add `Keywords []string` to `PrimitiveSpec`. |
| 4 | `repl/doc_provider.go` | Add `Keywords []string` to `DocInfo` and `DocSearchResult`. |
| 5 | `repl/registry_doc_provider.go` | Populate `Keywords` in `LookupDoc` (from spec), `Search`/`nonPrimitiveDocs` (from parsed doc). Add keywords to `matchesFields`. |
| 6 | `registry/core/prim_reflection.go` | `PrimApropos`: add keyword match (any keyword contains pattern). |
| 7 | `repl/meta.go` | Display `Keywords:` line in `formatPrimitiveDoc`. Populate keywords in `tryStructuredBindingDoc` and `searchBindings`. |

## Search Semantics

Boolean substring match: if the pattern is a substring of any keyword, the entry
matches. Same as existing name/doc/category matching. No ranking, no change to
result ordering (alphabetical by name).

## Display

`,doc` output gains a line after Category:

```
  Category: lists
  Keywords: sort, ordering, comparison
```

Only shown when keywords are non-empty.

## Design Decisions

- **No interface.** Three structs share fields by convention. An interface was
  considered but rejected: it doesn't catch the actual failure mode (forgetting
  to populate a new field), and there are no polymorphic consumers.
- **Struct field on PrimitiveSpec takes precedence** over docstring-parsed keywords
  for primitives, since `LookupDoc` reads struct fields directly for primitives
  and only parses docstrings for non-primitives. Same pattern as `Category`.
- **No merge of struct + parsed keywords.** If `PrimitiveSpec.Keywords` is set,
  that's authoritative. If it's nil, docstring parsing doesn't apply (for
  primitives). This matches how `Category` works today.

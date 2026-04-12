# Plan: Filter Examples from Documentation Display

## Problem

PR #589 added examples to all 573 primitive and library docstrings (~49 KB of
example text, 41% of total doc content). Examples are always displayed by default
in `,doc`, `procedure-documentation`, and the MCP `doc` tool. This adds noise
when users want a quick reference.

## Decision

**Keep examples in source, filter at display time.** The delimiter
`\n\nExamples:\n` is used uniformly across all 573 docstrings (Go `Doc:` fields
and Scheme stdlib docstrings). Zero exceptions — parsing is unambiguous via
`strings.SplitN(doc, "\n\nExamples:\n", 2)`.

This preserves examples as a resource without forcing them on every lookup.

## Design

### Behavior Change

| Surface | Current | After |
|---------|---------|-------|
| `,doc car` | Shows description + examples | Shows description only |
| `,doc -x car` | N/A | Shows description + examples |
| `(procedure-documentation car)` | Full string (description + examples) | **Unchanged** — full string |
| MCP `doc` tool | Shows description + examples | Shows description only |
| MCP `doc` tool (with examples param) | N/A | Shows description + examples |

**Rationale:** `procedure-documentation` is a data accessor — it returns the raw
docstring. Filtering belongs in presentation layers (REPL commands, MCP tools),
not in the data model. Users who call `procedure-documentation` programmatically
may want examples for their own formatting.

### Implementation

#### Phase 1: Add `StripExamples` helper

**File:** `internal/repl/doc_format.go` (new, or add to existing formatting code)

```go
// StripExamples removes the Examples: section from a docstring.
// Returns the description portion only. If no Examples: section
// exists, returns the original string unchanged.
func StripExamples(doc string) string {
    before, _, found := strings.Cut(doc, "\n\nExamples:\n")
    if !found {
        return doc
    }
    return before
}

// SplitExamples splits a docstring into (description, examples, hasExamples).
// The examples portion does NOT include the "Examples:\n" header.
func SplitExamples(doc string) (description, examples string, hasExamples bool) {
    before, after, found := strings.Cut(doc, "\n\nExamples:\n")
    return before, after, found
}
```

Lives in `internal/repl/` alongside the existing formatting code. The MCP
server already delegates through the REPL meta command path (`runMeta`).

#### Phase 2: Update `,doc` formatting

**File:** `internal/repl/meta.go`

In `formatPrimitiveDoc`, apply `StripExamples` to `info.Doc` before writing.
Add a flag parameter or use a format options struct to control whether examples
are included.

For the REPL command parsing in `cmdDoc`, recognize `-x` (or `--examples`) as
a flag:

```
,doc car        → description only
,doc -x car     → description + examples
```

Update the help text for `,doc` to mention the `-x` flag.

**For `formatBindingDoc`** (user-defined and Scheme-library procedures): apply
the same `StripExamples` to the doc field when rendering.

#### Phase 3: Update MCP `doc` tool

**File:** `cmd/wile/mcp.go`

Add an optional boolean parameter `examples` to the `doc` tool:

```go
mcp.WithBoolean("examples",
    mcp.Description("Include usage examples in the output (default: false)"),
),
```

When `examples` is false (default), the output strips examples.
When true, the full docstring is shown.

The MCP tool currently delegates to `,doc` via `runMeta`. Two options:
1. Pass `,doc -x name` when examples=true → simplest, reuses REPL path
2. Call doc provider directly → more control, but duplicates formatting

→ **Recommendation:** Option 1. Just conditionally prepend `-x`.

#### Phase 4: Update `,doc` help entry

**File:** `internal/repl/meta.go`

Update the help text for the `doc` command to document the `-x` flag.

## What Does NOT Change

- `procedure-documentation` primitive — returns full raw docstring
- Docstrings in source code — examples stay in `Doc:` fields and Scheme files
- `apropos` — unchanged (searches full doc text including examples)
- Binary size — unchanged (examples still compiled in)

## Testing

1. Unit test for `StripExamples` / `SplitExamples` — docstring with examples,
   without examples, empty string
2. Meta command test: `,doc car` output does NOT contain `Examples:`
3. Meta command test: `,doc -x car` output DOES contain `Examples:`
4. `procedure-documentation` test: still returns full docstring with examples
5. MCP test: `doc` tool without `examples` param strips examples
6. MCP test: `doc` tool with `examples: true` includes examples

## Estimated Size

~50 lines of production code, ~80 lines of tests. Small, focused change.

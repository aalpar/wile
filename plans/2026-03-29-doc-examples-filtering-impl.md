# Implementation: Filter Examples from Documentation Display

Design: `plans/2026-03-29-doc-examples-filtering.md`

## Task 1: Add `StripExamples` helper to `internal/repl/`

**File:** `internal/repl/doc_provider.go`

Add two functions after the `DocSearchProvider` interface (line 41):

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
```

Add `"strings"` to the import block.

**Why `doc_provider.go`:** This file defines `DocInfo` and is already the
shared vocabulary for doc formatting. Both `meta.go` and the MCP server
(via `runMeta` → `Handle`) flow through this package.

**No `SplitExamples` yet.** YAGNI — nothing needs the examples portion
separately. Add it when a consumer exists.

## Task 2: Add `showExamples` parameter to `formatPrimitiveDoc`

**File:** `internal/repl/meta.go`

Change the signature of `formatPrimitiveDoc` (line 331):

```go
// Before:
func formatPrimitiveDoc(w *strings.Builder, name string, info DocInfo)

// After:
func formatPrimitiveDoc(w *strings.Builder, name string, info DocInfo, showExamples bool)
```

In the function body, at line 349–351 where `info.Doc` is written:

```go
// Before:
if info.Doc != "" {
	fmt.Fprintf(w, "  %s\n", info.Doc)
}

// After:
doc := info.Doc
if !showExamples {
	doc = StripExamples(doc)
}
if doc != "" {
	fmt.Fprintf(w, "  %s\n", doc)
}
```

## Task 3: Add `showExamples` parameter to `formatBindingDoc`

**File:** `internal/repl/meta.go`

Change the signature of `formatBindingDoc` (line 385):

```go
// Before:
func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int)

// After:
func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int, showExamples bool)
```

At lines 409–412 where the doc is written, apply `StripExamples`:

```go
// Before:
if doc != "" {
	indented := strings.ReplaceAll(doc, "\n", "\n  ")
	fmt.Fprintf(w, "\n  %s\n", indented)
}

// After:
if doc != "" {
	if !showExamples {
		doc = StripExamples(doc)
	}
	indented := strings.ReplaceAll(doc, "\n", "\n  ")
	fmt.Fprintf(w, "\n  %s\n", indented)
}
```

## Task 4: Parse `-x` flag in `cmdDoc` and thread `showExamples`

**File:** `internal/repl/meta.go`

In `cmdDoc` (line 204), after the empty-args guard, parse the `-x` flag
from `args` before processing:

```go
func (p *MetaCommandHandler) cmdDoc(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,doc [-x] <name> or ,doc (<library-name>)")
		return
	}

	showExamples := false
	if args[0] == "-x" {
		showExamples = true
		args = args[1:]
		if len(args) == 0 {
			fmt.Fprintln(out, "Usage: ,doc [-x] <name> or ,doc (<library-name>)")
			return
		}
	}

	// ... rest of function unchanged except:
	// - All calls to formatPrimitiveDoc get showExamples appended
	// - All calls to formatBindingDoc get showExamples appended
```

There are four call sites to update inside `cmdDoc`:
1. Line 245: `formatPrimitiveDoc(&content, name, info)` → add `, showExamples`
2. Line 251: `formatBindingDoc(&content, name, bnd, phase)` → add `, showExamples`
3. Line 263: `formatPrimitiveDoc(&content, name, info)` → add `, showExamples`

(Line numbers are current values; adjust for the inserted flag-parsing block.)

## Task 5: Update `,doc` help text

**File:** `internal/repl/meta.go`

In the `metaCommands` slice (line 125), update the `doc` entry:

```go
// Before:
{"doc", nil, "Show documentation for a Scheme binding or library",
	"Usage: ,doc <name> or ,doc (<library-name>)\n\n...",
	"session"},

// After:
{"doc", nil, "Show documentation for a Scheme binding or library",
	"Usage: ,doc [-x] <name> or ,doc (<library-name>)\n\n" +
		"Looks up the named binding across all phase environments\n" +
		"(runtime, expand, compile) and displays documentation.\n" +
		"For primitives, shows signature, description, and category.\n" +
		"For user bindings, shows type and current value.\n" +
		"For libraries, shows description, source, and export list.\n\n" +
		"Options:\n  -x    Include usage examples in the output",
	"session"},
```

## Task 6: Update MCP `doc` tool with `examples` parameter

**File:** `cmd/wile/mcp.go`

Add a boolean parameter to the `doc` tool definition (after line 103):

```go
mcp.WithBoolean("examples",
	mcp.Description("Include usage examples in the output (default: false)"),
),
```

Update `handleDoc` (line 340) to read the flag and conditionally pass `-x`:

```go
func (p *mcpServer) handleDoc(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	name := req.GetString("name", "")
	if name == "" {
		return mcp.NewToolResultError("name parameter is required"), nil
	}
	cmd := ",doc " + name
	if req.GetBool("examples", false) {
		cmd = ",doc -x " + name
	}
	return p.runMeta(ctx, cmd)
}
```

## Task 7: Tests

### 7a: Unit test for `StripExamples`

**File:** `internal/repl/meta_test.go` (or `doc_provider_test.go` — whichever
already has the package-level test infrastructure)

Table-driven:

| Name | Input | Expected |
|------|-------|----------|
| with examples | `"Desc.\n\nExamples:\n  (f 1) => 2"` | `"Desc."` |
| without examples | `"Just a description."` | `"Just a description."` |
| empty string | `""` | `""` |
| examples at start (pathological) | `"\n\nExamples:\n  (f)"` | `""` |

### 7b: `,doc` strips examples by default

**File:** `internal/repl/meta_test.go`

Extend `TestCmdDoc` table with a case:

```go
{"primitive strips examples by default", []string{"car"}, "pair"},
// And a negative assertion: output should NOT contain "Examples:"
```

Add a separate test or extend the table to verify:
- `,doc car` → output does NOT contain `"Examples:"`
- `,doc -x car` → output DOES contain `"Examples:"`

### 7c: `procedure-documentation` unchanged

**File:** `registry/core/prim_reflection_test.go`

Verify that `(procedure-documentation car)` still returns the full string
including examples. There's likely an existing test — add an assertion that
the result contains `"Examples:"` if not already checked.

### 7d: MCP `doc` tool tests

**File:** `cmd/wile/mcp_test.go`

Extend `TestHandleDoc_KnownBinding`:
- Default (no `examples` param): output should NOT contain `"Examples:"`
- With `examples: true`: output DOES contain `"Examples:"`

## Build Order

Tasks 1–5 are sequential (each depends on the previous signature change).
Task 6 is independent of 2–5 (only depends on 1 existing and the `,doc -x`
flag working via `runMeta`).
Task 7 can be written alongside each task.

Recommended execution: Tasks 1 → 2 → 3 → 4 → 5 → 6 → 7, linear.
Run `make lint && make test` after task 6, before writing tests.

## Verification

After all tasks: `make lint && make covercheck` must pass.

Spot-check:
- `./dist/wile` → `,doc car` → no examples shown
- `./dist/wile` → `,doc -x car` → examples shown
- `./dist/wile` → `(procedure-documentation car)` → full string with examples

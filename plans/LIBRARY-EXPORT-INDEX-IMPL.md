# Library Export Index Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `apropos` discover procedures in unloaded libraries by building a static export index from `.sld` files.

**Architecture:** New `LibraryExportIndex` type in `machine/compilation/` parses `.sld` export lists without compiling. `SearchDoc` queries the index as a 6th search source. The index is built lazily on first search via `sync.Once` in `RegistryDocProvider`.

**Tech Stack:** Go standard library, existing `internal/parser` and `internal/syntax` packages for `.sld` parsing.

**Design doc:** `plans/LIBRARY-EXPORT-INDEX.md`

---

### Task 1: `ParseLibrarySummary` — static `.sld` parsing

**Files:**
- Create: `machine/compilation/library_export_index.go`
- Test: `machine/compilation/library_export_index_test.go`

**Step 1: Write the types and function stub**

Create `machine/compilation/library_export_index.go`:

```go
package compilation

import (
	"bufio"
	"context"
	"io"

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// LibrarySummary holds statically-parsed metadata from an .sld file.
// Built without compiling or executing the library.
type LibrarySummary struct {
	Name        LibraryName
	Description string
	Exports     []string
	SourceFile  string
}

// LibraryExportIndex maps library keys to their summaries.
// Immutable after construction.
type LibraryExportIndex struct {
	entries map[string]*LibrarySummary
}

// ParseLibrarySummary parses an .sld file and extracts export names and
// description without compiling or executing the library.
func ParseLibrarySummary(ctx context.Context, r io.Reader, filePath string, name LibraryName) (*LibrarySummary, error) {
	reader := bufio.NewReader(r)
	p := parser.NewParserWithFile(nil, false, reader, filePath)

	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"ParseLibrarySummary: could not parse %s", filePath)
	}

	pair, ok := stx.(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"ParseLibrarySummary: expected define-library form in %s", filePath)
	}

	carStx := pair.SyntaxCar()
	carSym, ok := carStx.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"ParseLibrarySummary: expected define-library keyword in %s", filePath)
	}
	symName := carSym.Unwrap().(*values.Symbol).Key
	if symName != "define-library" && symName != "library" {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"ParseLibrarySummary: expected define-library, got %s in %s", symName, filePath)
	}

	// Skip the library name (second element), walk declarations (rest).
	cdr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"ParseLibrarySummary: malformed define-library in %s", filePath)
	}
	// cdr.SyntaxCar() is the library name; skip it.
	declsVal := cdr.SyntaxCdr()

	q := &LibrarySummary{
		Name:       name,
		SourceFile: filePath,
	}

	// Walk declarations.
	declsPair, ok := declsVal.(*syntax.SyntaxPair)
	if !ok {
		// No declarations — valid but empty library.
		return q, nil
	}
	_, err = syntax.SyntaxForEach(ctx, declsPair, func(_ context.Context, _ int, _ bool, decl syntax.SyntaxValue) error {
		return parseSummaryDeclaration(ctx, q, decl)
	})
	if err != nil {
		return nil, err
	}

	return q, nil
}

// parseSummaryDeclaration extracts export and description info from one
// library declaration. All other declaration types are silently skipped.
func parseSummaryDeclaration(ctx context.Context, summary *LibrarySummary, decl syntax.SyntaxValue) error {
	pair, ok := decl.(*syntax.SyntaxPair)
	if !ok {
		return nil // skip non-pair declarations (comments, etc.)
	}

	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return nil // skip unrecognized forms
	}

	keyword := carSym.Unwrap().(*values.Symbol).Key
	args := pair.SyntaxCdr()

	switch keyword {
	case "export":
		return parseSummaryExports(ctx, summary, args)
	case "description":
		return parseSummaryDescription(summary, args)
	default:
		return nil // skip import, begin, include, cond-expand, etc.
	}
}

// parseSummaryExports extracts export names from (export <spec> ...).
func parseSummaryExports(ctx context.Context, summary *LibrarySummary, args syntax.SyntaxValue) error {
	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return nil // empty export is valid
	}
	_, err := syntax.SyntaxForEach(ctx, argsPair, func(_ context.Context, _ int, _ bool, spec syntax.SyntaxValue) error {
		return parseSummaryExportSpec(summary, spec)
	})
	return err
}

// parseSummaryExportSpec extracts the external name from one export spec.
func parseSummaryExportSpec(summary *LibrarySummary, spec syntax.SyntaxValue) error {
	switch s := spec.(type) {
	case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
		return nil

	case *syntax.SyntaxSymbol:
		summary.Exports = append(summary.Exports, s.Unwrap().(*values.Symbol).Key)
		return nil

	case *syntax.SyntaxPair:
		// (rename internal external) — we want the external name.
		carSym, ok := s.SyntaxCar().(*syntax.SyntaxSymbol)
		if !ok {
			return nil // skip malformed
		}
		if carSym.Unwrap().(*values.Symbol).Key != "rename" {
			return nil // skip unrecognized forms
		}
		// Walk to the external name: (rename internal external)
		cdr, ok := s.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok {
			return nil
		}
		cddr, ok := cdr.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok {
			return nil
		}
		extSym, ok := cddr.SyntaxCar().(*syntax.SyntaxSymbol)
		if !ok {
			return nil
		}
		summary.Exports = append(summary.Exports, extSym.Unwrap().(*values.Symbol).Key)
		return nil

	default:
		return nil // skip unrecognized
	}
}

// parseSummaryDescription extracts the description string.
func parseSummaryDescription(summary *LibrarySummary, args syntax.SyntaxValue) error {
	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return nil
	}
	str, ok := argsPair.SyntaxCar().UnwrapAll().(*values.String)
	if !ok {
		return nil
	}
	summary.Description = str.Value
	return nil
}
```

**Step 2: Write tests for `ParseLibrarySummary`**

Create `machine/compilation/library_export_index_test.go`:

```go
package compilation

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestParseSummary_SimpleLibrary(t *testing.T) {
	src := `(define-library (test simple)
  (description "A test library.")
  (export foo bar)
  (import (scheme base))
  (begin (define foo 1) (define bar 2)))`
	r := strings.NewReader(src)
	name := NewLibraryName("test", "simple")

	summary, err := ParseLibrarySummary(context.Background(), r, "test/simple.sld", name)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, summary.Description, qt.Equals, "A test library.")
	qt.Assert(t, summary.Exports, qt.DeepEquals, []string{"foo", "bar"})
	qt.Assert(t, summary.SourceFile, qt.Equals, "test/simple.sld")
}

func TestParseSummary_RenameExport(t *testing.T) {
	src := `(define-library (test rename)
  (export (rename internal-name public-name) plain))`
	r := strings.NewReader(src)
	name := NewLibraryName("test", "rename")

	summary, err := ParseLibrarySummary(context.Background(), r, "test/rename.sld", name)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, summary.Exports, qt.DeepEquals, []string{"public-name", "plain"})
}

func TestParseSummary_NoDescription(t *testing.T) {
	src := `(define-library (test nodesc)
  (export a b c))`
	r := strings.NewReader(src)
	name := NewLibraryName("test", "nodesc")

	summary, err := ParseLibrarySummary(context.Background(), r, "test/nodesc.sld", name)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, summary.Description, qt.Equals, "")
	qt.Assert(t, summary.Exports, qt.DeepEquals, []string{"a", "b", "c"})
}

func TestParseSummary_EmptyLibrary(t *testing.T) {
	src := `(define-library (test empty))`
	r := strings.NewReader(src)
	name := NewLibraryName("test", "empty")

	summary, err := ParseLibrarySummary(context.Background(), r, "test/empty.sld", name)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, len(summary.Exports), qt.Equals, 0)
}

func TestParseSummary_LibraryKeyword(t *testing.T) {
	src := `(library (test alt)
  (export x)
  (description "Alt keyword."))`
	r := strings.NewReader(src)
	name := NewLibraryName("test", "alt")

	summary, err := ParseLibrarySummary(context.Background(), r, "test/alt.sld", name)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, summary.Exports, qt.DeepEquals, []string{"x"})
	qt.Assert(t, summary.Description, qt.Equals, "Alt keyword.")
}

func TestParseSummary_MultipleExportClauses(t *testing.T) {
	src := `(define-library (test multi)
  (export a b)
  (export c d))`
	r := strings.NewReader(src)
	name := NewLibraryName("test", "multi")

	summary, err := ParseLibrarySummary(context.Background(), r, "test/multi.sld", name)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, summary.Exports, qt.DeepEquals, []string{"a", "b", "c", "d"})
}

func TestParseSummary_NotALibrary(t *testing.T) {
	src := `(define x 42)`
	r := strings.NewReader(src)
	name := NewLibraryName("test", "bad")

	_, err := ParseLibrarySummary(context.Background(), r, "test/bad.sld", name)
	qt.Assert(t, err, qt.IsNotNil)
}
```

**Step 3: Run tests**

Run: `go test -v -run TestParseSummary ./machine/compilation/...`
Expected: All pass.

**Step 4: Verify against a real `.sld` file**

Write a test that parses `stdlib/lib/scheme/char.sld` from the repo:

```go
func TestParseSummary_RealSldFile(t *testing.T) {
	f, err := os.Open("../../stdlib/lib/scheme/char.sld")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()

	name := NewLibraryName("scheme", "char")
	summary, err := ParseLibrarySummary(context.Background(), f, "scheme/char.sld", name)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, summary.Description, qt.Not(qt.Equals), "")
	qt.Assert(t, len(summary.Exports) > 0, qt.IsTrue)

	// Spot-check known exports.
	exports := make(map[string]bool)
	for _, e := range summary.Exports {
		exports[e] = true
	}
	qt.Assert(t, exports["char-upcase"], qt.IsTrue)
	qt.Assert(t, exports["string-downcase"], qt.IsTrue)
}
```

Run: `go test -v -run TestParseSummary_RealSldFile ./machine/compilation/...`
Expected: PASS.

**Step 5: Commit**

```
feat(compilation): add ParseLibrarySummary for static .sld parsing

Parses export names and description from .sld files without compiling
or executing the library. Foundation for unloaded library discovery
in apropos.
```

---

### Task 2: `BuildExportIndex` — scan and index all discoverable libraries

**Files:**
- Modify: `machine/compilation/library_export_index.go`
- Test: `machine/compilation/library_export_index_test.go`

**Step 1: Add `BuildExportIndex` and index query methods**

Append to `machine/compilation/library_export_index.go`:

```go
import (
	"errors"
	"sort"
)

// BuildExportIndex scans all discoverable .sld files via the resolver,
// parses export lists and descriptions, and returns a searchable index.
// Libraries already in reg are skipped.
//
// Parse errors for individual files are skipped silently — one bad
// file must not poison the entire index.
func BuildExportIndex(ctx context.Context, resolver FileResolver, reg *LibraryRegistry) (*LibraryExportIndex, error) {
	enumerator, ok := resolver.(LibraryEnumerator)
	if !ok {
		return &LibraryExportIndex{entries: make(map[string]*LibrarySummary)}, nil
	}

	libs, err := enumerator.EnumerateLibraries()
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "BuildExportIndex: enumerate libraries")
	}

	entries := make(map[string]*LibrarySummary, len(libs))
	for _, name := range libs {
		if reg != nil && reg.Lookup(name) != nil {
			continue // already loaded
		}

		summary := tryParseLibrary(ctx, resolver, name)
		if summary == nil {
			continue // skip on error
		}
		entries[name.Key()] = summary
	}

	return &LibraryExportIndex{entries: entries}, nil
}

// tryParseLibrary opens and parses a single library file, returning nil
// on any error. Tries .sld first, then .scm.
func tryParseLibrary(ctx context.Context, resolver FileResolver, name LibraryName) *LibrarySummary {
	sldPath := name.ToFSPath()
	f, filePath, err := resolver.ResolveAndOpen(ctx, sldPath)
	if err != nil {
		if !errors.Is(err, werr.ErrFileNotFound) {
			return nil
		}
		scmPath := strings.TrimSuffix(sldPath, ".sld") + ".scm"
		f, filePath, err = resolver.ResolveAndOpen(ctx, scmPath)
		if err != nil {
			return nil
		}
	}
	defer f.Close() //nolint:errcheck

	summary, err := ParseLibrarySummary(ctx, f, filePath, name)
	if err != nil {
		return nil
	}
	return summary
}

// Lookup returns the summary for a library, or nil if not indexed.
func (p *LibraryExportIndex) Lookup(name LibraryName) *LibrarySummary {
	if p == nil {
		return nil
	}
	return p.entries[name.Key()]
}

// Entries returns all indexed summaries sorted by library key.
func (p *LibraryExportIndex) Entries() []*LibrarySummary {
	if p == nil {
		return nil
	}
	q := make([]*LibrarySummary, 0, len(p.entries))
	for _, s := range p.entries {
		q = append(q, s)
	}
	sort.Slice(q, func(i, j int) bool {
		return q[i].Name.Key() < q[j].Name.Key()
	})
	return q
}
```

Note: add `"strings"` to the import block.

**Step 2: Write tests for `BuildExportIndex`**

Use `FSFileResolver` with `fstest.MapFS` to create a virtual filesystem:

```go
func TestBuildExportIndex(t *testing.T) {
	mapFS := fstest.MapFS{
		"test/simple.sld": &fstest.MapFile{
			Data: []byte(`(define-library (test simple)
  (description "Simple lib.")
  (export foo bar))`),
		},
		"test/other.sld": &fstest.MapFile{
			Data: []byte(`(define-library (test other)
  (export baz))`),
		},
	}

	resolver := NewFSFileResolver(mapFS)
	reg := NewLibraryRegistry(nil)

	idx, err := BuildExportIndex(context.Background(), resolver, reg)
	qt.Assert(t, err, qt.IsNil)

	entries := idx.Entries()
	qt.Assert(t, len(entries), qt.Equals, 2)
}

func TestBuildExportIndex_SkipsLoadedLibraries(t *testing.T) {
	mapFS := fstest.MapFS{
		"test/loaded.sld": &fstest.MapFile{
			Data: []byte(`(define-library (test loaded)
  (export x))`),
		},
	}

	resolver := NewFSFileResolver(mapFS)
	reg := NewLibraryRegistry(nil)
	// Simulate a loaded library.
	lib := &CompiledLibrary{Name: NewLibraryName("test", "loaded")}
	_ = reg.Register(lib)

	idx, err := BuildExportIndex(context.Background(), resolver, reg)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, len(idx.Entries()), qt.Equals, 0)
}

func TestBuildExportIndex_SkipsMalformedFiles(t *testing.T) {
	mapFS := fstest.MapFS{
		"test/good.sld": &fstest.MapFile{
			Data: []byte(`(define-library (test good) (export ok))`),
		},
		"test/bad.sld": &fstest.MapFile{
			Data: []byte(`not valid scheme`),
		},
	}

	resolver := NewFSFileResolver(mapFS)
	reg := NewLibraryRegistry(nil)

	idx, err := BuildExportIndex(context.Background(), resolver, reg)
	qt.Assert(t, err, qt.IsNil)
	// Only the good library should be indexed.
	qt.Assert(t, len(idx.Entries()), qt.Equals, 1)
	qt.Assert(t, idx.Entries()[0].Exports, qt.DeepEquals, []string{"ok"})
}
```

Note: adjust import paths and constructor signatures to match the actual codebase.
Check `NewFSFileResolver` signature and `NewLibraryRegistry` constructor before writing.
The `FSFileResolver` may need search paths or a different constructor.
Check `Register` method signature on `LibraryRegistry`.

**Step 3: Run tests**

Run: `go test -v -run TestBuildExportIndex ./machine/compilation/...`
Expected: All pass.

**Step 4: Commit**

```
feat(compilation): add BuildExportIndex for library discovery

Scans all discoverable .sld files via LibraryEnumerator, parses
export lists, and builds an in-memory index. Skips already-loaded
libraries and silently ignores malformed files.
```

---

### Task 3: Integrate with `SearchDoc`

**Files:**
- Modify: `registry/search.go` (lines 59, 101-116)
- Modify: `registry/search_test.go`
- Modify: `registry/core/prim_reflection.go` (line 353)

**Step 1: Add `searchUnloadedExports` function**

Add to `registry/search.go`:

```go
import "fmt"

// searchUnloadedExports searches the export index for matches, skipping
// libraries that are now loaded.
func searchUnloadedExports(idx *compilation.LibraryExportIndex, libReg *compilation.LibraryRegistry, lowerPattern string) []DocSearchResult {
	if idx == nil {
		return nil
	}
	var q []DocSearchResult
	for _, summary := range idx.Entries() {
		// Skip if library was imported after index was built.
		if libReg != nil && libReg.Lookup(summary.Name) != nil {
			continue
		}
		libLabel := summary.Name.SchemeString()
		for _, export := range summary.Exports {
			if strings.Contains(strings.ToLower(export), lowerPattern) {
				doc := libLabel
				if summary.Description != "" {
					doc = fmt.Sprintf("%s — %s", libLabel, summary.Description)
				}
				q = append(q, DocSearchResult{
					Name:     export,
					Doc:      doc,
					Category: "not imported",
				})
			}
		}
	}
	return q
}
```

**Step 2: Update `SearchDoc` signature and body**

Change the signature at `search.go:59`:

```go
func SearchDoc(reg *Registry, env *environment.EnvironmentFrame,
	libReg *compilation.LibraryRegistry,
	exportIndex *compilation.LibraryExportIndex,
	pattern string) []DocSearchResult {
```

Update the doc comment to mention the 6th source.

Add step 6 after the loaded libraries block (after line 110):

```go
	// 6. Unloaded library exports.
	if exportIndex != nil {
		for _, r := range searchUnloadedExports(exportIndex, libReg, lowerPattern) {
			if seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			q = append(q, r)
		}
	}
```

**Step 3: Fix callers**

1. `repl/registry_doc_provider.go:107` — will be updated in Task 4, pass `nil` for now:

```go
return registry.SearchDoc(p.reg, p.env, p.libReg, nil, pattern)
```

2. `registry/core/prim_reflection.go:353` — the Scheme-level `(apropos)` primitive.
   This caller doesn't have access to the export index. Pass `nil`:

```go
results := registry.SearchDoc(reg, env, registry.ExtractLibraryRegistry(env), nil, s.Value)
```

Note: The Scheme-level `(apropos)` won't search unloaded libraries — only the MCP
`apropos` tool will, via `RegistryDocProvider`. This is acceptable: the Scheme primitive
runs inside an engine where `(import ...)` is the intended discovery mechanism. The MCP
tool is the LLM-facing interface where the gap matters.

**Step 4: Fix existing tests**

All existing `SearchDoc` calls in `registry/search_test.go` need the new parameter.
Add `nil` as the 4th argument to every `registry.SearchDoc(reg, env, libReg, pattern)`
call, changing to `registry.SearchDoc(reg, env, libReg, nil, pattern)`.

**Step 5: Write test for unloaded export search**

Add to `registry/search_test.go`:

```go
func TestSearchDoc_UnloadedExports(t *testing.T) {
	reg := registry.NewRegistry()
	// Build a minimal export index.
	idx := compilation.NewLibraryExportIndexFromEntries(map[string]*compilation.LibrarySummary{
		"srfi/1": {
			Name:        compilation.NewLibraryName("srfi", "1"),
			Description: "SRFI 1: List library.",
			Exports:     []string{"fold", "unfold", "partition"},
		},
	})

	results := registry.SearchDoc(reg, nil, nil, idx, "fold")
	qt.Assert(t, len(results), qt.Equals, 1)
	qt.Assert(t, results[0].Name, qt.Equals, "fold")
	qt.Assert(t, results[0].Category, qt.Equals, "not imported")
	qt.Assert(t, strings.Contains(results[0].Doc, "(srfi 1)"), qt.IsTrue)
}
```

Note: This test needs a `NewLibraryExportIndexFromEntries` constructor. Add it
to `library_export_index.go`:

```go
// NewLibraryExportIndexFromEntries creates an index from pre-built entries.
// Intended for testing.
func NewLibraryExportIndexFromEntries(entries map[string]*LibrarySummary) *LibraryExportIndex {
	return &LibraryExportIndex{entries: entries}
}
```

**Step 6: Run all tests**

Run: `go test -v ./registry/... ./machine/compilation/...`
Expected: All pass.

**Step 7: Run lint**

Run: `make lint`
Expected: Clean.

**Step 8: Commit**

```
feat(registry): SearchDoc searches unloaded library exports

Adds a 6th search source to SearchDoc: the LibraryExportIndex.
Export names from unloaded libraries are matched by name and
returned with category "not imported" and the source library
in the doc field.
```

---

### Task 4: Lazy index in `RegistryDocProvider`

**Files:**
- Modify: `repl/registry_doc_provider.go` (lines 27-31, 106-108)

**Step 1: Add lazy index fields**

Update the struct at `registry_doc_provider.go:27-31`:

```go
type RegistryDocProvider struct {
	reg    *registry.Registry
	env    *environment.EnvironmentFrame
	libReg *compilation.LibraryRegistry

	indexOnce   sync.Once
	exportIndex *compilation.LibraryExportIndex
}
```

Add `"sync"` to imports.

**Step 2: Update `Search()` to build index lazily**

Replace `registry_doc_provider.go:106-108`:

```go
func (p *RegistryDocProvider) Search(pattern string) []registry.DocSearchResult {
	p.indexOnce.Do(func() {
		if p.env == nil {
			return
		}
		resolver := p.env.FileResolver()
		if resolver == nil {
			return
		}
		p.exportIndex, _ = compilation.BuildExportIndex(
			context.Background(), resolver, p.libReg)
	})
	return registry.SearchDoc(p.reg, p.env, p.libReg, p.exportIndex, pattern)
}
```

Add `"context"` to imports.

**Step 3: Run tests**

Run: `go test -v ./repl/...`
Expected: All pass.

**Step 4: Run full test suite**

Run: `make test`
Expected: All pass.

**Step 5: Run lint**

Run: `make lint`
Expected: Clean.

**Step 6: Commit**

```
feat(repl): lazy export index in RegistryDocProvider

Builds the LibraryExportIndex on first Search() call via sync.Once.
The MCP apropos tool now discovers procedures in unloaded libraries
without any changes to mcp.go or the tool handlers.
```

---

### Task 5: End-to-end verification

**Files:**
- No code changes — verification only.

**Step 1: Run the full test suite**

Run: `make lint && make test`
Expected: All clean.

**Step 2: Manual MCP verification**

Start the MCP server and verify the feature works:

1. Call `apropos` with `"fold"` — should now return `fold [not imported] (srfi 1) — SRFI 1: ...`
2. Call `apropos` with `"char-upcase"` — should find it in loaded `(scheme char)` if imported, or in unloaded export index
3. Call `apropos` with `"xyznonexistent"` — should return no matches
4. Call `(import (srfi 1))` then `apropos "fold"` — should now show `fold` from the loaded library (not "not imported")

**Step 3: Commit if any fixups needed**

---

### Task 6: Covercheck

**Step 1: Run covercheck**

Run: `make covercheck`
Expected: Coverage meets project threshold.

If coverage is below threshold, add targeted tests for uncovered branches
(error paths in `ParseLibrarySummary`, edge cases in `BuildExportIndex`).

**Step 2: Final commit if needed**

```
test(compilation): improve coverage for library export index
```

# Unified Apropos Search Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Consolidate the Scheme `(apropos)` and REPL `,apropos` into a single search function so both produce identical results.

**Architecture:** Create `registry.SearchDoc` as the single search function. `PrimApropos` and `RegistryDocProvider.Search` both delegate to it. Move `DocSearchResult` to `registry/`, delete duplicated search logic from `repl/`.

**Design doc:** `plans/2026-04-08-unified-apropos-design.md`

---

### Task 1: Create `registry/search.go` with `SearchDoc`

**Files:**
- Create: `registry/search.go`
- Create: `registry/search_test.go`

**Step 1: Write tests for `SearchDoc`**

Create `registry/search_test.go`:

```go
package registry_test

import (
    "slices"
    "testing"

    qt "github.com/frankban/quicktest"

    "github.com/aalpar/wile/registry"
)

func buildSearchTestRegistry() *registry.Registry {
    reg := registry.NewRegistry()
    reg.AddPrimitives([]registry.PrimitiveSpec{
        {
            Name:     "string-append",
            ParamCount: 1,
            IsVariadic: true,
            Doc:      "Concatenate strings.",
            Category: "strings",
        },
        {
            Name:     "+",
            ParamCount: 1,
            IsVariadic: true,
            Doc:      "Returns the sum of its arguments.",
            Category: "arithmetic",
        },
        {
            Name:       "list-sort",
            ParamCount: 2,
            Doc:        "Sort a list.",
            Category:   "lists",
            Keywords:   []string{"sort", "ordering", "comparison"},
        },
    }, registry.PhaseRuntime)

    reg.AddBindingSpecs([]registry.BindingSpec{
        {Name: "if", Doc: "Conditional.\nSyntax: (if TEST THEN ELSE)\nCategory: conditionals"},
    })
    reg.AddDocumentation("and",
        "Short-circuit conjunction.\nKeywords: boolean, logic\nCategory: conditionals")
    return reg
}

func TestSearchDoc(t *testing.T) {
    tcs := []struct {
        name     string
        pattern  string
        expected []string
    }{
        {
            name:     "match primitive by name",
            pattern:  "string-app",
            expected: []string{"string-append"},
        },
        {
            name:     "match primitive by doc",
            pattern:  "concatenate",
            expected: []string{"string-append"},
        },
        {
            name:     "match primitive by category",
            pattern:  "arithmetic",
            expected: []string{"+"},
        },
        {
            name:     "match primitive by keyword",
            pattern:  "ordering",
            expected: []string{"list-sort"},
        },
        {
            name:     "keyword partial match",
            pattern:  "compar",
            expected: []string{"list-sort"},
        },
        {
            name:     "match binding spec by name",
            pattern:  "if",
            expected: []string{"if"},
        },
        {
            name:     "match doc entry by keyword",
            pattern:  "boolean",
            expected: []string{"and"},
        },
        {
            name:     "match doc entry by category",
            pattern:  "conditionals",
            expected: []string{"and", "if"},
        },
        {
            name:     "case insensitive",
            pattern:  "STRING-APP",
            expected: []string{"string-append"},
        },
        {
            name:     "no match",
            pattern:  "zzzzzzz",
            expected: []string{},
        },
    }

    reg := buildSearchTestRegistry()
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            c := qt.New(t)
            results := registry.SearchDoc(reg, nil, nil, tc.pattern)
            names := make([]string, len(results))
            for i, r := range results {
                names[i] = r.Name
            }
            c.Assert(names, qt.DeepEquals, tc.expected)
        })
    }
}

func TestSearchDoc_PrimitivePrecedence(t *testing.T) {
    c := qt.New(t)
    reg := registry.NewRegistry()
    reg.AddPrimitive(registry.PrimitiveSpec{
        Name: "apply", ParamCount: 2, IsVariadic: true,
        Doc: "Apply PROC.", Category: "control",
    }, registry.PhaseRuntime)
    reg.AddBindingSpecs([]registry.BindingSpec{
        {Name: "apply", Doc: "Binding-level apply.\nCategory: control"},
    })

    results := registry.SearchDoc(reg, nil, nil, "apply")
    count := 0
    for _, r := range results {
        if r.Name == "apply" {
            count++
            c.Assert(r.Doc, qt.Equals, "Apply PROC.")
        }
    }
    c.Assert(count, qt.Equals, 1)
}

func TestSearchDoc_NilEnvAndLibReg(t *testing.T) {
    c := qt.New(t)
    reg := registry.NewRegistry()
    reg.AddPrimitive(registry.PrimitiveSpec{
        Name: "car", ParamCount: 1, Doc: "First of pair.", Category: "pairs",
    }, registry.PhaseRuntime)
    results := registry.SearchDoc(reg, nil, nil, "car")
    c.Assert(len(results), qt.Equals, 1)
    c.Assert(results[0].Name, qt.Equals, "car")
}

func TestSearchDoc_KeywordsInResult(t *testing.T) {
    c := qt.New(t)
    reg := buildSearchTestRegistry()
    results := registry.SearchDoc(reg, nil, nil, "list-sort")
    c.Assert(len(results), qt.Equals, 1)
    c.Assert(results[0].Keywords, qt.DeepEquals, []string{"sort", "ordering", "comparison"})
}

func TestSearchDoc_DocEntryKeywordsParsed(t *testing.T) {
    c := qt.New(t)
    reg := buildSearchTestRegistry()
    results := registry.SearchDoc(reg, nil, nil, "boolean")
    found := false
    for _, r := range results {
        if r.Name == "and" {
            found = true
            c.Assert(slices.Contains(r.Keywords, "boolean"), qt.IsTrue)
        }
    }
    c.Assert(found, qt.IsTrue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test ./registry/ -v -run TestSearchDoc`
Expected: compilation failure — `SearchDoc` doesn't exist.

**Step 3: Create `registry/search.go`**

```go
package registry

import (
    "sort"
    "strings"

    "github.com/aalpar/wile/docparse"
    "github.com/aalpar/wile/environment"
    "github.com/aalpar/wile/machine/compilation"
)

// DocSearchResult holds one search hit from SearchDoc.
type DocSearchResult struct {
    Name     string
    Doc      string
    Category string
    Keywords []string
}

// SearchDoc searches all documentation sources for case-insensitive
// substring matches on name, doc text, category, or keywords.
//
// Sources searched in order:
//  1. Registry primitives
//  2. Registry binding specs (parsed via docparse)
//  3. Registry doc entries (parsed via docparse)
//  4. Environment bindings (if env is non-nil)
//  5. Loaded libraries (if libReg is non-nil)
//
// Primitives take precedence over non-primitives with the same name.
// Results are sorted by name. env and libReg may be nil.
func SearchDoc(reg *Registry, env *environment.EnvironmentFrame, libReg *compilation.LibraryRegistry, pattern string) []DocSearchResult {
    lowerPattern := strings.ToLower(pattern)
    var results []DocSearchResult

    // 1. Registry primitives — always take precedence.
    prims := reg.Primitives()
    primNames := make(map[string]bool, len(prims))
    for _, pr := range prims {
        primNames[pr.Spec.Name] = true
        if matchesDoc(pr.Spec.Name, pr.Spec.Doc, pr.Spec.Category, pr.Spec.Keywords, lowerPattern) {
            results = append(results, DocSearchResult{
                Name:     pr.Spec.Name,
                Doc:      pr.Spec.Doc,
                Category: pr.Spec.Category,
                Keywords: pr.Spec.Keywords,
            })
        }
    }

    // 2-3. Binding specs and doc entries (non-primitive docs).
    seen := make(map[string]bool)
    for _, r := range nonPrimitiveDocs(reg) {
        if primNames[r.Name] || seen[r.Name] {
            continue
        }
        if matchesDoc(r.Name, r.Doc, r.Category, r.Keywords, lowerPattern) {
            seen[r.Name] = true
            results = append(results, r)
        }
    }

    // 4. Environment bindings.
    if env != nil {
        for _, r := range searchEnvironmentBindings(env, lowerPattern) {
            if primNames[r.Name] || seen[r.Name] {
                continue
            }
            seen[r.Name] = true
            results = append(results, r)
        }
    }

    // 5. Loaded libraries.
    if libReg != nil {
        for _, r := range searchLibraries(libReg, lowerPattern) {
            if seen[r.Name] {
                continue
            }
            seen[r.Name] = true
            results = append(results, r)
        }
    }

    sort.Slice(results, func(i, j int) bool {
        return results[i].Name < results[j].Name
    })
    return results
}

// nonPrimitiveDocs returns doc search results from binding specs and doc entries.
func nonPrimitiveDocs(reg *Registry) []DocSearchResult {
    var results []DocSearchResult
    for _, bs := range reg.BindingSpecs() {
        if bs.Doc == "" {
            continue
        }
        parsed := docparse.ParseDocstring(bs.Doc)
        results = append(results, DocSearchResult{
            Name:     bs.Name,
            Doc:      parsed.Doc,
            Category: parsed.Category,
            Keywords: parsed.Keywords,
        })
    }
    for _, de := range reg.Docs() {
        parsed := docparse.ParseDocstring(de.Doc)
        results = append(results, DocSearchResult{
            Name:     de.Name,
            Doc:      parsed.Doc,
            Category: parsed.Category,
            Keywords: parsed.Keywords,
        })
    }
    return results
}

// searchEnvironmentBindings walks phase environment bindings for matches.
func searchEnvironmentBindings(env *environment.EnvironmentFrame, lowerPattern string) []DocSearchResult {
    ns := env.Namespace()
    if ns == nil {
        return nil
    }
    phases := ns.Phases()
    phaseIndices := phases.Phases()

    seen := make(map[string]bool)
    var results []DocSearchResult
    for _, phase := range phaseIndices {
        phaseEnv := phases.Get(phase)
        if phaseEnv == nil {
            continue
        }
        global := phaseEnv.GlobalEnvironment()
        if global == nil {
            continue
        }
        keys := global.Keys()
        bindings := global.Bindings()
        for sym, idx := range keys {
            name := sym.Key
            if seen[name] {
                continue
            }
            seen[name] = true

            doc := ""
            if idx < len(bindings) {
                bnd := bindings[idx]
                if bnd == nil {
                    continue
                }
                doc = bnd.Doc()
                if doc == "" && bnd.BindingType() == environment.BindingTypeVariable {
                    if dc, ok := bnd.Value().(interface{ Doc() string }); ok {
                        doc = dc.Doc()
                    }
                }
            }

            category := ""
            var keywords []string
            displayDoc := doc
            if doc != "" {
                parsed := docparse.ParseDocstring(doc)
                if parsed.HasStructuredMetadata() {
                    category = parsed.Category
                    keywords = parsed.Keywords
                    displayDoc = parsed.Doc
                }
            }

            if matchesDoc(name, doc, category, keywords, lowerPattern) {
                results = append(results, DocSearchResult{
                    Name:     name,
                    Doc:      displayDoc,
                    Category: category,
                    Keywords: keywords,
                })
            }
        }
    }
    return results
}

// searchLibraries searches loaded libraries for matches.
func searchLibraries(libReg *compilation.LibraryRegistry, lowerPattern string) []DocSearchResult {
    var results []DocSearchResult
    for _, lib := range libReg.All() {
        name := lib.Name.SchemeString()
        if strings.Contains(strings.ToLower(name), lowerPattern) ||
            strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
            results = append(results, DocSearchResult{
                Name:     name,
                Doc:      lib.Description,
                Category: "library",
            })
        }
    }
    return results
}

// matchesDoc reports whether any of name, doc, category, or keywords
// contains the given lowercase pattern.
func matchesDoc(name, doc, category string, keywords []string, pattern string) bool {
    if strings.Contains(strings.ToLower(name), pattern) ||
        strings.Contains(strings.ToLower(doc), pattern) ||
        strings.Contains(strings.ToLower(category), pattern) {
        return true
    }
    for _, kw := range keywords {
        if strings.Contains(strings.ToLower(kw), pattern) {
            return true
        }
    }
    return false
}
```

**Step 4: Run tests**

Run: `go test ./registry/ -v -run TestSearchDoc`
Expected: PASS

---

### Task 2: Update `RegistryDocProvider` to delegate to `SearchDoc`

**Files:**
- Modify: `repl/doc_provider.go`
- Modify: `repl/registry_doc_provider.go`
- Modify: `repl/registry_doc_provider_test.go`

**Step 1: Update `DocSearchResult` references**

In `repl/doc_provider.go`:
- Delete the `DocSearchResult` struct definition
- Update `DocSearchProvider` interface to use `registry.DocSearchResult`:

```go
type DocSearchProvider interface {
    DocProvider
    Search(pattern string) []registry.DocSearchResult
    Categories() []string
    ByCategory(category string) []registry.DocSearchResult
}
```

Add `"github.com/aalpar/wile/registry"` to imports.

**Step 2: Update `RegistryDocProvider`**

In `repl/registry_doc_provider.go`:

Change struct to hold env and libReg:
```go
type RegistryDocProvider struct {
    reg    *registry.Registry
    env    *environment.EnvironmentFrame
    libReg *compilation.LibraryRegistry
}
```

Change constructor:
```go
func NewRegistryDocProvider(reg *registry.Registry, env *environment.EnvironmentFrame, libReg *compilation.LibraryRegistry) *RegistryDocProvider {
    return &RegistryDocProvider{
        reg:    reg,
        env:    env,
        libReg: libReg,
    }
}
```

Replace `Search` method body:
```go
func (p *RegistryDocProvider) Search(pattern string) []registry.DocSearchResult {
    return registry.SearchDoc(p.reg, p.env, p.libReg, pattern)
}
```

Replace `Categories` to use `nonPrimitiveDocs` from registry or keep existing logic (it queries the registry directly, not via SearchDoc — can stay).

Replace `ByCategory` — same, queries registry directly, return type changes to `[]registry.DocSearchResult`.

Replace `nonPrimitiveDocs` — delegates to the same registry function, or keep inline (it's used by `Categories` and `ByCategory`).

Delete: `matchesFields`, `containsKeywordLower` (moved to `registry/search.go` as `matchesDoc`).

Update imports: add `environment`, `machine/compilation`, remove `docparse` if no longer needed directly (check if `LookupDoc` still uses it — yes it does for non-primitive lookup).

**Step 3: Update all test call sites**

In `repl/registry_doc_provider_test.go`, change all `NewRegistryDocProvider(reg)` calls to `NewRegistryDocProvider(reg, nil, nil)`.

Change all `DocSearchResult` references from `repl.DocSearchResult` (implicit, since it's `package repl`) to `registry.DocSearchResult`.

**Step 4: Run tests**

Run: `go test ./repl/ -v -run TestRegistryDocProvider`
Expected: PASS

---

### Task 3: Update `cmdApropos` in `repl/meta.go`

**Files:**
- Modify: `repl/meta.go`

**Step 1: Simplify `cmdApropos`**

The `DocSearchProvider.Search` now returns everything (primitives + bindings + libraries). Remove calls to `searchBindings`, `searchLibraries`, and `mergeSearchResults`.

Replace `cmdApropos` method:
```go
func (p *MetaCommandHandler) cmdApropos(args []string, out io.Writer) {
    if len(args) == 0 {
        fmt.Fprintln(out, "Usage: ,apropos <pattern>")
        return
    }

    pattern := strings.Join(args, " ")
    searchProv, ok := p.docProv.(DocSearchProvider)
    if !ok {
        fmt.Fprintln(out, "Search not available")
        return
    }

    results := searchProv.Search(pattern)
    if len(results) == 0 {
        fmt.Fprintf(out, "No matches for %q\n", pattern)
        return
    }

    var content strings.Builder
    maxName := 0
    for _, r := range results {
        if len(r.Name) > maxName {
            maxName = len(r.Name)
        }
    }
    for _, r := range results {
        cat := ""
        if r.Category != "" {
            cat = fmt.Sprintf("[%s]", r.Category)
        }
        doc := firstLine(r.Doc)
        fmt.Fprintf(&content, "  %-*s  %-14s %s\n", maxName, r.Name, cat, doc)
    }
    writeWithPager(out, content.String(), p.pager)
}
```

Delete: `searchBindings`, `searchLibraries`, `mergeSearchResults`, `containsKeywordLower` methods/functions.

**Step 2: Update `DocSearchResult` references in `meta.go`**

All `DocSearchResult` references in `meta.go` (e.g. in `searchLibraries` return type) will be deleted with the functions. Any remaining references (like in `cmdApropos` locals) should use `registry.DocSearchResult`.

**Step 3: Run tests**

Run: `go test ./repl/ -v`
Expected: PASS

---

### Task 4: Update `PrimApropos` to use `SearchDoc`

**Files:**
- Modify: `registry/core/prim_reflection.go`

**Step 1: Replace `PrimApropos` implementation**

```go
func PrimApropos(mc machine.CallContext) error {
    s, ok := mc.Arg(0).(*values.String)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotAString,
            "apropos: expected string pattern")
    }

    reg := registryFromContext(mc)
    if reg == nil {
        mc.SetValue(values.EmptyList)
        return nil
    }

    env := mc.EnvironmentFrame()
    var libReg *compilation.LibraryRegistry
    if env != nil {
        if lr, ok := env.LibraryRegistry().(*compilation.LibraryRegistry); ok {
            libReg = lr
        }
    }

    results := registry.SearchDoc(reg, env, libReg, s.Value)
    names := make([]string, len(results))
    for i, r := range results {
        names[i] = r.Name
    }

    // Deduplicate (SearchDoc already deduplicates, but names from
    // library results like "(scheme base)" should stay as-is).
    syms := make([]values.Value, len(names))
    for i, n := range names {
        syms[i] = values.NewSymbol(n)
    }
    mc.SetValue(values.List(syms...))
    return nil
}
```

Add `"github.com/aalpar/wile/machine/compilation"` to imports.

Delete: the local `containsKeyword` helper function.

Remove unused imports (`sort` if no longer used elsewhere in the file — check first).

**Step 2: Run tests**

Run: `go test ./registry/core/ -v -run TestApropos`
Expected: PASS (existing tests still pass — broader results now)

---

### Task 5: Update production call sites

**Files:**
- Modify: `cmd/wile/main.go`
- Modify: `cmd/wile/mcp.go`

**Step 1: Update `main.go:411`**

Change:
```go
docProv := repl.NewRegistryDocProvider(reg)
```
To:
```go
env := eng.Environment()
var libReg *compilation.LibraryRegistry
if lr, ok := env.LibraryRegistry().(*compilation.LibraryRegistry); ok {
    libReg = lr
}
docProv := repl.NewRegistryDocProvider(reg, env, libReg)
```

**Step 2: Update `mcp.go:236`**

Same pattern:
```go
env := eng.Environment()
var libReg *compilation.LibraryRegistry
if lr, ok := env.LibraryRegistry().(*compilation.LibraryRegistry); ok {
    libReg = lr
}
docProv := repl.NewRegistryDocProvider(reg, env, libReg)
```

Add `"github.com/aalpar/wile/machine/compilation"` to imports in both files.

**Step 3: Update test call sites**

In `repl/meta_test.go`, all `NewRegistryDocProvider(eng.Registry())` calls
become `NewRegistryDocProvider(eng.Registry(), nil, nil)` (tests don't need
env/libReg for the functionality they test).

In `registry/core/specialforms_test.go`, same: `repl.NewRegistryDocProvider(reg)` → `repl.NewRegistryDocProvider(reg, nil, nil)`.

**Step 4: Verify build**

Run: `go build ./...`
Expected: clean build, no compilation errors.

---

### Task 6: Build, lint, test, covercheck

**Step 1:** `make build`
**Step 2:** `make lint`
**Step 3:** `make test`
**Step 4:** `make covercheck`

All must pass.

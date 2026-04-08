# Documentation Keywords Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `Keywords []string` field to the documentation system so `apropos` matches on curated tags.

**Architecture:** Add `Keywords` to three existing doc structs (`docparse.DocInfo`, `registry.PrimitiveSpec`, `repl.DocInfo`), parse `Keywords:` as a metadata header in `docparse.ParseDocstring`, wire keywords into all search and display paths. Same pattern as the existing `Category` field.

**Design doc:** `plans/2026-04-08-doc-keywords-design.md`

---

### Task 1: docparse — Parse Keywords from Docstrings

**Files:**
- Modify: `docparse/docparse.go`
- Test: `docparse/docparse_test.go`

**Step 1: Write the failing tests**

Add `wantKeywords []string` to the existing `TestParseDocstring` table and add new cases. In `docparse/docparse_test.go`:

Add field to the test struct:
```go
wantKeywords []string
```

Add assertion after the `wantCat` assertion:
```go
c.Assert(info.Keywords, qt.DeepEquals, tc.wantKeywords)
```

Add new test cases to the table:
```go
{
    name:         "keywords single",
    input:        "Sort a list.\nKeywords: sort\nCategory: lists",
    wantDoc:      "Sort a list.",
    wantCat:      "lists",
    wantKeywords: []string{"sort"},
    wantMeta:     true,
},
{
    name:         "keywords multiple",
    input:        "Sort a list.\nKeywords: sort, ordering, comparison\nCategory: lists",
    wantDoc:      "Sort a list.",
    wantCat:      "lists",
    wantKeywords: []string{"sort", "ordering", "comparison"},
    wantMeta:     true,
},
{
    name:         "keywords with extra whitespace",
    input:        "Sort a list.\nKeywords:  sort ,  ordering , comparison \nCategory: lists",
    wantDoc:      "Sort a list.",
    wantCat:      "lists",
    wantKeywords: []string{"sort", "ordering", "comparison"},
    wantMeta:     true,
},
{
    name:         "keywords without category",
    input:        "Sort a list.\nKeywords: sort, ordering",
    wantDoc:      "Sort a list.",
    wantKeywords: []string{"sort", "ordering"},
    wantMeta:     true,
},
```

Update existing test cases that have `wantKeywords: nil` implicitly — no change needed since nil `[]string` DeepEquals nil.

**Step 2: Run tests to verify they fail**

Run: `go test ./docparse/ -v -run TestParseDocstring`
Expected: compilation failure — `DocInfo` has no `Keywords` field.

**Step 3: Implement the parser changes**

In `docparse/docparse.go`:

Add `Keywords` field to `DocInfo`:
```go
type DocInfo struct {
    Doc        string
    Syntax     string
    ParamNames []string
    ParamTypes []values.ValueType
    ReturnType values.ValueType
    Category   string
    Keywords   []string
}
```

Add `Keywords` to `HasStructuredMetadata`:
```go
func (p DocInfo) HasStructuredMetadata() bool {
    return p.Syntax != "" || len(p.ParamNames) > 0 ||
        p.ReturnType != values.TypeAny || p.Category != "" ||
        len(p.Keywords) > 0
}
```

Add `Keywords:` to `isMetadataHeader`:
```go
func isMetadataHeader(line string) bool {
    return strings.HasPrefix(line, "Syntax:") ||
        strings.HasPrefix(line, "Parameters:") ||
        strings.HasPrefix(line, "Returns:") ||
        strings.HasPrefix(line, "Category:") ||
        strings.HasPrefix(line, "Keywords:")
}
```

Add the `Keywords:` case in the `isSectionHeader` switch inside `ParseDocstring`:
```go
case strings.HasPrefix(line, "Keywords:"):
    raw := strings.TrimSpace(strings.TrimPrefix(line, "Keywords:"))
    parts := strings.Split(raw, ",")
    keywords := make([]string, 0, len(parts))
    for _, p := range parts {
        trimmed := strings.TrimSpace(p)
        if trimmed != "" {
            keywords = append(keywords, trimmed)
        }
    }
    info.Keywords = keywords
    currentSection = "Keywords:"
```

Add `"Keywords:"` to the single-line ignore switch:
```go
case "Syntax:", "Returns:", "Category:", "Keywords:":
    // These are single-line sections; ignore continuation lines.
    continue
```

**Step 4: Run tests to verify they pass**

Run: `go test ./docparse/ -v -run TestParseDocstring`
Expected: PASS

**Step 5: Run full docparse tests**

Run: `go test ./docparse/ -v`
Expected: PASS — existing tests unaffected (nil Keywords DeepEquals nil).

---

### Task 2: registry — Add Keywords to PrimitiveSpec

**Files:**
- Modify: `registry/registry.go`

**Step 1: Add the field**

In `registry/registry.go`, add to `PrimitiveSpec`:
```go
type PrimitiveSpec struct {
    Name       string
    ParamCount int
    IsVariadic bool
    Impl       machine.ForeignFunction
    Doc        string
    ParamNames []string
    Category   string
    ParamTypes []values.ValueType
    ReturnType values.ValueType
    Keywords   []string // optional: searchable tags
}
```

**Step 2: Verify build**

Run: `go build ./registry/...`
Expected: compiles. No tests need changing — existing PrimitiveSpec literals don't set Keywords, which defaults to nil.

---

### Task 3: repl — Add Keywords to DocInfo and DocSearchResult

**Files:**
- Modify: `repl/doc_provider.go`

**Step 1: Add Keywords to both types**

In `repl/doc_provider.go`, add to `DocInfo`:
```go
type DocInfo struct {
    Doc        string
    Syntax     string
    TypeLabel  string
    ParamNames []string
    Category   string
    ParamCount int
    IsVariadic bool
    ParamTypes []values.ValueType
    ReturnType values.ValueType
    Keywords   []string
}
```

Add to `DocSearchResult`:
```go
type DocSearchResult struct {
    Name     string
    Doc      string
    Category string
    Keywords []string
}
```

**Step 2: Verify build**

Run: `go build ./repl/...`
Expected: compiles.

---

### Task 4: Wire Keywords Through RegistryDocProvider

**Files:**
- Modify: `repl/registry_doc_provider.go`
- Test: `repl/registry_doc_provider_test.go`

**Step 1: Write failing tests**

In `repl/registry_doc_provider_test.go`, add a primitive with keywords to `buildTestRegistry`:
```go
{
    Name:       "list-sort",
    ParamCount: 2,
    Doc:        "Sort a list.",
    Category:   "lists",
    Keywords:   []string{"sort", "ordering", "comparison"},
},
```

Add test cases to `TestRegistryDocProvider_Search`:
```go
{
    name:     "match by keyword",
    pattern:  "ordering",
    expected: []string{"list-sort"},
},
{
    name:     "keyword partial match",
    pattern:  "compar",
    expected: []string{"list-sort"},
},
```

Add a lookup test to `TestRegistryDocProvider_Found` or add a new test:
```go
func TestRegistryDocProvider_KeywordsInLookup(t *testing.T) {
    c := qt.New(t)
    reg := registry.NewRegistry()
    reg.AddPrimitive(registry.PrimitiveSpec{
        Name:     "list-sort",
        ParamCount: 2,
        Doc:      "Sort a list.",
        Category: "lists",
        Keywords: []string{"sort", "ordering"},
    }, registry.PhaseRuntime)
    prov := NewRegistryDocProvider(reg)
    info, found := prov.LookupDoc("list-sort")
    c.Assert(found, qt.IsTrue)
    c.Assert(info.Keywords, qt.DeepEquals, []string{"sort", "ordering"})
}
```

Add a test for non-primitive keywords from docstring:
```go
func TestRegistryDocProvider_KeywordsFromDocstring(t *testing.T) {
    c := qt.New(t)
    reg := registry.NewRegistry()
    reg.AddDocumentation("my-sort",
        "Sort things.\nKeywords: sort, ordering\nCategory: lists")
    prov := NewRegistryDocProvider(reg)
    info, found := prov.LookupDoc("my-sort")
    c.Assert(found, qt.IsTrue)
    c.Assert(info.Keywords, qt.DeepEquals, []string{"sort", "ordering"})

    results := prov.Search("ordering")
    names := make([]string, len(results))
    for i, r := range results {
        names[i] = r.Name
    }
    c.Assert(slices.Contains(names, "my-sort"), qt.IsTrue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test ./repl/ -v -run TestRegistryDocProvider`
Expected: FAIL — keywords not wired through.

**Step 3: Implement the wiring**

In `repl/registry_doc_provider.go`:

**LookupDoc** — add Keywords for primitives (line ~42-51):
```go
return DocInfo{
    Doc:        pr.Spec.Doc,
    TypeLabel:  "primitive",
    ParamNames: pr.Spec.ParamNames,
    Category:   pr.Spec.Category,
    ParamCount: pr.Spec.ParamCount,
    IsVariadic: pr.Spec.IsVariadic,
    ParamTypes: pr.Spec.ParamTypes,
    ReturnType: pr.Spec.ReturnType,
    Keywords:   pr.Spec.Keywords,
}, true
```

**lookupNonPrimitiveDoc** — add Keywords from parsed docstring (both binding spec and doc entry paths):
```go
return DocInfo{
    Doc:        parsed.Doc,
    Syntax:     parsed.Syntax,
    ParamNames: parsed.ParamNames,
    ParamTypes: parsed.ParamTypes,
    ReturnType: parsed.ReturnType,
    Category:   parsed.Category,
    Keywords:   parsed.Keywords,
}, true
```

**Search** — add Keywords to DocSearchResult for primitives:
```go
results = append(results, DocSearchResult{
    Name:     pr.Spec.Name,
    Doc:      pr.Spec.Doc,
    Category: pr.Spec.Category,
    Keywords: pr.Spec.Keywords,
})
```

**matchesFields** — add keywords parameter:
```go
func matchesFields(name, doc, category string, keywords []string, pattern string) bool {
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

Update all call sites of `matchesFields` (2 sites in `Search`):
- Primitive path: `matchesFields(pr.Spec.Name, pr.Spec.Doc, pr.Spec.Category, pr.Spec.Keywords, lowerPattern)`
- Non-primitive path: `matchesFields(r.Name, r.Doc, r.Category, r.Keywords, lowerPattern)`

**nonPrimitiveDocs** — add Keywords from parsed docstring:
```go
results = append(results, DocSearchResult{
    Name:     bs.Name,
    Doc:      parsed.Doc,
    Category: parsed.Category,
    Keywords: parsed.Keywords,
})
```
(Same for DocEntry path.)

**Step 4: Run tests to verify they pass**

Run: `go test ./repl/ -v -run TestRegistryDocProvider`
Expected: PASS

---

### Task 5: PrimApropos — Add Keywords to Primitive Search

**Files:**
- Modify: `registry/core/prim_reflection.go`
- Test: `registry/core/prim_reflection_test.go`

**Step 1: Write failing test**

In `registry/core/prim_reflection_test.go`, add a test case that searches by keyword. Find existing apropos tests or add:

```go
func TestAproposKeywordSearch(t *testing.T) {
    // Register a primitive with keywords, search by keyword
    code := `(apropos "ordering")`
    result, err := testhelpers.RunSchemeCode(t, code)
    qt.Assert(t, err, qt.IsNil)
    // This will only pass once a primitive with Keywords: ["ordering"] exists,
    // which means this test validates the search path, not just the field.
}
```

Actually, since no existing primitive has keywords yet, the simplest test is to verify the search code doesn't break. The real integration test is manual (`,apropos ordering` in REPL after annotating a primitive). For unit coverage, the `repl` tests in Task 4 cover the `matchesFields` path. Here, just update the `PrimApropos` implementation.

**Step 2: Implement the change**

In `registry/core/prim_reflection.go`, update `PrimApropos` (around line 353-358):

```go
for _, pr := range reg.Primitives() {
    spec := pr.Spec
    if strings.Contains(strings.ToLower(spec.Name), pattern) ||
        strings.Contains(strings.ToLower(spec.Doc), pattern) ||
        strings.Contains(strings.ToLower(spec.Category), pattern) ||
        containsKeyword(spec.Keywords, pattern) {
        names = append(names, spec.Name)
    }
}
```

Add helper at package level:
```go
// containsKeyword reports whether any keyword contains the pattern
// as a case-insensitive substring.
func containsKeyword(keywords []string, pattern string) bool {
    for _, kw := range keywords {
        if strings.Contains(strings.ToLower(kw), pattern) {
            return true
        }
    }
    return false
}
```

**Step 3: Run existing tests**

Run: `go test ./registry/core/ -v -run TestApropos`
Expected: PASS (existing tests unaffected).

---

### Task 6: Display Keywords in `,doc` Output

**Files:**
- Modify: `repl/meta.go`
- Test: `repl/meta_test.go`

**Step 1: Add Keywords display to `formatPrimitiveDoc`**

In `repl/meta.go`, after the Category block (around line 453-455), add:

```go
// Keywords
if len(info.Keywords) > 0 {
    fmt.Fprintf(w, "  Keywords: %s\n", strings.Join(info.Keywords, ", "))
}
```

**Step 2: Populate Keywords in `tryStructuredBindingDoc`**

In `repl/meta.go`, update the `DocInfo` literal in `tryStructuredBindingDoc` (around line 481-489):
```go
formatPrimitiveDoc(w, name, DocInfo{
    Doc:        parsed.Doc,
    Syntax:     parsed.Syntax,
    TypeLabel:  typeLabel,
    ParamNames: parsed.ParamNames,
    ParamTypes: parsed.ParamTypes,
    ReturnType: parsed.ReturnType,
    Category:   parsed.Category,
    Keywords:   parsed.Keywords,
}, showExamples)
```

Do the same for the `DocInfo` literal in `formatBindingDoc` (around line 517-525):
```go
formatPrimitiveDoc(w, name, DocInfo{
    Doc:        parsed.Doc,
    Syntax:     parsed.Syntax,
    TypeLabel:  formTypeLabel(val),
    ParamNames: parsed.ParamNames,
    ParamTypes: parsed.ParamTypes,
    ReturnType: parsed.ReturnType,
    Category:   parsed.Category,
    Keywords:   parsed.Keywords,
}, showExamples)
```

**Step 3: Populate Keywords in `searchBindings`**

In `repl/meta.go`, in the `searchBindings` method (around line 888-898), extract keywords from parsed docstring and include in search match:

After the existing category extraction:
```go
var keywords []string
if doc != "" {
    parsed := docparse.ParseDocstring(doc)
    if parsed.HasStructuredMetadata() {
        category = parsed.Category
        keywords = parsed.Keywords
        displayDoc = parsed.Doc
    }
}
```

Add keywords to the match condition:
```go
if strings.Contains(strings.ToLower(name), lowerPattern) ||
    strings.Contains(strings.ToLower(doc), lowerPattern) ||
    containsKeywordLower(keywords, lowerPattern) {
```

Add the helper (or reuse a shared one):
```go
func containsKeywordLower(keywords []string, pattern string) bool {
    for _, kw := range keywords {
        if strings.Contains(strings.ToLower(kw), pattern) {
            return true
        }
    }
    return false
}
```

Add `Keywords` to the `DocSearchResult` in `searchBindings`:
```go
results = append(results, DocSearchResult{
    Name:     name,
    Doc:      displayDoc,
    Category: category,
    Keywords: keywords,
})
```

**Step 4: Run all tests**

Run: `go test ./repl/ -v`
Expected: PASS

---

### Task 7: Build and Lint

**Step 1: Full build**

Run: `make build`
Expected: clean build.

**Step 2: Lint**

Run: `make lint`
Expected: no new warnings.

**Step 3: Full test suite**

Run: `make test`
Expected: all passing.

**Step 4: Covercheck**

Run: `make covercheck`
Expected: passes.

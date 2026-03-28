# Apropos & Topic Browsing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `apropos`, `doc-topics`, and `doc-topic` as both REPL meta-commands and Scheme primitives, enabling keyword search across names/docs/categories and category-based browsing.

**Architecture:** Extend `DocProvider` with a `DocSearchProvider` interface (`Search`, `Categories`, `ByCategory`). `RegistryDocProvider` implements it. REPL commands use this interface. Scheme primitives access the registry via `mc.EnvironmentFrame().Namespace().Registry()`. Search is case-insensitive substring across name + doc + category.

**Tech Stack:** Go, existing `registry/`, `internal/repl/`, `registry/core/`, `environment/` packages. No new dependencies.

---

### Task 1: Store registry on Namespace in bootstrap path

The bootstrap test helper `NewTopLevelWithRegistry` doesn't call `ns.SetRegistry(reg)`, so `env.Namespace().Registry()` returns `nil` in test code. The Scheme primitives need this.

**Files:**
- Modify: `internal/bootstrap/environment_tiny.go`

**Step 1: Add `SetRegistry` call**

In `initializeEnvironmentWithRegistry`, after `reg.Apply(ctx, env)` succeeds (line ~105), add:

```go
	// Store registry on namespace so runtime primitives (apropos, doc-topic,
	// doc-topics) can access it via mc.EnvironmentFrame().Namespace().Registry().
	env.Namespace().SetRegistry(reg)
```

Insert this immediately after the `reg.Apply` block (after line 108), before `RegisterSyntaxCompilers`.

**Step 2: Verify existing tests pass**

Run: `go test ./internal/bootstrap/... -count=1`
Expected: PASS

Run: `go test ./... -count=1 -short`
Expected: PASS (no regressions — SetRegistry is additive)

**Step 3: Commit**

```
fix(bootstrap): store registry on Namespace for runtime access
```

---

### Task 2: Add `DocSearchProvider` interface and implement on `RegistryDocProvider`

**Files:**
- Modify: `internal/repl/doc_provider.go`
- Modify: `internal/repl/registry_doc_provider.go`
- Modify: `internal/repl/registry_doc_provider_test.go`

**Step 1: Write the failing tests**

Add to `internal/repl/registry_doc_provider_test.go`:

```go
func TestRegistryDocProvider_Search(t *testing.T) {
	reg := buildTestRegistry()
	prov := NewRegistryDocProvider(reg)

	tcs := []struct {
		name    string
		pattern string
		want    []string // expected names in results
	}{
		{"match name", "string-app", []string{"string-append"}},
		{"match doc", "concatenate", []string{"string-append"}},
		{"match category", "arithmetic", []string{"+"}},
		{"case insensitive", "STRING-APP", []string{"string-append"}},
		{"no match", "zzzzzzz", nil},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			results := prov.Search(tc.pattern)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			for _, want := range tc.want {
				qt.Assert(t, slices.Contains(names, want), qt.IsTrue,
					qt.Commentf("expected %q in results %v", want, names))
			}
			if tc.want == nil {
				qt.Assert(t, len(results), qt.Equals, 0)
			}
		})
	}
}

func TestRegistryDocProvider_Categories(t *testing.T) {
	reg := buildTestRegistry()
	prov := NewRegistryDocProvider(reg)
	cats := prov.Categories()
	qt.Assert(t, slices.Contains(cats, "strings"), qt.IsTrue)
	qt.Assert(t, slices.Contains(cats, "arithmetic"), qt.IsTrue)
	// Should be sorted
	qt.Assert(t, sort.StringsAreSorted(cats), qt.IsTrue)
}

func TestRegistryDocProvider_ByCategory(t *testing.T) {
	reg := buildTestRegistry()
	prov := NewRegistryDocProvider(reg)

	results := prov.ByCategory("strings")
	qt.Assert(t, len(results) > 0, qt.IsTrue)
	for _, r := range results {
		qt.Assert(t, r.Category, qt.Equals, "strings")
	}

	// Unknown category returns empty
	results = prov.ByCategory("nonexistent")
	qt.Assert(t, len(results), qt.Equals, 0)
}

func buildTestRegistry() *registry.Registry {
	reg := registry.NewRegistry()
	reg.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-append", ParamCount: 0, IsVariadic: true,
			Doc: "Concatenate strings.", Category: "strings"},
		{Name: "+", ParamCount: 0, IsVariadic: true,
			Doc: "Returns the sum of its arguments.", Category: "arithmetic"},
	}, registry.PhaseRuntime)
	return reg
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestRegistryDocProvider_(Search|Categories|ByCategory)' ./internal/repl/...`
Expected: FAIL — methods don't exist

**Step 3: Add the interface to `doc_provider.go`**

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
	// Search returns entries whose name, doc, or category contains pattern
	// (case-insensitive substring match). Results are sorted by name.
	Search(pattern string) []DocSearchResult
	// Categories returns sorted category names.
	Categories() []string
	// ByCategory returns entries in the named category, sorted by name.
	ByCategory(category string) []DocSearchResult
}
```

**Step 4: Implement on `RegistryDocProvider`**

Add to `internal/repl/registry_doc_provider.go`:

```go
import (
	"sort"
	"strings"

	"github.com/aalpar/wile/registry"
)

// Search returns primitives whose name, doc, or category contains pattern
// (case-insensitive substring match). Results are sorted by name.
func (p *RegistryDocProvider) Search(pattern string) []DocSearchResult {
	pattern = strings.ToLower(pattern)
	var results []DocSearchResult
	for _, pr := range p.reg.Primitives() {
		if matchesPrimitive(pr.Spec, pattern) {
			results = append(results, DocSearchResult{
				Name:     pr.Spec.Name,
				Doc:      pr.Spec.Doc,
				Category: pr.Spec.Category,
			})
		}
	}
	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

// Categories returns sorted category names (non-empty only).
func (p *RegistryDocProvider) Categories() []string {
	byCategory := p.reg.PrimitivesByCategory()
	cats := make([]string, 0, len(byCategory))
	for cat := range byCategory {
		if cat != "" {
			cats = append(cats, cat)
		}
	}
	sort.Strings(cats)
	return cats
}

// ByCategory returns entries in the named category, sorted by name.
func (p *RegistryDocProvider) ByCategory(category string) []DocSearchResult {
	byCategory := p.reg.PrimitivesByCategory()
	prims, ok := byCategory[category]
	if !ok {
		return nil
	}
	results := make([]DocSearchResult, len(prims))
	for i, pr := range prims {
		results[i] = DocSearchResult{
			Name:     pr.Spec.Name,
			Doc:      pr.Spec.Doc,
			Category: pr.Spec.Category,
		}
	}
	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

func matchesPrimitive(spec registry.PrimitiveSpec, pattern string) bool {
	return strings.Contains(strings.ToLower(spec.Name), pattern) ||
		strings.Contains(strings.ToLower(spec.Doc), pattern) ||
		strings.Contains(strings.ToLower(spec.Category), pattern)
}
```

**Step 5: Run tests**

Run: `go test -v -run 'TestRegistryDocProvider_(Search|Categories|ByCategory)' ./internal/repl/...`
Expected: PASS

**Step 6: Commit**

```
feat(repl): add DocSearchProvider interface with search and category browsing
```

---

### Task 3: Add REPL meta-commands `,apropos`, `,topics`, `,topic`

**Files:**
- Modify: `internal/repl/meta.go`
- Modify: `internal/repl/meta_test.go`

**Step 1: Write the failing tests**

Add to `internal/repl/meta_test.go`:

```go
func TestCmdApropos(t *testing.T) {
	ctx := context.Background()
	env, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"no args", nil, "Usage"},
		{"matches name", []string{"string-app"}, "string-append"},
		{"matches category", []string{"arithmetic"}, "+"},
		{"no match", []string{"zzzzzzzzz"}, "No matches"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(env, nil, docProv)
			h.cmdApropos(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdTopics(t *testing.T) {
	ctx := context.Background()
	_, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, docProv)
	h.cmdTopics(&buf)
	output := buf.String()
	qt.Assert(t, strings.Contains(output, "arithmetic"), qt.IsTrue,
		qt.Commentf("output: %q", output))
	qt.Assert(t, strings.Contains(output, "strings"), qt.IsTrue,
		qt.Commentf("output: %q", output))
}

func TestCmdTopic(t *testing.T) {
	ctx := context.Background()
	_, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"no args", nil, "Usage"},
		{"valid category", []string{"arithmetic"}, "+"},
		{"unknown category", []string{"nonexistent"}, "No category"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(nil, nil, docProv)
			h.cmdTopic(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestCmd(Apropos|Topics|Topic)' ./internal/repl/...`
Expected: FAIL — methods don't exist

**Step 3: Register the new commands in metaCommands**

In `internal/repl/meta.go`, add to the `metaCommands` slice (after the `edit` entry):

```go
	{"apropos", []string{"a"}, "Search bindings by name, doc, or category",
		"Usage: ,apropos <pattern>\n\nSearches all bindings for case-insensitive substring matches\nagainst names, documentation, and categories.\nResults show name, category, and one-line description.",
		"session"},
	{"topics", nil, "List documentation categories",
		"Usage: ,topics\n\nShows all available documentation categories with entry counts.",
		"session"},
	{"topic", nil, "List bindings in a documentation category",
		"Usage: ,topic <category>\n\nLists all bindings in the named category.\nUse ,topics to see available categories.",
		"session"},
```

**Step 4: Add case arms in Handle**

In the `switch cmd` block in `Handle()`, add after the `"edit"` case:

```go
	case "apropos", "a":
		p.cmdApropos(args, out)
	case "topics":
		p.cmdTopics(out)
	case "topic":
		p.cmdTopic(args, out)
```

**Step 5: Implement the three command methods**

Add to `internal/repl/meta.go`:

```go
func (p *MetaCommandHandler) cmdApropos(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,apropos <pattern>")
		return
	}

	pattern := strings.Join(args, " ")
	searchProv, ok := p.docProv.(DocSearchProvider)
	if !ok {
		fmt.Fprintln(out, "Search not available (no search provider)")
		return
	}

	results := searchProv.Search(pattern)

	// Also search phase environment bindings
	if p.env != nil {
		envResults := p.searchBindings(pattern)
		results = mergeSearchResults(results, envResults)
	}

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
	writeWithPager(out, content.String(), os.Getenv("PAGER"))
}

func (p *MetaCommandHandler) cmdTopics(out io.Writer) {
	searchProv, ok := p.docProv.(DocSearchProvider)
	if !ok {
		fmt.Fprintln(out, "Topics not available (no search provider)")
		return
	}

	cats := searchProv.Categories()
	if len(cats) == 0 {
		fmt.Fprintln(out, "No categories found")
		return
	}

	var content strings.Builder
	fmt.Fprintln(&content, "Categories:")
	for _, cat := range cats {
		count := len(searchProv.ByCategory(cat))
		fmt.Fprintf(&content, "  %-18s (%d)\n", cat, count)
	}
	writeWithPager(out, content.String(), os.Getenv("PAGER"))
}

func (p *MetaCommandHandler) cmdTopic(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,topic <category>")
		return
	}

	category := args[0]
	searchProv, ok := p.docProv.(DocSearchProvider)
	if !ok {
		fmt.Fprintln(out, "Topics not available (no search provider)")
		return
	}

	results := searchProv.ByCategory(category)
	if len(results) == 0 {
		fmt.Fprintf(out, "No category %q (use ,topics to list categories)\n", category)
		return
	}

	var content strings.Builder
	fmt.Fprintf(&content, "%s (%d procedures):\n", category, len(results))
	maxName := 0
	for _, r := range results {
		if len(r.Name) > maxName {
			maxName = len(r.Name)
		}
	}
	for _, r := range results {
		doc := firstLine(r.Doc)
		fmt.Fprintf(&content, "  %-*s  %s\n", maxName, r.Name, doc)
	}
	writeWithPager(out, content.String(), os.Getenv("PAGER"))
}

// searchBindings searches phase environment bindings for the pattern.
func (p *MetaCommandHandler) searchBindings(pattern string) []DocSearchResult {
	pattern = strings.ToLower(pattern)
	topLevel := p.env.Namespace()
	if topLevel == nil {
		return nil
	}
	phases := topLevel.Phases()
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
		for sym := range global.Keys() {
			name := sym.Key
			if seen[name] {
				continue
			}
			seen[name] = true

			doc := ""
			bnd := global.GetOwnGlobalBindingBySymbol(&sym)
			if bnd != nil {
				doc = bnd.Doc()
				if doc == "" && bnd.BindingType() == environment.BindingTypeVariable {
					doc = callableDoc(bnd.Value())
				}
			}

			if strings.Contains(strings.ToLower(name), pattern) ||
				strings.Contains(strings.ToLower(doc), pattern) {
				results = append(results, DocSearchResult{
					Name: name,
					Doc:  doc,
				})
			}
		}
	}
	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

// mergeSearchResults merges registry and environment results, deduplicating by name.
// Registry results take precedence (richer metadata).
func mergeSearchResults(registry, env []DocSearchResult) []DocSearchResult {
	seen := make(map[string]bool, len(registry))
	for _, r := range registry {
		seen[r.Name] = true
	}
	for _, r := range env {
		if !seen[r.Name] {
			registry = append(registry, r)
		}
	}
	sort.Slice(registry, func(i, j int) bool {
		return registry[i].Name < registry[j].Name
	})
	return registry
}

// firstLine returns the first line of s, or s itself if single-line.
func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return s[:i]
	}
	return s
}
```

**Step 6: Verify `GlobalEnvironment()` accessor exists**

The code above uses `phaseEnv.GlobalEnvironment()` and `global.GetOwnGlobalBindingBySymbol(&sym)`. Before implementing, verify these methods exist. If `GlobalEnvironment()` doesn't exist on `EnvironmentFrame`, use the pattern from `cmdDoc` — walk `phases.Get(phase)` and use `GetBinding(sym)` instead. If `GetOwnGlobalBindingBySymbol` doesn't exist, use the existing `Keys()` + `GetOwnGlobalBinding(gi)` pattern.

**Adjust the implementation as needed** based on actual method names. The search logic is the important part; the environment access methods must match what exists.

**Step 7: Run tests**

Run: `go test -v -run 'TestCmd(Apropos|Topics|Topic)' ./internal/repl/...`
Expected: PASS

**Step 8: Run full repl test suite**

Run: `go test ./internal/repl/... -count=1`
Expected: PASS

**Step 9: Commit**

```
feat(repl): add ,apropos, ,topics, ,topic meta-commands
```

---

### Task 4: Add `apropos` Scheme primitive

**Files:**
- Modify: `registry/core/reflection.go`
- Modify: `registry/core/prim_reflection.go`
- Modify: `registry/core/prim_reflection_test.go`

**Step 1: Write the failing test**

Add to `registry/core/prim_reflection_test.go`:

```go
func TestApropos(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "find by name",
			Code: `(memq 'string-append (apropos "string-app"))`,
			Expected: nil, // just check it's not #f — exact value depends on list structure
		},
		{
			Name: "find by doc",
			Code: `(pair? (apropos "concatenate"))`,
			Expected: values.TrueValue,
		},
		{
			Name: "returns list of symbols",
			Code: `(let ((results (apropos "car")))
			          (and (list? results)
			               (symbol? (car results))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "no matches returns empty list",
			Code: `(apropos "zzzzzzzzzzz")`,
			Expected: values.EmptyList,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			if tc.Expected != nil {
				qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
			} else {
				// Just verify no error and result is a pair (non-empty list)
				_, ok := result.(*values.Pair)
				qt.Assert(t, ok, qt.IsTrue,
					qt.Commentf("expected pair, got %T: %s", result, result.SchemeString()))
			}
		})
	}
}

func TestAproposErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity zero", Code: `(apropos)`},
		{Name: "wrong arity two", Code: `(apropos "a" "b")`},
		{Name: "wrong type", Code: `(apropos 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestApropos ./registry/core/...`
Expected: FAIL — `apropos` is unbound

**Step 3: Register the primitive**

In `registry/core/reflection.go`, add to the spec slice:

```go
		{Name: "apropos", ParamCount: 1, Impl: PrimApropos,
			Doc: "Returns a list of symbols whose name, doc, or category matches the pattern string (case-insensitive substring).", ParamNames: []string{"pattern"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeString}},
```

**Step 4: Implement the primitive**

Add to `registry/core/prim_reflection.go`:

```go
// PrimApropos implements (apropos pattern).
// Returns a sorted list of symbols whose name, doc, or category contains
// the pattern as a case-insensitive substring.
func PrimApropos(mc *machine.MachineContext) error {
	pattern := strings.ToLower(mc.Arg(0).(*values.String).Value())

	regAny := mc.EnvironmentFrame().Namespace().Registry()
	if regAny == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}
	reg, ok := regAny.(*registry.Registry)
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}

	var names []string
	for _, pr := range reg.Primitives() {
		spec := pr.Spec
		if strings.Contains(strings.ToLower(spec.Name), pattern) ||
			strings.Contains(strings.ToLower(spec.Doc), pattern) ||
			strings.Contains(strings.ToLower(spec.Category), pattern) {
			names = append(names, spec.Name)
		}
	}
	sort.Strings(names)

	syms := make([]values.Value, len(names))
	for i, n := range names {
		syms[i] = values.NewSymbol(n)
	}
	mc.SetValue(values.List(syms...))
	return nil
}
```

Note: This imports `registry` in `prim_reflection.go`. If this creates a circular dependency, move the primitive to a new file `prim_apropos.go` in the same package. Check with `go build ./registry/core/...` before proceeding.

**Step 5: Run tests**

Run: `go test -v -run TestApropos ./registry/core/...`
Expected: PASS

**Step 6: Commit**

```
feat(core): add apropos primitive for documentation search
```

---

### Task 5: Add `doc-topics` and `doc-topic` Scheme primitives

**Files:**
- Modify: `registry/core/reflection.go`
- Modify: `registry/core/prim_reflection.go`
- Modify: `registry/core/prim_reflection_test.go`

**Step 1: Write the failing tests**

Add to `registry/core/prim_reflection_test.go`:

```go
func TestDocTopics(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "returns list of strings",
			Code: `(let ((ts (doc-topics)))
			          (and (list? ts) (string? (car ts))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "contains arithmetic",
			Code: `(member "arithmetic" (doc-topics))`,
			Expected: nil, // non-#f
		},
		{
			Name: "sorted",
			Code: `(let ((ts (doc-topics)))
			          (let check ((prev (car ts)) (rest (cdr ts)))
			            (cond
			              ((null? rest) #t)
			              ((string<=? prev (car rest))
			               (check (car rest) (cdr rest)))
			              (else #f))))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			if tc.Expected != nil {
				qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
			} else {
				qt.Assert(t, result.IsVoid(), qt.IsFalse,
					qt.Commentf("expected non-void, got: %s", result.SchemeString()))
			}
		})
	}
}

func TestDocTopicsErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity", Code: `(doc-topics "extra")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestDocTopic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "returns list of symbols",
			Code: `(let ((procs (doc-topic "arithmetic")))
			          (and (list? procs) (symbol? (car procs))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "contains +",
			Code: `(memq '+ (doc-topic "arithmetic"))`,
			Expected: nil, // non-#f
		},
		{
			Name: "unknown category returns empty list",
			Code: `(doc-topic "nonexistent")`,
			Expected: values.EmptyList,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			if tc.Expected != nil {
				qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
			} else {
				qt.Assert(t, result.IsVoid(), qt.IsFalse,
					qt.Commentf("expected non-void, got: %s", result.SchemeString()))
			}
		})
	}
}

func TestDocTopicErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity zero", Code: `(doc-topic)`},
		{Name: "wrong arity two", Code: `(doc-topic "a" "b")`},
		{Name: "wrong type", Code: `(doc-topic 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestDocTopic' ./registry/core/...`
Expected: FAIL — `doc-topics` and `doc-topic` are unbound

**Step 3: Register the primitives**

In `registry/core/reflection.go`, add to the spec slice:

```go
		{Name: "doc-topics", ParamCount: 0, Impl: PrimDocTopics,
			Doc: "Returns a sorted list of documentation category name strings.", Category: "reflection"},
		{Name: "doc-topic", ParamCount: 1, Impl: PrimDocTopic,
			Doc: "Returns a sorted list of symbols in the named documentation category.", ParamNames: []string{"category"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeString}},
```

**Step 4: Implement the primitives**

Add to `registry/core/prim_reflection.go`:

```go
// registryFromContext extracts the *registry.Registry from the MachineContext's
// namespace. Returns nil if unavailable.
func registryFromContext(mc *machine.MachineContext) *registry.Registry {
	regAny := mc.EnvironmentFrame().Namespace().Registry()
	if regAny == nil {
		return nil
	}
	reg, ok := regAny.(*registry.Registry)
	if !ok {
		return nil
	}
	return reg
}

// PrimDocTopics implements (doc-topics).
// Returns a sorted list of category name strings.
func PrimDocTopics(mc *machine.MachineContext) error {
	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	byCategory := reg.PrimitivesByCategory()
	cats := make([]string, 0, len(byCategory))
	for cat := range byCategory {
		if cat != "" {
			cats = append(cats, cat)
		}
	}
	sort.Strings(cats)

	items := make([]values.Value, len(cats))
	for i, cat := range cats {
		items[i] = values.NewString(cat)
	}
	mc.SetValue(values.List(items...))
	return nil
}

// PrimDocTopic implements (doc-topic category).
// Returns a sorted list of symbols in the named category.
func PrimDocTopic(mc *machine.MachineContext) error {
	category := mc.Arg(0).(*values.String).Value()

	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	byCategory := reg.PrimitivesByCategory()
	prims, ok := byCategory[category]
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}

	names := make([]string, len(prims))
	for i, pr := range prims {
		names[i] = pr.Spec.Name
	}
	sort.Strings(names)

	syms := make([]values.Value, len(names))
	for i, n := range names {
		syms[i] = values.NewSymbol(n)
	}
	mc.SetValue(values.List(syms...))
	return nil
}
```

**Step 5: Refactor `PrimApropos` to use `registryFromContext`**

Replace the inline registry extraction in `PrimApropos` with:

```go
	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}
```

**Step 6: Run tests**

Run: `go test -v -run 'TestDocTopic|TestApropos' ./registry/core/...`
Expected: PASS

**Step 7: Run full registry/core test suite**

Run: `go test ./registry/core/... -count=1`
Expected: PASS

**Step 8: Commit**

```
feat(core): add doc-topics and doc-topic primitives for category browsing
```

---

### Task 6: Lint and verification

**Step 1: Run linter**

Run: `make lint`
Expected: PASS

**Step 2: Run covercheck**

Run: `make covercheck`
Expected: PASS

**Step 3: Run full test suite**

Run: `make test`
Expected: PASS

**Step 4: Fix any issues and commit**

---

### Task 7: Update PRIMITIVES.md

**Files:**
- Modify: `PRIMITIVES.md`

Add `apropos`, `doc-topics`, and `doc-topic` to the reflection section, following the existing pattern for `procedure-documentation` and other reflection primitives.

**Commit:**

```
docs: add apropos, doc-topics, doc-topic to PRIMITIVES.md
```

---

### Task 8: Update TODO.md

**Files:**
- Modify: `TODO.md`

Mark the `apropos / documentation search` and `Cross-referencing and topic browsing` items as done. Note that "see also" cross-references are not included (no data source).

**Commit:**

```
docs: mark apropos and topic browsing as done in TODO.md
```

---

## Notes for Implementer

### Circular Dependency Risk

`registry/core/prim_reflection.go` importing `registry` creates a dependency: `registry/core` → `registry`. Check that this doesn't form a cycle. If it does, the fix is to have the primitive access the registry through the `any` typed `Namespace().Registry()` and avoid importing `registry` directly — type-assert inline:

```go
type primitiveSearchable interface {
	Primitives() []struct{ Spec struct{ Name, Doc, Category string } }
	PrimitivesByCategory() map[string][]struct{ Spec struct{ Name, Doc, Category string } }
}
```

But more likely: `registry/core` already imports `registry` (it imports `registry.PrimitiveSpec`), so this should be fine.

### Environment Access Pattern for `searchBindings`

The `searchBindings` method in Task 3 uses `GlobalEnvironment()` and `GetOwnGlobalBindingBySymbol`. Verify these exist:
- `EnvironmentFrame.GlobalEnvironment()` → may be called `Global()` or accessed through fields
- Use `global.Keys()` to iterate, then `global.Bindings()[index]` to get each binding

Adapt method names to match what actually exists in the `environment` package.

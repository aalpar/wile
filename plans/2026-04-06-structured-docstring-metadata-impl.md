# Structured Docstring Metadata Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Parse `Parameters:`, `Returns:`, `Category:` sections from Guile-style docstrings so Scheme-defined procedures get the same `,doc`/`,apropos`/`,topics` output as Go-implemented primitives.

**Architecture:** New `internal/docparse/` package with a `ParseDocstring` function that splits a raw docstring into prose + structured metadata. The REPL's `formatBindingDoc` uses it to render closures through `formatPrimitiveDoc` (the Go-primitive renderer). At engine startup, `RegisterSchemeDocstrings` walks runtime bindings and registers structured entries in the registry for `apropos`/`topics` visibility.

**Tech Stack:** Go, existing `internal/repl/`, `registry/`, `values/`, `engine.go`. No new dependencies.

**Design doc:** `plans/2026-04-06-structured-docstring-metadata-design.md`

---

### Task 1: Create `internal/docparse/` package — type vocabulary

**Files:**
- Create: `internal/docparse/docparse.go`
- Create: `internal/docparse/docparse_test.go`

**Step 1: Write the failing test**

In `internal/docparse/docparse_test.go`:

```go
package docparse

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestParseValueType(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected values.ValueType
	}{
		{Name: "procedure", input: "procedure", expected: values.TypeProcedure},
		{Name: "list", input: "list", expected: values.TypeList},
		{Name: "exact-integer", input: "exact-integer", expected: values.TypeExactInteger},
		{Name: "unknown maps to any", input: "frobnicate", expected: values.TypeAny},
		{Name: "empty maps to any", input: "", expected: values.TypeAny},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			qt.Assert(t, ParseValueType(tc.input), qt.Equals, tc.expected)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestParseValueType ./internal/docparse/...`
Expected: FAIL — package does not exist

**Step 3: Implement**

In `internal/docparse/docparse.go`:

```go
package docparse

import "github.com/aalpar/wile/values"

// typeNameToValueType maps docstring type names to ValueType constants.
// Built once from the canonical typeNames array in values/value_type.go.
var typeNameToValueType map[string]values.ValueType

func init() {
	typeNameToValueType = make(map[string]values.ValueType)
	for vt := values.ValueType(0); vt < values.TypeCount(); vt++ {
		name := vt.String()
		if name != "" && name != "unknown" {
			typeNameToValueType[name] = vt
		}
	}
}

// ParseValueType converts a docstring type name to a ValueType.
// Unrecognized names return TypeAny.
func ParseValueType(name string) values.ValueType {
	if vt, ok := typeNameToValueType[name]; ok {
		return vt
	}
	return values.TypeAny
}
```

Note: This requires `values.TypeCount()` to be exported. Check if it already is — if `typeCount` is unexported, add a one-line accessor `func TypeCount() ValueType { return typeCount }` to `values/value_type.go`. If it already exists under a different name, use that.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestParseValueType ./internal/docparse/...`
Expected: PASS

**Step 5: Commit**

```
feat(docparse): add type vocabulary for docstring metadata parsing
```

---

### Task 2: Implement `ParseDocstring`

**Files:**
- Modify: `internal/docparse/docparse.go`
- Modify: `internal/docparse/docparse_test.go`

**Step 1: Write the failing tests**

Add to `internal/docparse/docparse_test.go`:

```go
func TestParseDocstring(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		wantDoc    string
		wantParams []string
		wantTypes  []values.ValueType
		wantReturn values.ValueType
		wantCat    string
	}{
		{
			name:       "prose only",
			input:      "Apply F to each element of LST.",
			wantDoc:    "Apply F to each element of LST.",
			wantReturn: values.TypeAny,
		},
		{
			name:       "full structured",
			input:      "Apply F to each element of LST.\n\nParameters:\n  f : procedure\n  lst : list\nReturns: list\nCategory: lists",
			wantDoc:    "Apply F to each element of LST.",
			wantParams: []string{"f", "lst"},
			wantTypes:  []values.ValueType{values.TypeProcedure, values.TypeList},
			wantReturn: values.TypeList,
			wantCat:    "lists",
		},
		{
			name:       "category only",
			input:      "Return #t if X is #f.\n\nCategory: predicates",
			wantDoc:    "Return #t if X is #f.",
			wantReturn: values.TypeAny,
			wantCat:    "predicates",
		},
		{
			name:       "flexible ordering",
			input:      "Sum numbers.\n\nCategory: arithmetic\nReturns: number\nParameters:\n  z : number",
			wantDoc:    "Sum numbers.",
			wantParams: []string{"z"},
			wantTypes:  []values.ValueType{values.TypeNumber},
			wantReturn: values.TypeNumber,
			wantCat:    "arithmetic",
		},
		{
			name:       "examples preserved in doc",
			input:      "Double X.\n\nExamples:\n  (double 3)  => 6\n\nCategory: math",
			wantDoc:    "Double X.\n\nExamples:\n  (double 3)  => 6",
			wantReturn: values.TypeAny,
			wantCat:    "math",
		},
		{
			name:       "see also preserved in doc",
			input:      "Map F over LST.\n\nSee also: `for-each'.\n\nCategory: lists",
			wantDoc:    "Map F over LST.\n\nSee also: `for-each'.",
			wantReturn: values.TypeAny,
			wantCat:    "lists",
		},
		{
			name:       "unknown param type becomes any",
			input:      "Parameters:\n  x : widget",
			wantDoc:    "",
			wantParams: []string{"x"},
			wantTypes:  []values.ValueType{values.TypeAny},
			wantReturn: values.TypeAny,
		},
		{
			name:       "empty string",
			input:      "",
			wantDoc:    "",
			wantReturn: values.TypeAny,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			info := ParseDocstring(tc.input)
			qt.Assert(t, info.Doc, qt.Equals, tc.wantDoc)
			qt.Assert(t, info.ParamNames, qt.DeepEquals, tc.wantParams)
			qt.Assert(t, info.ParamTypes, qt.DeepEquals, tc.wantTypes)
			qt.Assert(t, info.ReturnType, qt.Equals, tc.wantReturn)
			qt.Assert(t, info.Category, qt.Equals, tc.wantCat)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestParseDocstring ./internal/docparse/...`
Expected: FAIL — `ParseDocstring` does not exist

**Step 3: Implement**

Add to `internal/docparse/docparse.go`:

```go
import (
	"strings"

	"github.com/aalpar/wile/values"
)

// DocInfo holds parsed structured metadata from a docstring.
type DocInfo struct {
	Doc        string
	ParamNames []string
	ParamTypes []values.ValueType
	ReturnType values.ValueType
	Category   string
}

// HasStructuredMetadata reports whether any structured fields were parsed.
func (p DocInfo) HasStructuredMetadata() bool {
	return len(p.ParamNames) > 0 || p.ReturnType != values.TypeAny || p.Category != ""
}

// metadataSections are the section headers that contain structured metadata.
// These are extracted from the prose. Examples: and See also: stay in the prose.
var metadataSections = map[string]bool{
	"Parameters:": true,
	"Returns:":    true,
	"Category:":   true,
}

// proseSections are section headers that remain part of the prose description.
var proseSections = map[string]bool{
	"Examples:": true,
	"See also:": true,
}

// ParseDocstring extracts structured metadata from a Guile-style docstring.
// Sections (Parameters:, Returns:, Category:) may appear in any order.
// Text before the first metadata section (including Examples: and See also:)
// is the prose description.
func ParseDocstring(raw string) DocInfo {
	if raw == "" {
		return DocInfo{}
	}

	lines := strings.Split(raw, "\n")
	var info DocInfo
	var proseLines []string
	var currentSection string
	inMetadata := false

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Check if this line starts a new section.
		sectionFound := ""
		for sec := range metadataSections {
			if strings.HasPrefix(trimmed, sec) {
				sectionFound = sec
				break
			}
		}
		if sectionFound == "" {
			for sec := range proseSections {
				if strings.HasPrefix(trimmed, sec) {
					sectionFound = sec
					break
				}
			}
		}

		if sectionFound != "" {
			if metadataSections[sectionFound] {
				// Entering a metadata section.
				currentSection = sectionFound
				inMetadata = true

				// Handle inline value (e.g., "Returns: list").
				rest := strings.TrimSpace(strings.TrimPrefix(trimmed, sectionFound))
				if rest != "" {
					switch sectionFound {
					case "Returns:":
						info.ReturnType = ParseValueType(rest)
					case "Category:":
						info.Category = rest
					}
				}
			} else {
				// Prose section (Examples:, See also:) — stays in prose.
				currentSection = ""
				inMetadata = false
				proseLines = append(proseLines, line)
			}
			continue
		}

		if inMetadata {
			// Indented line belongs to current metadata section.
			if strings.HasPrefix(line, "  ") && currentSection == "Parameters:" {
				parseParamLine(trimmed, &info)
			}
			// Non-indented non-blank line ends the section.
			if trimmed != "" && !strings.HasPrefix(line, "  ") {
				inMetadata = false
				currentSection = ""
				proseLines = append(proseLines, line)
			}
		} else {
			proseLines = append(proseLines, line)
		}
	}

	info.Doc = strings.TrimSpace(strings.Join(proseLines, "\n"))
	return info
}

// parseParamLine parses "name : type" from a trimmed parameter line.
func parseParamLine(line string, info *DocInfo) {
	parts := strings.SplitN(line, " : ", 2)
	if len(parts) != 2 {
		return
	}
	name := strings.TrimSpace(parts[0])
	typeName := strings.TrimSpace(parts[1])
	if name == "" {
		return
	}
	info.ParamNames = append(info.ParamNames, name)
	info.ParamTypes = append(info.ParamTypes, ParseValueType(typeName))
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestParseDocstring ./internal/docparse/...`
Expected: PASS

**Step 5: Run linter**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
feat(docparse): implement ParseDocstring with flexible section ordering
```

---

### Task 3: Integrate parser into `,doc` REPL command

**Files:**
- Modify: `internal/repl/meta.go`
- Modify: `internal/repl/meta_test.go`

**Step 1: Write the failing test**

Add to `internal/repl/meta_test.go`. This test verifies that `,doc map` renders structured output (signature line with types) rather than raw text. Find the existing `,doc` test function and add a case:

```go
{
	name:    "scheme closure with structured docstring",
	input:   ",doc map",
	wantContains: []string{"procedure", "list", "lists"},
},
```

The exact test structure depends on the existing test harness — adapt to match. The key assertion: `,doc map` output should contain "procedure" (param type), "list" (return type), and "lists" (category) once `map`'s docstring has structured sections.

Note: This test won't pass until Task 5 (annotating bootstrap procedures). For Task 3, verify the plumbing by writing a helper test that creates a `MachineClosure` with a structured docstring and checks `formatBindingDoc` output.

**Step 2: Implement**

In `internal/repl/meta.go`, modify `formatBindingDoc` (around line 417). After the existing `callableDoc` check, add parsing:

```go
func formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int, showExamples bool) {
	phaseName := phaseLabel(phase)

	switch bnd.BindingType() {
	case environment.BindingTypePrimitive:
		fmt.Fprintf(w, "%s: special form (%s)\n", name, phaseName)
	case environment.BindingTypeSyntax:
		fmt.Fprintf(w, "%s: syntax transformer (%s)\n", name, phaseName)
	case environment.BindingTypeVariable:
		val := bnd.Value()

		// Try structured docstring parsing for closures.
		if raw := callableDoc(val); raw != "" {
			parsed := docparse.ParseDocstring(raw)
			if parsed.HasStructuredMetadata() {
				formatPrimitiveDoc(w, name, DocInfo{
					Doc:        parsed.Doc,
					ParamNames: parsed.ParamNames,
					ParamTypes: parsed.ParamTypes,
					ReturnType: parsed.ReturnType,
					Category:   parsed.Category,
				}, showExamples)
				return
			}
		}

		fmt.Fprintf(w, "%s: %s (%s)\n", name, val.SchemeString(), phaseName)
	default:
		fmt.Fprintf(w, "%s: bound in %s\n", name, phaseName)
	}

	// Existing fallback for unstructured docs.
	doc := ""
	if bnd.BindingType() == environment.BindingTypeVariable {
		doc = callableDoc(bnd.Value())
	}
	if doc == "" {
		doc = bnd.Doc()
	}
	if doc != "" {
		if !showExamples {
			doc = StripExamples(doc)
		}
		indented := strings.ReplaceAll(doc, "\n", "\n  ")
		fmt.Fprintf(w, "\n  %s\n", indented)
	}
}
```

Important: The structured path returns early via `formatPrimitiveDoc`, so the unstructured fallback only runs for bindings without structured metadata. Add the import for `docparse`.

**Step 3: Run existing tests for regressions**

Run: `go test -v ./internal/repl/... -count=1`
Expected: PASS (no regressions — existing closures have unstructured docs, so they take the fallback path)

**Step 4: Commit**

```
feat(repl): use parsed docstring metadata in ,doc for closures
```

---

### Task 4: Add `RegisterSchemeDocstrings` to registry

**Files:**
- Modify: `registry/registry.go`
- Modify: `registry/registry_test.go`

**Step 1: Write the failing test**

Add to `registry/registry_test.go`:

```go
func TestRegisterDocOnlyPrimitive(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()

	reg.AddDocOnlyPrimitive(PrimitiveSpec{
		Name:       "map",
		Doc:        "Apply F to each element of LST.",
		ParamNames: []string{"f", "lst"},
		ParamTypes: []values.ValueType{values.TypeProcedure, values.TypeList},
		ReturnType: values.TypeList,
		Category:   "lists",
		ParamCount: 2,
	})

	pr, found := reg.FindPrimitive("map", 0)
	c.Assert(found, qt.IsTrue)
	c.Assert(pr.Spec.Category, qt.Equals, "lists")
	c.Assert(pr.Spec.Doc, qt.Equals, "Apply F to each element of LST.")

	// Should not overwrite an existing Go primitive.
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "car",
		ParamCount: 1,
		Impl:       func(mc *machine.MachineContext) error { return nil },
		Doc:        "Go car",
		Category:   "pairs",
	}, PhaseRuntime)

	reg.AddDocOnlyPrimitive(PrimitiveSpec{
		Name:     "car",
		Doc:      "Scheme car",
		Category: "pairs",
	})

	pr, found = reg.FindPrimitive("car", 0)
	c.Assert(found, qt.IsTrue)
	c.Assert(pr.Spec.Doc, qt.Equals, "Go car") // original preserved
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestRegisterDocOnlyPrimitive ./registry/...`
Expected: FAIL — `AddDocOnlyPrimitive` does not exist

**Step 3: Implement**

Add to `registry/registry.go`:

```go
// AddDocOnlyPrimitive registers a documentation-only primitive entry.
// It does not create a runtime binding — used for Scheme-defined procedures
// that are already bound in the environment but need registry visibility
// for apropos/topics. Skips registration if a primitive with the same name
// already exists (Go primitives take precedence).
func (p *Registry) AddDocOnlyPrimitive(spec PrimitiveSpec) {
	p.mu.Lock()
	defer p.mu.Unlock()

	for _, reg := range p.primitives {
		if reg.Spec.Name == spec.Name {
			return
		}
	}

	p.primitives = append(p.primitives, PrimitiveRegistration{
		Spec:   spec,
		Phases: 0, // no phase — doc-only, not applied to environments
	})
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestRegisterDocOnlyPrimitive ./registry/...`
Expected: PASS

**Step 5: Commit**

```
feat(registry): add AddDocOnlyPrimitive for Scheme-defined procedure docs
```

---

### Task 5: Add `RegisterSchemeDocstrings` to engine startup

**Files:**
- Modify: `engine.go`

**Step 1: Write the failing test**

Add to `wile_test.go` (or create a focused test). This is an integration test that verifies a Scheme-defined procedure with structured docstring appears in the registry after engine startup:

```go
func TestSchemeDocstringRegistration(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithAllExtensions())
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	// "map" is defined in bootstrap_procedures.scm. After Task 6 adds
	// structured metadata to its docstring, it should appear in the registry.
	reg := eng.Registry()
	pr, found := reg.FindPrimitive("map", 0)
	qt.Assert(t, found, qt.IsTrue)
	qt.Assert(t, pr.Spec.Category, qt.Equals, "lists")
}
```

Note: This test depends on Task 6 (annotating `map`'s docstring). Until then, mark it as a TODO or skip it. The plumbing can be verified by checking that `RegisterSchemeDocstrings` runs without error.

**Step 2: Implement**

Add to `engine.go`, a new function called from `applyBaseEnvironment` after `reg.ApplyDocs(env)`:

```go
// registerSchemeDocstrings walks runtime bindings and registers documentation-only
// entries in the registry for Scheme-defined procedures with structured docstrings.
// This makes them visible to apropos/topics via RegistryDocProvider.
func registerSchemeDocstrings(env *environment.EnvironmentFrame, reg *registry.Registry) {
	topLevel := env.Namespace()
	if topLevel == nil {
		return
	}

	runtime := topLevel.Phases().Get(0) // runtime phase
	if runtime == nil {
		return
	}

	for _, name := range runtime.BoundNames() {
		sym := values.NewSymbol(name)
		bnd := runtime.GetBinding(sym, nil)
		if bnd == nil || bnd.BindingType() != environment.BindingTypeVariable {
			continue
		}

		raw := callableDocFromValue(bnd.Value())
		if raw == "" {
			continue
		}

		parsed := docparse.ParseDocstring(raw)
		if !parsed.HasStructuredMetadata() {
			continue
		}

		reg.AddDocOnlyPrimitive(registry.PrimitiveSpec{
			Name:       name,
			Doc:        parsed.Doc,
			ParamNames: parsed.ParamNames,
			ParamTypes: parsed.ParamTypes,
			ReturnType: parsed.ReturnType,
			Category:   parsed.Category,
			ParamCount: len(parsed.ParamNames),
		})
	}
}

// callableDocFromValue extracts the docstring from a callable value.
func callableDocFromValue(v values.Value) string {
	dc, ok := v.(interface{ Doc() string })
	if ok {
		return dc.Doc()
	}
	return ""
}
```

Then in `applyBaseEnvironment`, add the call after `reg.ApplyDocs(env)`:

```go
	reg.ApplyDocs(env)

	// Register structured docstrings from Scheme-defined procedures.
	registerSchemeDocstrings(env, reg)

	return nil
```

**Step 3: Run tests for regressions**

Run: `go test ./... -count=1 -timeout 120s`
Expected: PASS

**Step 4: Commit**

```
feat(engine): register Scheme docstring metadata at bootstrap for apropos/topics
```

---

### Task 6: Annotate bootstrap procedures with structured metadata

**Files:**
- Modify: `registry/core/bootstrap_procedures.scm`

**Step 1: Add structured sections to ~30 key procedures**

Update the existing docstrings in `bootstrap_procedures.scm`. Each procedure that already has a Guile-style docstring gets `Parameters:`, `Returns:`, and `Category:` sections appended. Procedures without docstrings get a new docstring.

Key procedures to annotate (grouped by category):

**Category: lists**
- `map` — add `Parameters:\n  f : procedure\n  lst : list\nReturns: list\nCategory: lists`
- `for-each` — same params, `Returns: void`, `Category: lists`
- `member` — `Parameters:\n  obj : any\n  lst : list\nReturns: list\nCategory: lists`
- `assoc` — `Parameters:\n  obj : any\n  alist : list\nReturns: pair\nCategory: lists`
- `list?` — `Parameters:\n  x : any\nReturns: boolean\nCategory: predicates`

**Category: vectors**
- `vector-map` — `Parameters:\n  f : procedure\n  v : vector\nReturns: vector\nCategory: vectors`
- `vector-for-each` — same params, `Returns: void`, `Category: vectors`

**Category: strings**
- `string-map` — `Parameters:\n  f : procedure\n  s : string\nReturns: string\nCategory: strings`
- `string-for-each` — same params, `Returns: void`, `Category: strings`

**Category: predicates**
- `not` — `Parameters:\n  x : any\nReturns: boolean\nCategory: predicates`
- `zero?` — `Parameters:\n  z : number\nReturns: boolean\nCategory: predicates`
- `positive?` — same pattern
- `negative?` — same pattern
- `exact-integer?` — same pattern
- `boolean=?` — `Parameters:\n  b : boolean\nReturns: boolean\nCategory: predicates`
- `symbol=?` — `Parameters:\n  s : symbol\nReturns: boolean\nCategory: predicates`

**Category: arithmetic**
- `square` — `Parameters:\n  z : number\nReturns: number\nCategory: arithmetic`

**Category: pairs** (CxR accessors — mechanical)
- All 28 CxR accessors get `Category: pairs` added to existing docstrings.

**Step 2: Verify no test regressions**

Run: `make test`
Expected: PASS (adding structured sections to docstrings is additive — doesn't change behavior)

**Step 3: Verify `,doc` output**

Run the REPL and test:
```
> ,doc map
```
Expected: structured output with signature, parameter types, return type, and category — matching the format Go primitives use.

**Step 4: Verify `,apropos` and `,topics`**

```
> ,apropos map
> ,topics
> ,topic lists
```
Expected: `map` appears in apropos results and in the `lists` category.

**Step 5: Commit**

```
feat(bootstrap): add structured docstring metadata to core Scheme procedures
```

---

### Task 7: Update CODING_STYLE.md conventions

**Files:**
- Modify: `CODING_STYLE.md`

**Step 1: Add new sections to the docstring conventions**

In the "Scheme Docstring Conventions" section, add documentation for the three new section headers. Insert after the existing "Rules" table:

Add a new subsection "Structured Metadata Sections" that documents `Parameters:`, `Returns:`, and `Category:` with the format specified in the design doc. Reference the type vocabulary (same names as `ValueType.String()`).

**Step 2: Commit**

```
docs: add Parameters/Returns/Category to docstring conventions
```

---

### Task 8: Lint and final verification

**Step 1: Run linter**

Run: `make lint`
Expected: PASS

**Step 2: Run covercheck**

Run: `make covercheck`
Expected: PASS

**Step 3: Run full test suite**

Run: `make test`
Expected: PASS

**Step 4: Run integration tests**

Run: `go test -v ./integration/... -timeout 120s`
Expected: PASS

**Step 5: Commit any fixes**

---

## Out of Scope (Phase 3)

- Annotating ~270 stdlib library procedures (`stdlib/lib/`)
- `define-syntax` macro metadata
- Runtime type enforcement from docstring declarations
- Auto-generating PRIMITIVES.md from registry

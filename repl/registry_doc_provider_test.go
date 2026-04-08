// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package repl

import (
	"slices"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

func buildTestRegistry() *registry.Registry {
	reg := registry.NewRegistry()
	reg.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name:       "string-append",
			ParamCount: 1,
			IsVariadic: true,
			Doc:        "Concatenate strings.",
			Category:   "strings",
		},
		{
			Name:       "+",
			ParamCount: 1,
			IsVariadic: true,
			Doc:        "Returns the sum of its arguments.",
			Category:   "arithmetic",
		},
		{
			Name:       "list-sort",
			ParamCount: 2,
			Doc:        "Sort a list.",
			Category:   "lists",
			Keywords:   []string{"sort", "ordering", "comparison"},
		},
	}, registry.PhaseRuntime)
	return reg
}

// buildTestRegistryWithDocs creates a registry with primitives, binding specs,
// and doc entries to test unified category/search behavior.
func buildTestRegistryWithDocs() *registry.Registry {
	reg := buildTestRegistry()

	// Add an "apply" primitive so we can test primitive-over-binding-spec precedence.
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "apply",
		ParamCount: 2,
		IsVariadic: true,
		Doc:        "Apply PROC to ARGS.",
		Category:   "control",
	}, registry.PhaseRuntime)

	// Binding specs (compile-time forms with embedded metadata)
	reg.AddBindingSpecs([]registry.BindingSpec{
		{
			Name: "if",
			Doc:  "Conditional expression.\nSyntax: (if TEST CONSEQUENT ALTERNATE)\nCategory: conditionals",
		},
		{
			Name: "define",
			Doc:  "Variable definition.\nSyntax: (define VARIABLE EXPRESSION)\nCategory: definitions",
		},
		// apply exists as both a primitive and a binding spec — primitive should win.
		{
			Name: "apply",
			Doc:  "Binding-level apply doc.\nSyntax: (apply PROC ARGS)\nCategory: control",
		},
	})

	// Doc entries (macro docs with embedded metadata)
	reg.AddDocumentation("and",
		"Short-circuit conjunction.\nSyntax: (and TEST1 ...)\nCategory: conditionals")
	reg.AddDocumentation("cond",
		"Multi-way conditional.\nSyntax: (cond CLAUSE1 CLAUSE2 ...)\nCategory: conditionals")

	return reg
}

func TestRegistryDocProvider_Found(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "test-prim",
		ParamCount: 2,
		Doc:        "A test primitive.",
		ParamNames: []string{"a", "b"},
		Category:   "test",
	}, registry.PhaseRuntime)

	provider := NewRegistryDocProvider(reg)
	info, found := provider.LookupDoc("test-prim")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.Doc, qt.Equals, "A test primitive.")
	c.Assert(info.ParamNames, qt.DeepEquals, []string{"a", "b"})
	c.Assert(info.Category, qt.Equals, "test")
}

func TestRegistryDocProvider_ContractFields(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "test-contracted",
		ParamCount: 2,
		Impl: func(_ machine.CallContext) error {
			return nil
		},
		Doc:        "A test.",
		ParamNames: []string{"s", "k"},
		Category:   "test",
		ParamTypes: []values.ValueType{values.TypeString, values.TypeInteger},
		ReturnType: values.TypeCharacter,
	}, registry.PhaseRuntime)
	prov := NewRegistryDocProvider(reg)
	info, found := prov.LookupDoc("test-contracted")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.TypeLabel, qt.Equals, "primitive")
	c.Assert(info.ParamTypes, qt.HasLen, 2)
	c.Assert(info.ParamTypes[0], qt.Equals, values.TypeString)
	c.Assert(info.ParamTypes[1], qt.Equals, values.TypeInteger)
	c.Assert(info.ReturnType, qt.Equals, values.TypeCharacter)
}

func TestRegistryDocProvider_NotFound(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	provider := NewRegistryDocProvider(reg)
	_, found := provider.LookupDoc("nonexistent")
	c.Assert(found, qt.IsFalse)
}

func TestRegistryDocProvider_Search(t *testing.T) {
	tcs := []struct {
		name     string
		pattern  string
		expected []string
	}{
		{
			name:     "match by name substring",
			pattern:  "string-app",
			expected: []string{"string-append"},
		},
		{
			name:     "match by doc substring",
			pattern:  "concatenate",
			expected: []string{"string-append"},
		},
		{
			name:     "match by category",
			pattern:  "arithmetic",
			expected: []string{"+"},
		},
		{
			name:     "case insensitive",
			pattern:  "STRING-APP",
			expected: []string{"string-append"},
		},
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
		{
			name:     "no match",
			pattern:  "zzzzzzz",
			expected: []string{},
		},
	}
	provider := NewRegistryDocProvider(buildTestRegistry())
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			results := provider.Search(tc.pattern)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			c.Assert(names, qt.DeepEquals, tc.expected)
		})
	}
}

func TestRegistryDocProvider_Categories(t *testing.T) {
	c := qt.New(t)
	provider := NewRegistryDocProvider(buildTestRegistry())
	cats := provider.Categories()
	c.Assert(cats, qt.DeepEquals, []string{"arithmetic", "lists", "strings"})
}

func TestRegistryDocProvider_Categories_ExcludesEmpty(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "no-category",
		ParamCount: 0,
		Doc:        "Has no category.",
	}, registry.PhaseRuntime)
	provider := NewRegistryDocProvider(reg)
	cats := provider.Categories()
	c.Assert(cats, qt.HasLen, 0)
}

func TestStripExamples(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "no examples section",
			input:    "Returns the car of a pair.",
			expected: "Returns the car of a pair.",
		},
		{
			name:     "with examples section",
			input:    "Returns the car of a pair.\n\nExamples:\n  (car '(1 2)) => 1",
			expected: "Returns the car of a pair.",
		},
		{
			name:     "empty string",
			input:    "",
			expected: "",
		},
		{
			name:     "examples at start",
			input:    "\n\nExamples:\n  (f)",
			expected: "",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, StripExamples(tc.input), qt.Equals, tc.expected)
		})
	}
}

func TestRegistryDocProvider_ByCategory(t *testing.T) {
	tcs := []struct {
		name     string
		category string
		expected []string
	}{
		{
			name:     "existing category",
			category: "strings",
			expected: []string{"string-append"},
		},
		{
			name:     "nonexistent category",
			category: "nonexistent",
			expected: []string{},
		},
	}
	provider := NewRegistryDocProvider(buildTestRegistry())
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			results := provider.ByCategory(tc.category)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			c.Assert(names, qt.DeepEquals, tc.expected)
		})
	}
}

// Tests for unified doc provider (binding specs + doc entries included)

func TestRegistryDocProvider_CategoriesIncludesNonPrimitives(t *testing.T) {
	c := qt.New(t)
	provider := NewRegistryDocProvider(buildTestRegistryWithDocs())
	cats := provider.Categories()

	// Should include primitive categories
	c.Assert(slices.Contains(cats, "arithmetic"), qt.IsTrue,
		qt.Commentf("should include primitive category: %v", cats))
	c.Assert(slices.Contains(cats, "strings"), qt.IsTrue,
		qt.Commentf("should include primitive category: %v", cats))

	// Should include binding spec categories
	c.Assert(slices.Contains(cats, "conditionals"), qt.IsTrue,
		qt.Commentf("should include binding spec category: %v", cats))
	c.Assert(slices.Contains(cats, "definitions"), qt.IsTrue,
		qt.Commentf("should include binding spec category: %v", cats))
}

func TestRegistryDocProvider_ByCategoryFindsNonPrimitives(t *testing.T) {
	tcs := []struct {
		name     string
		category string
		expected []string
	}{
		{
			name:     "primitive category",
			category: "strings",
			expected: []string{"string-append"},
		},
		{
			name:     "binding spec category",
			category: "conditionals",
			expected: []string{"and", "cond", "if"},
		},
		{
			name:     "doc entry only category",
			category: "definitions",
			expected: []string{"define"},
		},
		{
			name:     "nonexistent category",
			category: "nonexistent",
			expected: []string{},
		},
	}
	provider := NewRegistryDocProvider(buildTestRegistryWithDocs())
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			results := provider.ByCategory(tc.category)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			c.Assert(names, qt.DeepEquals, tc.expected)
		})
	}
}

func TestRegistryDocProvider_SearchFindsNonPrimitives(t *testing.T) {
	tcs := []struct {
		name     string
		pattern  string
		expected []string
	}{
		{
			name:     "find binding spec by name",
			pattern:  "define",
			expected: []string{"define"},
		},
		{
			name:     "find doc entry by doc content",
			pattern:  "multi-way",
			expected: []string{"cond"},
		},
		{
			name:     "find by category from doc string",
			pattern:  "conditionals",
			expected: []string{"and", "cond", "if"},
		},
		{
			name:     "find primitive still works",
			pattern:  "string-append",
			expected: []string{"string-append"},
		},
	}
	provider := NewRegistryDocProvider(buildTestRegistryWithDocs())
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			results := provider.Search(tc.pattern)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			c.Assert(names, qt.DeepEquals, tc.expected)
		})
	}
}

func TestRegistryDocProvider_LookupDocFindsNonPrimitives(t *testing.T) {
	c := qt.New(t)
	provider := NewRegistryDocProvider(buildTestRegistryWithDocs())

	// Binding spec lookup
	info, found := provider.LookupDoc("if")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.Syntax, qt.Equals, "(if TEST CONSEQUENT ALTERNATE)")
	c.Assert(info.Category, qt.Equals, "conditionals")

	// Doc entry lookup
	info, found = provider.LookupDoc("and")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.Syntax, qt.Equals, "(and TEST1 ...)")
	c.Assert(info.Category, qt.Equals, "conditionals")

	// Still not found for unknown
	_, found = provider.LookupDoc("nonexistent")
	c.Assert(found, qt.IsFalse)
}

func TestRegistryDocProvider_PrimitiveTakesPriorityOverBindingSpec(t *testing.T) {
	c := qt.New(t)
	provider := NewRegistryDocProvider(buildTestRegistryWithDocs())

	// "apply" is registered as both a primitive and a binding spec.
	// The primitive should win everywhere.

	// LookupDoc should return the primitive's doc, not the binding spec's.
	info, found := provider.LookupDoc("apply")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.Doc, qt.Equals, "Apply PROC to ARGS.",
		qt.Commentf("LookupDoc should return primitive doc, not binding spec doc"))

	// ByCategory should show apply exactly once from the primitive.
	results := provider.ByCategory("control")
	count := 0
	for _, r := range results {
		if r.Name == "apply" {
			count++
			c.Assert(r.Doc, qt.Equals, "Apply PROC to ARGS.",
				qt.Commentf("ByCategory entry should be from primitive"))
		}
	}
	c.Assert(count, qt.Equals, 1, qt.Commentf("apply should appear exactly once"))

	// Search should return apply exactly once from the primitive.
	searchResults := provider.Search("apply")
	count = 0
	for _, r := range searchResults {
		if r.Name == "apply" {
			count++
			c.Assert(r.Doc, qt.Equals, "Apply PROC to ARGS.",
				qt.Commentf("Search entry should be from primitive"))
		}
	}
	c.Assert(count, qt.Equals, 1, qt.Commentf("apply should appear exactly once in search"))
}

func TestRegistryDocProvider_KeywordsInLookup(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "list-sort",
		ParamCount: 2,
		Doc:        "Sort a list.",
		Category:   "lists",
		Keywords:   []string{"sort", "ordering"},
	}, registry.PhaseRuntime)
	prov := NewRegistryDocProvider(reg)
	info, found := prov.LookupDoc("list-sort")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.Keywords, qt.DeepEquals, []string{"sort", "ordering"})
}

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

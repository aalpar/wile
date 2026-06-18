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

package registry_test

import (
	"context"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// stubLibrarySearcher is an in-memory test double for registry.LibrarySearcher.
// It removes the need to stand up a compilation.LibraryRegistry to exercise
// SearchDoc's loaded-library path.
type stubLibrarySearcher struct {
	libs []registry.LibraryDoc
}

func (p stubLibrarySearcher) AllLibraries() []registry.LibraryDoc {
	return p.libs
}

// stubExportSearcher is an in-memory test double for
// registry.LibraryExportSearcher.
type stubExportSearcher struct {
	exports []registry.LibraryExportDoc
}

func (p stubExportSearcher) AllLibraryExports() []registry.LibraryExportDoc {
	return p.exports
}

func buildSearchTestRegistry() *registry.Registry {
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
	}, registry.PhaseSetRuntime)

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
			results := registry.SearchDoc(reg, nil, nil, nil, tc.pattern)
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
	}, registry.PhaseSetRuntime)
	reg.AddBindingSpecs([]registry.BindingSpec{
		{Name: "apply", Doc: "Binding-level apply.\nCategory: control"},
	})

	results := registry.SearchDoc(reg, nil, nil, nil, "apply")
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
	}, registry.PhaseSetRuntime)
	results := registry.SearchDoc(reg, nil, nil, nil, "car")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "car")
}

func TestSearchDoc_KeywordsInResult(t *testing.T) {
	c := qt.New(t)
	reg := buildSearchTestRegistry()
	results := registry.SearchDoc(reg, nil, nil, nil, "list-sort")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Keywords, qt.DeepEquals, []string{"sort", "ordering", "comparison"})
}

func TestSearchDoc_DocEntryKeywordsParsed(t *testing.T) {
	c := qt.New(t)
	reg := buildSearchTestRegistry()
	results := registry.SearchDoc(reg, nil, nil, nil, "boolean")
	found := false
	for _, r := range results {
		if r.Name == "and" {
			found = true
			c.Assert(slices.Contains(r.Keywords, "boolean"), qt.IsTrue)
		}
	}
	c.Assert(found, qt.IsTrue)
}

func TestSearchDoc_EnvironmentBindings(t *testing.T) {
	c := qt.New(t)
	// bootstrap creates an environment with core primitives (car, cdr, cons, etc.)
	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	reg, ok := env.Namespace().Registry().(*registry.Registry)
	c.Assert(ok, qt.IsTrue)

	// Search for "car" — should find it via environment bindings even though
	// the env path is now exercised (car is also a primitive, so the primitive
	// path covers it too). Search for something that's only in the environment.
	results := registry.SearchDoc(reg, env, nil, nil, "car")
	names := make([]string, len(results))
	for i, r := range results {
		names[i] = r.Name
	}
	c.Assert(slices.Contains(names, "car"), qt.IsTrue)
}

func TestSearchDoc_CoreKeywordsDiscovery(t *testing.T) {
	c := qt.New(t)
	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	reg, ok := env.Namespace().Registry().(*registry.Registry)
	c.Assert(ok, qt.IsTrue)

	// Verify that common-name keywords on core primitives are discoverable
	// via SearchDoc. Each case searches a term that does NOT appear in the
	// primitive's name, doc, or category — only in its Keywords field.
	tcs := []struct {
		name    string
		pattern string
		expect  string
	}{
		{name: "dictionary finds make-hashtable", pattern: "dictionary", expect: "make-hashtable"},
		{name: "slice finds substring", pattern: "slice", expect: "substring"},
		{name: "head finds car", pattern: "head", expect: "car"},
		{name: "tail finds cdr", pattern: "tail", expect: "cdr"},
		{name: "ord finds char->integer", pattern: "ord", expect: "char->integer"},
		{name: "concat finds string-append", pattern: "concat", expect: "string-append"},
		{name: "greatest common divisor finds gcd", pattern: "greatest common divisor", expect: "gcd"},
		{name: "float finds inexact", pattern: "float", expect: "inexact"},
		{name: "integer division finds quotient", pattern: "integer division", expect: "quotient"},
		{name: "nth finds list-ref", pattern: "nth", expect: "list-ref"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			results := registry.SearchDoc(reg, nil, nil, nil, tc.pattern)
			names := make([]string, len(results))
			for i, r := range results {
				names[i] = r.Name
			}
			c.Assert(slices.Contains(names, tc.expect), qt.IsTrue,
				qt.Commentf("searching %q should find %q; got %v", tc.pattern, tc.expect, names))
		})
	}
}

func TestSearchDoc_Libraries(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()

	libs := stubLibrarySearcher{libs: []registry.LibraryDoc{
		{Name: "(test math)", Description: "Test math library"},
	}}

	results := registry.SearchDoc(reg, nil, libs, nil, "math")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "(test math)")
	c.Assert(results[0].Category, qt.Equals, "library")
	c.Assert(results[0].Doc, qt.Equals, "Test math library")
}

func TestSearchDoc_LibraryByDescription(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()

	libs := stubLibrarySearcher{libs: []registry.LibraryDoc{
		{Name: "(wile algebra)", Description: "Algebraic structures: rings, fields, lattices"},
	}}

	results := registry.SearchDoc(reg, nil, libs, nil, "lattice")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "(wile algebra)")
}

func TestSearchDoc_EnvironmentBindingKeywordsFromValue(t *testing.T) {
	c := qt.New(t)

	// Create an environment with a ForeignClosure whose Doc() contains Keywords.
	// This simulates Scheme-defined closures imported from a library: the binding
	// itself has no doc, but the closure value carries a structured docstring.
	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	reg, ok := env.Namespace().Registry().(*registry.Registry)
	c.Assert(ok, qt.IsTrue)

	// Create a ForeignClosure with a docstring containing Keywords.
	fc := machine.NewForeignClosure(env, 1, false, func(mc machine.CallContext) error {
		return nil
	})
	fc.SetName("make-widget")
	fc.SetDoc("Construct a widget.\nKeywords: factory, builder, abelian\nCategory: widgets")

	// Bind it in the global environment.
	sym := values.NewSymbol("make-widget")
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	err = env.SetOwnGlobalValue(gi, fc)
	c.Assert(err, qt.IsNil)

	// Search by keyword — should find make-widget via its closure's docstring.
	results := registry.SearchDoc(reg, env, nil, nil, "abelian")
	names := make([]string, len(results))
	for i, r := range results {
		names[i] = r.Name
	}
	c.Assert(slices.Contains(names, "make-widget"), qt.IsTrue,
		qt.Commentf("SearchDoc should match environment bindings by keyword from value Doc(); got %v", names))

	// Verify keywords were parsed and included in the result.
	for _, r := range results {
		if r.Name == "make-widget" {
			c.Assert(slices.Contains(r.Keywords, "abelian"), qt.IsTrue,
				qt.Commentf("result should have parsed keywords: %v", r.Keywords))
			c.Assert(r.Category, qt.Equals, "widgets")
		}
	}
}

func TestSearchDoc_UnloadedExports(t *testing.T) {
	c := qt.New(t)

	reg := registry.NewRegistry()
	exports := stubExportSearcher{exports: []registry.LibraryExportDoc{
		{
			Name:        "(srfi 1)",
			Description: "SRFI 1: List library.",
			Exports:     []string{"fold", "unfold", "partition"},
		},
	}}

	results := registry.SearchDoc(reg, nil, nil, exports, "partition")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "partition")
	c.Assert(results[0].Category, qt.Equals, "not imported")
	c.Assert(strings.Contains(results[0].Doc, "(srfi 1)"), qt.IsTrue)
	c.Assert(strings.Contains(results[0].Doc, "SRFI 1: List library."), qt.IsTrue)
}

func TestSearchDoc_UnloadedLibraryByName(t *testing.T) {
	c := qt.New(t)

	reg := registry.NewRegistry()
	exports := stubExportSearcher{exports: []registry.LibraryExportDoc{
		{
			Name:        "(wile algebra)",
			Description: "Algebraic structures: rings, fields, lattices.",
			Exports:     []string{"make-group", "make-ring", "make-field"},
		},
	}}

	// "algebra" matches the library name but none of the export names.
	results := registry.SearchDoc(reg, nil, nil, exports, "algebra")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "(wile algebra)")
	c.Assert(results[0].Category, qt.Equals, "library (not imported)")
	c.Assert(results[0].Doc, qt.Equals, "Algebraic structures: rings, fields, lattices.")
}

func TestSearchDoc_UnloadedLibraryByDescription(t *testing.T) {
	c := qt.New(t)

	reg := registry.NewRegistry()
	exports := stubExportSearcher{exports: []registry.LibraryExportDoc{
		{
			Name:        "(wile algebra)",
			Description: "Algebraic structures: rings, fields, lattices.",
			Exports:     []string{"make-group", "make-ring", "make-field"},
		},
	}}

	// "lattice" matches the description but none of the export names —
	// confirms description-only matching.
	results := registry.SearchDoc(reg, nil, nil, exports, "lattice")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "(wile algebra)")
	c.Assert(results[0].Category, qt.Equals, "library (not imported)")
}

func TestSearchDoc_UnloadedLibraryNameAndExportBothMatch(t *testing.T) {
	c := qt.New(t)

	reg := registry.NewRegistry()
	exports := stubExportSearcher{exports: []registry.LibraryExportDoc{
		{
			Name:        "(test foo)",
			Description: "A foo library.",
			Exports:     []string{"foo-bar", "foo-baz", "quux"},
		},
	}}

	// "foo" matches the library name AND two export names.
	results := registry.SearchDoc(reg, nil, nil, exports, "foo")
	names := make([]string, len(results))
	for i, r := range results {
		names[i] = r.Name
	}
	c.Assert(slices.Contains(names, "(test foo)"), qt.IsTrue,
		qt.Commentf("should include library-level result; got %v", names))
	c.Assert(slices.Contains(names, "foo-bar"), qt.IsTrue,
		qt.Commentf("should include export-level result; got %v", names))
	c.Assert(slices.Contains(names, "foo-baz"), qt.IsTrue,
		qt.Commentf("should include export-level result; got %v", names))
	c.Assert(!slices.Contains(names, "quux"), qt.IsTrue,
		qt.Commentf("quux should not match; got %v", names))
}

func TestSearchDoc_LoadedTakesPrecedenceOverUnloaded(t *testing.T) {
	c := qt.New(t)

	reg := registry.NewRegistry()
	// Register "fold" as a primitive so it appears as a loaded binding.
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "fold",
		ParamCount: 3,
		Doc:        "Fold over a list.",
		Category:   "lists",
	}, registry.PhaseSetRuntime)

	// Also put "fold" in the unloaded export index.
	exports := stubExportSearcher{exports: []registry.LibraryExportDoc{
		{
			Name:        "(srfi 1)",
			Description: "SRFI 1: List library.",
			Exports:     []string{"fold", "unfold", "partition"},
		},
	}}

	// Search for "fold" — matches both "fold" and "unfold" as substrings.
	results := registry.SearchDoc(reg, nil, nil, exports, "fold")

	// "fold" should appear exactly once — the primitive wins via primNames.
	foldCount := 0
	for _, r := range results {
		if r.Name == "fold" {
			foldCount++
			c.Assert(r.Category, qt.Equals, "lists")
		}
	}
	c.Assert(foldCount, qt.Equals, 1)

	// "unfold" should appear from the unloaded index (substring match on "fold").
	unfoldFound := false
	for _, r := range results {
		if r.Name == "unfold" {
			unfoldFound = true
			c.Assert(r.Category, qt.Equals, "not imported")
		}
	}
	c.Assert(unfoldFound, qt.IsTrue)

	// "partition" should NOT appear (doesn't match "fold").
	for _, r := range results {
		c.Assert(r.Name != "partition", qt.IsTrue)
	}
}

// TestSearchDoc_UnloadedSkipsLoadedLibrary verifies that a library present in
// the loaded-library searcher is excluded from the unloaded-export results,
// even when its name matches the pattern. This exercises searchUnloadedExports'
// loaded-name set, which matches by canonical library-name string.
func TestSearchDoc_UnloadedSkipsLoadedLibrary(t *testing.T) {
	c := qt.New(t)

	reg := registry.NewRegistry()
	libs := stubLibrarySearcher{libs: []registry.LibraryDoc{
		{Name: "(srfi 1)", Description: "SRFI 1: List library."},
	}}
	exports := stubExportSearcher{exports: []registry.LibraryExportDoc{
		{
			Name:        "(srfi 1)",
			Description: "SRFI 1: List library.",
			Exports:     []string{"fold", "partition"},
		},
	}}

	// "srfi" matches the library name. Because (srfi 1) is already loaded,
	// it must surface only as a loaded library, never as an unloaded one.
	results := registry.SearchDoc(reg, nil, libs, exports, "srfi")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "(srfi 1)")
	c.Assert(results[0].Category, qt.Equals, "library")
}

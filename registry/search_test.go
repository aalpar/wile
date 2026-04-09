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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

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
	}, registry.PhaseRuntime)
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
	}, registry.PhaseRuntime)
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
	env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
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

func TestSearchDoc_Libraries(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	libReg := compilation.NewLibraryRegistry()

	libName := compilation.NewLibraryName("test", "math")
	lib := compilation.NewCompiledLibrary(libName, environment.NewNamespace().Runtime())
	lib.Description = "Test math library"
	err := libReg.Register(lib)
	c.Assert(err, qt.IsNil)

	results := registry.SearchDoc(reg, nil, libReg, nil, "math")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "(test math)")
	c.Assert(results[0].Category, qt.Equals, "library")
	c.Assert(results[0].Doc, qt.Equals, "Test math library")
}

func TestSearchDoc_LibraryByDescription(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	libReg := compilation.NewLibraryRegistry()

	libName := compilation.NewLibraryName("wile", "algebra")
	lib := compilation.NewCompiledLibrary(libName, environment.NewNamespace().Runtime())
	lib.Description = "Algebraic structures: rings, fields, lattices"
	err := libReg.Register(lib)
	c.Assert(err, qt.IsNil)

	results := registry.SearchDoc(reg, nil, libReg, nil, "lattice")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "(wile algebra)")
}

func TestSearchDoc_EnvironmentBindingKeywordsFromValue(t *testing.T) {
	c := qt.New(t)

	// Create an environment with a ForeignClosure whose Doc() contains Keywords.
	// This simulates Scheme-defined closures imported from a library: the binding
	// itself has no doc, but the closure value carries a structured docstring.
	env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
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
	idx := compilation.NewLibraryExportIndexFromEntries(map[string]*compilation.LibrarySummary{
		"srfi/1": {
			Name:        compilation.NewLibraryName("srfi", "1"),
			Description: "SRFI 1: List library.",
			Exports:     []string{"fold", "unfold", "partition"},
		},
	})

	results := registry.SearchDoc(reg, nil, nil, idx, "partition")
	c.Assert(len(results), qt.Equals, 1)
	c.Assert(results[0].Name, qt.Equals, "partition")
	c.Assert(results[0].Category, qt.Equals, "not imported")
	c.Assert(strings.Contains(results[0].Doc, "(srfi 1)"), qt.IsTrue)
	c.Assert(strings.Contains(results[0].Doc, "SRFI 1: List library."), qt.IsTrue)
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
	}, registry.PhaseRuntime)

	// Also put "fold" in the unloaded export index.
	idx := compilation.NewLibraryExportIndexFromEntries(map[string]*compilation.LibrarySummary{
		"srfi/1": {
			Name:        compilation.NewLibraryName("srfi", "1"),
			Description: "SRFI 1: List library.",
			Exports:     []string{"fold", "unfold", "partition"},
		},
	})

	// Search for "fold" — matches both "fold" and "unfold" as substrings.
	results := registry.SearchDoc(reg, nil, nil, idx, "fold")

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

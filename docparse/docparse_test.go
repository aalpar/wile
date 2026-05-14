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

package docparse_test

import (
	"testing"

	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestParseValueType(t *testing.T) {
	tcs := []struct {
		name         string
		input        string
		expectedName string
	}{
		{
			name:         "known type procedure",
			input:        "procedure",
			expectedName: "procedure",
		},
		{
			name:         "known type list",
			input:        "list",
			expectedName: "list",
		},
		{
			name:         "unknown type",
			input:        "frobnicate",
			expectedName: "frobnicate",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := docparse.ParseValueType(tc.input)
			c.Assert(result.Name(), qt.Equals, tc.expectedName)
		})
	}

	// Empty string returns nil (unspecified).
	t.Run("empty string returns nil", func(t *testing.T) {
		c := qt.New(t)
		c.Assert(docparse.ParseValueType(""), qt.IsNil)
	})

	// Known types return ValueType constants.
	t.Run("known type returns ValueType", func(t *testing.T) {
		c := qt.New(t)
		c.Assert(
			docparse.ParseValueType("integer"),
			qt.Equals,
			values.TypeConstraint(values.TypeInteger),
		)
	})

	// Unknown types return *NamedTypeConstraint.
	t.Run("unknown type returns NamedTypeConstraint", func(t *testing.T) {
		c := qt.New(t)
		result := docparse.ParseValueType("frobnicate")
		_, isNamed := result.(*values.NamedTypeConstraint)
		c.Assert(isNamed, qt.IsTrue)
	})

	// "exact-integer" used to alias TypeInteger via TypeExactInteger.
	// The alias was removed in the values/ Phase 0 structural reduction
	// (see plans/2026-05-13-values-structural-reduction.md Finding 4).
	// The name must now flow through as an unresolved NamedTypeConstraint —
	// asserting type tag (not just Name()) so a future re-introduction
	// of the alias would fail this test rather than silently masquerading.
	t.Run("exact-integer falls through as NamedTypeConstraint", func(t *testing.T) {
		c := qt.New(t)
		result := docparse.ParseValueType("exact-integer")
		_, isNamed := result.(*values.NamedTypeConstraint)
		c.Assert(isNamed, qt.IsTrue)
		c.Assert(result.Name(), qt.Equals, "exact-integer")
	})
}

// typeNames extracts Name() from each TypeConstraint for assertion convenience.
func typeNames(tcs []values.TypeConstraint) []string {
	if tcs == nil {
		return nil
	}
	names := make([]string, len(tcs))
	for i, tc := range tcs {
		names[i] = tc.Name()
	}
	return names
}

func TestParseDocstring(t *testing.T) {
	tcs := []struct {
		name           string
		input          string
		wantDoc        string
		wantSyntax     string
		wantParams     []string
		wantTypeNames  []string
		wantReturnName string // empty means nil ReturnType
		wantCat        string
		wantKeywords   []string
		wantMeta       bool
	}{
		{
			name:     "empty string",
			input:    "",
			wantDoc:  "",
			wantMeta: false,
		},
		{
			name:     "prose only",
			input:    "Returns the length of a list.",
			wantDoc:  "Returns the length of a list.",
			wantMeta: false,
		},
		{
			name:           "full structured",
			input:          "Apply proc to each element of lst.\nParameters:\n  proc : procedure\n  lst : list\nReturns: list\nCategory: lists",
			wantDoc:        "Apply proc to each element of lst.",
			wantParams:     []string{"proc", "lst"},
			wantTypeNames:  []string{"procedure", "list"},
			wantReturnName: "list",
			wantCat:        "lists",
			wantMeta:       true,
		},
		{
			name:     "category only",
			input:    "Add two numbers.\nCategory: arithmetic",
			wantDoc:  "Add two numbers.",
			wantCat:  "arithmetic",
			wantMeta: true,
		},
		{
			name:           "flexible ordering — category before parameters",
			input:          "Transform a list.\nCategory: lists\nParameters:\n  proc : procedure\n  lst : list\nReturns: list",
			wantDoc:        "Transform a list.",
			wantParams:     []string{"proc", "lst"},
			wantTypeNames:  []string{"procedure", "list"},
			wantReturnName: "list",
			wantCat:        "lists",
			wantMeta:       true,
		},
		{
			name:     "examples section preserved in prose",
			input:    "Compute factorial.\nExamples:\n  (factorial 5) => 120",
			wantDoc:  "Compute factorial.\nExamples:\n  (factorial 5) => 120",
			wantMeta: false,
		},
		{
			name:     "see also preserved in prose",
			input:    "Reverse a list.\nSee also: append, map",
			wantDoc:  "Reverse a list.\nSee also: append, map",
			wantMeta: false,
		},
		{
			name:          "unknown param type preserves name",
			input:         "Do something.\nParameters:\n  x : frobnicate",
			wantDoc:       "Do something.",
			wantParams:    []string{"x"},
			wantTypeNames: []string{"frobnicate"},
			wantMeta:      true,
		},
		{
			name:           "parameters with no prose before them",
			input:          "Parameters:\n  x : number\nReturns: number",
			wantDoc:        "",
			wantParams:     []string{"x"},
			wantTypeNames:  []string{"number"},
			wantReturnName: "number",
			wantMeta:       true,
		},
		{
			name:       "syntax with category — special form style",
			input:      "Conditional expression. R7RS §4.1.5.\nSyntax: (if <test> <consequent> <alternate>)\nCategory: conditionals",
			wantDoc:    "Conditional expression. R7RS §4.1.5.",
			wantSyntax: "(if <test> <consequent> <alternate>)",
			wantCat:    "conditionals",
			wantMeta:   true,
		},
		{
			name:       "syntax only — no category",
			input:      "Short description.\nSyntax: (lambda <formals> <body>)",
			wantDoc:    "Short description.",
			wantSyntax: "(lambda <formals> <body>)",
			wantMeta:   true,
		},
		{
			name:          "parameters followed by examples",
			input:         "Do stuff.\nParameters:\n  x : number\n\nExamples:\n  (do-stuff 1) => 2",
			wantDoc:       "Do stuff.\n\nExamples:\n  (do-stuff 1) => 2",
			wantParams:    []string{"x"},
			wantTypeNames: []string{"number"},
			wantMeta:      true,
		},
		{
			name:       "syntax with examples preserved in prose",
			input:      "Binding form. R7RS §4.2.2.\nSyntax: (let ((<var> <init>) ...) <body>)\nCategory: binding\n\nExamples:\n  (let ((x 1)) x)  => 1",
			wantDoc:    "Binding form. R7RS §4.2.2.\n\nExamples:\n  (let ((x 1)) x)  => 1",
			wantSyntax: "(let ((<var> <init>) ...) <body>)",
			wantCat:    "binding",
			wantMeta:   true,
		},
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
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			info := docparse.ParseDocstring(tc.input)

			c.Assert(info.Doc, qt.Equals, tc.wantDoc)
			c.Assert(info.Syntax, qt.Equals, tc.wantSyntax)
			c.Assert(info.ParamNames, qt.DeepEquals, tc.wantParams)
			c.Assert(typeNames(info.ParamTypes), qt.DeepEquals, tc.wantTypeNames)
			if tc.wantReturnName == "" {
				c.Assert(info.ReturnType, qt.IsNil)
			} else {
				c.Assert(info.ReturnType.Name(), qt.Equals, tc.wantReturnName)
			}
			c.Assert(info.Category, qt.Equals, tc.wantCat)
			c.Assert(info.Keywords, qt.DeepEquals, tc.wantKeywords)
			c.Assert(info.HasStructuredMetadata(), qt.Equals, tc.wantMeta)
		})
	}
}

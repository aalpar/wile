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

	"github.com/aalpar/wile/internal/docparse"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestParseValueType(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected values.ValueType
	}{
		{
			name:     "known type procedure",
			input:    "procedure",
			expected: values.TypeProcedure,
		},
		{
			name:     "known type list",
			input:    "list",
			expected: values.TypeList,
		},
		{
			name:     "known type exact-integer",
			input:    "exact-integer",
			expected: values.TypeExactInteger,
		},
		{
			name:     "unknown type",
			input:    "frobnicate",
			expected: values.TypeAny,
		},
		{
			name:     "empty string",
			input:    "",
			expected: values.TypeAny,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(docparse.ParseValueType(tc.input), qt.Equals, tc.expected)
		})
	}
}

func TestParseDocstring(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		wantDoc    string
		wantSyntax string
		wantParams []string
		wantTypes  []values.ValueType
		wantReturn values.ValueType
		wantCat    string
		wantMeta   bool
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
			name:       "full structured",
			input:      "Apply proc to each element of lst.\nParameters:\n  proc : procedure\n  lst : list\nReturns: list\nCategory: lists",
			wantDoc:    "Apply proc to each element of lst.",
			wantParams: []string{"proc", "lst"},
			wantTypes:  []values.ValueType{values.TypeProcedure, values.TypeList},
			wantReturn: values.TypeList,
			wantCat:    "lists",
			wantMeta:   true,
		},
		{
			name:       "category only",
			input:      "Add two numbers.\nCategory: arithmetic",
			wantDoc:    "Add two numbers.",
			wantReturn: values.TypeAny,
			wantCat:    "arithmetic",
			wantMeta:   true,
		},
		{
			name:       "flexible ordering — category before parameters",
			input:      "Transform a list.\nCategory: lists\nParameters:\n  proc : procedure\n  lst : list\nReturns: list",
			wantDoc:    "Transform a list.",
			wantParams: []string{"proc", "lst"},
			wantTypes:  []values.ValueType{values.TypeProcedure, values.TypeList},
			wantReturn: values.TypeList,
			wantCat:    "lists",
			wantMeta:   true,
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
			name:       "unknown param type becomes TypeAny",
			input:      "Do something.\nParameters:\n  x : frobnicate",
			wantDoc:    "Do something.",
			wantParams: []string{"x"},
			wantTypes:  []values.ValueType{values.TypeAny},
			wantReturn: values.TypeAny,
			wantMeta:   true,
		},
		{
			name:       "parameters with no prose before them",
			input:      "Parameters:\n  x : number\nReturns: number",
			wantDoc:    "",
			wantParams: []string{"x"},
			wantTypes:  []values.ValueType{values.TypeNumber},
			wantReturn: values.TypeNumber,
			wantMeta:   true,
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
			name:       "parameters followed by examples",
			input:      "Do stuff.\nParameters:\n  x : number\n\nExamples:\n  (do-stuff 1) => 2",
			wantDoc:    "Do stuff.\n\nExamples:\n  (do-stuff 1) => 2",
			wantParams: []string{"x"},
			wantTypes:  []values.ValueType{values.TypeNumber},
			wantReturn: values.TypeAny,
			wantMeta:   true,
		},
		{
			name:       "syntax with examples preserved in prose",
			input:      "Binding form. R7RS §4.2.2.\nSyntax: (let ((<var> <init>) ...) <body>)\nCategory: binding\n\nExamples:\n  (let ((x 1)) x)  => 1",
			wantDoc:    "Binding form. R7RS §4.2.2.\n\nExamples:\n  (let ((x 1)) x)  => 1",
			wantSyntax: "(let ((<var> <init>) ...) <body>)",
			wantCat:    "binding",
			wantMeta:   true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			info := docparse.ParseDocstring(tc.input)

			c.Assert(info.Doc, qt.Equals, tc.wantDoc)
			c.Assert(info.Syntax, qt.Equals, tc.wantSyntax)
			c.Assert(info.ParamNames, qt.DeepEquals, tc.wantParams)
			c.Assert(info.ParamTypes, qt.DeepEquals, tc.wantTypes)
			c.Assert(info.ReturnType, qt.Equals, tc.wantReturn)
			c.Assert(info.Category, qt.Equals, tc.wantCat)
			c.Assert(info.HasStructuredMetadata(), qt.Equals, tc.wantMeta)
		})
	}
}

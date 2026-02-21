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

package parser

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestParser_Brackets tests R7RS §2.1 square bracket support.
// Square brackets [ and ] are equivalent to ( and ) but must match.
func TestParser_Brackets(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name   string
		in     string
		expect values.Value
	}{
		{
			name:   "empty bracket list",
			in:     "[]",
			expect: values.EmptyList,
		},
		{
			name:   "bracket list with integers",
			in:     "[1 2 3]",
			expect: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name:   "bracket improper list",
			in:     "[a . b]",
			expect: values.NewCons(values.NewSymbol("a"), values.NewSymbol("b")),
		},
		{
			name:   "nested brackets",
			in:     "[[a] [b]]",
			expect: values.List(values.List(values.NewSymbol("a")), values.List(values.NewSymbol("b"))),
		},
		{
			name:   "mixed parens and brackets - outer paren",
			in:     "([a] (b))",
			expect: values.List(values.List(values.NewSymbol("a")), values.List(values.NewSymbol("b"))),
		},
		{
			name:   "mixed parens and brackets - outer bracket",
			in:     "[(a) [b]]",
			expect: values.List(values.List(values.NewSymbol("a")), values.List(values.NewSymbol("b"))),
		},
		{
			name:   "quote with bracket",
			in:     "'[a b]",
			expect: values.List(values.NewSymbol("quote"), values.List(values.NewSymbol("a"), values.NewSymbol("b"))),
		},
		{
			name:   "bracket list single element",
			in:     "[x]",
			expect: values.List(values.NewSymbol("x")),
		},
		{
			name: "bracket improper list multiple elements",
			in:   "[a b . c]",
			expect: values.NewCons(
				values.NewSymbol("a"),
				values.NewCons(values.NewSymbol("b"), values.NewSymbol("c")),
			),
		},
		{
			name:   "deeply nested mixed delimiters",
			in:     "[([a])]",
			expect: values.List(values.List(values.List(values.NewSymbol("a")))),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			env := environment.NewTopLevelEnvironment().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			stx, err := p.ReadSyntax(context.Background())
			c.Assert(err, qt.IsNil)
			c.Assert(stx.UnwrapAll(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_BracketMismatch tests that bracket/paren mismatches are detected.
func TestParser_BracketMismatch(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name        string
		in          string
		errContains string
	}{
		{
			name:        "open paren close bracket",
			in:          "(a]",
			errContains: "mismatched delimiters",
		},
		{
			name:        "open bracket close paren",
			in:          "[a)",
			errContains: "mismatched delimiters",
		},
		{
			name:        "nested mismatch - inner",
			in:          "[(a])",
			errContains: "mismatched delimiters",
		},
		{
			name:        "improper list mismatch",
			in:          "(a . b]",
			errContains: "mismatched delimiters",
		},
		{
			name:        "improper list mismatch bracket",
			in:          "[a . b)",
			errContains: "mismatched delimiters",
		},
		{
			name:        "unexpected close bracket at top level",
			in:          "]",
			errContains: "unexpected close ]",
		},
		{
			name:        "unexpected close paren at top level",
			in:          ")",
			errContains: "unexpected close )",
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			env := environment.NewTopLevelEnvironment().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			_, err := p.ReadSyntax(context.Background())
			c.Assert(err, qt.IsNotNil)
			c.Assert(err.Error(), qt.Contains, tc.errContains)
		})
	}
}

// TestParser_BracketDatumLabels tests bracket support with datum labels.
func TestParser_BracketDatumLabels(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name   string
		in     string
		expect values.Value
	}{
		{
			name:   "datum label with bracket list",
			in:     "#0=[a b]",
			expect: values.List(values.NewSymbol("a"), values.NewSymbol("b")),
		},
		{
			name:   "datum label reference in bracket",
			in:     "[#0=a #0#]",
			expect: values.List(values.NewSymbol("a"), values.NewSymbol("a")),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			env := environment.NewTopLevelEnvironment().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			stx, err := p.ReadSyntax(context.Background())
			c.Assert(err, qt.IsNil)
			// For datum label assignments, unwrap the assignment to get the actual list
			unwrapped := stx.UnwrapAll()
			c.Assert(unwrapped, valuestest.SchemeEquals, tc.expect)
		})
	}
}

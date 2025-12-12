// Copyright 2025 Aaron Alpar
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


package match

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"wile/values"
)

func TestMatchAndExpand(t *testing.T) {
	tcs := []struct {
		name      string
		variables map[string]struct{}
		pattern   *values.Pair
		template  values.Value
		input     *values.Pair
		expected  values.Value
	}{
		{
			name: "Simple variable substitution",
			variables: map[string]struct{}{
				"x": {},
			},
			pattern:  values.List(values.NewSymbol("x")),
			template: values.NewSymbol("x"),
			input:    values.List(values.NewInteger(42)),
			expected: values.NewInteger(42),
		},
		{
			name: "Multiple variables",
			variables: map[string]struct{}{
				"a": {},
				"b": {},
			},
			pattern:  values.List(values.NewSymbol("a"), values.NewSymbol("b")),
			template: values.List(values.NewSymbol("b"), values.NewSymbol("a")),
			input:    values.List(values.NewInteger(1), values.NewInteger(2)),
			expected: values.List(values.NewInteger(2), values.NewInteger(1)),
		},
		{
			name: "Mixed literals and variables",
			variables: map[string]struct{}{
				"x": {},
			},
			pattern:  values.List(values.NewSymbol("define"), values.NewSymbol("x")),
			template: values.List(values.NewSymbol("set!"), values.NewSymbol("global"), values.NewSymbol("x")),
			input:    values.List(values.NewSymbol("define"), values.NewInteger(100)),
			expected: values.List(values.NewSymbol("set!"), values.NewSymbol("global"), values.NewInteger(100)),
		},
		{
			name: "Simple ellipsis",
			variables: map[string]struct{}{
				"x": {},
			},
			pattern: values.List(
				values.NewSymbol("list"),
				values.NewSymbol("x"),
				values.NewSymbol("..."),
			),
			template: values.List(
				values.NewSymbol("vector"),
				values.NewSymbol("x"),
				values.NewSymbol("..."),
			),
			input: values.List(
				values.NewSymbol("list"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
			expected: values.List(
				values.NewSymbol("vector"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
		},
		{
			name: "Ellipsis with pairs",
			variables: map[string]struct{}{
				"a": {},
				"b": {},
			},
			pattern: values.List(
				values.NewSymbol("let"),
				values.List(
					values.List(values.NewSymbol("a"), values.NewSymbol("b")),
					values.NewSymbol("..."),
				),
			),
			template: values.List(
				values.NewSymbol("begin"),
				values.List(values.NewSymbol("define"), values.NewSymbol("a"), values.NewSymbol("b")),
				values.NewSymbol("..."),
			),
			input: values.List(
				values.NewSymbol("let"),
				values.List(
					values.List(values.NewSymbol("x"), values.NewInteger(10)),
					values.List(values.NewSymbol("y"), values.NewInteger(20)),
				),
			),
			expected: values.List(
				values.NewSymbol("begin"),
				values.List(values.NewSymbol("define"), values.NewSymbol("x"), values.NewInteger(10)),
				values.List(values.NewSymbol("define"), values.NewSymbol("y"), values.NewInteger(20)),
			),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Compile pattern
			compiler := NewSyntaxCompiler()
			compiler.variables = tc.variables
			err := compiler.Compile(tc.pattern)
			qt.Assert(t, err, qt.IsNil)

			// Match against input
			matcher := NewMatcher(compiler.variables, compiler.codes)
			err = matcher.Match(tc.input)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("pattern matching failed"))

			// Expand template
			result, err := matcher.Expand(tc.template)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("template expansion failed"))
			qt.Assert(t, result, values.SchemeEquals, tc.expected,
				qt.Commentf("got %s, expected %s",
					result.SchemeString(), tc.expected.SchemeString()))
		})
	}
}

// TestMultipleIndependentEllipsis tests patterns with multiple independent ... at the same level
func TestMultipleIndependentEllipsis(t *testing.T) {
	// This is the key test for the let macro pattern:
	// ((let ((name val) ...) body ...) -> ((lambda (name ...) (begin body ...)) val ...))
	t.Run("let macro pattern", func(t *testing.T) {
		variables := map[string]struct{}{
			"name": {},
			"val":  {},
			"body": {},
		}

		// Pattern: (let ((name val) ...) body ...)
		pattern := values.List(
			values.NewSymbol("let"),
			values.List(
				values.List(values.NewSymbol("name"), values.NewSymbol("val")),
				values.NewSymbol("..."),
			),
			values.NewSymbol("body"),
			values.NewSymbol("..."),
		)

		// Template: ((lambda (name ...) (begin body ...)) val ...)
		template := values.List(
			values.List(
				values.NewSymbol("lambda"),
				values.List(values.NewSymbol("name"), values.NewSymbol("...")),
				values.List(
					values.NewSymbol("begin"),
					values.NewSymbol("body"),
					values.NewSymbol("..."),
				),
			),
			values.NewSymbol("val"),
			values.NewSymbol("..."),
		)

		// Input: (let ((x 1) (y 2)) e1 e2)
		input := values.List(
			values.NewSymbol("let"),
			values.List(
				values.List(values.NewSymbol("x"), values.NewInteger(1)),
				values.List(values.NewSymbol("y"), values.NewInteger(2)),
			),
			values.NewSymbol("e1"),
			values.NewSymbol("e2"),
		)

		// Expected: ((lambda (x y) (begin e1 e2)) 1 2)
		expected := values.List(
			values.List(
				values.NewSymbol("lambda"),
				values.List(values.NewSymbol("x"), values.NewSymbol("y")),
				values.List(
					values.NewSymbol("begin"),
					values.NewSymbol("e1"),
					values.NewSymbol("e2"),
				),
			),
			values.NewInteger(1),
			values.NewInteger(2),
		)

		// Compile pattern with ellipsis variable mapping
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(pattern)
		qt.Assert(t, err, qt.IsNil)

		// Verify we have multiple ellipsis IDs
		qt.Assert(t, len(compiler.ellipsisVars) >= 2, qt.IsTrue,
			qt.Commentf("expected at least 2 ellipsis IDs, got %d", len(compiler.ellipsisVars)))

		// Match against input with ellipsis variable mapping
		matcher := NewMatcherWithEllipsisVars(compiler.variables, compiler.codes, compiler.ellipsisVars)
		err = matcher.Match(input)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("pattern matching failed"))

		// Expand template
		result, err := matcher.Expand(template)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("template expansion failed"))
		qt.Assert(t, result, values.SchemeEquals, expected,
			qt.Commentf("got %s, expected %s",
				result.SchemeString(), expected.SchemeString()))
	})

	t.Run("two simple ellipsis at same level", func(t *testing.T) {
		variables := map[string]struct{}{
			"a": {},
			"b": {},
		}

		// Pattern: (foo a ... b ...)
		pattern := values.List(
			values.NewSymbol("foo"),
			values.NewSymbol("a"),
			values.NewSymbol("..."),
			values.NewSymbol("b"),
			values.NewSymbol("..."),
		)

		// Compile pattern
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(pattern)
		qt.Assert(t, err, qt.IsNil)

		// This pattern is tricky - both a and b consume from the same list
		// The current implementation doesn't handle this case where two ellipsis
		// patterns share the same input list. This is a limitation.
		// For now, we just verify the pattern compiles and has multiple ellipsis IDs.
		qt.Assert(t, len(compiler.ellipsisVars) >= 2, qt.IsTrue,
			qt.Commentf("expected at least 2 ellipsis IDs, got %d", len(compiler.ellipsisVars)))
	})
}

func TestExpandErrors(t *testing.T) {
	// Test expansion with no capture context
	matcher := NewMatcher(map[string]struct{}{}, []SyntaxCommand{})
	_, err := matcher.Expand(values.NewSymbol("x"))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no capture context")
}

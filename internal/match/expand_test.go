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
	"context"
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestMatchAndExpand(t *testing.T) {
	list := func(vs ...values.Value) *values.Pair { return values.List(vs...).(*values.Pair) }
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
			pattern:  list(values.NewSymbol("x")),
			template: values.NewSymbol("x"),
			input:    list(values.NewInteger(42)),
			expected: values.NewInteger(42),
		},
		{
			name: "Multiple variables",
			variables: map[string]struct{}{
				"a": {},
				"b": {},
			},
			pattern:  list(values.NewSymbol("a"), values.NewSymbol("b")),
			template: values.List(values.NewSymbol("b"), values.NewSymbol("a")),
			input:    list(values.NewInteger(1), values.NewInteger(2)),
			expected: values.List(values.NewInteger(2), values.NewInteger(1)),
		},
		{
			name: "Mixed literals and variables",
			variables: map[string]struct{}{
				"x": {},
			},
			pattern:  list(values.NewSymbol("define"), values.NewSymbol("x")),
			template: values.List(values.NewSymbol("set!"), values.NewSymbol("global"), values.NewSymbol("x")),
			input:    list(values.NewSymbol("define"), values.NewInteger(100)),
			expected: values.List(values.NewSymbol("set!"), values.NewSymbol("global"), values.NewInteger(100)),
		},
		{
			name: "Simple ellipsis",
			variables: map[string]struct{}{
				"x": {},
			},
			pattern: list(
				values.NewSymbol("list"),
				values.NewSymbol("x"),
				values.NewSymbol("..."),
			),
			template: values.List(
				values.NewSymbol("vector"),
				values.NewSymbol("x"),
				values.NewSymbol("..."),
			),
			input: list(
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
			pattern: list(
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
			input: list(
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
			err := compiler.Compile(context.TODO(), tc.pattern)
			qt.Assert(t, err, qt.IsNil)

			// Match against input
			matcher := NewMatcher(compiler.variables, compiler.codes)
			err = matcher.Match(context.Background(), tc.input)
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
	list := func(vs ...values.Value) *values.Pair { return values.List(vs...).(*values.Pair) }
	// This is the key test for the let macro pattern:
	// ((let ((name val) ...) body ...) -> ((lambda (name ...) (begin body ...)) val ...))
	t.Run("let macro pattern", func(t *testing.T) {
		variables := map[string]struct{}{
			"name": {},
			"val":  {},
			"body": {},
		}

		// Pattern: (let ((name val) ...) body ...)
		pattern := list(
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
		input := list(
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
		err := compiler.Compile(context.TODO(), pattern)
		qt.Assert(t, err, qt.IsNil)

		// Verify we have multiple ellipsis IDs
		qt.Assert(t, len(compiler.ellipsisVars) >= 2, qt.IsTrue,
			qt.Commentf("expected at least 2 ellipsis IDs, got %d", len(compiler.ellipsisVars)))

		// Match against input with ellipsis variable mapping
		matcher := NewMatcherWithEllipsisVars(compiler.variables, compiler.codes, compiler.ellipsisVars)
		err = matcher.Match(context.Background(), input)
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
		pattern := list(
			values.NewSymbol("foo"),
			values.NewSymbol("a"),
			values.NewSymbol("..."),
			values.NewSymbol("b"),
			values.NewSymbol("..."),
		)

		// Compile pattern
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.TODO(), pattern)
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

// TestEllipsisEscape tests R7RS §4.3.2 ellipsis escape form.
// A template of the form (... <template>) is identical to <template>,
// except that ellipses within the template have no special meaning.
func TestEllipsisEscape(t *testing.T) {
	list := func(vs ...values.Value) *values.Pair { return values.List(vs...).(*values.Pair) }
	t.Run("Simple ellipsis escape", func(t *testing.T) {
		// Pattern: (foo x)
		// Template: (... ...) - the (... ...) escape form should produce literal ...
		// Expected: ... (a single ellipsis symbol)
		variables := map[string]struct{}{"x": {}}
		pattern := list(values.NewSymbol("foo"), values.NewSymbol("x"))
		input := list(values.NewSymbol("foo"), values.NewInteger(42))

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.Background(), pattern)
		qt.Assert(t, err, qt.IsNil)

		matcher := NewMatcher(variables, compiler.codes)
		matcher.ellipsisID = DefaultEllipsis
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil)

		// Template: (... ...) - escape form containing ellipsis
		escapeTemplate := values.List(values.NewSymbol("..."), values.NewSymbol("..."))
		result, err := matcher.Expand(escapeTemplate)
		qt.Assert(t, err, qt.IsNil)

		// Result should be the literal ... symbol
		sym, ok := result.(*values.Symbol)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected symbol, got %T", result))
		qt.Assert(t, sym.Key, qt.Equals, "...")
	})

	t.Run("Escape form with pattern variable", func(t *testing.T) {
		// Pattern: (foo x)
		// Template: (... (x ...)) - escape form containing x and ...
		// Expected: (42 ...) - x substituted, ... kept literally
		variables := map[string]struct{}{"x": {}}
		pattern := list(values.NewSymbol("foo"), values.NewSymbol("x"))
		input := list(values.NewSymbol("foo"), values.NewInteger(42))

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.Background(), pattern)
		qt.Assert(t, err, qt.IsNil)

		matcher := NewMatcher(variables, compiler.codes)
		matcher.ellipsisID = DefaultEllipsis
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil)

		// Template: (... (x ...)) - escape form containing pattern var and ellipsis
		innerTemplate := values.List(values.NewSymbol("x"), values.NewSymbol("..."))
		escapeTemplate := values.List(values.NewSymbol("..."), innerTemplate)
		result, err := matcher.Expand(escapeTemplate)
		qt.Assert(t, err, qt.IsNil)

		// Result should be (42 ...)
		resultPair, ok := result.(*values.Pair)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected pair, got %T", result))

		// First element should be 42
		firstInt, ok := resultPair[0].(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected integer, got %T", resultPair[0]))
		qt.Assert(t, firstInt.Value, qt.Equals, int64(42))

		// Second element should be (... ()) - a list containing ... and empty list
		restPair, ok := resultPair[1].(*values.Pair)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected pair, got %T", resultPair[1]))

		// The car of rest should be the ... symbol
		ellipsisSym, ok := restPair[0].(*values.Symbol)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected symbol, got %T", restPair[0]))
		qt.Assert(t, ellipsisSym.Key, qt.Equals, "...")
	})
}

// TestEllipsisInMiddle tests R7RS §4.3.2 patterns with ellipsis in the middle of a list.
// For example: (_ a b c ... x y) where the ellipsis is followed by additional pattern elements.
func TestEllipsisInMiddle(t *testing.T) {
	list := func(vs ...values.Value) *values.Pair { return values.List(vs...).(*values.Pair) }
	t.Run("Simple ellipsis in middle - zero iterations", func(t *testing.T) {
		// Pattern: (_ a c ... x y)
		// Input: (_ 1 6 7)
		// Expected: a=1, c=<none>, x=6, y=7
		variables := map[string]struct{}{"a": {}, "c": {}, "x": {}, "y": {}}
		pattern := list(
			values.NewSymbol("_"),
			values.NewSymbol("a"),
			values.NewSymbol("c"),
			values.NewSymbol("..."),
			values.NewSymbol("x"),
			values.NewSymbol("y"),
		)
		input := list(
			values.NewSymbol("_"),
			values.NewInteger(1),
			values.NewInteger(6),
			values.NewInteger(7),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.Background(), pattern)
		qt.Assert(t, err, qt.IsNil)

		// Check bytecode includes SkipIfTailCount
		hasSkipIfTailCount := false
		for _, code := range compiler.codes {
			if _, ok := code.(ByteCodeSkipIfTailCount); ok { //nolint:gocritic
				hasSkipIfTailCount = true
				break
			}
		}
		qt.Assert(t, hasSkipIfTailCount, qt.IsTrue, qt.Commentf("expected SkipIfTailCount in bytecode"))

		matcher := NewMatcherWithEllipsisVars(variables, compiler.codes, compiler.ellipsisVars)
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("pattern matching failed"))

		bindings := matcher.GetBindings()
		qt.Assert(t, bindings["a"].UnwrapAll(), values.SchemeEquals, values.NewInteger(1))
		qt.Assert(t, bindings["x"].UnwrapAll(), values.SchemeEquals, values.NewInteger(6))
		qt.Assert(t, bindings["y"].UnwrapAll(), values.SchemeEquals, values.NewInteger(7))
	})

	t.Run("Simple ellipsis in middle - multiple iterations", func(t *testing.T) {
		// Pattern: (_ a c ... x y)
		// Input: (_ 1 2 3 4 5 6 7)
		// Expected: a=1, c=2,3,4,5 (4 iterations), x=6, y=7
		variables := map[string]struct{}{"a": {}, "c": {}, "x": {}, "y": {}}
		pattern := list(
			values.NewSymbol("_"),
			values.NewSymbol("a"),
			values.NewSymbol("c"),
			values.NewSymbol("..."),
			values.NewSymbol("x"),
			values.NewSymbol("y"),
		)
		input := list(
			values.NewSymbol("_"),
			values.NewInteger(1),
			values.NewInteger(2),
			values.NewInteger(3),
			values.NewInteger(4),
			values.NewInteger(5),
			values.NewInteger(6),
			values.NewInteger(7),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.Background(), pattern)
		qt.Assert(t, err, qt.IsNil)

		matcher := NewMatcherWithEllipsisVars(variables, compiler.codes, compiler.ellipsisVars)
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("pattern matching failed"))

		bindings := matcher.GetBindings()
		qt.Assert(t, bindings["a"].UnwrapAll(), values.SchemeEquals, values.NewInteger(1))
		qt.Assert(t, bindings["x"].UnwrapAll(), values.SchemeEquals, values.NewInteger(6))
		qt.Assert(t, bindings["y"].UnwrapAll(), values.SchemeEquals, values.NewInteger(7))
	})

	t.Run("Ellipsis in middle with template expansion", func(t *testing.T) {
		// Pattern: (_ a c ... x y)
		// Input: (_ 1 2 3 6 7)
		// Template: (list a x y)
		// Expected: (list 1 6 7)
		variables := map[string]struct{}{"a": {}, "c": {}, "x": {}, "y": {}}
		pattern := list(
			values.NewSymbol("_"),
			values.NewSymbol("a"),
			values.NewSymbol("c"),
			values.NewSymbol("..."),
			values.NewSymbol("x"),
			values.NewSymbol("y"),
		)
		input := list(
			values.NewSymbol("_"),
			values.NewInteger(1),
			values.NewInteger(2),
			values.NewInteger(3),
			values.NewInteger(6),
			values.NewInteger(7),
		)
		template := values.List(
			values.NewSymbol("list"),
			values.NewSymbol("a"),
			values.NewSymbol("x"),
			values.NewSymbol("y"),
		)
		expected := values.List(
			values.NewSymbol("list"),
			values.NewInteger(1),
			values.NewInteger(6),
			values.NewInteger(7),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.Background(), pattern)
		qt.Assert(t, err, qt.IsNil)

		matcher := NewMatcherWithEllipsisVars(variables, compiler.codes, compiler.ellipsisVars)
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("pattern matching failed"))

		result, err := matcher.Expand(template)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("template expansion failed"))
		qt.Assert(t, result, values.SchemeEquals, expected)
	})
}

// TestImproperListPattern tests R7RS §4.3.2 improper list patterns like (_ a . rest).
func TestImproperListPattern(t *testing.T) {
	list := func(vs ...values.Value) *values.Pair { return values.List(vs...).(*values.Pair) }
	t.Run("Simple improper list pattern", func(t *testing.T) {
		// Pattern: (_ a . rest)
		// Input: (_ 1 2 3)
		// Expected: a=1, rest=(2 3)
		variables := map[string]struct{}{"a": {}, "rest": {}}
		// Create improper list: (_ a . rest)
		pattern := values.NewCons(
			values.NewSymbol("_"),
			values.NewCons(
				values.NewSymbol("a"),
				values.NewSymbol("rest"), // improper tail
			),
		)
		input := list(
			values.NewSymbol("_"),
			values.NewInteger(1),
			values.NewInteger(2),
			values.NewInteger(3),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.Background(), pattern)
		qt.Assert(t, err, qt.IsNil)

		// Check bytecode includes CaptureCdr
		hasCaptureCdr := false
		for _, code := range compiler.codes {
			if _, ok := code.(ByteCodeCaptureCdr); ok { //nolint:gocritic
				hasCaptureCdr = true
				break
			}
		}
		qt.Assert(t, hasCaptureCdr, qt.IsTrue, qt.Commentf("expected CaptureCdr in bytecode, got: %v", compiler.codes))

		matcher := NewMatcherWithEllipsisVars(variables, compiler.codes, compiler.ellipsisVars)
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("pattern matching failed"))

		bindings := matcher.GetBindings()
		qt.Assert(t, bindings["a"].UnwrapAll(), values.SchemeEquals, values.NewInteger(1))
		qt.Assert(t, bindings["rest"].UnwrapAll(), values.SchemeEquals, values.List(values.NewInteger(2), values.NewInteger(3)))
	})

	t.Run("Improper list pattern with single element rest", func(t *testing.T) {
		// Pattern: (_ a . rest)
		// Input: (_ 1 2)
		// Expected: a=1, rest=(2)
		variables := map[string]struct{}{"a": {}, "rest": {}}
		pattern := values.NewCons(
			values.NewSymbol("_"),
			values.NewCons(
				values.NewSymbol("a"),
				values.NewSymbol("rest"),
			),
		)
		input := list(
			values.NewSymbol("_"),
			values.NewInteger(1),
			values.NewInteger(2),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.Background(), pattern)
		qt.Assert(t, err, qt.IsNil)

		matcher := NewMatcherWithEllipsisVars(variables, compiler.codes, compiler.ellipsisVars)
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("pattern matching failed"))

		bindings := matcher.GetBindings()
		qt.Assert(t, bindings["a"].UnwrapAll(), values.SchemeEquals, values.NewInteger(1))
		qt.Assert(t, bindings["rest"].UnwrapAll(), values.SchemeEquals, values.List(values.NewInteger(2)))
	})
}

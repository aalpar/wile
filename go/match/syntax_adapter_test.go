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
	"wile/syntax"
	"wile/values"
)

func TestSyntaxMatcher(t *testing.T) {
	t.Run("NewSyntaxMatcher", func(t *testing.T) {
		variables := map[string]struct{}{
			"x": {},
		}
		codes := []SyntaxCommand{
			ByteCodeCaptureCar{Binding: "x"},
			ByteCodeDone{},
		}

		matcher := NewSyntaxMatcher(variables, codes)
		qt.Assert(t, matcher, qt.IsNotNil)
		qt.Assert(t, matcher.matcher, qt.IsNotNil)
		qt.Assert(t, matcher.syntaxMap, qt.IsNotNil)
	})

	t.Run("NewSyntaxMatcherWithEllipsisVars", func(t *testing.T) {
		variables := map[string]struct{}{
			"x": {},
		}
		codes := []SyntaxCommand{
			ByteCodeCaptureCar{Binding: "x"},
			ByteCodeDone{},
		}
		ellipsisVars := map[int]map[string]struct{}{
			0: {"x": {}},
		}

		matcher := NewSyntaxMatcherWithEllipsisVars(variables, codes, ellipsisVars)
		qt.Assert(t, matcher, qt.IsNotNil)
		qt.Assert(t, matcher.matcher.ellipsisVars, qt.DeepEquals, ellipsisVars)
	})

	t.Run("Match and Expand", func(t *testing.T) {
		variables := map[string]struct{}{
			"x": {},
		}

		// Compile pattern: (define x)
		pattern := values.List(
			values.NewSymbol("define"),
			values.NewSymbol("x"),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(pattern)
		qt.Assert(t, err, qt.IsNil)

		// Create syntax input
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		input := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("define", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxObject(values.NewInteger(42), srcCtx),
				syntax.NewSyntaxEmptyList(srcCtx),
				srcCtx,
			),
			srcCtx,
		)

		// Match
		matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
		err = matcher.Match(input)
		qt.Assert(t, err, qt.IsNil)

		// Expand template: x
		template := syntax.NewSyntaxSymbol("x", srcCtx)
		result, err := matcher.Expand(template)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})

	t.Run("Match error on non-pair", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeDone{},
		}

		matcher := NewSyntaxMatcher(variables, codes)
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		input := syntax.NewSyntaxObject(values.NewInteger(42), srcCtx)

		err := matcher.Match(input)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, "requires a pair")
	})

	t.Run("ExpandWithIntroScope", func(t *testing.T) {
		variables := map[string]struct{}{
			"x": {},
		}

		pattern := values.List(
			values.NewSymbol("define"),
			values.NewSymbol("x"),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(pattern)
		qt.Assert(t, err, qt.IsNil)

		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		input := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("define", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("foo", srcCtx),
				syntax.NewSyntaxEmptyList(srcCtx),
				srcCtx,
			),
			srcCtx,
		)

		matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
		err = matcher.Match(input)
		qt.Assert(t, err, qt.IsNil)

		// Create intro scope
		introScope := syntax.NewScope(nil)
		freeIds := make(map[string]struct{})

		template := syntax.NewSyntaxSymbol("x", srcCtx)
		result, err := matcher.ExpandWithIntroScope(template, introScope, freeIds)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})
}

func TestCompileSyntaxPattern(t *testing.T) {
	t.Run("CompileSyntaxPattern simple", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		pattern := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("define", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("x", srcCtx),
				syntax.NewSyntaxEmptyList(srcCtx),
				srcCtx,
			),
			srcCtx,
		)

		variables := map[string]struct{}{
			"x": {},
		}

		codes, err := CompileSyntaxPattern(pattern, variables)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, codes, qt.IsNotNil)
		qt.Assert(t, len(codes) > 0, qt.IsTrue)
	})

	t.Run("CompileSyntaxPatternFull", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		pattern := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("let", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("x", srcCtx),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxSymbol("...", srcCtx),
					syntax.NewSyntaxEmptyList(srcCtx),
					srcCtx,
				),
				srcCtx,
			),
			srcCtx,
		)

		variables := map[string]struct{}{
			"x": {},
		}

		compiled, err := CompileSyntaxPatternFull(pattern, variables)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, compiled, qt.IsNotNil)
		qt.Assert(t, compiled.Codes, qt.IsNotNil)
		qt.Assert(t, compiled.EllipsisVars, qt.IsNotNil)
	})

	t.Run("CompileSyntaxPattern error on non-pair", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		pattern := syntax.NewSyntaxObject(values.NewInteger(42), srcCtx)

		variables := map[string]struct{}{}

		codes, err := CompileSyntaxPattern(pattern, variables)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, "must be a list")
		qt.Assert(t, codes, qt.IsNil)
	})
}

func TestSyntaxToValue(t *testing.T) {
	t.Run("SyntaxSymbol to Symbol", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		stx := syntax.NewSyntaxSymbol("foo", srcCtx)

		val := syntaxToValue(stx)
		sym, ok := val.(*values.Symbol)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, sym.Key, qt.Equals, "foo")
	})

	t.Run("SyntaxPair to Pair", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		stx := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("a", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("b", srcCtx),
				syntax.NewSyntaxEmptyList(srcCtx),
				srcCtx,
			),
			srcCtx,
		)

		val := syntaxToValue(stx)
		pair, ok := val.(*values.Pair)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, pair, qt.IsNotNil)
	})

	t.Run("SyntaxObject to underlying value", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		stx := syntax.NewSyntaxObject(values.NewInteger(42), srcCtx)

		val := syntaxToValue(stx)
		num, ok := val.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, num.Value, qt.Equals, int64(42))
	})

	t.Run("nil syntax", func(t *testing.T) {
		val := syntaxToValue(nil)
		qt.Assert(t, val, qt.IsNil)
	})

	t.Run("Empty list", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		stx := syntax.NewSyntaxEmptyList(srcCtx)

		val := syntaxToValue(stx)
		qt.Assert(t, values.IsEmptyList(val), qt.IsTrue)
	})
}

func TestValueToSyntax(t *testing.T) {
	srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})

	t.Run("Symbol to SyntaxSymbol", func(t *testing.T) {
		val := values.NewSymbol("foo")
		stx := valueToSyntax(val, syntax.NewSyntaxSymbol("template", srcCtx))

		sym, ok := stx.(*syntax.SyntaxSymbol)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, sym.Key, qt.Equals, "foo")
	})

	t.Run("Pair to SyntaxPair", func(t *testing.T) {
		val := values.List(values.NewSymbol("a"), values.NewSymbol("b"))
		stx := valueToSyntax(val, syntax.NewSyntaxSymbol("template", srcCtx))

		pair, ok := stx.(*syntax.SyntaxPair)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, pair, qt.IsNotNil)
	})

	t.Run("Integer to SyntaxObject", func(t *testing.T) {
		val := values.NewInteger(42)
		stx := valueToSyntax(val, syntax.NewSyntaxSymbol("template", srcCtx))

		obj, ok := stx.(*syntax.SyntaxObject)
		qt.Assert(t, ok, qt.IsTrue)
		num, ok := obj.Datum.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, num.Value, qt.Equals, int64(42))
	})

	t.Run("Float to SyntaxObject", func(t *testing.T) {
		val := values.NewFloat(3.14)
		stx := valueToSyntax(val, syntax.NewSyntaxSymbol("template", srcCtx))

		obj, ok := stx.(*syntax.SyntaxObject)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, obj.Datum, qt.IsNotNil)
	})

	t.Run("String to SyntaxObject", func(t *testing.T) {
		val := values.NewString("hello")
		stx := valueToSyntax(val, syntax.NewSyntaxSymbol("template", srcCtx))

		obj, ok := stx.(*syntax.SyntaxObject)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, obj.Datum, qt.IsNotNil)
	})

	t.Run("Boolean to SyntaxObject", func(t *testing.T) {
		val := values.NewBoolean(true)
		stx := valueToSyntax(val, syntax.NewSyntaxSymbol("template", srcCtx))

		obj, ok := stx.(*syntax.SyntaxObject)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, obj.Datum, qt.IsNotNil)
	})

	t.Run("Character to SyntaxObject", func(t *testing.T) {
		val := values.NewCharacter('x')
		stx := valueToSyntax(val, syntax.NewSyntaxSymbol("template", srcCtx))

		obj, ok := stx.(*syntax.SyntaxObject)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, obj.Datum, qt.IsNotNil)
	})

	t.Run("nil value", func(t *testing.T) {
		stx := valueToSyntax(nil, syntax.NewSyntaxSymbol("template", srcCtx))
		qt.Assert(t, stx, qt.IsNil)
	})

	t.Run("Empty list", func(t *testing.T) {
		stx := valueToSyntax(values.EmptyList, syntax.NewSyntaxSymbol("template", srcCtx))
		pair, ok := stx.(*syntax.SyntaxPair)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, pair.IsEmptyList(), qt.IsTrue)
	})
}

// TestExpandWithUseSite verifies that ExpandWithUseSite uses the use-site
// source context for newly created syntax objects instead of the template's context.
func TestExpandWithUseSite(t *testing.T) {
	c := qt.New(t)

	// Set up a simple pattern (macro x) that captures x
	variables := map[string]struct{}{
		"x": {},
	}

	pattern := values.List(
		values.NewSymbol("macro"),
		values.NewSymbol("x"),
	)

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(pattern)
	c.Assert(err, qt.IsNil)

	// Template source context (where macro is defined)
	templateSc := syntax.NewSourceContext("(let ((tmp x)) tmp)", "macro.scm",
		syntax.NewSourceIndexes(0, 0, 10),
		syntax.NewSourceIndexes(20, 20, 10))

	// Use-site source context (where macro is invoked)
	useSiteSc := syntax.NewSourceContext("(macro 42)", "user.scm",
		syntax.NewSourceIndexes(0, 0, 5),
		syntax.NewSourceIndexes(10, 10, 5))

	// Create input syntax with use-site context
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", useSiteSc),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(42), useSiteSc),
			syntax.NewSyntaxEmptyList(useSiteSc),
			useSiteSc,
		),
		useSiteSc,
	)

	// Match the input
	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = matcher.Match(input)
	c.Assert(err, qt.IsNil)

	// Create a template with templateSc context
	// Template: (let ((tmp x)) tmp)
	template := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("let", templateSc),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxCons(
				syntax.NewSyntaxCons(
					syntax.NewSyntaxSymbol("tmp", templateSc),
					syntax.NewSyntaxCons(
						syntax.NewSyntaxSymbol("x", templateSc),
						syntax.NewSyntaxEmptyList(templateSc),
						templateSc,
					),
					templateSc,
				),
				syntax.NewSyntaxEmptyList(templateSc),
				templateSc,
			),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("tmp", templateSc),
				syntax.NewSyntaxEmptyList(templateSc),
				templateSc,
			),
			templateSc,
		),
		templateSc,
	)

	// Expand with use-site context
	introScope := syntax.NewScope(nil)
	freeIds := map[string]struct{}{"let": {}}
	result, err := matcher.ExpandWithUseSite(template, introScope, freeIds, useSiteSc)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)

	// The result should have use-site source context for newly created elements
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.File, qt.Equals, "user.scm")
	c.Assert(resultSc.Start.Line(), qt.Equals, 5) // Use-site line
}

// TestExpandWithUseSite_PreservesPatternVars verifies that pattern variable
// substitutions preserve their original source context (not use-site).
func TestExpandWithUseSite_PreservesPatternVars(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{
		"x": {},
	}

	pattern := values.List(
		values.NewSymbol("test"),
		values.NewSymbol("x"),
	)

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(pattern)
	c.Assert(err, qt.IsNil)

	// Source context for the captured value
	capturedSc := syntax.NewSourceContext("original-value", "input.scm",
		syntax.NewSourceIndexes(5, 5, 3),
		syntax.NewSourceIndexes(15, 15, 3))

	// Use-site source context
	useSiteSc := syntax.NewSourceContext("(test val)", "main.scm",
		syntax.NewSourceIndexes(0, 0, 1),
		syntax.NewSourceIndexes(10, 10, 1))

	// Create input with specific context for the captured value
	inputSc := syntax.NewSourceContext("(test val)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 3),
		syntax.NewSourceIndexes(10, 10, 3))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("val", capturedSc), // This should be preserved
			syntax.NewSyntaxEmptyList(inputSc),
			inputSc,
		),
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = matcher.Match(input)
	c.Assert(err, qt.IsNil)

	// Template: just returns x (the pattern variable)
	templateSc := syntax.NewSourceContext("x", "template.scm",
		syntax.NewSourceIndexes(0, 0, 100),
		syntax.NewSourceIndexes(1, 1, 100))
	template := syntax.NewSyntaxSymbol("x", templateSc)

	// Expand
	result, err := matcher.ExpandWithUseSite(template, nil, nil, useSiteSc)
	c.Assert(err, qt.IsNil)

	// The result should preserve the original captured value's context
	// (not use-site, not template)
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.File, qt.Equals, "input.scm")
	c.Assert(resultSc.Start.Line(), qt.Equals, 3) // Original captured line
}

// TestExpandWithUseSite_NilUseSite verifies that when useSiteCtx is nil,
// the behavior falls back to using template context.
func TestExpandWithUseSite_NilUseSite(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{}

	pattern := values.List(values.NewSymbol("test"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(pattern)
	c.Assert(err, qt.IsNil)

	inputSc := syntax.NewSourceContext("(test)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(6, 6, 1))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.NewSyntaxEmptyList(inputSc),
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = matcher.Match(input)
	c.Assert(err, qt.IsNil)

	// Template with specific context
	templateSc := syntax.NewSourceContext("result", "template.scm",
		syntax.NewSourceIndexes(0, 0, 50), syntax.NewSourceIndexes(6, 6, 50))
	template := syntax.NewSyntaxSymbol("result", templateSc)

	// Expand with nil use-site context
	result, err := matcher.ExpandWithUseSite(template, nil, nil, nil)
	c.Assert(err, qt.IsNil)

	// Should fall back to template context
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.File, qt.Equals, "template.scm")
	c.Assert(resultSc.Start.Line(), qt.Equals, 50)
}

// TestExpandWithOrigin verifies that origin info is attached to expanded syntax.
func TestExpandWithOrigin(t *testing.T) {
	c := qt.New(t)

	// Set up pattern: (test)
	pattern := values.List(values.NewSymbol("test"))
	compiler := NewSyntaxCompiler()
	compiler.variables = map[string]struct{}{}
	err := compiler.Compile(pattern)
	c.Assert(err, qt.IsNil)

	// Create input
	inputSc := syntax.NewSourceContext("(my-macro)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 5), syntax.NewSourceIndexes(10, 10, 5))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.NewSyntaxEmptyList(inputSc),
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = matcher.Match(input)
	c.Assert(err, qt.IsNil)

	// Template
	templateSc := syntax.NewSourceContext("result", "template.scm",
		syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(6, 6, 1))
	template := syntax.NewSyntaxSymbol("result", templateSc)

	// Create origin info
	origin := &syntax.OriginInfo{
		Identifier: "my-macro",
		Location:   inputSc,
	}

	// Expand with origin
	result, err := matcher.ExpandWithOrigin(template, nil, nil, inputSc, origin)
	c.Assert(err, qt.IsNil)

	// Result should have origin attached
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.Origin, qt.IsNotNil)
	c.Assert(resultSc.Origin.Identifier, qt.Equals, "my-macro")
	c.Assert(resultSc.Origin.Location, qt.Equals, inputSc)
}

// TestExpandWithOrigin_ChainedOrigins verifies origin chaining works.
func TestExpandWithOrigin_ChainedOrigins(t *testing.T) {
	c := qt.New(t)

	// Set up pattern: (test)
	pattern := values.List(values.NewSymbol("test"))
	compiler := NewSyntaxCompiler()
	compiler.variables = map[string]struct{}{}
	err := compiler.Compile(pattern)
	c.Assert(err, qt.IsNil)

	// Create input
	inputSc := syntax.NewSourceContext("(outer-macro)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 10), syntax.NewSourceIndexes(13, 13, 10))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.NewSyntaxEmptyList(inputSc),
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = matcher.Match(input)
	c.Assert(err, qt.IsNil)

	// Template
	template := syntax.NewSyntaxSymbol("result", nil)

	// Create chained origin (outer-macro expanded from inner-macro)
	innerOrigin := &syntax.OriginInfo{
		Identifier: "inner-macro",
		Location:   nil,
	}
	outerOrigin := &syntax.OriginInfo{
		Identifier: "outer-macro",
		Location:   inputSc,
		Parent:     innerOrigin,
	}

	// Expand with chained origin
	result, err := matcher.ExpandWithOrigin(template, nil, nil, inputSc, outerOrigin)
	c.Assert(err, qt.IsNil)

	// Result should have full origin chain
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.Origin, qt.IsNotNil)
	c.Assert(resultSc.Origin.Identifier, qt.Equals, "outer-macro")
	c.Assert(resultSc.Origin.Parent, qt.IsNotNil)
	c.Assert(resultSc.Origin.Parent.Identifier, qt.Equals, "inner-macro")
}

// TestExpandWithOrigin_PreservesPatternVars verifies pattern variables keep original syntax.
func TestExpandWithOrigin_PreservesPatternVars(t *testing.T) {
	c := qt.New(t)

	// Set up pattern: (test x) where x is a pattern variable
	pattern := values.List(
		values.NewSymbol("test"),
		values.NewSymbol("x"),
	)
	compiler := NewSyntaxCompiler()
	compiler.variables = map[string]struct{}{"x": {}}
	err := compiler.Compile(pattern)
	c.Assert(err, qt.IsNil)

	// Create input with specific source context for the captured value
	inputSc := syntax.NewSourceContext("(test 42)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(9, 9, 1))
	valueSc := syntax.NewSourceContext("42", "input.scm",
		syntax.NewSourceIndexes(6, 6, 1), syntax.NewSourceIndexes(8, 8, 1))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(42), valueSc),
			syntax.NewSyntaxEmptyList(inputSc),
			inputSc,
		),
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = matcher.Match(input)
	c.Assert(err, qt.IsNil)

	// Template: just x (the pattern variable)
	template := syntax.NewSyntaxSymbol("x", nil)

	// Create origin
	origin := &syntax.OriginInfo{
		Identifier: "test-macro",
		Location:   inputSc,
	}

	// Expand with origin
	result, err := matcher.ExpandWithOrigin(template, nil, nil, inputSc, origin)
	c.Assert(err, qt.IsNil)

	// Pattern variable should preserve original context (NOT have origin added)
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.File, qt.Equals, "input.scm")
	// Pattern variables keep their original context, so no origin should be added
	c.Assert(resultSc.Origin, qt.IsNil)
}

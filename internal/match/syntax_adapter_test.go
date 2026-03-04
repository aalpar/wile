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

package match

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
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

		matcher := NewSyntaxMatcher(variables, codes, nil)
		qt.Assert(t, matcher, qt.IsNotNil)
		qt.Assert(t, matcher.matcher, qt.IsNotNil)
	})

	t.Run("NewSyntaxMatcher with ellipsis vars", func(t *testing.T) {
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

		matcher := NewSyntaxMatcher(variables, codes, &SyntaxMatcherOpts{EllipsisVars: ellipsisVars})
		qt.Assert(t, matcher, qt.IsNotNil)
		qt.Assert(t, matcher.matcher.ellipsisVars, qt.DeepEquals, ellipsisVars)
	})

	t.Run("Match and Expand", func(t *testing.T) {
		variables := map[string]struct{}{
			"x": {},
		}

		// Compile pattern: (define x)
		pattern := testSyntaxList(
			testSyntaxSym("define"),
			testSyntaxSym("x"),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.TODO(), pattern)
		qt.Assert(t, err, qt.IsNil)

		// Create syntax input
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		input := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("define", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxObject(values.NewInteger(42), srcCtx),
				syntax.SyntaxEmptyList,
				srcCtx,
			),
			srcCtx,
		)

		// Match
		matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil)

		// Expand template: x
		template := syntax.NewSyntaxSymbol("x", srcCtx)
		result, err := matcher.Expand(template, ExpandOptions{})
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})

	t.Run("Match error on non-pair", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeDone{},
		}

		matcher := NewSyntaxMatcher(variables, codes, nil)
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		input := syntax.NewSyntaxObject(values.NewInteger(42), srcCtx)

		err := matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, "requires a pair")
	})

	t.Run("ExpandWithIntroScope", func(t *testing.T) {
		variables := map[string]struct{}{
			"x": {},
		}

		pattern := testSyntaxList(
			testSyntaxSym("define"),
			testSyntaxSym("x"),
		)

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.TODO(), pattern)
		qt.Assert(t, err, qt.IsNil)

		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		input := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("define", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("foo", srcCtx),
				syntax.SyntaxEmptyList,
				srcCtx,
			),
			srcCtx,
		)

		matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
		err = matcher.Match(context.Background(), input)
		qt.Assert(t, err, qt.IsNil)

		// Create intro scope
		introScope := syntax.NewScope()
		freeIds := make(map[string]any)

		template := syntax.NewSyntaxSymbol("x", srcCtx)
		result, err := matcher.Expand(template, ExpandOptions{IntroScope: introScope, FreeIds: freeIds})
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
				syntax.SyntaxEmptyList,
				srcCtx,
			),
			srcCtx,
		)

		variables := map[string]struct{}{
			"x": {},
		}

		compiled, err := CompileSyntaxPattern(context.Background(), pattern, variables, nil)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, compiled, qt.IsNotNil)
		qt.Assert(t, len(compiled.Codes) > 0, qt.IsTrue)
	})

	t.Run("CompileSyntaxPattern with ellipsis", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		pattern := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("let", srcCtx),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("x", srcCtx),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxSymbol("...", srcCtx),
					syntax.SyntaxEmptyList,
					srcCtx,
				),
				srcCtx,
			),
			srcCtx,
		)

		variables := map[string]struct{}{
			"x": {},
		}

		compiled, err := CompileSyntaxPattern(context.Background(), pattern, variables, nil)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, compiled, qt.IsNotNil)
		qt.Assert(t, compiled.Codes, qt.IsNotNil)
		qt.Assert(t, compiled.EllipsisVars, qt.IsNotNil)
	})

	t.Run("CompileSyntaxPattern error on non-pair", func(t *testing.T) {
		srcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
		pattern := syntax.NewSyntaxObject(values.NewInteger(42), srcCtx)

		variables := map[string]struct{}{}

		compiled, err := CompileSyntaxPattern(context.Background(), pattern, variables, nil)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, "must be a list")
		qt.Assert(t, compiled, qt.IsNil)
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

	pattern := testSyntaxList(
		testSyntaxSym("macro"),
		testSyntaxSym("x"),
	)

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
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
			syntax.SyntaxEmptyList,
			useSiteSc,
		),
		useSiteSc,
	)

	// Match
	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = matcher.Match(context.Background(), input)
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
						syntax.SyntaxEmptyList,
						templateSc,
					),
					templateSc,
				),
				syntax.SyntaxEmptyList,
				templateSc,
			),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("tmp", templateSc),
				syntax.SyntaxEmptyList,
				templateSc,
			),
			templateSc,
		),
		templateSc,
	)

	// Expand with use-site context
	introScope := syntax.NewScope()
	freeIds := map[string]any{"let": nil}
	result, err := matcher.Expand(template, ExpandOptions{IntroScope: introScope, FreeIds: freeIds, UseSiteCtx: useSiteSc})
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

	pattern := testSyntaxList(
		testSyntaxSym("test"),
		testSyntaxSym("x"),
	)

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.Background(), pattern)
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
			syntax.SyntaxEmptyList,
			inputSc,
		),
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = matcher.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	// Template: just returns x (the pattern variable)
	templateSc := syntax.NewSourceContext("x", "template.scm",
		syntax.NewSourceIndexes(0, 0, 100),
		syntax.NewSourceIndexes(1, 1, 100))
	template := syntax.NewSyntaxSymbol("x", templateSc)

	// Expand
	result, err := matcher.Expand(template, ExpandOptions{UseSiteCtx: useSiteSc})
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

	pattern := testSyntaxList(testSyntaxSym("test"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	inputSc := syntax.NewSourceContext("(test)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(6, 6, 1))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.SyntaxEmptyList,
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = matcher.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	// Template with specific context
	templateSc := syntax.NewSourceContext("result", "template.scm",
		syntax.NewSourceIndexes(0, 0, 50), syntax.NewSourceIndexes(6, 6, 50))
	template := syntax.NewSyntaxSymbol("result", templateSc)

	// Expand with nil use-site context
	result, err := matcher.Expand(template, ExpandOptions{})
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
	pattern := testSyntaxList(testSyntaxSym("test"))
	compiler := NewSyntaxCompiler()
	compiler.variables = map[string]struct{}{}
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	// Create input
	inputSc := syntax.NewSourceContext("(my-macro)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 5), syntax.NewSourceIndexes(10, 10, 5))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.SyntaxEmptyList,
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = matcher.Match(context.Background(), input)
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
	result, err := matcher.Expand(template, ExpandOptions{UseSiteCtx: inputSc, Origin: origin})
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
	pattern := testSyntaxList(testSyntaxSym("test"))
	compiler := NewSyntaxCompiler()
	compiler.variables = map[string]struct{}{}
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	// Create input
	inputSc := syntax.NewSourceContext("(outer-macro)", "input.scm",
		syntax.NewSourceIndexes(0, 0, 10), syntax.NewSourceIndexes(13, 13, 10))
	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("test", inputSc),
		syntax.SyntaxEmptyList,
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = matcher.Match(context.Background(), input)
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
	result, err := matcher.Expand(template, ExpandOptions{UseSiteCtx: inputSc, Origin: outerOrigin})
	c.Assert(err, qt.IsNil)

	// Result should have full origin chain
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.Origin, qt.IsNotNil)
	c.Assert(resultSc.Origin.Identifier, qt.Equals, "outer-macro")
	c.Assert(resultSc.Origin.Parent, qt.IsNotNil)
	c.Assert(resultSc.Origin.Parent.Identifier, qt.Equals, "inner-macro")
}

// TestExpandWithOrigin_StructuralNodes verifies that origin info propagates to
// newly created structural syntax nodes (pairs, vectors, ellipsis-generated lists,
// and escaped template pairs), not just symbols.
//
// Each Expand call stamps its OriginInfo onto every node it creates. The chain
// of successive macro expansions lives inside OriginInfo.Parent, not across
// SourceContext.Origin values on the same node. The caller builds the chain
// before calling Expand (see operation_syntax_rules_transform.go):
//
//	SourceContext (on each expanded node)
//	┌─────────────────────┐
//	│ File: "user.scm"    │
//	│ Text: "(my-macro …" │       OriginInfo
//	│ Origin ─────────────┼──→ ┌──────────────────────┐
//	└─────────────────────┘    │ Identifier: "my-macro"│
//	                           │ Location: useSiteSc   │
//	                           │ Parent: nil           │
//	                           └───────────────────────┘
//
// For nested macros (outer expands, then inner expands the result):
//
//	SourceContext                OriginInfo (inner)          OriginInfo (outer)
//	┌──────────────────┐     ┌───────────────────┐      ┌───────────────────┐
//	│ Origin ───────────┼──→ │ Ident: "inner"    │      │ Ident: "outer"    │
//	└──────────────────┘     │ Parent ────────────┼──→  │ Parent ──→ nil    │
//	                         └───────────────────┘      └───────────────────┘
//
// WithOrigin replaces SourceContext.Origin (last caller wins). This is correct
// because the inner expansion already folded the outer's OriginInfo into Parent.
//
// Regression test for: https://github.com/aalpar/wile/pull/235
func TestExpandWithOrigin_StructuralNodes(t *testing.T) {
	c := qt.New(t)

	// Build the OriginInfo that the caller (operation_syntax_rules_transform)
	// would construct. In a nested scenario, Parent would point to the prior
	// expansion's OriginInfo; here we test a single expansion (Parent: nil).
	origin := &syntax.OriginInfo{
		Identifier: "my-macro",
		Location: syntax.NewSourceContext("(my-macro x)", "call.scm",
			syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(12, 12, 1)),
	}
	useSiteSc := syntax.NewSourceContext("(my-macro 42)", "user.scm",
		syntax.NewSourceIndexes(0, 0, 7), syntax.NewSourceIndexes(13, 13, 7))
	opts := ExpandOptions{UseSiteCtx: useSiteSc, Origin: origin}

	c.Run("pair template carries origin", func(c *qt.C) {
		// Exercises: expandSyntaxValue → *SyntaxPair case
		// Pattern: (macro x), template: (a x) → expanded pair must carry origin.
		variables := map[string]struct{}{"x": {}}
		pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.TODO(), pattern)
		c.Assert(err, qt.IsNil)

		input := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("macro", nil),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxObject(values.NewInteger(42), nil),
				syntax.SyntaxEmptyList,
				nil,
			),
			nil,
		)

		sm := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		template := testSyntaxList(
			syntax.NewSyntaxSymbol("a", nil),
			syntax.NewSyntaxSymbol("x", nil),
		)
		result, err := sm.Expand(template, opts)
		c.Assert(err, qt.IsNil)

		pr, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		sc := pr.SourceContext()
		c.Assert(sc, qt.IsNotNil)
		c.Assert(sc.Origin, qt.IsNotNil, qt.Commentf("pair node should carry origin"))
		c.Assert(sc.Origin.Identifier, qt.Equals, "my-macro")
	})

	c.Run("vector template carries origin", func(c *qt.C) {
		// Exercises: expandSyntaxValue → *SyntaxVector case
		variables := map[string]struct{}{"x": {}}
		pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.TODO(), pattern)
		c.Assert(err, qt.IsNil)

		input := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("macro", nil),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxObject(values.NewInteger(7), nil),
				syntax.SyntaxEmptyList,
				nil,
			),
			nil,
		)

		sm := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		template := syntax.NewSyntaxVector(nil,
			syntax.NewSyntaxSymbol("x", nil),
			syntax.NewSyntaxSymbol("a", nil),
		)
		result, err := sm.Expand(template, opts)
		c.Assert(err, qt.IsNil)

		vec, ok := result.(*syntax.SyntaxVector)
		c.Assert(ok, qt.IsTrue)
		sc := vec.SourceContext()
		c.Assert(sc, qt.IsNotNil)
		c.Assert(sc.Origin, qt.IsNotNil, qt.Commentf("vector node should carry origin"))
		c.Assert(sc.Origin.Identifier, qt.Equals, "my-macro")
	})

	c.Run("ellipsis expansion carries origin", func(c *qt.C) {
		// Exercises: expandSyntaxEllipsis → cons cells built for repetition
		variables := map[string]struct{}{"x": {}}

		// Pattern: (_ x ...)
		pattern := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("_", nil),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("x", nil),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxSymbol("...", nil),
					syntax.SyntaxEmptyList,
					nil,
				),
				nil,
			),
			nil,
		)

		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ 1 2)
		input := testSyntaxList(
			syntax.NewSyntaxSymbol("_", nil),
			syntax.NewSyntaxObject(values.NewInteger(1), nil),
			syntax.NewSyntaxObject(values.NewInteger(2), nil),
		)

		sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: (x ...)
		template := testSyntaxList(
			syntax.NewSyntaxSymbol("x", nil),
			syntax.NewSyntaxSymbol("...", nil),
		)
		result, err := sm.Expand(template, opts)
		c.Assert(err, qt.IsNil)

		pr, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		c.Assert(syntax.IsSyntaxEmptyList(pr), qt.IsFalse)
		sc := pr.SourceContext()
		c.Assert(sc, qt.IsNotNil)
		c.Assert(sc.Origin, qt.IsNotNil, qt.Commentf("ellipsis-generated cons should carry origin"))
		c.Assert(sc.Origin.Identifier, qt.Equals, "my-macro")
	})

	c.Run("escaped template carries origin", func(c *qt.C) {
		// Exercises: expandEscapedSyntaxTemplate → *SyntaxPair case
		variables := map[string]struct{}{"x": {}}
		pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.TODO(), pattern)
		c.Assert(err, qt.IsNil)

		input := syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("macro", nil),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxObject(values.NewInteger(42), nil),
				syntax.SyntaxEmptyList,
				nil,
			),
			nil,
		)

		sm := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: (... (x a)) — escaped, so ... is not treated as repetition
		innerTemplate := testSyntaxList(
			syntax.NewSyntaxSymbol("x", nil),
			syntax.NewSyntaxSymbol("a", nil),
		)
		template := testSyntaxList(
			syntax.NewSyntaxSymbol("...", nil),
			innerTemplate,
		)
		result, err := sm.Expand(template, opts)
		c.Assert(err, qt.IsNil)

		pr, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		sc := pr.SourceContext()
		c.Assert(sc, qt.IsNotNil)
		c.Assert(sc.Origin, qt.IsNotNil, qt.Commentf("escaped template pair should carry origin"))
		c.Assert(sc.Origin.Identifier, qt.Equals, "my-macro")
	})
}

// TestExpandWithOrigin_PreservesPatternVars verifies pattern variables keep original syntax.
func TestExpandWithOrigin_PreservesPatternVars(t *testing.T) {
	c := qt.New(t)

	// Set up pattern: (test x) where x is a pattern variable
	pattern := testSyntaxList(
		testSyntaxSym("test"),
		testSyntaxSym("x"),
	)
	compiler := NewSyntaxCompiler()
	compiler.variables = map[string]struct{}{"x": {}}
	err := compiler.Compile(context.TODO(), pattern)
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
			syntax.SyntaxEmptyList,
			inputSc,
		),
		inputSc,
	)

	matcher := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = matcher.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	// Template: just x (the pattern variable)
	template := syntax.NewSyntaxSymbol("x", nil)

	// Create origin
	origin := &syntax.OriginInfo{
		Identifier: "test-macro",
		Location:   inputSc,
	}

	// Expand with origin
	result, err := matcher.Expand(template, ExpandOptions{UseSiteCtx: inputSc, Origin: origin})
	c.Assert(err, qt.IsNil)

	// Pattern variable should preserve original context (NOT have origin added)
	resultSc := result.SourceContext()
	c.Assert(resultSc, qt.IsNotNil)
	c.Assert(resultSc.File, qt.Equals, "input.scm")
	// Pattern variables keep their original context, so no origin should be added
	c.Assert(resultSc.Origin, qt.IsNil)
}

// mockBindingChecker implements BindingChecker for testing.
type mockBindingChecker struct {
	bindings map[string]*environment.Binding // sym -> binding (nil means no binding)
}

func (p *mockBindingChecker) HasBinding(sym string, scopes []*syntax.Scope) bool {
	binding, ok := p.bindings[sym]
	return ok && binding != nil
}

func (p *mockBindingChecker) GetBinding(sym string, scopes []*syntax.Scope) *environment.Binding {
	return p.bindings[sym]
}

// TestLiteralScopesMatchWithChecker verifies R7RS §4.3.2 literal matching:
// literals match only when both identifiers have the same binding.
func TestLiteralScopesMatchWithChecker(t *testing.T) {
	c := qt.New(t)

	emptySrcCtx := syntax.NewSourceContext("", "", syntax.SourceIndexes{}, syntax.SourceIndexes{})
	sharedBinding := environment.NewBinding(values.NewSymbol("=>"), environment.BindingTypeVariable)
	letScope := syntax.NewScope()
	scopedSrcCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{letScope}}
	inputBinding := environment.NewBinding(values.NewSymbol("=>"), environment.BindingTypeVariable)
	patternBinding := environment.NewBinding(values.NewSymbol("=>"), environment.BindingTypePrimitive)

	tests := []struct {
		name           string
		inputSrcCtx    *syntax.SourceContext
		patternSrcCtx  *syntax.SourceContext
		bindingChecker BindingChecker
		expected       bool
	}{
		{
			name:          "both have same binding",
			inputSrcCtx:   emptySrcCtx,
			patternSrcCtx: emptySrcCtx,
			bindingChecker: &mockBindingChecker{
				bindings: map[string]*environment.Binding{"=>": sharedBinding},
			},
			expected: true,
		},
		{
			name:          "different bindings",
			inputSrcCtx:   scopedSrcCtx,
			patternSrcCtx: emptySrcCtx,
			bindingChecker: &mockBindingCheckerWithScopes{
				inputBinding:   inputBinding,
				patternBinding: patternBinding,
			},
			expected: false,
		},
		{
			name:          "both unbound",
			inputSrcCtx:   emptySrcCtx,
			patternSrcCtx: emptySrcCtx,
			bindingChecker: &mockBindingChecker{
				bindings: map[string]*environment.Binding{},
			},
			expected: true,
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			inputSym := syntax.NewSyntaxSymbol("=>", tt.inputSrcCtx)
			patternSym := syntax.NewSyntaxSymbol("=>", tt.patternSrcCtx)

			literalSyntax := map[string]*syntax.SyntaxSymbol{
				"=>": patternSym,
			}
			matcher := NewSyntaxMatcher(nil, nil, &SyntaxMatcherOpts{EllipsisID: "...", LiteralSyntax: literalSyntax})
			matcher.bindingChecker = tt.bindingChecker

			result := matcher.literalScopesMatchWithChecker(inputSym, patternSym)
			c.Assert(result, qt.Equals, tt.expected)
		})
	}
}

// mockBindingCheckerWithScopes returns different bindings for input vs pattern.
type mockBindingCheckerWithScopes struct {
	inputBinding   *environment.Binding
	patternBinding *environment.Binding
}

func (p *mockBindingCheckerWithScopes) HasBinding(sym string, scopes []*syntax.Scope) bool {
	// Determine if this is input (has scopes) or pattern (no scopes)
	if len(scopes) > 0 {
		return p.inputBinding != nil
	}
	return p.patternBinding != nil
}

func (p *mockBindingCheckerWithScopes) GetBinding(sym string, scopes []*syntax.Scope) *environment.Binding {
	// Determine if this is input (has scopes) or pattern (no scopes)
	if len(scopes) > 0 {
		return p.inputBinding
	}
	return p.patternBinding
}

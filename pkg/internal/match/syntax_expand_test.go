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
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestScopesCompatibleForSubstitution(t *testing.T) {
	c := qt.New(t)

	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()

	tcs := []struct {
		name           string
		templateScopes []*syntax.Scope
		patternScopes  []*syntax.Scope
		expected       bool
	}{
		{
			name:           "both empty scopes",
			templateScopes: nil,
			patternScopes:  nil,
			expected:       true,
		},
		{
			name:           "equal single scope",
			templateScopes: []*syntax.Scope{scopeA},
			patternScopes:  []*syntax.Scope{scopeA},
			expected:       true,
		},
		{
			name:           "equal multiple scopes",
			templateScopes: []*syntax.Scope{scopeA, scopeB},
			patternScopes:  []*syntax.Scope{scopeA, scopeB},
			expected:       true,
		},
		{
			name:           "template has extra scope",
			templateScopes: []*syntax.Scope{scopeA, scopeB},
			patternScopes:  []*syntax.Scope{scopeA},
			expected:       false,
		},
		{
			name:           "pattern has extra scope",
			templateScopes: []*syntax.Scope{scopeA},
			patternScopes:  []*syntax.Scope{scopeA, scopeB},
			expected:       false,
		},
		{
			name:           "disjoint scopes",
			templateScopes: []*syntax.Scope{scopeA},
			patternScopes:  []*syntax.Scope{scopeB},
			expected:       false,
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result := scopesCompatibleForSubstitution(tc.templateScopes, tc.patternScopes)
			c.Assert(result, qt.Equals, tc.expected)
		})
	}
}

func TestFindSyntaxPatternVariables(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name      string
		variables map[string]struct{}
		template  syntax.SyntaxValue
		expected  map[string]struct{}
	}{
		{
			name:      "finds single variable",
			variables: map[string]struct{}{"x": {}},
			template:  syntax.NewSyntaxSymbol("x", nil),
			expected:  map[string]struct{}{"x": {}},
		},
		{
			name:      "no variables found",
			variables: map[string]struct{}{"x": {}},
			template:  syntax.NewSyntaxSymbol("y", nil),
			expected:  map[string]struct{}{},
		},
		{
			name:      "finds multiple variables in list",
			variables: map[string]struct{}{"x": {}, "y": {}},
			template: testSyntaxList(
				syntax.NewSyntaxSymbol("x", nil),
				syntax.NewSyntaxSymbol("y", nil),
			),
			expected: map[string]struct{}{"x": {}, "y": {}},
		},
		{
			name:      "finds variables in nested list",
			variables: map[string]struct{}{"x": {}, "y": {}},
			template: testSyntaxList(
				testSyntaxList(syntax.NewSyntaxSymbol("x", nil)),
				syntax.NewSyntaxSymbol("y", nil),
			),
			expected: map[string]struct{}{"x": {}, "y": {}},
		},
		{
			name:      "ignores non-variable symbols",
			variables: map[string]struct{}{"x": {}},
			template: testSyntaxList(
				syntax.NewSyntaxSymbol("a", nil),
				syntax.NewSyntaxSymbol("x", nil),
				syntax.NewSyntaxSymbol("b", nil),
			),
			expected: map[string]struct{}{"x": {}},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			sm := NewSyntaxMatcher(tc.variables, []SyntaxCommand{ByteCodeDone{}}, nil)
			result := sm.findSyntaxPatternVariables(tc.template)
			c.Assert(result, qt.DeepEquals, tc.expected)
		})
	}
}

func TestCapturedValueToSyntax(t *testing.T) {
	c := qt.New(t)

	sm := NewSyntaxMatcher(map[string]struct{}{}, []SyntaxCommand{ByteCodeDone{}}, nil)

	tcs := []struct {
		name       string
		val        values.Value
		expectType string
	}{
		{
			name:       "syntax value passthrough",
			val:        syntax.NewSyntaxObject(values.NewInteger(42), nil),
			expectType: "SyntaxObject",
		},
		{
			name:       "syntax symbol passthrough",
			val:        syntax.NewSyntaxSymbol("foo", nil),
			expectType: "SyntaxSymbol",
		},
		{
			name:       "integer wrapping",
			val:        values.NewInteger(99),
			expectType: "SyntaxObject",
		},
		{
			name:       "symbol wrapping",
			val:        values.NewSymbol("bar"),
			expectType: "SyntaxSymbol",
		},
		{
			name:       "empty list wrapping",
			val:        values.EmptyList,
			expectType: "SyntaxPair",
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := sm.capturedValueToSyntax(tc.val, &ExpandOptions{})
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.IsNotNil)

			switch tc.expectType {
			case "SyntaxObject":
				_, ok := result.(*syntax.SyntaxObject)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxObject, got %T", result))
			case "SyntaxSymbol":
				_, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxSymbol, got %T", result))
			case "SyntaxPair":
				c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue, qt.Commentf("expected empty list, got %T", result))
			}
		})
	}
}

func TestSyntaxExpandSimpleSubstitution(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		inputVal syntax.SyntaxValue
		template syntax.SyntaxValue
		checkFn  func(c *qt.C, result syntax.SyntaxValue)
	}{
		{
			name:     "variable substitution yields captured integer",
			inputVal: syntax.NewSyntaxObject(values.NewInteger(42), nil),
			template: syntax.NewSyntaxSymbol("x", nil),
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				obj, ok := result.(*syntax.SyntaxObject)
				c.Assert(ok, qt.IsTrue)
				c.Assert(obj.Datum(), qt.DeepEquals, values.NewInteger(42))
			},
		},
		{
			name:     "variable substitution yields captured symbol",
			inputVal: syntax.NewSyntaxSymbol("hello", nil),
			template: syntax.NewSyntaxSymbol("x", nil),
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Key(), qt.Equals, "hello")
			},
		},
		{
			name:     "non-variable symbol returned with hygiene",
			inputVal: syntax.NewSyntaxObject(values.NewInteger(42), nil),
			template: syntax.NewSyntaxSymbol("other", nil),
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Key(), qt.Equals, "other")
			},
		},
		{
			name:     "empty list template returns empty list",
			inputVal: syntax.NewSyntaxObject(values.NewInteger(42), nil),
			template: syntax.SyntaxEmptyList,
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue)
			},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			variables := map[string]struct{}{"x": {}}
			pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

			compiler := NewSyntaxCompiler()
			compiler.variables = variables
			err := compiler.Compile(context.TODO(), pattern)
			c.Assert(err, qt.IsNil)

			input := syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("macro", nil),
				syntax.NewSyntaxCons(
					tc.inputVal,
					syntax.SyntaxEmptyList,
					nil,
				),
				nil,
			)

			sm := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
			err = sm.Match(context.Background(), input)
			c.Assert(err, qt.IsNil)

			result, err := sm.Expand(tc.template, ExpandOptions{})
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.IsNotNil)
			tc.checkFn(c, result)
		})
	}
}

func TestSyntaxExpandWithIntroScope(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name    string
		freeIds map[string]FreeIdResolver
		checkFn func(c *qt.C, result syntax.SyntaxValue, introScope *syntax.Scope)
	}{
		{
			name:    "non-variable symbol gets intro scope",
			freeIds: nil,
			checkFn: func(c *qt.C, result syntax.SyntaxValue, introScope *syntax.Scope) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Key(), qt.Equals, "tmp")
				scopes := sym.Scopes()
				c.Assert(len(scopes), qt.Equals, 1)
				c.Assert(scopes[0], qt.Equals, introScope)
			},
		},
		{
			name: "free identifier with nil resolution still gets intro scope",
			// When freeIds contains a key with nil value, the free ID is recognized
			// but has no resolution. The implementation falls through to the default
			// path which adds intro scope. Only non-nil resolutions with local/global
			// binding providers skip the intro scope.
			freeIds: map[string]FreeIdResolver{"tmp": nil},
			checkFn: func(c *qt.C, result syntax.SyntaxValue, introScope *syntax.Scope) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Key(), qt.Equals, "tmp")
				scopes := sym.Scopes()
				c.Assert(len(scopes), qt.Equals, 1)
				c.Assert(scopes[0], qt.Equals, introScope)
			},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			variables := map[string]struct{}{"x": {}}
			pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

			compiler := NewSyntaxCompiler()
			compiler.variables = variables
			err := compiler.Compile(context.TODO(), pattern)
			c.Assert(err, qt.IsNil)

			input := syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("macro", nil),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxObject(values.NewInteger(1), nil),
					syntax.SyntaxEmptyList,
					nil,
				),
				nil,
			)

			sm := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
			err = sm.Match(context.Background(), input)
			c.Assert(err, qt.IsNil)

			introScope := syntax.NewScope()
			template := syntax.NewSyntaxSymbol("tmp", nil)
			result, err := sm.Expand(template, ExpandOptions{IntroScope: introScope, FreeIds: tc.freeIds})
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.IsNotNil)
			tc.checkFn(c, result, introScope)
		})
	}
}

func TestSyntaxExpandPairTemplate(t *testing.T) {
	c := qt.New(t)

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

	// Template: (a x) where x is a variable bound to 42
	template := testSyntaxList(
		syntax.NewSyntaxSymbol("a", nil),
		syntax.NewSyntaxSymbol("x", nil),
	)

	result, err := sm.Expand(template, ExpandOptions{})
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)

	resultPair, ok := result.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)

	// Car should be "a" (literal symbol)
	carSym, ok := resultPair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Key(), qt.Equals, "a")

	// Cadr should be integer 42 (substituted)
	cdrPair, ok := resultPair.SyntaxCdr().(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	cadrObj, ok := cdrPair.SyntaxCar().(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue)
	c.Assert(cadrObj.Datum(), qt.DeepEquals, values.NewInteger(42))
}

func TestSyntaxExpandEllipsis(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		inputVal []syntax.SyntaxValue
		checkFn  func(c *qt.C, result syntax.SyntaxValue)
	}{
		{
			name:     "zero repetitions yields empty list",
			inputVal: []syntax.SyntaxValue{},
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue)
			},
		},
		{
			name:     "single repetition",
			inputVal: []syntax.SyntaxValue{syntax.NewSyntaxObject(values.NewInteger(1), nil)},
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				pr, ok := result.(*syntax.SyntaxPair)
				c.Assert(ok, qt.IsTrue)
				c.Assert(syntax.IsSyntaxEmptyList(pr), qt.IsFalse)
				carObj, ok := pr.SyntaxCar().(*syntax.SyntaxObject)
				c.Assert(ok, qt.IsTrue)
				c.Assert(carObj.Datum(), qt.DeepEquals, values.NewInteger(1))
			},
		},
		{
			name: "multiple repetitions",
			inputVal: []syntax.SyntaxValue{
				syntax.NewSyntaxObject(values.NewInteger(10), nil),
				syntax.NewSyntaxObject(values.NewInteger(20), nil),
				syntax.NewSyntaxObject(values.NewInteger(30), nil),
			},
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				// Walk the list and collect unwrapped values
				var collected []int64
				current := result
				for {
					pr, ok := current.(*syntax.SyntaxPair)
					if !ok || syntax.IsSyntaxEmptyList(pr) {
						break
					}
					obj, ok := pr.SyntaxCar().(*syntax.SyntaxObject)
					c.Assert(ok, qt.IsTrue)
					intVal, ok := obj.Datum().(*values.Integer)
					c.Assert(ok, qt.IsTrue)
					collected = append(collected, intVal.Value)
					current = pr.SyntaxCdr()
				}
				c.Assert(collected, qt.DeepEquals, []int64{10, 20, 30})
			},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
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

			// Build input: (_ val1 val2 ...)
			inputElems := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("_", nil)}
			inputElems = append(inputElems, tc.inputVal...)
			input := testSyntaxList(inputElems...)

			sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
			err = sm.Match(context.Background(), input)
			c.Assert(err, qt.IsNil)

			// Template: (x ...)
			template := testSyntaxList(
				syntax.NewSyntaxSymbol("x", nil),
				syntax.NewSyntaxSymbol("...", nil),
			)

			result, err := sm.Expand(template, ExpandOptions{})
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.IsNotNil)
			tc.checkFn(c, result)
		})
	}
}

func TestSyntaxExpandNoContext(t *testing.T) {
	c := qt.New(t)

	sm := NewSyntaxMatcher(map[string]struct{}{}, []SyntaxCommand{}, nil)
	template := syntax.NewSyntaxSymbol("x", nil)

	_, err := sm.Expand(template, ExpandOptions{})
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "no capture context")
}

func TestSyntaxExpandPreservesPatternVarScopes(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	// Create input where the captured value has specific scopes
	capturedScope := syntax.NewScope()
	capturedCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{capturedScope}}
	capturedSym := syntax.NewSyntaxSymbol("myvar", capturedCtx)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			capturedSym,
			syntax.SyntaxEmptyList,
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = sm.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	introScope := syntax.NewScope()
	template := syntax.NewSyntaxSymbol("x", nil)
	result, err := sm.Expand(template, ExpandOptions{IntroScope: introScope})
	c.Assert(err, qt.IsNil)

	// The result should be the captured symbol with its ORIGINAL scopes,
	// NOT the intro scope (pattern variable substitutions preserve their scopes).
	resultSym, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(resultSym.Key(), qt.Equals, "myvar")
	scopes := resultSym.Scopes()
	c.Assert(len(scopes), qt.Equals, 1)
	c.Assert(scopes[0], qt.Equals, capturedScope)
}

func TestSyntaxExpandScopeAwareSubstitution(t *testing.T) {
	c := qt.New(t)

	// Test that pattern variable substitution respects scope compatibility
	// when patternVarSyntax is provided.
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

	// When template and pattern have the same scopes, substitution occurs
	patternVarSyntax := map[string]*syntax.SyntaxSymbol{
		"x": syntax.NewSyntaxSymbol("x", nil),
	}
	template := syntax.NewSyntaxSymbol("x", nil)
	result, err := sm.Expand(template, ExpandOptions{PatternVarSyntax: patternVarSyntax})
	c.Assert(err, qt.IsNil)
	obj, ok := result.(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected substitution, got %T", result))
	c.Assert(obj.Datum(), qt.DeepEquals, values.NewInteger(42))
}

func TestSyntaxExpandScopeAwareNoSubstitution(t *testing.T) {
	c := qt.New(t)

	// When template symbol has extra scopes vs pattern variable, substitution
	// should NOT occur (nested macro hygiene).
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

	// Pattern var "x" has no scopes, but template "x" has an extra scope.
	// This models the case where an outer macro introduced "x" with intro scope.
	outerScope := syntax.NewScope()
	templateCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{outerScope}}
	template := syntax.NewSyntaxSymbol("x", templateCtx)

	patternVarSyntax := map[string]*syntax.SyntaxSymbol{
		"x": syntax.NewSyntaxSymbol("x", nil), // pattern var has no scopes
	}

	result, err := sm.Expand(template, ExpandOptions{PatternVarSyntax: patternVarSyntax})
	c.Assert(err, qt.IsNil)
	// Should NOT substitute - return the symbol as-is (with hygiene applied)
	sym, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol (no substitution), got %T", result))
	c.Assert(sym.Key(), qt.Equals, "x")
}

func TestSyntaxExpandEscapedTemplate(t *testing.T) {
	c := qt.New(t)

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

	tcs := []struct {
		name    string
		checkFn func(c *qt.C)
	}{
		{
			name: "ellipsis escape produces literal ellipsis",
			checkFn: func(c *qt.C) {
				// Template: (... ...) -> should produce literal ...
				template := testSyntaxList(
					syntax.NewSyntaxSymbol("...", nil),
					syntax.NewSyntaxSymbol("...", nil),
				)
				result, err := sm.Expand(template, ExpandOptions{})
				c.Assert(err, qt.IsNil)
				// Result should be the literal ... symbol
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol, got %T", result))
				c.Assert(sym.Key(), qt.Equals, "...")
			},
		},
		{
			name: "escape form substitutes pattern vars but keeps ellipsis literal",
			checkFn: func(c *qt.C) {
				// Template: (... (x ...)) -> should produce (42 ...)
				innerTemplate := testSyntaxList(
					syntax.NewSyntaxSymbol("x", nil),
					syntax.NewSyntaxSymbol("...", nil),
				)
				template := testSyntaxList(
					syntax.NewSyntaxSymbol("...", nil),
					innerTemplate,
				)
				result, err := sm.Expand(template, ExpandOptions{})
				c.Assert(err, qt.IsNil)

				resultPair, ok := result.(*syntax.SyntaxPair)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected pair, got %T", result))

				// First element should be 42 (substituted)
				carObj, ok := resultPair.SyntaxCar().(*syntax.SyntaxObject)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxObject, got %T", resultPair.SyntaxCar()))
				c.Assert(carObj.Datum(), qt.DeepEquals, values.NewInteger(42))

				// Second element should be literal ...
				cdrPair, ok := resultPair.SyntaxCdr().(*syntax.SyntaxPair)
				c.Assert(ok, qt.IsTrue)
				ellipsisSym, ok := cdrPair.SyntaxCar().(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(ellipsisSym.Key(), qt.Equals, "...")
			},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			tc.checkFn(c)
		})
	}
}

func TestSyntaxExpandVectorTemplate(t *testing.T) {
	c := qt.New(t)

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

	// Template: #(x a) where x is a variable
	template := syntax.NewSyntaxVector(nil,
		syntax.NewSyntaxSymbol("x", nil),
		syntax.NewSyntaxSymbol("a", nil),
	)

	result, err := sm.Expand(template, ExpandOptions{})
	c.Assert(err, qt.IsNil)
	vec, ok := result.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxVector, got %T", result))
	c.Assert(len(vec.Values), qt.Equals, 2)

	// First element should be 7 (substituted)
	firstObj, ok := vec.Values[0].(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue)
	c.Assert(firstObj.Datum(), qt.DeepEquals, values.NewInteger(7))

	// Second element should be symbol "a"
	secondSym, ok := vec.Values[1].(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(secondSym.Key(), qt.Equals, "a")
}

func TestSyntaxExpandNilTemplate(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(1), nil),
			syntax.SyntaxEmptyList,
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	err = sm.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	result, err := sm.Expand(nil, ExpandOptions{})
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNil)
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
	freeIds := map[string]FreeIdResolver{"let": nil}
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

// TestSyntaxExpandCrossGroupEllipsis verifies that template expansion correctly
// handles pattern variables from different ellipsis groups used under a single
// template ellipsis. For example:
//
//	(syntax-rules ()
//	  ((_ (a ...) (b ...))
//	   ((list a b) ...)))
//
// The template ellipsis should "zip" the two groups, iterating them in lockstep.
// findMatchingEllipsisIDs (plural) returns all contributing IDs, and the
// cross-group path merges their child contexts per iteration.
func TestSyntaxExpandCrossGroupEllipsis(t *testing.T) {
	c := qt.New(t)

	c.Run("two groups zipped", func(c *qt.C) {
		variables := map[string]struct{}{"a": {}, "b": {}}

		// Pattern: (_ (a ...) (b ...))
		// This has two sibling ellipsis groups.
		pattern := syntax.NewSyntaxCons(
			testSyntaxSym("_"),
			syntax.NewSyntaxCons(
				// (a ...)
				syntax.NewSyntaxCons(
					testSyntaxSym("a"),
					syntax.NewSyntaxCons(
						testSyntaxSym("..."),
						syntax.SyntaxEmptyList,
						nil,
					),
					nil,
				),
				syntax.NewSyntaxCons(
					// (b ...)
					syntax.NewSyntaxCons(
						testSyntaxSym("b"),
						syntax.NewSyntaxCons(
							testSyntaxSym("..."),
							syntax.SyntaxEmptyList,
							nil,
						),
						nil,
					),
					syntax.SyntaxEmptyList,
					nil,
				),
				nil,
			),
			nil,
		)

		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ (1 2 3) (10 20 30))
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxList(testSyntaxInt(1), testSyntaxInt(2), testSyntaxInt(3)),
			testSyntaxList(testSyntaxInt(10), testSyntaxInt(20), testSyntaxInt(30)),
		)

		sm := NewSyntaxMatcher(
			variables,
			compiled.Codes,
			&SyntaxMatcherOpts{
				EllipsisVars:   compiled.EllipsisVars,
				EllipsisDepths: compiled.EllipsisDepths,
			},
		)
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: ((list a b) ...)
		// Under the template ellipsis, both a and b should iterate in lockstep.
		template := testSyntaxList(
			testSyntaxList(testSyntaxSym("list"), testSyntaxSym("a"), testSyntaxSym("b")),
			testSyntaxSym("..."),
		)

		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)

		// Expected: ((list 1 10) (list 2 20) (list 3 30))
		// Walk the result list and verify each element.
		type triple struct {
			Sym  string
			Val1 int64
			Val2 int64
		}
		var collected []triple
		current := result
		for {
			pr, ok := current.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(current) {
				break
			}

			// Each element should be (list N M)
			inner, ok := pr.SyntaxCar().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected pair for inner list, got %T", pr.SyntaxCar()))

			// car = "list" symbol
			sym, ok := inner.SyntaxCar().(*syntax.SyntaxSymbol)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol for 'list', got %T", inner.SyntaxCar()))

			// cadr = integer from group a
			rest1, ok := inner.SyntaxCdr().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue)
			obj1, ok := rest1.SyntaxCar().(*syntax.SyntaxObject)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxObject for 'a' value, got %T", rest1.SyntaxCar()))
			int1, ok := obj1.Datum().(*values.Integer)
			c.Assert(ok, qt.IsTrue)

			// caddr = integer from group b
			rest2, ok := rest1.SyntaxCdr().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue)
			obj2, ok := rest2.SyntaxCar().(*syntax.SyntaxObject)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxObject for 'b' value, got %T", rest2.SyntaxCar()))
			int2, ok := obj2.Datum().(*values.Integer)
			c.Assert(ok, qt.IsTrue)

			collected = append(collected, triple{Sym: sym.Key(), Val1: int1.Value, Val2: int2.Value})
			current = pr.SyntaxCdr()
		}

		c.Assert(len(collected), qt.Equals, 3)
		c.Assert(collected[0], qt.DeepEquals, triple{Sym: "list", Val1: 1, Val2: 10})
		c.Assert(collected[1], qt.DeepEquals, triple{Sym: "list", Val1: 2, Val2: 20})
		c.Assert(collected[2], qt.DeepEquals, triple{Sym: "list", Val1: 3, Val2: 30})
	})

	c.Run("mismatched counts error", func(c *qt.C) {
		variables := map[string]struct{}{"a": {}, "b": {}}

		// Pattern: (_ (a ...) (b ...))
		pattern := syntax.NewSyntaxCons(
			testSyntaxSym("_"),
			syntax.NewSyntaxCons(
				syntax.NewSyntaxCons(
					testSyntaxSym("a"),
					syntax.NewSyntaxCons(
						testSyntaxSym("..."),
						syntax.SyntaxEmptyList,
						nil,
					),
					nil,
				),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxCons(
						testSyntaxSym("b"),
						syntax.NewSyntaxCons(
							testSyntaxSym("..."),
							syntax.SyntaxEmptyList,
							nil,
						),
						nil,
					),
					syntax.SyntaxEmptyList,
					nil,
				),
				nil,
			),
			nil,
		)

		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ (1 2) (10 20 30)) — mismatched lengths
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxList(testSyntaxInt(1), testSyntaxInt(2)),
			testSyntaxList(testSyntaxInt(10), testSyntaxInt(20), testSyntaxInt(30)),
		)

		sm := NewSyntaxMatcher(
			variables,
			compiled.Codes,
			&SyntaxMatcherOpts{
				EllipsisVars:   compiled.EllipsisVars,
				EllipsisDepths: compiled.EllipsisDepths,
			},
		)
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: ((list a b) ...)
		template := testSyntaxList(
			testSyntaxList(testSyntaxSym("list"), testSyntaxSym("a"), testSyntaxSym("b")),
			testSyntaxSym("..."),
		)

		// Expansion should fail because a has 2 elements and b has 3.
		_, err = sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNotNil, qt.Commentf("expected error for mismatched group lengths"))
	})

	c.Run("three groups zipped", func(c *qt.C) {
		variables := map[string]struct{}{"a": {}, "b": {}, "c": {}}

		// Pattern: (_ (a ...) (b ...) (c ...))
		pattern := syntax.NewSyntaxCons(
			testSyntaxSym("_"),
			syntax.NewSyntaxCons(
				// (a ...)
				syntax.NewSyntaxCons(
					testSyntaxSym("a"),
					syntax.NewSyntaxCons(
						testSyntaxSym("..."),
						syntax.SyntaxEmptyList,
						nil,
					),
					nil,
				),
				syntax.NewSyntaxCons(
					// (b ...)
					syntax.NewSyntaxCons(
						testSyntaxSym("b"),
						syntax.NewSyntaxCons(
							testSyntaxSym("..."),
							syntax.SyntaxEmptyList,
							nil,
						),
						nil,
					),
					syntax.NewSyntaxCons(
						// (c ...)
						syntax.NewSyntaxCons(
							testSyntaxSym("c"),
							syntax.NewSyntaxCons(
								testSyntaxSym("..."),
								syntax.SyntaxEmptyList,
								nil,
							),
							nil,
						),
						syntax.SyntaxEmptyList,
						nil,
					),
					nil,
				),
				nil,
			),
			nil,
		)

		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ (1 2) (10 20) (100 200))
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxList(testSyntaxInt(1), testSyntaxInt(2)),
			testSyntaxList(testSyntaxInt(10), testSyntaxInt(20)),
			testSyntaxList(testSyntaxInt(100), testSyntaxInt(200)),
		)

		sm := NewSyntaxMatcher(
			variables,
			compiled.Codes,
			&SyntaxMatcherOpts{
				EllipsisVars:   compiled.EllipsisVars,
				EllipsisDepths: compiled.EllipsisDepths,
			},
		)
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: ((list a b c) ...)
		template := testSyntaxList(
			testSyntaxList(testSyntaxSym("list"), testSyntaxSym("a"), testSyntaxSym("b"), testSyntaxSym("c")),
			testSyntaxSym("..."),
		)

		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)

		// Expected: ((list 1 10 100) (list 2 20 200))
		type quad struct {
			Sym  string
			Val1 int64
			Val2 int64
			Val3 int64
		}
		var collected []quad
		current := result
		for {
			pr, ok := current.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(current) {
				break
			}

			inner, ok := pr.SyntaxCar().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected pair for inner list, got %T", pr.SyntaxCar()))

			sym, ok := inner.SyntaxCar().(*syntax.SyntaxSymbol)
			c.Assert(ok, qt.IsTrue)

			rest1, ok := inner.SyntaxCdr().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue)
			obj1, ok := rest1.SyntaxCar().(*syntax.SyntaxObject)
			c.Assert(ok, qt.IsTrue)
			int1, ok := obj1.Datum().(*values.Integer)
			c.Assert(ok, qt.IsTrue)

			rest2, ok := rest1.SyntaxCdr().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue)
			obj2, ok := rest2.SyntaxCar().(*syntax.SyntaxObject)
			c.Assert(ok, qt.IsTrue)
			int2, ok := obj2.Datum().(*values.Integer)
			c.Assert(ok, qt.IsTrue)

			rest3, ok := rest2.SyntaxCdr().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue)
			obj3, ok := rest3.SyntaxCar().(*syntax.SyntaxObject)
			c.Assert(ok, qt.IsTrue)
			int3, ok := obj3.Datum().(*values.Integer)
			c.Assert(ok, qt.IsTrue)

			collected = append(collected, quad{Sym: sym.Key(), Val1: int1.Value, Val2: int2.Value, Val3: int3.Value})
			current = pr.SyntaxCdr()
		}

		c.Assert(len(collected), qt.Equals, 2)
		c.Assert(collected[0], qt.DeepEquals, quad{Sym: "list", Val1: 1, Val2: 10, Val3: 100})
		c.Assert(collected[1], qt.DeepEquals, quad{Sym: "list", Val1: 2, Val2: 20, Val3: 200})
	})
}

// TestSyntaxExpandNestedEllipsis verifies that template expansion correctly
// handles nested ellipsis (depth > 1). For pattern (_ (a ...) ...) and
// template ((list a ...) ...), the outer ellipsis should iterate over each
// inner group, producing one (list ...) sub-list per outer repetition.
// The excludeEllipsisIDs mechanism ensures the outer expansion consumes
// the outer ID first, then inner expansion selects the inner ID.
func TestSyntaxExpandNestedEllipsis(t *testing.T) {
	c := qt.New(t)

	// Helper to build the pattern and template shared by all sub-tests.
	//
	// Pattern: (_ (a ...) ...)
	// This is: (cons "_" (cons (cons "a" (cons "..." nil)) (cons "..." nil)))
	// The inner (a ...) captures a's at depth 0 within each outer repetition.
	// The outer ... repeats the (a ...) sub-lists.
	buildPatternAndTemplate := func(c *qt.C) (*CompiledPattern, map[string]struct{}) {
		variables := map[string]struct{}{"a": {}}

		pattern := syntax.NewSyntaxCons(
			testSyntaxSym("_"),
			syntax.NewSyntaxCons(
				// (a ...)
				syntax.NewSyntaxCons(
					testSyntaxSym("a"),
					syntax.NewSyntaxCons(
						testSyntaxSym("..."),
						syntax.SyntaxEmptyList,
						nil,
					),
					nil,
				),
				syntax.NewSyntaxCons(
					// outer ...
					testSyntaxSym("..."),
					syntax.SyntaxEmptyList,
					nil,
				),
				nil,
			),
			nil,
		)

		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)
		return compiled, variables
	}

	// Template: ((list a ...) ...)
	// This is: (cons (cons "list" (cons "a" (cons "..." nil))) (cons "..." nil))
	buildTemplate := func() *syntax.SyntaxPair {
		return testSyntaxList(
			testSyntaxList(testSyntaxSym("list"), testSyntaxSym("a"), testSyntaxSym("...")),
			testSyntaxSym("..."),
		)
	}

	c.Run("basic nested ellipsis", func(c *qt.C) {
		compiled, variables := buildPatternAndTemplate(c)

		// Input: (_ (1 2 3) (4 5))
		// Two outer repetitions: first has 3 inner values, second has 2.
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxList(testSyntaxInt(1), testSyntaxInt(2), testSyntaxInt(3)),
			testSyntaxList(testSyntaxInt(4), testSyntaxInt(5)),
		)

		sm := NewSyntaxMatcher(
			variables,
			compiled.Codes,
			&SyntaxMatcherOpts{
				EllipsisVars:   compiled.EllipsisVars,
				EllipsisDepths: compiled.EllipsisDepths,
			},
		)
		err := sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		template := buildTemplate()
		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)

		// Expected: ((list 1 2 3) (list 4 5))
		// Walk the outer list and collect each inner list's structure.
		type innerResult struct {
			Sym    string
			Values []int64
		}
		var collected []innerResult
		current := result
		for {
			pr, ok := current.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(current) {
				break
			}

			// Each outer element should be (list N M ...)
			inner, ok := pr.SyntaxCar().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected pair for inner list, got %T", pr.SyntaxCar()))

			// car = "list" symbol
			sym, ok := inner.SyntaxCar().(*syntax.SyntaxSymbol)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol for 'list', got %T", inner.SyntaxCar()))

			// Walk the rest to collect integer values
			var vals []int64
			rest := inner.SyntaxCdr()
			for {
				rp, ok := rest.(*syntax.SyntaxPair)
				if !ok || syntax.IsSyntaxEmptyList(rest) {
					break
				}
				obj, ok := rp.SyntaxCar().(*syntax.SyntaxObject)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxObject for inner value, got %T", rp.SyntaxCar()))
				intVal, ok := obj.Datum().(*values.Integer)
				c.Assert(ok, qt.IsTrue)
				vals = append(vals, intVal.Value)
				rest = rp.SyntaxCdr()
			}

			collected = append(collected, innerResult{Sym: sym.Key(), Values: vals})
			current = pr.SyntaxCdr()
		}

		c.Assert(len(collected), qt.Equals, 2, qt.Commentf("expected 2 outer elements, got %d", len(collected)))
		c.Assert(collected[0], qt.DeepEquals, innerResult{Sym: "list", Values: []int64{1, 2, 3}})
		c.Assert(collected[1], qt.DeepEquals, innerResult{Sym: "list", Values: []int64{4, 5}})
	})

	c.Run("empty outer nested ellipsis", func(c *qt.C) {
		compiled, variables := buildPatternAndTemplate(c)

		// Input: (_) — no outer repetitions at all
		input := testSyntaxList(
			testSyntaxSym("_"),
		)

		sm := NewSyntaxMatcher(
			variables,
			compiled.Codes,
			&SyntaxMatcherOpts{
				EllipsisVars:   compiled.EllipsisVars,
				EllipsisDepths: compiled.EllipsisDepths,
			},
		)
		err := sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		template := buildTemplate()
		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)

		// Expected: () — empty list
		c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue,
			qt.Commentf("expected empty list for zero outer repetitions, got %T", result))
	})

	c.Run("single outer repetition", func(c *qt.C) {
		compiled, variables := buildPatternAndTemplate(c)

		// Input: (_ (10 20))
		// One outer repetition with two inner values.
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxList(testSyntaxInt(10), testSyntaxInt(20)),
		)

		sm := NewSyntaxMatcher(
			variables,
			compiled.Codes,
			&SyntaxMatcherOpts{
				EllipsisVars:   compiled.EllipsisVars,
				EllipsisDepths: compiled.EllipsisDepths,
			},
		)
		err := sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		template := buildTemplate()
		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)

		// Expected: ((list 10 20))
		// One outer element containing a list with two inner values.
		type innerResult struct {
			Sym    string
			Values []int64
		}
		var collected []innerResult
		current := result
		for {
			pr, ok := current.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(current) {
				break
			}

			inner, ok := pr.SyntaxCar().(*syntax.SyntaxPair)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected pair for inner list, got %T", pr.SyntaxCar()))

			sym, ok := inner.SyntaxCar().(*syntax.SyntaxSymbol)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol for 'list', got %T", inner.SyntaxCar()))

			var vals []int64
			rest := inner.SyntaxCdr()
			for {
				rp, ok := rest.(*syntax.SyntaxPair)
				if !ok || syntax.IsSyntaxEmptyList(rest) {
					break
				}
				obj, ok := rp.SyntaxCar().(*syntax.SyntaxObject)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxObject for inner value, got %T", rp.SyntaxCar()))
				intVal, ok := obj.Datum().(*values.Integer)
				c.Assert(ok, qt.IsTrue)
				vals = append(vals, intVal.Value)
				rest = rp.SyntaxCdr()
			}

			collected = append(collected, innerResult{Sym: sym.Key(), Values: vals})
			current = pr.SyntaxCdr()
		}

		c.Assert(len(collected), qt.Equals, 1, qt.Commentf("expected 1 outer element, got %d", len(collected)))
		c.Assert(collected[0], qt.DeepEquals, innerResult{Sym: "list", Values: []int64{10, 20}})
	})
}

// flattenExpanded converts an expanded syntax tree into a nested []any of
// int64 (for integer leaves) and string (for symbol leaves) so results can be
// compared structurally with qt.DeepEquals.
func flattenExpanded(v syntax.SyntaxValue) any {
	pr, ok := v.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(v) {
		obj, isObj := v.(*syntax.SyntaxObject)
		if isObj {
			iv, isInt := obj.Datum().(*values.Integer)
			if isInt {
				return iv.Value
			}
			return obj.Datum()
		}
		sym, isSym := v.(*syntax.SyntaxSymbol)
		if isSym {
			return sym.Key()
		}
		return []any{} // empty list
	}
	var items []any
	cur := syntax.SyntaxValue(pr)
	for {
		p, isPair := cur.(*syntax.SyntaxPair)
		if !isPair || syntax.IsSyntaxEmptyList(cur) {
			break
		}
		items = append(items, flattenExpanded(p.SyntaxCar()))
		cur = p.SyntaxCdr()
	}
	return items
}

// TestSyntaxExpandDepth0Broadcast verifies that a pattern variable bound at
// ellipsis depth 0 is replicated ("broadcast") into an ellipsis sub-template,
// once per iteration, as required by R7RS §4.3.2 ("a subtemplate followed by
// an ellipsis... the pattern variables it contains" — lower-depth variables
// are replicated). This is a regression for the depth-0-inside-ellipsis bug:
// the per-iteration capture context held only the ellipsis-captured (depth-1)
// bindings, so a depth-0 variable referenced inside the ellipsis fell through
// to hygiene and emitted an unbound reference. The fix walks the capture
// context's ancestor chain, so the property must hold at arbitrary nesting
// depth.
func TestSyntaxExpandDepth0Broadcast(t *testing.T) {
	c := qt.New(t)

	c.Run("depth-0 var broadcast into a depth-1 ellipsis", func(c *qt.C) {
		variables := map[string]struct{}{"x": {}, "e": {}}

		// Pattern: (_ x e ...) — x is depth 0, e is depth 1.
		pattern := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxSym("x"),
			testSyntaxSym("e"),
			testSyntaxSym("..."),
		)
		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ 10 1 2 3)
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxInt(10),
			testSyntaxInt(1),
			testSyntaxInt(2),
			testSyntaxInt(3),
		)

		sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{
			EllipsisVars:   compiled.EllipsisVars,
			EllipsisDepths: compiled.EllipsisDepths,
		})
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: ((x e) ...) — x (depth 0) must appear in each iteration.
		template := testSyntaxList(
			testSyntaxList(testSyntaxSym("x"), testSyntaxSym("e")),
			testSyntaxSym("..."),
		)
		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)

		// Expected: ((10 1) (10 2) (10 3))
		c.Assert(flattenExpanded(result), qt.DeepEquals, []any{
			[]any{int64(10), int64(1)},
			[]any{int64(10), int64(2)},
			[]any{int64(10), int64(3)},
		})
	})

	c.Run("depth-0 var broadcast into a depth-2 nested ellipsis", func(c *qt.C) {
		variables := map[string]struct{}{"x": {}, "e": {}}

		// Pattern: (_ x (e ...) ...) — x is depth 0, e is depth 2.
		pattern := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxSym("x"),
			testSyntaxList(testSyntaxSym("e"), testSyntaxSym("...")),
			testSyntaxSym("..."),
		)
		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ 99 (1 2) (3 4 5))
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxInt(99),
			testSyntaxList(testSyntaxInt(1), testSyntaxInt(2)),
			testSyntaxList(testSyntaxInt(3), testSyntaxInt(4), testSyntaxInt(5)),
		)

		sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{
			EllipsisVars:   compiled.EllipsisVars,
			EllipsisDepths: compiled.EllipsisDepths,
		})
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: (((x e) ...) ...) — x (depth 0) must reach depth 2.
		template := testSyntaxList(
			testSyntaxList(
				testSyntaxList(testSyntaxSym("x"), testSyntaxSym("e")),
				testSyntaxSym("..."),
			),
			testSyntaxSym("..."),
		)
		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)

		// Expected: (((99 1) (99 2)) ((99 3) (99 4) (99 5)))
		c.Assert(flattenExpanded(result), qt.DeepEquals, []any{
			[]any{[]any{int64(99), int64(1)}, []any{int64(99), int64(2)}},
			[]any{[]any{int64(99), int64(3)}, []any{int64(99), int64(4)}, []any{int64(99), int64(5)}},
		})
	})

	c.Run("depth-0 var broadcast into a cross-group zip", func(c *qt.C) {
		// Two sibling ellipsis groups (a ...) and (b ...) are zipped in the
		// template, which drives expandEllipsisCrossGroup; the depth-0 var x
		// must broadcast into each zipped iteration via the merged context's
		// parent link. This is the only path that exercises the merged-context
		// parent wiring — single-group tests do not reach it.
		variables := map[string]struct{}{"x": {}, "a": {}, "b": {}}

		// Pattern: (_ x (a ...) (b ...))
		pattern := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxSym("x"),
			testSyntaxList(testSyntaxSym("a"), testSyntaxSym("...")),
			testSyntaxList(testSyntaxSym("b"), testSyntaxSym("...")),
		)
		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ 7 (1 2) (3 4))
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxInt(7),
			testSyntaxList(testSyntaxInt(1), testSyntaxInt(2)),
			testSyntaxList(testSyntaxInt(3), testSyntaxInt(4)),
		)

		sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{
			EllipsisVars:   compiled.EllipsisVars,
			EllipsisDepths: compiled.EllipsisDepths,
		})
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: ((x a b) ...) — zips a with b, broadcasting x into each.
		template := testSyntaxList(
			testSyntaxList(testSyntaxSym("x"), testSyntaxSym("a"), testSyntaxSym("b")),
			testSyntaxSym("..."),
		)
		result, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)

		// Expected: ((7 1 3) (7 2 4))
		c.Assert(flattenExpanded(result), qt.DeepEquals, []any{
			[]any{int64(7), int64(1), int64(3)},
			[]any{int64(7), int64(2), int64(4)},
		})
	})

	c.Run("depth-0 var used with its own ellipsis is an expansion error", func(c *qt.C) {
		// A depth-0 variable followed by `...` (here `x ...` where x is bound at
		// depth 0) is malformed per R7RS §4.3.2 — the subtemplate has no
		// variable of the matching ellipsis depth. The filter in
		// findMatchingEllipsisIDs must surface this as an error, not silently
		// drop x (which is what "constant template followed by ..." does).
		variables := map[string]struct{}{"x": {}, "e": {}}

		// Pattern: (_ x e ...)
		pattern := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxSym("x"),
			testSyntaxSym("e"),
			testSyntaxSym("..."),
		)
		compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
		c.Assert(err, qt.IsNil)

		// Input: (_ 10 1 2 3)
		input := testSyntaxList(
			testSyntaxSym("_"),
			testSyntaxInt(10),
			testSyntaxInt(1),
			testSyntaxInt(2),
			testSyntaxInt(3),
		)

		sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{
			EllipsisVars:   compiled.EllipsisVars,
			EllipsisDepths: compiled.EllipsisDepths,
		})
		err = sm.Match(context.Background(), input)
		c.Assert(err, qt.IsNil)

		// Template: (x ...) — x is depth 0, so this is ill-formed.
		template := testSyntaxList(testSyntaxSym("x"), testSyntaxSym("..."))
		_, err = sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, werr.ErrExpansion), qt.IsTrue,
			qt.Commentf("expected ErrExpansion, got %v", err))
	})
}

// collectSyntaxInts unwraps a syntax list or vector element slice into int64s.
func collectSyntaxInts(c *qt.C, elems []syntax.SyntaxValue) []int64 {
	q := make([]int64, 0, len(elems))
	for _, elem := range elems {
		obj, ok := elem.(*syntax.SyntaxObject)
		c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxObject, got %T", elem))
		intVal, ok := obj.Datum().(*values.Integer)
		c.Assert(ok, qt.IsTrue)
		q = append(q, intVal.Value)
	}
	return q
}

// TestSyntaxExpandVectorEllipsisTemplate pins R7RS §4.3.2: a vector template's
// elements obey the same rules as a list template's, so `#(x ...)` repeats the
// captured values rather than emitting the literal symbols `x` and `...`.
func TestSyntaxExpandVectorEllipsisTemplate(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	// Pattern: (_ x ...)
	pattern := testSyntaxList(
		testSyntaxSym("_"),
		testSyntaxSym("x"),
		testSyntaxSym("..."),
	)
	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name  string
		input []syntax.SyntaxValue
		want  []int64
	}{
		{
			name:  "zero repetitions yields empty vector",
			input: []syntax.SyntaxValue{},
			want:  []int64{},
		},
		{
			name: "multiple repetitions",
			input: []syntax.SyntaxValue{
				testSyntaxInt(1),
				testSyntaxInt(2),
				testSyntaxInt(3),
			},
			want: []int64{1, 2, 3},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			inputElems := []syntax.SyntaxValue{testSyntaxSym("_")}
			inputElems = append(inputElems, tc.input...)

			sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
			err := sm.Match(context.Background(), testSyntaxList(inputElems...))
			c.Assert(err, qt.IsNil)

			// Template: #(x ...)
			template := testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("..."))

			result, err := sm.Expand(template, ExpandOptions{})
			c.Assert(err, qt.IsNil)

			vec, ok := result.(*syntax.SyntaxVector)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxVector, got %T", result))
			c.Assert(collectSyntaxInts(c, vec.Values), qt.DeepEquals, tc.want)
		})
	}
}

// TestSyntaxExpandVectorEllipsisSublistTemplate covers `#((a b) ...)`: an
// ellipsis over a compound element inside a vector template (R7RS §4.3.2).
func TestSyntaxExpandVectorEllipsisSublistTemplate(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"a": {}, "b": {}}
	// Pattern: (_ (a b) ...)
	pattern := testSyntaxList(
		testSyntaxSym("_"),
		testSyntaxList(testSyntaxSym("a"), testSyntaxSym("b")),
		testSyntaxSym("..."),
	)
	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	// Input: (_ (1 2) (3 4))
	input := testSyntaxList(
		testSyntaxSym("_"),
		testSyntaxList(testSyntaxInt(1), testSyntaxInt(2)),
		testSyntaxList(testSyntaxInt(3), testSyntaxInt(4)),
	)

	sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
	err = sm.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	// Template: #((a b) ...)
	template := testSyntaxVec(
		testSyntaxList(testSyntaxSym("a"), testSyntaxSym("b")),
		testSyntaxSym("..."),
	)

	result, err := sm.Expand(template, ExpandOptions{})
	c.Assert(err, qt.IsNil)

	vec, ok := result.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxVector, got %T", result))
	c.Assert(len(vec.Values), qt.Equals, 2)

	want := [][]int64{{1, 2}, {3, 4}}
	for i, elem := range vec.Values {
		pr, ok := elem.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue, qt.Commentf("element %d: expected SyntaxPair, got %T", i, elem))
		cdr, ok := pr.SyntaxCdr().(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		c.Assert(collectSyntaxInts(c, []syntax.SyntaxValue{pr.SyntaxCar(), cdr.SyntaxCar()}), qt.DeepEquals, want[i])
	}
}

// TestSyntaxExpandEscapedVectorTemplate covers a vector inside an ellipsis
// escape. R7RS §4.3.2: `(... <template>)` suppresses the ellipsis's meaning, not
// pattern-variable substitution — so `x` must still be substituted inside the
// escaped vector while `...` survives as a literal symbol.
func TestSyntaxExpandEscapedVectorTemplate(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))

	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	input := testSyntaxList(testSyntaxSym("macro"), testSyntaxInt(5))

	sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
	err = sm.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	// Template: (... #(x ...))
	template := testSyntaxList(
		testSyntaxSym("..."),
		testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("...")),
	)

	result, err := sm.Expand(template, ExpandOptions{})
	c.Assert(err, qt.IsNil)

	vec, ok := result.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxVector, got %T", result))
	c.Assert(len(vec.Values), qt.Equals, 2)

	obj, ok := vec.Values[0].(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected substituted value, got %T", vec.Values[0]))
	c.Assert(obj.Datum(), qt.DeepEquals, values.NewInteger(5))

	sym, ok := vec.Values[1].(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(sym.Key(), qt.Equals, "...")
}

// TestSyntaxExpandVectorSubPatternUnderEllipsis is the regression guard for a
// vector sub-pattern nested inside an ellipsis group: pattern (_ (x #(a)) ...)
// with template (a ...). R7RS §4.3.2 makes `a` an ellipsis-depth-1 variable, so
// it must drive the repetition. When the analyzer dropped a vector's variables
// from the enclosing subtree, the ellipsis group did not claim `a` and expansion
// failed outright.
func TestSyntaxExpandVectorSubPatternUnderEllipsis(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}, "a": {}}
	// Pattern: (_ (x #(a)) ...)
	pattern := testSyntaxList(
		testSyntaxSym("_"),
		testSyntaxList(
			testSyntaxSym("x"),
			testSyntaxVec(testSyntaxSym("a")),
		),
		testSyntaxSym("..."),
	)
	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	// Input: (_ (1 #(2)) (3 #(4)))
	input := testSyntaxList(
		testSyntaxSym("_"),
		testSyntaxList(testSyntaxInt(1), testSyntaxVec(testSyntaxInt(2))),
		testSyntaxList(testSyntaxInt(3), testSyntaxVec(testSyntaxInt(4))),
	)

	sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
	err = sm.Match(context.Background(), input)
	c.Assert(err, qt.IsNil)

	// Template: (a ...) — only the vector's variable repeats.
	template := testSyntaxList(testSyntaxSym("a"), testSyntaxSym("..."))

	result, err := sm.Expand(template, ExpandOptions{})
	c.Assert(err, qt.IsNil)

	var elems []syntax.SyntaxValue
	for cur := result; !syntax.IsSyntaxEmptyList(cur); {
		pr, ok := cur.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue, qt.Commentf("expected proper list, got %T", cur))
		elems = append(elems, pr.SyntaxCar())
		cur = pr.SyntaxCdr()
	}
	c.Assert(collectSyntaxInts(c, elems), qt.DeepEquals, []int64{2, 4})
}

// TestFreeIdKey_DiscriminatesScopeAndName pins the key contract the FreeIds map
// depends on: (name, scopes) is the identity, and the '|' delimiter is
// unambiguous even when the name itself contains '|'.
func TestFreeIdKey_DiscriminatesScopeAndName(t *testing.T) {
	c := qt.New(t)
	s1 := syntax.NewScope()
	s2 := syntax.NewScope()

	// Same name, same scope set (any order) → same key.
	c.Assert(FreeIdKey("x", []*syntax.Scope{s1, s2}),
		qt.Equals, FreeIdKey("x", []*syntax.Scope{s2, s1}))

	// Same name, different scope sets → different keys. This is the collapse the
	// bare-name key allowed.
	c.Assert(FreeIdKey("x", []*syntax.Scope{s1}),
		qt.Not(qt.Equals), FreeIdKey("x", []*syntax.Scope{s2}))
	c.Assert(FreeIdKey("x", nil),
		qt.Not(qt.Equals), FreeIdKey("x", []*syntax.Scope{s1}))

	// nil and empty scope sets fingerprint identically (both "no scopes").
	c.Assert(FreeIdKey("x", nil),
		qt.Equals, FreeIdKey("x", []*syntax.Scope{}))

	// A name containing '|' does not collide with a different (name, scopes)
	// pair. The fingerprint is digits-and-commas only, so the FIRST '|' delimits
	// it: "<id>|a|b" decomposes only as (fp=<id>, name="a|b").
	c.Assert(FreeIdKey("a|b", []*syntax.Scope{s1}),
		qt.Not(qt.Equals), FreeIdKey("b", []*syntax.Scope{s1}))
}

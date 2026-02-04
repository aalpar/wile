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

	"github.com/aalpar/wile/go/syntax"
	"github.com/aalpar/wile/go/values"

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
			sm := NewSyntaxMatcher(tc.variables, []SyntaxCommand{ByteCodeDone{}})
			result := sm.findSyntaxPatternVariables(tc.template)
			c.Assert(result, qt.DeepEquals, tc.expected)
		})
	}
}

func TestCapturedValueToSyntax(t *testing.T) {
	c := qt.New(t)

	sm := NewSyntaxMatcher(map[string]struct{}{}, []SyntaxCommand{ByteCodeDone{}})

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
			result, err := sm.capturedValueToSyntax(tc.val, nil, nil, nil)
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
				pr, ok := result.(*syntax.SyntaxPair)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxPair, got %T", result))
				c.Assert(syntax.IsSyntaxEmptyList(pr), qt.IsTrue)
			}
		})
	}
}

func TestSyntaxExpandSimpleSubstitution(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		inputVal values.Value
		template syntax.SyntaxValue
		checkFn  func(c *qt.C, result syntax.SyntaxValue)
	}{
		{
			name:     "variable substitution yields captured integer",
			inputVal: values.NewInteger(42),
			template: syntax.NewSyntaxSymbol("x", nil),
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				obj, ok := result.(*syntax.SyntaxObject)
				c.Assert(ok, qt.IsTrue)
				c.Assert(obj.Datum(), qt.DeepEquals, values.NewInteger(42))
			},
		},
		{
			name:     "variable substitution yields captured symbol",
			inputVal: values.NewSymbol("hello"),
			template: syntax.NewSyntaxSymbol("x", nil),
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Sym.Key, qt.Equals, "hello")
			},
		},
		{
			name:     "non-variable symbol returned with hygiene",
			inputVal: values.NewInteger(42),
			template: syntax.NewSyntaxSymbol("other", nil),
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Sym.Key, qt.Equals, "other")
			},
		},
		{
			name:     "empty list template returns empty list",
			inputVal: values.NewInteger(42),
			template: syntax.NewSyntaxEmptyList(nil),
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				pr, ok := result.(*syntax.SyntaxPair)
				c.Assert(ok, qt.IsTrue)
				c.Assert(syntax.IsSyntaxEmptyList(pr), qt.IsTrue)
			},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			variables := map[string]struct{}{"x": {}}
			pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

			compiler := NewSyntaxCompiler()
			compiler.variables = variables
			err := compiler.Compile(context.TODO(), pattern)
			c.Assert(err, qt.IsNil)

			input := syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("macro", nil),
				syntax.NewSyntaxCons(
					valueToSyntaxValue(tc.inputVal),
					syntax.NewSyntaxEmptyList(nil),
					nil,
				),
				nil,
			)

			sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
			err = sm.Match(input)
			c.Assert(err, qt.IsNil)

			result, err := sm.Expand(tc.template)
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
		freeIds map[string]any
		checkFn func(c *qt.C, result syntax.SyntaxValue, introScope *syntax.Scope)
	}{
		{
			name:    "non-variable symbol gets intro scope",
			freeIds: nil,
			checkFn: func(c *qt.C, result syntax.SyntaxValue, introScope *syntax.Scope) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Sym.Key, qt.Equals, "tmp")
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
			freeIds: map[string]any{"tmp": nil},
			checkFn: func(c *qt.C, result syntax.SyntaxValue, introScope *syntax.Scope) {
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue)
				c.Assert(sym.Sym.Key, qt.Equals, "tmp")
				scopes := sym.Scopes()
				c.Assert(len(scopes), qt.Equals, 1)
				c.Assert(scopes[0], qt.Equals, introScope)
			},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			variables := map[string]struct{}{"x": {}}
			pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

			compiler := NewSyntaxCompiler()
			compiler.variables = variables
			err := compiler.Compile(context.TODO(), pattern)
			c.Assert(err, qt.IsNil)

			input := syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("macro", nil),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxObject(values.NewInteger(1), nil),
					syntax.NewSyntaxEmptyList(nil),
					nil,
				),
				nil,
			)

			sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
			err = sm.Match(input)
			c.Assert(err, qt.IsNil)

			introScope := syntax.NewScope()
			template := syntax.NewSyntaxSymbol("tmp", nil)
			result, err := sm.ExpandWithIntroScope(template, introScope, tc.freeIds)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.IsNotNil)
			tc.checkFn(c, result, introScope)
		})
	}
}

func TestSyntaxExpandPairTemplate(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(42), nil),
			syntax.NewSyntaxEmptyList(nil),
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = sm.Match(input)
	c.Assert(err, qt.IsNil)

	// Template: (a x) where x is a variable bound to 42
	template := testSyntaxList(
		syntax.NewSyntaxSymbol("a", nil),
		syntax.NewSyntaxSymbol("x", nil),
	)

	result, err := sm.Expand(template)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)

	resultPair, ok := result.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)

	// Car should be "a" (literal symbol)
	carSym, ok := resultPair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "a")

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
		inputVal []values.Value
		checkFn  func(c *qt.C, result syntax.SyntaxValue)
	}{
		{
			name:     "zero repetitions yields empty list",
			inputVal: []values.Value{},
			checkFn: func(c *qt.C, result syntax.SyntaxValue) {
				pr, ok := result.(*syntax.SyntaxPair)
				c.Assert(ok, qt.IsTrue)
				c.Assert(syntax.IsSyntaxEmptyList(pr), qt.IsTrue)
			},
		},
		{
			name:     "single repetition",
			inputVal: []values.Value{values.NewInteger(1)},
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
			inputVal: []values.Value{
				values.NewInteger(10),
				values.NewInteger(20),
				values.NewInteger(30),
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
						syntax.NewSyntaxEmptyList(nil),
						nil,
					),
					nil,
				),
				nil,
			)

			compiled, err := CompileSyntaxPatternFull(context.TODO(), pattern, variables)
			c.Assert(err, qt.IsNil)

			// Build input: (_ val1 val2 ...)
			inputElems := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("_", nil)}
			for _, v := range tc.inputVal {
				inputElems = append(inputElems, valueToSyntaxValue(v))
			}
			input := testSyntaxList(inputElems...)

			sm := NewSyntaxMatcherWithEllipsisVars(variables, compiled.Codes, compiled.EllipsisVars)
			err = sm.Match(input)
			c.Assert(err, qt.IsNil)

			// Template: (x ...)
			template := testSyntaxList(
				syntax.NewSyntaxSymbol("x", nil),
				syntax.NewSyntaxSymbol("...", nil),
			)

			result, err := sm.Expand(template)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.IsNotNil)
			tc.checkFn(c, result)
		})
	}
}

func TestSyntaxExpandNoContext(t *testing.T) {
	c := qt.New(t)

	sm := NewSyntaxMatcher(map[string]struct{}{}, []SyntaxCommand{})
	template := syntax.NewSyntaxSymbol("x", nil)

	_, err := sm.Expand(template)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "no capture context")
}

func TestSyntaxExpandPreservesPatternVarScopes(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

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
			syntax.NewSyntaxEmptyList(nil),
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = sm.Match(input)
	c.Assert(err, qt.IsNil)

	introScope := syntax.NewScope()
	template := syntax.NewSyntaxSymbol("x", nil)
	result, err := sm.ExpandWithIntroScope(template, introScope, nil)
	c.Assert(err, qt.IsNil)

	// The result should be the captured symbol with its ORIGINAL scopes,
	// NOT the intro scope (pattern variable substitutions preserve their scopes).
	resultSym, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(resultSym.Sym.Key, qt.Equals, "myvar")
	scopes := resultSym.Scopes()
	c.Assert(len(scopes), qt.Equals, 1)
	c.Assert(scopes[0], qt.Equals, capturedScope)
}

func TestSyntaxExpandScopeAwareSubstitution(t *testing.T) {
	c := qt.New(t)

	// Test that pattern variable substitution respects scope compatibility
	// when patternVarSyntax is provided.
	variables := map[string]struct{}{"x": {}}
	pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(42), nil),
			syntax.NewSyntaxEmptyList(nil),
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = sm.Match(input)
	c.Assert(err, qt.IsNil)

	// When template and pattern have the same scopes, substitution occurs
	patternVarSyntax := map[string]*syntax.SyntaxSymbol{
		"x": syntax.NewSyntaxSymbol("x", nil),
	}
	template := syntax.NewSyntaxSymbol("x", nil)
	result, err := sm.ExpandWithPatternVarSyntax(template, nil, nil, nil, nil, patternVarSyntax)
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
	pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(42), nil),
			syntax.NewSyntaxEmptyList(nil),
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = sm.Match(input)
	c.Assert(err, qt.IsNil)

	// Pattern var "x" has no scopes, but template "x" has an extra scope.
	// This models the case where an outer macro introduced "x" with intro scope.
	outerScope := syntax.NewScope()
	templateCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{outerScope}}
	template := syntax.NewSyntaxSymbol("x", templateCtx)

	patternVarSyntax := map[string]*syntax.SyntaxSymbol{
		"x": syntax.NewSyntaxSymbol("x", nil), // pattern var has no scopes
	}

	result, err := sm.ExpandWithPatternVarSyntax(template, nil, nil, nil, nil, patternVarSyntax)
	c.Assert(err, qt.IsNil)
	// Should NOT substitute - return the symbol as-is (with hygiene applied)
	sym, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol (no substitution), got %T", result))
	c.Assert(sym.Sym.Key, qt.Equals, "x")
}

func TestSyntaxExpandEscapedTemplate(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(42), nil),
			syntax.NewSyntaxEmptyList(nil),
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = sm.Match(input)
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
				result, err := sm.Expand(template)
				c.Assert(err, qt.IsNil)
				// Result should be the literal ... symbol
				sym, ok := result.(*syntax.SyntaxSymbol)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol, got %T", result))
				c.Assert(sym.Sym.Key, qt.Equals, "...")
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
				result, err := sm.Expand(template)
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
				c.Assert(ellipsisSym.Sym.Key, qt.Equals, "...")
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
	pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(7), nil),
			syntax.NewSyntaxEmptyList(nil),
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = sm.Match(input)
	c.Assert(err, qt.IsNil)

	// Template: #(x a) where x is a variable
	template := syntax.NewSyntaxVector(nil,
		syntax.NewSyntaxSymbol("x", nil),
		syntax.NewSyntaxSymbol("a", nil),
	)

	result, err := sm.Expand(template)
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
	c.Assert(secondSym.Sym.Key, qt.Equals, "a")
}

func TestSyntaxExpandNilTemplate(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := values.List(values.NewSymbol("macro"), values.NewSymbol("x"))

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	input := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("macro", nil),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(1), nil),
			syntax.NewSyntaxEmptyList(nil),
			nil,
		),
		nil,
	)

	sm := NewSyntaxMatcher(compiler.variables, compiler.codes)
	err = sm.Match(input)
	c.Assert(err, qt.IsNil)

	result, err := sm.ExpandWithPatternVarSyntax(nil, nil, nil, nil, nil, nil)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNil)
}

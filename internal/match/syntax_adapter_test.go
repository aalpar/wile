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

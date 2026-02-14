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
	"testing"

	"github.com/aalpar/wile/internal/syntax"

	qt "github.com/frankban/quicktest"
)

func TestAnalyzePatternWithLiterals(t *testing.T) {
	tcs := []struct {
		name      string
		pattern   *syntax.SyntaxPair
		literals  map[string]struct{}
		isKeyword bool
	}{
		{
			name: "Simple pattern with literals",
			pattern: testSyntaxList(
				testSyntaxSym("define"),
				testSyntaxSym("x"),
				testSyntaxSym("y"),
			),
			literals: map[string]struct{}{
				"define": {},
			},
			isKeyword: true,
		},
		{
			name: "Pattern with no literals",
			pattern: testSyntaxList(
				testSyntaxSym("foo"),
				testSyntaxSym("a"),
				testSyntaxSym("b"),
			),
			literals:  map[string]struct{}{},
			isKeyword: true,
		},
		{
			name: "Pattern with multiple literals",
			pattern: testSyntaxList(
				testSyntaxSym("let"),
				testSyntaxSym("name"),
				testSyntaxSym("else"),
				testSyntaxSym("x"),
			),
			literals: map[string]struct{}{
				"let":  {},
				"else": {},
			},
			isKeyword: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			analysis := AnalyzePatternWithLiterals(tc.pattern, tc.literals, tc.isKeyword)
			qt.Assert(t, analysis, qt.IsNotNil)
			qt.Assert(t, analysis.containsVariables, qt.IsNotNil)
			qt.Assert(t, analysis.variablesInSubtree, qt.IsNotNil)
		})
	}
}

func TestCollectPatternVariables(t *testing.T) {
	tcs := []struct {
		name         string
		pattern      syntax.SyntaxValue
		literals     map[string]struct{}
		isFirst      bool
		expectedVars map[string]struct{}
	}{
		{
			name: "Simple symbol pattern",
			pattern: testSyntaxList(
				testSyntaxSym("define"),
				testSyntaxSym("x"),
			),
			literals: map[string]struct{}{
				"define": {},
			},
			isFirst: true,
			expectedVars: map[string]struct{}{
				"x": {},
			},
		},
		{
			name: "Pattern with ellipsis",
			pattern: testSyntaxList(
				testSyntaxSym("let"),
				testSyntaxSym("x"),
				testSyntaxSym("..."),
			),
			literals: map[string]struct{}{
				"let": {},
			},
			isFirst: true,
			expectedVars: map[string]struct{}{
				"x": {},
			},
		},
		{
			name: "Nested pattern",
			pattern: testSyntaxList(
				testSyntaxSym("lambda"),
				testSyntaxList(testSyntaxSym("x"), testSyntaxSym("y")),
				testSyntaxSym("body"),
			),
			literals: map[string]struct{}{
				"lambda": {},
			},
			isFirst: true,
			expectedVars: map[string]struct{}{
				"x":    {},
				"y":    {},
				"body": {},
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			variables := make(map[string]struct{})
			collectPatternVariables(tc.pattern, tc.literals, tc.isFirst, variables)
			qt.Assert(t, variables, qt.DeepEquals, tc.expectedVars)
		})
	}
}

func TestContainsVariables(t *testing.T) {
	pattern := testSyntaxList(
		testSyntaxSym("define"),
		testSyntaxSym("x"),
		testSyntaxInt(42),
	)
	variables := map[string]struct{}{
		"x": {},
	}

	analysis := AnalyzePattern(pattern, variables)

	// Test nil pair
	qt.Assert(t, analysis.ContainsVariables(nil), qt.IsFalse)

	// Test pattern pair (should contain variables)
	qt.Assert(t, analysis.ContainsVariables(pattern), qt.IsTrue)
}

func TestGetVariables(t *testing.T) {
	pattern := testSyntaxList(
		testSyntaxSym("define"),
		testSyntaxSym("x"),
		testSyntaxSym("y"),
	)
	variables := map[string]struct{}{
		"x": {},
		"y": {},
	}

	analysis := AnalyzePattern(pattern, variables)

	// Test nil pair
	qt.Assert(t, analysis.GetVariables(nil), qt.IsNil)

	// Test pattern pair
	vars := analysis.GetVariables(pattern)
	qt.Assert(t, vars, qt.IsNotNil)
}

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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestContainsVariables(t *testing.T) {
	pattern := testSyntaxList(
		testSyntaxSym("define"),
		testSyntaxSym("x"),
		testSyntaxInt(42),
	)
	variables := values.StringSet{
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
	variables := values.StringSet{
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

// TestGetVariablesVectorSubPattern pins R7RS §4.3.2: a vector sub-pattern's
// variables belong to the enclosing subtree, exactly as a list sub-pattern's do.
// compileEllipsis reads the enclosing pair's variable set to decide what an
// ellipsis group captures, so a name dropped here leaves the group unable to
// claim it and the template fails to expand.
func TestGetVariablesVectorSubPattern(t *testing.T) {
	c := qt.New(t)

	// Sub-pattern: (x #(a)) — the enclosing pair must report BOTH x and a.
	sub := testSyntaxList(
		testSyntaxSym("x"),
		testSyntaxVec(testSyntaxSym("a")),
	)
	// Pattern: (_ (x #(a)) ...)
	pattern := testSyntaxList(
		testSyntaxSym("_"),
		sub,
		testSyntaxSym("..."),
	)
	variables := values.StringSet{
		"x": {},
		"a": {},
	}

	analysis := AnalyzePattern(pattern, variables)

	c.Assert(analysis.ContainsVariables(sub), qt.IsTrue)
	c.Assert(analysis.GetVariables(sub), qt.DeepEquals, values.StringSet{
		"x": {},
		"a": {},
	})
}

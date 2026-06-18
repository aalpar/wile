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
)

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

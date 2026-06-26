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

package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestCompileCondExpand tests cond-expand compilation with various feature
// requirement forms: simple identifiers, and, or, not, else, and library.
//
// Source: compile_cond_expand.go (CompileCondExpand, resolveCondExpandClause,
// parseFeatureRequirement, parseFeatureRequirementList).
func TestCompileCondExpand(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Simple feature present
		{
			Name:     "r7rs feature present",
			Code:     `(cond-expand (r7rs 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "wile feature present",
			Code:     `(cond-expand (wile 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},

		// Feature absent falls through to else
		{
			Name:     "nonexistent feature falls to else",
			Code:     `(cond-expand (nonexistent 'no) (else 'yes))`,
			Expected: values.NewSymbol("yes"),
		},

		// Compound requirements: and
		{
			Name:     "and with all satisfied",
			Code:     `(cond-expand ((and r7rs wile) 'both) (else 'no))`,
			Expected: values.NewSymbol("both"),
		},
		{
			Name:     "and with one unsatisfied",
			Code:     `(cond-expand ((and r7rs nonexistent) 'no) (else 'yes))`,
			Expected: values.NewSymbol("yes"),
		},

		// Compound requirements: or
		{
			Name:     "or with one satisfied",
			Code:     `(cond-expand ((or nonexistent r7rs) 'found) (else 'no))`,
			Expected: values.NewSymbol("found"),
		},
		{
			Name:     "or with none satisfied",
			Code:     `(cond-expand ((or nonexistent also-nonexistent) 'no) (else 'yes))`,
			Expected: values.NewSymbol("yes"),
		},

		// Compound requirements: not
		{
			Name:     "not with unsatisfied inner",
			Code:     `(cond-expand ((not nonexistent) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "not with satisfied inner",
			Code:     `(cond-expand ((not r7rs) 'no) (else 'yes))`,
			Expected: values.NewSymbol("yes"),
		},

		// else clause
		{
			Name:     "else always matches",
			Code:     `(cond-expand (else 'yes))`,
			Expected: values.NewSymbol("yes"),
		},

		// Multi-expression body
		{
			Name:     "multi-expression body returns last",
			Code:     `(cond-expand (r7rs 1 2 3) (else 0))`,
			Expected: values.NewInteger(3),
		},

		// First matching clause wins
		{
			Name:     "first matching clause selected",
			Code:     `(cond-expand (r7rs 'first) (wile 'second) (else 'third))`,
			Expected: values.NewSymbol("first"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestCompileCondExpandErrors tests error conditions for cond-expand.
func TestCompileCondExpandErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "no matching clause", Code: `(cond-expand (nonexistent 'no))`},
		// Phase 8 Task 8G(ii): R7RS §4.2.1 — else must be the last clause. A
		// non-final else is a malformed cond-expand and must error at compile time,
		// not silently shadow the clauses after it.
		{Name: "else not last", Code: `(cond-expand (else 'first) (r7rs 'second))`},
		{Name: "else before another else", Code: `(cond-expand (else 'a) (else 'b))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestCompileCondExpandElseLast pins that a final else clause is honored (the
// complement of the else-not-last error): a present feature is selected before
// else, and else is the fallback when no feature matches.
func TestCompileCondExpandElseLast(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "feature before final else wins",
			Code:     `(cond-expand (r7rs 'feature) (else 'fallback))`,
			Expected: values.NewSymbol("feature"),
		},
		{
			Name:     "final else is fallback",
			Code:     `(cond-expand (nonexistent 'no) (else 'fallback))`,
			Expected: values.NewSymbol("fallback"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

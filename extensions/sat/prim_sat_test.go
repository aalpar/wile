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

package sat_test

import (
	"context"
	"slices"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/extensions/sat"
)

// newEngine builds a Wile engine with only the sat extension.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(sat.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// evalString evaluates Scheme source and returns its string representation.
func evalString(t *testing.T, engine *wile.Engine, code string) string {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result.SchemeString()
}

// evalExpectError asserts that the Scheme code raises a Go-side error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestPrimSatCNFFlat(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		// want lists the acceptable result strings. Most cases are
		// deterministic (a single entry); the budget-exhaustion case admits
		// more than one outcome.
		want []string
	}{
		// Single clause (1 2): any assignment making x1 or x2 true works.
		{"single clause SAT", `(sat-cnf-flat? #(1 2 0) #f)`, []string{"#t"}},
		// (x1) ∧ (¬x1): trivially UNSAT.
		{"contradiction UNSAT", `(sat-cnf-flat? #(1 0 -1 0) #f)`, []string{"#f"}},
		// Zero clauses: trivially SAT.
		{"empty vector trivially SAT", `(sat-cnf-flat? #() #f)`, []string{"#t"}},
		// Budget=1000 is ample: still decides.
		{"SAT within budget", `(sat-cnf-flat? #(1 2 0) 1000)`, []string{"#t"}},
		// (x1 ∨ x2) ∧ (¬x1) ∧ (¬x2): UNSAT.
		{"three clauses UNSAT", `(sat-cnf-flat? #(1 2 0 -1 0 -2 0) #f)`, []string{"#f"}},
		// Budget=0 forces UNKNOWN on any instance needing a branch decision.
		// A two-literal clause may still be solved by unit propagation at
		// level 0 before the budget check, so #t is also acceptable.
		{"budget zero may be unknown", `(sat-cnf-flat? #(1 2 0) 0)`, []string{"#t", "unknown"}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := evalString(t, engine, tc.code)
			if !slices.Contains(tc.want, got) {
				t.Errorf("%s: got %s, want one of %v", tc.code, got, tc.want)
			}
		})
	}
}

func TestPrimSatCNFFlatErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"first argument not a vector", `(sat-cnf-flat? '(1 2 0) #f)`},
		{"budget not an integer", `(sat-cnf-flat? #(1 0) "big")`},
		{"non-integer element in vector", `(sat-cnf-flat? #(1 "x" 0) #f)`},
		{"empty clause (consecutive 0s)", `(sat-cnf-flat? #(1 0 0 2 0) #f)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestPrimSatCNFFlatModel(t *testing.T) {
	tcs := []struct {
		name string
		// code runs to completion on a fresh engine; the value of its last
		// expression is what gets asserted. A fresh engine per case keeps the
		// stored model from leaking across cases.
		code string
		// want is the exact expected result string, unless wantVector is set.
		want string
		// wantVector asserts the result is a vector literal (a satisfying
		// model) rather than a fixed string. Used where the precise model is
		// solver-dependent and only its shape is contractual.
		wantVector bool
	}{
		{"no prior call yields #f", `(sat-cnf-flat-model)`, "#f", false},
		{
			"after UNSAT yields #f",
			`(sat-cnf-flat? #(1 0 -1 0) #f) (sat-cnf-flat-model)`,
			"#f", false,
		},
		{
			// (x1 ∨ x2) ∧ (¬x1 ∨ ¬x2): SAT with two satisfying assignments.
			"after SAT yields a model vector",
			`(sat-cnf-flat? #(1 2 0 -1 -2 0) #f) (sat-cnf-flat-model)`,
			"", true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine := newEngine(t)
			got := evalString(t, engine, tc.code)
			if tc.wantVector {
				// Model display form starts with "#(".
				if len(got) < 2 || got[0] != '#' || got[1] != '(' {
					t.Errorf("model: got %q, want a vector literal", got)
				}
				return
			}
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

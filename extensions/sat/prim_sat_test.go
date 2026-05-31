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

func TestPrimSatCNFFlat_SAT(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// Single clause (1 2): SAT — any assignment making x1 or x2 true works.
	c.Assert(evalString(t, engine, `(sat-cnf-flat? #(1 2 0) #f)`), qt.Equals, "#t")
}

func TestPrimSatCNFFlat_UNSAT(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// (x1) ∧ (¬x1): trivially UNSAT.
	c.Assert(evalString(t, engine, `(sat-cnf-flat? #(1 0 -1 0) #f)`), qt.Equals, "#f")
}

func TestPrimSatCNFFlat_EmptyVector(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// Zero clauses: trivially SAT.
	c.Assert(evalString(t, engine, `(sat-cnf-flat? #() #f)`), qt.Equals, "#t")
}

func TestPrimSatCNFFlat_Budget(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// Simple SAT instance with budget=1000: should still decide.
	c.Assert(evalString(t, engine, `(sat-cnf-flat? #(1 2 0) 1000)`), qt.Equals, "#t")
}

func TestPrimSatCNFFlat_BudgetZero(t *testing.T) {
	engine := newEngine(t)
	// Budget=0 forces immediate UNKNOWN on any non-trivial instance.
	// A single-literal clause is resolved by unit propagation at level 0
	// before the budget check, so it may still return SAT. Use a 2-literal
	// clause which requires a branch decision.
	got := evalString(t, engine, `(sat-cnf-flat? #(1 2 0) 0)`)
	// Acceptable results: #t (solved by UP before first conflict) or 'unknown.
	if got != "#t" && got != "unknown" {
		t.Errorf("budget=0: got %s, want #t or unknown", got)
	}
}

func TestPrimSatCNFFlat_UNSAT_ThreeClauses(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// (x1 ∨ x2) ∧ (¬x1) ∧ (¬x2): UNSAT.
	c.Assert(evalString(t, engine, `(sat-cnf-flat? #(1 2 0 -1 0 -2 0) #f)`), qt.Equals, "#f")
}

func TestPrimSatCNFFlat_Errors(t *testing.T) {
	engine := newEngine(t)
	// First argument not a vector.
	evalExpectError(t, engine, `(sat-cnf-flat? '(1 2 0) #f)`)
	// Budget not an integer.
	evalExpectError(t, engine, `(sat-cnf-flat? #(1 0) "big")`)
	// Non-integer element in vector.
	evalExpectError(t, engine, `(sat-cnf-flat? #(1 "x" 0) #f)`)
	// Empty clause (two consecutive 0s).
	evalExpectError(t, engine, `(sat-cnf-flat? #(1 0 0 2 0) #f)`)
}

func TestPrimSatCNFFlatModel_ReturnsFalseWhenNoModel(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// Before any sat-cnf-flat? call, model is #f.
	c.Assert(evalString(t, engine, `(sat-cnf-flat-model)`), qt.Equals, "#f")
}

func TestPrimSatCNFFlatModel_ReturnsFalseAfterUNSAT(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// After an UNSAT result, model is still #f.
	c.Assert(evalString(t, engine, `(sat-cnf-flat? #(1 0 -1 0) #f)`), qt.Equals, "#f")
	c.Assert(evalString(t, engine, `(sat-cnf-flat-model)`), qt.Equals, "#f")
}

func TestPrimSatCNFFlat_ModelRetrieval(t *testing.T) {
	engine := newEngine(t)
	// (x1 ∨ x2) ∧ (¬x1 ∨ ¬x2) — SAT with two satisfying assignments.
	result := evalString(t, engine, `(sat-cnf-flat? #(1 2 0 -1 -2 0) #f)`)
	if result != "#t" {
		t.Fatalf("expected SAT (#t), got %q", result)
	}
	got := evalString(t, engine, `(sat-cnf-flat-model)`)
	// Model must be a vector (starts with #( in display form), not #f.
	if got == "#f" {
		t.Errorf("model should not be #f after SAT result; got %q", got)
	}
	if len(got) < 2 || got[0] != '#' || got[1] != '(' {
		t.Errorf("model: got %q, want a vector literal", got)
	}
}

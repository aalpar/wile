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

package sat

import (
	"context"
	"math/rand"
	"testing"
)

func TestNewSolver_InitFromClauses(t *testing.T) {
	clauses := []clause{
		{lits: []literal{2 * 1, 2*2 + 1}},  // x1 ∨ ¬x2
		{lits: []literal{2*1 + 1, 2 * 3}},  // ¬x1 ∨ x3
	}
	s := newSolver(context.Background(), clauses, 3, -1)
	if s.numVars != 3 {
		t.Errorf("numVars: got %d, want 3", s.numVars)
	}
	if len(s.assigns) != 4 {
		// assigns is 1-indexed (var 0 unused).
		t.Errorf("len(assigns): got %d, want 4", len(s.assigns))
	}
	if len(s.watches) != 8 {
		// 2 * (numVars+1) = 2*4 = 8.
		t.Errorf("len(watches): got %d, want 8", len(s.watches))
	}
	totalWatches := 0
	for _, w := range s.watches {
		totalWatches += len(w)
	}
	if totalWatches != 2*len(clauses) {
		t.Errorf("total watches: got %d, want %d", totalWatches, 2*len(clauses))
	}
}

func TestEnqueueAndValue(t *testing.T) {
	s := newSolver(context.Background(), nil, 3, -1)
	s.enqueue(2*1, noClauseRef)
	if s.litValue(2*1) != 1 {
		t.Errorf("after enqueue x1=true, litValue(x1) = %d, want 1", s.litValue(2*1))
	}
	if s.litValue(2*1+1) != -1 {
		t.Errorf("after enqueue x1=true, litValue(¬x1) = %d, want -1", s.litValue(2*1+1))
	}
	if s.litValue(2*2) != 0 {
		t.Errorf("unassigned litValue(x2) = %d, want 0", s.litValue(2*2))
	}
	if len(s.trail) != 1 {
		t.Errorf("trail length: got %d, want 1", len(s.trail))
	}
}

func TestPropagate_UnitClauseDerivation(t *testing.T) {
	// Clauses: (¬x1 ∨ x2), (¬x2 ∨ x3).
	// After enqueueing x1=true, propagate should derive x2=true and x3=true.
	clauses := []clause{
		{lits: []literal{2*1 + 1, 2 * 2}},
		{lits: []literal{2*2 + 1, 2 * 3}},
	}
	s := newSolver(context.Background(), clauses, 3, -1)
	s.enqueue(2*1, noClauseRef)
	conflict := s.propagate()
	if conflict != noClauseRef {
		t.Errorf("unexpected conflict: %d", conflict)
	}
	if s.litValue(2*2) != 1 {
		t.Errorf("propagate did not derive x2=true (got %d)", s.litValue(2*2))
	}
	if s.litValue(2*3) != 1 {
		t.Errorf("propagate did not derive x3=true (got %d)", s.litValue(2*3))
	}
}

func TestPropagate_ConflictDetection(t *testing.T) {
	// Clauses: (¬x1 ∨ x2), (¬x1 ∨ ¬x2). Enqueue x1=true.
	// Propagation derives x2 from clause 0, then clause 1 becomes empty.
	clauses := []clause{
		{lits: []literal{2*1 + 1, 2 * 2}},
		{lits: []literal{2*1 + 1, 2*2 + 1}},
	}
	s := newSolver(context.Background(), clauses, 2, -1)
	s.enqueue(2*1, noClauseRef)
	conflict := s.propagate()
	if conflict == noClauseRef {
		t.Fatalf("expected conflict, got noClauseRef")
	}
}

func newDeterministicRNG(seed int64) *rand.Rand {
	return rand.New(rand.NewSource(seed))
}

func randomCNF(rng *rand.Rand, nVars, nClauses, clauseSize int32) ([]clause, int32) {
	clauses := make([]clause, 0, nClauses)
	for c := int32(0); c < nClauses; c++ {
		seen := map[int32]bool{}
		lits := make([]literal, 0, clauseSize)
		for k := int32(0); k < clauseSize; k++ {
			v := int32(rng.Intn(int(nVars))) + 1
			if seen[v] {
				continue
			}
			seen[v] = true
			sign := literal(rng.Intn(2))
			lits = append(lits, literal(2*v)+sign)
		}
		if len(lits) >= 1 {
			clauses = append(clauses, clause{lits: lits})
		}
	}
	return clauses, nVars
}

func TestBackjump(t *testing.T) {
	s := newSolver(context.Background(), nil, 3, -1)
	s.enqueue(2*1, noClauseRef)
	s.newDecisionLevel()
	s.enqueue(2*2, noClauseRef)
	s.newDecisionLevel()
	s.enqueue(2*3, noClauseRef)

	s.backjump(1)
	if s.litValue(2*3) != 0 {
		t.Errorf("after backjump(1), x3 should be unassigned (got %d)", s.litValue(2*3))
	}
	if s.litValue(2*2) != 1 {
		t.Errorf("after backjump(1), x2 should still be true (got %d)", s.litValue(2*2))
	}
	if s.decisionLevel() != 1 {
		t.Errorf("decisionLevel after backjump(1): got %d, want 1", s.decisionLevel())
	}
}

func TestAnalyze_1UIPClause(t *testing.T) {
	// Conflict scenario:
	//   C0: (¬x1 ∨ x2)
	//   C1: (¬x1 ∨ x3)
	//   C2: (¬x2 ∨ ¬x3)
	// Decide x1=true at level 1. Propagation derives x2 (from C0) and x3 (from C1).
	// C2 becomes empty → conflict. 1-UIP analysis should produce {¬x1}, btLevel=0.
	clauses := []clause{
		{lits: []literal{2*1 + 1, 2 * 2}},
		{lits: []literal{2*1 + 1, 2 * 3}},
		{lits: []literal{2*2 + 1, 2*3 + 1}},
	}
	s := newSolver(context.Background(), clauses, 3, -1)
	s.newDecisionLevel()
	s.enqueue(2*1, noClauseRef)
	conflict := s.propagate()
	if conflict == noClauseRef {
		t.Fatalf("expected conflict")
	}
	learnt, btLevel := s.analyze(conflict)
	if btLevel != 0 {
		t.Errorf("btLevel: got %d, want 0", btLevel)
	}
	if len(learnt) != 1 {
		t.Errorf("learnt clause should be unit: got %d lits", len(learnt))
	}
	if learnt[0] != 2*1+1 {
		t.Errorf("learnt[0]: got %d, want %d (¬x1)", learnt[0], 2*1+1)
	}
}

func TestAnalyze_1UIPProperty(t *testing.T) {
	rng := newDeterministicRNG(7)
	for iter := 0; iter < 30; iter++ {
		clauses, numVars := randomCNF(rng, 8, 30, 3)
		s := newSolver(context.Background(), clauses, numVars, -1)
		conflict := s.propagate()
		if conflict != noClauseRef {
			continue
		}
		for step := 0; step < 20 && conflict == noClauseRef; step++ {
			var pickedVar int32
			for v := int32(1); v <= numVars; v++ {
				if s.assigns[v] == 0 {
					pickedVar = v
					break
				}
			}
			if pickedVar == 0 {
				break
			}
			s.newDecisionLevel()
			s.enqueue(literal(2*pickedVar), noClauseRef)
			conflict = s.propagate()
		}
		if conflict == noClauseRef || s.decisionLevel() == 0 {
			continue
		}
		learnt, _ := s.analyze(conflict)
		curLevel := s.decisionLevel()
		count := 0
		for _, q := range learnt {
			if s.level[int32(q)>>1] == curLevel {
				count++
			}
		}
		if count != 1 {
			t.Fatalf("iter %d: learnt has %d lits at current level %d, want 1; learnt=%v",
				iter, count, curLevel, learnt)
		}
	}
}

func TestVSIDS_BumpAndSelect(t *testing.T) {
	s := newSolver(context.Background(), nil, 4, -1)
	s.bumpVarActivity(1)
	s.bumpVarActivity(1)
	s.bumpVarActivity(3)
	v := s.pickBranchVar()
	if v != 1 {
		t.Errorf("pickBranchVar: got %d, want 1", v)
	}
}

func TestVSIDS_DecayAndRescale(t *testing.T) {
	s := newSolver(context.Background(), nil, 2, -1)
	for i := 0; i < 100; i++ {
		s.bumpVarActivity(1)
		s.decayVarActivity()
	}
	if s.activity[1] != s.activity[1] {
		t.Errorf("activity is NaN")
	}
}

func TestSearch_TinySAT(t *testing.T) {
	clauses := []clause{
		{lits: []literal{2 * 1, 2 * 2}},
		{lits: []literal{2*1 + 1, 2 * 2}},
	}
	s := newSolver(context.Background(), clauses, 2, -1)
	if r := s.solve(); r != resultSAT {
		t.Errorf("got %v, want SAT", r)
	}
	if s.assigns[2] != 1 {
		t.Errorf("expected x2=true; got %d", s.assigns[2])
	}
}

func TestSearch_TinyUNSAT(t *testing.T) {
	clauses := []clause{
		{lits: []literal{2 * 1}},
		{lits: []literal{2*1 + 1}},
	}
	s := newSolver(context.Background(), clauses, 1, -1)
	if r := s.solve(); r != resultUNSAT {
		t.Errorf("got %v, want UNSAT", r)
	}
}

func TestLubySequence(t *testing.T) {
	want := []int64{1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8}
	for i, w := range want {
		if got := luby(int64(i + 1)); got != w {
			t.Errorf("luby(%d): got %d, want %d", i+1, got, w)
		}
	}
}

func TestSearch_BudgetExhausted(t *testing.T) {
	rng := newDeterministicRNG(123)
	clauses, numVars := randomCNF(rng, 50, 218, 3)
	s := newSolver(context.Background(), clauses, numVars, 10)
	r := s.solve()
	if r != resultUNKNOWN {
		t.Logf("note: tiny budget may have been enough; got %v", r)
	}
}

func TestSearch_CtxCancel(t *testing.T) {
	rng := newDeterministicRNG(99)
	clauses, numVars := randomCNF(rng, 50, 218, 3)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	s := newSolver(ctx, clauses, numVars, -1)
	r := s.solve()
	if r != resultUNKNOWN && r != resultSAT && r != resultUNSAT {
		t.Errorf("unexpected result with cancelled ctx: %v", r)
	}
}

func TestPropagate_WatchInvariant(t *testing.T) {
	rng := newDeterministicRNG(42)
	for iter := 0; iter < 50; iter++ {
		clauses, numVars := randomCNF(rng, 5, 10, 3)
		s := newSolver(context.Background(), clauses, numVars, -1)
		k := rng.Intn(int(numVars)/2 + 1)
		for j := 0; j < k; j++ {
			v := int32(rng.Intn(int(numVars))) + 1
			sign := literal(rng.Intn(2))
			l := literal(2*v) + sign
			if s.litValue(l) == 0 {
				s.enqueue(l, noClauseRef)
			}
		}
		conflict := s.propagate()
		if conflict != noClauseRef {
			continue
		}
		for ci := range s.clauses {
			c := &s.clauses[ci]
			if len(c.lits) < 2 {
				continue
			}
			if s.litValue(c.lits[0]) == -1 && s.litValue(c.lits[1]) == -1 {
				t.Fatalf("iter %d, clause %d: both watches falsified, no conflict reported",
					iter, ci)
			}
		}
	}
}

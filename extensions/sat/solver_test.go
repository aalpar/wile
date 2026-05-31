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

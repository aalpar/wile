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

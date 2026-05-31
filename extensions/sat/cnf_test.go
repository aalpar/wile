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
	"testing"

	"github.com/aalpar/wile/values"
)

func TestParseCNF_HappyPath(t *testing.T) {
	// CNF: (x1 ∨ ¬x2 ∨ x3) ∧ (¬x1 ∨ x4) ∧ (x2 ∨ ¬x3 ∨ ¬x4)
	// Flat:  1 -2 3 0 -1 4 0 2 -3 -4 0
	input := values.NewVector(
		values.NewInteger(1), values.NewInteger(-2), values.NewInteger(3), values.NewInteger(0),
		values.NewInteger(-1), values.NewInteger(4), values.NewInteger(0),
		values.NewInteger(2), values.NewInteger(-3), values.NewInteger(-4), values.NewInteger(0),
	)

	clauses, numVars, err := parseCNF(input)
	if err != nil {
		t.Fatalf("parseCNF: unexpected error: %v", err)
	}
	if numVars != 4 {
		t.Errorf("numVars: got %d, want 4", numVars)
	}
	if len(clauses) != 3 {
		t.Errorf("len(clauses): got %d, want 3", len(clauses))
	}
	want0 := []literal{2*1 + 0, 2*2 + 1, 2*3 + 0}
	if !equalLits(clauses[0].lits, want0) {
		t.Errorf("clauses[0].lits: got %v, want %v", clauses[0].lits, want0)
	}
}

func equalLits(a, b []literal) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

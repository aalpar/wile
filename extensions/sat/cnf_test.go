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
	"slices"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/values"
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
	if !slices.Equal(clauses[0].lits, want0) {
		t.Errorf("clauses[0].lits: got %v, want %v", clauses[0].lits, want0)
	}
	want1 := []literal{2*1 + 1, 2 * 4}
	if !slices.Equal(clauses[1].lits, want1) {
		t.Errorf("clauses[1].lits: got %v, want %v", clauses[1].lits, want1)
	}
	want2 := []literal{2 * 2, 2*3 + 1, 2*4 + 1}
	if !slices.Equal(clauses[2].lits, want2) {
		t.Errorf("clauses[2].lits: got %v, want %v", clauses[2].lits, want2)
	}
}

func TestParseCNF_Int32OverflowBoundary(t *testing.T) {
	// var == 1<<30 must be rejected: 2*(1<<30) overflows int32.
	input := values.NewVector(values.NewInteger(1 << 30))
	_, _, err := parseCNF(input)
	if err == nil {
		t.Fatal("parseCNF: expected error for var 1<<30, got nil")
	}
	const want = "overflows int32"
	if !strings.Contains(err.Error(), want) {
		t.Errorf("error %q does not contain %q", err.Error(), want)
	}
}

func TestParseCNF_MaxVarsBoundary(t *testing.T) {
	// A variable index above maxVars must be rejected before newSolver
	// allocates O(numVars) arrays. maxVars+1 is below the int32-overflow
	// guard (1<<30), so this exercises the allocation bound specifically.
	input := values.NewVector(values.NewInteger(int64(maxVars) + 1))
	_, _, err := parseCNF(input)
	if err == nil {
		t.Fatalf("parseCNF: expected error for var maxVars+1, got nil")
	}
	const want = "too many variables"
	if !strings.Contains(err.Error(), want) {
		t.Errorf("error %q does not contain %q", err.Error(), want)
	}
	// maxVars itself must still be accepted.
	if _, n, err := parseCNF(values.NewVector(values.NewInteger(int64(maxVars)))); err != nil {
		t.Errorf("parseCNF: maxVars must be accepted, got error: %v", err)
	} else if n != maxVars {
		t.Errorf("parseCNF: got numVars %d, want %d", n, maxVars)
	}
}

func mkVec(xs ...int64) *values.Vector {
	vs := make([]values.Value, len(xs))
	for i, x := range xs {
		vs[i] = values.NewInteger(x)
	}
	return values.NewVector(vs...)
}

func TestParseCNF_Errors(t *testing.T) {
	cases := []struct {
		name    string
		input   *values.Vector
		wantSub string
	}{
		{
			name:    "empty clause via leading zero",
			input:   mkVec(0),
			wantSub: "empty clause",
		},
		{
			name:    "empty clause between clauses",
			input:   mkVec(1, 2, 0, 0, 3, 0),
			wantSub: "empty clause",
		},
		{
			name:    "variable index overflows int32",
			input:   mkVec(1 << 30),
			wantSub: "overflows int32",
		},
		{
			// values.NewString produces a *values.String, which ExactInteger rejects.
			name:    "non-integer element",
			input:   values.NewVector(values.NewString("bad")),
			wantSub: "not an exact integer",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, _, err := parseCNF(tc.input)
			if err == nil {
				t.Fatalf("parseCNF: expected error, got nil")
			}
			if !strings.Contains(err.Error(), tc.wantSub) {
				t.Errorf("error %q does not contain %q", err.Error(), tc.wantSub)
			}
		})
	}
}

func TestParseCNF_TrivialTrue(t *testing.T) {
	in := values.NewVector()
	clauses, n, err := parseCNF(in)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(clauses) != 0 || n != 0 {
		t.Errorf("got %d clauses, n=%d; want 0, 0", len(clauses), n)
	}
}

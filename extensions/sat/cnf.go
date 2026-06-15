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
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// literal is a packed boolean literal: 2*var + sign, where sign=0 means
// positive and sign=1 means negative. Variable indices start at 1 (DIMACS
// convention).
type literal int32

// clauseRef is an index into a clause table.
type clauseRef int32

// noClauseRef is the sentinel for "no clause".
const noClauseRef clauseRef = -1

// maxVars bounds the inferred variable count so a public caller cannot drive
// newSolver into an oversized allocation. The dominant per-variable cost is the
// watches table — make([][]clauseRef, 2*(numVars+1)), ~48 bytes/var of slice
// headers alone — so the int32-overflow guard (1<<30) still permits a ~48GB
// allocation from a two-element vector like #(1073741823 0). At 1<<22 variables
// the worst-case allocation is a few hundred MB, well beyond any realistic
// embedded SAT instance yet no longer a host-OOM DoS.
const maxVars int32 = 1 << 22

// clause is a single CNF clause.
type clause struct {
	learnt   bool
	activity float32
	// The first two literals (lits[0] and lits[1]) are the watched pair; solver.go relies on this invariant.
	lits []literal
}

// litFromInt packs a DIMACS literal (nonzero signed int) into the internal
// encoding. Positive DIMACS literal v maps to 2*v; negative DIMACS literal -v
// maps to 2*v+1.
func litFromInt(v int64) literal {
	if v > 0 {
		return literal(2 * v)
	}
	return literal(2*(-v) + 1)
}

// parseCNF walks a flat vector of exact integers terminated by 0-sentinels and
// returns the parsed clauses, the inferred variable count (max |literal|), and
// any error. A missing trailing 0 is accepted; trailing literals without a 0
// are collected into a final clause.
func parseCNF(v *values.Vector) ([]clause, int32, error) {
	if v == nil {
		return nil, 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "parseCNF: nil vector")
	}

	var clauses []clause
	var pending []literal
	var maxVar int32

	n := v.Length()
	for i := range n {
		elem := v.Get(i)
		val, ok := values.ExactInteger(elem)
		if !ok {
			return nil, 0, werr.WrapForeignErrorf(werr.ErrNotAnInteger,
				"parseCNF: element at index %d is not an exact integer, got %T", i, elem)
		}

		if val == 0 {
			if len(pending) == 0 {
				return nil, 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
					"parseCNF: empty clause at index %d", i)
			}
			clauses = append(clauses, clause{lits: pending})
			pending = nil
			continue
		}

		abs := val
		if abs < 0 {
			abs = -abs
		}
		if abs >= (1 << 30) {
			return nil, 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"parseCNF: variable index %d overflows int32", abs)
		}
		if abs > int64(maxVars) {
			return nil, 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"parseCNF: variable index %d exceeds maximum (too many variables; limit %d)", abs, maxVars)
		}
		if abs > int64(maxVar) {
			maxVar = int32(abs)
		}
		pending = append(pending, litFromInt(val))
	}

	// Accept trailing literals without a terminating 0.
	if len(pending) > 0 {
		clauses = append(clauses, clause{lits: pending})
	}

	return clauses, maxVar, nil
}

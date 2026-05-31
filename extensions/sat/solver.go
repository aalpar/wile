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
)

// solver holds the state of one CDCL search.
type solver struct {
	numVars int32

	// Assignment + trail.
	// assigns is 1-indexed (index 0 unused): 0=unassigned, 1=true, -1=false.
	assigns  []int8
	level    []int32     // decision level at which a var was set
	reason   []clauseRef // antecedent clause; noClauseRef for decisions
	trail    []literal
	trailLim []int32

	// Clause database. Indices into this slice are clauseRef values.
	clauses []clause

	// watches[lit] = clauses where lit is one of the first two literals.
	// Length 2*(numVars+1).
	watches [][]clauseRef

	// VSIDS-related fields filled in later tasks.
	activity      []float32
	activityInc   float32
	activityDecay float32

	// Restart + clause-DB policy filled in later tasks.
	conflicts      int64
	conflictBudget int64 // -1 = unlimited
	nextRestart    int64
	learntLimit    int

	ctx context.Context
}

// newSolver builds a solver from parsed clauses. numVars is the
// inferred variable count from parseCNF. conflictBudget = -1 means
// unlimited.
func newSolver(ctx context.Context, clauses []clause, numVars int32, conflictBudget int64) *solver {
	s := &solver{
		numVars:        numVars,
		assigns:        make([]int8, numVars+1),
		level:          make([]int32, numVars+1),
		reason:         make([]clauseRef, numVars+1),
		trail:          make([]literal, 0, numVars),
		clauses:        make([]clause, 0, len(clauses)),
		watches:        make([][]clauseRef, 2*(numVars+1)),
		activity:       make([]float32, numVars+1),
		activityInc:    1.0,
		activityDecay:  0.95,
		conflictBudget: conflictBudget,
		ctx:            ctx,
	}
	for v := range s.reason {
		s.reason[v] = noClauseRef
	}
	for _, c := range clauses {
		s.addClause(c)
	}
	return s
}

// litValue returns 1 if lit is currently true under the assignment,
// -1 if false, 0 if its variable is unassigned.
func (s *solver) litValue(l literal) int8 {
	v := int32(l) >> 1
	sign := int8(l & 1)
	a := s.assigns[v]
	if a == 0 {
		return 0
	}
	if sign == 0 {
		return a
	}
	return -a
}

// decisionLevel returns the current decision level.
func (s *solver) decisionLevel() int32 {
	return int32(len(s.trailLim))
}

// enqueue commits lit as true at the current decision level. reason is
// the antecedent clause (or noClauseRef for decisions). Caller must
// ensure litValue(lit) was 0 (unassigned) before calling.
func (s *solver) enqueue(l literal, reason clauseRef) {
	v := int32(l) >> 1
	sign := int8(l & 1)
	if sign == 0 {
		s.assigns[v] = 1
	} else {
		s.assigns[v] = -1
	}
	s.level[v] = s.decisionLevel()
	s.reason[v] = reason
	s.trail = append(s.trail, l)
}

// addClause appends a clause to the database and registers watches on
// its first two literals. The caller guarantees the clause has at least
// one literal (the empty-clause case is rejected by parseCNF).
func (s *solver) addClause(c clause) clauseRef {
	ref := clauseRef(len(s.clauses))
	s.clauses = append(s.clauses, c)
	if len(c.lits) == 1 {
		// Unit clause: watch its only literal once.
		s.watches[c.lits[0]] = append(s.watches[c.lits[0]], ref)
		return ref
	}
	s.watches[c.lits[0]] = append(s.watches[c.lits[0]], ref)
	s.watches[c.lits[1]] = append(s.watches[c.lits[1]], ref)
	return ref
}

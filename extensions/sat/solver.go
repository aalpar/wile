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

// newDecisionLevel pushes a new decision level by recording the current
// trail length.
func (s *solver) newDecisionLevel() {
	s.trailLim = append(s.trailLim, int32(len(s.trail)))
}

// backjump undoes all assignments above the given decision level.
func (s *solver) backjump(target int32) {
	if s.decisionLevel() <= target {
		return
	}
	cutoff := s.trailLim[target]
	for i := int32(len(s.trail)) - 1; i >= cutoff; i-- {
		v := int32(s.trail[i]) >> 1
		s.assigns[v] = 0
		s.level[v] = 0
		s.reason[v] = noClauseRef
	}
	s.trail = s.trail[:cutoff]
	s.trailLim = s.trailLim[:target]
}

// propagate runs watched-literal unit propagation from the current trail
// head. Returns noClauseRef on success (no conflict, all units enqueued)
// or the clauseRef of a falsified clause on conflict.
//
// Algorithm: for each literal l on the trail not yet processed, walk
// watches[¬l]. For each watching clause:
//   - If the other watched lit is already true, the clause is satisfied;
//     leave the watch in place.
//   - Otherwise, scan lits[2:] for a non-false literal to swap into the
//     watch slot. If found, move the watch and continue.
//   - If no replacement and the other watched lit is unassigned, the
//     clause is unit: enqueue the other lit with this clause as reason.
//   - If no replacement and the other watched lit is false, the clause
//     is empty under the assignment: conflict.
//
// qhead tracks how far down the trail we've processed.
func (s *solver) propagate() clauseRef {
	qhead := 0
	for qhead < len(s.trail) {
		p := s.trail[qhead]
		qhead++
		notP := p ^ 1
		ws := s.watches[notP]
		newWatches := ws[:0]
		i := 0
		for i < len(ws) {
			cr := ws[i]
			c := &s.clauses[cr]
			if len(c.lits) == 0 {
				// Tombstoned by reduceClauseDB (added in Task 13); skip without re-watching.
				i++
				continue
			}
			if len(c.lits) == 1 {
				// Unit clause: its only literal is being falsified — conflict.
				newWatches = append(newWatches, ws[i:]...)
				s.watches[notP] = newWatches
				return cr
			}
			// Ensure lits[1] is the false watched lit (the one we're processing).
			if c.lits[0] == notP {
				c.lits[0], c.lits[1] = c.lits[1], c.lits[0]
			}
			other := c.lits[0]
			if s.litValue(other) == 1 {
				// Already satisfied; keep current watches.
				newWatches = append(newWatches, cr)
				i++
				continue
			}
			// Find a non-false replacement in lits[2:].
			found := false
			for k := 2; k < len(c.lits); k++ {
				if s.litValue(c.lits[k]) != -1 {
					c.lits[1], c.lits[k] = c.lits[k], c.lits[1]
					s.watches[c.lits[1]] = append(s.watches[c.lits[1]], cr)
					found = true
					break
				}
			}
			if found {
				i++
				continue
			}
			// No replacement. Unit or conflict.
			if s.litValue(other) == -1 {
				// Conflict: restore remaining watches and return.
				newWatches = append(newWatches, ws[i:]...)
				s.watches[notP] = newWatches
				return cr
			}
			s.enqueue(other, cr)
			newWatches = append(newWatches, cr)
			i++
		}
		s.watches[notP] = newWatches
	}
	return noClauseRef
}

// analyze derives a 1-UIP learnt clause from the given conflict.
// Returns the literal slice of the learnt clause and the backtrack
// decision level (highest level among non-asserting literals; 0 if unit).
//
// Walks the implication graph backwards via the trail, marking variables
// seen at the current decision level. When exactly one current-level
// variable remains in the front, it is the 1-UIP; the remaining lits
// (lower levels) form the learnt clause.
func (s *solver) analyze(conflict clauseRef) ([]literal, int32) {
	seen := make([]bool, s.numVars+1)
	learnt := []literal{0} // placeholder for asserting literal
	pathCount := 0
	curLevel := s.decisionLevel()
	p := literal(-1) // sentinel
	idx := int32(len(s.trail)) - 1
	cr := conflict

	for {
		c := &s.clauses[cr]
		if c.learnt {
			s.bumpClauseActivity(cr)
		}
		startK := 0
		if p != literal(-1) {
			startK = 1
		}
		for k := startK; k < len(c.lits); k++ {
			q := c.lits[k]
			v := int32(q) >> 1
			if !seen[v] && s.level[v] > 0 {
				seen[v] = true
				s.bumpVarActivity(v)
				if s.level[v] >= curLevel {
					pathCount++
				} else {
					learnt = append(learnt, q)
				}
			}
		}
		for !seen[int32(s.trail[idx])>>1] {
			idx--
		}
		p = s.trail[idx]
		seen[int32(p)>>1] = false
		idx--
		pathCount--
		if pathCount == 0 {
			break
		}
		cr = s.reason[int32(p)>>1]
	}
	learnt[0] = p ^ 1

	var btLevel int32
	for k := 1; k < len(learnt); k++ {
		lv := s.level[int32(learnt[k])>>1]
		if lv > btLevel {
			btLevel = lv
		}
	}
	return learnt, btLevel
}

// bumpClauseActivity / bumpVarActivity: VSIDS bookkeeping. Implemented
// in Task 11. Stubs so analyze compiles.
func (s *solver) bumpClauseActivity(cr clauseRef) {
	_ = cr
}
func (s *solver) bumpVarActivity(v int32) {
	_ = v
}

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

	"github.com/aalpar/wile/pkg/values"
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

	// VSIDS activity scores and the decay/rescale increment.
	activity      []float32
	activityInc   float32
	activityDecay float32

	// Activity-ordered max-heap over variables, so a decision costs
	// O(log n) instead of a scan of every variable. order holds variable
	// ids; orderPos[v] is v's index in order, or -1 when v is absent.
	// Assigned variables are left in place and skipped on pop, MiniSat's
	// arrangement; backjump re-inserts what it unassigns.
	order    []int32
	orderPos []int32

	// Propagation cursor into trail. Persistent across propagate() calls:
	// see propagate's comment for why re-scanning from the base was safe
	// but quadratic.
	qhead int

	// Luby restart schedule and learnt-clause-DB budget.
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
	// Seed the decision heap with every variable, in index order. All
	// activities are zero here, and orderBefore breaks that tie by index, so
	// inserting 1..numVars in increasing order performs no sift at all: the
	// build is O(n), not O(n log n).
	s.order = make([]int32, 0, numVars)
	s.orderPos = make([]int32, numVars+1)
	s.orderPos[0] = -1
	for v := int32(1); v <= numVars; v++ {
		s.orderPos[v] = -1
		s.orderInsert(v)
	}
	for _, c := range clauses {
		s.addClause(c)
	}
	if s.learntLimit == 0 {
		s.learntLimit = max(len(clauses)/3, 100)
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
		s.orderInsert(v)
	}
	s.trail = s.trail[:cutoff]
	s.trailLim = s.trailLim[:target]
	// The propagation cursor follows the trail down; anything above cutoff
	// was never processed, or was processed under an assignment that no
	// longer holds.
	s.qhead = int(cutoff)
}

// propagate runs watched-literal unit propagation over the whole current
// trail. Returns noClauseRef on success (no conflict, all units enqueued)
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
// qhead is a PERSISTENT watermark, not a per-call cursor. It used to be
// per-call, and that was correct but quadratic: re-walking the whole trail on
// every call made a solve with n decisions cost O(n²) propagation, which a
// Go CPU profile put at ~41% of a 200,000-variable run.
//
// Why persistence is sound: a literal already processed has had every clause
// watching its negation examined. A clause that becomes unit LATER does so
// because one of its two watched literals is falsified later, and that
// falsification is itself a trail entry above the watermark, so the clause is
// re-examined then. A learnt clause added between calls needs no event at all
// — addLearntClause puts the asserting literal at position 0 and solve
// enqueues it directly with the clause as its reason.
//
// The watermark moves backwards in exactly two places, and both are here:
// a conflict abandons the rest of the trail (so the cursor jumps to the end,
// and backjump then rewinds it), and backjump truncates the trail (so the
// cursor follows it down).
func (s *solver) propagate() clauseRef {
	for s.qhead < len(s.trail) {
		p := s.trail[s.qhead]
		s.qhead++
		notP := p ^ 1
		ws := s.watches[notP]
		newWatches := ws[:0]
		i := 0
		for i < len(ws) {
			cr := ws[i]
			c := &s.clauses[cr]
			if len(c.lits) == 0 {
				// Tombstoned by reduceClauseDB; skip without re-watching.
				i++
				continue
			}
			if len(c.lits) == 1 {
				// Unit clause: its only literal is being falsified — conflict.
				newWatches = append(newWatches, ws[i:]...)
				s.watches[notP] = newWatches
				s.qhead = len(s.trail)
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
				// Conflict: restore remaining watches and return. The cursor
				// jumps to the end because the rest of the trail is about to
				// be unwound by backjump, which rewinds the cursor with it.
				newWatches = append(newWatches, ws[i:]...)
				s.watches[notP] = newWatches
				s.qhead = len(s.trail)
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
		// Skip lits[0] on subsequent iterations: when this loop body runs after
		// the first iteration, the clause is a reason clause for the literal p
		// just resolved against. By MiniSat convention, propagate places the
		// implied literal (the pivot) at lits[0], so lits[0] is already
		// accounted for via the trail walk and must not be re-counted here.
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

// bumpVarActivity increments variable v's VSIDS activity score by the
// current increment, then rescales all scores if any would exceed 1e20.
func (s *solver) bumpVarActivity(v int32) {
	s.activity[v] += s.activityInc
	if s.orderPos[v] >= 0 {
		// Activity only ever rises here, so the variable can only move
		// toward the root.
		s.orderUp(int(s.orderPos[v]))
	}
	if s.activity[v] > 1e20 {
		for i := range s.activity {
			s.activity[i] *= 1e-20
		}
		s.activityInc *= 1e-20
		// Rebuild, because *1e-20 on float32 is NOT order-preserving. It is
		// not injective in two separate ways: values below the normal range
		// flush to a subnormal or to zero, and even inside the normal range
		// two distinct float32 can round to one (1.5000003e-18 and
		// 1.5000004e-18 both land on 1.5000003e-38). Every collapse is a new
		// tie, and orderBefore breaks ties by INDEX, so a pre-rescale heap
		// can be a post-rescale violation — after which pickBranchVar returns
		// a different variable from the linear scan it replaced, which is the
		// one property TestPickBranchVarMatchesLinearScan exists to pin.
		//
		// Instrumented over 643k conflicts on 8 random 3-SAT instances: 712
		// rescales, 1404 distinct-to-equal collapses, and ZERO violations —
		// so this is unreached at realistic scales. It is here because the
		// invariant is pinned, not because the search was observed to break
		// it. Floyd's build is O(n) and runs about once per 900 conflicts.
		s.rebuildOrder()
	}
}

// rebuildOrder restores the heap property over the whole array in O(n),
// bottom-up. Used after an activity rescale, which can reorder ties.
func (s *solver) rebuildOrder() {
	for i := len(s.order)/2 - 1; i >= 0; i-- {
		s.orderDown(i)
	}
}

// decayVarActivity ages all variable scores by growing the increment;
// equivalent to multiplying all existing scores by activityDecay (0.95).
func (s *solver) decayVarActivity() {
	s.activityInc /= s.activityDecay
}

// bumpClauseActivity increments a learnt clause's activity score,
// rescaling the entire clause database if any score exceeds 1e20.
func (s *solver) bumpClauseActivity(cr clauseRef) {
	c := &s.clauses[cr]
	c.activity += 1.0
	if c.activity > 1e20 {
		for i := range s.clauses {
			s.clauses[i].activity *= 1e-20
		}
	}
}

// orderBefore is the heap's ordering: higher activity first, and among
// equal activities the LOWER variable index first.
//
// The index tie-break is not decoration. It makes the heap select exactly
// what the linear scan it replaced selected — the scan compared with a strict
// `>`, so the first variable at the maximum won — and a solver that picks
// differently returns a different satisfying assignment for any instance with
// more than one model. Without this the change would be observable at the
// Scheme surface for every instance whose variables start at equal activity,
// which is every instance before the first conflict.
func (s *solver) orderBefore(a, b int32) bool {
	if s.activity[a] != s.activity[b] {
		return s.activity[a] > s.activity[b]
	}
	return a < b
}

// orderUp restores the heap property by moving position i toward the root.
func (s *solver) orderUp(i int) {
	v := s.order[i]
	for i > 0 {
		parent := (i - 1) / 2
		if !s.orderBefore(v, s.order[parent]) {
			break
		}
		s.order[i] = s.order[parent]
		s.orderPos[s.order[i]] = int32(i)
		i = parent
	}
	s.order[i] = v
	s.orderPos[v] = int32(i)
}

// orderDown restores the heap property by moving position i toward the leaves.
func (s *solver) orderDown(i int) {
	v := s.order[i]
	n := len(s.order)
	for {
		child := 2*i + 1
		if child >= n {
			break
		}
		if child+1 < n && s.orderBefore(s.order[child+1], s.order[child]) {
			child++
		}
		if !s.orderBefore(s.order[child], v) {
			break
		}
		s.order[i] = s.order[child]
		s.orderPos[s.order[i]] = int32(i)
		i = child
	}
	s.order[i] = v
	s.orderPos[v] = int32(i)
}

// orderInsert puts v back in the heap. Idempotent: a variable already
// present is left where it is.
func (s *solver) orderInsert(v int32) {
	if s.orderPos[v] >= 0 {
		return
	}
	s.order = append(s.order, v)
	s.orderPos[v] = int32(len(s.order) - 1)
	s.orderUp(len(s.order) - 1)
}

// orderPop removes and returns the heap's front variable, or 0 when empty.
func (s *solver) orderPop() int32 {
	if len(s.order) == 0 {
		return 0
	}
	v := s.order[0]
	s.orderPos[v] = -1
	last := len(s.order) - 1
	if last > 0 {
		s.order[0] = s.order[last]
		s.orderPos[s.order[0]] = 0
	}
	s.order = s.order[:last]
	if last > 0 {
		s.orderDown(0)
	}
	return v
}

// pickBranchVar returns the unassigned variable with the highest VSIDS
// activity, or 0 if all variables are assigned.
//
// This was a scan of every variable, with a comment inviting a heap "only if
// benchmarks show this is hot". They do: a Go CPU profile puts it at ~59% of
// a 200,000-variable run, because a solve with n decisions performs n scans of
// n variables. The cost is not theoretical — (sat-cnf-flat? (vector 4194304 0) 1)
// is a legal two-element input, inside the documented maxVars bound, and it
// extrapolated to about eight and a half hours of CPU. It is now O(n log n).
//
// Assigned variables are left in the heap and skipped here rather than being
// removed on assignment; that is MiniSat's arrangement, and backjump
// re-inserts what it unassigns.
func (s *solver) pickBranchVar() int32 {
	for {
		v := s.orderPop()
		if v == 0 {
			return 0
		}
		if s.assigns[v] == 0 {
			return v
		}
	}
}

// luby returns the i-th element of the Luby restart sequence (1-indexed).
// The sequence is: 1,1,2,1,1,2,4,1,1,2,1,1,2,4,8,...
// Each "super-sequence" of length 2^k-1 ends with 2^(k-1), preceded by
// two copies of the super-sequence of length 2^(k-1)-1.
func luby(i int64) int64 {
	k := int64(1)
	for {
		size := (int64(1) << uint(k)) - 1
		if i == size {
			return int64(1) << uint(k-1)
		}
		if i < size {
			i = i - (int64(1) << uint(k-1)) + 1
			k = 1
			continue
		}
		k++
	}
}

// reduceClauseDB halves the learnt-clause set by activity, keeping any
// clause currently used as a reason on the trail. Tombstoned clauses are
// skipped in propagate via the len(c.lits)==0 check.
func (s *solver) reduceClauseDB() {
	locked := values.NewMapSet[clauseRef](0)
	for _, l := range s.trail {
		r := s.reason[int32(l)>>1]
		if r != noClauseRef {
			locked.Set(r)
		}
	}
	type idxAct struct {
		i   int
		act float32
	}
	sortable := make([]idxAct, 0)
	for i := range s.clauses {
		if s.clauses[i].learnt && len(s.clauses[i].lits) > 0 {
			sortable = append(sortable, idxAct{i, s.clauses[i].activity})
		}
	}
	// Insertion sort descending by activity (small N, no allocation).
	for i := 1; i < len(sortable); i++ {
		for j := i; j > 0 && sortable[j-1].act < sortable[j].act; j-- {
			sortable[j-1], sortable[j] = sortable[j], sortable[j-1]
		}
	}
	keep := len(sortable) / 2
	for k := keep; k < len(sortable); k++ {
		inUse := locked.Get(clauseRef(sortable[k].i))
		if !inUse {
			s.clauses[sortable[k].i].lits = nil // tombstone
		}
	}
	s.learntLimit *= 2
}

// learntCount returns the count of non-tombstone learnt clauses.
func (s *solver) learntCount() int {
	n := 0
	for _, c := range s.clauses {
		if c.learnt && len(c.lits) > 0 {
			n++
		}
	}
	return n
}

// SolverResult is the outcome of one solve() call.
type SolverResult int8

const (
	resultUnsat   SolverResult = -1
	resultUnknown SolverResult = 0
	resultSat     SolverResult = 1
)

// solve runs the main CDCL search loop until SAT, UNSAT, or budget/ctx
// exhaustion. On SAT, the satisfying assignment is in s.assigns.
func (s *solver) solve() SolverResult {
	// Level-0: enqueue input units (single-literal clauses).
	for ci := range s.clauses {
		c := &s.clauses[ci]
		if len(c.lits) == 1 && s.litValue(c.lits[0]) == 0 {
			s.enqueue(c.lits[0], clauseRef(ci))
		}
	}
	if s.propagate() != noClauseRef {
		return resultUnsat
	}
	for {
		conflict := s.propagate()
		if conflict != noClauseRef {
			// Conflict: learn a clause, backjump, and re-propagate.
			if s.decisionLevel() == 0 {
				return resultUnsat
			}
			s.conflicts++
			if s.conflictBudget >= 0 && s.conflicts >= s.conflictBudget {
				return resultUnknown
			}
			learnt, btLevel := s.analyze(conflict)
			s.backjump(btLevel)
			if len(learnt) == 1 {
				s.enqueue(learnt[0], noClauseRef)
			} else {
				cr := s.addLearntClause(learnt)
				s.enqueue(learnt[0], cr)
			}
			s.decayVarActivity()
			continue
		}
		// No conflict. Restart on Luby schedule and reduce clause DB if needed.
		const lubyUnit = 100
		if s.conflicts >= s.nextRestart {
			s.backjump(0)
			s.nextRestart = s.conflicts + lubyUnit*luby(s.conflicts/lubyUnit+1)
			if s.learntCount() > s.learntLimit {
				s.reduceClauseDB()
			}
		}
		if s.ctx != nil {
			select {
			case <-s.ctx.Done():
				return resultUnknown
			default:
			}
		}
		v := s.pickBranchVar()
		if v == 0 {
			return resultSat
		}
		s.newDecisionLevel()
		s.enqueue(literal(2*v), noClauseRef)
	}
}

// addLearntClause adds a clause produced by analyze() to the database.
// The asserting lit (learnt[0]) is watched at position 0; position 1
// must hold the literal with the highest decision level among learnt[1:].
//
// Why: after backjump to the second-highest decision level, the position-1
// literal is the watch most likely to become unassigned (it sits at the
// level we just unwound through). The asserting literal at position 0
// stays assigned and drives the unit propagation that makes the learnt
// clause immediately useful. Violating this invariant breaks the
// watched-literal scheme silently — both watches stay falsified after
// backjump, but propagate has no event to fire on, so the unit never
// triggers.
func (s *solver) addLearntClause(lits []literal) clauseRef {
	c := clause{learnt: true, lits: lits}
	if len(lits) >= 2 {
		maxIdx := 1
		maxLevel := s.level[int32(lits[1])>>1]
		for k := 2; k < len(lits); k++ {
			lv := s.level[int32(lits[k])>>1]
			if lv > maxLevel {
				maxLevel = lv
				maxIdx = k
			}
		}
		c.lits[1], c.lits[maxIdx] = c.lits[maxIdx], c.lits[1]
	}
	return s.addClause(c)
}

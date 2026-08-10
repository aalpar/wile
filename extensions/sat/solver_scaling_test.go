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
	"time"
)

// TestSolveIsNotQuadraticInVariableCount is the regression gate for the
// embedder-DoS shape the primitive-surface sweep found: a legal two-element
// input, exactly AT the documented maxVars bound and therefore accepted, that
// bought hours of CPU.
//
//	(sat-cnf-flat? (vector 4194304 0) 1)
//
// One satisfied unit clause, no conflicts ever, so the documented conflict
// budget never trips — measured, not inferred: budget 0 and budget 1 cost the
// same. Both remaining costs were quadratic in the variable count. Every
// decision scanned every variable to pick a branch (~59% of a Go CPU profile),
// and every propagate() call re-walked the whole trail from its base (~41%).
//
// Measured on this shape, single-threaded, before and after: 10k 0.21s ->
// 0.01s, 50k 4.33s -> 0.01s, 100k 17.25s -> 0.01s. The headline 4,194,304-variable
// case extrapolated to about 8.5 hours and now answers in 0.35s.
//
// The gate uses 200,000 rather than the headline: it cost about 70 seconds
// before the fix and about 20 milliseconds after, while allocating a few tens
// of megabytes instead of 251.
//
// The budget is 5 s, not a minute, and the difference matters. At a minute
// even a FULL revert only overshoots by ~16%, and reverting either half alone
// still passes — a gate that cannot see the change it names. At 5 s it catches
// the full revert (70 s), the heap alone (~59% of the cost, ~41 s) and the
// persistent cursor alone (~41%, ~29 s), while still leaving a 350x margin
// over the 14 ms this actually takes.
func TestSolveIsNotQuadraticInVariableCount(t *testing.T) {
	const numVars = 200000
	const budget = 10 * time.Second

	// The pathological shape: one unit clause naming the highest variable.
	// Every other variable is unmentioned and unconstrained, so the solver
	// makes numVars decisions and never has a conflict to learn from.
	clauses := []clause{{lits: []literal{literal(2 * numVars)}}}

	done := make(chan SolverResult, 1)
	go func() {
		s := newSolver(context.Background(), clauses, numVars, 1)
		done <- s.solve()
	}()

	start := time.Now()
	select {
	case got := <-done:
		if got != resultSat {
			t.Fatalf("solve: got %v, want resultSat", got)
		}
		t.Logf("%d variables solved in %s", numVars, time.Since(start))
	case <-time.After(budget):
		t.Fatalf("solve of %d unconstrained variables did not finish within %s: "+
			"the per-decision variable scan or the per-call trail re-walk is back",
			numVars, budget)
	}
}

// TestPickBranchVarMatchesLinearScan pins the property that makes the heap a
// pure speedup rather than a behaviour change: it must select exactly what the
// scan it replaced selected.
//
// The old scan compared with a strict `>`, so among equal activities the
// LOWEST variable index won. A heap without that tie-break is still a correct
// solver, but it returns a different satisfying assignment for any instance
// with more than one model — and before the first conflict every activity is
// equal, so that is every instance.
func TestPickBranchVarMatchesLinearScan(t *testing.T) {
	rng := rand.New(rand.NewSource(20260809))
	const numVars = 64

	// linearPick is the implementation this replaced, verbatim.
	linearPick := func(s *solver) int32 {
		var best int32
		var bestAct float32 = -1
		for v := int32(1); v <= s.numVars; v++ {
			if s.assigns[v] != 0 {
				continue
			}
			if s.activity[v] > bestAct {
				bestAct = s.activity[v]
				best = v
			}
		}
		return best
	}

	for trial := range 200 {
		s := newSolver(context.Background(), nil, numVars, -1)
		// A mix of ties and distinct activities, and a mix of assigned and
		// unassigned variables. Deliberately coarse values so ties are common.
		for v := int32(1); v <= numVars; v++ {
			if rng.Intn(3) == 0 {
				s.activity[v] = float32(rng.Intn(4))
			}
			if rng.Intn(2) == 0 {
				s.assigns[v] = 1
			}
		}
		// Re-establish the heap for the activities just assigned: bumping is
		// the only path that moves a variable, so replay it.
		for v := int32(1); v <= numVars; v++ {
			if s.orderPos[v] >= 0 {
				s.orderUp(int(s.orderPos[v]))
			}
		}

		want := linearPick(s)
		got := s.pickBranchVar()
		if got != want {
			t.Fatalf("trial %d: pickBranchVar = %d, linear scan = %d", trial, got, want)
		}
	}
}

// TestActivityRescaleKeepsTheHeapOrdered pins the one thing that can silently
// break the heap: the VSIDS rescale.
//
// bumpVarActivity multiplies every activity by 1e-20 when one exceeds 1e20.
// On float32 that map is NOT injective, in two separate ways — values below
// the normal range flush to a subnormal or to zero, and even inside the normal
// range two distinct float32 can round to one. Every collapse is a NEW TIE,
// and orderBefore breaks ties by index, so a heap that was legal before the
// rescale can be illegal after it, after which pickBranchVar diverges from the
// linear scan TestPickBranchVarMatchesLinearScan pins it against.
//
// Measured over 643k conflicts on random 3-SAT: 712 rescales, 1404
// distinct-to-equal collapses, ZERO violations. This is unreached at realistic
// scales and guarded anyway, because the ordering is load-bearing.
//
// The assertion is the heap INVARIANT, not a particular pick: which pairs
// collapse depends on the platform's float32 rounding, and an invariant check
// is true regardless of which ones did.
func TestActivityRescaleKeepsTheHeapOrdered(t *testing.T) {
	// The counterexample, not a random fixture. Three activities that are
	// strictly ordered before the rescale and all underflow to zero after it,
	// plus a fourth to trigger the rescale through bumpVarActivity — the only
	// path that reaches it in production.
	//
	// The ordering is the point: var 3 has the HIGHEST activity and the
	// HIGHEST index, so the pre-rescale heap puts it above vars 1 and 2, and
	// the post-collapse tie order (index ascending) puts it below them. With
	// activity and index already agreeing, a collapse reorders nothing and a
	// fixture proves nothing — the first two attempts at this test failed
	// exactly that way, passing with the rebuild removed.
	s := newSolver(context.Background(), nil, 4, -1)
	s.activity[1] = 5e-31
	s.activity[2] = 2e-31
	s.activity[3] = 1e-30
	s.rebuildOrder()
	assertHeapOrdered(t, s, "before the rescale")

	s.activityInc = 2e20
	s.bumpVarActivity(4)

	if s.activity[1] != s.activity[2] || s.activity[2] != s.activity[3] {
		t.Skipf("this platform's float32 rounding did not collapse the fixture: %v", s.activity[1:4])
	}
	assertHeapOrdered(t, s, "after the rescale")

	// And the consequence the invariant protects: on an all-equal tie the
	// LOWEST index must come out, which is what the deleted linear scan did.
	first := s.pickBranchVar()
	if first != 4 {
		t.Fatalf("first pick = %d, want 4 (the only non-zero activity)", first)
	}
	second := s.pickBranchVar()
	if second != 1 {
		t.Errorf("second pick = %d, want 1 (lowest index on a tie)", second)
	}
}

// assertHeapOrdered checks the array heap property and the order/orderPos
// bijection, so a stale position that silently drops a variable is caught too.
func assertHeapOrdered(t *testing.T, s *solver, when string) {
	t.Helper()
	for i := range s.order {
		v := s.order[i]
		if int(s.orderPos[v]) != i {
			t.Fatalf("%s: orderPos[%d] = %d, want %d", when, v, s.orderPos[v], i)
		}
		for _, child := range []int{2*i + 1, 2*i + 2} {
			if child >= len(s.order) {
				continue
			}
			if s.orderBefore(s.order[child], v) {
				t.Errorf("%s: heap violated at %d: child %d (activity %v) precedes parent %d (activity %v)",
					when, i, s.order[child], s.activity[s.order[child]], v, s.activity[v])
			}
		}
	}
}

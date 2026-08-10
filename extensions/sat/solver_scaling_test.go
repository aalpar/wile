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
	const budget = 5 * time.Second

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

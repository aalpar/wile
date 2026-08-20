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

// A procedure whose body BUILDS A CLOSURE must still return its parameter frame
// to the pool (flat-closure arc, phase 4c).
//
// Under linked closures `OpMakeClosure` recorded mc.env as the new closure's
// static link, so the frame had to be marked unpoolable or RestoreAndRelease
// would recycle a frame the closure still pointed at. A flat closure records
// env.TopLevel() instead and copies its free variables into a vector, so it
// reaches no activation frame and the clear became unnecessary — for every
// template except one that retains the lexical env on purpose.
//
// This is the arc's compensating win and it is otherwise UNMEASURED by the
// suite: dropping the narrowing costs only pool hit rate, which no value
// assertion can see. It is also the direction that has historically been unsafe
// — restoring the flag wholesale is a use-after-release reverted three times —
// so the negative control below matters as much as the positive.

package wile

import (
	"context"
	"testing"
)

// closureReturningWorkload calls a procedure that builds and returns a closure
// directly in its body, with no enclosing `let`. The absence of the `let` is the
// whole point: OpPushEnv clears envPooled for its own reason (a let frame really
// is parented at mc.env), which would mask what this measures.
const closureReturningWorkload = `(begin
  (define (mk a) (lambda () a))
  (let loop ((i 0) (acc 0))
    (if (= i 500) acc (loop (+ i 1) (+ acc ((mk i)))))))`

// runCountingCounters evaluates code on a default Engine and returns the VM
// counters for the run.
func runCountingCounters(t *testing.T, code string) (string, uint64, uint64) {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	got, err := engine.EvalMultiple(ctx, code)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	c := engine.LastCounters()
	return got.SchemeString(), c.EnvFramePoolReleases, c.ClosuresApplied
}

// TestClosureCreatingBodyReleasesItsFrame pins that the frames of
// closure-building procedure bodies come back to the pool.
//
// The value assertion is the guard on the risky direction: 500 closures each
// copied a DIFFERENT `a`, so a frame recycled while one of them still read
// through it would show up as a wrong sum, not merely as a lost optimization.
func TestClosureCreatingBodyReleasesItsFrame(t *testing.T) {
	got, releases, applies := runCountingCounters(t, closureReturningWorkload)

	// sum 0..499
	const want = "124750"
	if got != want {
		t.Fatalf("workload evaluates to %s, want %s — a wrong sum here means a "+
			"frame was recycled while a closure built over it was still live, "+
			"which is the failure mode this narrowing must not reintroduce",
			got, want)
	}

	t.Logf("env_frame_pool_releases=%d closures_applied=%d", releases, applies)

	// MEASURED, AND THE MARGIN IS THE WHOLE POINT. 1501 with the narrowing,
	// 1001 without: the difference is exactly the 500 calls to mk, whose frames
	// leaked before phase 4c because OpMakeClosure cleared envPooled
	// unconditionally.
	//
	// A LOOSE BOUND DOES NOT WORK HERE. The first version of this test asserted
	// >= 500 and passed in BOTH directions — the loop and the stdlib supply 1001
	// releases on their own, which swamps the signal. Any replacement must keep a
	// bound tighter than 500 releases, or it is measuring the ambient traffic
	// rather than the property.
	const wantReleases = 1501
	if releases != wantReleases {
		t.Errorf("env_frame_pool_releases = %d, want %d. Below %d (1001 is the "+
			"pre-4c figure) a procedure body that builds a closure has stopped "+
			"returning its frame to the pool, i.e. the envPooled narrowing at "+
			"OpMakeClosure was lost. Above it, something started releasing MORE "+
			"than this arc licenses — check that against Invariant H before "+
			"updating this number", releases, wantReleases, wantReleases)
	}
}

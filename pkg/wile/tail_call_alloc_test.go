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

package wile

import (
	"context"
	"testing"
)

// Phase 2 (TDD) regression tests for plans/2026-06-10-tail-frame-recycling.local.md.
//
// Every function whose body ends in a call currently leaks its own env frame to
// GC instead of recycling it via the freelist (~1 frame/call). These tests pin
// the leak: they measure the *slope* of heap allocations against problem size.
//
//   - leaked:   slope ≈ 1.0 frame per tail call  → FAILS the assertions below
//   - recycled: slope ≈ 0.0 (O(1) frames total)  → PASSES
//
// EXPECTED STATE: RED until Phase 3 wires the per-frame `captured` bit and the
// tail-release guard. After Phase 3 these must go green, and the Phase 4
// continuation suite must stay green.
//
// Loop/recursion counters stay inside the cached-integer window
// [-32768, 32767] (values/integer.go) so the counter itself allocates no boxed
// Integer — env-frame allocation is the only size-dependent signal.

// allocsForRun compiles code once (after running setup into the engine's global
// scope) and returns the average heap allocations of a single Engine.Run.
//
// Each Engine.Run mints a fresh top-level context with a cold per-thread pool
// (AcquireTopLevelContext), so cross-Run warm-up does not carry over. That is
// fine for these probes: the tail-call leak is intrinsic *within* a single run
// (a leaked frame is never returned to the pool during that run), so the
// size-dependent slope isolates it regardless of cross-Run pool state.
func allocsForRun(t *testing.T, setup, code string) float64 {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if setup != "" {
		_, err = engine.Eval(ctx, engine.MustParse(ctx, setup))
		if err != nil {
			t.Fatal(err)
		}
	}
	compiled, err := engine.Compile(ctx, engine.MustParse(ctx, code))
	if err != nil {
		t.Fatal(err)
	}
	return testing.AllocsPerRun(5, func() {
		_, runErr := engine.Run(ctx, compiled)
		if runErr != nil {
			t.Fatal(runErr)
		}
	})
}

// allocSlope returns allocations-per-extra-unit-of-work: (big - small) / (workBig
// - workSmall). The fixed per-Run overhead (result boxing, dispatch) cancels in
// the subtraction, leaving the per-call env-frame cost.
func allocSlope(small, big float64, workSmall, workBig int) float64 {
	return (big - small) / float64(workBig-workSmall)
}

// TestTailLoopEnvFrameAllocations is the headline leak probe: a pure tail loop.
// With recycling it is O(1) allocations regardless of trip count; today it is
// O(n). The per-iteration slope must be near zero.
func TestTailLoopEnvFrameAllocations(t *testing.T) {
	// SKIPPED: the tail-frame recycler is unimplemented. Phase 3 of
	// plans/2026-06-10-tail-frame-recycling.local.md proved the per-frame
	// `captured`-bit design unsound — it crashed TestApplyWindingStackInheritance
	// and TestCallCCCoroutines with use-after-release (a captured continuation
	// reached frames the {Copy, MarkChainShared} chokepoints did not mark). The
	// runtime approach was reverted. Un-skip when the compile-time self-tail
	// subset (the plan's fallback) lands. Documents the O(1) target (slope 2.0
	// today → < 0.1 recycled).
	t.Skip("tail-frame recycling unimplemented — runtime design unsound, see plan")
	const setup = `(define (tail-loop i n) (if (>= i n) i (tail-loop (+ i 1) n)))`

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, setup, "(tail-loop 0 10000)")
	big := allocsForRun(t, setup, "(tail-loop 0 30000)")

	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("tail-loop allocs: %d trips=%.0f, %d trips=%.0f, slope=%.3f frames/iter",
		smallTrips, small, bigTrips, big, slope)

	// Recycled target: well under 0.1 frame/iteration. Leak sits at ~1.0.
	if slope > 0.1 {
		t.Errorf("tail loop leaks env frames: %.3f frames/iter (want < 0.1); "+
			"%d→%.0f allocs, %d→%.0f allocs", slope, smallTrips, small, bigTrips, big)
	}
}

// TestFibEnvFrameAllocations probes the fib shape from the plan: fib's body ends
// in (+ (fib ...) (fib ...)), so the final (+ ...) is a tail call from fib's
// frame — leaking ~1 frame per fib call. Slope is measured per recursive call.
func TestFibEnvFrameAllocations(t *testing.T) {
	// SKIPPED for the same reason as TestTailLoopEnvFrameAllocations: the
	// tail-frame recycler is unimplemented (design unsound, reverted). fib's
	// tail (+ ...) leaks via OpPullApply→applyForeign, the re-entrant path where
	// releasing the caller frame before the foreign call corrupts continuations.
	t.Skip("tail-frame recycling unimplemented — runtime design unsound, see plan")
	const setup = `(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))`

	// calls(fib n) = 2*fib(n+1) - 1.  fib(18): 2*4181-1=8361, fib(20): 2*10946-1=21891.
	const smallCalls = 8361
	const bigCalls = 21891
	small := allocsForRun(t, setup, "(fib 18)")
	big := allocsForRun(t, setup, "(fib 20)")

	slope := allocSlope(small, big, smallCalls, bigCalls)
	t.Logf("fib allocs: fib(18)=%.0f, fib(20)=%.0f, slope=%.3f frames/call",
		small, big, slope)

	if slope > 0.1 {
		t.Errorf("fib leaks env frames: %.3f frames/call (want < 0.1); "+
			"fib(18)→%.0f allocs, fib(20)→%.0f allocs", slope, small, big)
	}
}

// TestNonTailRecursionControl is the control proving the fix is *specific* to
// tail-position frames. Non-tail recursion keeps every frame in the chain
// simultaneously live, so it genuinely allocates O(depth) frames per run — and
// the Phase 3 tail-release guard never fires on these frames (the recursive call
// is non-tail; nt's body ends in the literal 0, a normal return). The result
// stays cached (0), isolating env frames from integer boxing.
//
// Unlike the leak probes above, this assertion PASSES before AND after Phase 3:
// the slope must stay elevated. If the fix wrongly recycled these still-live
// frames the slope would collapse here (and the Phase 4 continuation suite would
// crash). A floor assertion is the cheap allocation-side guard for that.
func TestNonTailRecursionControl(t *testing.T) {
	const setup = `(define (nt n) (if (<= n 0) 0 (begin (nt (- n 1)) 0)))`

	// Depths stay under the default 10000 call-depth limit — non-tail recursion
	// consumes continuation depth (unlike the tail loop).
	const smallDepth = 2000
	const bigDepth = 8000
	small := allocsForRun(t, setup, "(nt 2000)")
	big := allocsForRun(t, setup, "(nt 8000)")

	slope := allocSlope(small, big, smallDepth, bigDepth)
	t.Logf("non-tail control allocs: depth %d=%.0f, depth %d=%.0f, slope=%.3f frames/level",
		smallDepth, small, bigDepth, big, slope)

	// Floor: non-tail frames are genuinely live, so per-level allocation must NOT
	// drop to the tail-call target. The fix must leave this shape untouched.
	if slope < 0.5 {
		t.Errorf("non-tail recursion no longer allocates per live level: %.3f "+
			"frames/level (want >= 0.5) — the tail-release guard may be "+
			"over-firing on still-live frames", slope)
	}
}

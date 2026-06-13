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

	"github.com/aalpar/wile/stdlib"
)

// Phase 3 (TDD RED) allocation tests for the escape-gated frame-reclamation
// optimizer (plans/2026-06-11-escape-gated-frame-allocation.local.md §"Phase 3").
//
// Every function whose body ends in a tail call currently leaks its own env
// frame to GC instead of releasing it to the FreeList pool (~1 frame/call). The
// Phase-1 classifier (validate.ClassifyFrameReclaim, DONE) already proves at
// compile time which functions can never expose their frame to a continuation;
// Phase 4 codegen (OpApplyTailReleasing for general tail calls, OpSelfTailCall
// for self/depth-0 loops, NOT YET BUILT) will release the proven-safe frames.
// These tests pin the leak by measuring the *slope* of heap allocations against
// problem size:
//
//   - leaked (today):   slope ≈ 1–2 frames per tail call  → FAILS the < 0.1 assertions
//   - released (Phase 4): slope ≈ 0    (O(1) frames total) → PASSES
//
// EXPECTED STATE: the leak probes are RED until Phase 4 wires the release
// opcodes. They are RED *for the right reason* — the classifier proves these
// shapes reclaimable (asserted below via classifyAmbient) and a memory profile
// confirms every size-dependent allocation is an env frame component
// (newEnvFramePoolEntry + EnvironmentFrame.PreAllocateBindings, both owned by the
// pool entry the release recycles). After Phase 4 the leak probes must go green
// and the Phase-5 continuation suite must stay green.
//
// The negative controls are the inverse: self-tail loops the classifier REFUSES
// to release (call/cc in body, escaping closure, mutated callee). They allocate
// today and must KEEP allocating after Phase 4 — a collapsed slope there means
// the release opcode over-fired on a frame a continuation can still reach (the
// exact corruption that killed the abandoned runtime recycler, see
// tail-frame-recycling.local.md Phase 3). They PASS before and after Phase 4.
//
// Engine config: KitchenSink + WithImmutableTopLevel(). The flag is load-bearing,
// not incidental — it stamps the producer's Stable bit on defined-once,
// never-set! top-level defines AND (Phase 2.6) on the ambient capture-safe base
// primitives (+, -, <, …). Without it NO same-unit edge is immutable, the
// classifier recovers nothing, and the leak probes would be RED forever
// regardless of Phase 4 (RED for the WRONG reason). See framereclaim_ambient_test.go.
//
// Loop/recursion counters stay inside the cached-integer window [-32768, 32767]
// (values/integer.go, verified) so the counter itself allocates no boxed Integer
// — env-frame allocation is the only size-dependent signal.

// allocsForRun compiles code once (after running setup into the engine's global
// scope) and returns the average heap allocations of a single Engine.Run.
//
// The engine enables WithImmutableTopLevel under a KitchenSink profile so the
// compiler stamps Stable on same-unit defines and ambient capture-safe
// primitives — the prerequisite for the Phase-1 classifier to call the probe
// shapes reclaimable, which is what makes Phase 4 release their frames.
//
// Each Engine.Run mints a fresh top-level context with a cold per-thread pool
// (AcquireTopLevelContext), so cross-Run warm-up does not carry over. That is
// fine for these probes: the tail-call leak is intrinsic *within* a single run
// (a leaked frame is never returned to the pool during that run), so the
// size-dependent slope isolates it regardless of cross-Run pool state.
func allocsForRun(t *testing.T, setup, code string) float64 {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths(),
		WithImmutableTopLevel(),
	)
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

// assertReclaimable fails unless the Phase-1 classifier's verdict for the named
// top-level define under WithImmutableTopLevel matches want. It pins each probe's
// "RED/floor for the right reason": a leak probe is RED because codegen is
// missing (classifier already says reclaimable), and a negative control keeps
// allocating because the classifier refuses it — not by accident of shape.
// src must be a single begin-wrapped unit so the producer stamps Stable.
func assertReclaimable(t *testing.T, src, fn string, want bool) {
	t.Helper()
	verdict := classifyAmbient(context.Background(), t, src, true)
	got, present := verdict[fn]
	if !present {
		t.Fatalf("classifier produced no verdict for top-level define %q (verdict=%v)", fn, verdict)
	}
	if got != want {
		t.Fatalf("classifier reclaimable[%s]=%v, want %v — probe would be %s for the wrong reason",
			fn, got, want, map[bool]string{true: "green", false: "permanently red"}[want])
	}
}

// TestTailLoopEnvFrameAllocations is the headline leak probe: a self-recursive
// top-level tail loop. With frame release it is O(1) allocations regardless of
// trip count; today it is O(n). The per-iteration slope must be near zero.
//
// RED until Phase 4: the classifier proves tail-loop reclaimable (asserted), but
// no opcode releases its frame yet, so each iteration allocates one env-frame
// pool entry + its bindings slice (observed slope ≈ 2.0). Both are recycled by
// the pool once OpSelfTailCall/OpApplyTailReleasing release p.env → slope < 0.1.
func TestTailLoopEnvFrameAllocations(t *testing.T) {
	const def = "(begin (define (tail-loop i n) (if (>= i n) i (tail-loop (+ i 1) n)))\n)"
	assertReclaimable(t, def, "tail-loop", true)

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, def, "(tail-loop 0 10000)")
	big := allocsForRun(t, def, "(tail-loop 0 30000)")

	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("tail-loop allocs: %d trips=%.0f, %d trips=%.0f, slope=%.3f frames/iter",
		smallTrips, small, bigTrips, big, slope)

	// Released target: well under 0.1 frame/iteration. Leak sits at ~2.0 today.
	if slope > 0.1 {
		t.Errorf("tail loop leaks env frames: %.3f frames/iter (want < 0.1); "+
			"%d→%.0f allocs, %d→%.0f allocs", slope, smallTrips, small, bigTrips, big)
	}
}

// TestFibEnvFrameAllocations probes the general tail-call shape from the plan:
// fib's body ends in (+ (fib ...) (fib ...)), so the final (+ ...) is a tail call
// from fib's frame — leaking ~1 frame per fib call (slope ≈ 1.0, the cleanest
// probe: every size-dependent allocation is the one fib frame). Slope is measured
// per recursive call. fib classifies reclaimable only because (+ - <) are stamped
// Stable under the flag (Phase 2.6); its release is the OpApplyTailReleasing path.
func TestFibEnvFrameAllocations(t *testing.T) {
	const def = "(begin (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))\n)"
	assertReclaimable(t, def, "fib", true)

	// calls(fib n) = 2*fib(n+1) - 1.  fib(18): 2*4181-1=8361, fib(20): 2*10946-1=21891.
	const smallCalls = 8361
	const bigCalls = 21891
	small := allocsForRun(t, def, "(fib 18)")
	big := allocsForRun(t, def, "(fib 20)")

	slope := allocSlope(small, big, smallCalls, bigCalls)
	t.Logf("fib allocs: fib(18)=%.0f, fib(20)=%.0f, slope=%.3f frames/call",
		small, big, slope)

	if slope > 0.1 {
		t.Errorf("fib leaks env frames: %.3f frames/call (want < 0.1); "+
			"fib(18)→%.0f allocs, fib(20)→%.0f allocs", slope, small, big)
	}
}

// TestNamedLetEnvFrameAllocations probes a named-let self-tail loop. Unlike the
// top-level defines above, named-let's loop variable is a LOCAL letrec binding,
// so the interprocedural top-level classifier (ClassifyFrameReclaim) does not
// verdict it — its payoff comes through Phase 4's OpSelfTailCall (the self/depth-0
// rewrite: in-place rebind + pc=0), gated by the same per-closure capture proof.
// Today each iteration leaks one env-frame pool entry + its bindings slice
// (slope ≈ 2.0); after the self-tail rewrite the loop reuses one frame → < 0.1.
func TestNamedLetEnvFrameAllocations(t *testing.T) {
	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, "", "(let loop ((i 0)) (if (>= i 10000) i (loop (+ i 1))))")
	big := allocsForRun(t, "", "(let loop ((i 0)) (if (>= i 30000) i (loop (+ i 1))))")

	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("named-let allocs: %d trips=%.0f, %d trips=%.0f, slope=%.3f frames/iter",
		smallTrips, small, bigTrips, big, slope)

	if slope > 0.1 {
		t.Errorf("named-let leaks env frames: %.3f frames/iter (want < 0.1); "+
			"%d→%.0f allocs, %d→%.0f allocs", slope, smallTrips, small, bigTrips, big)
	}
}

// TestNonTailRecursionControl is the control proving the fix is *specific* to
// tail-position frames. Non-tail recursion keeps every frame in the chain
// simultaneously live, so it genuinely allocates O(depth) frames per run — and
// the Phase 4 release never fires on these frames (the recursive call is
// non-tail; nt's body ends in the literal 0, a normal return). The result stays
// cached (0), isolating env frames from integer boxing.
//
// Unlike the leak probes above, this assertion PASSES before AND after Phase 4:
// the slope must stay elevated. If the fix wrongly released these still-live
// frames the slope would collapse here (and the Phase 5 continuation suite would
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
			"frames/level (want >= 0.5) — the release may be over-firing on "+
			"still-live frames", slope)
	}
}

// allocFloorControl runs a self-tail loop the classifier must REJECT and asserts
// (a) the classifier verdict is non-reclaimable and (b) the per-iteration alloc
// slope stays above floorPerIter. Both hold before AND after Phase 4: a rejected
// shape is never released, so its frames keep leaking. A collapse here is the
// over-fire alarm — Phase 4 released a frame a continuation can still reach.
func allocFloorControl(t *testing.T, def, fn, small, big string, floorPerIter float64) {
	t.Helper()
	assertReclaimable(t, def, fn, false)

	const smallTrips = 10000
	const bigTrips = 30000
	s := allocsForRun(t, def, small)
	b := allocsForRun(t, def, big)
	slope := allocSlope(s, b, smallTrips, bigTrips)
	t.Logf("%s control allocs: %d=%.0f, %d=%.0f, slope=%.3f frames/iter",
		fn, smallTrips, s, bigTrips, b, slope)

	if slope < floorPerIter {
		t.Errorf("%s no longer allocates per iteration: %.3f frames/iter (want >= %.1f) "+
			"— the release over-fired on a classifier-rejected shape", fn, slope, floorPerIter)
	}
}

// TestCallCCBodyAllocControl: a self-tail loop with call/cc in its body. The
// captured continuation pins the frame, so bodyReferencesCaptureOperator rejects
// the define and Phase 4 must never release it. (call/cc here returns i and
// discards k, so the loop runs as a plain counter while still being disqualified.)
func TestCallCCBodyAllocControl(t *testing.T) {
	const def = "(begin (define (cc i n) (if (>= i n) i (cc (+ (call/cc (lambda (k) i)) 1) n)))\n)"
	allocFloorControl(t, def, "cc", "(cc 0 10000)", "(cc 0 30000)", 1.0)
}

// TestEscapingLambdaAllocControl: a self-tail loop that creates an escaping
// closure each iteration ((lambda () i) passed to cons, not in operator
// position). bodyCreatesEscapingClosure rejects the define — the closure may
// outlive the call and parent the frame — so Phase 4 must never release it.
func TestEscapingLambdaAllocControl(t *testing.T) {
	const def = "(begin (define (esc i n) (if (>= i n) i (begin (cons (lambda () i) (quote ())) (esc (+ i 1) n))))\n)"
	allocFloorControl(t, def, "esc", "(esc 0 10000)", "(esc 0 30000)", 1.0)
}

// TestMutatedCalleeAllocControl: a self-tail loop whose body calls a callee that
// is set! within the same unit. The in-unit set! leaves sq non-Stable (so the
// set! is legal — it was never stamped immutable), and the mutable edge to sq
// makes the caller non-reclaimable. Phase 4 must never release a frame whose
// callee can be rebound to a capturing procedure.
func TestMutatedCalleeAllocControl(t *testing.T) {
	const def = "(begin (define (sq x) (* x x)) (set! sq sq) " +
		"(define (mc i n) (if (>= i n) i (begin (sq i) (mc (+ i 1) n))))\n)"
	allocFloorControl(t, def, "mc", "(mc 0 10000)", "(mc 0 30000)", 1.0)
}

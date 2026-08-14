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

	"github.com/aalpar/wile/pkg/stdlib"
)

// Allocation-slope tests for the escape-gated frame-reclamation optimizer
// (plans/2026-06-11-escape-gated-frame-allocation.local.md). Authored as Phase-3
// TDD RED probes; Phase 4 codegen has since SHIPPED and turned them green, so they
// now guard both directions: a leak creeping back (slope rising) and the release
// over-firing on a still-live frame (a control's slope collapsing).
//
// Every function whose body ends in a tail call would otherwise leak its own env
// frame to GC instead of releasing it to the FreeList pool (~1 frame/call). The
// Phase-1 classifier (validate.ClassifyFrameReclaim) proves at compile time which
// functions can never expose their frame to a continuation; Phase 4 codegen
// (OpReleaseEnvFrame for general tail calls, OpSelfTailCall for self/depth-0 loops
// — both SHIPPED) releases the proven-safe frames. These tests pin the behavior by
// measuring the *slope* of heap allocations against problem size:
//
//   - leaked (pre-Phase-4): slope ≈ 1–2 frames per tail call → FAILS the < 0.1 assertions
//   - released (now):       slope ≈ 0   (O(1) frames total)  → PASSES
//
// EXPECTED STATE: the reclaimable probes are GREEN (Phase 4 wires the release
// opcodes). They pass *for the right reason* — the classifier proves these shapes
// reclaimable (asserted below via classifyAmbient) and a memory profile confirms
// every size-dependent allocation is an env frame component (newEnvFramePoolEntry +
// EnvironmentFrame.PreAllocateBindings, both owned by the pool entry the release
// recycles). The Phase-5 continuation suite must stay green.
//
// The negative controls are the inverse: self-tail loops the classifier REFUSES
// to release (call/cc in body, escaping closure, mutated callee). They allocate
// and must KEEP allocating — a collapsed slope there means the release opcode
// over-fired on a frame a continuation can still reach (the exact corruption that
// killed the abandoned runtime recycler, see tail-frame-recycling.local.md
// Phase 3). They PASS regardless of the optimization.
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

// TestReclaimableLoopAcrossContinuationReentry is the behavioral counterpart to
// the allocation-slope probes: it runs a frame-reclaimable self-tail loop
// (accumulate) WITH the optimization armed (WithImmutableTopLevel), inside a
// program that captures a continuation and re-enters it multiple times, and
// asserts the RESULTING VALUE. A slope probe only catches over-firing if it
// collapses the slope; this catches the subtler failure the design exists to
// prevent — a reused/released frame corrupting a re-entered continuation would
// change the value, not merely the allocation count.
//
// accumulate (Stable, capture-safe callees) emits OpSelfTailCall and runs on every
// re-entry; tag walks 0→1→2 across two re-entries, so visits=3, accumulate=50, and
// the result is 3*50=150 iff the optimization left the continuation intact.
func TestReclaimableLoopAcrossContinuationReentry(t *testing.T) {
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
	const program = `
(define (accumulate i n acc)
  (if (>= i n) acc (accumulate (+ i 1) n (+ acc 1))))
(let ((k #f) (visits 0))
  (let ((tag (call/cc (lambda (c) (set! k c) 0))))
    (set! visits (+ visits 1))
    (let ((s (accumulate 0 50 0)))
      (if (< tag 2)
          (k (+ tag 1))
          (* visits s)))))`
	result, err := engine.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "150" {
		t.Errorf("reclaimable loop across continuation re-entry = %s, want 150 "+
			"(a corrupted re-entered continuation would change this)", got)
	}
}

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

// TestMutualRecursionEnvFrameAllocations is Lever A's headline probe: ping and pong
// are mutually tail-recursive top-level defines. The per-body predicate refuses each
// one's tail call to the OTHER (not self, not a capture-safe primitive), so today every
// cross-call leaks a frame: slope ~2.0 (env-frame pool entry + bindings slice, the same
// two allocations the other leak probes in this file observe). The interprocedural
// classifier proves both safe (asserted); wiring its verdict into codegen drops it to ~0.
func TestMutualRecursionEnvFrameAllocations(t *testing.T) {
	const def = "(begin " +
		"(define (ping n) (if (= n 0) 'done (pong (- n 1)))) " +
		"(define (pong n) (if (= n 0) 'done (ping (- n 1))))\n)"
	assertReclaimable(t, def, "ping", true)
	assertReclaimable(t, def, "pong", true)

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, def, "(ping 10000)")
	big := allocsForRun(t, def, "(ping 30000)")
	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("mutual-recursion allocs: ping(10000)=%.0f, ping(30000)=%.0f, slope=%.3f frames/call",
		small, big, slope)
	if slope > 0.1 {
		t.Errorf("mutual recursion leaks env frames: %.3f frames/call (want < 0.1); "+
			"%d->%.0f, %d->%.0f", slope, smallTrips, small, bigTrips, big)
	}
}

// TestThreeCycleEnvFrameAllocations extends the mutual-recursion probe to a 3-node
// cycle (a->b->c->a). Like the 2-cycle, every node is structurally safe over
// immutable edges, so the mayCapture fixpoint converges in a single sweep — this
// does NOT exercise deeper solver convergence (a clean N-cycle forces no transitive
// knockout; that needs an unsafe node to propagate). What it adds over the 2-cycle
// is builder + codegen coverage: three same-unit forward references resolved into
// the reclaim graph, and the frameReuseForDefine union emitting OpReleaseEnvFrame
// for three mutually recursive bodies. Each define's only non-self callee is another
// user define, so BodyIsFrameReleasable refuses all three — the release comes SOLELY
// from frameReclaimVerdict. Slope ~2.0 before the verdict is wired, ~0 after.
func TestThreeCycleEnvFrameAllocations(t *testing.T) {
	const def = "(begin " +
		"(define (cyc-a n) (if (= n 0) 'done (cyc-b (- n 1)))) " +
		"(define (cyc-b n) (if (= n 0) 'done (cyc-c (- n 1)))) " +
		"(define (cyc-c n) (if (= n 0) 'done (cyc-a (- n 1))))\n)"
	assertReclaimable(t, def, "cyc-a", true)
	assertReclaimable(t, def, "cyc-b", true)
	assertReclaimable(t, def, "cyc-c", true)

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, def, "(cyc-a 10000)")
	big := allocsForRun(t, def, "(cyc-a 30000)")
	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("3-cycle allocs: cyc-a(10000)=%.0f, cyc-a(30000)=%.0f, slope=%.3f frames/call",
		small, big, slope)
	if slope > 0.1 {
		t.Errorf("3-cycle leaks env frames: %.3f frames/call (want < 0.1); "+
			"%d->%.0f, %d->%.0f", slope, smallTrips, small, bigTrips, big)
	}
}

// TestUnlistedPrimitiveCalleeEnvFrameAllocations is Lever E's headline probe: a
// clean depth-0 self-tail loop (the exact shape as TestTailLoopEnvFrameAllocations,
// which is already reclaimable) whose only extra callee is eq? — a capture-safe
// primitive (PrimEqQ compares identity, never invokes a procedure) that is NOT in
// the old 16-name captureSafePrimitiveNames whitelist. eq? sits in argument position
// so the loop stays a clean self-tail, and it returns a cached boolean singleton (no
// per-iteration allocation), making the env frame the sole size-dependent signal —
// the ONLY difference from the known-slope-0 tail-loop is that one callee. Today both
// bodyCalleesAllCaptureSafe and the classifier read eq? as untrusted and refuse the
// loop, leaking its frame (slope ~2.0). Once eq? is trusted via the flipped
// PrimitiveSpec.InvokesProcedure default flowing to BindingMeta.CaptureSafe, both
// predicates trust it and the slope drops to ~0 — attributing the win cleanly to
// Lever E. (assq, the audit's deriv-shaped example, and max are trusted identically;
// eq? is used here because its boolean result needs no allocation, unlike assq's
// quoted-list argument or max's variadic boxing, which would confound the slope.)
func TestUnlistedPrimitiveCalleeEnvFrameAllocations(t *testing.T) {
	const def = "(begin (define (eq-loop n acc) " +
		"(if (= n 0) acc (eq-loop (- n 1) (eq? n acc))))\n)"
	assertReclaimable(t, def, "eq-loop", true)

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, def, "(eq-loop 10000 #f)")
	big := allocsForRun(t, def, "(eq-loop 30000 #f)")
	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("unlisted-prim-callee allocs: eq-loop(10000)=%.0f, eq-loop(30000)=%.0f, slope=%.3f frames/iter",
		small, big, slope)
	if slope > 0.1 {
		t.Errorf("unlisted-prim-callee loop leaks env frames: %.3f frames/iter (want < 0.1); "+
			"%d->%.0f, %d->%.0f", slope, smallTrips, small, bigTrips, big)
	}
}

// TestProvenProcedureCalleeEnvFrameAllocations is the end-to-end probe for Lever E's
// PROVE-THEM path (distinct from TestUnlistedPrimitiveCalleeEnvFrameAllocations, whose
// eq? callee is a Go primitive trusted via the InvokesProcedure capability). Here the
// trusted callee is zero? — a stdlib SCHEME procedure ((= z 0)) with no Go primitive
// behind it, trusted only because ProcedureBodyIsCaptureSafe proves its body
// capture-safe at compile time and stamps its binding CaptureSafe. zero? returns a
// cached boolean (no per-iteration allocation), so the env frame is the sole
// size-dependent signal. A regression where the compile-time proof stamps the binding
// but the classifier fails to consume it for a Scheme-procedure callee (vs. a
// primitive) would leave this slope at ~2.0; the proof path working drops it to ~0.
func TestProvenProcedureCalleeEnvFrameAllocations(t *testing.T) {
	const def = "(begin (define (z-loop n acc) " +
		"(if (= n 0) acc (z-loop (- n 1) (zero? n))))\n)"
	assertReclaimable(t, def, "z-loop", true)

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, def, "(z-loop 10000 #f)")
	big := allocsForRun(t, def, "(z-loop 30000 #f)")
	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("proven-proc-callee allocs: z-loop(10000)=%.0f, z-loop(30000)=%.0f, slope=%.3f frames/iter",
		small, big, slope)
	if slope > 0.1 {
		t.Errorf("proven-proc-callee loop leaks env frames: %.3f frames/iter (want < 0.1); "+
			"%d->%.0f, %d->%.0f", slope, smallTrips, small, bigTrips, big)
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

// --- Phase C: self-tail calls nested under a let (depth > 0) ---

// TestNestedLetSelfTailAllocations is Phase C's headline probe: a self-tail call
// wrapped in a body-level `let`. Before Phase C the reuse disposition was CLEARED
// on the let descent, so neither OpSelfTailCall nor OpReleaseEnvFrame fired and
// the callee's parameter frame leaked once per iteration.
//
// THE TARGET IS NOT ZERO, and the plan's Step 1 (\"assert 0-alloc\") was wrong
// about that. Measured decomposition at HEAD, per iteration:
//
//	depth-0 self-tail, no let ......................... 0.0
//	let in ARGUMENT position (self call still depth 0) . 3.0   <- the let frame alone
//	let WRAPPING the tail call (this probe) ............ 5.0   <- + the leaked param frame
//	two nested lets wrapping the tail call ............. 8.0   <- 3 per let + 2
//
// A `let` frame costs 3 allocations and is NOT pool-owned: OpPushEnv calls
// NewLocalEnvironment + NewEnvironmentFrameWithParent directly and sets
// envPooled=false, and in tail position no OpPopEnv is even emitted. Phase C pops
// those frames so the parameter frame can be rebound in place; it does not and
// cannot make them free. The fixed 2.0 it removes is exactly the amount the
// depth-0 case already saves, which is why the ceiling here is the let-frame
// floor of 3.0 and not 0.
//
// Pooling OpPushEnv frames is a different lever and is not this phase.
func TestNestedLetSelfTailAllocations(t *testing.T) {
	const def = "(begin (define (nested-loop i n) " +
		"(if (>= i n) i (let ((j (+ i 1))) (nested-loop j n))))\n)"
	assertReclaimable(t, def, "nested-loop", true)

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, def, "(nested-loop 0 10000)")
	big := allocsForRun(t, def, "(nested-loop 0 30000)")

	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("nested-loop allocs: %d trips=%.0f, %d trips=%.0f, slope=%.3f frames/iter",
		smallTrips, small, bigTrips, big, slope)

	// 3.0 is the let-frame floor; 5.0 is the pre-Phase-C leak. The midpoint
	// separates them with room for measurement noise in either direction.
	if slope > 4.0 {
		t.Errorf("self-tail call under a let leaks its parameter frame: %.3f frames/iter "+
			"(want the 3.0 let-frame floor, not the 5.0 leak); %d→%.0f allocs, %d→%.0f allocs",
			slope, smallTrips, small, bigTrips, big)
	}
}

// TestDoublyNestedLetSelfTailAllocations pins that the pop count is a COUNT and
// not a flag. Two lets wrap the self call, so the op must pop two frames; popping
// one would rebind the inner let's frame as if it were the parameter frame —
// which is corruption, not a lost optimization, and is why the value assertion in
// TestNestedLetSelfTailCallReturnsCorrectValues runs beside this one.
//
// Floor is 6.0 (3 per let frame), leak is 8.0.
func TestDoublyNestedLetSelfTailAllocations(t *testing.T) {
	const def = "(begin (define (deep-loop i n) " +
		"(if (>= i n) i (let ((j (+ i 1))) (let ((k j)) (deep-loop k n)))))\n)"
	assertReclaimable(t, def, "deep-loop", true)

	small := allocsForRun(t, def, "(deep-loop 0 10000)")
	big := allocsForRun(t, def, "(deep-loop 0 30000)")

	slope := allocSlope(small, big, 10000, 30000)
	t.Logf("deep-loop allocs: 10000 trips=%.0f, 30000 trips=%.0f, slope=%.3f frames/iter",
		small, big, slope)

	if slope > 7.0 {
		t.Errorf("doubly-nested self-tail call leaks its parameter frame: %.3f frames/iter "+
			"(want the 6.0 two-let floor, not the 8.0 leak)", slope)
	}
}

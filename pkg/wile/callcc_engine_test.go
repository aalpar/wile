package wile_test

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestRunawayContinuationIsBounded verifies that re-invoking a captured
// continuation in a non-converging loop surfaces a catchable
// ErrCallDepthExceeded rather than overflowing the Go stack and aborting the
// host process (C2). The continuation re-invocation nests a sub-context Run()
// frame per iteration, bypassing SaveContinuation's eval-stack depth gate, so
// applyCapturedContinuation enforces the dedicated maxContinuationDepth bound on
// nesting.
func TestRunawayContinuationIsBounded(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	// Explicit small limit: deterministic and fast (trips at depth limit+1 rather
	// than nesting to the default DefaultMaxContinuationDepth). The bound is
	// WithMaxContinuationDepth, not WithMaxCallDepth — continuation re-invocation
	// has its own, much larger budget than ordinary call depth.
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMaxContinuationDepth(200))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Classic non-tail call/cc loop that never converges: each (k 0) re-enters
	// at the pending (+ 1 _) and immediately re-invokes k.
	src := `(let ((n 0) (k #f))
	          (+ 1 (call/cc (lambda (c) (set! k c) 0)))
	          (set! n (+ n 1))
	          (if (< n 100000000) (k 0) n))`
	_, err = eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNotNil)
	if !errors.Is(err, werr.ErrCallDepthExceeded) {
		t.Fatalf("want ErrCallDepthExceeded, got %v", err)
	}
}

// TestContinuationLoopBoundedConverges is the no-false-positive companion to
// TestRunawayContinuationIsBounded: the same call/cc loop with a bound well
// under maxContinuationDepth must complete and return its value, confirming the
// depth guard does not reject legitimate (if unusual) bounded continuation loops.
func TestContinuationLoopBoundedConverges(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	// Explicit limit, well above this loop's ~100 re-invocations, so the test
	// does not implicitly depend on DefaultMaxContinuationDepth.
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMaxContinuationDepth(500))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	src := `(let ((n 0) (k #f))
	          (call/cc (lambda (c) (set! k c)))
	          (set! n (+ n 1))
	          (if (< n 100) (k #f) n))`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "100")
}

// TestContinuationDepthBoundIsSeparateFromCallDepth pins that the continuation
// re-invocation bound is maxContinuationDepth, NOT maxCallDepth — the two are
// decoupled. A non-converging loop trips the dedicated continuation bound even
// when maxCallDepth is set unlimited, and is NOT bounded by a small maxCallDepth
// alone. (ctak-style deep continuation programs legitimately exceed maxCallDepth's
// budget; sharing it rejected them — see TestDeepConvergingContinuationConverges.)
func TestContinuationDepthBoundIsSeparateFromCallDepth(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	const contLimit = 500
	// maxCallDepth unlimited proves the bound is the continuation one, not call depth.
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink),
		wile.WithMaxCallDepth(0), wile.WithMaxContinuationDepth(contLimit))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// ~10 re-invocations: well under the limit, must converge.
	under := `(let ((n 0) (k #f))
	            (call/cc (lambda (c) (set! k c)))
	            (set! n (+ n 1))
	            (if (< n 10) (k #f) n))`
	result, err := eng.EvalMultiple(ctx, under)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "10")

	// Non-converging: must trip the continuation bound, not overflow the Go stack.
	over := `(let ((n 0) (k #f))
	           (+ 1 (call/cc (lambda (c) (set! k c) 0)))
	           (set! n (+ n 1))
	           (if (< n 100000) (k 0) n))`
	_, err = eng.EvalMultiple(ctx, over)
	if !errors.Is(err, werr.ErrCallDepthExceeded) {
		t.Fatalf("want ErrCallDepthExceeded at maxContinuationDepth=%d, got %v", contLimit, err)
	}
}

// TestDeepConvergingContinuationConverges is the regression guard for the bug
// this bound caused on the Gabriel ctak benchmark. ctak(18,12,6) is a legitimate,
// converging continuation program whose non-tail call/cc structure nests deeply.
// Two distinct defects had to be fixed:
//
//  1. The bound shared maxCallDepth (10000), far too small — a single ctak(18,12,6)
//     peaks ~40k live re-invocation frames. Fixed by the dedicated, larger
//     DefaultMaxContinuationDepth.
//  2. The bound counted CUMULATIVE re-invocations, not LIVE Go-stack nesting, so a
//     program making forward progress through continuation returns accumulated the
//     counter without bound — a single ctak passed but a SEQUENCE of them (as in the
//     benchmark's warmup + 10-iteration loop) tripped the bound mid-run even though
//     the live Go stack receded between resumes. Fixed by tracking live nesting
//     (threadPools.contNestDepth, decremented on unwind).
//
// This test exercises BOTH: it runs the benchmark's exact warmup + 10-iteration
// shape, which only completes if (1) the per-call peak fits the default AND (2) the
// counter recedes between iterations. ctak(18,12,6) = 7 (the Takeuchi result).
// A single ctak(18,12,6) alone would pass even under the cumulative bug, so the
// loop is load-bearing.
func TestDeepConvergingContinuationConverges(t *testing.T) {
	// ctak(18,12,6) nests ~40k LIVE continuation re-invocation frames on the Go
	// stack (one sub.Run() per applyCapturedContinuation). That fits Go's 1 GB
	// goroutine-stack limit without the race detector, but -race inflates per-frame
	// cost several-fold, so the same nesting overflows the stack (fatal, not the
	// catchable maxContinuationDepth bound, which sits far above the ~40k peak). ctak
	// is single-threaded, so -race adds no race-detection value here; the depth-bound
	// and convergence semantics are fully exercised by the non-race run. The
	// architectural fix is to trampoline continuation invocation so a resume does not
	// nest a Go frame (TODO.md "Continuation re-invocation nests Go frames").
	if raceEnabled {
		t.Skip("ctak nests ~40k Go frames; -race frame inflation overflows the 1 GB goroutine stack — see TODO trampoline fix")
	}
	c := qt.New(t)
	ctx := context.Background()
	// Default engine — no depth options — exercises DefaultMaxContinuationDepth.
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	const prog = `(begin
(define (ctak x y z) (call-with-current-continuation (lambda (k) (ctak-aux k x y z))))
(define (ctak-aux k x y z)
  (if (not (< y x)) (k z)
      (call-with-current-continuation (lambda (k)
        (ctak-aux k
          (call-with-current-continuation (lambda (k) (ctak-aux k (- x 1) y z)))
          (call-with-current-continuation (lambda (k) (ctak-aux k (- y 1) z x)))
          (call-with-current-continuation (lambda (k) (ctak-aux k (- z 1) x y))))))))
(define last 0)
(ctak 18 12 6)                                              ; warmup
(let loop ((i 0))                                           ; 10 iterations, like the benchmark
  (when (< i 10) (set! last (ctak 18 12 6)) (loop (+ i 1))))
last)`
	result, err := eng.EvalMultiple(ctx, prog)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "7")
}

func TestCallCC_Procedure(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()
	result, err := eng.EvalMultiple(ctx, `(call-with-current-continuation procedure?)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

// TestCallCC_FusedCallForeignCached is a regression test for the
// callForeignCached double-restore bug. When call/cc is fused to a non-tail
// CallForeignCached and PrimCallCC inline mode calls ApplyCallable with a
// ForeignClosure, applyForeign consumes the SaveContinuation frame. Without
// the savedCont guard, callForeignCached would double-restore from mc.cont.
func TestCallCC_FusedCallForeignCached(t *testing.T) {
	ctx := context.Background()

	tests := []struct {
		name string
		code string
		want string
	}{
		{
			// Core reproducer: call/cc with ForeignClosure argument in non-tail position.
			// PrimCallCC inline mode calls applyForeign(procedure?, capturedK).
			// applyForeign consumes the SaveCont frame → callForeignCached must not double-restore.
			name: "call/cc procedure? non-tail",
			code: `(call-with-current-continuation procedure?)`,
			want: "#t",
		},
		{
			// call/cc with ForeignClosure in non-tail position nested inside another expression.
			name: "list wrapping call/cc procedure?",
			code: `(list (call-with-current-continuation procedure?))`,
			want: "(#t)",
		},
		{
			// call/cc with lambda that returns a value (MachineClosure path — template check).
			name: "call/cc lambda return value non-tail",
			code: `(+ 1 (call-with-current-continuation (lambda (k) (k 2))))`,
			want: "3",
		},
		{
			// call/cc escape through fused call chain.
			name: "call/cc escape and reinvoke",
			code: `(let ((r (call-with-current-continuation (lambda (k) k))))
                     (if (procedure? r) (r 42) r))`,
			want: "42",
		},
		{
			// Nested fused calls with call/cc as argument producer.
			name: "string-length with call/cc escape",
			code: `(string-length (call-with-current-continuation (lambda (k) (k "hello"))))`,
			want: "5",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			defer eng.Close()

			result, err := eng.EvalMultiple(ctx, tt.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tt.want)
		})
	}
}

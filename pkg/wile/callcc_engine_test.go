package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// TestDeepNonTailCallccLoopCompletes verifies the resume trampoline's payoff: a
// deep NON-TAIL call/cc loop — each (k 0) re-enters the pending (+ 1 _) and
// re-invokes k — completes in O(1) Go frames per resume instead of nesting a
// sub-context Run() frame per iteration. Before the flip this nesting overflowed the
// Go stack (the host process aborted), which the now-retired maxContinuationDepth
// band-aid bounded; the flip removes the nesting entirely, so the loop simply runs to
// convergence. (A genuinely NON-converging continuation loop is now an ordinary
// infinite loop bounded by context cancellation, like (let loop () (loop)).)
func TestDeepNonTailCallccLoopCompletes(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	src := `(let ((n 0) (k #f))
	          (+ 1 (call/cc (lambda (c) (set! k c) 0)))
	          (set! n (+ n 1))
	          (if (< n 50000) (k 0) n))`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "50000")
}

// TestContinuationLoopBoundedConverges confirms a tail-position call/cc loop that
// converges after ~100 re-invocations completes and returns its value — the resume
// trampoline replays it in O(1) Go frames.
func TestContinuationLoopBoundedConverges(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
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

// TestDeepConvergingContinuationConverges is the KC-5 acceptance test for the resume
// trampoline (the flip). ctak(18,12,6) is a legitimate, converging continuation
// program whose non-tail call/cc structure peaks ~40k LIVE re-invocations. Before the
// flip each applyCapturedContinuation nested a sub-context Run() frame, so the peak
// sat ~40k Go frames deep — fine without the race detector, but -race inflates
// per-frame cost several-fold and the same nesting overflowed the 1 GB goroutine stack
// (a fatal, uncatchable crash), so this test was -race-skipped and a contNestDepth
// band-aid bounded it. The flip resumes a continuation on the driver loop in O(1) Go
// frames, so ctak no longer nests — this now runs UNDER -race (the skip is retired,
// confirming the trampoline) and the band-aid is gone. ctak(18,12,6) = 7 (Takeuchi).
// The warmup + 10-iteration loop is load-bearing: it only completes if each resume
// truly recedes rather than accumulating Go frames.
func TestDeepConvergingContinuationConverges(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	// Default engine — the trampoline resumes each call/cc in O(1) Go frames.
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

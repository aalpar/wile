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
// applyCapturedContinuation enforces the same maxCallDepth bound on nesting.
func TestRunawayContinuationIsBounded(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
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
// under maxCallDepth must complete and return its value, confirming the depth
// guard does not reject legitimate (if unusual) bounded continuation loops.
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

// TestContinuationDepthBoundTracksMaxCallDepth pins that the continuation
// re-invocation bound IS maxCallDepth — not a hardcoded constant, and not a
// per-NewSubContext count (which would trip a 10-iteration loop far below the
// limit). With an explicit small limit, a loop well under it converges and a
// runaway trips at exactly limit+1.
func TestContinuationDepthBoundTracksMaxCallDepth(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	const limit = 500
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMaxCallDepth(limit))
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

	// Non-converging: must trip the bound, not overflow the Go stack.
	over := `(let ((n 0) (k #f))
	           (+ 1 (call/cc (lambda (c) (set! k c) 0)))
	           (set! n (+ n 1))
	           (if (< n 100000) (k 0) n))`
	_, err = eng.EvalMultiple(ctx, over)
	if !errors.Is(err, werr.ErrCallDepthExceeded) {
		t.Fatalf("want ErrCallDepthExceeded at maxCallDepth=%d, got %v", limit, err)
	}
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

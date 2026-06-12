package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

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

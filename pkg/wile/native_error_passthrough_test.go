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

package wile_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// The gate for wave 4 item 4: a primitive may RETURN a *values.NativeError and
// it reaches Scheme intact.
//
// CODING_STYLE.md has prescribed that pattern since 2026-06-27, and it did not
// work on the day it was written: ConditionFromError rebuilt a fresh condition
// from err.Error(), so the message survived and the irritants and the kind did
// not. The one in-tree site that tried it backed out to RaiseInPlace the same
// day (prim_exceptions.go), which is why nothing failed for fourteen months.
//
// The negative arm is the load-bearing one. The fix is a DIRECT type assertion,
// never errors.As: errors.As also matches a *NativeError buried under a werr
// wrap, and forwarding that would discard the wrap's own message — one
// losslessness bug traded for another.

// nativeErrorEngine builds a KitchenSink engine carrying primitives that return
// conditions by value, one per constructor under test, plus the wrapped control.
func nativeErrorEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = eng.Close()
	})

	prims := []wile.PrimitiveSpec{
		{
			Name:       "return-file-error",
			ParamCount: 0,
			Impl: func(_ wile.CallContext) error {
				return values.NewFileError("boom", values.NewInteger(5))
			},
		},
		{
			Name:       "return-read-error",
			ParamCount: 0,
			Impl: func(_ wile.CallContext) error {
				return values.NewReadError("boom", values.NewInteger(5))
			},
		},
		{
			Name:       "return-error-object",
			ParamCount: 0,
			Impl: func(_ wile.CallContext) error {
				return values.NewErrorObjectWithCause("boom", werr.ErrNotANumber, values.NewInteger(5))
			},
		},
		{
			// The control for the errors.As trap. A werr wrap around a condition
			// is a Go error that has ADDED context; forwarding the inner value
			// would throw that context away, so this one must be rebuilt.
			Name:       "return-wrapped-file-error",
			ParamCount: 0,
			Impl: func(_ wile.CallContext) error {
				inner := values.NewFileError("inner", values.NewInteger(5))
				return werr.WrapForeignErrorf(inner, "outer context")
			},
		},
	}
	for _, spec := range prims {
		err = eng.RegisterPrimitive(spec)
		if err != nil {
			t.Fatalf("RegisterPrimitive(%s): %v", spec.Name, err)
		}
	}
	return eng
}

// TestReturnedNativeErrorKeepsIrritantsAndKind is item 4's positive gate. Each
// row returns a condition built by one of the three constructors CODING_STYLE.md
// names, and reads back the three facts a rebuild used to destroy.
func TestReturnedNativeErrorKeepsIrritantsAndKind(t *testing.T) {
	ctx := context.Background()
	eng := nativeErrorEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "file error keeps its irritants",
			code: `(guard (e (#t (error-object-irritants e))) (return-file-error))`,
			want: values.List(values.NewInteger(5)),
		},
		{
			name: "file error keeps its kind",
			code: `(guard (e (#t (file-error? e))) (return-file-error))`,
			want: values.TrueValue,
		},
		{
			name: "read error keeps its kind",
			code: `(guard (e (#t (read-error? e))) (return-read-error))`,
			want: values.TrueValue,
		},
		{
			name: "read error is not a file error",
			code: `(guard (e (#t (file-error? e))) (return-read-error))`,
			want: values.FalseValue,
		},
		{
			// The fact that never broke, kept as the baseline the other rows are
			// read against: a message-only regression would otherwise look like a
			// pass-through failure.
			name: "message survives",
			code: `(guard (e (#t (error-object-message e))) (return-file-error))`,
			want: values.NewString("boom"),
		},
		{
			name: "generic error keeps its irritants",
			code: `(guard (e (#t (error-object-irritants e))) (return-error-object))`,
			want: values.List(values.NewInteger(5)),
		},
		{
			name: "generic error stays generic",
			code: `(guard (e (#t (list (file-error? e) (read-error? e)))) (return-error-object))`,
			want: values.List(values.FalseValue, values.FalseValue),
		},
		{
			// Scheme-side (error "boom" 5) is the baseline the returned form must
			// now match. Same three facts, produced the way that always worked.
			name: "scheme-side error is the baseline (control)",
			code: `(guard (e (#t (error-object-irritants e))) (error "boom" 5))`,
			want: values.List(values.NewInteger(5)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v, err := eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(v.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestWrappedNativeErrorIsNotForwarded is the negative arm: a condition reached
// only through errors.Unwrap is NOT the returned value, and rebuilding it is
// correct.
//
// If this file's fix is ever "simplified" to errors.As, every row of the
// positive gate above still passes and only this one fails. That asymmetry is
// the reason it exists.
func TestWrappedNativeErrorIsNotForwarded(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := nativeErrorEngine(t)

	t.Run("the wrap's message survives", func(t *testing.T) {
		v, err := eng.EvalMultiple(ctx,
			`(guard (e (#t (error-object-message e))) (return-wrapped-file-error))`)
		c.Assert(err, qt.IsNil)
		msg, ok := v.Internal().(*values.String)
		c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
		c.Assert(msg.Value, qt.Contains, "outer context",
			qt.Commentf("the werr wrap's own message was discarded: the assertion is errors.As, not a direct type assertion"))
	})

	t.Run("the inner condition's irritants do not leak through the wrap", func(t *testing.T) {
		v, err := eng.EvalMultiple(ctx,
			`(guard (e (#t (error-object-irritants e))) (return-wrapped-file-error))`)
		c.Assert(err, qt.IsNil)
		c.Assert(v.Internal(), valuestest.SchemeEquals, values.EmptyList)
	})

	t.Run("the wrapped chain still reports its file kind (control)", func(t *testing.T) {
		// Rebuilding is not lossy in every direction: ConditionFromError reads
		// the KIND off the chain with errors.As, and that arm is untouched. This
		// row is here so the negative arm above is not misread as "a wrapped
		// condition loses everything".
		v, err := eng.EvalMultiple(ctx,
			`(guard (e (#t (file-error? e))) (return-wrapped-file-error))`)
		c.Assert(err, qt.IsNil)
		c.Assert(v.Internal(), valuestest.SchemeEquals, values.FalseValue)
	})
}

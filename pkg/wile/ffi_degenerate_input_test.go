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
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// Degenerate Scheme input must reach a RegisterFunc parameter converter as a
// catchable condition, never as a panic.
//
// This is the standing form of the audit that followed wave 4 item 5+7 half C.
// The sweep that phase depended on covered pkg/registry and extensions/* by
// hand — the RegisterPrimitive population, whose panics already bypassed guard
// — and reached THIS population, the RegisterFunc converter chain, only through
// a tree-wide ruleguard pass that is structurally blind to the shape that was
// actually here. One live defect was in it: makeStructArgConverter asserted
// values.Tuple on an alist element and called Car() on it, and '() satisfies
// values.Tuple.
//
// Half C is what makes that class matter. Before it, the FFI wrapper's blanket
// recover turned such a panic into an ordinary catchable condition by accident;
// after it, a panic escapes guard and reaches the embedder as a host fault. So
// a converter arm that omits a guard now mislabels a caller's mistake as a bug
// in the host's own code.
//
// Registered as one engine with one function per converter arm, driven by a
// table of degenerate inputs. Every row asserts the SAME thing — guard catches
// it — because the property is uniform across the arms; what varies is the
// route that reaches the converter.

type degenerateInner struct {
	Alpha int64
}

type degenerateOuter struct {
	In    degenerateInner
	Items []int64
	Tags  map[string]int64
}

type degenerateWithFunc struct {
	Cb func(int64) int64
}

func degenerateEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = eng.Close()
	})

	err = eng.RegisterFuncs(map[string]any{
		"d-scalar": func(n int64) int64 {
			return n
		},
		"d-bytes": func(b []byte) int64 {
			return int64(len(b))
		},
		"d-slice": func(xs []int64) int64 {
			return int64(len(xs))
		},
		"d-map": func(m map[string]int64) int64 {
			return int64(len(m))
		},
		"d-struct": func(c degenerateInner) int64 {
			return c.Alpha
		},
		"d-outer": func(o degenerateOuter) int64 {
			return o.In.Alpha
		},
		"d-slicestruct": func(xs []degenerateInner) int64 {
			return int64(len(xs))
		},
		"d-mapstruct": func(m map[string]degenerateInner) int64 {
			return int64(len(m))
		},
		"d-varstruct": func(xs ...degenerateInner) int64 {
			return int64(len(xs))
		},
		"d-structfn": func(w degenerateWithFunc) int64 {
			return w.Cb(2)
		},
		"d-cbstruct": func(f func(int64) degenerateInner) int64 {
			return f(1).Alpha
		},
		"d-cbslice": func(f func(int64) []int64) int64 {
			return int64(len(f(1)))
		},
		"d-cb": func(f func(int64) int64) int64 {
			return f(1)
		},
		"d-cb2": func(f func(int64, int64) int64) int64 {
			return f(1, 2)
		},
	})
	if err != nil {
		t.Fatalf("RegisterFuncs: %v", err)
	}
	return eng
}

func TestFFIDegenerateInputIsCatchable(t *testing.T) {
	ctx := context.Background()
	eng := degenerateEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		// The empty list satisfies values.Tuple, so it passes an alist-element
		// assertion and then panics on Car. These are the routes that REACH
		// makeStructArgConverter; a guard in the arm must cover every one of
		// them, and only the first was pinned when the guard was added.
		{name: "struct: empty-list alist entry", code: `(d-struct '(()))`},
		{name: "struct: nested through a struct field", code: `(d-outer '((In . (()))))`},
		{name: "struct: through a slice element", code: `(d-slicestruct '((())))`},
		{name: "struct: through a variadic element", code: `(d-varstruct '(()))`},
		{name: "struct: through a callback return", code: `(d-cbstruct (lambda (x) '(())))`},
		{name: "struct: through a struct's func field", code: `(d-structfn '((Cb . (()))))`},
		{
			name: "struct: through a hashtable value",
			code: `(let ((h (make-equal-hashtable))) (hashtable-set! h "a" '(())) (d-mapstruct h))`,
		},

		// Other converter arms, same question.
		{name: "scalar: wrong type", code: `(d-scalar "s")`},
		{name: "scalar: out of int64 range", code: `(d-scalar (* 9223372036854775807 2))`},
		{name: "bytes: given a list", code: `(d-bytes '(1 2))`},
		{name: "slice: improper list", code: `(d-slice '(1 . 2))`},
		{name: "slice: ill-typed element", code: `(d-slice '(1 "x"))`},
		{name: "slice: not a list at all", code: `(d-slice 5)`},
		{name: "slice: circular", code: `(let ((x (list 1))) (set-cdr! x x) (d-slice x))`},
		{name: "map: given an alist", code: `(d-map '((a . 1)))`},
		{name: "map: nested, given a non-hashtable", code: `(d-outer '((Tags . 5)))`},
		{name: "struct: non-symbol alist key", code: `(d-struct '(("Alpha" . 1)))`},
		{name: "struct: ill-typed field value", code: `(d-struct '((Alpha . "s")))`},
		{name: "struct: improper alist", code: `(d-struct '((Alpha . 1) . 2))`},
		{name: "struct: nested field ill-typed", code: `(d-outer '((In . 5)))`},
		{name: "struct: func field is not a procedure", code: `(d-structfn '((Cb . 5)))`},

		// Callbacks: the Scheme side can be the wrong kind, the wrong arity, or
		// return something unconvertible.
		{name: "callback: a primitive, not a lambda", code: `(d-cb car)`},
		{name: "callback: a continuation", code: `(d-cb (call-with-current-continuation (lambda (k) k)))`},
		{name: "callback: wrong arity", code: `(d-cb (lambda () 1))`},
		{name: "callback: unconvertible return", code: `(d-cb (lambda (x) "s"))`},
		{name: "callback: raises", code: `(d-cb (lambda (x) (raise 'inner)))`},
		{name: "callback: faults on a domain error", code: `(d-cb (lambda (x) (car '())))`},
		{name: "callback: returns an improper list", code: `(d-cbslice (lambda (x) '(1 . 2)))`},
		// A Parameter object is accepted as a callback, but is callable with 0 or
		// 1 argument only; a 2-arity Go callback type therefore overruns it.
		{name: "callback: parameter object overrun by arity", code: `(d-cb2 (make-parameter 1))`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Per-subtest, so one red row does not FailNow the parent and skip
			// every route after it. The whole point is that the routes are
			// independent evidence.
			c := qt.New(t)
			v, err := eng.EvalMultiple(ctx, `(guard (e (#t 'caught)) `+tc.code+`)`)
			c.Assert(err, qt.IsNil,
				qt.Commentf("degenerate input escaped guard; a converter arm is missing a guard "+
					"and half C makes that an uncatchable host fault"))
			c.Assert(v.Internal(), valuestest.SchemeEquals, values.NewSymbol("caught"))
		})
	}
}

// TestFFIWellFormedCompositesStillConvert is the control. Every guard added
// above rejects something; these rows prove none of them rejects everything,
// which a guard written one predicate too wide would.
func TestFFIWellFormedCompositesStillConvert(t *testing.T) {
	ctx := context.Background()
	eng := degenerateEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{name: "struct", code: `(d-struct '((Alpha . 7)))`, want: values.NewInteger(7)},
		{name: "nested struct", code: `(d-outer '((In . ((Alpha . 7)))))`, want: values.NewInteger(7)},
		{name: "slice of structs", code: `(d-slicestruct (list '((Alpha . 1)) '((Alpha . 2))))`, want: values.NewInteger(2)},
		{name: "variadic structs", code: `(d-varstruct '((Alpha . 1)) '((Alpha . 2)))`, want: values.NewInteger(2)},
		{name: "callback returning a struct", code: `(d-cbstruct (lambda (x) '((Alpha . 3))))`, want: values.NewInteger(3)},
		{name: "struct with a func field", code: `(d-structfn (list (cons 'Cb (lambda (n) (* n 10)))))`, want: values.NewInteger(20)},
		{name: "slice", code: `(d-slice '(1 2 3))`, want: values.NewInteger(3)},
		{name: "empty slice", code: `(d-slice '())`, want: values.NewInteger(0)},
		{name: "bytes", code: `(d-bytes (bytevector 1 2))`, want: values.NewInteger(2)},
		{name: "callback", code: `(d-cb (lambda (x) (* x 9)))`, want: values.NewInteger(9)},
		{name: "callback returning a slice", code: `(d-cbslice (lambda (x) '(1 2 3)))`, want: values.NewInteger(3)},
		{name: "case-lambda as callback", code: `(d-cb (case-lambda ((x) x)))`, want: values.NewInteger(1)},
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

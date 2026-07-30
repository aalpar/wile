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

package compilation_test

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// checkSource compiles src without running it and returns the error, using a
// fresh engine so no earlier case's top-level definitions are in scope.
func checkSource(t *testing.T, src string, opts ...wile.EngineOption) error {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, opts...)
	qt.Assert(t, err, qt.IsNil)
	return eng.CheckProgram(ctx, src, "t.scm")
}

// TestCallArity_MismatchAgainstStableCallee covers the callees Phase 2 reaches:
// ambient primitives, which WithStableBasePrimitives stamps Stable under the
// immutable top-level default. Every case sits inside a procedure that is never
// called, which is the point — a test run would never reach these.
func TestCallArity_MismatchAgainstStableCallee(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "fixed arity, too many",
			src:  `(define (k) (car 1 2 3))`,
			want: "call to car: expected 1 argument(s), got 3",
		},
		{
			name: "fixed arity, too few",
			src:  `(define (k) (cons 1))`,
			want: "call to cons: expected 2 argument(s), got 1",
		},
		{
			name: "fixed arity, none",
			src:  `(define (k) (car))`,
			want: "call to car: expected 1 argument(s), got 0",
		},
		{
			name: "variadic, below minimum",
			src:  `(define (k) (-))`,
			want: "call to -: expected at least 1 argument(s), got 0",
		},
		{
			name: "mismatch in a nested body",
			src:  `(define (k) (lambda () (let ((y 1)) (car 1 2))))`,
			want: "call to car: expected 1 argument(s), got 2",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := checkSource(t, tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue,
				qt.Commentf("err: %v", err))
			qt.Assert(t, err.Error(), qt.Contains, tc.want)
			qt.Assert(t, err.Error(), qt.Contains, "t.scm:",
				qt.Commentf("the diagnostic must carry the call site — err: %v", err))
		})
	}
}

// TestCallArity_AcceptsWellFormedCalls pins the other half: the check must be
// silent on everything correct, including both variadic directions.
func TestCallArity_AcceptsWellFormedCalls(t *testing.T) {
	tcs := []struct {
		name string
		src  string
	}{
		{name: "exact arity", src: `(define (k) (car '(1 2)))`},
		{name: "variadic above minimum", src: `(define (k) (+ 1 2 3 4))`},
		{name: "variadic at zero", src: `(define (k) (list))`},
		{name: "variadic at minimum", src: `(define (k) (- 1))`},
		{name: "nested correct call", src: `(define (k) (car (cdr '(1 2))))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, checkSource(t, tc.src), qt.IsNil)
		})
	}
}

// TestCallArity_DeclinesWhenNotStaticallyKnown enumerates the cases the check
// must stay out of. Each would be a false compile error on correct code.
func TestCallArity_DeclinesWhenNotStaticallyKnown(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		why  string
	}{
		// Both shadowing cases call with an argument count the SHADOWED global
		// would reject, so a check that resolved globals-only would report a
		// false error here. That is the discriminating shape: a local arity
		// equal to the global's proves nothing.
		{
			name: "local shadows a same-named global",
			src:  `(define (k) (let ((car (lambda (a b) a))) (car 1 2)))`,
			why:  "the operator denotes the local lambda (arity 2), not the primitive (arity 1)",
		},
		{
			name: "lambda parameter shadows a global",
			src:  `(define (k car) (car 1 2))`,
			why:  "a parameter's value is not known until the call runs",
		},
		{
			name: "operator is not a symbol",
			src:  `(define (k) ((lambda (x y) x) 1))`,
			why:  "no binding to resolve — tryInlineCall owns this shape",
		},
		{
			name: "callee reached through a parameter",
			src:  `(define (k f) (f 1 2 3))`,
			why:  "higher-order calls are the runtime check's business",
		},
		{
			name: "apply defeats static arity",
			src:  `(define (k) (apply car '(1 2 3)))`,
			why:  "the argument count is a runtime property of the list",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, checkSource(t, tc.src), qt.IsNil, qt.Commentf("%s", tc.why))
		})
	}
}

// TestCallArity_UnstableBindingIsNotChecked is the soundness gate itself. Under
// WithMutableTopLevel a name may be rebound to a different arity before the call
// runs, so a static verdict would be unsound and the check must decline — even
// though the very same source is a confirmed error under the default.
func TestCallArity_UnstableBindingIsNotChecked(t *testing.T) {
	const src = `(define (k) (car 1 2 3))`

	qt.Assert(t, checkSource(t, src), qt.IsNotNil,
		qt.Commentf("immutable default: the check fires"))
	qt.Assert(t, checkSource(t, src, wile.WithMutableTopLevel()), qt.IsNil,
		qt.Commentf("mutable top level: car is rebindable, so no static verdict"))
}

// TestCallArity_SameUnitDefineMismatch covers the case a programmer hits most
// while editing one file: a define called with the wrong argument count from
// elsewhere in that same file. Its binding exists at compile time but holds no
// closure, so the value path cannot see it and the validator's formals table
// answers instead.
func TestCallArity_SameUnitDefineMismatch(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "too few arguments",
			src:  `(define (h x y) x) (define (k) (h 1))`,
			want: "call to h: expected 2 argument(s), got 1",
		},
		{
			name: "too many arguments",
			src:  `(define (h x) x) (define (k) (h 1 2))`,
			want: "call to h: expected 1 argument(s), got 2",
		},
		{
			// The call compiles before h's define is reached, but the table is
			// built over the whole unit before any of it compiles, so the check
			// still fires.
			name: "forward reference",
			src:  `(define (k) (h 1)) (define (h x y) x)`,
			want: "call to h: expected 2 argument(s), got 1",
		},
		{
			name: "variadic below minimum",
			src:  `(define (g a b . rest) a) (define (k) (g 1))`,
			want: "call to g: expected at least 2 argument(s), got 1",
		},
		{
			name: "lambda value form",
			src:  `(define h (lambda (x y) x)) (define (k) (h 1))`,
			want: "call to h: expected 2 argument(s), got 1",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := checkSource(t, tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue,
				qt.Commentf("err: %v", err))
			qt.Assert(t, err.Error(), qt.Contains, tc.want)
		})
	}
}

// TestCallArity_SameUnitAcceptsWellFormedCalls pins the other direction. The
// variadic cases exist to catch the RequiredCount/parameterCount conversion
// being inverted: if it were, these would fail while the mismatch table above
// still passed.
func TestCallArity_SameUnitAcceptsWellFormedCalls(t *testing.T) {
	tcs := []struct {
		name string
		src  string
	}{
		{name: "exact arity", src: `(define (h x y) x) (define (k) (h 1 2))`},
		{name: "variadic at minimum", src: `(define (g a . rest) a) (define (k) (g 1))`},
		{name: "variadic above minimum", src: `(define (g a . rest) a) (define (k) (g 1 2 3))`},
		{name: "thunk", src: `(define (z) 1) (define (k) (z))`},
		{name: "self recursion", src: `(define (f n) (if (= n 0) 1 (f (- n 1))))`},
		{name: "mutual recursion", src: `(define (ev? n) (od? n)) (define (od? n) (ev? n))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, checkSource(t, tc.src), qt.IsNil)
		})
	}
}

// TestCallArity_SameUnitDeclinesWhenUnstable is the string key's safety net. The
// unit table is keyed by bare name, so anything that could make one name mean
// two things must keep it out of the table entirely.
func TestCallArity_SameUnitDeclinesWhenUnstable(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		why  string
	}{
		{
			name: "defined twice",
			src:  `(define (h x) x) (define (h a b) a) (define (k) (h 1))`,
			why:  "which arity wins is exactly what the double definition makes unknowable",
		},
		{
			name: "set! in unit",
			src:  `(define (h x y) x) (set! h (lambda (z) z)) (define (k) (h 1))`,
			why:  "h may hold the one-argument lambda by the time k runs",
		},
		{
			name: "local shadows the same-unit define",
			src:  `(define (h x y) x) (define (k) (let ((h (lambda (z) z))) (h 1)))`,
			why:  "the operator denotes the local lambda, not the top-level define",
		},
		{
			name: "parameter shadows the same-unit define",
			src:  `(define (h x y) x) (define (k h) (h 1))`,
			why:  "a parameter's value is not known until the call runs",
		},
		{
			name: "mutable top level",
			src:  `(define (h x y) x) (define (k) (h 1))`,
			why:  "h is rebindable, so no static verdict is sound",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var opts []wile.EngineOption
			if tc.name == "mutable top level" {
				opts = append(opts, wile.WithMutableTopLevel())
			}
			qt.Assert(t, checkSource(t, tc.src, opts...), qt.IsNil, qt.Commentf("%s", tc.why))
		})
	}
}

// TestCallArity_NonCallableOperatorIsNotThisCheck pins the boundary: a stable
// binding holding a non-procedure is a guaranteed runtime error too, but
// reporting it belongs to a type check, not an arity check. Silence here is
// deliberate, not an oversight.
func TestCallArity_NonCallableOperatorIsNotThisCheck(t *testing.T) {
	err := checkSource(t, `(define n 5) (define (k) (n 1 2))`)
	qt.Assert(t, err, qt.IsNil)
}

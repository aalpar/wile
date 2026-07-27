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
	"strconv"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// Regressions from the 2026-07-25 whole-codebase review. Each case below took
// the host process down (fatal error: stack overflow / uncatchable panic) or
// silently produced a wrong value before its fix. A Go `fatal error` cannot be
// recovered, so these assert termination-with-a-value rather than a raised
// condition wherever the pre-fix behaviour was a crash.

// TestHostSafetyRegressions covers the crash and hang paths reachable from
// ordinary Scheme. Each Code is expected to terminate and print Want.
func TestHostSafetyRegressions(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			// pkg/registry/core/prim_syntax.go — Tuple.Length panicked on an
			// improper tail, producing an error no guard could catch.
			name: "generate-temporaries improper list is catchable",
			code: `(guard (e (#t 'caught)) (generate-temporaries (cons 1 2)))`,
			want: "caught",
		},
		{
			// Same line: a circular list made Tuple.Length loop forever.
			name: "generate-temporaries circular list terminates",
			code: `(begin (define l (list 1 2 3))
			              (set-cdr! (cddr l) l)
			              (guard (e (#t 'caught)) (generate-temporaries l)))`,
			want: "caught",
		},
		{
			name: "generate-temporaries proper list still works",
			code: `(length (generate-temporaries '(1 2 3)))`,
			want: "3",
		},
		{
			// pkg/values/native_error.go — equal? on two distinct error
			// objects whose irritant graphs cycle recursed until the host
			// stack overflowed. Reachable from pure R7RS §6.11.
			name: "equal? on cyclic error objects terminates",
			code: `(begin (define e1 (guard (c (#t c)) (error "m" (list 1))))
			              (define e2 (guard (c (#t c)) (error "m" (list 1))))
			              (set-car! (car (error-object-irritants e1)) e1)
			              (set-car! (car (error-object-irritants e2)) e2)
			              (equal? e1 e2))`,
			want: "#t",
		},
		{
			// pkg/registry/core/prim_vectors.go — vector-copy! was the one
			// mutator that skipped the immutable-literal gate, silently
			// rewriting the constant pool.
			name: "vector-copy! rejects an immutable literal",
			code: `(guard (e (#t 'raised)) (vector-copy! '#(1 2) 0 (vector 7 7)))`,
			want: "raised",
		},
		{
			name: "vector-copy! still mutates a mutable vector",
			code: `(begin (define v (vector 1 2)) (vector-copy! v 0 (vector 7 7)) v)`,
			want: "#(7 7)",
		},
		{
			// pkg/values/big_complex.go — Sqrt's im/re quotient was Inf/Inf,
			// which big.Float.Quo panics on, in both the a>=0 arm and its
			// a<0 mirror. #m forces the BigComplex path.
			name: "BigComplex sqrt with infinite imaginary part",
			code: `(sqrt (+ (make-rectangular 1.0 +inf.0) #m0.0))`,
			want: "+inf.0+inf.0i",
		},
		{
			name: "BigComplex sqrt mirror arm, negative real",
			code: `(sqrt (+ (make-rectangular -1.0 +inf.0) #m0.0))`,
			want: "+inf.0+inf.0i",
		},
		{
			name: "BigComplex sqrt sign follows the imaginary part",
			code: `(sqrt (+ (make-rectangular 1.0 -inf.0) #m0.0))`,
			want: "+inf.0-inf.0i",
		},
		{
			// Finite control: the guard must not perturb ordinary values.
			name: "BigComplex sqrt finite operands unaffected",
			code: `(sqrt (+ (make-rectangular 3.0 4.0) #m0.0))`,
			want: "2.0+1.0i",
		},
		{
			// An infinite REAL part never reached the panicking quotient and
			// must keep its existing answer.
			name: "BigComplex sqrt infinite real part unaffected",
			code: `(sqrt (+ (make-rectangular +inf.0 1.0) #m0.0))`,
			want: "+inf.0+0.0i",
		},
		{
			// pkg/extensions/io/prim_binary.go — read-bytevector! was the
			// second mutator skipping the immutable-literal gate.
			name: "read-bytevector! rejects an immutable literal",
			code: `(guard (e (#t 'raised))
			         (read-bytevector! '#u8(1 2) (open-input-bytevector (bytevector 65 66))))`,
			want: "raised",
		},
		{
			name: "read-bytevector! still fills a mutable bytevector",
			code: `(begin (define bv (bytevector 1 2))
			              (read-bytevector! bv (open-input-bytevector (bytevector 65 66)))
			              bv)`,
			want: "#u8(65 66)",
		},
		{
			// pkg/machine/call_foreign_cached.go — the tail arm restored a
			// continuation the primitive had already consumed, dropping one
			// activation frame per call. Pre-fix this printed
			// ((#f) (#f) (x #f)): note (g 1) losing its cons.
			name: "tail call-with-immediate-continuation-mark keeps every frame",
			code: `(begin (define (g n)
			                 (if (= n 0)
			                     (call-with-immediate-continuation-mark 'k list)
			                     (cons 'x (g (- n 1)))))
			              (list (g 0) (g 1) (g 2)))`,
			want: "((#f) (x #f) (x x #f))",
		},
	}

	eng := newSRFITestEngine(t)
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := evalSRFI(t, eng, tc.code)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestLongFlatListDoesNotOverflowHostStack pins the length-vs-depth invariant on
// both entry points that walked a list's cdr chain recursively: the reader
// (syntax.UnwrapAllShared) and the quoted-literal compile path
// (validateQuotedLiteralWithVisited). List *length* must not become Go stack
// depth — DefaultMaxParseDepth bounds nesting, not length, so nothing else
// stops these.
//
// The element count must exceed the pre-fix overflow threshold or this test
// pins nothing. Measured on darwin/arm64 by reverting each fix in turn:
// the quoted-literal path first crashed between 400k and 800k, the reader path
// between 1.2M and 2M. 3M is confirmed to crash on both. Do not lower this to
// speed the test up without re-measuring — at 400k both subtests pass with the
// fixes reverted.
func TestLongFlatListDoesNotOverflowHostStack(t *testing.T) {
	const elements = 3000000

	tcs := []struct {
		name    string
		program func(list string) string
	}{
		{
			// syntax.UnwrapAllShared, via (read).
			name: "read",
			program: func(list string) string {
				return `(length (read (open-input-string "` + list + `")))`
			},
		},
		{
			// validateQuotedLiteralWithVisited, at compile time.
			name: "quoted literal",
			program: func(list string) string {
				return `(length (quote ` + list + `))`
			},
		},
	}

	list := "(" + strings.Repeat("1 ", elements) + ")"
	want := strconv.Itoa(elements)

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(context.Background(), WithProfile(KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			got := evalSRFI(t, eng, tc.program(list))
			qt.Assert(t, got, qt.Equals, want)
		})
	}
}

// TestCyclicAtomicRenders pins that an atomic reaching itself renders the
// back-edge marker instead of recursing until the host stack overflows.
// pkg/values/atomic.go rendered outside the package's cycle machinery, and a Go
// stack overflow is an unrecoverable runtime.throw, not a catchable panic.
//
// The atomic's id comes from a process-global counter, so this asserts the
// shape rather than an exact string.
func TestCyclicAtomicRenders(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{
			name: "atomic pointing at itself",
			code: `(begin (import (wile gointerop))
			              (define a (make-atomic 1))
			              (atomic-store! a a)
			              (let ((p (open-output-string)))
			                (display a p)
			                (get-output-string p)))`,
		},
		{
			// Indirect cycle: the atomic reaches itself through a vector, so
			// the marker must come from the vector's renderer descending back
			// into the already-visited atomic.
			name: "atomic reaching itself through a vector",
			code: `(begin (import (wile gointerop))
			              (define a (make-atomic 1))
			              (define v (vector 0))
			              (vector-set! v 0 a)
			              (atomic-store! a v)
			              (let ((p (open-output-string)))
			                (write v p)
			                (get-output-string p)))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// A fresh engine per case: top-level bindings are immutable by
			// default, so the cases cannot share one and reuse the same names.
			eng := newSRFITestEngine(t)
			got := evalSRFI(t, eng, tc.code)
			qt.Assert(t, strings.Contains(got, "#<atomic"), qt.IsTrue,
				qt.Commentf("want an atomic rendering, got: %s", got))
			qt.Assert(t, strings.Contains(got, "..."), qt.IsTrue,
				qt.Commentf("want the cycle back-edge marker, got: %s", got))
		})
	}
}

// TestQuotedLiteralCycleStillRejected guards the iterative spine walk in
// validateQuotedLiteralWithVisited against losing the cycle rejection it
// replaced: the visited marks must still cover the live spine.
func TestQuotedLiteralCycleStillRejected(t *testing.T) {
	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	_, evalErr := eng.EvalMultiple(context.Background(), `(quote #0=(a . #0#))`)
	qt.Assert(t, evalErr, qt.IsNotNil)
	qt.Assert(t, strings.Contains(evalErr.Error(), "circular datum label"), qt.IsTrue,
		qt.Commentf("want a circular-datum-label rejection, got: %v", evalErr))
}

// TestQuotedLiteralStructuresPreserved pins that the rebuild-only-if-changed
// path in validateQuotedLiteralWithVisited still returns ordinary quoted data
// unaltered, including improper tails and nesting.
func TestQuotedLiteralStructuresPreserved(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "proper list", code: `(quote (1 2 3))`, want: "(1 2 3)"},
		{name: "nested", code: `(quote (a (b (c))))`, want: "(a (b (c)))"},
		{name: "improper tail", code: `(quote (1 . 2))`, want: "(1 . 2)"},
		{name: "vector", code: `(quote #(1 2))`, want: "#(1 2)"},
		{name: "empty list", code: `(quote ())`, want: "()"},
	}

	eng := newSRFITestEngine(t)
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := evalSRFI(t, eng, tc.code)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

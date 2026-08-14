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

package core_test

import (
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestImmutableLiteral_MutatorsRaise verifies the vector and bytevector mutators
// reject a quoted literal (R7RS §4.1.2) with the per-type sentinel, matching the
// existing immutable-string behavior.
//
// The three PAIR mutators used to be rows here. They moved to
// TestPairLiteral_IsMutableByDecision rather than being deleted, so the change
// of answer is pinned instead of merely un-asserted.
func TestImmutableLiteral_MutatorsRaise(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want error
	}{
		{"vector-set! on literal", `(vector-set! '#(1 2 3) 0 9)`, werr.ErrImmutableVector},
		{"vector-fill! on literal", `(vector-fill! '#(1 2 3) 0)`, werr.ErrImmutableVector},
		{"bytevector-u8-set! on literal", `(bytevector-u8-set! '#u8(1 2 3) 0 9)`, werr.ErrImmutableBytevector},
		{"bytevector-copy! on literal", `(bytevector-copy! '#u8(1 2 3) 0 (bytevector 9))`, werr.ErrImmutableBytevector},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue,
				qt.Commentf("want %v, got %v", tc.want, err))
		})
	}
}

// TestImmutableLiteral_AllocatedStillMutable verifies freshly-allocated
// pairs/vectors remain mutable — the guard keys on literal identity, not type.
func TestImmutableLiteral_AllocatedStillMutable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set-car! on allocated pair",
			Code: `(let ((p (list 1 2))) (set-car! p 9) (car p))`, Expected: values.NewInteger(9)},
		{Name: "set-cdr! on allocated pair",
			Code: `(let ((p (list 1 2))) (set-cdr! p 9) (cdr p))`, Expected: values.NewInteger(9)},
		{Name: "list-set! on allocated list",
			Code: `(let ((p (list 1 2 3))) (list-set! p 1 9) (list-ref p 1))`, Expected: values.NewInteger(9)},
		{Name: "vector-set! on allocated vector",
			Code: `(let ((v (make-vector 2 0))) (vector-set! v 0 9) (vector-ref v 0))`, Expected: values.NewInteger(9)},
		{Name: "vector-fill! on allocated vector",
			Code: `(let ((v (make-vector 2 0))) (vector-fill! v 7) (vector-ref v 1))`, Expected: values.NewInteger(7)},
		{Name: "bytevector-u8-set! on allocated bytevector",
			Code: `(let ((b (make-bytevector 2 0))) (bytevector-u8-set! b 0 9) (bytevector-u8-ref b 0))`, Expected: values.NewInteger(9)},
		{Name: "bytevector-copy! on allocated bytevector",
			Code: `(let ((b (make-bytevector 2 0))) (bytevector-copy! b 0 (bytevector 7)) (bytevector-u8-ref b 0))`, Expected: values.NewInteger(7)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestPairLiteral_IsMutableByDecision is the flipped half of
// TestImmutableLiteral_MutatorsRaise. Each row raised werr.ErrImmutablePair
// until the side set was deleted; each now succeeds.
//
// This is a narrowing of a documented deviation, not a new one: R7RS §4.1.2's
// "it is an error" is a case an implementation is encouraged but not required to
// detect (§1.3.2), so dropping detection returns Wile to the conforming
// baseline. What bought it is that *Pair stays [2]Value — a flag word is ~25%
// growth on the dominant heap object, on a mutation path the peephole promotes.
func TestPairLiteral_IsMutableByDecision(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set-car! on literal",
			Code: `(let ((p '(a b))) (set-car! p 'x) (car p))`, Expected: values.NewSymbol("x")},
		{Name: "set-cdr! on literal",
			Code: `(let ((p '(a b))) (set-cdr! p 'x) (cdr p))`, Expected: values.NewSymbol("x")},
		{Name: "list-set! on literal",
			Code: `(let ((p '(a b c))) (list-set! p 1 'x) (list-ref p 1))`, Expected: values.NewSymbol("x")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestPairLiteral_StructureSharingCorrupts is the price of the test above, and
// it is asserted rather than left as "no error".
//
// Two equal? pair literals in ONE compilation unit dedup to a single object, so
// set-cdr! through one is visible through the other. That corruption is what
// pair-literal immutability bought, and dropping the flag gives it back; a test
// that only checked "the mutation succeeds" would hide it. The (begin ...)
// wrapper forces a single compilation unit (per-template equal? dedup); separate
// REPL units would give distinct objects and the row would prove nothing.
func TestPairLiteral_StructureSharingCorrupts(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `(begin
		(define x '(1 2 3))
		(define y '(1 2 3))
		(set-cdr! x 99)
		y)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(1 . 99)",
		qt.Commentf("y must show x's mutation: the two literals are one object, and "+
			"nothing refuses the write any more"))
}

// TestImmutableLiteral_EveryConstantAppenderMarks covers the three compile-time
// constant appenders, not just the quoted one. CompileSelfEvaluating and
// compileQuasiquoteDatum's constant fast path used to append without marking, so
// only the two 'quoted rows below refused.
//
// Observed at 003b3353, each form run as (let ((v <literal>)) (mutate v) v):
//
//	#(1 2 3)     => #(9 2 3)     ' #(1 2 3)   => RAISED
//	`#(1 2 3)    => #(9 2 3)     '#u8(1 2 3)  => RAISED
//	#u8(1 2 3)   => #u8(9 2 3)   `#u8(1 2 3)  => #u8(9 2 3)
//
// The two quoted rows are negative controls: they were the only appender that
// marked, and they must keep refusing.
func TestImmutableLiteral_EveryConstantAppenderMarks(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want error
	}{
		{"self-evaluating vector", `(let ((v #(1 2 3))) (vector-set! v 0 9))`, werr.ErrImmutableVector},
		{"quoted vector", `(let ((v '#(1 2 3))) (vector-set! v 0 9))`, werr.ErrImmutableVector},
		{"quasiquoted constant vector", "(let ((v `#(1 2 3))) (vector-set! v 0 9))", werr.ErrImmutableVector},
		{"self-evaluating bytevector", `(let ((v #u8(1 2 3))) (bytevector-u8-set! v 0 9))`, werr.ErrImmutableBytevector},
		{"quoted bytevector", `(let ((v '#u8(1 2 3))) (bytevector-u8-set! v 0 9))`, werr.ErrImmutableBytevector},
		{"quasiquoted constant bytevector", "(let ((v `#u8(1 2 3))) (bytevector-u8-set! v 0 9))", werr.ErrImmutableBytevector},
		{"nested vector inside a quasiquoted list", "(let ((v `(1 #(2 3)))) (vector-set! (cadr v) 0 9))", werr.ErrImmutableVector},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue,
				qt.Commentf("want %v, got %v", tc.want, err))
		})
	}
}

// TestImmutableLiteral_OrderIndependence is the strongest of the gates and the
// one nothing in the tree covered. Two structurally equal aggregates in ONE
// compilation unit dedup to a single pooled object, and the flag rides on that
// object. While only the quote path marked, the mark landed on whichever copy
// dedup DISCARDED whenever an unmarked twin was appended first — so a quoted
// literal's immutability depended on declaration order.
//
// Every "unquoted first" row below is the red one: at 003b3353 mutating the
// QUOTED name succeeded and returned #(9 2 3) / #u8(9 2 3). The "quoted first"
// rows already refused, and must keep refusing — after the fix they refuse on
// their own mark rather than by deduping onto a marked twin.
//
// The two PAIR rows went with pair-literal immutability. Order independence
// still holds for pairs — both twins are equally mutable — but it is no longer
// observable through an error, so there is nothing left here to assert.
func TestImmutableLiteral_OrderIndependence(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want error
	}{
		{"vector, unquoted first (red at 003b3353)", `(begin
			(define v1 #(1 2 3))
			(define v2 '#(1 2 3))
			(vector-set! v2 0 9))`, werr.ErrImmutableVector},
		{"vector, quoted first", `(begin
			(define v2 '#(1 2 3))
			(define v1 #(1 2 3))
			(vector-set! v2 0 9))`, werr.ErrImmutableVector},
		{"vector, quasiquoted first (red at 003b3353)", "(begin\n" +
			"(define v1 `#(1 2 3))\n" +
			"(define v2 '#(1 2 3))\n" +
			"(vector-set! v2 0 9))", werr.ErrImmutableVector},
		{"vector, quoted before quasiquoted", "(begin\n" +
			"(define v2 '#(1 2 3))\n" +
			"(define v1 `#(1 2 3))\n" +
			"(vector-set! v2 0 9))", werr.ErrImmutableVector},
		{"bytevector, unquoted first (red at 003b3353)", `(begin
			(define b1 #u8(1 2 3))
			(define b2 '#u8(1 2 3))
			(bytevector-u8-set! b2 0 9))`, werr.ErrImmutableBytevector},
		{"bytevector, quoted first", `(begin
			(define b2 '#u8(1 2 3))
			(define b1 #u8(1 2 3))
			(bytevector-u8-set! b2 0 9))`, werr.ErrImmutableBytevector},
		{"nested vector, quasiquoted first (red at 003b3353)", "(begin\n" +
			"(define a `(1 #(2 3)))\n" +
			"(define b '(1 #(2 3)))\n" +
			"(vector-set! (cadr b) 0 9))", werr.ErrImmutableVector},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue,
				qt.Commentf("want %v, got %v", tc.want, err))
		})
	}
}

// TestImmutableLiteral_PooledSharingIsIntended pins the other half of the
// order-independence answer: per-template dedup is deliberate (Wave 1 §8 Q1),
// so the two names stay eq? in BOTH orders. Freezing the survivor is the fix;
// splitting the pool is not. A future dedup key must not be gated on this
// program returning #f.
func TestImmutableLiteral_PooledSharingIsIntended(t *testing.T) {
	tcs := []string{
		"(begin (define x `(1 2 3)) (define y '(1 2 3)) (eq? x y))",
		"(begin (define y '(1 2 3)) (define x `(1 2 3)) (eq? x y))",
		"(begin (define v1 #(1 2 3)) (define v2 '#(1 2 3)) (eq? v1 v2))",
		"(begin (define v2 '#(1 2 3)) (define v1 #(1 2 3)) (eq? v1 v2))",
	}
	for _, code := range tcs {
		t.Run(code, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
		})
	}
}

// TestImmutableLiteral_RuntimeAggregatesUnaffected is the verifier's pre-check,
// kept as an assertion: marking at CompileSelfEvaluating must not be able to
// freeze a caller's runtime aggregate. (vector 1 2) is a call, never pools, and
// eval returns a non-eq? copy of any vector handed to it.
func TestImmutableLiteral_RuntimeAggregatesUnaffected(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "vector call result is mutable",
			Code: `(let ((v (vector 1 2 3))) (vector-set! v 0 9) (vector-ref v 0))`, Expected: values.NewInteger(9)},
		{Name: "vector-copy of a literal is mutable",
			Code: `(let ((v (vector-copy #(1 2 3)))) (vector-set! v 0 9) (vector-ref v 0))`, Expected: values.NewInteger(9)},
		{Name: "bytevector call result is mutable",
			Code: `(let ((b (bytevector 1 2 3))) (bytevector-u8-set! b 0 9) (bytevector-u8-ref b 0))`, Expected: values.NewInteger(9)},
		{Name: "list call result is mutable",
			Code: `(let ((p (list 1 2 3))) (set-car! p 9) (car p))`, Expected: values.NewInteger(9)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestImmutableLiteral_StringParityPreserved confirms literal strings remain
// immutable — the new pair/vector behavior is consistent with strings and did
// not regress them.
func TestImmutableLiteral_StringParityPreserved(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, `(string-set! "abc" 0 #\z)`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrImmutableString), qt.IsTrue,
		qt.Commentf("string-set! on a literal must still raise ErrImmutableString, got %v", err))
}

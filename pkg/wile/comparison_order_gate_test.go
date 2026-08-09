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

	qt "github.com/frankban/quicktest"
)

// evalString evaluates src on a fresh engine and returns the written form of the
// result. Every assertion in this file is on the SCHEME-level answer, because the
// defect being pinned is reachable three ways -- the `=` primitive, the `<`
// primitive family, and the peephole-promoted inline forms of both -- and only a
// Scheme-level witness crosses all three.
func evalString(t *testing.T, src string) string {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx, WithProfile(Small))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	v, err := engine.EvalMultiple(ctx, src)
	if err != nil {
		t.Fatalf("EvalMultiple(%s): %v", src, err)
	}
	return v.SchemeString()
}

// TestComparisonOrder_Witnesses is the must-fail-first gate for
// reviews/2026-08-07 item 2.3.12a/b/c: `=` and `<` were not a consistent order.
//
// Every row here was RED at 003b3353, and the observed wrong answer is recorded
// in the row's comment so a future reader can tell a real regression from a
// re-specification. R7RS §6.2.6 anticipates the mistake by name: "The
// implementation approach of converting all arguments to inexact numbers if any
// argument is inexact is not transitive."
func TestComparisonOrder_Witnesses(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name string
		src  string
		want string
	}{
		{
			// 003b3353: (#f #f #f #t #t) -- all three of <, >, = false at once
			// while <= and >= are both true, which contradicts =.
			name: "12c/exact operand needing 301 bits vs float",
			src:  `(let ((f (expt 2.0 300)) (a (+ (expt 2 300) 1))) (list (< f a) (> f a) (= f a) (<= f a) (>= f a)))`,
			want: "(#t #f #f #t #f)",
		},
		{
			// 003b3353: (#f #f) -- trichotomy failed outright.
			name: "12c/trichotomy in both directions",
			src:  `(let ((f (expt 2.0 300)) (a (+ (expt 2 300) 1))) (list (< f a) (< a f)))`,
			want: "(#t #f)",
		},
		{
			// 003b3353: (#t #f #t) -- = and > both true. Rational had no
			// hand-written comparison arm, so = fell through to
			// Subtract(...).IsZero(), which routes through the CONTAGION table.
			name: "12a/rational vs float",
			src:  `(list (= 1/3 0.3333333333333333) (< 1/3 0.3333333333333333) (> 1/3 0.3333333333333333))`,
			want: "(#f #f #t)",
		},
		{
			// 003b3353: (#f #t) -- (= x x) was #f for an infinity in a non-*Float
			// representation, contradicting R7RS §6.1's "numerically equal (in the
			// sense of =)". A *Float infinity did NOT reproduce it (next row).
			name: "12b/BigFloat infinity is equal to itself",
			src:  `(let ((x (/ #m1.0 #m0.0))) (list (= x x) (eqv? x x)))`,
			want: "(#t #t)",
		},
		{
			// 003b3353: (#t #t) -- green before the change, recorded so nobody
			// reads the row above as covering the float64 case too.
			name: "12b/float infinity was already equal to itself",
			src:  `(let ((x (/ 1.0 0.0))) (list (= x x) (eqv? x x)))`,
			want: "(#t #t)",
		},
		{
			// 003b3353: (#f #t) -- the second answer was right for the wrong
			// reason. DefaultBigFloatPrecision is 256 and the operand needs 301
			// significant bits, so the "lossless lattice" rounded it on the way in
			// and the two happened to land on the same rounded value.
			name: "12c/equality reached without rounding",
			src:  `(list (= (expt 2.0 300) (+ (expt 2 300) 1)) (= (expt 2.0 300) (expt 2 300)))`,
			want: "(#f #t)",
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(evalString(t, tt.src), qt.Equals, tt.want)
		})
	}
}

// TestComparisonOrder_MixedExactnessPreservationPin is NOT a must-fail-first
// gate. Both answers are already correct at 003b3353 -- measured -- and the
// kernel rewrite must PRESERVE them, not produce them. It is written down here
// because Wave 5 §11 Q4 accepted the Big* write/read asymmetry on exactly this
// property, so it is load-bearing for a documented deviation and must not be
// left true by accident.
//
// (= 3.0 #m3.0) is #t because they are the same number. (eqv? 3.0 #m3.0) is #f
// because R7RS §6.1 requires two inexact numbers that are eqv? to agree under
// any finite composition of arithmetic, and a float64 and a 256-bit value do
// not. eqv? is deliberately NOT derived from the comparison kernel.
func TestComparisonOrder_MixedExactnessPreservationPin(t *testing.T) {
	c := qt.New(t)
	c.Assert(evalString(t, `(list (= 3.0 #m3.0) (eqv? 3.0 #m3.0))`), qt.Equals, "(#t #f)")
}

// comparisonWitnesses spans one value per numeric kind reachable by the ordering
// predicates, plus the four review witnesses. Complex and BigComplex are absent
// on purpose: `<` and its family raise on a non-real operand, so they have no
// place in an ordering property. NaN is absent for the same structural reason --
// it is unordered against everything, so NONE of <, =, > holds and trichotomy
// does not apply to it. It gets its own pin below.
//
// No randomness and no seed, so a failure is a fixed pair or triple that names
// itself.
const comparisonWitnesses = `
(define witnesses
  (list (cons 'int-0            0)
        (cons 'int-1            1)
        (cons 'int-3            3)
        (cons 'int-neg          -7)
        (cons 'bigint-2^300     (expt 2 300))
        (cons 'bigint-2^300+1   (+ (expt 2 300) 1))
        (cons 'bigint-neg       (- (expt 2 300)))
        (cons 'rat-1/3          1/3)
        (cons 'rat-neg-7/2      -7/2)
        (cons 'float-2^300      (expt 2.0 300))
        (cons 'float-third      0.3333333333333333)
        (cons 'float-1.5        1.5)
        (cons 'float-neg-zero   -0.0)
        (cons 'bigfloat-3       #m3.0)
        (cons 'bigfloat-half    #m0.5)
        (cons 'float+inf        (/ 1.0 0.0))
        (cons 'float-inf        (- (/ 1.0 0.0)))
        (cons 'bigfloat+inf     (/ #m1.0 #m0.0))
        (cons 'bigfloat-inf     (- (/ #m1.0 #m0.0)))))
`

// TestComparisonOrder_IsATotalOrder is the property half of the gate: over a
// fixed witness table, exactly one of <, =, > holds for every pair, <= and >=
// agree with it, and < is transitive over every triple.
//
// Transitivity is the property R7RS §6.2.6 calls out, and it is the one a
// promote-the-exact-operand-to-float implementation cannot have: rounding is not
// injective, so two distinct exact values collapse onto one float and the strict
// order between them evaporates.
func TestComparisonOrder_IsATotalOrder(t *testing.T) {
	c := qt.New(t)

	trichotomy := comparisonWitnesses + `
(define (holds-count a b)
  (+ (if (< a b) 1 0) (if (= a b) 1 0) (if (> a b) 1 0)))
(let outer ((as witnesses) (acc '()))
  (if (null? as)
      (reverse acc)
      (let inner ((bs witnesses) (acc acc))
        (if (null? bs)
            (outer (cdr as) acc)
            (let* ((na (caar as))
                   (nb (caar bs))
                   (a (cdar as))
                   (b (cdar bs))
                   (lt (< a b))
                   (same (= a b))
                   (gt (> a b))
                   (le (<= a b))
                   (ge (>= a b)))
              (inner (cdr bs)
                     (cond ((not (= (holds-count a b) 1))
                            (cons (list 'trichotomy na nb lt same gt) acc))
                           ((not (eq? le (or lt same)))
                            (cons (list 'le-disagrees na nb le lt same) acc))
                           ((not (eq? ge (or gt same)))
                            (cons (list 'ge-disagrees na nb ge gt same) acc))
                           (else acc))))))))
`
	c.Assert(evalString(t, trichotomy), qt.Equals, "()")

	transitivity := comparisonWitnesses + `
(let a-loop ((as witnesses) (acc '()))
  (if (null? as)
      (reverse acc)
      (let b-loop ((bs witnesses) (acc acc))
        (if (null? bs)
            (a-loop (cdr as) acc)
            (let c-loop ((cs witnesses) (acc acc))
              (if (null? cs)
                  (b-loop (cdr bs) acc)
                  (let ((a (cdar as)) (b (cdar bs)) (d (cdar cs)))
                    (c-loop (cdr cs)
                            (if (and (< a b) (< b d) (not (< a d)))
                                (cons (list 'not-transitive (caar as) (caar bs) (caar cs)) acc)
                                acc)))))))))
`
	c.Assert(evalString(t, transitivity), qt.Equals, "()")
}

// TestComparisonOrder_NaNIsUnordered pins the fourth verdict. NaN is not a
// trichotomy case -- all five predicates are #f -- while (eqv? +nan.0 +nan.0)
// stays #t, because eqv? is an equivalence relation and is not derived from the
// comparison kernel.
func TestComparisonOrder_NaNIsUnordered(t *testing.T) {
	c := qt.New(t)

	c.Assert(
		evalString(t, `(let ((n (/ 0.0 0.0)))
                          (list (< n n) (= n n) (> n n) (<= n n) (>= n n) (eqv? n n)))`),
		qt.Equals, "(#f #f #f #f #f #t)",
	)
	c.Assert(
		evalString(t, `(let ((n (/ 0.0 0.0)))
                          (list (< n 1) (= n 1) (> n 1) (<= n 1) (>= n 1)))`),
		qt.Equals, "(#f #f #f #f #f)",
	)
	c.Assert(
		evalString(t, `(let ((n (/ 0.0 0.0)))
                          (list (< 1 n) (= 1 n) (> 1 n) (<= 1 n) (>= 1 n)))`),
		qt.Equals, "(#f #f #f #f #f)",
	)
}

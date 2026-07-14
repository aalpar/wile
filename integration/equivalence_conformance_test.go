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

package integration_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// Equivalence-predicate conformance, pinned against R7RS-small §6.1 and against
// Petite Chez Scheme 10.4.1 as a second implementation.
//
// The `want` column is what Wile MUST answer. Where Wile is currently WRONG, the
// case carries a `bug` note and `want` still states the correct answer — the test
// is skipped with that note until the fix lands, so the pin is a specification,
// not a ratification of the defect. See
// plans/2026-07-14-equivalence-predicate-divergence.md.
//
// R7RS §6.1 rules this table encodes:
//
//   - eq? ⊆ eqv? ⊆ equal? — equal? may never be FINER than eqv?.
//   - equal? "returns the same as eqv? when applied to booleans, symbols, numbers,
//     characters, ports, procedures, and the empty list." No latitude for numbers.
//   - equal? recurses into pairs, vectors, strings, bytevectors, and must terminate
//     on circular structure.
//   - "In all other cases, equal? may return either #t or #f" — this is what makes
//     Wile's structural equal? on records/hashtables/boxes LEGAL, though it diverges
//     from Chez. Those rows are pinned as deliberate policy, not accident.
//   - eqv? on two NaNs is explicitly unspecified. Wile's #f conforms; Chez says #t.
type equivCase struct {
	name string
	expr string
	want string
	// bug, when non-empty, names the finding in the plan. The case is skipped and
	// `want` records the answer the fix must produce.
	bug string
	// chez, when non-empty, records Petite Chez 10.4.1's answer where it differs
	// from Wile's *correct* answer — i.e. a legal divergence, not a defect.
	chez string
}

var equivCases = []equivCase{
	// ---- Structural equal?: the R7RS-mandated recursion. ----
	{name: "equal?/list", expr: `(equal? '(1 2) '(1 2))`, want: "#t"},
	{name: "equal?/nested-list", expr: `(equal? '(1 (2 (3))) '(1 (2 (3))))`, want: "#t"},
	{name: "equal?/list-differs", expr: `(equal? '(1 2) '(1 3))`, want: "#f"},
	{name: "equal?/string", expr: `(equal? "abc" "abc")`, want: "#t"},
	{name: "equal?/vector", expr: `(equal? (vector 1 2) (vector 1 2))`, want: "#t"},
	{name: "equal?/bytevector", expr: `(equal? (bytevector 1 2) (bytevector 1 2))`, want: "#t"},
	{name: "equal?/empty-list", expr: `(equal? '() '())`, want: "#t"},
	{name: "equal?/symbol", expr: `(equal? 'a 'a)`, want: "#t"},
	{name: "equal?/char", expr: `(equal? #\a #\a)`, want: "#t"},

	// §6.1: "Even if its arguments are circular data structures, equal? must
	// always terminate." Equal's visited set is the coinductive hypothesis.
	{
		name: "equal?/circular-terminates",
		expr: `(let ((a (list 1 2)) (b (list 1 2)))
		         (set-cdr! (cdr a) a)
		         (set-cdr! (cdr b) b)
		         (equal? a b))`,
		want: "#t",
	},

	// ---- eq? — finest; identity, with symbols by name. ----
	{name: "eq?/symbol", expr: `(eq? 'a 'a)`, want: "#t"},
	{name: "eq?/distinct-pairs", expr: `(eq? (list 1) (list 1))`, want: "#f"},
	{name: "eq?/same-object", expr: `(let ((x (list 1))) (eq? x x))`, want: "#t"},

	// ---- Numbers: equal? MUST agree with eqv?. No latitude (§6.1). ----
	{name: "eqv?/exactness-differs", expr: `(eqv? 1 1.0)`, want: "#f"},
	{name: "equal?/exactness-differs", expr: `(equal? 1 1.0)`, want: "#f"},
	{name: "eqv?/same-float", expr: `(eqv? 1.0 1.0)`, want: "#t"},
	{name: "equal?/same-float", expr: `(equal? 1.0 1.0)`, want: "#t"},
	{name: "eqv?/exact-int-bignum", expr: `(eqv? 1 (- (expt 2 100) (- (expt 2 100) 1)))`, want: "#t"},
	{name: "eqv?/rational-vs-float", expr: `(eqv? 1/2 0.5)`, want: "#f"},
	{name: "equal?/rational-vs-float", expr: `(equal? 1/2 0.5)`, want: "#f"},

	// F2 — equal? and eqv? DISAGREE on numbers. Forbidden outright.
	// eqv?'s #f is the correct answer: a float64 and an arbitrary-precision
	// BigFloat are not substitutable under a finite composition of standard
	// arithmetic ((+ x 1e-20) separates them). So the defect is in equal?'s
	// cross-kind arm (values/float.go), not in Eqv.
	{
		name: "eqv?/float-vs-bigfloat", expr: `(eqv? 1.0 #m1.0)`, want: "#f",
	},
	{
		name: "equal?/float-vs-bigfloat", expr: `(equal? 1.0 #m1.0)`, want: "#f",
		bug: "F2: equal? returns #t; Float.EqualTo carries a cross-kind *BigFloat arm that Eqv does not",
	},

	// F1 — signed zero. §6.1's eqv? #f clause fires whenever the implementation
	// distinguishes negative zero, and Wile does: (/ 1.0 -0.0) => -inf.0 while
	// (/ 1.0 0.0) => +inf.0. That is a finite composition of standard arithmetic
	// yielding different, non-NaN results. Wile's own output is the witness.
	{name: "F1/witness-positive", expr: `(/ 1.0 0.0)`, want: "+inf.0"},
	{name: "F1/witness-negative", expr: `(/ 1.0 -0.0)`, want: "-inf.0"},
	{
		name: "eqv?/signed-zero", expr: `(eqv? 0.0 -0.0)`, want: "#f",
		bug: "F1: helpers.Eqv compares *Float with Go ==, and IEEE-754 says 0.0 == -0.0",
	},
	{
		name: "equal?/signed-zero", expr: `(equal? 0.0 -0.0)`, want: "#f",
		bug: "F1: inherited from Eqv via Float.EqualTo's ==",
	},
	{
		name: "memv/signed-zero", expr: `(memv -0.0 (list 0.0))`, want: "#f",
		bug: "F1 fallout: memv is eqv?-based, so it finds a 0.0 when handed a -0.0",
	},

	// F3 — NaN. §6.1: "the behavior of eqv? is unspecified when both obj1 and obj2
	// are NaN." Wile's #f CONFORMS; Chez answers #t. Pinned as the status quo, with
	// the divergence recorded. Phase 3 of the plan proposes matching Chez.
	{name: "eqv?/nan-distinct", expr: `(eqv? +nan.0 +nan.0)`, want: "#f", chez: "#t"},
	{name: "equal?/nan-distinct", expr: `(equal? +nan.0 +nan.0)`, want: "#f", chez: "#t"},
	{
		name: "case/nan-arm-never-fires",
		expr: `(case (/ 0.0 0.0) ((+nan.0) 'hit) (else 'miss))`,
		want: "miss", chez: "hit",
	},

	// Reflexivity is NOT optional, whatever NaN does across objects: eqv? settles
	// identity before it looks at the value, and equal? may not be finer than eqv?.
	// Established at each numeric leaf's pointer compare, NOT by an `a == b` in
	// values.Equal — see values/float.go.
	{name: "eqv?/nan-reflexive", expr: `(let ((x (/ 0.0 0.0))) (eqv? x x))`, want: "#t"},
	{name: "equal?/nan-reflexive", expr: `(let ((x (/ 0.0 0.0))) (equal? x x))`, want: "#t"},
	{name: "memv/nan-reflexive", expr: `(let ((x (/ 0.0 0.0))) (if (memv x (list x)) 'found 'lost))`, want: "found"},

	// ---- F4/F5 — "all other cases": equal? may return either. Wile chooses
	// structural; Chez chooses identity. Legal, deliberate, and pinned so it stays
	// a decision rather than an emergent property of EqualComponents existing. ----
	{
		name: "equal?/records-structural",
		expr: `(begin
		         (define-record-type <pt> (make-pt x y) pt? (x pt-x) (y pt-y))
		         (equal? (make-pt 1 2) (make-pt 1 2)))`,
		want: "#t", chez: "#f",
	},
	{
		name: "eqv?/records-identity",
		expr: `(begin
		         (define-record-type <pt2> (make-pt2 x y) pt2? (x pt2-x) (y pt2-y))
		         (eqv? (make-pt2 1 2) (make-pt2 1 2)))`,
		want: "#f",
	},
	{
		name: "equal?/records-differing-fields",
		expr: `(begin
		         (define-record-type <pt3> (make-pt3 x y) pt3? (x pt3-x) (y pt3-y))
		         (equal? (make-pt3 1 2) (make-pt3 1 9)))`,
		want: "#f",
	},
	{name: "equal?/box-structural", expr: `(equal? (box 1) (box 1))`, want: "#t"},
	{
		name: "equal?/hashtable-structural",
		expr: `(let ((a (make-hashtable)) (b (make-hashtable)))
		         (hashtable-set! a 'k 1)
		         (hashtable-set! b 'k 1)
		         (equal? a b))`,
		want: "#t", chez: "#f",
	},

	// Procedures: equal? returns the same as eqv? (§6.1), i.e. identity.
	{name: "equal?/distinct-lambdas", expr: `(equal? (lambda (x) x) (lambda (x) x))`, want: "#f"},
	{name: "eqv?/same-lambda", expr: `(let ((f (lambda (x) x))) (eqv? f f))`, want: "#t"},
}

// TestEquivalencePredicateConformance pins eq?/eqv?/equal? against R7RS §6.1.
//
// Cases carrying a `bug` note are skipped: their `want` is the answer the fix must
// produce, so the skip disappears — rather than the expectation changing — when the
// defect is closed. A case that starts passing while still marked `bug` fails loudly,
// which is what stops a fix from landing without its pin being un-skipped.
func TestEquivalencePredicateConformance(t *testing.T) {
	for _, tc := range equivCases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)

			result, err := engine.EvalMultiple(ctx, tc.expr)
			c.Assert(err, qt.IsNil)
			got := result.SchemeString()

			if tc.bug == "" {
				c.Assert(got, qt.Equals, tc.want, qt.Commentf("expr: %s", tc.expr))
				return
			}
			// Known defect: assert it still misbehaves, and say what it should be.
			// When the fix lands this assertion fires, forcing the note's removal.
			c.Assert(got, qt.Not(qt.Equals), tc.want,
				qt.Commentf("FIXED — drop the `bug` note on %q; it now correctly answers %s. (%s)",
					tc.name, tc.want, tc.bug))
			t.Logf("known defect (%s): got %s, R7RS requires %s", tc.bug, got, tc.want)
		})
	}
}

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
// The `want` column is what Wile MUST answer.
//
// This table was written while F1 (signed zero) and F2 (equal? disagreeing with
// eqv? on cross-representation inexacts) were still open: each defective case
// carried a `bug` note and asserted the WRONG answer, with `want` recording the
// R7RS-required one. Closing the defects made those pins fail, which is what
// forced the notes off. The notes are gone; the cases remain. See
// docs/reference/r7rs-differences.md (items 9 and 10).
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
//   - eqv? on two NaNs is explicitly unspecified ("As an exception, the behavior of
//     eqv? is unspecified when both obj1 and obj2 are NaN"). Wile answers #t, matching
//     Chez and Racket. Numeric = keeps IEEE semantics, so (= +nan.0 +nan.0) is still #f.
type equivCase struct {
	name string
	expr string
	want string
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

	// F2 (fixed) — equal? and eqv? must agree on numbers; §6.1 gives no latitude.
	// #f is the correct answer for both: a float64 and an arbitrary-precision
	// BigFloat are not substitutable under a finite composition of standard
	// arithmetic ((+ x 1e-20) separates them). equal? used to answer #t here,
	// via a cross-kind arm in Float.EqualTo that Eqv did not have. Both now
	// route through values.EqvNumber and cannot drift apart again.
	{
		name: "eqv?/float-vs-bigfloat", expr: `(eqv? 1.0 #m1.0)`, want: "#f",
	},
	{
		name: "equal?/float-vs-bigfloat", expr: `(equal? 1.0 #m1.0)`, want: "#f",
	},

	// F1 (fixed) — signed zero. §6.1's eqv? #f clause fires whenever the
	// implementation distinguishes negative zero, and Wile does: the two witness
	// rows below are the proof, and they are in this table precisely so that a
	// future change making Wile STOP distinguishing -0.0 would break here and
	// flag that (eqv? 0.0 -0.0) may then legally become #t.
	{name: "F1/witness-positive", expr: `(/ 1.0 0.0)`, want: "+inf.0"},
	{name: "F1/witness-negative", expr: `(/ 1.0 -0.0)`, want: "-inf.0"},
	{
		name: "eqv?/signed-zero", expr: `(eqv? 0.0 -0.0)`, want: "#f",
	},
	{
		name: "equal?/signed-zero", expr: `(equal? 0.0 -0.0)`, want: "#f",
	},
	{
		name: "memv/signed-zero", expr: `(memv -0.0 (list 0.0))`, want: "#f",
	},

	// F3 (resolved) — NaN. §6.1: "As an exception, the behavior of eqv? is
	// unspecified when both obj1 and obj2 are NaN." Both answers conform. Wile now
	// answers #t, matching Chez and Racket; it used to answer #f, which made
	// (memv +nan.0 lst) unable to find a NaN it had not itself allocated and made
	// the (case x ((+nan.0) …)) arm dead code.
	//
	// This is NOT IEEE equality and must not be confused with it: eqv? is an
	// EQUIVALENCE relation, hence reflexive, which IEEE `==` deliberately is not.
	// Numeric = keeps IEEE semantics — see the row below.
	{name: "eqv?/nan-distinct", expr: `(eqv? +nan.0 +nan.0)`, want: "#t"},
	{name: "equal?/nan-distinct", expr: `(equal? +nan.0 +nan.0)`, want: "#t"},
	{
		// Different NaN PAYLOADS: the +nan.0 literal and (/ 0.0 0.0) have different
		// bit patterns. They are still eqv?, which is why HashCode canonicalizes NaN.
		name: "eqv?/nan-across-payloads",
		expr: `(eqv? +nan.0 (/ 0.0 0.0))`,
		want: "#t",
	},
	{
		// The Hashable contract in action: equal? implies equal hashes, so a table
		// keyed on one NaN payload must be found by another.
		name: "hashtable/nan-key-across-payloads",
		expr: `(let ((h (make-equal-hashtable)))
		         (hashtable-set! h (/ 0.0 0.0) 'found)
		         (hashtable-ref h +nan.0 'missing))`,
		want: "found",
	},
	{
		name: "case/nan-arm-fires",
		expr: `(case (/ 0.0 0.0) ((+nan.0) 'hit) (else 'miss))`,
		want: "hit",
	},
	{
		// Numeric = is a DIFFERENT predicate and stays IEEE-754. Reflexivity is a law
		// of equivalence relations, not of =.
		name: "=/nan-stays-ieee",
		expr: `(= +nan.0 +nan.0)`,
		want: "#f",
	},

	// ---- Numeric leaves INSIDE containers. ----
	//
	// This is where values.Equal's worklist reorder actually lives: a numeric leaf
	// reached as a container COMPONENT settles through its own EqualTo (and so through
	// EqvNumber), never through an interface ==. The top-level rows above cannot exercise
	// that path, and the pre-existing NaN-in-container row compared a container against
	// ITSELF — which the identity short-circuit answers without ever reaching a leaf.
	// Every row here uses DISTINCT objects.
	{name: "equal?/nan-in-distinct-vectors", expr: `(equal? (vector +nan.0) (vector +nan.0))`, want: "#t"},
	{name: "equal?/nan-in-distinct-lists", expr: `(equal? (list +nan.0) (list +nan.0))`, want: "#t"},
	{
		// Signed zero must survive the descent into a container, exactly as at top level.
		name: "equal?/signed-zero-in-lists",
		expr: `(equal? (list 0.0) (list -0.0))`,
		want: "#f",
	},
	{
		// member is equal?-based, so it inherits the leaf rule — including this one.
		name: "member/signed-zero-distinguished",
		expr: `(if (member 0.0 (list -0.0)) #t #f)`,
		want: "#f",
	},
	{
		// Exactness is observable through a container too: 1/2 is exact, 0.5 is not.
		name: "equal?/exact-vs-inexact-in-lists",
		expr: `(equal? (list 1/2) (list 0.5))`,
		want: "#f",
	},
	{
		// ...and the exact family still collapses inside a container.
		name: "equal?/exact-family-in-lists",
		expr: `(equal? (list 1) (list #e1))`,
		want: "#t",
	},
	{
		// A float64 NaN and an arbitrary-precision NaN are still different kinds:
		// precision is observable for inexact numbers, NaN included.
		name: "eqv?/nan-cross-kind",
		expr: `(eqv? +nan.0 (/ #m0.0 #m0.0))`,
		want: "#f",
	},

	// Reflexivity is NOT optional, whatever NaN does across objects: eqv? settles
	// identity before it looks at the value, and equal? may not be finer than eqv?.
	// Established by the identity check at the head of values.EqvNumber.
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
		expr: `(let ((a (make-equal-hashtable)) (b (make-equal-hashtable)))
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
func TestEquivalencePredicateConformance(t *testing.T) {
	for _, tc := range equivCases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)

			result, err := engine.EvalMultiple(ctx, tc.expr)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want, qt.Commentf("expr: %s", tc.expr))

			// The chez field is an ASSERTION, not a comment. It exists to say "this row
			// is a known, legal divergence from Chez", so if it ever records the same
			// answer Wile gives, it is stale and the row is no longer a divergence at
			// all — which is exactly the sort of drift nobody notices in a struct field
			// that is only ever written. It was write-only until this check existed.
			if tc.chez == "" {
				return
			}
			c.Assert(tc.chez, qt.Not(qt.Equals), tc.want, qt.Commentf(
				"row %q records Chez's answer as %s, which is also Wile's answer — so this "+
					"is NOT a divergence and the chez field should be dropped", tc.name, tc.chez))
			t.Logf("legal divergence: Wile %s, Chez %s (R7RS permits either)", tc.want, tc.chez)
		})
	}
}

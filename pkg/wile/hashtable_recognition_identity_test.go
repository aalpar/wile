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
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestMakeHashtableRecognitionSurvivesImports pins that (make-hashtable
// equal-hash equal?) keeps working after an import — including (import (scheme
// base)), which every conforming R7RS program begins with.
//
// A library environment is a flat island: environment/namespace.go NewChildRuntime
// gives it parent nil, so the library env factory re-applies the whole registry
// into it and every primitive is minted a SECOND *machine.ForeignClosure there.
// (scheme base) exports equal?, so after that import the caller's equal? is the
// library's closure while the sealed base still holds the engine's. Recognition by
// closure POINTER therefore saw two different objects for one primitive and
// refused the pair.
//
// The identity token is what makes these cases agree: every closure built from one
// PrimitiveSpec carries the same *machine.PrimitiveIdentity, whichever environment
// minted it, so recognition asks "is this the registered equal-hash?" rather than
// "is this the sealed base's copy of it?".
func TestMakeHashtableRecognitionSurvivesImports(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"before any import",
			`(hashtable? (make-hashtable equal-hash equal?))`},
		// The regression case: (scheme base) exports equal?, which is enough on
		// its own — equal-hash need not be exported by anything.
		{"after (import (scheme base))",
			`(begin (import (scheme base)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"after (import (rnrs hashtables))",
			`(begin (import (rnrs hashtables)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"after (import (srfi 13))",
			`(begin (import (srfi 13)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"after both",
			`(begin (import (scheme base)) (import (rnrs hashtables)) (hashtable? (make-hashtable equal-hash equal?)))`},
		// The table is genuinely equal?-keyed, not merely a hashtable: a fresh
		// list must find the entry a structurally-equal list stored.
		{"and the table is equal?-keyed after the import",
			`(begin (import (scheme base)) (let ((h (make-hashtable equal-hash equal?))) (hashtable-set! h (list 1 2) 'x) (eq? (hashtable-ref h (list 1 2) #f) 'x)))`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			// A FRESH engine per case: an import mutates the top level, so
			// reusing one engine would let an earlier case decide a later one.
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, `#t`)
		})
	}
}

// TestMakeHashtableRejectsUnregisteredProcedures is the fail-closed half. The
// token must not be forgeable from Scheme: a user procedure named equal-hash, or
// any other Scheme closure, carries no identity and is refused. Widening
// recognition to a name compare would accept these and silently hash with the
// wrong function.
func TestMakeHashtableRejectsUnregisteredProcedures(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"a lambda in the hash position",
			`(let ((eh (lambda (x) 0))) (make-hashtable eh equal?))`},
		{"a lambda in the equivalence position",
			`(let ((eq2 (lambda (a b) #t))) (make-hashtable equal-hash eq2))`},
		{"a different registered primitive in the hash position",
			`(make-hashtable string-hash equal?)`},
		{"a different registered primitive in the equivalence position",
			`(make-hashtable equal-hash eqv?)`},
		{"a non-procedure",
			`(make-hashtable 1 2)`},
		{"the pair in the wrong order",
			`(make-hashtable equal? equal-hash)`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			eng := newSRFITestEngine(t)
			_, err := eng.EvalMultiple(t.Context(), tc.expr)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// TestHashtableAccessorsRoundTripAfterImport pins the accessors against the same
// split. R6RS lets a program read a table's (hash, equivalence) pair back off it
// and build another table from them, so whatever hashtable-hash-function returns
// must be something make-hashtable accepts — before and after an import.
//
// The eq? cases are the stronger claim: the accessor hands back the binding the
// CALLER holds, not the sealed base's copy, so the pair a program reads off a
// table is the pair it wrote.
func TestHashtableAccessorsRoundTripAfterImport(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"round-trip without an import",
			`(let ((h (make-equal-hashtable))) (hashtable? (make-hashtable (hashtable-hash-function h) (hashtable-equivalence-function h))))`},
		{"round-trip after (import (scheme base))",
			`(begin (import (scheme base)) (let ((h (make-equal-hashtable))) (hashtable? (make-hashtable (hashtable-hash-function h) (hashtable-equivalence-function h)))))`},
		{"hash-function is eq? to the caller's equal-hash",
			`(begin (import (scheme base)) (eq? (hashtable-hash-function (make-equal-hashtable)) equal-hash))`},
		{"equivalence-function is eq? to the caller's equal?",
			`(begin (import (scheme base)) (eq? (hashtable-equivalence-function (make-equal-hashtable)) equal?))`},
		{"eqv table's equivalence-function is eq? to the caller's eqv?",
			`(begin (import (scheme base)) (eq? (hashtable-equivalence-function (make-eqv-hashtable)) eqv?))`},
		{"eq table's equivalence-function is eq? to the caller's eq?",
			`(begin (import (scheme base)) (eq? (hashtable-equivalence-function (make-eq-hashtable)) eq?))`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, `#t`)
		})
	}
}

// TestHashtableAccessorsSurviveAUserShadow pins the SEALED-tier fallback, which
// is the branch reached only when the mutable tier holds something that is not
// this primitive.
//
// name only LOCATES a candidate; identity DECIDES. A user (define equal-hash …)
// wins the ranked probe from the runtime view and then fails the identity check,
// and the accessor must ask the sealed tiers directly rather than conclude the
// primitive is unregistered. Nothing else exercises that branch: without a
// shadow the mutable tier is either empty (so the probe falls through to the
// sealed slot on its own) or holds the real primitive after an import.
func TestHashtableAccessorsSurviveAUserShadow(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"shadowed equal-hash",
			`(begin (define equal-hash 42)
				(procedure? (hashtable-hash-function (make-equal-hashtable))))`},
		{"shadowed equal?",
			`(begin (define equal? 42)
				(procedure? (hashtable-equivalence-function (make-equal-hashtable))))`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, `#t`)
		})
	}
}

// TestHashtableRemembersTheProceduresItWasMadeWith pins I106: R6RS §13.1 says
// hashtable-equivalence-function returns the equivalence function the table WAS
// MADE WITH, and that hashtable-copy keeps it. Chez agrees — `petite -q --script`
// on the same program answers (#t #t).
//
// The name probe in setRecognizedPrimitive cannot answer that question, and this
// is the case that separates the two. Under a RENAMED import the program holds
// the equal? primitive as my-eq and never binds the spelling `equal?` at all, so
// the probe falls through to the sealed base's copy — a different closure object
// from the one the caller passed to make-hashtable, because a library environment
// is a flat island that mints its own. Recognition is right that the two are the
// same PRIMITIVE; the accessor's contract is about the OBJECT.
//
// Both elements are load-bearing. (*values.Hashtable).Copy is an explicit field
// list rather than a struct copy, so a remembered field that Copy does not carry
// regresses to the probe on every hashtable-copy while the first element still
// passes. A single-element row would be green over that bug.
func TestHashtableRemembersTheProceduresItWasMadeWith(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
		want string
	}{
		// The gate. my-eq is the (scheme base) closure under a name the probe
		// will never look up.
		{"a renamed equivalence procedure survives the table and its copy",
			`(begin (import (rnrs hashtables)
					(only (rename (scheme base) (equal? my-eq)) my-eq))
				(let ((h (make-hashtable equal-hash my-eq)))
					(list (eq? (hashtable-equivalence-function h) my-eq)
						(eq? (hashtable-equivalence-function (hashtable-copy h)) my-eq))))`,
			`(#t #t)`},
		// The hash side has no red row of its own, and the reason is worth
		// recording rather than leaving as a gap: no library in the tree exports
		// equal-hash (pkg/stdlib/lib/rnrs/hashtables.sld says so and why), so
		// there is no second copy of it to hold under another name and the probe
		// and the remembered object cannot disagree. It is covered here against
		// the Copy field list, and at the unit level in pkg/values.
		{"the hash procedure survives the table and its copy",
			`(begin (import (rnrs hashtables)
					(only (rename (scheme base) (equal? my-eq)) my-eq))
				(let ((h (make-hashtable equal-hash my-eq)))
					(list (eq? (hashtable-hash-function h) equal-hash)
						(eq? (hashtable-hash-function (hashtable-copy h)) equal-hash))))`,
			`(#t #t)`},
		// The control: with (scheme base) imported plainly the probe and the
		// remembered value agree, and must keep agreeing.
		{"the plain-import control is unchanged",
			`(begin (import (rnrs hashtables) (scheme base))
				(let ((h (make-hashtable equal-hash equal?)))
					(list (eq? (hashtable-equivalence-function h) equal?)
						(eq? (hashtable-equivalence-function (hashtable-copy h)) equal?)
						(eq? (hashtable-hash-function h) equal-hash))))`,
			`(#t #t #t)`},
		// A fixed-kind constructor takes no procedures, so there is nothing to
		// remember and the probe stays the only answer. This is the residual
		// I069 depends on, pinned so the fix cannot be mistaken for deleting it.
		{"a fixed-kind table still answers from the probe",
			`(begin (import (rnrs hashtables) (scheme base))
				(let ((h (make-equal-hashtable)))
					(list (eq? (hashtable-equivalence-function h) equal?)
						(eq? (hashtable-hash-function h) equal-hash))))`,
			`(#t #t)`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

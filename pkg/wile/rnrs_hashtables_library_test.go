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

// TestRnrsHashtablesLibrary covers (rnrs hashtables), the first (rnrs ...)
// library in the tree.
//
// NOTE ON THE NAME. R6RS spells it (rnrs hashtables (6)), and that form does NOT
// resolve here: Wile's import-set resolver rejects a list as a library-name part
// ("library name part must be identifier or integer"). R6RS versioned library
// names are a separate feature and out of scope for this work, so the library is
// registered under the versionless name and the versioned spelling is simply not
// accepted. That is a real gap for a portable R6RS program, and it is recorded in
// docs/reference/r7rs-differences.md rather than papered over here.
func TestRnrsHashtablesLibrary(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	cases := []struct {
		name string
		expr string
		want string
	}{
		{"import and construct",
			`(begin (import (rnrs hashtables)) (hashtable? (make-eq-hashtable)))`, `#t`},
		{"R6RS spelling of the equal table",
			`(begin (import (rnrs hashtables)) (let ((h (make-hashtable equal-hash equal?))) (hashtable-set! h (list 1) 'x) (hashtable-ref h (list 1) #f)))`, `x`},
		{"entries via the library",
			`(begin (import (rnrs hashtables)) (let ((h (make-eqv-hashtable))) (hashtable-set! h 1 'a) (call-with-values (lambda () (hashtable-entries h)) (lambda (ks vs) (vector-length ks)))))`, `1`},
		{"update! via the library",
			`(begin (import (rnrs hashtables)) (let ((h (make-equal-hashtable))) (hashtable-update! h 'a (lambda (v) (+ v 1)) 0) (hashtable-ref h 'a #f)))`, `1`},
		{"the inspection procedures round-trip",
			`(begin (import (rnrs hashtables)) (let ((h (make-eqv-hashtable))) (and (eq? (hashtable-equivalence-function h) eqv?) (not (hashtable-hash-function h)) (hashtable-mutable? h))))`, `#t`},
		// The hash procedures are reachable after the import even though the
		// library does NOT export them — they are sealed-base bindings.
		{"the hash functions are still reachable",
			`(begin (import (rnrs hashtables)) (and (= (equal-hash '(1 2)) (equal-hash (list 1 2))) (= (string-ci-hash "A") (string-ci-hash "a")) (= (symbol-hash 'q) (symbol-hash 'q)) (>= (string-hash "s") 0)))`, `#t`},
		{"immutability reaches through the library",
			`(begin (import (rnrs hashtables)) (hashtable-mutable? (hashtable-copy (make-equal-hashtable))))`, `#f`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, eng, tc.expr)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestRnrsHashtablesVersionedNameUnsupported pins the gap named above, so that a
// later change adding R6RS versioned library names fails here and prompts the
// library to be reachable under its standard spelling.
func TestRnrsHashtablesVersionedNameUnsupported(t *testing.T) {
	eng := newSRFITestEngine(t)
	_, err := eng.EvalMultiple(t.Context(), `(begin (import (rnrs hashtables (6))) (hashtable? (make-eq-hashtable)))`)
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("if versioned library names now resolve, register (rnrs hashtables (6)) and update r7rs-differences"))
}

// TestRnrsHashtablesDoesNotShadowTheRecognizedPair is why (rnrs hashtables) does
// NOT export equal-hash, string-hash, string-ci-hash or symbol-hash, which R6RS
// does list in it.
//
// make-hashtable recognizes its (hash, equiv) pair by POINTER IDENTITY against
// the sealed base. An import COPIES the exported binding into the importing
// environment as a distinct object, so exporting equal-hash rebound the name to
// something make-hashtable no longer recognized — measured: (import (rnrs
// hashtables)) then (make-hashtable equal-hash equal?) raised "unsupported
// hash/equivalence pair". Importing the R6RS library broke the R6RS spelling,
// which is the exact opposite of the library's purpose.
//
// (import (srfi 13)) is the control: it does not export equal-hash, and the
// spelling survives it. If this test starts failing because the export list grew
// back, the fix is to shrink the export list, not to loosen recognition —
// matching against the runtime binding would accept a user's top-level
// (define (equal-hash x) 0) and fail OPEN.
func TestRnrsHashtablesDoesNotShadowTheRecognizedPair(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"before any import", `(hashtable? (make-hashtable equal-hash equal?))`},
		{"after importing (rnrs hashtables)", `(begin (import (rnrs hashtables)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"control: after importing (srfi 13)", `(begin (import (srfi 13)) (hashtable? (make-hashtable equal-hash equal?)))`},
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

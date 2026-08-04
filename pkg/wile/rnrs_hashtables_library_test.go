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
// NOTE ON THE NAME. R6RS spells it (rnrs hashtables (6)). The library is
// registered under the VERSIONLESS name, and the versioned spelling reaches it
// because ParseLibraryNameFromDatum drops a final-position version reference
// rather than treating it as a name part — see TestLibraryNameVersionReference.
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

// TestLibraryNameVersionReference covers the R6RS version reference, which
// ParseLibraryNameFromDatum drops from a library name's final position.
//
// The library is registered under the versionless name and the versioned
// spellings reach it because the reference never becomes a name part. Nothing is
// matched: Wile has no version metadata, so every reference is vacuously
// satisfied, and the `(7)` case pins that rather than leaving it to be discovered.
//
// The negative cases are the boundary. A list is R6RS only in the FINAL position —
// R7RS name parts are identifiers and exact non-negative integers, so a list
// elsewhere stays an error — and a name that is nothing but a version denotes no
// library.
func TestLibraryNameVersionReference(t *testing.T) {
	c := qt.New(t)

	accepted := []struct {
		name string
		expr string
	}{
		{"the R6RS spelling", `(begin (import (rnrs hashtables (6))) (hashtable? (make-eq-hashtable)))`},
		{"a version reference, not a version", `(begin (import (rnrs hashtables (and (>= 6) (< 7)))) (hashtable? (make-eq-hashtable)))`},
		{"the empty reference matches any version", `(begin (import (rnrs hashtables ())) (hashtable? (make-eq-hashtable)))`},
		{"an unsatisfiable version is still ignored", `(begin (import (rnrs hashtables (7))) (hashtable? (make-eq-hashtable)))`},
		{"under an import modifier", `(begin (import (only (rnrs hashtables (6)) make-eq-hashtable hashtable?)) (hashtable? (make-eq-hashtable)))`},
	}
	for _, tc := range accepted {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, `#t`)
		})
	}

	rejected := []struct {
		name string
		expr string
	}{
		{"a list in a non-final position", `(import ((6) rnrs hashtables))`},
		{"a name that is only a version", `(import ((6)))`},
		{"a version reference on a library that does not exist", `(import (no such library (6)))`},
	}
	for _, tc := range rejected {
		c.Run(tc.name, func(c *qt.C) {
			_, err := newSRFITestEngine(t).EvalMultiple(t.Context(), tc.expr)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// TestLibraryNameVersionIsNotAnIntegerPart pins that dropping the reference does
// not disturb R7RS integer name parts, which look adjacent and are not: (srfi 13)
// binds 13 as a part, where (srfi (13)) would drop it.
func TestLibraryNameVersionIsNotAnIntegerPart(t *testing.T) {
	c := qt.New(t)
	got := evalSRFI(c, newSRFITestEngine(t), `(begin (import (srfi 13)) (string-index "abc" #\b))`)
	c.Assert(got, qt.Equals, `1`)
}

// TestRnrsHashtablesRecognitionIsIndependentOfTheExportList pins that what
// (rnrs hashtables) exports cannot affect whether (make-hashtable equal-hash
// equal?) is recognized.
//
// It once could, and the export list carried the workaround. Recognition compared
// the argument's CLOSURE POINTER against the sealed base's, and an import installs
// the exporting library's own copy of a primitive — a library environment is a
// flat island that mints its own — so exporting equal-hash rebound the name to an
// object recognition no longer matched. The library therefore withheld the four
// hash procedures R6RS lists in it.
//
// That reading of the cause was too narrow, and the workaround too: (scheme base)
// exports equal?, so the pair broke under the import every R7RS program opens
// with, whatever this library exported. The fix is identity-token recognition
// (machine.PrimitiveIdentity), which is blind to which environment minted the
// closure — see TestMakeHashtableRecognitionSurvivesImports for the full matrix.
//
// So this test no longer defends an export list. It defends the independence: if
// it fails, recognition has re-acquired a dependence on where a binding came from,
// and the fix is there, not in the .sld.
func TestRnrsHashtablesRecognitionIsIndependentOfTheExportList(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"before any import", `(hashtable? (make-hashtable equal-hash equal?))`},
		{"after importing (rnrs hashtables)", `(begin (import (rnrs hashtables)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"after importing (srfi 13)", `(begin (import (srfi 13)) (hashtable? (make-hashtable equal-hash equal?)))`},
		// The library re-exporting a hash procedure under a new name must not
		// disturb the pair either — a rename installs a third copy of it.
		{"after a renaming import of the library",
			`(begin (import (rename (rnrs hashtables) (hashtable? ht?))) (and (ht? (make-hashtable equal-hash equal?)) #t))`},
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

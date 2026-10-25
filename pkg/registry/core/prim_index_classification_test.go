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
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestIndexArgumentsShareOneClassification pins that the list family
// (list-ref, list-tail, list-set!) and the vector family (vector-ref,
// string-ref and their setters) classify an index argument by one rule
// (values.ExactIndex): not an exact integer is ErrNotAnInteger; an exact
// integer that no sequence can reach (a bignum) or a negative one is
// ErrIndexOutOfRange, with one text per case. Before, a bignum was a range
// error for lists and a type error for vectors, and a negative index had a
// different text in each family.
//
// Oracles: petite and racket both call the bignum a range error ("index ... is
// out of range for list", "not a valid index", "index is out of range").
func TestIndexArgumentsShareOneClassification(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		code     string
		sentinel error
		want     string
	}{
		{
			name:     "bignum vector-ref index is out of range",
			code:     `(vector-ref (vector 1) 100000000000000000000)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "vector-ref: index 100000000000000000000 out of range",
		},
		{
			name:     "bignum vector-set! index is out of range",
			code:     `(vector-set! (vector 1) 100000000000000000000 0)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "vector-set!: index 100000000000000000000 out of range",
		},
		{
			name:     "bignum string-ref index is out of range",
			code:     `(string-ref "a" 100000000000000000000)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "string-ref: index 100000000000000000000 out of range",
		},
		{
			name:     "bignum bytevector-u8-ref index is out of range",
			code:     `(bytevector-u8-ref (bytevector 1) 100000000000000000000)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "bytevector-u8-ref: index 100000000000000000000 out of range",
		},
		{
			name:     "negative bignum is out of range, not a type error",
			code:     `(vector-ref (vector 1) -100000000000000000000)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "vector-ref: index -100000000000000000000 out of range",
		},
		{
			name:     "bignum list-ref index is out of range",
			code:     `(list-ref (list 1 2) 100000000000000000000)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-ref: index 100000000000000000000 out of range",
		},
		{
			name:     "bignum vector-copy start is out of range",
			code:     `(vector-copy (vector 1 2) (expt 10 20))`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "vector-copy: start: index 100000000000000000000 out of range",
		},
		{
			name:     "bignum vector-fill! start is out of range",
			code:     `(vector-fill! (vector 1 2) 0 (expt 10 20))`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "vector-fill!: start: index 100000000000000000000 out of range",
		},
		{
			name:     "a symbol vector-copy end is not an integer, rendered as Scheme",
			code:     `(vector-copy (vector 1 2) 0 'x)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "vector-copy: end: expected an exact integer index but got x",
		},
		{
			name:     "negative vector-ref index",
			code:     `(vector-ref (vector 1) -1)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "vector-ref: index -1 must be non-negative",
		},
		{
			name:     "negative string-ref index",
			code:     `(string-ref "a" -1)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "string-ref: index -1 must be non-negative",
		},
		{
			name:     "negative list-ref index",
			code:     `(list-ref (list 1 2) -1)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-ref: index -1 must be non-negative",
		},
		{
			name:     "negative list-tail index",
			code:     `(list-tail (list 1 2) -1)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-tail: index -1 must be non-negative",
		},
		{
			name:     "control: vector upper bound keeps its length",
			code:     `(vector-ref (vector 1) 1)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "vector-ref: index 1 out of bounds for length 1",
		},
		{
			name:     "control: a symbol index is not an integer (vector-ref)",
			code:     `(vector-ref (vector 1) 'x)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "vector-ref: expected an exact integer index but got x",
		},
		{
			name:     "control: a symbol index is not an integer (list-set!)",
			code:     `(list-set! (list 1 2) 'x 0)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-set!: expected an exact integer index but got x",
		},
		{
			// racket: "list-ref: contract violation expected: pair? given: 5";
			// petite: "index 0 reaches a non-pair in 5".
			name:     "a non-list list-ref argument is not a list",
			code:     `(list-ref 5 0)`,
			sentinel: werr.ErrNotAList,
			want:     "list-ref: argument 1: expected a list but got 5",
		},
		{
			name:     "a non-list list-set! argument renders as a Scheme value",
			code:     `(list-set! 5 0 'x)`,
			sentinel: werr.ErrNotAList,
			want:     "list-set!: argument 1: expected a list but got 5",
		},
		{
			// list-ref reads the index first, as before.
			name:     "list-ref reports a bad index before a bad list",
			code:     `(list-ref 5 'x)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-ref: expected an exact integer index but got x",
		},
		{
			// A syntax pair is a Tuple but not a mutable list (as on master).
			name:     "list-set! refuses a syntax pair as a list",
			code:     `(list-set! (syntax (a b)) 0 'x)`,
			sentinel: werr.ErrNotAList,
			want:     "list-set!: argument 1: expected a list but got",
		},
		{
			// list-set! keeps its own order: the list is read first.
			name:     "list-set! reports a bad list before a bad index",
			code:     `(list-set! 5 'x 0)`,
			sentinel: werr.ErrNotAList,
			want:     "list-set!: argument 1: expected a list but got 5",
		},
		{
			// Chez agrees: "index 0 is out of range for list ()".
			name:     "the empty list is a list that runs out (list-ref)",
			code:     `(list-ref '() 0)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-ref: index 0 points past the last element",
		},
		{
			// Both oracles: "index 1 reaches a non-pair in 5"; an index fault,
			// not a type fault, because (list-tail 5 0) is 5.
			name:     "list-tail past a non-list is an index fault",
			code:     `(list-tail 5 1)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-tail: index 1 out of bounds: improper tail at depth 0",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			c := qt.New(t)
			err := testhelpers.RunSchemeCodeExpectError(t, tc.code)
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, tc.sentinel), qt.IsTrue, qt.Commentf("%v", err))
			c.Check(err.Error(), qt.Contains, tc.want)
		})
	}
}

// TestSubrangeBoundsAcceptIntegralLiterals is a control: the reader already
// reduces the literal 4/2 to the integer 2, so these rows pass on master too.
// The integer-valued *Rational path itself is pinned at the Go level by
// TestParseOptionalStartEndClassifiesBounds (helpers), which builds one.
func TestSubrangeBoundsAcceptIntegralLiterals(t *testing.T) {
	t.Parallel()
	tests := []struct {
		code string
		want string
	}{
		{`(vector-copy (vector 1 2 3) 4/2)`, "#(3)"},
		{`(vector-copy (vector 1 2 3) 0 4/2)`, "#(1 2)"},
		{`(list-ref '(a b c) 4/2)`, "c"},
	}
	for _, tc := range tests {
		t.Run(tc.code, func(t *testing.T) {
			t.Parallel()
			c := qt.New(t)
			v, err := testhelpers.RunSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Check(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestIndexKeyedListPrimitivesOnCircularLists pins that a circular list with
// an index far past its length answers instead of walking the index out: R7RS
// §6.4 says list-ref's "list argument can be circular", and Chez answers these
// at once. The index reduces modulo the cycle length once the walk detects the
// cycle. Before, (list-ref c (expt 2 62)) spun until killed.
func TestIndexKeyedListPrimitivesOnCircularLists(t *testing.T) {
	t.Parallel()

	const circ = `(define c (list 1 2 3)) (set-cdr! (cddr c) c) `
	const lead = `(define c (list 'a 'b 1 2 3)) (set-cdr! (cddr (cddr c)) (cddr c)) `
	tests := []struct {
		name string
		code string
		want string
	}{
		{"list-ref a huge index", circ + `(list-ref c 4611686018427387904)`, "2"},
		{"list-ref a small index", circ + `(list-ref c 7)`, "2"},
		{"an index that lands on the detecting cell", circ + `(list-ref c 8)`, "3"},
		{"list-tail a huge index", circ + `(car (list-tail c 4611686018427387905))`, "3"},
		{"list-set! a huge index", circ + `(list-set! c 4611686018427387904 'z) (cadr c)`, "z"},
		{"a lead-in before the cycle", lead + `(list-ref c 4611686018427387904)`, "3"},
		{"a lead-in, index inside the lead", lead + `(list-ref c 1)`, "b"},
		{"a one-cell cycle", `(define c (list 7)) (set-cdr! c c) (list-ref c 4611686018427387904)`, "7"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			c := qt.New(t)
			v, err := testhelpers.RunSchemeCode(t, `(begin `+tc.code+`)`)
			c.Assert(err, qt.IsNil)
			c.Check(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

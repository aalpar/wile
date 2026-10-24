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
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestListSetReportsWhereTheWalkRanOut pins that list-set! walks the list the
// way list-ref and list-tail do (values.NthCons), so the three index-keyed
// primitives say the same thing about the same mistake. Before the walk was
// shared, list-set! answered every overshoot with "index out of range", which
// names neither the index nor where the list ended.
//
// R7RS §6.4 leaves the message unspecified; the settlement is consistency with
// the siblings, not a conformance claim.
func TestListSetReportsWhereTheWalkRanOut(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		sentinel error
		want     string
	}{
		{
			name:     "overshoot mid-walk names the depth",
			code:     `(list-set! (list 1 2) 9 0)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-set!: index 9 out of bounds at depth 2",
		},
		{
			name:     "index exactly at the end points past the last element",
			code:     `(list-set! (list 1 2) 2 0)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-set!: index 2 points past the last element",
		},
		{
			name:     "index at an improper tail points past the last element",
			code:     `(list-set! '(a b . c) 2 'x)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-set!: index 2 points past the last element",
		},
		{
			name:     "overshoot through an improper tail names the tail",
			code:     `(list-set! '(a b . c) 5 'x)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-set!: index 5 out of bounds: improper tail at depth 2",
		},
		{
			name:     "negative index keeps its message",
			code:     `(list-set! (list 1 2) -1 0)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-set!: index must be non-negative",
		},
		{
			// Message deliberately unpinned: it leaks a Go type name (filed).
			name:     "the empty list is rejected as a non-pair before any walk",
			code:     `(list-set! '() 0 'x)`,
			sentinel: werr.ErrNotAList,
		},
		{
			name:     "a non-integer index renders as a Scheme value",
			code:     `(list-set! (list 1 2) 'zork 0)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-set!: expected an exact integer index but got zork",
		},
		{
			name:     "list-ref renders a non-integer index the same way",
			code:     `(list-ref (list 1 2) 'zork)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-ref: expected an exact integer index but got zork",
		},
		{
			name:     "list-tail renders a non-integer index the same way",
			code:     `(list-tail (list 1 2) 'zork)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-tail: expected an exact integer index but got zork",
		},
		{
			name:     "an inexact index is not an exact integer (list-set!)",
			code:     `(list-set! (list 1 2) 1.0 0)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-set!: expected an exact integer index but got 1.0",
		},
		{
			name:     "an inexact index is not an exact integer (list-ref)",
			code:     `(list-ref (list 1 2) 1.0)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-ref: expected an exact integer index but got 1.0",
		},
		{
			name:     "an inexact index is not an exact integer (list-tail)",
			code:     `(list-tail (list 1 2) 1.0)`,
			sentinel: werr.ErrNotAnInteger,
			want:     "list-tail: expected an exact integer index but got 1.0",
		},
		{
			name:     "a bignum index is out of range, not a type error (list-set!)",
			code:     `(list-set! (list 1 2) 100000000000000000000 0)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-set!: index 100000000000000000000 out of range",
		},
		{
			name:     "a bignum index is out of range, not a type error (list-ref)",
			code:     `(list-ref (list 1 2) 100000000000000000000)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-ref: index 100000000000000000000 out of range",
		},
		{
			name:     "a bignum index is out of range, not a type error (list-tail)",
			code:     `(list-tail (list 1 2) 100000000000000000000)`,
			sentinel: werr.ErrIndexOutOfRange,
			want:     "list-tail: index 100000000000000000000 out of range",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			err := testhelpers.RunSchemeCodeExpectError(t, tc.code)
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, tc.sentinel), qt.IsTrue, qt.Commentf("%v", err))
			if tc.want != "" {
				c.Check(err.Error(), qt.Contains, tc.want)
			}
		})
	}
}

// TestListSetStoresAtTheIndexedCell pins the success path of the shared walk:
// the cell stored into is the one k cdr steps from the head, including k=0,
// the last cell, and a circular list (k bounds the walk, so a cycle neither
// hangs nor fails).
func TestListSetStoresAtTheIndexedCell(t *testing.T) {
	tests := []struct {
		name string
		code string
		want string
	}{
		{
			name: "first cell",
			code: `(let ((l (list 1 2 3))) (list-set! l 0 'z) l)`,
			want: "(z 2 3)",
		},
		{
			name: "middle cell",
			code: `(let ((l (list 1 2 3))) (list-set! l 1 'z) l)`,
			want: "(1 z 3)",
		},
		{
			name: "last cell",
			code: `(let ((l (list 1 2 3))) (list-set! l 2 'z) l)`,
			want: "(1 2 z)",
		},
		{
			name: "cell before an improper tail",
			code: `(let ((l (cons 1 (cons 2 3)))) (list-set! l 1 'z) l)`,
			want: "(1 z . 3)",
		},
		{
			name: "circular list, index past the cycle length",
			code: `(let ((l (list 1 2))) (set-cdr! (cdr l) l) (list-set! l 5 'z) (list-ref l 1))`,
			want: "z",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v, err := testhelpers.RunSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(v, qt.IsNotNil)
			c.Check(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestListSetReturnsVoid pins list-set!'s return value, which R7RS leaves
// unspecified and Wile makes the void value (the same as set-car!).
func TestListSetReturnsVoid(t *testing.T) {
	c := qt.New(t)
	v, err := testhelpers.RunSchemeCode(t, `(list-set! (list 1 2) 1 'z)`)
	c.Assert(err, qt.IsNil)
	c.Check(v, qt.Equals, values.Void)
}

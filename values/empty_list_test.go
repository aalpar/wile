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

package values_test

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// TestEmptyListSingletonIdentity pins the merge invariant: EmptyList and
// SyntaxEmptyList are the same singleton (same struct value of the
// unexported emptyListType). A future refactor that makes these point at
// different structs would break the Chez-conformant
// `(equal? (syntax ()) '()) → #t` behavior.
func TestEmptyListSingletonIdentity(t *testing.T) {
	c := qt.New(t)
	// Interface comparison: equal iff same dynamic type and equal value.
	// emptyListType is a zero-size struct, so all instances compare equal.
	c.Assert(values.EmptyList == values.SyntaxEmptyList, qt.IsTrue)
}

// TestEmptyListEqualToSymmetry pins the symmetric-equality fix that the
// duality merge enables. Pre-merge the strict pointer-type EqualTo on
// the deleted *syntaxEmptyListType returned #f for one direction, which
// was contrary to Chez 10.3.0's `(equal? (syntax ()) '()) → #t`.
func TestEmptyListEqualToSymmetry(t *testing.T) {
	c := qt.New(t)
	c.Assert(values.EmptyList.EqualTo(values.SyntaxEmptyList), qt.IsTrue)
	c.Assert(values.SyntaxEmptyList.EqualTo(values.EmptyList), qt.IsTrue)
}

// TestEmptyListSyntaxTupleMethods exercises the SyntaxValue/SyntaxTuple
// methods added to emptyListType so the Go-level contract is locked in
// independently of the Scheme-level (equal? ...) tests.
func TestEmptyListSyntaxTupleMethods(t *testing.T) {
	c := qt.New(t)
	stx, ok := values.EmptyList.(values.SyntaxTuple)
	c.Assert(ok, qt.IsTrue)

	c.Assert(stx.SourceContext(), qt.IsNil)
	c.Assert(stx.Unwrap(), qt.Equals, values.Value(values.EmptyList))
	c.Assert(stx.UnwrapAll(), qt.Equals, values.Value(values.EmptyList))

	vec := stx.AsSyntaxVector()
	c.Assert(vec, qt.IsNotNil)
	c.Assert(len(vec.Values), qt.Equals, 0)

	c.Assert(stx.SyntaxAppend(values.SyntaxEmptyList), qt.Equals, values.SyntaxValue(values.SyntaxEmptyList))

	tail, err := stx.SyntaxForEach(t.Context(), nil)
	c.Assert(err, qt.IsNil)
	c.Assert(tail, qt.Equals, values.SyntaxValue(values.SyntaxEmptyList))
}

// TestEmptyListSyntaxCarCdrPanic verifies SyntaxCar/SyntaxCdr panic
// with werr.ErrNotAPair (parallels Car/Cdr on the empty list per R7RS
// 6.4: "(car '()) is an error").
func TestEmptyListSyntaxCarCdrPanic(t *testing.T) {
	c := qt.New(t)
	stx := values.EmptyList.(values.SyntaxTuple)

	tcs := []struct {
		name string
		call func() any
	}{
		{name: "SyntaxCar", call: func() any { return stx.SyntaxCar() }},
		{name: "SyntaxCdr", call: func() any { return stx.SyntaxCdr() }},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			defer func() {
				r := recover()
				if r == nil {
					t.Fatalf("%s on empty list did not panic", tc.name)
				}
				err, ok := r.(error)
				if !ok {
					t.Fatalf("panic value not an error: %v", r)
				}
				if !errors.Is(err, werr.ErrNotAPair) {
					t.Fatalf("panic does not wrap werr.ErrNotAPair: %v", err)
				}
			}()
			tc.call()
		})
	}
	_ = c
}

// TestIsEmptyListPredicate locks in the IsEmptyList(v) semantics across
// the values that the rest of the codebase passes through it. The
// previous strict pointer-type singleton check in IsSyntaxEmptyList
// returned #f for nil and any non-singleton value; the merged predicate
// handles all three branches (singleton, nil, non-empty).
func TestIsEmptyListPredicate(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		v    values.Value
		want bool
	}{
		{name: "EmptyList singleton", v: values.EmptyList, want: true},
		{name: "SyntaxEmptyList alias", v: values.SyntaxEmptyList, want: true},
		{name: "nil", v: nil, want: false},
		{name: "non-empty pair", v: values.NewCons(values.NewInteger(1), values.EmptyList), want: false},
		{name: "integer", v: values.NewInteger(0), want: false},
		{name: "void", v: values.Void, want: false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(values.IsEmptyList(tc.v), qt.Equals, tc.want)
		})
	}
}

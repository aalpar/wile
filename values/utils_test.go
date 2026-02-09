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

package values

import (
	"context"
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"
)

func Test_List(t *testing.T) {
	tcs := []struct {
		in     Tuple
		out    Tuple
		expect bool
	}{
		{
			in:     List(),
			out:    EmptyList,
			expect: true,
		},
		{
			in:     List(nil),
			out:    NewCons(nil, EmptyList),
			expect: true,
		},
		{
			in:     List(NewSymbol("first")),
			out:    NewCons(NewSymbol("first"), EmptyList),
			expect: true,
		},
		{
			in:     List(NewSymbol("first"), NewSymbol("second")),
			out:    NewCons(NewSymbol("first"), NewCons(NewSymbol("second"), EmptyList)),
			expect: true,
		},
	}

	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%d", i), func(t *testing.T) {
			q := tc.in.EqualTo(tc.out)
			qt.Assert(t, q, qt.Equals, tc.expect)
		})
	}
}

func Test_FlipVectorToList(t *testing.T) {
	tcs := []struct {
		name string
		in   *Vector
		out  Tuple
	}{
		{
			name: "nil vector returns empty list",
			in:   nil,
			out:  EmptyList,
		},
		{
			name: "empty vector returns empty list",
			in:   NewVector(),
			out:  EmptyList,
		},
		{
			name: "single element vector",
			in:   NewVector(NewInteger(1)),
			out:  List(NewInteger(1)),
		},
		{
			name: "two element vector",
			in:   NewVector(NewInteger(1), NewInteger(2)),
			out:  List(NewInteger(1), NewInteger(2)),
		},
		{
			name: "three element vector",
			in:   NewVector(NewInteger(1), NewInteger(2), NewInteger(3)),
			out:  List(NewInteger(1), NewInteger(2), NewInteger(3)),
		},
		{
			name: "mixed types",
			in:   NewVector(NewInteger(1), NewString("hello"), TrueValue),
			out:  List(NewInteger(1), NewString("hello"), TrueValue),
		},
		{
			name: "nested list as element",
			in:   NewVector(List(NewInteger(1), NewInteger(2)), NewInteger(3)),
			out:  List(List(NewInteger(1), NewInteger(2)), NewInteger(3)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := VectorToList(tc.in)
			qt.Assert(t, got, SchemeEquals, tc.out)
		})
	}
}

func Test_ForEach(t *testing.T) {
	list := List(NewInteger(1), NewInteger(2), NewInteger(3))
	count := 0
	sum := int64(0)

	tail, err := ForEach(context.TODO(), list, func(_ context.Context, _ int, _ bool, v Value) error {
		count++
		intVal, ok := v.(*Integer)
		if ok {
			sum += intVal.Value
		}
		return nil
	})

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tail, SchemeEquals, EmptyList)
	qt.Assert(t, count, qt.Equals, 3)
	qt.Assert(t, sum, qt.Equals, int64(6))
}

func Test_ForEach_NonTuple(t *testing.T) {
	i := NewInteger(42)
	tail, err := ForEach(context.TODO(), i, func(_ context.Context, _ int, _ bool, _ Value) error {
		return fmt.Errorf("should not be called")
	})
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tail, SchemeEquals, i)
}

func Test_NewTemporaryVariableName(t *testing.T) {
	sym1 := NewTemporaryVariableName()
	sym2 := NewTemporaryVariableName()

	qt.Assert(t, sym1.Key[:4], qt.Equals, "__T_")
	qt.Assert(t, sym2.Key[:4], qt.Equals, "__T_")
	qt.Assert(t, sym1.Key, qt.Not(qt.Equals), sym2.Key)
}

func Test_IsList(t *testing.T) {
	tests := []struct {
		name string
		in   Value
		out  bool
	}{
		{"nil is not a list", nil, false},
		{"empty list is a list", EmptyList, true},
		{"proper list is a list", List(NewInteger(1), NewInteger(2)), true},
		{"improper list is not a list", NewCons(NewInteger(1), NewInteger(2)), false},
		{"arraylist proper list is a list", NewArrayList(NewInteger(1), NewInteger(2), EmptyList), true},
		{"arraylist improper list is not a list", NewArrayList(NewInteger(1), NewInteger(2)), false},
		{"integer is not a list", NewInteger(42), false},
		{"string is not a list", NewString("hello"), false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, IsList(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestEqualTo_CircularPair(t *testing.T) {
	c := qt.New(t)

	// Self-referential pair: x = (1 . x)
	x := NewCons(NewInteger(1), EmptyList)
	x.SetCdr(x)
	c.Assert(EqualTo(x, x), qt.IsTrue)

	// Two structurally identical circular pairs: a = (1 . a), b = (1 . b)
	a := NewCons(NewInteger(1), EmptyList)
	a.SetCdr(a)
	b := NewCons(NewInteger(1), EmptyList)
	b.SetCdr(b)
	c.Assert(EqualTo(a, b), qt.IsTrue)

	// Different circular pairs: c = (1 . c), d = (2 . d)
	d := NewCons(NewInteger(1), EmptyList)
	d.SetCdr(d)
	e := NewCons(NewInteger(2), EmptyList)
	e.SetCdr(e)
	c.Assert(EqualTo(d, e), qt.IsFalse)

	// Circular list: (1 2 1 2 ...) vs itself
	p1 := NewCons(NewInteger(1), NewCons(NewInteger(2), EmptyList))
	p1.Cdr().(*Pair).SetCdr(p1)
	p2 := NewCons(NewInteger(1), NewCons(NewInteger(2), EmptyList))
	p2.Cdr().(*Pair).SetCdr(p2)
	c.Assert(EqualTo(p1, p2), qt.IsTrue)
}

func TestEqualTo_CircularVector(t *testing.T) {
	c := qt.New(t)

	// Vector containing itself: #(v)
	v := NewVector(NewInteger(0))
	v.Set(0, v)
	c.Assert(EqualTo(v, v), qt.IsTrue)

	// Two vectors each containing themselves
	v1 := NewVector(NewInteger(0))
	v1.Set(0, v1)
	v2 := NewVector(NewInteger(0))
	v2.Set(0, v2)
	c.Assert(EqualTo(v1, v2), qt.IsTrue)
}

func TestEqualTo_NonCircular(t *testing.T) {
	c := qt.New(t)

	// Ensure non-circular comparisons still work
	c.Assert(EqualTo(List(NewInteger(1), NewInteger(2)), List(NewInteger(1), NewInteger(2))), qt.IsTrue)
	c.Assert(EqualTo(List(NewInteger(1)), List(NewInteger(2))), qt.IsFalse)
	c.Assert(EqualTo(NewVector(NewInteger(1)), NewVector(NewInteger(1))), qt.IsTrue)
	c.Assert(EqualTo(NewVector(NewInteger(1)), NewVector(NewInteger(2))), qt.IsFalse)
	c.Assert(EqualTo(NewInteger(42), NewInteger(42)), qt.IsTrue)
	c.Assert(EqualTo(nil, nil), qt.IsTrue)
}

func Test_ExactInteger(t *testing.T) {
	tests := []struct {
		name   string
		in     Value
		want   int64
		wantOk bool
	}{
		// Integer cases
		{"positive integer", NewInteger(42), 42, true},
		{"zero integer", NewInteger(0), 0, true},
		{"negative integer", NewInteger(-5), -5, true},
		{"max int64", NewInteger(9223372036854775807), 9223372036854775807, true},

		// BigInteger cases
		{"small bigint", NewBigIntegerFromInt64(100), 100, true},
		{"bigint at int64 max", NewBigIntegerFromString("9223372036854775807", 10), 9223372036854775807, true},
		{"bigint too large", NewBigIntegerFromString("9223372036854775808", 10), 0, false},

		// Rational cases - integers
		{"rational 2/1", NewRational(2, 1), 2, true},
		{"rational 0/1", NewRational(0, 1), 0, true},
		{"rational -3/1", NewRational(-3, 1), -3, true},

		// Rational cases - non-integers
		{"rational 1/2", NewRational(1, 2), 0, false},
		{"rational 5/3", NewRational(5, 3), 0, false},

		// Non-numeric types
		{"float", NewFloat(2.0), 0, false},
		{"string", NewString("2"), 0, false},
		{"symbol", NewSymbol("x"), 0, false},
		{"nil", nil, 0, false},
		{"pair", NewCons(NewInteger(1), EmptyList), 0, false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, ok := ExactInteger(tc.in)
			qt.Assert(t, ok, qt.Equals, tc.wantOk)
			if tc.wantOk {
				qt.Assert(t, got, qt.Equals, tc.want)
			}
		})
	}
}

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
	"context"
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func Test_List(t *testing.T) {
	tcs := []struct {
		in     values.Tuple
		out    values.Tuple
		expect bool
	}{
		{
			in:     values.List(),
			out:    values.EmptyList,
			expect: true,
		},
		{
			in:     values.List(nil),
			out:    values.NewCons(nil, values.EmptyList),
			expect: true,
		},
		{
			in:     values.List(values.NewSymbol("first")),
			out:    values.NewCons(values.NewSymbol("first"), values.EmptyList),
			expect: true,
		},
		{
			in:     values.List(values.NewSymbol("first"), values.NewSymbol("second")),
			out:    values.NewCons(values.NewSymbol("first"), values.NewCons(values.NewSymbol("second"), values.EmptyList)),
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

func Test_List_MutationSemantics(t *testing.T) {
	// Block-allocated pairs must support set-car!/set-cdr! identically
	// to individually allocated pairs.
	c := qt.New(t)

	lst := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	p := lst.(*values.Pair)

	// set-car! on first cell
	p.SetCar(values.NewInteger(99))
	c.Assert(p.Car(), valuestest.SchemeEquals, values.NewInteger(99))

	// set-cdr! to truncate list
	p.SetCdr(values.EmptyList)
	c.Assert(p.Cdr(), valuestest.SchemeEquals, values.EmptyList)

	// Remaining cells are independent — mutating the head didn't corrupt them
	lst2 := values.List(values.NewInteger(10), values.NewInteger(20))
	p2 := lst2.(*values.Pair)
	second := p2.Cdr().(*values.Pair)
	second.SetCar(values.NewInteger(42))
	c.Assert(second.Car(), valuestest.SchemeEquals, values.NewInteger(42))
	// First cell unchanged
	c.Assert(p2.Car(), valuestest.SchemeEquals, values.NewInteger(10))
}

func Test_FlipVectorToList(t *testing.T) {
	tcs := []struct {
		name string
		in   *values.Vector
		out  values.Tuple
	}{
		{
			name: "nil vector returns empty list",
			in:   nil,
			out:  values.EmptyList,
		},
		{
			name: "empty vector returns empty list",
			in:   values.NewVector(),
			out:  values.EmptyList,
		},
		{
			name: "single element vector",
			in:   values.NewVector(values.NewInteger(1)),
			out:  values.List(values.NewInteger(1)),
		},
		{
			name: "two element vector",
			in:   values.NewVector(values.NewInteger(1), values.NewInteger(2)),
			out:  values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "three element vector",
			in:   values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "mixed types",
			in:   values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
			out:  values.List(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
		},
		{
			name: "nested list as element",
			in:   values.NewVector(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)),
			out:  values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := values.VectorToList(tc.in)
			qt.Assert(t, got, valuestest.SchemeEquals, tc.out)
		})
	}
}

func Test_ForEach(t *testing.T) {
	list := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	count := 0
	sum := int64(0)

	tail, err := values.ForEach(context.TODO(), list, func(_ context.Context, _ int, _ bool, v values.Value) error {
		count++
		intVal, ok := v.(*values.Integer)
		if ok {
			sum += intVal.Value
		}
		return nil
	})

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tail, valuestest.SchemeEquals, values.EmptyList)
	qt.Assert(t, count, qt.Equals, 3)
	qt.Assert(t, sum, qt.Equals, int64(6))
}

func Test_ForEach_NonTuple(t *testing.T) {
	i := values.NewInteger(42)
	tail, err := values.ForEach(context.TODO(), i, func(_ context.Context, _ int, _ bool, _ values.Value) error {
		return fmt.Errorf("should not be called")
	})
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tail, valuestest.SchemeEquals, i)
}

func Test_NewTemporaryVariableName(t *testing.T) {
	sym1 := values.NewTemporaryVariableName()
	sym2 := values.NewTemporaryVariableName()

	qt.Assert(t, sym1.Key[:4], qt.Equals, "__T_")
	qt.Assert(t, sym2.Key[:4], qt.Equals, "__T_")
	qt.Assert(t, sym1.Key, qt.Not(qt.Equals), sym2.Key)
}

func Test_NewTemporaryVariableName_Uniqueness(t *testing.T) {
	c := qt.New(t)
	seen := make(map[string]bool)

	// Generate 1000 names rapidly to test PRNG initialization and uniqueness
	for range 1000 {
		name := values.NewTemporaryVariableName()
		if seen[name.Key] {
			c.Fatalf("duplicate name generated: %s", name.Key)
		}
		seen[name.Key] = true

		// Verify format
		c.Assert(name.Key[:4], qt.Equals, "__T_")
	}
}

func Test_IsList(t *testing.T) {
	tests := []struct {
		name string
		in   values.Value
		out  bool
	}{
		{"nil is not a list", nil, false},
		{"empty list is a list", values.EmptyList, true},
		{"proper list is a list", values.List(values.NewInteger(1), values.NewInteger(2)), true},
		{"improper list is not a list", values.NewCons(values.NewInteger(1), values.NewInteger(2)), false},
		{"integer is not a list", values.NewInteger(42), false},
		{"string is not a list", values.NewString("hello"), false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.IsList(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestEqualTo_CircularPair(t *testing.T) {
	c := qt.New(t)

	// Self-referential pair: x = (1 . x)
	x := values.NewCons(values.NewInteger(1), values.EmptyList)
	x.SetCdr(x)
	c.Assert(values.EqualTo(x, x), qt.IsTrue)

	// Two structurally identical circular pairs: a = (1 . a), b = (1 . b)
	a := values.NewCons(values.NewInteger(1), values.EmptyList)
	a.SetCdr(a)
	b := values.NewCons(values.NewInteger(1), values.EmptyList)
	b.SetCdr(b)
	c.Assert(values.EqualTo(a, b), qt.IsTrue)

	// Different circular pairs: c = (1 . c), d = (2 . d)
	d := values.NewCons(values.NewInteger(1), values.EmptyList)
	d.SetCdr(d)
	e := values.NewCons(values.NewInteger(2), values.EmptyList)
	e.SetCdr(e)
	c.Assert(values.EqualTo(d, e), qt.IsFalse)

	// Circular list: (1 2 1 2 ...) vs itself
	p1 := values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList))
	p1.Cdr().(*values.Pair).SetCdr(p1)
	p2 := values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList))
	p2.Cdr().(*values.Pair).SetCdr(p2)
	c.Assert(values.EqualTo(p1, p2), qt.IsTrue)
}

func TestEqualTo_CircularVector(t *testing.T) {
	c := qt.New(t)

	// Vector containing itself: #(v)
	v := values.NewVector(values.NewInteger(0))
	v.Set(0, v)
	c.Assert(values.EqualTo(v, v), qt.IsTrue)

	// Two vectors each containing themselves
	v1 := values.NewVector(values.NewInteger(0))
	v1.Set(0, v1)
	v2 := values.NewVector(values.NewInteger(0))
	v2.Set(0, v2)
	c.Assert(values.EqualTo(v1, v2), qt.IsTrue)
}

func TestEqualTo_NonCircular(t *testing.T) {
	c := qt.New(t)

	// Ensure non-circular comparisons still work
	c.Assert(values.EqualTo(values.List(values.NewInteger(1), values.NewInteger(2)), values.List(values.NewInteger(1), values.NewInteger(2))), qt.IsTrue)
	c.Assert(values.EqualTo(values.List(values.NewInteger(1)), values.List(values.NewInteger(2))), qt.IsFalse)
	c.Assert(values.EqualTo(values.NewVector(values.NewInteger(1)), values.NewVector(values.NewInteger(1))), qt.IsTrue)
	c.Assert(values.EqualTo(values.NewVector(values.NewInteger(1)), values.NewVector(values.NewInteger(2))), qt.IsFalse)
	c.Assert(values.EqualTo(values.NewInteger(42), values.NewInteger(42)), qt.IsTrue)
	c.Assert(values.EqualTo(nil, nil), qt.IsTrue)
}

func Test_ExactInteger(t *testing.T) {
	tests := []struct {
		name   string
		in     values.Value
		want   int64
		wantOk bool
	}{
		// Integer cases
		{"positive integer", values.NewInteger(42), 42, true},
		{"zero integer", values.NewInteger(0), 0, true},
		{"negative integer", values.NewInteger(-5), -5, true},
		{"max int64", values.NewInteger(9223372036854775807), 9223372036854775807, true},

		// BigInteger cases
		{"small bigint", values.NewBigIntegerFromInt64(100), 100, true},
		{"bigint at int64 max", values.NewBigIntegerFromString("9223372036854775807", 10), 9223372036854775807, true},
		{"bigint too large", values.NewBigIntegerFromString("9223372036854775808", 10), 0, false},

		// Rational cases - integers
		{"rational 2/1", values.NewRational(2, 1), 2, true},
		{"rational 0/1", values.NewRational(0, 1), 0, true},
		{"rational -3/1", values.NewRational(-3, 1), -3, true},

		// Rational cases - non-integers
		{"rational 1/2", values.NewRational(1, 2), 0, false},
		{"rational 5/3", values.NewRational(5, 3), 0, false},

		// Non-numeric types
		{"float", values.NewFloat(2.0), 0, false},
		{"string", values.NewString("2"), 0, false},
		{"symbol", values.NewSymbol("x"), 0, false},
		{"nil", nil, 0, false},
		{"pair", values.NewCons(values.NewInteger(1), values.EmptyList), 0, false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, ok := values.ExactInteger(tc.in)
			qt.Assert(t, ok, qt.Equals, tc.wantOk)
			if tc.wantOk {
				qt.Assert(t, got, qt.Equals, tc.want)
			}
		})
	}
}

func intSet(vs ...int) values.IntSet {
	q := values.NewIntSet(len(vs))
	q.SetAll(vs...)
	return q
}

func Test_MapSet_Membership(t *testing.T) {
	s := values.NewStringSet(0)
	qt.Assert(t, s.Len(), qt.Equals, 0)
	qt.Assert(t, s.ContainsOne("a"), qt.IsFalse)
	qt.Assert(t, s.ContainsAll(), qt.IsTrue, qt.Commentf("vacuous truth"))

	s.Set("a")
	s.SetAll("b", "c", "a")
	qt.Assert(t, s.Len(), qt.Equals, 3, qt.Commentf("re-adding is idempotent"))
	qt.Assert(t, s.ContainsAll("a", "b", "c"), qt.IsTrue)
	qt.Assert(t, s.ContainsAll("a", "z"), qt.IsFalse)

	s.Unset("z")
	s.UnsetAll("a", "c", "z")
	qt.Assert(t, s, qt.DeepEquals, values.StringSet{"b": {}})
}

// A nil MapSet is a valid empty set for every read and every removal; only
// insertion needs an allocated map.
func Test_MapSet_NilReads(t *testing.T) {
	var s values.StringSet
	qt.Assert(t, s.Len(), qt.Equals, 0)
	qt.Assert(t, s.ContainsOne("a"), qt.IsFalse)
	qt.Assert(t, s.ContainsAll("a"), qt.IsFalse)
	qt.Assert(t, s.ContainsAll(), qt.IsTrue)
	s.Unset("a")
	s.UnsetAll("a", "b")
	qt.Assert(t, s, qt.IsNil)
}

func Test_MapSet_Algebra(t *testing.T) {
	tests := []struct {
		name   string
		op     func(values.IntSet, ...values.IntSet) values.IntSet
		set    values.IntSet
		others []values.IntSet
		want   values.IntSet
	}{
		{"intersection: no others is a copy", values.Intersection[int], intSet(1, 2), nil, intSet(1, 2)},
		{"intersection: keeps elements common to all", values.Intersection[int], intSet(1, 2, 3), []values.IntSet{intSet(2, 3, 4), intSet(3, 2)}, intSet(2, 3)},
		{"intersection: disjoint", values.Intersection[int], intSet(1), []values.IntSet{intSet(2)}, intSet()},
		{"intersection: nil set", values.Intersection[int], nil, []values.IntSet{intSet(1)}, intSet()},
		{"intersection: nil other", values.Intersection[int], intSet(1), []values.IntSet{nil}, intSet()},

		{"union: no others is a copy", values.Union[int], intSet(1, 2), nil, intSet(1, 2)},
		{"union: merges all", values.Union[int], intSet(1), []values.IntSet{intSet(2), intSet(3, 1)}, intSet(1, 2, 3)},
		{"union: nil set", values.Union[int], nil, []values.IntSet{intSet(1)}, intSet(1)},
		{"union: nil other", values.Union[int], intSet(1), []values.IntSet{nil}, intSet(1)},
		{"union: all nil", values.Union[int], nil, []values.IntSet{nil}, intSet()},

		{"difference: no others is a copy", values.Difference[int], intSet(1, 2), nil, intSet(1, 2)},
		{"difference: removes elements in any other", values.Difference[int], intSet(1, 2, 3), []values.IntSet{intSet(2), intSet(3, 9)}, intSet(1)},
		{"difference: nil set", values.Difference[int], nil, []values.IntSet{intSet(1)}, intSet()},
		{"difference: nil other", values.Difference[int], intSet(1), []values.IntSet{nil}, intSet(1)},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.op(tc.set, tc.others...)
			// DeepEquals distinguishes a nil map from an empty one, so this also
			// pins "the result is always an allocated, writable set".
			qt.Assert(t, got, qt.DeepEquals, tc.want)
			got.Set(99)
			qt.Assert(t, got.ContainsOne(99), qt.IsTrue)
		})
	}
}

// The algebra returns a fresh set: neither the receiver nor any operand is
// aliased, so mutating the result leaves the inputs untouched.
func Test_MapSet_AlgebraDoesNotAliasInputs(t *testing.T) {
	a := intSet(1)
	b := intSet(2)
	results := []struct {
		name string
		got  values.IntSet
	}{
		{"intersection", values.Intersection(a, a)},
		{"union", values.Union(a, b)},
		{"difference", values.Difference(a, b)},
	}
	for _, r := range results {
		r.got.Set(99)
		r.got.Unset(1)
		qt.Assert(t, a, qt.DeepEquals, intSet(1), qt.Commentf("%s aliased its receiver", r.name))
		qt.Assert(t, b, qt.DeepEquals, intSet(2), qt.Commentf("%s aliased an operand", r.name))
	}
}

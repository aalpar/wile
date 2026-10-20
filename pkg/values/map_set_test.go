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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

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

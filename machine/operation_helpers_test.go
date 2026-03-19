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

package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestSameType(t *testing.T) {
	type dummy struct{}

	tcs := []struct {
		name string
		p    *dummy
		v    *dummy
		ok   bool
		want bool
	}{
		{
			name: "same non-nil pointers",
			p: func() *dummy {
				d := &dummy{}
				return d
			}(),
			v: func() *dummy {
				d := &dummy{}
				return d
			}(),
			ok:   true,
			want: true,
		},
		{
			name: "ok is false (type assertion failed)",
			p:    &dummy{},
			v:    nil,
			ok:   false,
			want: false,
		},
		{
			name: "both nil pointers",
			p:    nil,
			v:    nil,
			ok:   true,
			want: true,
		},
		{
			name: "p nil v non-nil",
			p:    nil,
			v:    &dummy{},
			ok:   true,
			want: false,
		},
		{
			name: "p non-nil v nil",
			p:    &dummy{},
			v:    nil,
			ok:   true,
			want: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := sameType(tc.p, tc.v, tc.ok)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

func TestFieldMatches(t *testing.T) {
	type pair struct {
		val int
	}

	getter := func(p *pair) int {
		return p.val
	}

	tcs := []struct {
		name string
		p    *pair
		v    *pair
		ok   bool
		want bool
	}{
		{
			name: "same field values",
			p:    &pair{val: 42},
			v:    &pair{val: 42},
			ok:   true,
			want: true,
		},
		{
			name: "different field values",
			p:    &pair{val: 1},
			v:    &pair{val: 2},
			ok:   true,
			want: false,
		},
		{
			name: "ok is false",
			p:    &pair{val: 1},
			v:    nil,
			ok:   false,
			want: false,
		},
		{
			name: "both nil",
			p:    nil,
			v:    nil,
			ok:   true,
			want: true,
		},
		{
			name: "p nil v non-nil",
			p:    nil,
			v:    &pair{val: 1},
			ok:   true,
			want: false,
		},
		{
			name: "p non-nil v nil",
			p:    &pair{val: 1},
			v:    nil,
			ok:   true,
			want: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := fieldMatches(tc.p, tc.v, tc.ok, getter)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

func TestFieldMethodMatches(t *testing.T) {
	type wrapper struct {
		s string
	}

	getter := func(p *wrapper) string {
		return p.s
	}
	eq := func(a, b string) bool {
		return a == b
	}

	tcs := []struct {
		name string
		p    *wrapper
		v    *wrapper
		ok   bool
		want bool
	}{
		{
			name: "matching fields via method",
			p:    &wrapper{s: "hello"},
			v:    &wrapper{s: "hello"},
			ok:   true,
			want: true,
		},
		{
			name: "non-matching fields via method",
			p:    &wrapper{s: "hello"},
			v:    &wrapper{s: "world"},
			ok:   true,
			want: false,
		},
		{
			name: "ok is false",
			p:    &wrapper{s: "x"},
			v:    nil,
			ok:   false,
			want: false,
		},
		{
			name: "both nil",
			p:    nil,
			v:    nil,
			ok:   true,
			want: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := fieldMethodMatches(tc.p, tc.v, tc.ok, getter, eq)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

func TestSliceMatches(t *testing.T) {
	type bag struct {
		items []int
	}

	getter := func(p *bag) []int {
		return p.items
	}

	tcs := []struct {
		name string
		p    *bag
		v    *bag
		ok   bool
		want bool
	}{
		{
			name: "equal slices",
			p:    &bag{items: []int{1, 2, 3}},
			v:    &bag{items: []int{1, 2, 3}},
			ok:   true,
			want: true,
		},
		{
			name: "different slices",
			p:    &bag{items: []int{1, 2}},
			v:    &bag{items: []int{3, 4}},
			ok:   true,
			want: false,
		},
		{
			name: "different lengths",
			p:    &bag{items: []int{1}},
			v:    &bag{items: []int{1, 2}},
			ok:   true,
			want: false,
		},
		{
			name: "both empty slices",
			p:    &bag{items: []int{}},
			v:    &bag{items: []int{}},
			ok:   true,
			want: true,
		},
		{
			name: "both nil slices",
			p:    &bag{items: nil},
			v:    &bag{items: nil},
			ok:   true,
			want: true,
		},
		{
			name: "ok is false",
			p:    &bag{items: []int{1}},
			v:    nil,
			ok:   false,
			want: false,
		},
		{
			name: "both pointers nil",
			p:    nil,
			v:    nil,
			ok:   true,
			want: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := sliceMatches(tc.p, tc.v, tc.ok, getter)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

func TestOperationBase(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "NewOperationBase SchemeString",
			checkFn: func(t *testing.T) {
				base := NewOperationBase("my-op")
				qt.Assert(t, base.SchemeString(), qt.Equals, "#<my-op>")
			},
		},
		{
			name: "NewOperationBase IsVoid false",
			checkFn: func(t *testing.T) {
				base := NewOperationBase("x")
				qt.Assert(t, base.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "NewOperationBase String uses opName when no goName",
			checkFn: func(t *testing.T) {
				base := NewOperationBase("the-op")
				qt.Assert(t, base.String(), qt.Equals, "the-op")
			},
		},
		{
			name: "NewOperationBaseWithGoName String uses goName",
			checkFn: func(t *testing.T) {
				base := NewOperationBaseWithGoName("scheme-name", "GoName")
				qt.Assert(t, base.String(), qt.Equals, "GoName")
				qt.Assert(t, base.SchemeString(), qt.Equals, "#<scheme-name>")
			},
		},
		{
			name: "NewOperationBaseWithGoName IsVoid false",
			checkFn: func(t *testing.T) {
				base := NewOperationBaseWithGoName("a", "B")
				qt.Assert(t, base.IsVoid(), qt.IsFalse)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}

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

func TestDynamicWindFrame(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "NewDynamicWindFrame assigns unique IDs",
			checkFn: func(t *testing.T) {
				a := NewDynamicWindFrame(nil, nil)
				b := NewDynamicWindFrame(nil, nil)
				qt.Assert(t, a.ID != b.ID, qt.IsTrue)
			},
		},
		{
			name: "NewDynamicWindFrame stores before and after closures",
			checkFn: func(t *testing.T) {
				before := &MachineClosure{}
				after := &MachineClosure{}
				frame := NewDynamicWindFrame(before, after)
				qt.Assert(t, frame.Before, qt.Equals, before)
				qt.Assert(t, frame.After, qt.Equals, after)
			},
		},
		{
			name: "NewDynamicWindFrame IDs are monotonically increasing",
			checkFn: func(t *testing.T) {
				a := NewDynamicWindFrame(nil, nil)
				b := NewDynamicWindFrame(nil, nil)
				qt.Assert(t, b.ID > a.ID, qt.IsTrue)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}

func TestWindingStack(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "empty stack has Depth zero",
			checkFn: func(t *testing.T) {
				var ws WindingStack
				qt.Assert(t, ws.Depth(), qt.Equals, 0)
			},
		},
		{
			name: "Push increases Depth",
			checkFn: func(t *testing.T) {
				var ws WindingStack
				ws.Push(NewDynamicWindFrame(nil, nil))
				qt.Assert(t, ws.Depth(), qt.Equals, 1)
				ws.Push(NewDynamicWindFrame(nil, nil))
				qt.Assert(t, ws.Depth(), qt.Equals, 2)
			},
		},
		{
			name: "Pop returns innermost frame (LIFO)",
			checkFn: func(t *testing.T) {
				var ws WindingStack
				f1 := NewDynamicWindFrame(nil, nil)
				f2 := NewDynamicWindFrame(nil, nil)
				ws.Push(f1)
				ws.Push(f2)

				popped := ws.Pop()
				qt.Assert(t, popped, qt.Equals, f2)
				qt.Assert(t, ws.Depth(), qt.Equals, 1)

				popped = ws.Pop()
				qt.Assert(t, popped, qt.Equals, f1)
				qt.Assert(t, ws.Depth(), qt.Equals, 0)
			},
		},
		{
			name: "Pop on empty stack returns nil",
			checkFn: func(t *testing.T) {
				var ws WindingStack
				qt.Assert(t, ws.Pop(), qt.IsNil)
			},
		},
		{
			name: "Copy returns independent stack",
			checkFn: func(t *testing.T) {
				var ws WindingStack
				f1 := NewDynamicWindFrame(nil, nil)
				f2 := NewDynamicWindFrame(nil, nil)
				ws.Push(f1)
				ws.Push(f2)

				copied := ws.Copy()
				qt.Assert(t, copied.Depth(), qt.Equals, 2)

				// Mutating original does not affect copy
				ws.Pop()
				qt.Assert(t, ws.Depth(), qt.Equals, 1)
				qt.Assert(t, copied.Depth(), qt.Equals, 2)

				// Mutating copy does not affect original
				copied.Push(NewDynamicWindFrame(nil, nil))
				qt.Assert(t, copied.Depth(), qt.Equals, 3)
				qt.Assert(t, ws.Depth(), qt.Equals, 1)
			},
		},
		{
			name: "Copy of empty stack",
			checkFn: func(t *testing.T) {
				var ws WindingStack
				copied := ws.Copy()
				qt.Assert(t, copied.Depth(), qt.Equals, 0)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}

func TestFindCommonWindingPrefix(t *testing.T) {
	// Pre-create frames with known IDs
	f1 := NewDynamicWindFrame(nil, nil)
	f2 := NewDynamicWindFrame(nil, nil)
	f3 := NewDynamicWindFrame(nil, nil)
	f4 := NewDynamicWindFrame(nil, nil)

	tcs := []struct {
		name    string
		current WindingStack
		target  WindingStack
		want    int
	}{
		{
			name:    "both empty",
			current: WindingStack{},
			target:  WindingStack{},
			want:    0,
		},
		{
			name:    "current empty",
			current: WindingStack{},
			target:  WindingStack{f1, f2},
			want:    0,
		},
		{
			name:    "target empty",
			current: WindingStack{f1, f2},
			target:  WindingStack{},
			want:    0,
		},
		{
			name:    "identical stacks",
			current: WindingStack{f1, f2, f3},
			target:  WindingStack{f1, f2, f3},
			want:    3,
		},
		{
			name:    "shared prefix then diverge",
			current: WindingStack{f1, f2, f3},
			target:  WindingStack{f1, f2, f4},
			want:    2,
		},
		{
			name:    "no common prefix (disjoint)",
			current: WindingStack{f1, f2},
			target:  WindingStack{f3, f4},
			want:    0,
		},
		{
			name:    "current is prefix of target",
			current: WindingStack{f1, f2},
			target:  WindingStack{f1, f2, f3},
			want:    2,
		},
		{
			name:    "target is prefix of current",
			current: WindingStack{f1, f2, f3},
			target:  WindingStack{f1, f2},
			want:    2,
		},
		{
			name:    "single shared frame",
			current: WindingStack{f1},
			target:  WindingStack{f1},
			want:    1,
		},
		{
			name:    "single frame no match",
			current: WindingStack{f1},
			target:  WindingStack{f2},
			want:    0,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := FindCommonWindingPrefix(tc.current, tc.target)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

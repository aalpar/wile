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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

func TestMarkLookup(t *testing.T) {
	c := qt.New(t)
	k1 := values.NewSymbol("k1")
	k2 := values.NewSymbol("k2")
	absent := values.NewSymbol("absent")
	ms := []markEntry{{key: k1, val: values.NewInteger(1)}, {key: k2, val: values.NewInteger(2)}}

	c.Assert(markIndex(ms, k1), qt.Equals, 0)
	c.Assert(markIndex(ms, values.NewSymbol("k2")), qt.Equals, 1) // eq? on symbols is by key
	c.Assert(markIndex(ms, absent), qt.Equals, -1)
	c.Assert(markIndex(nil, absent), qt.Equals, -1)

	c.Assert(lookupMark(ms, k2), qt.Equals, values.NewInteger(2))
	c.Assert(lookupMark(ms, absent), qt.IsNil)
	c.Assert(lookupMark(nil, absent), qt.IsNil)
}

// newReachableMarksFixture builds a sub-context whose reachable mark frames are,
// nearest first: its live marks (key=1), one saved frame (key=2), then EITHER its
// capture-time snapshot (key=4, when isolated) OR the parent's live marks (key=3).
// The parent's own saved frame carries no marks. other is set only on the parent.
func newReachableMarksFixture(key, other values.Value, isolated bool) *MachineContext {
	env := environment.NewNamespace().Runtime()
	parent := newContMarkTestContext()
	parent.SetMark(key, values.NewInteger(3))
	parent.SetMark(other, values.NewInteger(30))

	sub := parent.NewSubContext()
	sub.cont = NewMachineContinuation(nil, namedTemplate("saved"), env)
	sub.cont.marks = []markEntry{{key: key, val: values.NewInteger(2)}}
	sub.SetMark(key, values.NewInteger(1))
	sub.capturedMarks = []markEntry{{key: key, val: values.NewInteger(4)}}
	sub.isolatedMarks = isolated
	return sub
}

// reachableMarkValues renders key's value in each yielded frame, "-" where the
// frame lacks it, so the expected slice shows exactly which frames were visited.
func reachableMarkValues(mc *MachineContext, key values.Value) []string {
	var q []string
	mc.forEachReachableMarkFrame(func(frame []markEntry) bool {
		v := lookupMark(frame, key)
		if v == nil {
			q = append(q, "-")
			return true
		}
		q = append(q, v.SchemeString())
		return true
	})
	return q
}

func TestForEachReachableMarkFrame_Order(t *testing.T) {
	tcs := []struct {
		name     string
		isolated bool
		want     []string
	}{
		{name: "hops parentMC, nearest first", isolated: false, want: []string{"1", "2", "3"}},
		{name: "isolated: snapshot replaces the parent hop", isolated: true, want: []string{"1", "2", "4"}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			key := values.NewSymbol("k")
			mc := newReachableMarksFixture(key, values.NewSymbol("other"), tc.isolated)
			c.Assert(reachableMarkValues(mc, key), qt.DeepEquals, tc.want)
		})
	}
}

func TestForEachReachableMarkFrame_StopsWhenVisitReturnsFalse(t *testing.T) {
	c := qt.New(t)
	key := values.NewSymbol("k")
	mc := newReachableMarksFixture(key, values.NewSymbol("other"), false)
	visited := 0
	mc.forEachReachableMarkFrame(func([]markEntry) bool {
		visited++
		return false
	})
	c.Assert(visited, qt.Equals, 1)
}

func TestFindParameterInMarks_NearestWins(t *testing.T) {
	c := qt.New(t)
	param := &Parameter{}
	other := &Parameter{}
	mc := newReachableMarksFixture(param, other, false)
	c.Assert(mc.findParameterInMarks(param), qt.Equals, values.NewInteger(1))
	c.Assert(mc.findParameterInMarks(other), qt.Equals, values.NewInteger(30))
	c.Assert(mc.findParameterInMarks(&Parameter{}), qt.IsNil)
}

// The walk runs on every parameter read and every raise (ResolveParameterValue),
// so it must not allocate: the walker takes a non-escaping callback rather than
// returning an iter.Seq closure.
func TestFindParameterInMarks_NoAlloc(t *testing.T) {
	c := qt.New(t)
	param := &Parameter{}
	mc := newReachableMarksFixture(param, &Parameter{}, false)
	absent := &Parameter{}
	allocs := testing.AllocsPerRun(100, func() {
		mc.findParameterInMarks(param)
		mc.findParameterInMarks(absent)
	})
	c.Assert(allocs, qt.Equals, 0.0)
}

func TestCollectReachableMarks_NearestWinsPerKey(t *testing.T) {
	tcs := []struct {
		name     string
		isolated bool
		want     string
	}{
		{name: "parent key folded in", isolated: false, want: "((k . 1) (other . 30))"},
		{name: "isolated: parent key unreachable", isolated: true, want: "((k . 1))"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			key := values.NewSymbol("k")
			other := values.NewSymbol("other")
			mc := newReachableMarksFixture(key, other, tc.isolated)
			var pairs []values.Value
			for _, e := range mc.collectReachableMarks() {
				pairs = append(pairs, values.NewCons(e.key, e.val))
			}
			c.Assert(values.List(pairs...).SchemeString(), qt.Equals, tc.want)
		})
	}
}

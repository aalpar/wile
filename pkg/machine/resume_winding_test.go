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
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"

	qt "github.com/frankban/quicktest"
)

func newTestRoot(t *testing.T) *MachineContext {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	root := AcquireTopLevelContext(context.Background(), NewEmptyNativeTemplate(), env)
	t.Cleanup(func() {
		ReleaseTopLevelContext(root)
	})
	return root
}

// TestResumeWindingDepthDelimitsAtTheDriverAbove pins where a call/cc capture
// with no default-tag frame of its own is delimited: at the nearest driver above
// the sub-context (a runtime root, or a parentless expansion-time root), so the
// continuation carries the extents the sub-context inherited; and at the
// capturing context itself when it has a default prompt.
func TestResumeWindingDepthDelimitsAtTheDriverAbove(t *testing.T) {
	f1 := NewDynamicWindFrame(nil, nil)
	f2 := NewDynamicWindFrame(nil, nil)

	t.Run("a runtime root above", func(t *testing.T) {
		root := newTestRoot(t)
		root.promptTag = DefaultPromptTag
		root.windingStack = WindingStack{f1}
		sub := root.NewSubContextWithWinding(WindingStack{f1, f2})
		defer ReleaseSubContext(sub)
		nested := sub.NewSubContext()
		defer ReleaseSubContext(nested)

		_, found := sub.FindPrompt(DefaultPromptTag)
		qt.Assert(t, found, qt.IsFalse)
		qt.Assert(t, sub.ResumeWindingDepth(nil, found), qt.Equals, 0)
		qt.Assert(t, nested.ResumeWindingDepth(nil, found), qt.Equals, 0)
		// The root's own capture is delimited at its own boundary.
		_, found = root.FindPrompt(DefaultPromptTag)
		qt.Assert(t, found, qt.IsTrue)
		qt.Assert(t, root.ResumeWindingDepth(nil, found), qt.Equals, 0)
	})
	t.Run("a prompt frame on the driver above", func(t *testing.T) {
		// The capture is delimited at the frame, so only the extents entered
		// inside it are carried: f1, outside the frame, is not re-entered.
		root := newTestRoot(t)
		root.promptTag = DefaultPromptTag
		root.windingStack = WindingStack{f1, f2}
		saved := root.cont
		frame := NewMachineContinuationWithPrompt(saved, NewEmptyNativeTemplate(), nil, DefaultPromptTag, nil)
		frame.windingStack = WindingStack{f1}
		root.cont = frame
		defer func() {
			root.cont = saved
		}()
		sub := root.NewSubContextWithWinding(WindingStack{f1, f2})
		defer ReleaseSubContext(sub)

		_, found := sub.FindPrompt(DefaultPromptTag)
		qt.Assert(t, found, qt.IsFalse)
		qt.Assert(t, sub.ResumeWindingDepth(nil, found), qt.Equals, 1)
	})
	t.Run("a parentless expansion-time root above", func(t *testing.T) {
		root := newTestRoot(t)
		root.windingStack = WindingStack{f1}
		sub := root.NewSubContextWithWinding(WindingStack{f1, f2})
		defer ReleaseSubContext(sub)

		_, found := sub.FindPrompt(DefaultPromptTag)
		qt.Assert(t, found, qt.IsFalse)
		qt.Assert(t, sub.ResumeWindingDepth(nil, found), qt.Equals, 0)
		qt.Assert(t, root.ResumeWindingDepth(nil, false), qt.Equals, 0)
	})
	t.Run("a default prompt of its own", func(t *testing.T) {
		root := newTestRoot(t)
		sub := root.NewSubContextWithWinding(WindingStack{f1, f2})
		defer ReleaseSubContext(sub)
		qt.Assert(t, sub.ResumeWindingDepth(nil, true), qt.Equals, 2)
	})
}

// TestComposeWindingKeepsASharedExtentOnce pins the reconcile target of a
// reinstatement: the extents outside the boundary, then the carried ones; at a
// context's own boundary (shared) a carried extent the outside stack already
// holds is kept once.
func TestComposeWindingKeepsASharedExtentOnce(t *testing.T) {
	a := NewDynamicWindFrame(nil, nil)
	b := NewDynamicWindFrame(nil, nil)
	ids := func(ws WindingStack) []uint64 {
		q := make([]uint64, 0, len(ws))
		for _, f := range ws {
			q = append(q, f.ID)
		}
		return q
	}
	tcs := []struct {
		name             string
		outside, carried WindingStack
		shared           bool
		want             []uint64
	}{
		{"prompt frame: carried on top", WindingStack{a}, WindingStack{a, b}, false, []uint64{a.ID, a.ID, b.ID}},
		{"own boundary: shared extent once", WindingStack{a}, WindingStack{a, b}, true, []uint64{a.ID, b.ID}},
		{"own boundary, nothing outside", nil, WindingStack{a, b}, true, []uint64{a.ID, b.ID}},
		{"nothing carried", WindingStack{a}, nil, true, []uint64{a.ID}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, ids(composeWinding(tc.outside, tc.carried, tc.shared)), qt.DeepEquals, tc.want)
		})
	}
}

// TestWindingStackHolds pins the two membership queries the reconcile uses: an
// extent by ID, and a timer by the frame that carries it.
func TestWindingStackHolds(t *testing.T) {
	timer := &timerState{}
	a := NewDynamicWindFrame(nil, nil)
	tf := newTimerFrame(timer)
	ws := WindingStack{a, tf}
	qt.Assert(t, ws.holds(a.ID), qt.IsTrue)
	qt.Assert(t, ws.holds(tf.ID), qt.IsTrue)
	qt.Assert(t, ws.holds(a.ID+tf.ID+1), qt.IsFalse)
	qt.Assert(t, ws.holdsTimer(timer), qt.IsTrue)
	qt.Assert(t, ws[:1].holdsTimer(timer), qt.IsFalse)
	qt.Assert(t, ws.holdsTimer(&timerState{}), qt.IsFalse)
}

// TestCompleteRunLeavesOnlyOwnExtents pins a root's completion: the extents the
// context entered itself are left, the inherited ones are its parent's.
func TestCompleteRunLeavesOnlyOwnExtents(t *testing.T) {
	inherited := NewDynamicWindFrame(nil, nil)
	own := NewDynamicWindFrame(nil, nil)
	root := newTestRoot(t)
	sub := root.NewSubContextWithWinding(WindingStack{inherited})
	defer ReleaseSubContext(sub)

	qt.Assert(t, sub.completeRun(), qt.IsNil)
	qt.Assert(t, len(sub.windingStack), qt.Equals, 1)

	sub.PushWindingFrame(own)
	qt.Assert(t, sub.completeRun(), qt.IsNil)
	qt.Assert(t, len(sub.windingStack), qt.Equals, 1)
	qt.Assert(t, sub.windingStack[0].ID, qt.Equals, inherited.ID)
}

// TestResumePromptOwnership pins which driver a resume belongs to: a
// continuation delimited at a context's own boundary resumes there; a runtime
// root resolves whatever reaches it; a sub-context without the token passes it
// up; an expansion-time root resolves its own and a closed expansion-time root's
// continuations and never a runtime root's.
func TestResumePromptOwnership(t *testing.T) {
	capture := func(p *MachineContext) *ErrResumeContinuation {
		comp := NewComposableContinuation(nil, nil, 0, p.ThreadID(), p.BarrierValid())
		p.DelimitAt(comp, nil)
		return &ErrResumeContinuation{Tag: DefaultPromptTag, Segment: comp}
	}

	t.Run("runtime root and its sub-contexts", func(t *testing.T) {
		root := newTestRoot(t)
		root.promptTag = DefaultPromptTag
		sub := root.NewSubContext()
		defer ReleaseSubContext(sub)
		other := root.NewSubContext()
		defer ReleaseSubContext(other)

		fromSub := capture(sub)
		_, ok := sub.resumePrompt(fromSub)
		qt.Assert(t, ok, qt.IsTrue)
		_, ok = other.resumePrompt(fromSub)
		qt.Assert(t, ok, qt.IsFalse)
		_, ok = root.resumePrompt(fromSub)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, other.passUpResume(fromSub), qt.IsNil)
		qt.Assert(t, fromSub.rebaseWinding, qt.IsFalse)
	})
	t.Run("expansion-time root", func(t *testing.T) {
		runtime := newTestRoot(t)
		runtime.promptTag = DefaultPromptTag
		exp := newTestRoot(t)
		exp.expansionRoot = true
		expSub := exp.NewSubContext()
		defer ReleaseSubContext(expSub)
		earlier := newTestRoot(t)
		earlier.expansionRoot = true

		fromRuntime := capture(runtime)
		_, ok := exp.resumePrompt(fromRuntime)
		qt.Assert(t, ok, qt.IsFalse)
		// Not even at a default-tag frame on the expansion-time root's own chain
		// (a prompt the transformer installed): the runtime's remainder would run
		// at phase 1.
		saved := exp.cont
		exp.cont = NewMachineContinuationWithPrompt(saved, NewEmptyNativeTemplate(), nil, DefaultPromptTag, nil)
		_, ok = exp.resumePrompt(fromRuntime)
		exp.cont = saved
		qt.Assert(t, ok, qt.IsFalse)

		fromOwn := capture(expSub)
		_, ok = exp.resumePrompt(fromOwn)
		qt.Assert(t, ok, qt.IsTrue)

		fromEarlier := capture(earlier)
		_, ok = exp.resumePrompt(fromEarlier)
		qt.Assert(t, ok, qt.IsFalse)
		closeBoundary(earlier)
		_, ok = exp.resumePrompt(fromEarlier)
		qt.Assert(t, ok, qt.IsTrue)

		// Passing up leaves the root's own extents and marks the winding for the
		// driver above, which replaces it with its own.
		f := NewDynamicWindFrame(nil, nil)
		exp.windingStack = WindingStack{f}
		fromRuntime.SourceWinding = WindingStack{f}
		qt.Assert(t, exp.passUpResume(fromRuntime), qt.IsNil)
		qt.Assert(t, len(exp.windingStack), qt.Equals, 0)
		qt.Assert(t, fromRuntime.rebaseWinding, qt.IsTrue)
		runtime.windingStack = WindingStack{f}
		runtime.rebaseResumeWinding(fromRuntime)
		qt.Assert(t, fromRuntime.rebaseWinding, qt.IsFalse)
		qt.Assert(t, len(fromRuntime.SourceWinding), qt.Equals, 1)
	})
}

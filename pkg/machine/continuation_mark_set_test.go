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

	"github.com/aalpar/wile/pkg/values"
)

// Symbols for tests. markKeyEqual compares symbols by Key string, so
// freshly allocated symbols with the same name are equivalent keys.
var (
	testKeyK     = values.NewSymbol("k")
	testKeyA     = values.NewSymbol("a")
	testKeyB     = values.NewSymbol("b")
	testKeyOther = values.NewSymbol("other")
)

func TestContinuationMarkSet_ToList(t *testing.T) {
	tcs := []struct {
		name     string
		frames   [][]markEntry
		key      values.Value
		expected string
	}{
		{
			name:     "empty mark set",
			frames:   nil,
			key:      testKeyK,
			expected: "()",
		},
		{
			name: "single frame with mark",
			frames: [][]markEntry{
				{{testKeyK, values.NewInteger(1)}},
			},
			key:      testKeyK,
			expected: "(1)",
		},
		{
			name: "key absent",
			frames: [][]markEntry{
				{{testKeyK, values.NewInteger(1)}},
			},
			key:      testKeyOther,
			expected: "()",
		},
		{
			name: "multiple frames",
			frames: [][]markEntry{
				{{testKeyK, values.NewInteger(1)}},
				{{testKeyK, values.NewInteger(2)}},
			},
			key:      testKeyK,
			expected: "(1 2)",
		},
		{
			name: "mixed keys across frames",
			frames: [][]markEntry{
				{{testKeyA, values.NewInteger(1)}, {testKeyB, values.NewInteger(10)}},
				{{testKeyA, values.NewInteger(2)}},
				{{testKeyB, values.NewInteger(20)}},
			},
			key:      testKeyB,
			expected: "(10 20)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			cms := &ContinuationMarkSet{frames: tc.frames}
			qt.Assert(t, cms.ToList(tc.key).SchemeString(), qt.Equals, tc.expected)
		})
	}
}

func TestContinuationMarkSet_First(t *testing.T) {
	tcs := []struct {
		name     string
		frames   [][]markEntry
		key      values.Value
		expected values.Value
	}{
		{
			name:     "empty returns default",
			frames:   nil,
			key:      testKeyK,
			expected: values.FalseValue,
		},
		{
			name: "single frame",
			frames: [][]markEntry{
				{{testKeyK, values.NewInteger(1)}},
			},
			key:      testKeyK,
			expected: values.NewInteger(1),
		},
		{
			name: "returns nearest",
			frames: [][]markEntry{
				{{testKeyK, values.NewInteger(1)}},
				{{testKeyK, values.NewInteger(2)}},
			},
			key:      testKeyK,
			expected: values.NewInteger(1),
		},
		{
			name: "key absent returns default",
			frames: [][]markEntry{
				{{testKeyK, values.NewInteger(1)}},
			},
			key:      testKeyOther,
			expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			cms := &ContinuationMarkSet{frames: tc.frames}
			qt.Assert(t, cms.First(tc.key, values.FalseValue), qt.Equals, tc.expected)
		})
	}
}

func TestContinuationMarkSet_SchemeString(t *testing.T) {
	cms := &ContinuationMarkSet{}
	qt.Assert(t, cms.SchemeString(), qt.Equals, "#<continuation-mark-set>")
}

func TestContinuationMarkSet_EqualTo(t *testing.T) {
	c := qt.New(t)
	a := &ContinuationMarkSet{}
	b := &ContinuationMarkSet{}
	c.Assert(a.EqualTo(a), qt.IsTrue)
	c.Assert(a.EqualTo(b), qt.IsFalse)
	c.Assert(a.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestCollectContinuationMarks_NoMarks(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	cms := mc.CollectContinuationMarks(DefaultPromptTag)
	c.Assert(len(cms.frames), qt.Equals, 0)
}

func TestCollectContinuationMarks_CurrentFrameOnly(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	mc.SetMark(key, values.NewInteger(1))

	cms := mc.CollectContinuationMarks(DefaultPromptTag)
	c.Assert(cms.ToList(key).SchemeString(), qt.Equals, "(1)")
}

func TestCollectContinuationMarks_CurrentAndChain(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	mc.SetMark(key, values.NewInteger(1))

	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	// mc.marks is nil (callee), cont has k=1
	mc.SetMark(key, values.NewInteger(2))

	cms := mc.CollectContinuationMarks(DefaultPromptTag)
	c.Assert(cms.ToList(key).SchemeString(), qt.Equals, "(2 1)")
}

func TestCollectContinuationMarks_StopsAtPrompt(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	tag := NewPromptTag("test")

	// Frame below prompt
	mc.SetMark(key, values.NewInteger(1))
	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	// Prompt frame — mark then save, set prompt on the saved frame
	mc.SetMark(key, values.NewInteger(2))
	err = mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)
	mc.cont.SetPromptTag(tag)

	// Callee frame above prompt
	mc.SetMark(key, values.NewInteger(3))

	cms := mc.CollectContinuationMarks(tag)
	// Current (3), prompt frame (2) included, below-prompt (1) excluded
	c.Assert(cms.ToList(key).SchemeString(), qt.Equals, "(3 2)")
}

func TestCollectContinuationMarks_SnapshotImmutability(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	mc.SetMark(key, values.NewInteger(1))

	cms := mc.CollectContinuationMarks(DefaultPromptTag)

	// Mutate mc marks after snapshot
	mc.SetMark(key, values.NewInteger(999))

	// Snapshot unaffected
	c.Assert(cms.First(key, values.FalseValue), qt.Equals, values.NewInteger(1))
}

// CollectMarksFromContinuation shares its chain walk with CollectContinuationMarks
// (appendChainMarks); this pins the entry that starts at a captured frame rather
// than the live one: the live frame is excluded, the prompt frame included, and
// frames below the prompt excluded.
func TestCollectMarksFromContinuation_StopsAtPrompt(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	tag := NewPromptTag("test")

	mc.SetMark(key, values.NewInteger(1))
	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	mc.SetMark(key, values.NewInteger(2))
	err = mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)
	mc.cont.SetPromptTag(tag)

	mc.SetMark(key, values.NewInteger(3))

	c.Assert(CollectMarksFromContinuation(mc.cont, tag).ToList(key).SchemeString(), qt.Equals, "(2)")
	c.Assert(CollectMarksFromContinuation(mc.cont, DefaultPromptTag).ToList(key).SchemeString(), qt.Equals, "(2 1)")
	c.Assert(CollectMarksFromContinuation(nil, tag).ToList(key).SchemeString(), qt.Equals, "()")
}

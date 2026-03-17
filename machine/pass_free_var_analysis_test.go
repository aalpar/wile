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

	"github.com/aalpar/wile/environment"

	qt "github.com/frankban/quicktest"
)

// encodeLocal is a test helper that packs (slot, depth) into an int32
// using the same encoding as EncodeLocalIndex.
func encodeLocal(slot, depth int) int32 {
	li := environment.NewLocalIndex(slot, depth)
	return EncodeLocalIndex(li)
}

func TestAnalyzeFreeVars(t *testing.T) {
	tcs := []struct {
		name            string
		setupFn         func() *NativeTemplate
		wantCaptures    []CaptureEntry
		wantMutatedKeys [][2]int
	}{
		{
			name: "no free vars — only depth=0 references",
			setupFn: func() *NativeTemplate {
				tpl := NewEmptyNativeTemplate()
				tpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 0)},
					{Op: OpStoreLocal, Arg: encodeLocal(1, 0)},
					{Op: OpRestoreContinuation},
				}
				tpl.sourceRefs = make([]uint16, len(tpl.code))
				return tpl
			},
			wantCaptures:    []CaptureEntry{},
			wantMutatedKeys: nil,
		},
		{
			name: "one free var — read only",
			setupFn: func() *NativeTemplate {
				tpl := NewEmptyNativeTemplate()
				tpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
					{Op: OpRestoreContinuation},
				}
				tpl.sourceRefs = make([]uint16, len(tpl.code))
				return tpl
			},
			wantCaptures: []CaptureEntry{
				{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
			},
			wantMutatedKeys: nil,
		},
		{
			name: "one free var — mutated",
			setupFn: func() *NativeTemplate {
				tpl := NewEmptyNativeTemplate()
				tpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
					{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
					{Op: OpRestoreContinuation},
				}
				tpl.sourceRefs = make([]uint16, len(tpl.code))
				return tpl
			},
			wantCaptures: []CaptureEntry{
				{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
			},
			wantMutatedKeys: [][2]int{{0, 1}},
		},
		{
			name: "multiple free vars — sorted by depth then slot",
			setupFn: func() *NativeTemplate {
				tpl := NewEmptyNativeTemplate()
				tpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
					{Op: OpLoadLocal, Arg: encodeLocal(1, 2)},
					{Op: OpRestoreContinuation},
				}
				tpl.sourceRefs = make([]uint16, len(tpl.code))
				return tpl
			},
			wantCaptures: []CaptureEntry{
				{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
				{SourceSlot: 1, SourceDepth: 2, ClosureSlot: 1},
			},
			wantMutatedKeys: nil,
		},
		{
			name: "deduplicate — same reference appears twice",
			setupFn: func() *NativeTemplate {
				tpl := NewEmptyNativeTemplate()
				tpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
					{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
					{Op: OpRestoreContinuation},
				}
				tpl.sourceRefs = make([]uint16, len(tpl.code))
				return tpl
			},
			wantCaptures: []CaptureEntry{
				{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
			},
			wantMutatedKeys: nil,
		},
		{
			name: "PushLocal treated as LoadLocal",
			setupFn: func() *NativeTemplate {
				tpl := NewEmptyNativeTemplate()
				tpl.code = []Instruction{
					{Op: OpPushLocal, Arg: encodeLocal(0, 1)},
					{Op: OpRestoreContinuation},
				}
				tpl.sourceRefs = make([]uint16, len(tpl.code))
				return tpl
			},
			wantCaptures: []CaptureEntry{
				{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
			},
			wantMutatedKeys: nil,
		},
		{
			name: "nested closure — transitive propagation at depth=2",
			setupFn: func() *NativeTemplate {
				// Inner template references (slot=0, depth=2).
				// After propagation, outer template should have (slot=0, depth=1).
				subTpl := NewEmptyNativeTemplate()
				subTpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 2)},
					{Op: OpRestoreContinuation},
				}
				subTpl.sourceRefs = make([]uint16, len(subTpl.code))

				outerTpl := NewEmptyNativeTemplate()
				outerTpl.literals = append(outerTpl.literals, subTpl)
				outerTpl.code = []Instruction{
					{Op: OpRestoreContinuation},
				}
				outerTpl.sourceRefs = make([]uint16, len(outerTpl.code))
				return outerTpl
			},
			wantCaptures: []CaptureEntry{
				{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
			},
			wantMutatedKeys: nil,
		},
		{
			name: "nested closure — no propagation for depth=1",
			setupFn: func() *NativeTemplate {
				// Inner template references (slot=0, depth=1).
				// This comes from the outer template's locals, so the
				// outer template should NOT have it in its own free vars.
				subTpl := NewEmptyNativeTemplate()
				subTpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
					{Op: OpRestoreContinuation},
				}
				subTpl.sourceRefs = make([]uint16, len(subTpl.code))

				outerTpl := NewEmptyNativeTemplate()
				outerTpl.literals = append(outerTpl.literals, subTpl)
				outerTpl.code = []Instruction{
					{Op: OpRestoreContinuation},
				}
				outerTpl.sourceRefs = make([]uint16, len(outerTpl.code))
				return outerTpl
			},
			wantCaptures:    []CaptureEntry{},
			wantMutatedKeys: nil,
		},
		{
			name: "mutation propagation from inner template",
			setupFn: func() *NativeTemplate {
				// Inner template mutates (slot=0, depth=2).
				// After propagation, outer should have (slot=0, depth=1)
				// in both captures and mutated.
				subTpl := NewEmptyNativeTemplate()
				subTpl.code = []Instruction{
					{Op: OpLoadLocal, Arg: encodeLocal(0, 2)},
					{Op: OpStoreLocal, Arg: encodeLocal(0, 2)},
					{Op: OpRestoreContinuation},
				}
				subTpl.sourceRefs = make([]uint16, len(subTpl.code))

				outerTpl := NewEmptyNativeTemplate()
				outerTpl.literals = append(outerTpl.literals, subTpl)
				outerTpl.code = []Instruction{
					{Op: OpRestoreContinuation},
				}
				outerTpl.sourceRefs = make([]uint16, len(outerTpl.code))
				return outerTpl
			},
			wantCaptures: []CaptureEntry{
				{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
			},
			wantMutatedKeys: [][2]int{{0, 1}},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tpl := tc.setupFn()
			info := AnalyzeFreeVars(tpl)

			qt.Assert(t, info, qt.IsNotNil)
			qt.Assert(t, info.Captures, qt.HasLen, len(tc.wantCaptures),
				qt.Commentf("expected %d captures", len(tc.wantCaptures)))

			for i, want := range tc.wantCaptures {
				got := info.Captures[i]
				qt.Assert(t, got.SourceSlot, qt.Equals, want.SourceSlot,
					qt.Commentf("capture[%d].SourceSlot", i))
				qt.Assert(t, got.SourceDepth, qt.Equals, want.SourceDepth,
					qt.Commentf("capture[%d].SourceDepth", i))
				qt.Assert(t, got.ClosureSlot, qt.Equals, want.ClosureSlot,
					qt.Commentf("capture[%d].ClosureSlot", i))
			}

			if len(tc.wantMutatedKeys) == 0 {
				qt.Assert(t, len(info.Mutated), qt.Equals, 0,
					qt.Commentf("expected no mutations"))
			} else {
				for _, key := range tc.wantMutatedKeys {
					qt.Assert(t, info.Mutated[key], qt.IsTrue,
						qt.Commentf("expected mutation at (%d, %d)", key[0], key[1]))
				}
			}
		})
	}
}

// TestAnalyzeFreeVars_SubTemplateInfoStored verifies that the analysis
// stores FreeVarInfo on sub-templates as a side effect of the bottom-up walk.
func TestAnalyzeFreeVars_SubTemplateInfoStored(t *testing.T) {
	subTpl := NewEmptyNativeTemplate()
	subTpl.code = []Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	}
	subTpl.sourceRefs = make([]uint16, len(subTpl.code))

	outerTpl := NewEmptyNativeTemplate()
	outerTpl.literals = append(outerTpl.literals, subTpl)
	outerTpl.code = []Instruction{
		{Op: OpRestoreContinuation},
	}
	outerTpl.sourceRefs = make([]uint16, len(outerTpl.code))

	// Before analysis, sub-template has no FreeVarInfo.
	qt.Assert(t, subTpl.FreeVarInfo(), qt.IsNil)

	AnalyzeFreeVars(outerTpl)

	// After analysis, sub-template should have FreeVarInfo set.
	subInfo := subTpl.FreeVarInfo()
	qt.Assert(t, subInfo, qt.IsNotNil)
	qt.Assert(t, subInfo.Captures, qt.HasLen, 1)
	qt.Assert(t, subInfo.Captures[0].SourceSlot, qt.Equals, 0)
	qt.Assert(t, subInfo.Captures[0].SourceDepth, qt.Equals, 1)
}

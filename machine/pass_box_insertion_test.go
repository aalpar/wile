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

// makeTemplate creates a NativeTemplate with the given code and a matching
// sourceRefs slice (all zeroed). Convenience for test setup.
func makeTemplate(code []Instruction) *NativeTemplate {
	tpl := NewEmptyNativeTemplate()
	tpl.code = code
	tpl.sourceRefs = make([]uint16, len(code))
	return tpl
}

func TestInsertBoxes_NoBoxingNeeded(t *testing.T) {
	// Sub-template captures slot 0 at depth=1, but nobody mutates it.
	// InsertBoxes should not modify the outer template's bytecodes.
	subTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 0)},
		{Op: OpPush},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	// Run Pass 1 (analysis), then Pass 2 (box insertion).
	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// Code should be unchanged.
	qt.Assert(t, outerTpl.Code(), qt.HasLen, 3)
	qt.Assert(t, outerTpl.Code()[0].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, outerTpl.Code()[1].Op, qt.Equals, OpPush)
	qt.Assert(t, outerTpl.Code()[2].Op, qt.Equals, OpRestoreContinuation)
}

func TestInsertBoxes_ParameterBoxing(t *testing.T) {
	// Sub-template captures slot 0 at depth=1 AND mutates it.
	// The outer template should get a boxing preamble.
	subTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// Expected: 4 preamble instructions + 2 original = 6 total.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 6,
		qt.Commentf("got %d instructions", len(code)))

	// Preamble: LoadLocal(0,0), Box, Push, StoreLocal(0,0)
	qt.Assert(t, code[0].Op, qt.Equals, OpLoadLocal)
	slot0, depth0 := DecodeLocalIndex(code[0].Arg)
	qt.Assert(t, slot0, qt.Equals, 0)
	qt.Assert(t, depth0, qt.Equals, 0)
	qt.Assert(t, code[1].Op, qt.Equals, OpBox)
	qt.Assert(t, code[2].Op, qt.Equals, OpPush)
	qt.Assert(t, code[3].Op, qt.Equals, OpStoreLocal)

	// Original code follows.
	qt.Assert(t, code[4].Op, qt.Equals, OpLoadLiteral)
	qt.Assert(t, code[5].Op, qt.Equals, OpRestoreContinuation)
}

func TestInsertBoxes_ReadRewrite(t *testing.T) {
	// Sub-template captures slot 0 at depth=1 and mutates it.
	// Outer template reads slot 0 at depth=0 — should get Unbox inserted.
	subTpl := makeTemplate([]Instruction{
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 0)},
		{Op: OpPush},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// After read rewrite: LoadLocal(0,0) becomes LoadLocal(0,0) + Unbox.
	// After preamble: 4 preamble + (LoadLocal + Unbox + Push + RestoreCont) = 8.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 8,
		qt.Commentf("got %d instructions", len(code)))

	// Skip preamble (indices 0-3), check rewritten read.
	qt.Assert(t, code[4].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, code[5].Op, qt.Equals, OpUnbox)
	qt.Assert(t, code[6].Op, qt.Equals, OpPush)
	qt.Assert(t, code[7].Op, qt.Equals, OpRestoreContinuation)
}

func TestInsertBoxes_WriteRewrite(t *testing.T) {
	// Outer template does set! on a captured variable.
	// Sub-template captures slot 0 at depth=1 (read-only from sub),
	// but the outer template itself mutates it.
	subTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpStoreLocal, Arg: encodeLocal(0, 0)},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// StoreLocal(0,0) should become LoadLocal(0,0) + SetBox.
	// After rewrite: LoadLiteral, Push, LoadLocal(0,0), SetBox, RestoreCont.
	// After preamble: 4 + 5 = 9.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 9,
		qt.Commentf("got %d instructions", len(code)))

	// Skip preamble (0-3), check rewritten write.
	qt.Assert(t, code[4].Op, qt.Equals, OpLoadLiteral)
	qt.Assert(t, code[5].Op, qt.Equals, OpPush)
	qt.Assert(t, code[6].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, code[7].Op, qt.Equals, OpSetBox)
	qt.Assert(t, code[8].Op, qt.Equals, OpRestoreContinuation)
}

func TestInsertBoxes_PushLocalRewrite(t *testing.T) {
	// OpPushLocal(slot=0, depth=0) for a boxed var should become
	// OpLoadLocal + OpUnbox + OpPush.
	subTpl := makeTemplate([]Instruction{
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpPushLocal, Arg: encodeLocal(0, 0)},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// PushLocal becomes LoadLocal + Unbox + Push.
	// After preamble: 4 + (LoadLocal + Unbox + Push + RestoreCont) = 8.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 8,
		qt.Commentf("got %d instructions", len(code)))

	// Skip preamble.
	qt.Assert(t, code[4].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, code[5].Op, qt.Equals, OpUnbox)
	qt.Assert(t, code[6].Op, qt.Equals, OpPush)
	qt.Assert(t, code[7].Op, qt.Equals, OpRestoreContinuation)
}

func TestInsertBoxes_BranchOffsetAdjustment(t *testing.T) {
	// Template with a branch that jumps over a boxed read.
	// The branch offset should be adjusted to account for the inserted Unbox.
	subTpl := makeTemplate([]Instruction{
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	// Original code:
	//   0: Branch +3          (targets instruction 3)
	//   1: LoadLocal(0, 0)    (boxed read)
	//   2: Push
	//   3: RestoreContinuation (branch target)
	outerTpl := makeTemplate([]Instruction{
		{Op: OpBranch, Arg: 3},
		{Op: OpLoadLocal, Arg: encodeLocal(0, 0)},
		{Op: OpPush},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// After read rewrite (LoadLocal becomes LoadLocal + Unbox, +1 instr):
	//   0: Branch +4          (adjusted from +3 to +4)
	//   1: LoadLocal(0,0)
	//   2: Unbox
	//   3: Push
	//   4: RestoreContinuation
	// After preamble (4 instrs at front, shifts everything +4):
	//   0-3: preamble
	//   4: Branch +4          (offset is relative, not absolute — stays +4)
	//   5: LoadLocal(0,0)
	//   6: Unbox
	//   7: Push
	//   8: RestoreContinuation
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 9,
		qt.Commentf("got %d instructions", len(code)))

	// The branch is at index 4 after preamble. It should jump over
	// LoadLocal + Unbox + Push to reach RestoreContinuation.
	// Relative offset: target(8) - branch(4) = 4.
	qt.Assert(t, code[4].Op, qt.Equals, OpBranch)
	qt.Assert(t, code[4].Arg, qt.Equals, int32(4),
		qt.Commentf("branch offset should be 4, got %d", code[4].Arg))
	qt.Assert(t, code[8].Op, qt.Equals, OpRestoreContinuation)
}

func TestInsertBoxes_MultipleBoxedVars(t *testing.T) {
	// Sub-template captures slots 0 and 2 at depth=1, mutates both.
	subTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpLoadLocal, Arg: encodeLocal(2, 1)},
		{Op: OpStoreLocal, Arg: encodeLocal(2, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// 2 boxed vars => 8 preamble instructions + 2 original = 10.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 10,
		qt.Commentf("got %d instructions", len(code)))

	// First boxing: slot 0.
	qt.Assert(t, code[0].Op, qt.Equals, OpLoadLocal)
	s, d := DecodeLocalIndex(code[0].Arg)
	qt.Assert(t, s, qt.Equals, 0)
	qt.Assert(t, d, qt.Equals, 0)
	qt.Assert(t, code[1].Op, qt.Equals, OpBox)
	qt.Assert(t, code[2].Op, qt.Equals, OpPush)
	qt.Assert(t, code[3].Op, qt.Equals, OpStoreLocal)

	// Second boxing: slot 2.
	qt.Assert(t, code[4].Op, qt.Equals, OpLoadLocal)
	s2, d2 := DecodeLocalIndex(code[4].Arg)
	qt.Assert(t, s2, qt.Equals, 2)
	qt.Assert(t, d2, qt.Equals, 0)
	qt.Assert(t, code[5].Op, qt.Equals, OpBox)
	qt.Assert(t, code[6].Op, qt.Equals, OpPush)
	qt.Assert(t, code[7].Op, qt.Equals, OpStoreLocal)
}

func TestInsertBoxes_BoxedFlagSet(t *testing.T) {
	// After InsertBoxes, the sub-template's captures should have Boxed=true
	// for variables that were boxed.
	subTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpLoadLocal, Arg: encodeLocal(1, 1)}, // captured but NOT mutated
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	subInfo := subTpl.FreeVarInfo()
	qt.Assert(t, subInfo, qt.IsNotNil)

	// Slot 0 is captured AND mutated => Boxed=true.
	// Slot 1 is captured but NOT mutated => Boxed=false.
	var cap0, cap1 *CaptureEntry
	for i := range subInfo.Captures {
		if subInfo.Captures[i].SourceSlot == 0 && subInfo.Captures[i].SourceDepth == 1 {
			cap0 = &subInfo.Captures[i]
		}
		if subInfo.Captures[i].SourceSlot == 1 && subInfo.Captures[i].SourceDepth == 1 {
			cap1 = &subInfo.Captures[i]
		}
	}
	qt.Assert(t, cap0, qt.IsNotNil, qt.Commentf("capture for slot 0 not found"))
	qt.Assert(t, cap0.Boxed, qt.IsTrue, qt.Commentf("slot 0 should be boxed"))
	qt.Assert(t, cap1, qt.IsNotNil, qt.Commentf("capture for slot 1 not found"))
	qt.Assert(t, cap1.Boxed, qt.IsFalse, qt.Commentf("slot 1 should NOT be boxed"))
}

func TestInsertBoxes_OwnScopeMutation(t *testing.T) {
	// The DEFINING scope itself does set! on a captured variable.
	// Sub-template only reads it — the mutation is in the outer scope.
	subTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpStoreLocal, Arg: encodeLocal(0, 0)}, // own-scope set!
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// Slot 0 should be boxed because it's captured AND mutated (by own scope).
	// 4 preamble + (LoadLiteral + Push + LoadLocal + SetBox + RestoreCont) = 9.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 9,
		qt.Commentf("got %d instructions", len(code)))

	// Verify preamble.
	qt.Assert(t, code[0].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, code[1].Op, qt.Equals, OpBox)
	qt.Assert(t, code[2].Op, qt.Equals, OpPush)
	qt.Assert(t, code[3].Op, qt.Equals, OpStoreLocal)

	// Verify rewritten write.
	qt.Assert(t, code[6].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, code[7].Op, qt.Equals, OpSetBox)
}

func TestInsertBoxes_NonBoxedVarUntouched(t *testing.T) {
	// Slot 0 is boxed, slot 1 is not. Reads of slot 1 should be unchanged.
	subTpl := makeTemplate([]Instruction{
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(1, 0)}, // non-boxed read
		{Op: OpPush},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// 4 preamble + 3 original = 7. Slot 1 read stays as LoadLocal.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 7,
		qt.Commentf("got %d instructions", len(code)))

	// Slot 1 read should NOT have Unbox inserted.
	qt.Assert(t, code[4].Op, qt.Equals, OpLoadLocal)
	s, d := DecodeLocalIndex(code[4].Arg)
	qt.Assert(t, s, qt.Equals, 1)
	qt.Assert(t, d, qt.Equals, 0)
	qt.Assert(t, code[5].Op, qt.Equals, OpPush) // no Unbox between
}

func TestInsertBoxes_DepthGreaterThanZeroUntouched(t *testing.T) {
	// Reads at depth > 0 should NOT be rewritten by InsertBoxes.
	// They are free vars in this template — handled by the enclosing scope.
	//
	// Here: sub-template captures slot 0 at depth=1 (outerTpl's local)
	// and mutates it. Slot 0 IS boxed. But outerTpl also has a read of
	// slot 3 at depth=1 (a free var of outerTpl, NOT a local). This read
	// should remain untouched even though boxing rewrites are applied.
	subTpl := makeTemplate([]Instruction{
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(3, 1)}, // depth=1 free var
		{Op: OpPush},
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, subTpl)

	info := AnalyzeFreeVars(outerTpl)
	outerTpl.SetFreeVarInfo(info)
	InsertBoxes(outerTpl)

	// Slot 0 is boxed (4 preamble instructions), then original code.
	// The LoadLocal(3, 1) should remain unchanged — no Unbox inserted.
	code := outerTpl.Code()
	qt.Assert(t, code, qt.HasLen, 7,
		qt.Commentf("got %d instructions", len(code)))

	// Skip preamble (0-3), check the depth=1 read is untouched.
	qt.Assert(t, code[4].Op, qt.Equals, OpLoadLocal)
	s, d := DecodeLocalIndex(code[4].Arg)
	qt.Assert(t, s, qt.Equals, 3)
	qt.Assert(t, d, qt.Equals, 1, qt.Commentf("depth should remain 1"))
	qt.Assert(t, code[5].Op, qt.Equals, OpPush) // no Unbox between
}

func TestInsertBoxes_NoSubTemplates(t *testing.T) {
	// Template with no sub-templates — InsertBoxes should be a no-op.
	tpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 0)},
		{Op: OpStoreLocal, Arg: encodeLocal(0, 0)},
		{Op: OpRestoreContinuation},
	})

	info := AnalyzeFreeVars(tpl)
	tpl.SetFreeVarInfo(info)
	InsertBoxes(tpl)

	qt.Assert(t, tpl.Code(), qt.HasLen, 3)
}

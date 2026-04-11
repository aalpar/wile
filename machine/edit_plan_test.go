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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// --- EditPlan Core ---

func TestEditPlan_EmptyPlan(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{{Op: OpLoadVoid}, {Op: OpPush}}
	tpl.sourceRefs = []uint32{1, 2}

	plan := NewEditPlan(tpl)
	delta := plan.Apply()

	qt.Assert(t, delta, qt.Equals, 0)
	qt.Assert(t, len(tpl.code), qt.Equals, 2)
}

func TestEditPlan_HasEdits(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{{Op: OpLoadVoid}}
	tpl.sourceRefs = []uint32{0}

	plan := NewEditPlan(tpl)
	qt.Assert(t, plan.HasEdits(), qt.IsFalse)

	plan.Delete(0, 1)
	qt.Assert(t, plan.HasEdits(), qt.IsTrue)
}

// --- Delete ---

func TestEditPlan_SingleDelete(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpStoreGlobal, Arg: 0},
		{Op: OpLoadVoid}, // delete this
		{Op: OpLoadLiteral, Arg: 1},
		{Op: OpPush},
	}
	tpl.sourceRefs = []uint32{1, 2, 3, 4}

	plan := NewEditPlan(tpl)
	plan.Delete(1, 2)
	delta := plan.Apply()

	qt.Assert(t, delta, qt.Equals, -1)
	qt.Assert(t, opcodes(tpl), qt.DeepEquals, []OpCode{OpStoreGlobal, OpLoadLiteral, OpPush})
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint32{1, 3, 4})
}

func TestEditPlan_DeleteRange(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpStoreGlobal, Arg: 0}, // 0: keep
		{Op: OpLoadVoid},            // 1: delete
		{Op: OpPush},                // 2: delete
		{Op: OpLoadLiteral, Arg: 1}, // 3: delete
		{Op: OpApply},               // 4: keep
	}
	tpl.sourceRefs = []uint32{1, 2, 3, 4, 5}

	plan := NewEditPlan(tpl)
	plan.Delete(1, 4)
	plan.Apply()

	qt.Assert(t, opcodes(tpl), qt.DeepEquals, []OpCode{OpStoreGlobal, OpApply})
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint32{1, 5})
}

func TestEditPlan_MultipleDeletes(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadLiteral, Arg: 0}, // 0: keep
		{Op: OpLoadVoid},            // 1: delete
		{Op: OpPush},                // 2: keep
		{Op: OpLoadVoid},            // 3: delete
		{Op: OpApply},               // 4: keep
	}
	tpl.sourceRefs = []uint32{1, 2, 3, 4, 5}

	plan := NewEditPlan(tpl)
	plan.Delete(1, 2)
	plan.Delete(3, 4)
	plan.Apply()

	qt.Assert(t, opcodes(tpl), qt.DeepEquals, []OpCode{OpLoadLiteral, OpPush, OpApply})
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint32{1, 3, 5})
}

// --- Replace ---

func TestEditPlan_SingleReplace(t *testing.T) {
	// Simulates constant folding: replace a call sequence with a single LoadLiteral.
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpSaveContinuation, Arg: 8}, // 0
		{Op: OpLoadGlobal, Arg: 0},       // 1
		{Op: OpPush},                     // 2
		{Op: OpLoadLiteral, Arg: 1},      // 3
		{Op: OpPush},                     // 4
		{Op: OpLoadLiteral, Arg: 2},      // 5
		{Op: OpPush},                     // 6
		{Op: OpPull},                     // 7
		{Op: OpApply},                    // 8
		{Op: OpPush},                     // 9: after the call
	}
	tpl.sourceRefs = []uint32{10, 10, 10, 10, 10, 10, 10, 10, 10, 11}
	tpl.literals = MultipleValues{values.NewSymbol("+"), values.NewInteger(3), values.NewInteger(4)}

	plan := NewEditPlan(tpl)
	litIdx := plan.AddLiteral(values.NewInteger(7))
	plan.Replace(0, 9, []Instruction{
		{Op: OpLoadLiteral, Arg: litIdx},
	}, 10)
	plan.Apply()

	qt.Assert(t, opcodes(tpl), qt.DeepEquals, []OpCode{OpLoadLiteral, OpPush})
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, litIdx)
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint32{10, 11})
}

func TestEditPlan_ReplaceWithLarger(t *testing.T) {
	// Replace one instruction with three.
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadVoid}, // 0: replace with 3 instrs
		{Op: OpPush},     // 1: keep
	}
	tpl.sourceRefs = []uint32{1, 2}

	plan := NewEditPlan(tpl)
	plan.Replace(0, 1, []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpPop},
	}, 5)
	plan.Apply()

	qt.Assert(t, opcodes(tpl), qt.DeepEquals, []OpCode{OpLoadLiteral, OpPush, OpPop, OpPush})
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint32{5, 5, 5, 2})
}

// --- Insert ---

func TestEditPlan_Insert(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadVoid}, // 0
		{Op: OpPush},     // 1
	}
	tpl.sourceRefs = []uint32{1, 2}

	plan := NewEditPlan(tpl)
	plan.Insert(1, []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
	}, 3)
	plan.Apply()

	qt.Assert(t, opcodes(tpl), qt.DeepEquals, []OpCode{OpLoadVoid, OpLoadLiteral, OpPush})
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint32{1, 3, 2})
}

// --- Branch Fixup ---

func TestEditPlan_BranchShrinks(t *testing.T) {
	// Branch spans a deleted range; offset must shrink.
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpBranch, Arg: 4},      // 0: target = 4
		{Op: OpStoreGlobal, Arg: 0}, // 1
		{Op: OpLoadVoid},            // 2: delete
		{Op: OpLoadLiteral, Arg: 1}, // 3
		{Op: OpApply},               // 4
	}
	tpl.sourceRefs = make([]uint32, 5)

	plan := NewEditPlan(tpl)
	plan.Delete(2, 3)
	plan.Apply()

	// After: [0] Branch +3  [1] StoreGlobal  [2] LoadLiteral  [3] Apply
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(3))
}

func TestEditPlan_BranchGrows(t *testing.T) {
	// Branch spans an insertion; offset must grow.
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpBranch, Arg: 2}, // 0: target = 2
		{Op: OpPush},           // 1
		{Op: OpApply},          // 2
	}
	tpl.sourceRefs = make([]uint32, 3)

	plan := NewEditPlan(tpl)
	plan.Insert(1, []Instruction{
		{Op: OpLoadVoid},
		{Op: OpLoadVoid},
	}, 0)
	plan.Apply()

	// After: [0] Branch +4  [1] LoadVoid  [2] LoadVoid  [3] Push  [4] Apply
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(4))
}

func TestEditPlan_BranchAcrossReplace(t *testing.T) {
	// Branch at 0 targets 6. Replace [2,5) with 1 instruction.
	// Net: delete 3, insert 1 => shrink by 2.
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpBranch, Arg: 6}, // 0: target = 6
		{Op: OpPush},           // 1
		{Op: OpLoadVoid},       // 2: replaced
		{Op: OpLoadVoid},       // 3: replaced
		{Op: OpLoadVoid},       // 4: replaced
		{Op: OpPush},           // 5
		{Op: OpApply},          // 6
	}
	tpl.sourceRefs = make([]uint32, 7)

	plan := NewEditPlan(tpl)
	plan.Replace(2, 5, []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
	}, 0)
	plan.Apply()

	// After: [0] Branch +4  [1] Push  [2] LoadLiteral  [3] Push  [4] Apply
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(4))
}

func TestEditPlan_SaveContinuationSentinel(t *testing.T) {
	// SaveContinuation targeting past the end (sentinel).
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpSaveContinuation, Arg: 4}, // 0: target = 4 (sentinel)
		{Op: OpLoadVoid},                 // 1: delete
		{Op: OpLoadLiteral, Arg: 0},      // 2
		{Op: OpApply},                    // 3
	}
	tpl.sourceRefs = make([]uint32, 4)

	plan := NewEditPlan(tpl)
	plan.Delete(1, 2)
	plan.Apply()

	// After: [0] SaveContinuation +3  [1] LoadLiteral  [2] Apply
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(3))
}

func TestEditPlan_BranchBeforeReplace_NotSpanning(t *testing.T) {
	// Branch whose target is before the edit — should be unchanged.
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpPush},            // 0
		{Op: OpBranch, Arg: -1}, // 1: target = 0 (backward)
		{Op: OpLoadVoid},        // 2: delete
		{Op: OpApply},           // 3
	}
	tpl.sourceRefs = make([]uint32, 4)

	plan := NewEditPlan(tpl)
	plan.Delete(2, 3)
	plan.Apply()

	// After: [0] Push  [1] Branch -1  [2] Apply
	qt.Assert(t, tpl.code[1].Arg, qt.Equals, int32(-1))
}

// --- AddLiteral ---

func TestEditPlan_AddLiteral_New(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{{Op: OpLoadVoid}}
	tpl.sourceRefs = []uint32{0}
	tpl.MaybeAppendLiteral(values.NewInteger(1))

	plan := NewEditPlan(tpl)
	idx := plan.AddLiteral(values.NewInteger(42))

	qt.Assert(t, idx, qt.Equals, int32(1))
	qt.Assert(t, len(tpl.literals), qt.Equals, 2)
}

func TestEditPlan_AddLiteral_Dedup(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{{Op: OpLoadVoid}}
	tpl.sourceRefs = []uint32{0}
	tpl.MaybeAppendLiteral(values.NewInteger(42))

	plan := NewEditPlan(tpl)
	idx := plan.AddLiteral(values.NewInteger(42))

	qt.Assert(t, idx, qt.Equals, int32(0))
	qt.Assert(t, len(tpl.literals), qt.Equals, 1)
}

// --- SideTable GC ---

func TestEditPlan_SideTableGC(t *testing.T) {
	op0 := NewOperationMakeClosure()
	op1 := NewOperationMakeClosure()
	op2 := NewOperationMakeClosure()

	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpComplex, Arg: 0}, // 0: references op0
		{Op: OpComplex, Arg: 1}, // 1: references op1 — will be deleted
		{Op: OpComplex, Arg: 2}, // 2: references op2
	}
	tpl.sourceRefs = []uint32{0, 0, 0}
	tpl.sideTable = []InlinedOperation{op0, op1, op2}

	plan := NewEditPlan(tpl)
	plan.Delete(1, 2) // remove the instruction referencing op1
	plan.Apply()

	// op1 should be GC'd; op0 stays at 0, op2 remapped to 1.
	qt.Assert(t, len(tpl.sideTable), qt.Equals, 2)
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(0))
	qt.Assert(t, tpl.code[1].Arg, qt.Equals, int32(1))
	qt.Assert(t, tpl.sideTable[0], qt.Equals, InlinedOperation(op0))
	qt.Assert(t, tpl.sideTable[1], qt.Equals, InlinedOperation(op2))
}

func TestEditPlan_SideTableGC_NoneUnreferenced(t *testing.T) {
	op0 := NewOperationMakeClosure()

	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpComplex, Arg: 0},
		{Op: OpLoadVoid}, // delete this (doesn't reference sideTable)
		{Op: OpLoadLiteral, Arg: 0},
	}
	tpl.sourceRefs = []uint32{0, 0, 0}
	tpl.sideTable = []InlinedOperation{op0}

	plan := NewEditPlan(tpl)
	plan.Delete(1, 2)
	plan.Apply()

	qt.Assert(t, len(tpl.sideTable), qt.Equals, 1) // no GC needed
}

// --- Validation ---

func TestEditPlan_OverlappingEdits_Panics(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadVoid},
		{Op: OpPush},
		{Op: OpApply},
	}
	tpl.sourceRefs = make([]uint32, 3)

	plan := NewEditPlan(tpl)
	plan.Delete(0, 2)
	plan.Delete(1, 3) // overlaps with first

	qt.Assert(t, func() { plan.Apply() }, qt.PanicMatches, "edit_plan: overlapping edits.*")
}

func TestEditPlan_OutOfBounds_Panics(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{{Op: OpLoadVoid}}
	tpl.sourceRefs = []uint32{0}

	plan := NewEditPlan(tpl)
	plan.Delete(0, 5) // end > len(code)

	qt.Assert(t, func() { plan.Apply() }, qt.PanicMatches, "edit_plan: edit out of bounds.*")
}

// --- Compound: Delete + Replace + Insert in one plan ---

func TestEditPlan_MixedEdits(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadVoid},            // 0: delete
		{Op: OpPush},                // 1: keep
		{Op: OpLoadLiteral, Arg: 0}, // 2: replace with 2 instrs
		{Op: OpApply},               // 3: keep
	}
	tpl.sourceRefs = []uint32{1, 2, 3, 4}

	plan := NewEditPlan(tpl)
	plan.Delete(0, 1)
	plan.Replace(2, 3, []Instruction{
		{Op: OpLoadGlobal, Arg: 0},
		{Op: OpPop},
	}, 7)
	delta := plan.Apply()

	// New: [Push, LoadGlobal, Pop, Apply]
	qt.Assert(t, delta, qt.Equals, 0) // -1 + 1 = 0 net
	qt.Assert(t, opcodes(tpl), qt.DeepEquals, []OpCode{OpPush, OpLoadGlobal, OpPop, OpApply})
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint32{2, 7, 7, 4})
}

// --- buildEditRemap ---

func TestBuildEditRemap_SingleDelete(t *testing.T) {
	edits := []edit{{start: 2, end: 4}}
	remap := buildEditRemap(edits, 6)

	// old: 0 1 [2 3] 4 5  sentinel
	// new: 0 1  2 2  2 3  4
	qt.Assert(t, remap, qt.DeepEquals, []int{0, 1, 2, 2, 2, 3, 4})
}

func TestBuildEditRemap_SingleInsert(t *testing.T) {
	edits := []edit{{start: 1, end: 1, replace: []Instruction{{Op: OpLoadVoid}, {Op: OpLoadVoid}}}}
	remap := buildEditRemap(edits, 3)

	// old: 0 1 2  sentinel
	// Insert 2 instrs at position 1. After: 0 [new new] 1 2
	// remap[0] = 0, remap[1] = 3 (shifted by 2), remap[2] = 4, remap[3] = 5
	qt.Assert(t, remap, qt.DeepEquals, []int{0, 3, 4, 5})
}

func TestBuildEditRemap_Replace(t *testing.T) {
	// Replace [1,3) with 1 instruction.
	edits := []edit{{start: 1, end: 3, replace: []Instruction{{Op: OpLoadVoid}}}}
	remap := buildEditRemap(edits, 5)

	// old: 0 [1 2] 3 4  sentinel
	// new: 0 [1]   2 3  4
	// remap[0]=0, remap[1]=1 (replacement start), remap[2]=1, remap[3]=2, remap[4]=3, remap[5]=4
	qt.Assert(t, remap, qt.DeepEquals, []int{0, 1, 1, 2, 3, 4})
}

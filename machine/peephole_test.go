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

// --- Helper ---

// opcodes extracts the opcode sequence from a template for concise assertions.
func opcodes(tpl *NativeTemplate) []OpCode {
	ops := make([]OpCode, len(tpl.code))
	for i, instr := range tpl.code {
		ops[i] = instr.Op
	}
	return ops
}

// --- Rule Correctness ---

func TestPeephole_DeadLoadVoid(t *testing.T) {
	tests := []struct {
		name     string
		code     []Instruction
		wantOps  []OpCode
		wantDead int // number of instructions removed
	}{
		{
			name: "LoadVoid before LoadLiteral is dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadLiteral, Arg: 1},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpLoadLiteral},
			wantDead: 1,
		},
		{
			name: "LoadVoid before LoadGlobal is dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadGlobal, Arg: 1},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpLoadGlobal},
			wantDead: 1,
		},
		{
			name: "LoadVoid before LoadLocal is dead",
			code: []Instruction{
				{Op: OpStoreLocal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadLocal, Arg: 1},
			},
			wantOps:  []OpCode{OpStoreLocal, OpLoadLocal},
			wantDead: 1,
		},
		{
			name: "LoadVoid before LoadVoid is dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadVoid},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpLoadVoid},
			wantDead: 1,
		},
		{
			name: "LoadVoid at end of code is NOT dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpLoadVoid},
			wantDead: 0,
		},
		{
			name: "LoadVoid before Push is NOT dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpPush},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpLoadVoid, OpPush},
			wantDead: 0,
		},
		{
			name: "LoadVoid before OpComplex is NOT dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpComplex, Arg: 0},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpLoadVoid, OpComplex},
			wantDead: 0,
		},
		{
			name: "consecutive dead LoadVoids",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid}, // dead: followed by LoadVoid
				{Op: OpLoadVoid}, // dead: followed by LoadLiteral
				{Op: OpLoadLiteral, Arg: 1},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpLoadLiteral},
			wantDead: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceRefs = make([]uint16, len(tt.code))

			before := len(tpl.code)
			tpl.Optimize()
			after := len(tpl.code)

			qt.Assert(t, opcodes(tpl), qt.DeepEquals, tt.wantOps)
			qt.Assert(t, before-after, qt.Equals, tt.wantDead)
		})
	}
}

// --- Offset Fixup ---

func TestPeephole_BranchOffsetFixup(t *testing.T) {
	tests := []struct {
		name    string
		code    []Instruction
		wantArg map[int]int32 // index in optimized code → expected Arg
	}{
		{
			name: "branch spanning removed instruction shrinks",
			// [0] Branch +4  →  target is [4]
			// [1] StoreGlobal
			// [2] LoadVoid    ← dead
			// [3] LoadLiteral
			// [4] Apply
			code: []Instruction{
				{Op: OpBranch, Arg: 4},
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadLiteral, Arg: 1},
				{Op: OpApply},
			},
			// After: [0] Branch +3  [1] StoreGlobal  [2] LoadLiteral  [3] Apply
			wantArg: map[int]int32{0: 3},
		},
		{
			name: "branch not spanning removed instruction unchanged",
			// [0] StoreGlobal
			// [1] LoadVoid    ← dead
			// [2] LoadLiteral
			// [3] Branch +1   → target is [4]
			// [4] Apply
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadLiteral, Arg: 1},
				{Op: OpBranch, Arg: 1},
				{Op: OpApply},
			},
			// After: [0] StoreGlobal  [1] LoadLiteral  [2] Branch +1  [3] Apply
			wantArg: map[int]int32{2: 1},
		},
		{
			name: "SaveContinuation spanning removed instruction",
			// [0] SaveContinuation +4  → target is [4]
			// [1] StoreGlobal
			// [2] LoadVoid            ← dead
			// [3] LoadLiteral
			// [4] RestoreContinuation
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 4},
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadLiteral, Arg: 1},
				{Op: OpRestoreContinuation},
			},
			// After: [0] SaveContinuation +3  [1] StoreGlobal  [2] LoadLiteral  [3] RestoreContinuation
			wantArg: map[int]int32{0: 3},
		},
		{
			name: "BranchOnFalseValue spanning removed instruction",
			code: []Instruction{
				{Op: OpBranchOnFalseValue, Arg: 3},
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadLiteral, Arg: 1},
			},
			// dead[2] removed; target was 0+3=3, remap: 0→0,1→1,2→2,3→2
			// new arg = remap[3] - remap[0] = 2 - 0 = 2
			wantArg: map[int]int32{0: 2},
		},
		{
			name: "multiple removals between branch and target",
			// [0] Branch +6            → target is [6]
			// [1] StoreGlobal
			// [2] LoadVoid             ← dead
			// [3] LoadVoid             ← dead (followed by LoadGlobal)
			// [4] LoadGlobal
			// [5] Push
			// [6] Apply
			code: []Instruction{
				{Op: OpBranch, Arg: 6},
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpLoadVoid},
				{Op: OpLoadGlobal, Arg: 1},
				{Op: OpPush},
				{Op: OpApply},
			},
			// After: [0] Branch +4  [1] StoreGlobal  [2] LoadGlobal  [3] Push  [4] Apply
			wantArg: map[int]int32{0: 4},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceRefs = make([]uint16, len(tt.code))

			tpl.Optimize()

			for idx, expectedArg := range tt.wantArg {
				qt.Assert(t, tpl.code[idx].Arg, qt.Equals, expectedArg,
					qt.Commentf("instruction %d (%s)", idx, tpl.code[idx].Op))
			}
		})
	}
}

func TestPeephole_BranchTargetSentinel(t *testing.T) {
	// SaveContinuation targeting len(code) — the sentinel position
	// (one past the last instruction). This exercises pcRemap[len(dead)].
	//
	// [0] SaveContinuation +5  → target is [5] (sentinel)
	// [1] LoadLiteral
	// [2] Push
	// [3] LoadVoid             ← dead
	// [4] LoadGlobal
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpSaveContinuation, Arg: 5},
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpLoadVoid},
		{Op: OpLoadGlobal, Arg: 1},
	}
	tpl.sourceRefs = make([]uint16, 5)

	tpl.Optimize()

	// After: [0] SaveContinuation +4  [1] LoadLiteral  [2] Push  [3] LoadGlobal
	qt.Assert(t, len(tpl.code), qt.Equals, 4)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpSaveContinuation)
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(4),
		qt.Commentf("sentinel target should remap to new len(code)"))
}

// --- Source Map ---

func TestPeephole_SourceRefsParallel(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpStoreGlobal, Arg: 0}, // sourceRef 1
		{Op: OpLoadVoid},            // sourceRef 2 (dead)
		{Op: OpLoadLiteral, Arg: 1}, // sourceRef 3
		{Op: OpPush},                // sourceRef 4
	}
	tpl.sourceRefs = []uint16{1, 2, 3, 4}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, len(tpl.sourceRefs))
	// The surviving instructions should keep their original source refs.
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint16{1, 3, 4})
}

// --- Edge Cases ---

func TestPeephole_EmptyCode(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.Optimize() // should not panic
	qt.Assert(t, len(tpl.code), qt.Equals, 0)
}

func TestPeephole_SingleInstruction(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{{Op: OpLoadVoid}}
	tpl.sourceRefs = []uint16{0}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, 1)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpLoadVoid)
}

func TestPeephole_Idempotent(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpStoreGlobal, Arg: 0},
		{Op: OpLoadVoid},
		{Op: OpLoadLiteral, Arg: 1},
	}
	tpl.sourceRefs = []uint16{0, 0, 0}

	tpl.Optimize()
	first := len(tpl.code)
	qt.Assert(t, first, qt.Equals, 2) // one removed

	tpl.Optimize()
	qt.Assert(t, len(tpl.code), qt.Equals, first) // no change
}

func TestPeephole_NoOptimizablePatterns(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpApply},
	}
	tpl.sourceRefs = []uint16{0, 0, 0}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, 3)
}

// --- Recursive Sub-Templates ---

func TestPeephole_RecursiveSubTemplate(t *testing.T) {
	// Create a sub-template with a dead LoadVoid
	sub := NewEmptyNativeTemplate()
	sub.code = []Instruction{
		{Op: OpStoreLocal, Arg: 0},
		{Op: OpLoadVoid}, // dead
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpRestoreContinuation},
	}
	sub.sourceRefs = []uint16{0, 0, 0, 0}

	// Parent template stores sub in its literals pool
	parent := NewEmptyNativeTemplate()
	parent.code = []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
	}
	parent.sourceRefs = []uint16{0, 0}
	parent.literals = MultipleValues{sub}

	parent.Optimize()

	// Parent is unchanged (no dead LoadVoid)
	qt.Assert(t, len(parent.code), qt.Equals, 2)

	// Sub-template was optimized
	qt.Assert(t, len(sub.code), qt.Equals, 3)
	qt.Assert(t, opcodes(sub), qt.DeepEquals, []OpCode{OpStoreLocal, OpLoadLiteral, OpRestoreContinuation})
}

func TestPeephole_NonTemplateLiteralsIgnored(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
	}
	tpl.sourceRefs = []uint16{0}
	tpl.literals = MultipleValues{values.NewInteger(42)}

	tpl.Optimize() // should not panic on non-template literal
	qt.Assert(t, len(tpl.code), qt.Equals, 1)
}

// --- Internal Helpers ---

func TestIsLoadOp(t *testing.T) {
	loads := []OpCode{OpLoadVoid, OpLoadLiteral, OpLoadGlobal, OpLoadLocal}
	for _, op := range loads {
		qt.Assert(t, isLoadOp(op), qt.IsTrue, qt.Commentf("%s", op))
	}

	nonLoads := []OpCode{OpPush, OpPop, OpApply, OpBranch, OpComplex, OpStoreGlobal}
	for _, op := range nonLoads {
		qt.Assert(t, isLoadOp(op), qt.IsFalse, qt.Commentf("%s", op))
	}
}

func TestIsBranchOp(t *testing.T) {
	branches := []OpCode{OpBranch, OpBranchOnFalseValue, OpSaveContinuation}
	for _, op := range branches {
		qt.Assert(t, isBranchOp(op), qt.IsTrue, qt.Commentf("%s", op))
	}

	nonBranches := []OpCode{OpPush, OpLoadVoid, OpApply, OpComplex}
	for _, op := range nonBranches {
		qt.Assert(t, isBranchOp(op), qt.IsFalse, qt.Commentf("%s", op))
	}
}

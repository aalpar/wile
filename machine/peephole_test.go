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
			name: "LoadVoid before Pop is dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpPop},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpPop},
			wantDead: 1,
		},
		{
			name: "LoadVoid before Pull is dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpPull},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpPull},
			wantDead: 1,
		},
		{
			name: "LoadVoid before PeekK is dead",
			code: []Instruction{
				{Op: OpStoreGlobal, Arg: 0},
				{Op: OpLoadVoid},
				{Op: OpPeekK, Arg: 0},
			},
			wantOps:  []OpCode{OpStoreGlobal, OpPeekK},
			wantDead: 1,
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
			// [4] LoadGlobal           ← fused with Push
			// [5] Push                 ← fused into PushGlobal
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
			// After: [0] Branch +3  [1] StoreGlobal  [2] PushGlobal  [3] Apply
			wantArg: map[int]int32{0: 3},
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
	// [1] LoadLiteral          ← fused with Push
	// [2] Push                 ← fused into PushLiteral
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

	// After: [0] SaveContinuation +3  [1] PushLiteral  [2] LoadGlobal
	qt.Assert(t, len(tpl.code), qt.Equals, 3)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpSaveContinuation)
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(3),
		qt.Commentf("sentinel target should remap to new len(code)"))
}

// --- Source Map ---

func TestPeephole_SourceRefsParallel(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpStoreGlobal, Arg: 0}, // sourceRef 1
		{Op: OpLoadVoid},            // sourceRef 2 (dead)
		{Op: OpLoadLiteral, Arg: 1}, // sourceRef 3
		{Op: OpPush},                // sourceRef 4 (fused with LoadLiteral)
	}
	tpl.sourceRefs = []uint16{1, 2, 3, 4}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, len(tpl.sourceRefs))
	// LoadVoid removed; LoadLiteral+Push fused to PushLiteral with Load's sourceRef.
	qt.Assert(t, tpl.sourceRefs, qt.DeepEquals, []uint16{1, 3})
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
		{Op: OpApply},
		{Op: OpRestoreContinuation},
	}
	tpl.sourceRefs = []uint16{0, 0}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, 2)
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

	// Parent: LoadLiteral+Push fused into PushLiteral
	qt.Assert(t, len(parent.code), qt.Equals, 1)
	qt.Assert(t, parent.code[0].Op, qt.Equals, OpPushLiteral)

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

// --- Fused Push ---

func TestPeephole_FuseLoadPush(t *testing.T) {
	tests := []struct {
		name    string
		code    []Instruction
		wantOps []OpCode
		wantArg map[int]int32 // index → expected Arg in optimized code
	}{
		{
			name: "LoadLiteral+Push fuses to PushLiteral",
			code: []Instruction{
				{Op: OpLoadLiteral, Arg: 7},
				{Op: OpPush},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPushLiteral, OpApply},
			wantArg: map[int]int32{0: 7},
		},
		{
			name: "LoadGlobal+Push fuses to PushGlobal",
			code: []Instruction{
				{Op: OpLoadGlobal, Arg: 3},
				{Op: OpPush},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPushGlobal, OpApply},
			wantArg: map[int]int32{0: 3},
		},
		{
			name: "LoadLocal+Push fuses to PushLocal",
			code: []Instruction{
				{Op: OpLoadLocal, Arg: 42},
				{Op: OpPush},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPushLocal, OpApply},
			wantArg: map[int]int32{0: 42},
		},
		{
			name: "LoadLiteral+non-Push does not fuse",
			code: []Instruction{
				{Op: OpLoadLiteral, Arg: 0},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpLoadLiteral, OpApply},
		},
		{
			name: "non-Load+Push does not fuse",
			code: []Instruction{
				{Op: OpPop},
				{Op: OpPush},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPop, OpPush, OpApply},
		},
		{
			name: "multiple consecutive fusions",
			code: []Instruction{
				{Op: OpLoadLiteral, Arg: 1},
				{Op: OpPush},
				{Op: OpLoadGlobal, Arg: 2},
				{Op: OpPush},
				{Op: OpLoadLocal, Arg: 3},
				{Op: OpPush},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPushLiteral, OpPushGlobal, OpPushLocal, OpApply},
			wantArg: map[int]int32{0: 1, 1: 2, 2: 3},
		},
		{
			name: "interleaved fusible and non-fusible",
			code: []Instruction{
				{Op: OpLoadLiteral, Arg: 1},
				{Op: OpPush},
				{Op: OpLoadLiteral, Arg: 2},
				{Op: OpBranchOnFalseValue, Arg: 1},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPushLiteral, OpLoadLiteral, OpBranchOnFalseValue, OpApply},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceRefs = make([]uint16, len(tt.code))

			tpl.Optimize()

			qt.Assert(t, opcodes(tpl), qt.DeepEquals, tt.wantOps)
			for idx, expectedArg := range tt.wantArg {
				qt.Assert(t, tpl.code[idx].Arg, qt.Equals, expectedArg,
					qt.Commentf("instruction %d (%s)", idx, tpl.code[idx].Op))
			}
		})
	}
}

func TestPeephole_FuseLoadPush_SourceRef(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
	}
	tpl.sourceRefs = []uint16{5, 6}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, 1)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpPushLiteral)
	// Fused instruction inherits sourceRef from the Load, not the Push.
	qt.Assert(t, tpl.sourceRefs[0], qt.Equals, uint16(5))
}

func TestPeephole_FuseLoadPush_BranchAcrossFusion(t *testing.T) {
	// Branch target lands past a fused pair.
	// [0] Branch +4  → target is [4]
	// [1] LoadLiteral
	// [2] Push         ← fused with LoadLiteral
	// [3] LoadGlobal
	// [4] Apply
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpBranch, Arg: 4},
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpLoadGlobal, Arg: 1},
		{Op: OpApply},
	}
	tpl.sourceRefs = make([]uint16, 5)

	tpl.Optimize()

	// After: [0] Branch +3  [1] PushLiteral  [2] LoadGlobal  [3] Apply
	qt.Assert(t, opcodes(tpl), qt.DeepEquals,
		[]OpCode{OpBranch, OpPushLiteral, OpLoadGlobal, OpApply})
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(3))
}

func TestPeephole_FuseLoadPush_PushIsBranchTarget(t *testing.T) {
	// Push is a branch target (convergence point for if-expression).
	// Must NOT fuse LoadLiteral+Push because the Branch also targets Push.
	//
	// [0] LoadLocal               ; value = test
	// [1] BranchOnFalseValue +3   ; → [4]
	// [2] LoadLiteral #t          ; consequent
	// [3] Branch +2               ; → [5] Push
	// [4] LoadLiteral #f          ; alternative
	// [5] Push                    ; convergence: push if-result
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadLocal, Arg: 0},
		{Op: OpBranchOnFalseValue, Arg: 3},
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpBranch, Arg: 2},
		{Op: OpLoadLiteral, Arg: 1},
		{Op: OpPush},
	}
	tpl.sourceRefs = make([]uint16, 6)

	tpl.Optimize()

	// Push at [5] is a branch target (Branch +2 from [3] targets [5]).
	// Neither LoadLiteral+Push pair should be fused.
	qt.Assert(t, opcodes(tpl), qt.DeepEquals,
		[]OpCode{OpLoadLocal, OpBranchOnFalseValue, OpLoadLiteral, OpBranch, OpLoadLiteral, OpPush})
}

func TestPeephole_FusePullApply(t *testing.T) {
	tests := []struct {
		name    string
		code    []Instruction
		wantOps []OpCode
	}{
		{
			name: "Pull+Apply fuses to PullApply",
			code: []Instruction{
				{Op: OpPushLiteral, Arg: 0},
				{Op: OpPull},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPushLiteral, OpPullApply},
		},
		{
			name: "Pull without Apply does not fuse",
			code: []Instruction{
				{Op: OpPull},
				{Op: OpPush},
			},
			wantOps: []OpCode{OpPull, OpPush},
		},
		{
			name: "Apply without Pull does not fuse",
			code: []Instruction{
				{Op: OpPush},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPush, OpApply},
		},
		{
			name: "multiple Pull+Apply pairs all fuse",
			code: []Instruction{
				{Op: OpPull},
				{Op: OpApply},
				{Op: OpPull},
				{Op: OpApply},
			},
			wantOps: []OpCode{OpPullApply, OpPullApply},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceRefs = make([]uint16, len(tt.code))

			tpl.Optimize()

			qt.Assert(t, opcodes(tpl), qt.DeepEquals, tt.wantOps)
		})
	}
}

func TestPeephole_FusePullApply_SourceRef(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpPull},
		{Op: OpApply},
	}
	tpl.sourceRefs = []uint16{5, 6}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, 1)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpPullApply)
	// Fused instruction inherits sourceRef from Pull, not Apply.
	qt.Assert(t, tpl.sourceRefs[0], qt.Equals, uint16(5))
}

func TestPeephole_FusePullApply_ApplyIsBranchTarget(t *testing.T) {
	// Apply is a branch target — must NOT fuse.
	//
	// [0] Pull
	// [1] BranchOnFalseValue +1  ; → [2] Apply
	// [2] Apply                  ; branch target
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpPull},
		{Op: OpBranchOnFalseValue, Arg: 1},
		{Op: OpApply},
	}
	tpl.sourceRefs = make([]uint16, 3)

	tpl.Optimize()

	// Pull+Apply should NOT be fused because Apply at [2] is a branch target.
	qt.Assert(t, opcodes(tpl), qt.DeepEquals,
		[]OpCode{OpPull, OpBranchOnFalseValue, OpApply})
}

// --- Internal Helpers ---

func TestWritesValueRegister(t *testing.T) {
	writers := []OpCode{OpLoadVoid, OpLoadLiteral, OpLoadGlobal, OpLoadLocal, OpPop, OpPull, OpPeekK}
	for _, op := range writers {
		qt.Assert(t, writesValueRegister(op), qt.IsTrue, qt.Commentf("%s", op))
	}

	nonWriters := []OpCode{OpPush, OpApply, OpBranch, OpComplex, OpStoreGlobal, OpDrop, OpPopEnv,
		OpPushLiteral, OpPushGlobal, OpPushLocal, OpPullApply}
	for _, op := range nonWriters {
		qt.Assert(t, writesValueRegister(op), qt.IsFalse, qt.Commentf("%s", op))
	}
}

func TestIsBranchOp(t *testing.T) {
	branches := []OpCode{OpBranch, OpBranchOnFalseValue, OpSaveContinuation}
	for _, op := range branches {
		qt.Assert(t, isBranchOp(op), qt.IsTrue, qt.Commentf("%s", op))
	}

	nonBranches := []OpCode{OpPush, OpLoadVoid, OpApply, OpComplex, OpPullApply}
	for _, op := range nonBranches {
		qt.Assert(t, isBranchOp(op), qt.IsFalse, qt.Commentf("%s", op))
	}
}

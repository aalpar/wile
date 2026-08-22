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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

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
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))

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
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))

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
	tpl.sourceTableRefs = make(values.SourceTableRefs, 5)

	tpl.Optimize()

	// After: [0] SaveContinuation +3  [1] PushLiteral  [2] LoadGlobal
	qt.Assert(t, len(tpl.code), qt.Equals, 3)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpSaveContinuation)
	qt.Assert(t, tpl.code[0].Arg, qt.Equals, int32(3),
		qt.Commentf("sentinel target should remap to new len(code)"))
}

// --- Source Map ---

func TestPeephole_SourceTableRefsParallel(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpStoreGlobal, Arg: 0}, // sourceRef 1
		{Op: OpLoadVoid},            // sourceRef 2 (dead)
		{Op: OpLoadLiteral, Arg: 1}, // sourceRef 3
		{Op: OpPush},                // sourceRef 4 (fused with LoadLiteral)
	}
	tpl.sourceTableRefs = values.SourceTableRefs{1, 2, 3, 4}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, len(tpl.sourceTableRefs))
	// LoadVoid removed; LoadLiteral+Push fused to PushLiteral with Load's sourceRef.
	qt.Assert(t, tpl.sourceTableRefs, qt.DeepEquals, values.SourceTableRefs{1, 3})
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
	tpl.sourceTableRefs = values.SourceTableRefs{0}

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
	tpl.sourceTableRefs = values.SourceTableRefs{0, 0, 0}

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
	tpl.sourceTableRefs = values.SourceTableRefs{0, 0}

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
	sub.sourceTableRefs = values.SourceTableRefs{0, 0, 0, 0}

	// Parent template stores sub in its literals pool
	parent := NewEmptyNativeTemplate()
	parent.code = []Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
	}
	parent.sourceTableRefs = values.SourceTableRefs{0, 0}
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
	tpl.sourceTableRefs = values.SourceTableRefs{0}
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
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))

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
	tpl.sourceTableRefs = values.SourceTableRefs{5, 6}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, 1)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpPushLiteral)
	// Fused instruction inherits sourceRef from the Load, not the Push.
	qt.Assert(t, tpl.sourceTableRefs[0], qt.Equals, uint32(5))
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
	tpl.sourceTableRefs = make(values.SourceTableRefs, 5)

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
	tpl.sourceTableRefs = make(values.SourceTableRefs, 6)

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
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))

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
	tpl.sourceTableRefs = values.SourceTableRefs{5, 6}

	tpl.Optimize()

	qt.Assert(t, len(tpl.code), qt.Equals, 1)
	qt.Assert(t, tpl.code[0].Op, qt.Equals, OpPullApply)
	// Fused instruction inherits sourceRef from Pull, not Apply.
	qt.Assert(t, tpl.sourceTableRefs[0], qt.Equals, uint32(5))
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
	tpl.sourceTableRefs = make(values.SourceTableRefs, 3)

	tpl.Optimize()

	// Pull+Apply should NOT be fused because Apply at [2] is a branch target.
	qt.Assert(t, opcodes(tpl), qt.DeepEquals,
		[]OpCode{OpPull, OpBranchOnFalseValue, OpApply})
}

// --- Internal Helpers ---

func TestWritesValueRegister(t *testing.T) {
	writers := []OpCode{OpLoadVoid, OpLoadLiteral, OpLoadGlobal, OpLoadLocal, OpLoadCachedBinding, OpPop, OpPull, OpPeekK, OpMakeClosure}
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

	nonBranches := []OpCode{OpPush, OpLoadVoid, OpApply, OpComplex, OpPullApply, OpMakeClosure}
	for _, op := range nonBranches {
		qt.Assert(t, isBranchOp(op), qt.IsFalse, qt.Commentf("%s", op))
	}
}

func TestIsPushOp(t *testing.T) {
	pushOps := []OpCode{OpPush, OpPushLiteral, OpPushGlobal, OpPushLocal, OpPushCachedBinding}
	for _, op := range pushOps {
		qt.Assert(t, isPushOp(op), qt.IsTrue, qt.Commentf("%s", op))
	}

	nonPushOps := []OpCode{OpPull, OpApply, OpLoadVoid, OpBranch, OpComplex, OpPullApply, OpLoadLocal}
	for _, op := range nonPushOps {
		qt.Assert(t, isPushOp(op), qt.IsFalse, qt.Commentf("%s", op))
	}
}

// --- FuseCallForeignCached ---

// makeForeignBinding creates a *Binding holding a *ForeignClosure suitable
// for peephole tests. The closure's function body is a no-op.
func makeForeignBinding() *environment.Binding {
	env := environment.NewNamespace().Runtime()
	fc := NewForeignClosure(env, 0, false, func(_ CallContext) error {
		return nil
	})
	return environment.NewBinding(fc, environment.BindingTypeVariable)
}

// makeMachineClosureBinding creates a *Binding holding a *MachineClosure
// (non-foreign) for negative tests.
func makeMachineClosureBinding() *environment.Binding {
	env := environment.NewNamespace().Runtime()
	mc := NewClosureWithTemplate(NewNativeTemplate(0, 0, false), env)
	return environment.NewBinding(mc, environment.BindingTypeVariable)
}

func TestFuseCallForeignCached(t *testing.T) {
	foreignBinding := makeForeignBinding()
	machineBinding := makeMachineClosureBinding()

	tests := []struct {
		name           string
		code           []Instruction
		cachedBindings []*environment.Binding
		wantOps        []OpCode
		wantArg        map[int]int32
	}{
		{
			name: "non-tail: SaveCont + PushCachedBinding + PullApply",
			// SaveCont(+3) PushCachedBinding(0) PullApply — offset targets return point
			// Keeps SaveCont for stack isolation; only PushCachedBinding deleted.
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{foreignBinding},
			wantOps:        []OpCode{OpSaveContinuation, OpCallForeignCached},
			wantArg:        map[int]int32{0: 2, 1: 0},
		},
		{
			name: "non-tail with args: SaveCont + PushCachedBinding + PushLocal + PushLocal + PullApply",
			// SaveCont(+5) PushCachedBinding(0) PushLocal(0) PushLocal(1) PullApply — offset targets return point
			// Keeps SaveCont; only PushCachedBinding deleted.
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 5},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{foreignBinding},
			wantOps:        []OpCode{OpSaveContinuation, OpPushLocal, OpPushLocal, OpCallForeignCached},
			wantArg:        map[int]int32{0: 4, 1: 0, 2: 1, 3: 0},
		},
		{
			name: "tail: PushCachedBinding + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{foreignBinding},
			wantOps:        []OpCode{OpCallForeignCachedTail},
			wantArg:        map[int]int32{0: 0},
		},
		{
			// Frame reclaim inside a merged `let` body puts a release between the
			// args and the tail apply. The callee is a cached binding, resolved off
			// the template rather than through mc.env, so the fusion is sound and
			// the release stays ahead of it.
			name: "tail across a ReleaseEnvFrame: PushCachedBinding + arg + release + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpReleaseEnvFrame},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{foreignBinding},
			wantOps:        []OpCode{OpPushLocal, OpReleaseEnvFrame, OpCallForeignCachedTail},
			wantArg:        map[int]int32{0: 3, 2: 0},
		},
		{
			// THE ARITY PIN. A promoted primitive only fuses when the argument
			// count equals its arity, and the release is NOT an argument. Counting
			// it (pullIdx-i-1 == 3 against `+`'s arity 2) refuses the promotion
			// silently, so this row fails as a MISSING OpAddTail rather than as a
			// wrong answer — which is why it is pinned rather than left to the
			// value-assertion suite.
			name: "tail promoted across a ReleaseEnvFrame: release is not counted as an argument",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpReleaseEnvFrame},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makePromotedBinding("+")},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpReleaseEnvFrame, OpAddTail},
			wantArg:        map[int]int32{0: 0, 1: 1, 3: 0},
		},
		{
			name: "tail with args: PushCachedBinding + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{foreignBinding},
			wantOps:        []OpCode{OpPushLocal, OpCallForeignCachedTail},
			wantArg:        map[int]int32{0: 0, 1: 0},
		},
		{
			name: "non-foreign binding: fused to CallCachedBinding by pass 3",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{machineBinding},
			wantOps:        []OpCode{OpSaveContinuation, OpCallCachedBinding},
			wantArg:        map[int]int32{0: 2, 1: 0},
		},
		{
			name: "no match: branch target in interior",
			// Branch targets PushCachedBinding, preventing fusion.
			code: []Instruction{
				{Op: OpBranch, Arg: 2},
				{Op: OpSaveContinuation, Arg: 4},
				{Op: OpPushCachedBinding, Arg: 0}, // branch target
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{foreignBinding},
			wantOps:        []OpCode{OpBranch, OpSaveContinuation, OpPushCachedBinding, OpPushLocal, OpPullApply},
		},
		{
			name: "no match: SaveCont offset doesn't land on PullApply",
			// SaveCont offset points to PushLocal, not PullApply.
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 2},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{foreignBinding},
			wantOps:        []OpCode{OpSaveContinuation, OpPushCachedBinding, OpPushLocal, OpPullApply},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))
			tpl.cachedBindings = tt.cachedBindings

			tpl.Optimize()

			qt.Assert(t, opcodes(tpl), qt.DeepEquals, tt.wantOps)
			for idx, expectedArg := range tt.wantArg {
				qt.Assert(t, tpl.code[idx].Arg, qt.Equals, expectedArg,
					qt.Commentf("instruction %d (%s)", idx, tpl.code[idx].Op))
			}
		})
	}
}

// --- FusePromotedCompoundArgs (pass 4) ---

// promotedIdentityForName returns the identity token of the promoted primitive
// with the given Scheme name, or nil if no promoted op has that name. It is what
// lets the test factories below stand in for the registry, which stamps the same
// token onto the closures it mints.
func promotedIdentityForName(name string) *PrimitiveIdentity {
	for _, op := range promotedOps {
		if op.name == name {
			return op.identity
		}
	}
	return nil
}

// makePromotedBinding builds a *Binding holding a *ForeignClosure carrying a
// promoted primitive's identity (e.g. "+"), so promotedOpForIdentity resolves it.
func makePromotedBinding(name string) *environment.Binding {
	env := environment.NewNamespace().Runtime()
	fc := NewForeignClosure(env, 2, false, func(_ CallContext) error {
		return nil
	})
	fc.SetName(name)
	fc.SetIdentity(promotedIdentityForName(name))
	return environment.NewBinding(fc, environment.BindingTypeVariable)
}

// runPromotedCompoundPass runs only the pass-4 fusion in isolation so the
// expected output is deterministic (passes 1–3 would otherwise rewrite the
// inner calls used to build compound arguments).
func runPromotedCompoundPass(tpl *NativeTemplate) {
	plan := NewEditPlan(tpl)
	fusePromotedCompoundArgs(tpl, plan)
	plan.Apply()
}

// TestFusePromotedCompoundArgs covers the tail-position promotion of a
// primitive call whose arguments are compound subexpressions — the shape
// fuseCallForeignCached rejects (it requires all-push args). The canonical
// case is fib's (+ (fib (- n 1)) (fib (- n 2))): two SaveContinuation-delimited
// arguments around a tail `+`.
func TestFusePromotedCompoundArgs(t *testing.T) {
	plus := makePromotedBinding("+")
	machine := makeMachineClosureBinding()
	unnamedForeign := makeForeignBinding()

	tests := []struct {
		name           string
		code           []Instruction
		cachedBindings []*environment.Binding
		wantOps        []OpCode
		wantArg        map[int]int32
	}{
		{
			// Without OpReleaseEnvFrame ahead of the apply there is no
			// frame-reclaim proof, so the call must NOT be promoted: an argument
			// might capture a continuation that re-enters needing this frame's
			// locals (the `map` hazard — its callback can call/cc). This is the
			// exact shape that previously miscompiled.
			name: "no ReleaseEnvFrame proof: not promoted",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 1}, // 0: +
				{Op: OpSaveContinuation, Arg: 3},  // 1: arg1 → resume 4
				{Op: OpPushLocal, Arg: 0},         // 2
				{Op: OpPullApply},                 // 3: inner apply
				{Op: OpPush},                      // 4: arg1 result
				{Op: OpSaveContinuation, Arg: 3},  // 5: arg2 → resume 8
				{Op: OpPushLocal, Arg: 1},         // 6
				{Op: OpPullApply},                 // 7: inner apply
				{Op: OpPush},                      // 8: arg2 result
				{Op: OpPullApply},                 // 9: + apply (no preceding ReleaseEnvFrame)
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpPullApply,
			},
		},
		{
			// The real immutable-top-level fib shape: a frame-reclaim
			// OpReleaseEnvFrame sits between the last arg push and the tail
			// PullApply. It is stack-neutral, must be skipped during the arg
			// walk, and must remain ahead of the promoted tail op.
			name: "frame-reclaim ReleaseEnvFrame before tail apply is preserved",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 1}, // 0: +
				{Op: OpSaveContinuation, Arg: 3},  // 1: arg1 → resume 4
				{Op: OpPushLocal, Arg: 0},         // 2
				{Op: OpPullApply},                 // 3
				{Op: OpPush},                      // 4: arg1 result
				{Op: OpSaveContinuation, Arg: 3},  // 5: arg2 → resume 8
				{Op: OpPushLocal, Arg: 1},         // 6
				{Op: OpPullApply},                 // 7
				{Op: OpPush},                      // 8: arg2 result
				{Op: OpReleaseEnvFrame},           // 9: frame-reclaim before tail +
				{Op: OpPullApply},                 // 10: + apply
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpReleaseEnvFrame, OpAddTail,
			},
			wantArg: map[int]int32{0: 3, 4: 3, 9: 1},
		},
		{
			name: "wrong arity (three compound args) is left alone",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 1}, // 0: +
				{Op: OpSaveContinuation, Arg: 3},  // 1 → 4
				{Op: OpPushLocal, Arg: 0},         // 2
				{Op: OpPullApply},                 // 3
				{Op: OpPush},                      // 4
				{Op: OpSaveContinuation, Arg: 3},  // 5 → 8
				{Op: OpPushLocal, Arg: 1},         // 6
				{Op: OpPullApply},                 // 7
				{Op: OpPush},                      // 8
				{Op: OpSaveContinuation, Arg: 3},  // 9 → 12
				{Op: OpPushLocal, Arg: 2},         // 10
				{Op: OpPullApply},                 // 11
				{Op: OpPush},                      // 12
				{Op: OpPullApply},                 // 13
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpPullApply,
			},
		},
		{
			name: "non-promoted foreign callee is left alone",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0}, // unnamed foreign → not promoted
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
				{Op: OpPush},
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
				{Op: OpPush},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{unnamedForeign, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpPullApply,
			},
		},
		{
			name: "non-tail (preceded by SaveContinuation) is left alone",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 10}, // outer non-tail context
				{Op: OpPushCachedBinding, Arg: 1}, // + — callee, but preceded by SaveCont
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
				{Op: OpPush},
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
				{Op: OpPush},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpSaveContinuation, OpPushCachedBinding, OpSaveContinuation, OpPushLocal,
				OpPullApply, OpPush, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpPullApply,
			},
		},
		{
			// A branch targeting the tail PullApply makes it a control-flow join:
			// the inline op's fixed Pop(arity) would run on a stack shaped by the
			// branch predecessor, so the fusion must bail. Exercises the
			// targets[pullIdx] guard. A Branch-family op is required — a
			// SaveContinuation resume must land on a push, never on the apply.
			name: "branch targeting the tail apply (control-flow join) is left alone",
			code: []Instruction{
				{Op: OpBranchOnFalseValue, Arg: 11}, // 0: → 11, the tail apply
				{Op: OpPushCachedBinding, Arg: 1},   // 1: + (preceded by a branch, so tail)
				{Op: OpSaveContinuation, Arg: 3},    // 2: arg1 → resume 5
				{Op: OpPushLocal, Arg: 0},           // 3
				{Op: OpPullApply},                   // 4
				{Op: OpPush},                        // 5: arg1 result
				{Op: OpSaveContinuation, Arg: 3},    // 6: arg2 → resume 9
				{Op: OpPushLocal, Arg: 1},           // 7
				{Op: OpPullApply},                   // 8
				{Op: OpPush},                        // 9: arg2 result
				{Op: OpReleaseEnvFrame},             // 10
				{Op: OpPullApply},                   // 11: + apply (branch target)
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpBranchOnFalseValue, OpPushCachedBinding, OpSaveContinuation, OpPushLocal,
				OpPullApply, OpPush, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpReleaseEnvFrame, OpPullApply,
			},
		},
		{
			// ReleaseEnvFrame as the final instruction: pullIdx == len(code), so
			// the pullIdx >= len(code) bound must decline (and must not index
			// code[pullIdx] out of range).
			name: "ReleaseEnvFrame as the final instruction is left alone",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 1}, // 0: +
				{Op: OpSaveContinuation, Arg: 3},  // 1: arg1 → resume 4
				{Op: OpPushLocal, Arg: 0},         // 2
				{Op: OpPullApply},                 // 3
				{Op: OpPush},                      // 4
				{Op: OpSaveContinuation, Arg: 3},  // 5: arg2 → resume 8
				{Op: OpPushLocal, Arg: 1},         // 6
				{Op: OpPullApply},                 // 7
				{Op: OpPush},                      // 8
				{Op: OpReleaseEnvFrame},           // 9: termIdx, last instr (pullIdx 10 == len)
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpReleaseEnvFrame,
			},
		},
		{
			// ReleaseEnvFrame followed by a non-PullApply op: the in-range
			// counterpart to the case above, exercising the
			// code[pullIdx].Op != OpPullApply arm of the terminator check.
			name: "ReleaseEnvFrame not followed by PullApply is left alone",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 1}, // 0: +
				{Op: OpSaveContinuation, Arg: 3},  // 1
				{Op: OpPushLocal, Arg: 0},         // 2
				{Op: OpPullApply},                 // 3
				{Op: OpPush},                      // 4
				{Op: OpSaveContinuation, Arg: 3},  // 5
				{Op: OpPushLocal, Arg: 1},         // 6
				{Op: OpPullApply},                 // 7
				{Op: OpPush},                      // 8
				{Op: OpReleaseEnvFrame},           // 9: termIdx
				{Op: OpPush},                      // 10: pullIdx — not a PullApply
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpReleaseEnvFrame, OpPush,
			},
		},
		{
			// Callee binding holds a *MachineClosure, not a *ForeignClosure, so the
			// type assertion fails and the call stays on the generic path.
			name: "callee binding is not a ForeignClosure is left alone",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0}, // 0: machine closure (index 0)
				{Op: OpSaveContinuation, Arg: 3},  // 1
				{Op: OpPushLocal, Arg: 0},         // 2
				{Op: OpPullApply},                 // 3
				{Op: OpPush},                      // 4
				{Op: OpSaveContinuation, Arg: 3},  // 5
				{Op: OpPushLocal, Arg: 1},         // 6
				{Op: OpPullApply},                 // 7
				{Op: OpPush},                      // 8
				{Op: OpReleaseEnvFrame},           // 9
				{Op: OpPullApply},                 // 10
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpReleaseEnvFrame, OpPullApply,
			},
		},
		{
			// Callee push references a cached-binding index past the end of the
			// slice; the int(bindingIdx) >= len(cachedBindings) bound must decline
			// before indexing.
			name: "callee binding index out of range is left alone",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 5}, // 0: 5 >= len(cachedBindings) == 2
				{Op: OpSaveContinuation, Arg: 3},  // 1
				{Op: OpPushLocal, Arg: 0},         // 2
				{Op: OpPullApply},                 // 3
				{Op: OpPush},                      // 4
				{Op: OpSaveContinuation, Arg: 3},  // 5
				{Op: OpPushLocal, Arg: 1},         // 6
				{Op: OpPullApply},                 // 7
				{Op: OpPush},                      // 8
				{Op: OpReleaseEnvFrame},           // 9
				{Op: OpPullApply},                 // 10
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpSaveContinuation, OpPushLocal, OpPullApply, OpPush,
				OpSaveContinuation, OpPushLocal, OpPullApply, OpPush, OpReleaseEnvFrame, OpPullApply,
			},
		},
		{
			// The defect this pass shipped with: the callee push was identified
			// as "not preceded by SaveContinuation or a push", which is not the
			// question OpPullApply asks. The real callee (index 0) is still on
			// the stack when the OpStoreLocal at 2 pops the set!'s value, so the
			// promoted push at 3 looked like a fresh group start — and promoting
			// it deletes the REAL callee's push and rewrites the call.
			// `(h (begin (set! a 1) car) n)` compiled to `(car n)`.
			name: "promoted push over a non-empty stack is not the callee",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0}, // 0: the real callee, depth 0→1
				{Op: OpPushLiteral, Arg: 0},       // 1: set!'s value,   depth 1→2
				{Op: OpStoreLocal, Arg: 0},        // 2: set! pops,      depth 2→1
				{Op: OpPushCachedBinding, Arg: 1}, // 3: + — NOT the group's bottom
				{Op: OpPushLocal, Arg: 0},         // 4
				{Op: OpPushLocal, Arg: 1},         // 5
				{Op: OpReleaseEnvFrame},           // 6
				{Op: OpPullApply},                 // 7
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpPushCachedBinding, OpPushLiteral, OpStoreLocal, OpPushCachedBinding,
				OpPushLocal, OpPushLocal, OpReleaseEnvFrame, OpPullApply,
			},
		},
		{
			// The height guard at the apply, which is the one fact walkCallArgs
			// structurally cannot supply: the opcode sequence 1..5 is exactly the
			// promotable shape, and the branch into the argument region changes
			// the stack height without changing a single opcode. targets[pullIdx]
			// does not see it either — the branch lands at 4, not at the apply.
			name: "branch into the argument region (wrong height at the apply) is left alone",
			code: []Instruction{
				{Op: OpBranchOnFalseValue, Arg: 4}, // 0: → 4, skipping both arg pushes
				{Op: OpPushCachedBinding, Arg: 1},  // 1: + — group bottom, stack empty
				{Op: OpPushLocal, Arg: 0},          // 2
				{Op: OpPushLocal, Arg: 1},          // 3
				{Op: OpReleaseEnvFrame},            // 4: reached at depth 3 and at 0
				{Op: OpPullApply},                  // 5: height unknown, not arity+1
			},
			cachedBindings: []*environment.Binding{machine, plus},
			wantOps: []OpCode{
				OpBranchOnFalseValue, OpPushCachedBinding, OpPushLocal, OpPushLocal,
				OpReleaseEnvFrame, OpPullApply,
			},
		},
		{
			// Fewer than two instructions: the len(code) < 2 early return.
			name: "code shorter than two instructions is left alone",
			code: []Instruction{
				{Op: OpPullApply}, // 0
			},
			cachedBindings: []*environment.Binding{plus},
			wantOps:        []OpCode{OpPullApply},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))
			tpl.cachedBindings = tt.cachedBindings

			runPromotedCompoundPass(tpl)

			qt.Assert(t, opcodes(tpl), qt.DeepEquals, tt.wantOps)
			for idx, expectedArg := range tt.wantArg {
				qt.Assert(t, tpl.code[idx].Arg, qt.Equals, expectedArg,
					qt.Commentf("instruction %d (%s)", idx, tpl.code[idx].Op))
			}
		})
	}
}

// walkCallArgs is the argument-region scanner shared by the promoted-tail
// passes. These cases pin its contract directly: a malformed SaveContinuation
// offset — notably a negative one, which a len-only bound check would index out
// of range and panic on — is rejected as ok=false, while a bare terminator
// opcode is returned as termIdx with ok=true for the caller to validate.
func TestWalkCallArgs(t *testing.T) {
	tests := []struct {
		name        string
		code        []Instruction
		start       int
		wantTermIdx int
		wantArgs    int
		wantOK      bool
	}{
		{
			name:   "negative SaveContinuation offset is rejected without panicking",
			code:   []Instruction{{Op: OpSaveContinuation, Arg: -1}, {Op: OpPush}},
			start:  0,
			wantOK: false,
		},
		{
			name:   "non-advancing (zero) offset is rejected",
			code:   []Instruction{{Op: OpSaveContinuation, Arg: 0}, {Op: OpPush}},
			start:  0,
			wantOK: false,
		},
		{
			name:   "forward offset past the end is rejected",
			code:   []Instruction{{Op: OpSaveContinuation, Arg: 5}, {Op: OpPush}},
			start:  0,
			wantOK: false,
		},
		{
			name: "offset not landing on a result push is rejected",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 2}, {Op: OpPush}, {Op: OpPullApply}, {Op: OpPush},
			},
			start:  0,
			wantOK: false,
		},
		{
			name: "two simple push args, terminator is PullApply",
			code: []Instruction{
				{Op: OpPush}, {Op: OpPushLocal}, {Op: OpPullApply},
			},
			start:       0,
			wantTermIdx: 2,
			wantArgs:    2,
			wantOK:      true,
		},
		{
			name: "compound arg skipped via forward offset, ReleaseEnvFrame terminator",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 3}, {Op: OpPushLocal}, {Op: OpPullApply},
				{Op: OpPush}, {Op: OpReleaseEnvFrame}, {Op: OpPullApply},
			},
			start:       0,
			wantTermIdx: 4,
			wantArgs:    1,
			wantOK:      true,
		},
		{
			name: "bare branch terminator is returned with ok=true (caller validates)",
			code: []Instruction{
				{Op: OpPush}, {Op: OpBranch},
			},
			start:       0,
			wantTermIdx: 1,
			wantArgs:    1,
			wantOK:      true,
		},
		{
			name: "running off the end without a terminator is rejected",
			code: []Instruction{
				{Op: OpPush}, {Op: OpPushLocal},
			},
			start:  0,
			wantOK: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			termIdx, argCount, ok := walkCallArgs(tt.code, tt.start)
			qt.Assert(t, ok, qt.Equals, tt.wantOK)
			if tt.wantOK {
				qt.Assert(t, termIdx, qt.Equals, tt.wantTermIdx)
				qt.Assert(t, argCount, qt.Equals, tt.wantArgs)
			}
		})
	}
}

// --- FuseCallGeneric (pass 3) ---

func TestFuseCallGeneric(t *testing.T) {
	machineBinding := makeMachineClosureBinding()

	tests := []struct {
		name           string
		code           []Instruction
		cachedBindings []*environment.Binding
		wantOps        []OpCode
		wantArg        map[int]int32
	}{
		{
			name: "non-tail CallLocal: SaveCont + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushLocal, Arg: 42},
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpSaveContinuation, OpCallLocal},
			wantArg: map[int]int32{0: 2, 1: 42},
		},
		{
			name: "non-tail CallLocal with args: SaveCont + PushLocal + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 5},
				{Op: OpPushLocal, Arg: 42},
				{Op: OpPushLocal, Arg: 10},
				{Op: OpPushLocal, Arg: 11},
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpSaveContinuation, OpPushLocal, OpPushLocal, OpCallLocal},
			wantArg: map[int]int32{0: 4, 1: 10, 2: 11, 3: 42},
		},
		{
			name: "tail CallLocal: PushLocal + PullApply (no SaveCont)",
			code: []Instruction{
				{Op: OpPushLocal, Arg: 7},
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpCallLocal},
			wantArg: map[int]int32{0: 7},
		},
		{
			name: "tail CallLocal with args: PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushLocal, Arg: 7},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpPushLocal, OpCallLocal},
			wantArg: map[int]int32{0: 3, 1: 7},
		},
		{
			// THE SOUNDNESS CASE. Frame reclaim inside a merged `let` body puts an
			// OpReleaseEnvFrame between the last argument push and the tail apply.
			// Fusing here would move the callee's resolution to the apply, i.e.
			// AFTER mc.env has been handed to the pool and zeroed by ResetForPool —
			// OpCallLocal would read slot 7 out of a recycled frame. The scan must
			// stop at the release and leave the whole sequence alone.
			name: "tail local callee is NOT fused across a ReleaseEnvFrame",
			code: []Instruction{
				{Op: OpPushLocal, Arg: 7},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpReleaseEnvFrame},
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpPushLocal, OpPushLocal, OpReleaseEnvFrame, OpPullApply},
			wantArg: map[int]int32{0: 7, 1: 3},
		},
		{
			// The other half of the split: a cached binding is resolved off the
			// template, never through mc.env, so the release cannot invalidate it
			// and the fusion is taken. The release must survive AHEAD of the fused
			// call — deleting it would silently retract the frame reclaim.
			name: "tail cached-binding callee IS fused across a ReleaseEnvFrame",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpReleaseEnvFrame},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{machineBinding},
			wantOps:        []OpCode{OpPushLocal, OpReleaseEnvFrame, OpCallCachedBinding},
			wantArg:        map[int]int32{0: 3, 2: 0},
		},
		{
			// A free callee reads mc.free, the running closure's own vector, which
			// a flat closure copied by value at creation. Independent of mc.env, so
			// it fuses too — and this is the row that matters most in higher-order
			// code, where the callee is nearly always captured.
			name: "tail free callee IS fused across a ReleaseEnvFrame",
			code: []Instruction{
				{Op: OpPushFree, Arg: 5},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpReleaseEnvFrame},
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpPushLocal, OpReleaseEnvFrame, OpCallFree},
			wantArg: map[int]int32{0: 3, 2: 5},
		},
		{
			name: "non-tail CallCachedBinding: SaveCont + PushCachedBinding(machine closure) + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{machineBinding},
			wantOps:        []OpCode{OpSaveContinuation, OpCallCachedBinding},
			wantArg:        map[int]int32{0: 2, 1: 0},
		},
		{
			name: "tail CallCachedBinding: PushCachedBinding(machine closure) + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{machineBinding},
			wantOps:        []OpCode{OpCallCachedBinding},
			wantArg:        map[int]int32{0: 0},
		},
		{
			name: "no match: branch target on callee push",
			code: []Instruction{
				{Op: OpBranch, Arg: 1},
				{Op: OpPushLocal, Arg: 7}, // branch target
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpBranch, OpPushLocal, OpPullApply},
		},
		{
			name: "no match: PushLocal preceded by SaveCont is non-tail, requires offset match",
			// SaveCont offset 2 → pullIdx = 0+2-1 = 1 (PushLocal, not PullApply).
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 2},
				{Op: OpPushLocal, Arg: 7},
				{Op: OpPullApply},
			},
			wantOps: []OpCode{OpSaveContinuation, OpPushLocal, OpPullApply},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))
			tpl.cachedBindings = tt.cachedBindings

			tpl.Optimize()

			qt.Assert(t, opcodes(tpl), qt.DeepEquals, tt.wantOps)
			for idx, expectedArg := range tt.wantArg {
				qt.Assert(t, tpl.code[idx].Arg, qt.Equals, expectedArg,
					qt.Commentf("instruction %d (%s)", idx, tpl.code[idx].Op))
			}
		})
	}
}

// --- Promoted Primitive Fusion ---

// makeNamedForeignBinding creates a *Binding holding a *ForeignClosure with the
// given name and parameter count, stamped with that name's promoted identity if
// it has one. A name no promoted op claims (e.g. "length") leaves the closure
// unstamped, which is the negative case the fusion table exercises.
func makeNamedForeignBinding(name string, paramCount int) *environment.Binding {
	env := environment.NewNamespace().Runtime()
	fc := NewForeignClosure(env, paramCount, false, func(_ CallContext) error {
		return nil
	})
	fc.SetName(name)
	fc.SetIdentity(promotedIdentityForName(name))
	return environment.NewBinding(fc, environment.BindingTypeVariable)
}

func TestFusePromotedPrimitives(t *testing.T) {
	tests := []struct {
		name           string
		code           []Instruction
		cachedBindings []*environment.Binding
		wantOps        []OpCode
		wantArg        map[int]int32
	}{
		{
			name: "non-tail eq?: SaveCont + PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 5},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("eq?", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpEqQ},
			wantArg:        map[int]int32{0: 1, 1: 2, 2: 0},
		},
		{
			name: "tail eq?: PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("eq?", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpEqQTail},
			wantArg:        map[int]int32{0: 1, 1: 2, 2: 0},
		},
		{
			name: "non-tail vector?: SaveCont + PushCachedBinding + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 4},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 5},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("vector?", 1)},
			wantOps:        []OpCode{OpPushLocal, OpVectorQ},
			wantArg:        map[int]int32{0: 5, 1: 0},
		},
		{
			name: "tail vector?: PushCachedBinding + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 5},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("vector?", 1)},
			wantOps:        []OpCode{OpPushLocal, OpVectorQTail},
			wantArg:        map[int]int32{0: 5, 1: 0},
		},
		{
			name: "non-tail vector-ref: SaveCont + PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 5},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpPushLocal, Arg: 4},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("vector-ref", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpVectorRef},
			wantArg:        map[int]int32{0: 3, 1: 4, 2: 0},
		},
		{
			name: "tail vector-ref: PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpPushLocal, Arg: 4},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("vector-ref", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpVectorRefTail},
			wantArg:        map[int]int32{0: 3, 1: 4, 2: 0},
		},
		{
			name: "non-tail cons: SaveCont + PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 5},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("cons", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpCons},
			wantArg:        map[int]int32{0: 1, 1: 2, 2: 0},
		},
		{
			name: "tail cons: PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("cons", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpConsTail},
			wantArg:        map[int]int32{0: 1, 1: 2, 2: 0},
		},
		{
			name: "non-tail *: SaveCont + PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 5},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("*", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpMul},
			wantArg:        map[int]int32{0: 1, 1: 2, 2: 0},
		},
		{
			name: "tail /: PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("/", 2)},
			wantOps:        []OpCode{OpPushLocal, OpPushLocal, OpDivTail},
			wantArg:        map[int]int32{0: 1, 1: 2, 2: 0},
		},
		{
			name: "wrong arity: * with 3 args falls back to CallForeignCached",
			// Variadic * with 3 args doesn't match promoted arity (2).
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 6},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPushLocal, Arg: 3},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("*", 2)},
			wantOps:        []OpCode{OpSaveContinuation, OpPushLocal, OpPushLocal, OpPushLocal, OpCallForeignCached},
			wantArg:        map[int]int32{0: 5, 1: 1, 2: 2, 3: 3, 4: 0},
		},
		{
			name: "wrong arity: eq? with 1 arg falls back to CallForeignCached",
			// Not promoted (arity mismatch), so treated as generic CallForeignCached.
			// SaveCont is kept for stack isolation.
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 4},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("eq?", 2)},
			wantOps:        []OpCode{OpSaveContinuation, OpPushLocal, OpCallForeignCached},
			wantArg:        map[int]int32{0: 3, 1: 1, 2: 0},
		},
		{
			name: "non-promoted foreign: falls back to CallForeignCached",
			// Not promoted, so treated as generic CallForeignCached.
			// SaveCont is kept for stack isolation.
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 4},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
			},
			cachedBindings: []*environment.Binding{makeNamedForeignBinding("length", 1)},
			wantOps:        []OpCode{OpSaveContinuation, OpPushLocal, OpCallForeignCached},
			wantArg:        map[int]int32{0: 3, 1: 1, 2: 0},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl := NewEmptyNativeTemplate()
			tpl.code = make([]Instruction, len(tt.code))
			copy(tpl.code, tt.code)
			tpl.sourceTableRefs = make(values.SourceTableRefs, len(tt.code))
			tpl.cachedBindings = tt.cachedBindings

			tpl.Optimize()

			qt.Assert(t, opcodes(tpl), qt.DeepEquals, tt.wantOps)
			for idx, expectedArg := range tt.wantArg {
				qt.Assert(t, tpl.code[idx].Arg, qt.Equals, expectedArg,
					qt.Commentf("instruction %d (%s)", idx, tpl.code[idx].Op))
			}
		})
	}
}

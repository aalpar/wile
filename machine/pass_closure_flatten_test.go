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

// runPasses1and2 runs Pass 1 (FreeVarAnalysis) and Pass 2 (BoxInsertion)
// on the given template, returning the top-level FreeVarInfo.
func runPasses1and2(tpl *NativeTemplate) *FreeVarInfo {
	info := AnalyzeFreeVars(tpl)
	tpl.SetFreeVarInfo(info)
	InsertBoxes(tpl)
	return info
}

func TestFlattenClosures_NoFreeVars(t *testing.T) {
	// Template with only depth=0 references. FlattenClosures should not
	// change any bytecodes.
	tpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 0)},
		{Op: OpPush},
		{Op: OpLoadLocal, Arg: encodeLocal(1, 0)},
		{Op: OpRestoreContinuation},
	})

	runPasses1and2(tpl)
	FlattenClosures(tpl, nil)

	code := tpl.Code()
	qt.Assert(t, code, qt.HasLen, 4)
	qt.Assert(t, code[0].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, code[1].Op, qt.Equals, OpPush)
	qt.Assert(t, code[2].Op, qt.Equals, OpLoadLocal)
	qt.Assert(t, code[3].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_SimpleFreeVar(t *testing.T) {
	// Inner template references (slot=0, depth=1).
	// After flatten, the OpLoadLocal(0, 1) should become OpLoadFreeVar(0).
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	// Check inner template's code was rewritten.
	innerCode := innerTpl.Code()
	qt.Assert(t, innerCode, qt.HasLen, 2)
	qt.Assert(t, innerCode[0].Op, qt.Equals, OpLoadFreeVar)
	qt.Assert(t, innerCode[0].Arg, qt.Equals, int32(0))
	qt.Assert(t, innerCode[1].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_PushLocalFreeVar(t *testing.T) {
	// Inner template has OpPushLocal(0, 1). After flatten, it should
	// become OpLoadFreeVar(0) + OpPush.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpPushLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	innerCode := innerTpl.Code()
	qt.Assert(t, innerCode, qt.HasLen, 3)
	qt.Assert(t, innerCode[0].Op, qt.Equals, OpLoadFreeVar)
	qt.Assert(t, innerCode[0].Arg, qt.Equals, int32(0))
	qt.Assert(t, innerCode[1].Op, qt.Equals, OpPush)
	qt.Assert(t, innerCode[2].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_MultipleFreeVars(t *testing.T) {
	// Inner template references two different free variables.
	// Captures are sorted by (depth, slot): (0,1) → closureSlot 0, (1,1) → closureSlot 1.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(1, 1)},
		{Op: OpPush},
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	innerCode := innerTpl.Code()
	qt.Assert(t, innerCode, qt.HasLen, 4)

	// (slot=0, depth=1) gets closureSlot 0, (slot=1, depth=1) gets closureSlot 1.
	// First instruction references (1, 1) → closureSlot 1.
	qt.Assert(t, innerCode[0].Op, qt.Equals, OpLoadFreeVar)
	qt.Assert(t, innerCode[0].Arg, qt.Equals, int32(1))
	qt.Assert(t, innerCode[1].Op, qt.Equals, OpPush)
	// Third instruction references (0, 1) → closureSlot 0.
	qt.Assert(t, innerCode[2].Op, qt.Equals, OpLoadFreeVar)
	qt.Assert(t, innerCode[2].Arg, qt.Equals, int32(0))
	qt.Assert(t, innerCode[3].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_BoxedCaptureRewrite(t *testing.T) {
	// When a captured variable is boxed (Boxed=true in CaptureEntry),
	// Pass 3 rewrites OpLoadLocal(0, 1) → OpLoadFreeVar(closureSlot).
	// The freeVars array will hold the *values.Box at runtime;
	// unboxing is handled downstream (integration pass or runtime).
	//
	// Note: OpStoreLocal(slot, depth>0) is NOT rewritten by Pass 3 —
	// that requires a separate box-aware rewrite pass for capturing scopes.
	// Pass 2 only inserts box rewrites in the DEFINING scope (depth=0).
	//
	// This test verifies the Boxed flag is set and LoadLocal is rewritten.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},  // read captured var
		{Op: OpStoreLocal, Arg: encodeLocal(0, 1)}, // write (triggers boxing in outer)
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)

	// Verify the capture is marked Boxed after Pass 2.
	innerInfo := innerTpl.FreeVarInfo()
	qt.Assert(t, innerInfo, qt.IsNotNil)
	qt.Assert(t, innerInfo.Captures, qt.HasLen, 1)
	qt.Assert(t, innerInfo.Captures[0].Boxed, qt.IsTrue,
		qt.Commentf("capture should be boxed"))

	FlattenClosures(outerTpl, nil)

	// After flatten, the LoadLocal(0,1) should be LoadFreeVar(0).
	// The StoreLocal(0,1) remains (not handled by Pass 3).
	innerCode := innerTpl.Code()
	qt.Assert(t, innerCode[0].Op, qt.Equals, OpLoadFreeVar)
	qt.Assert(t, innerCode[0].Arg, qt.Equals, int32(0))
}

func TestFlattenClosures_ReadOnlyBoxedCapture(t *testing.T) {
	// A variable that is captured AND mutated gets boxed. When the inner
	// template only reads it (the mutation is in the outer scope via set!),
	// Pass 3 should rewrite the inner template's LoadLocal(0,1) to
	// LoadFreeVar(closureSlot). At runtime the freeVar holds the Box.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)}, // read
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpStoreLocal, Arg: encodeLocal(0, 0)}, // outer mutates → boxing
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)

	// Verify boxing was triggered.
	innerInfo := innerTpl.FreeVarInfo()
	qt.Assert(t, innerInfo, qt.IsNotNil)
	qt.Assert(t, innerInfo.Captures[0].Boxed, qt.IsTrue)

	FlattenClosures(outerTpl, nil)

	innerCode := innerTpl.Code()
	qt.Assert(t, innerCode[0].Op, qt.Equals, OpLoadFreeVar)
	qt.Assert(t, innerCode[0].Arg, qt.Equals, int32(0))
	qt.Assert(t, innerCode[1].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_FromFreeVarsDepth1(t *testing.T) {
	// Capture at SourceDepth == 1 should have FromFreeVars = false.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	innerInfo := innerTpl.FreeVarInfo()
	qt.Assert(t, innerInfo, qt.IsNotNil)
	qt.Assert(t, innerInfo.Captures, qt.HasLen, 1)
	qt.Assert(t, innerInfo.Captures[0].FromFreeVars, qt.IsFalse)
}

func TestFlattenClosures_FromFreeVarsDepthGreaterThan1(t *testing.T) {
	// Three levels: grandparent → parent → child.
	// Child captures (slot=0, depth=2), which passes through parent.
	// After flatten, the child's capture should have FromFreeVars=true
	// and SourceSlot rewritten to the parent's closureSlot.
	childTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 2)},
		{Op: OpRestoreContinuation},
	})

	parentTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	parentTpl.literals = append(parentTpl.literals, childTpl)

	grandparentTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	grandparentTpl.literals = append(grandparentTpl.literals, parentTpl)

	runPasses1and2(grandparentTpl)
	FlattenClosures(grandparentTpl, nil)

	// The child's capture (slot=0, depth=2) should be resolved:
	// - FromFreeVars = true (comes from parent's freeVars, not parent's locals)
	// - SourceSlot = parent's closureSlot for (slot=0, depth=1)
	childInfo := childTpl.FreeVarInfo()
	qt.Assert(t, childInfo, qt.IsNotNil)
	qt.Assert(t, childInfo.Captures, qt.HasLen, 1)

	entry := childInfo.Captures[0]
	qt.Assert(t, entry.FromFreeVars, qt.IsTrue,
		qt.Commentf("expected FromFreeVars=true for depth>1 capture"))

	// The parent's capture for (slot=0, depth=1) has closureSlot 0.
	parentInfo := parentTpl.FreeVarInfo()
	qt.Assert(t, parentInfo, qt.IsNotNil)
	qt.Assert(t, parentInfo.Captures, qt.HasLen, 1)
	qt.Assert(t, parentInfo.Captures[0].ClosureSlot, qt.Equals, 0)

	// The child's SourceSlot should be rewritten to the parent's closureSlot.
	qt.Assert(t, entry.SourceSlot, qt.Equals, 0,
		qt.Commentf("expected SourceSlot rewritten to parent's closureSlot"))
}

func TestFlattenClosures_MakeClosureToMakeFlatClosure(t *testing.T) {
	// Outer template creates a closure for an inner template that has captures.
	// The MakeClosure instruction sequence should be rewritten to MakeFlatClosure.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	// Simulate the compiled closure creation: LoadLiteral(tplIdx), Push,
	// LoadLiteral(envIdx), Push, MakeClosure.
	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0}, // tplIdx = 0 (will be innerTpl)
		{Op: OpPush},
		{Op: OpLoadLiteral, Arg: 1}, // envIdx = 1 (environment)
		{Op: OpPush},
		{Op: OpMakeClosure},
		{Op: OpRestoreContinuation},
	})

	// Set up the literal pool: [innerTpl, env placeholder]
	outerTpl.literals = append(outerTpl.literals, innerTpl)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	outerTpl.literals = append(outerTpl.literals, topEnv)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	// After flatten, the sequence should be:
	//   LoadLiteral(0), Push, MakeFlatClosure, RestoreContinuation
	// (3 instructions removed, 1 added → net -2)
	outerCode := outerTpl.Code()
	qt.Assert(t, outerCode, qt.HasLen, 4,
		qt.Commentf("expected 4 instructions, got %d", len(outerCode)))

	qt.Assert(t, outerCode[0].Op, qt.Equals, OpLoadLiteral)
	qt.Assert(t, outerCode[0].Arg, qt.Equals, int32(0))
	qt.Assert(t, outerCode[1].Op, qt.Equals, OpPush)
	qt.Assert(t, outerCode[2].Op, qt.Equals, OpMakeFlatClosure)
	qt.Assert(t, outerCode[3].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_MakeClosureUnchangedForNoCaptureSubTemplate(t *testing.T) {
	// Sub-template has no free variables. OpMakeClosure should NOT be rewritten.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 0)}, // depth=0, not a free var
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLiteral, Arg: 0},
		{Op: OpPush},
		{Op: OpLoadLiteral, Arg: 1},
		{Op: OpPush},
		{Op: OpMakeClosure},
		{Op: OpRestoreContinuation},
	})

	outerTpl.literals = append(outerTpl.literals, innerTpl)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	outerTpl.literals = append(outerTpl.literals, topEnv)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	// MakeClosure should remain unchanged.
	outerCode := outerTpl.Code()
	qt.Assert(t, outerCode, qt.HasLen, 6)
	qt.Assert(t, outerCode[4].Op, qt.Equals, OpMakeClosure)
}

func TestFlattenClosures_FusedPushLiteralPattern(t *testing.T) {
	// After peephole optimization, the closure creation might be:
	//   PushLiteral(tplIdx), PushLiteral(envIdx), MakeClosure
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpPushLiteral, Arg: 0}, // fused load+push template
		{Op: OpPushLiteral, Arg: 1}, // fused load+push env
		{Op: OpMakeClosure},
		{Op: OpRestoreContinuation},
	})

	outerTpl.literals = append(outerTpl.literals, innerTpl)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	outerTpl.literals = append(outerTpl.literals, topEnv)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	// After flatten: PushLiteral(0), MakeFlatClosure, RestoreContinuation
	outerCode := outerTpl.Code()
	qt.Assert(t, outerCode, qt.HasLen, 3,
		qt.Commentf("expected 3 instructions, got %d", len(outerCode)))

	qt.Assert(t, outerCode[0].Op, qt.Equals, OpPushLiteral)
	qt.Assert(t, outerCode[0].Arg, qt.Equals, int32(0))
	qt.Assert(t, outerCode[1].Op, qt.Equals, OpMakeFlatClosure)
	qt.Assert(t, outerCode[2].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_DepthZeroReferencesUntouched(t *testing.T) {
	// Free-var references at depth=0 should NOT be rewritten.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 0)}, // local, not free
		{Op: OpPush},
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)}, // free var
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	innerCode := innerTpl.Code()
	// The depth=0 LoadLocal should remain.
	qt.Assert(t, innerCode[0].Op, qt.Equals, OpLoadLocal)
	s, d := DecodeLocalIndex(innerCode[0].Arg)
	qt.Assert(t, s, qt.Equals, 0)
	qt.Assert(t, d, qt.Equals, 0)

	// The depth=1 LoadLocal should be rewritten to LoadFreeVar.
	// Find it (may have shifted due to PushLocal expansion).
	found := false
	for _, instr := range innerCode {
		if instr.Op == OpLoadFreeVar {
			found = true
			break
		}
	}
	qt.Assert(t, found, qt.IsTrue, qt.Commentf("expected OpLoadFreeVar for depth=1 reference"))
}

func TestFlattenClosures_BranchOffsetAdjustedAfterPushLocalExpansion(t *testing.T) {
	// When PushLocal is expanded to LoadFreeVar + Push, branch offsets
	// that span the expansion site must be adjusted.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpBranch, Arg: 2},                    // jump over PushLocal
		{Op: OpPushLocal, Arg: encodeLocal(0, 1)}, // free var, will expand
		{Op: OpRestoreContinuation},               // branch target
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	// After flatten: PushLocal becomes LoadFreeVar + Push (+1 instruction).
	// Branch offset should adjust from 2 to 3.
	innerCode := innerTpl.Code()
	qt.Assert(t, innerCode[0].Op, qt.Equals, OpBranch)
	qt.Assert(t, innerCode[0].Arg, qt.Equals, int32(3),
		qt.Commentf("branch offset should be 3, got %d", innerCode[0].Arg))
	qt.Assert(t, innerCode[1].Op, qt.Equals, OpLoadFreeVar)
	qt.Assert(t, innerCode[2].Op, qt.Equals, OpPush)
	qt.Assert(t, innerCode[3].Op, qt.Equals, OpRestoreContinuation)
}

func TestFlattenClosures_NoLoadLocalRemains(t *testing.T) {
	// After flatten, there should be NO OpLoadLocal or OpPushLocal with
	// depth > 0 in the inner template.
	innerTpl := makeTemplate([]Instruction{
		{Op: OpLoadLocal, Arg: encodeLocal(0, 1)},
		{Op: OpPushLocal, Arg: encodeLocal(1, 1)},
		{Op: OpLoadLocal, Arg: encodeLocal(2, 0)}, // depth=0, should remain
		{Op: OpRestoreContinuation},
	})

	outerTpl := makeTemplate([]Instruction{
		{Op: OpRestoreContinuation},
	})
	outerTpl.literals = append(outerTpl.literals, innerTpl)

	runPasses1and2(outerTpl)
	FlattenClosures(outerTpl, nil)

	for _, instr := range innerTpl.Code() {
		if instr.Op == OpLoadLocal || instr.Op == OpPushLocal {
			_, depth := DecodeLocalIndex(instr.Arg)
			qt.Assert(t, depth, qt.Equals, 0,
				qt.Commentf("found %s at depth=%d, expected all depth>0 rewritten", instr.Op, depth))
		}
	}
}

func TestMatchMakeClosurePattern_AllVariants(t *testing.T) {
	tcs := []struct {
		name         string
		code         []Instruction
		pc           int
		wantValid    bool
		wantTplLit   int32
		wantEnvStart int
	}{
		{
			name: "pattern A — fully unfused",
			code: []Instruction{
				{Op: OpLoadLiteral, Arg: 0},
				{Op: OpPush},
				{Op: OpLoadLiteral, Arg: 1},
				{Op: OpPush},
				{Op: OpMakeClosure},
			},
			pc:           4,
			wantValid:    true,
			wantTplLit:   0,
			wantEnvStart: 2,
		},
		{
			name: "pattern B — fully fused",
			code: []Instruction{
				{Op: OpPushLiteral, Arg: 0},
				{Op: OpPushLiteral, Arg: 1},
				{Op: OpMakeClosure},
			},
			pc:           2,
			wantValid:    true,
			wantTplLit:   0,
			wantEnvStart: 1,
		},
		{
			name: "pattern C — tpl unfused, env fused",
			code: []Instruction{
				{Op: OpLoadLiteral, Arg: 0},
				{Op: OpPush},
				{Op: OpPushLiteral, Arg: 1},
				{Op: OpMakeClosure},
			},
			pc:           3,
			wantValid:    true,
			wantTplLit:   0,
			wantEnvStart: 2,
		},
		{
			name: "pattern D — tpl fused, env unfused",
			code: []Instruction{
				{Op: OpPushLiteral, Arg: 0},
				{Op: OpLoadLiteral, Arg: 1},
				{Op: OpPush},
				{Op: OpMakeClosure},
			},
			pc:           3,
			wantValid:    true,
			wantTplLit:   0,
			wantEnvStart: 1,
		},
		{
			name: "no match — too few instructions",
			code: []Instruction{
				{Op: OpMakeClosure},
			},
			pc:        0,
			wantValid: false,
		},
		{
			name: "no match — wrong preceding ops",
			code: []Instruction{
				{Op: OpLoadGlobal, Arg: 0},
				{Op: OpPush},
				{Op: OpLoadGlobal, Arg: 1},
				{Op: OpPush},
				{Op: OpMakeClosure},
			},
			pc:        4,
			wantValid: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			match := matchMakeClosurePattern(tc.code, tc.pc)
			qt.Assert(t, match.valid, qt.Equals, tc.wantValid)
			if tc.wantValid {
				qt.Assert(t, match.tplLitIdx, qt.Equals, tc.wantTplLit)
				qt.Assert(t, match.envStart, qt.Equals, tc.wantEnvStart)
			}
		})
	}
}

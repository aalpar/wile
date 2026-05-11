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
	"unsafe"

	"github.com/aalpar/wile/environment"

	qt "github.com/frankban/quicktest"
)

func TestInstructionSize(t *testing.T) {
	c := qt.New(t)
	// Instruction should be 8 bytes: uint16 Op + 2 padding + int32 Arg
	c.Assert(unsafe.Sizeof(Instruction{}), qt.Equals, uintptr(8))
}

func TestInstructionStringZeroOperand(t *testing.T) {
	c := qt.New(t)
	instr := Instruction{Op: OpPush}
	c.Assert(instr.String(), qt.Equals, "Push")
}

func TestInstructionStringSingleOperand(t *testing.T) {
	c := qt.New(t)
	instr := Instruction{Op: OpBranch, Arg: 5}
	c.Assert(instr.String(), qt.Equals, "Branch 5")
}

func TestInstructionStringLocalIndex(t *testing.T) {
	c := qt.New(t)
	li := environment.NewLocalIndex(3, 2)
	instr := Instruction{Op: OpLoadLocal, Arg: EncodeLocalIndex(li)}
	c.Assert(instr.String(), qt.Equals, "LoadLocal slot=3 depth=2")
}

func TestLocalIndexBitPacking(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		slot  int
		depth int
	}{
		{0, 0},
		{1, 0},
		{0, 1},
		{5, 3},
		{127, 10},
		{32767, 32767}, // max int16
	}

	for _, tc := range tests {
		li := environment.NewLocalIndex(tc.slot, tc.depth)
		encoded := EncodeLocalIndex(li)
		slot, depth := DecodeLocalIndex(encoded)
		c.Assert(slot, qt.Equals, tc.slot,
			qt.Commentf("slot mismatch for input (%d, %d)", tc.slot, tc.depth))
		c.Assert(depth, qt.Equals, tc.depth,
			qt.Commentf("depth mismatch for input (%d, %d)", tc.slot, tc.depth))
	}
}

func TestInstructionStringComplex(t *testing.T) {
	c := qt.New(t)
	instr := Instruction{Op: OpComplex, Arg: 7}
	c.Assert(instr.String(), qt.Equals, "Complex 7")
}

func TestLocalIndexBitPackingEdgeCases(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name  string
		slot  int
		depth int
	}{
		{"negative slot", -1, 0},
		{"negative depth", 0, -1},
		{"both negative", -10, -20},
		{"min int16", -32768, -32768},
		{"mixed signs", -100, 200},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			li := environment.NewLocalIndex(tc.slot, tc.depth)
			encoded := EncodeLocalIndex(li)
			slot, depth := DecodeLocalIndex(encoded)
			c.Assert(slot, qt.Equals, tc.slot)
			c.Assert(depth, qt.Equals, tc.depth)
		})
	}
}

func TestOperationToInstructionWave3(t *testing.T) {
	c := qt.New(t)
	li := environment.NewLocalIndex(7, 4)

	tests := []struct {
		name       string
		op         Operation
		expectedOp OpCode
	}{
		{
			"LoadLocal",
			NewOperationLoadLocalByLocalIndexImmediate(li),
			OpLoadLocal,
		},
		{
			"StoreLocal",
			NewOperationStoreLocalByLocalIndexImmediate(li),
			OpStoreLocal,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			instr, ok := operationToInstruction(tc.op)
			c.Assert(ok, qt.IsTrue, qt.Commentf("operationToInstruction should succeed"))
			c.Assert(instr.Op, qt.Equals, tc.expectedOp)

			// Verify round-trip
			roundtrip := instructionToOperation(instr)
			c.Assert(roundtrip, qt.Not(qt.IsNil))
			c.Assert(tc.op.EqualTo(roundtrip), qt.IsTrue,
				qt.Commentf("round-trip should preserve operation equality"))
		})
	}
}

// TestOperationToInstruction_AllDirectDispatch covers every direct-dispatch
// Op type. Pins the OpKind() return value matches the opcode the type
// switch (or default branch) emits, so an OpKind() typo fails locally
// instead of via a confusing end-to-end Scheme regression.
func TestOperationToInstruction_AllDirectDispatch(t *testing.T) {
	c := qt.New(t)
	li := environment.NewLocalIndex(7, 4)

	tests := []struct {
		name       string
		op         Operation
		expectedOp OpCode
	}{
		// Zero-operand ops (fall through operationToInstruction's default
		// branch, which trusts op.OpKind()).
		{"Push", NewOperationPush(), OpPush},
		{"Pop", NewOperationPop(), OpPop},
		{"Pull", NewOperationPull(), OpPull},
		{"Drop", NewOperationDrop(), OpDrop},
		{"LoadVoid", NewOperationLoadVoid(), OpLoadVoid},
		{"PopEnv", NewOperationPopEnv(), OpPopEnv},
		{"Apply", NewOperationApply(), OpApply},
		{"UnpackListToStack", NewOperationUnpackListToStack(), OpUnpackListToStack},
		{"RestoreContinuation", NewOperationRestoreContinuation(), OpRestoreContinuation},
		{"MakeClosure", NewOperationMakeClosure(), OpMakeClosure},

		// Operand-bearing ops (matched by the type switch).
		{"PeekK", NewOperationPeekK(3), OpPeekK},
		{"PushEnv", NewOperationPushEnv(5), OpPushEnv},
		{"Branch", NewOperationBranchOffsetImmediate(10), OpBranch},
		{"BranchOnFalseValue", NewOperationBranchOnFalseValueOffsetImmediate(10), OpBranchOnFalseValue},
		{"SaveContinuation", NewOperationSaveContinuationOffsetImmediate(10), OpSaveContinuation},
		{"LoadLiteral", NewOperationLoadLiteralByLiteralIndexImmediate(0), OpLoadLiteral},
		{"LoadGlobal", NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0), OpLoadGlobal},
		{"StoreGlobal", NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0), OpStoreGlobal},
		{"LoadCachedBinding", NewOperationLoadCachedBinding(0), OpLoadCachedBinding},
		{"LoadLocalDirect", NewOperationLoadLocalByLocalIndexImmediate(li), OpLoadLocal},
		{"StoreLocalDirect", NewOperationStoreLocalByLocalIndexImmediate(li), OpStoreLocal},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			instr, ok := operationToInstruction(tc.op)
			c.Assert(ok, qt.IsTrue,
				qt.Commentf("direct-dispatch op %T should return ok=true", tc.op))
			c.Assert(instr.Op, qt.Equals, tc.expectedOp,
				qt.Commentf("%T.OpKind() reports %s; operationToInstruction emitted %s",
					tc.op, tc.op.OpKind(), instr.Op))
			c.Assert(tc.op.OpKind(), qt.Equals, tc.expectedOp,
				qt.Commentf("OpKind() must agree with the emitted opcode"))
		})
	}
}

// TestOperationToInstruction_SideTableReturnsFalse covers every machine/
// Op type that dispatches via OpComplex. operationToInstruction must
// return ok=false so AppendOperationsWithSource routes them through the
// side table.
func TestOperationToInstruction_SideTableReturnsFalse(t *testing.T) {
	c := qt.New(t)

	// Construct a no-op ForeignFunction for OperationForeignFunctionCall.
	noopFn := func(CallContext) error {
		return nil
	}

	tests := []struct {
		name string
		op   Operation
	}{
		{"ForeignFunctionCall", NewOperationForeignFunctionCall(noopFn)},
		{"MakeCaseLambdaClosure", NewOperationMakeCaseLambdaClosure(1)},
		{"PushWind", NewOperationPushWind()},
		{"PopWind", NewOperationPopWind()},
		{"SetContMark", NewOperationSetContMark()},
		{"SaveContMark", NewOperationSaveContMark()},
		{"RestoreContMark", NewOperationRestoreContMark()},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.op.OpKind(), qt.Equals, OpComplex,
				qt.Commentf("%T must report OpKind=OpComplex", tc.op))
			instr, ok := operationToInstruction(tc.op)
			c.Assert(ok, qt.IsFalse,
				qt.Commentf("OpComplex op %T should return ok=false", tc.op))
			c.Assert(instr, qt.Equals, Instruction{},
				qt.Commentf("OpComplex op should return zero-value Instruction"))
		})
	}
}

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

// This file centralizes the Operation -> OpCode mapping for the machine/
// package. Compilation-time operations defined in machine/compilation/
// declare their own OpKind in machine/compilation/op_kind.go (all return
// OpComplex). Operations that have a dedicated opcode in the Run() switch
// return that opcode; operations dispatched through the side table return
// OpComplex.
//
// Covers every Op* type in package machine. Operand-bearing ops also
// appear in operationToInstruction's type switch (native_template.go) for
// operand extraction; zero-operand ops are encoded purely from OpKind via
// that function's default branch.
//
// Per-type method centralization here is a deliberate deviation from the
// usual co-location convention (EqualTo, SchemeString, Apply all live with
// their type definitions). The choice optimizes for "one place to scan the
// Op -> OpCode map" over per-type discoverability.
//
// When adding a new operation:
//   1. Add OpKind here (or in machine/compilation/op_kind.go if defined
//      in that package).
//   2. Add the operand-extraction case in operationToInstruction (if it has
//      operands; zero-operand ops fall through the default branch).
//   3. For OpComplex types, also add var _ InlinedOperation = ... below.

// --- Direct-dispatch (own opcode in Run() switch) ---

func (*OperationPush) OpKind() OpCode {
	return OpPush
}

func (*OperationPushValues) OpKind() OpCode {
	return OpPushValues
}

func (*OperationPop) OpKind() OpCode {
	return OpPop
}

func (*OperationPull) OpKind() OpCode {
	return OpPull
}

func (*OperationDrop) OpKind() OpCode {
	return OpDrop
}

func (*OperationPeekK) OpKind() OpCode {
	return OpPeekK
}

func (*OperationLoadVoid) OpKind() OpCode {
	return OpLoadVoid
}

func (*OperationLoadLiteralByLiteralIndexImmediate) OpKind() OpCode {
	return OpLoadLiteral
}

func (*OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) OpKind() OpCode {
	return OpLoadGlobal
}

func (*OperationLoadLocalByLocalIndexImmediate) OpKind() OpCode {
	return OpLoadLocal
}

func (*OperationLoadCachedBinding) OpKind() OpCode {
	return OpLoadCachedBinding
}

func (*OperationStoreLocalByLocalIndexImmediate) OpKind() OpCode {
	return OpStoreLocal
}

func (*OperationBoxSlot) OpKind() OpCode {
	return OpBoxSlot
}

func (*OperationStoreThroughBox) OpKind() OpCode {
	return OpStoreThroughBox
}

func (*OperationUnbox) OpKind() OpCode {
	return OpUnbox
}

func (*OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) OpKind() OpCode {
	return OpStoreGlobal
}

func (*OperationBranchOffsetImmediate) OpKind() OpCode {
	return OpBranch
}

func (*OperationBranchOnFalseValueOffsetImmediate) OpKind() OpCode {
	return OpBranchOnFalseValue
}

func (*OperationSaveContinuationOffsetImmediate) OpKind() OpCode {
	return OpSaveContinuation
}

func (*OperationRestoreContinuation) OpKind() OpCode {
	return OpRestoreContinuation
}

func (*OperationApply) OpKind() OpCode {
	return OpApply
}

func (*OperationUnpackListToStack) OpKind() OpCode {
	return OpUnpackListToStack
}

func (*OperationPopEnv) OpKind() OpCode {
	return OpPopEnv
}

func (*OperationPushEnv) OpKind() OpCode {
	return OpPushEnv
}

func (*OperationSelfTailCall) OpKind() OpCode {
	return OpSelfTailCall
}

func (*OperationReleaseEnvFrame) OpKind() OpCode {
	return OpReleaseEnvFrame
}

// OpKind returns OpMakeClosure.
//
// OperationMakeClosure is the only Op type that both has a dedicated
// opcode (OpMakeClosure) and implements Apply. The Apply method is
// vestigial in production -- the compiler emits OpMakeClosure directly,
// routing through the Run() switch case rather than the side table.
//
// Apply is preserved so OperationMakeClosure satisfies InlinedOperation,
// allowing TestEditPlan_SideTableGC to use it as
// a no-arg placeholder when populating tpl.sideTable. That test asserts
// only GC remapping of indices and never invokes Apply itself, so the
// production and test code paths are decoupled. If Apply's body is ever
// changed, the matching OpMakeClosure inline case in machine_context.go's
// Run() must be updated to keep production and test behavior consistent
// (or Apply should be removed and the test migrated to use testInlinedOp
// from machine_context_test.go).
func (*OperationMakeClosure) OpKind() OpCode {
	return OpMakeClosure
}

// --- Side-table dispatch (OpComplex in Run() switch) ---
//
// Each type below must also implement InlinedOperation (the Apply method)
// because OpComplex dispatch routes through sideTable[i].Apply(mc). The
// var _ InlinedOperation declarations enforce this invariant at compile
// time -- any new OpComplex type that forgets Apply will fail to build
// rather than panic at runtime in AppendOperationsWithSource.

var (
	_ InlinedOperation = (*OperationMakeCaseLambdaClosure)(nil)
	_ InlinedOperation = (*OperationPushWind)(nil)
	_ InlinedOperation = (*OperationPopWind)(nil)
	_ InlinedOperation = (*OperationSetContMark)(nil)
	_ InlinedOperation = (*OperationSaveContMark)(nil)
	_ InlinedOperation = (*OperationRestoreContMark)(nil)
	_ InlinedOperation = (*OperationBoxValues)(nil)
	_ InlinedOperation = (*OperationUnboxValues)(nil)
	_ InlinedOperation = (*OperationGoReturn)(nil)
)

func (*OperationMakeCaseLambdaClosure) OpKind() OpCode {
	return OpComplex
}

func (*OperationPushWind) OpKind() OpCode {
	return OpComplex
}

func (*OperationPopWind) OpKind() OpCode {
	return OpComplex
}

func (*OperationSetContMark) OpKind() OpCode {
	return OpComplex
}

func (*OperationSaveContMark) OpKind() OpCode {
	return OpComplex
}

func (*OperationRestoreContMark) OpKind() OpCode {
	return OpComplex
}

func (*OperationBoxValues) OpKind() OpCode {
	return OpComplex
}

func (*OperationUnboxValues) OpKind() OpCode {
	return OpComplex
}

func (*OperationGoReturn) OpKind() OpCode {
	return OpComplex
}

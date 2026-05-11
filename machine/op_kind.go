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

// This file centralizes the Operation -> OpCode mapping. Every Operation
// implementation declares its dispatch identity here. Operations that have
// a dedicated opcode in the Run() switch return that opcode; operations
// dispatched through the side table return OpComplex.
//
// Mirrors the operand-extraction switch in operationToInstruction
// (native_template.go). When adding a new operation:
//   1. Add OpKind here.
//   2. Add the operand-extraction case in operationToInstruction (if it has
//      operands; zero-operand ops fall through the default branch).

// --- Direct-dispatch (own opcode in Run() switch) ---

func (*OperationPush) OpKind() OpCode {
	return OpPush
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

// OperationMakeClosure is the only Op type that both has a dedicated
// opcode (OpMakeClosure) and implements Apply. The Apply method is
// vestigial in production -- the compiler emits OpMakeClosure directly
// rather than routing through the side table -- but is preserved to
// retain the InlinedOperation contract for the test side-table path.
func (*OperationMakeClosure) OpKind() OpCode {
	return OpMakeClosure
}

// --- Side-table dispatch (OpComplex in Run() switch) ---

func (*OperationForeignFunctionCall) OpKind() OpCode {
	return OpComplex
}

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

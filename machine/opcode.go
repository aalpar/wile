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

// OpCode is an integer opcode for the switch-dispatch VM loop.
// Operations migrated from interface dispatch to integer dispatch
// get a dedicated OpCode. Complex operations that remain as interface
// values use OpComplex with a side table index.
//
// Adding a new opcode requires changes in:
//  1. opcode.go — add OpXxx constant and entry in opcodeTable (name + metadata flags;
//     operandKind must match step 3's extraction logic)
//  2. machine_context.go Run() — add dispatch case in the main switch
//  3. native_template.go — add a case in instructionToOperation() (always required) and in
//     operationToInstruction() (required for operand-bearing ops; zero-operand ops fall
//     through the default branch, which cross-checks opcodeTable[kind].operandKind == OperandNone)
//  4. operation_xxx.go — create new operation type (or add to existing file)
//  5. op_kind.go — add OpKind() returning the new OpCode (or compilation/op_kind.go for compilation/ types);
//     for OpComplex types, also add a var _ InlinedOperation assertion
//  6. compile_*.go — add compiler method to emit the new opcode
//  7. Relevant _test.go files
//  8. peephole.go — if the new op participates in fusion/chaining (e.g. loadToFusedPush)
//
// For promoted primitive ops specifically, see the guide comment at the top
// of call_promoted.go — promoted ops have a different (smaller) set of edit sites.
type OpCode uint16

const (
	OpInvalid OpCode = iota

	// Wave 1: zero-operand operations
	OpPush
	OpPop
	OpPull
	OpLoadVoid
	OpDrop
	OpPopEnv
	OpApply
	OpUnpackListToStack
	OpRestoreContinuation

	// Wave 2: single-operand operations (Arg = offset, index, or depth)
	OpBranchOnFalseValue
	OpBranch
	OpSaveContinuation
	OpLoadLiteral
	OpLoadGlobal
	OpStoreGlobal
	OpPeekK
	OpPushEnv // Push new env frame with Arg local slots

	// Wave 3: two-operand operations (Arg = bit-packed slot|depth)
	OpLoadLocal
	OpStoreLocal

	// Wave 4: fused push operations (Arg = same as unfused Load variant)
	OpPushLiteral // LoadLiteral + Push
	OpPushGlobal  // LoadGlobal + Push
	OpPushLocal   // LoadLocal + Push

	// Wave 5: fused call operations (zero-operand)
	OpPullApply // Pull + Apply

	// Wave 5: promoted complex operations (zero-operand)
	OpMakeClosure // MakeClosure (was OpComplex)

	// Wave 6: cached binding operations (Arg = index into cachedBindings)
	OpLoadCachedBinding // Load from compile-time resolved *Binding
	OpPushCachedBinding // LoadCachedBinding + Push (fused)

	// Wave 7: direct foreign call operations (Arg = index into cachedBindings)
	// Emitted by peephole only — compiler never produces these.
	OpCallForeignCached     // Non-tail: call ForeignClosure, then mc.pc++
	OpCallForeignCachedTail // Tail: call ForeignClosure, then returnImmediate()

	// Wave 8: general call fusion (Arg = same encoding as PushLocal/PushCachedBinding)
	// Fused PushLocal/PushCachedBinding + PullApply for non-foreign callables.
	// Emitted by peephole only — compiler never produces these.
	OpCallLocal         // Resolve local binding, drain args, ApplyCallable
	OpCallCachedBinding // Resolve cached binding, drain args, ApplyCallable

	// Wave 9: promoted primitive operations
	// (Arg = encodePromotedArg(bindingIdx, tail); high bit = tail flag,
	// low 31 bits = index into cachedBindings).
	//
	// Inline the hot primitive logic directly, bypassing arity check,
	// arg binding, and indirect function call. Emitted by peephole only
	// when the cached binding holds a known promoted ForeignClosure.
	//
	// Each opcode handles BOTH tail and non-tail positions; execPromoted
	// decodes the tail flag from Arg. See
	// plans/2026-05-11-machine-sr-finding2-impl.md (Finding 2).
	OpEqQ       // inlined eq?
	OpVectorQ   // inlined vector?
	OpVectorRef // inlined vector-ref
	OpNullQ     // inlined null?
	OpPairQ     // inlined pair?
	OpCar       // inlined car
	OpCdr       // inlined cdr
	OpAdd       // inlined 2-arg +
	OpSub       // inlined 2-arg -
	OpNumLt     // inlined 2-arg <
	OpNumLe     // inlined 2-arg <=
	OpNumGt     // inlined 2-arg >
	OpNumGe     // inlined 2-arg >=
	OpNumEq     // inlined 2-arg =
	OpCons      // inlined cons
	OpMul       // inlined 2-arg *
	OpDiv       // inlined 2-arg /

	// Fallback: dispatch to sideTable[Arg]
	OpComplex

	// opCount is not a valid opcode; it marks the end of the enum
	// for use in table sizing and validation.
	opCount
)

// OperandKind classifies what an opcode's Arg field means. Used by
// cold-path consumers (Disassemble, instructionToOperation) to avoid
// re-deriving operand semantics in per-opcode switch branches.
type OperandKind uint8

const (
	// OperandNone means Arg is unused (zero-operand ops).
	OperandNone OperandKind = iota
	// OperandRaw means Arg is a meaningful integer but needs no resolution
	// (e.g., PushEnv slot count, PeekK depth).
	OperandRaw
	// OperandLiteralIdx means Arg indexes into the literals pool.
	OperandLiteralIdx
	// OperandLocalIdx means Arg is a bit-packed (slot, depth) pair.
	OperandLocalIdx
	// OperandBranchOffset means Arg is a relative PC offset.
	OperandBranchOffset
	// OperandCachedBinding means Arg indexes into cachedBindings.
	OperandCachedBinding
	// OperandPromotedCachedBinding means Arg encodes a promoted-op operand:
	// the high bit signals tail position, the low 31 bits index into
	// cachedBindings. Distinct from OperandCachedBinding because the
	// disassembler and any future tooling must decode the tail flag.
	// See encodePromotedArg / decodePromotedArg below.
	OperandPromotedCachedBinding
	// OperandSideTable means Arg indexes into the side table.
	OperandSideTable
)

// promotedTailBit is the high bit of instr.Arg, used by promoted opcodes
// to encode tail-call position. Non-promoted opcodes with operand kind
// OperandCachedBinding (e.g. OpCallForeignCached) do not use this bit;
// they carry tail-ness in the opcode itself.
//
// Binding indexes are non-negative integers assigned by the peephole
// optimizer, so the sign bit is free. The post-collapse maximum binding
// index is 2^31 - 1, well above any realistic template.
//
// See plans/2026-05-11-machine-sr-finding2-impl.md.
const promotedTailBit int32 = -1 << 31 // = math.MinInt32

// encodePromotedArg packs a binding index and tail flag into a single
// int32 suitable for storage in Instruction.Arg.
func encodePromotedArg(bindingIdx int32, tail bool) int32 {
	if tail {
		return bindingIdx | promotedTailBit
	}
	return bindingIdx
}

// decodePromotedArg unpacks a promoted-op Arg into (binding index, tail).
func decodePromotedArg(arg int32) (bindingIdx int32, tail bool) {
	return arg &^ promotedTailBit, arg < 0
}

// opcodeInfo holds metadata for a single opcode. All opcode properties are
// centralized here so that adding a new opcode requires updating exactly one
// table entry rather than maintaining parallel arrays and predicates.
type opcodeInfo struct {
	name        string
	operandKind OperandKind // what Arg means (used by Disassemble, instructionToOperation)
	writesValue bool        // unconditionally writes value register without reading it first
	isBranch    bool        // Arg is a relative PC offset that needs fixup
}

// opcodeTable is the single source of truth for opcode metadata.
// Indexed by OpCode value; must stay in sync with the const block above.
//
// writesValue: opcode unconditionally overwrites the value register, making
// a preceding LoadVoid dead. Only applies to original (non-fused) opcodes.
//
// isBranch: opcode's Arg is a relative PC offset that must be adjusted
// when instructions are removed or inserted by the edit plan.
var opcodeTable = [opCount]opcodeInfo{
	OpInvalid:               {name: "Invalid"},
	OpPush:                  {name: "Push"},
	OpPop:                   {name: "Pop", writesValue: true},
	OpPull:                  {name: "Pull", writesValue: true},
	OpLoadVoid:              {name: "LoadVoid", writesValue: true},
	OpDrop:                  {name: "Drop"},
	OpPopEnv:                {name: "PopEnv"},
	OpApply:                 {name: "Apply"},
	OpUnpackListToStack:     {name: "UnpackListToStack"},
	OpRestoreContinuation:   {name: "RestoreContinuation"},
	OpBranchOnFalseValue:    {name: "BranchOnFalseValue", operandKind: OperandBranchOffset, isBranch: true},
	OpBranch:                {name: "Branch", operandKind: OperandBranchOffset, isBranch: true},
	OpSaveContinuation:      {name: "SaveContinuation", operandKind: OperandBranchOffset, isBranch: true},
	OpLoadLiteral:           {name: "LoadLiteral", operandKind: OperandLiteralIdx, writesValue: true},
	OpLoadGlobal:            {name: "LoadGlobal", operandKind: OperandLiteralIdx, writesValue: true},
	OpStoreGlobal:           {name: "StoreGlobal", operandKind: OperandLiteralIdx},
	OpPeekK:                 {name: "PeekK", operandKind: OperandRaw, writesValue: true},
	OpPushEnv:               {name: "PushEnv", operandKind: OperandRaw},
	OpLoadLocal:             {name: "LoadLocal", operandKind: OperandLocalIdx, writesValue: true},
	OpStoreLocal:            {name: "StoreLocal", operandKind: OperandLocalIdx},
	OpPushLiteral:           {name: "PushLiteral", operandKind: OperandLiteralIdx},
	OpPushGlobal:            {name: "PushGlobal", operandKind: OperandLiteralIdx},
	OpPushLocal:             {name: "PushLocal", operandKind: OperandLocalIdx},
	OpPullApply:             {name: "PullApply"},
	OpMakeClosure:           {name: "MakeClosure", writesValue: true},
	OpLoadCachedBinding:     {name: "LoadCachedBinding", operandKind: OperandCachedBinding, writesValue: true},
	OpPushCachedBinding:     {name: "PushCachedBinding", operandKind: OperandCachedBinding},
	OpCallForeignCached:     {name: "CallForeignCached", operandKind: OperandCachedBinding},
	OpCallForeignCachedTail: {name: "CallForeignCachedTail", operandKind: OperandCachedBinding},
	OpCallLocal:             {name: "CallLocal", operandKind: OperandLocalIdx},
	OpCallCachedBinding:     {name: "CallCachedBinding", operandKind: OperandCachedBinding},
	OpEqQ:                   {name: "EqQ", operandKind: OperandPromotedCachedBinding},
	OpVectorQ:               {name: "VectorQ", operandKind: OperandPromotedCachedBinding},
	OpVectorRef:             {name: "VectorRef", operandKind: OperandPromotedCachedBinding},
	OpNullQ:                 {name: "NullQ", operandKind: OperandPromotedCachedBinding},
	OpPairQ:                 {name: "PairQ", operandKind: OperandPromotedCachedBinding},
	OpCar:                   {name: "Car", operandKind: OperandPromotedCachedBinding},
	OpCdr:                   {name: "Cdr", operandKind: OperandPromotedCachedBinding},
	OpAdd:                   {name: "Add", operandKind: OperandPromotedCachedBinding},
	OpSub:                   {name: "Sub", operandKind: OperandPromotedCachedBinding},
	OpNumLt:                 {name: "NumLt", operandKind: OperandPromotedCachedBinding},
	OpNumLe:                 {name: "NumLe", operandKind: OperandPromotedCachedBinding},
	OpNumGt:                 {name: "NumGt", operandKind: OperandPromotedCachedBinding},
	OpNumGe:                 {name: "NumGe", operandKind: OperandPromotedCachedBinding},
	OpNumEq:                 {name: "NumEq", operandKind: OperandPromotedCachedBinding},
	OpCons:                  {name: "Cons", operandKind: OperandPromotedCachedBinding},
	OpMul:                   {name: "Mul", operandKind: OperandPromotedCachedBinding},
	OpDiv:                   {name: "Div", operandKind: OperandPromotedCachedBinding},
	OpComplex:               {name: "Complex", operandKind: OperandSideTable},
}

// String returns the human-readable name of the opcode.
func (op OpCode) String() string {
	if op < opCount {
		return opcodeTable[op].name
	}
	return "Unknown"
}

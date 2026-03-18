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
//  1. opcode.go — add OpXxx constant and entry in opcodeTable (name + metadata flags)
//  2. machine_context.go Run() — add dispatch case in the main switch
//  3. native_template.go — add cases in both operationToInstruction() and instructionToOperation()
//  4. operation_xxx.go — create new operation type (or add to existing file)
//  5. compile_*.go — add compiler method to emit the new opcode
//  6. Relevant _test.go files
//  7. peephole.go — if the new op participates in fusion/chaining (e.g. loadToFusedPush)
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

	// Wave 9: promoted primitive operations (Arg = index into cachedBindings)
	// Inline the hot primitive logic directly, bypassing arity check, arg
	// binding, and indirect function call. Emitted by peephole only when
	// the cached binding holds a known promoted ForeignClosure.
	OpEqQ           // Non-tail inlined eq?
	OpEqQTail       // Tail inlined eq?
	OpVectorQ       // Non-tail inlined vector?
	OpVectorQTail   // Tail inlined vector?
	OpVectorRef     // Non-tail inlined vector-ref
	OpVectorRefTail // Tail inlined vector-ref
	OpNullQ         // Non-tail inlined null?
	OpNullQTail     // Tail inlined null?
	OpPairQ         // Non-tail inlined pair?
	OpPairQTail     // Tail inlined pair?
	OpCar           // Non-tail inlined car
	OpCarTail       // Tail inlined car
	OpCdr           // Non-tail inlined cdr
	OpCdrTail       // Tail inlined cdr
	OpAdd           // Non-tail inlined 2-arg +
	OpAddTail       // Tail inlined 2-arg +
	OpSub           // Non-tail inlined 2-arg -
	OpSubTail       // Tail inlined 2-arg -
	OpNumLt         // Non-tail inlined 2-arg <
	OpNumLtTail     // Tail inlined 2-arg <
	OpNumLe         // Non-tail inlined 2-arg <=
	OpNumLeTail     // Tail inlined 2-arg <=
	OpNumGt         // Non-tail inlined 2-arg >
	OpNumGtTail     // Tail inlined 2-arg >
	OpNumGe         // Non-tail inlined 2-arg >=
	OpNumGeTail     // Tail inlined 2-arg >=
	OpNumEq         // Non-tail inlined 2-arg =
	OpNumEqTail     // Tail inlined 2-arg =

	// Fallback: dispatch to sideTable[Arg]
	OpComplex

	// opCount is not a valid opcode; it marks the end of the enum
	// for use in table sizing and validation.
	opCount
)

// opcodeInfo holds metadata for a single opcode. All opcode properties are
// centralized here so that adding a new opcode requires updating exactly one
// table entry rather than maintaining parallel arrays and predicates.
type opcodeInfo struct {
	name        string
	writesValue bool // unconditionally writes value register without reading it first
	isBranch    bool // Arg is a relative PC offset that needs fixup
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
	OpBranchOnFalseValue:    {name: "BranchOnFalseValue", isBranch: true},
	OpBranch:                {name: "Branch", isBranch: true},
	OpSaveContinuation:      {name: "SaveContinuation", isBranch: true},
	OpLoadLiteral:           {name: "LoadLiteral", writesValue: true},
	OpLoadGlobal:            {name: "LoadGlobal", writesValue: true},
	OpStoreGlobal:           {name: "StoreGlobal"},
	OpPeekK:                 {name: "PeekK", writesValue: true},
	OpLoadLocal:             {name: "LoadLocal", writesValue: true},
	OpStoreLocal:            {name: "StoreLocal"},
	OpPushLiteral:           {name: "PushLiteral"},
	OpPushGlobal:            {name: "PushGlobal"},
	OpPushLocal:             {name: "PushLocal"},
	OpPullApply:             {name: "PullApply"},
	OpMakeClosure:           {name: "MakeClosure", writesValue: true},
	OpLoadCachedBinding:     {name: "LoadCachedBinding", writesValue: true},
	OpPushCachedBinding:     {name: "PushCachedBinding"},
	OpCallForeignCached:     {name: "CallForeignCached"},
	OpCallForeignCachedTail: {name: "CallForeignCachedTail"},
	OpCallLocal:             {name: "CallLocal"},
	OpCallCachedBinding:     {name: "CallCachedBinding"},
	OpEqQ:                   {name: "EqQ"},
	OpEqQTail:               {name: "EqQTail"},
	OpVectorQ:               {name: "VectorQ"},
	OpVectorQTail:           {name: "VectorQTail"},
	OpVectorRef:             {name: "VectorRef"},
	OpVectorRefTail:         {name: "VectorRefTail"},
	OpNullQ:                 {name: "NullQ"},
	OpNullQTail:             {name: "NullQTail"},
	OpPairQ:                 {name: "PairQ"},
	OpPairQTail:             {name: "PairQTail"},
	OpCar:                   {name: "Car"},
	OpCarTail:               {name: "CarTail"},
	OpCdr:                   {name: "Cdr"},
	OpCdrTail:               {name: "CdrTail"},
	OpAdd:                   {name: "Add"},
	OpAddTail:               {name: "AddTail"},
	OpSub:                   {name: "Sub"},
	OpSubTail:               {name: "SubTail"},
	OpNumLt:                 {name: "NumLt"},
	OpNumLtTail:             {name: "NumLtTail"},
	OpNumLe:                 {name: "NumLe"},
	OpNumLeTail:             {name: "NumLeTail"},
	OpNumGt:                 {name: "NumGt"},
	OpNumGtTail:             {name: "NumGtTail"},
	OpNumGe:                 {name: "NumGe"},
	OpNumGeTail:             {name: "NumGeTail"},
	OpNumEq:                 {name: "NumEq"},
	OpNumEqTail:             {name: "NumEqTail"},
	OpComplex:               {name: "Complex"},
}

// String returns the human-readable name of the opcode.
func (op OpCode) String() string {
	if op < opCount {
		return opcodeTable[op].name
	}
	return "Unknown"
}

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
	"fmt"
	"math"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/werr"
)

// ErrLocalIndexOverflow signals that a De Bruijn index slot or depth
// exceeds the int16 encoding range (-32768..32767). EncodeLocalIndex panics
// with this sentinel when the bytecode format cannot represent the index.
var ErrLocalIndexOverflow = werr.NewStaticError("local index overflow")

// ErrFreeIndexOutOfRange reports a free-vector index that the executing
// closure's vector does not contain. It is a compiler/VM disagreement, not a
// user error: the index is an immediate the emitter chose against a layout the
// same emitter fixed.
var ErrFreeIndexOutOfRange = werr.NewStaticError("free variable index out of range")

// ErrNoFreeVector reports a free-vector access executed with no closure vector
// installed — a body compiled as a closure body running outside Apply.
var ErrNoFreeVector = werr.NewStaticError("no free vector installed")

// Instruction is a single VM instruction for the switch-dispatch loop.
// Op selects the operation; Arg carries an immediate operand whose meaning
// depends on Op:
//   - Zero-operand ops (Push, Pop, ...): Arg is unused (0).
//   - Single-operand ops (Branch, LoadLiteral, ...): Arg is the offset, index, or depth.
//   - Two-operand ops (LoadLocal, StoreLocal): Arg is bit-packed (slot in low 16, depth in high 16).
//   - OpComplex: Arg is the index into the template's sideTable.
//
// Size: 8 bytes (uint16 Op + 2 bytes padding + int32 Arg).
type Instruction struct {
	Op  OpCode
	Arg int32
}

// String returns a human-readable representation of the instruction.
func (instr Instruction) String() string {
	switch instr.Op {
	case OpLoadLocal, OpStoreLocal, OpPushLocal:
		slot, depth := DecodeLocalIndex(instr.Arg)
		return fmt.Sprintf("%s slot=%d depth=%d", instr.Op, slot, depth)
	}
	if instr.Arg != 0 {
		return fmt.Sprintf("%s %d", instr.Op, instr.Arg)
	}
	return instr.Op.String()
}

// EncodeLocalIndex packs a LocalIndex's slot and depth into a single int32
// for storage in Instruction.Arg. Slot occupies the low 16 bits; depth
// occupies the high 16 bits. Both values must fit in int16 range (max 32767).
//
// De Bruijn indices (de Bruijn 1972). Variables are addressed by numeric
// coordinates, eliminating name lookup at runtime.
//
//	addr(x) = (slot, depth), where:
//	  depth = number of enclosing λ-binders from use to definition
//	  slot  = index within the binding array at that depth
//
//	Encoding: Arg = (depth << 16) | (slot & 0xFFFF)
//
//	Invariant: the same variable always has the same (slot, depth)
//	  regardless of its name. Alpha-equivalence is a non-issue at runtime.
//	Constrains: GetLocalBindingBySlotDepth / SetLocalValueBySlotDepth
//	  (runtime access walks depth parent pointers, indexes by slot),
//	  linked closures (parent chain must match compile-time depth).
//	Constrained by: resolveLocal (compile-time computation of depth
//	  by walking the EnvironmentFrame parent chain).
//
// See BIBLIOGRAPHY.md "De Bruijn Indices / Lexical Addressing".
func EncodeLocalIndex(li *environment.LocalIndex) int32 {
	slot := li.Over()
	depth := li.Up()
	if slot > math.MaxInt16 || slot < math.MinInt16 {
		panic(werr.WrapForeignErrorf(ErrLocalIndexOverflow,
			"EncodeLocalIndex: slot %d exceeds int16 range (%d..%d)", slot, math.MinInt16, math.MaxInt16))
	}
	if depth > math.MaxInt16 || depth < math.MinInt16 {
		panic(werr.WrapForeignErrorf(ErrLocalIndexOverflow,
			"EncodeLocalIndex: depth %d exceeds int16 range (%d..%d)", depth, math.MinInt16, math.MaxInt16))
	}
	return int32(depth<<16) | int32(slot&0xFFFF)
}

// DecodeLocalIndex unpacks slot and depth from a bit-packed Instruction.Arg.
func DecodeLocalIndex(arg int32) (slot, depth int) {
	slot = int(int16(arg))
	depth = int(int16(arg >> 16))
	return
}

// EncodeSelfTailCall packs OpSelfTailCall's two operands: the argument count in
// the low half, the count of intermediate env frames to pop in the high half.
//
// The low half carries argCount so that popCount == 0 encodes to argCount itself.
// Every depth-0 self-tail site therefore emits the same instruction word it did
// before Phase C widened the op, which keeps the change invisible to the peephole
// pass, to the disassembler's existing goldens, and to any stored template.
//
// Both operands are non-negative and small — argCount is bounded by the frame's
// slot capacity and popCount by lexical let nesting — so the int16 halves are not
// a practical limit; the panic is a corrupt-compiler assertion, matching
// EncodeLocalIndex.
func EncodeSelfTailCall(argCount, popCount int) int32 {
	if argCount < 0 || argCount > math.MaxInt16 {
		panic(werr.WrapForeignErrorf(ErrLocalIndexOverflow,
			"EncodeSelfTailCall: argCount %d outside 0..%d", argCount, math.MaxInt16))
	}
	if popCount < 0 || popCount > math.MaxInt16 {
		panic(werr.WrapForeignErrorf(ErrLocalIndexOverflow,
			"EncodeSelfTailCall: popCount %d outside 0..%d", popCount, math.MaxInt16))
	}
	return int32(popCount)<<16 | int32(argCount)
}

// DecodeSelfTailCall unpacks OpSelfTailCall's argument count and frame-pop count.
func DecodeSelfTailCall(arg int32) (argCount, popCount int) {
	argCount = int(arg & 0xFFFF)
	popCount = int(arg >> 16)
	return
}

// EncodeMakeClosure packs OpMakeClosure's two operands: the free-variable count
// in the low half, the self-reference slot in the high half.
//
// selfSlot is stored as selfSlot+1 so that 0 means "no self slot". The natural
// -1 cannot ride in a half of a codec that refuses a negative operand, which is
// the same constraint EncodeSelfTailCall works under.
//
// freeCount 0 with no self slot encodes to 0, so every closure that captures
// nothing emits the same instruction word OpMakeClosure emitted before this
// change — invisible to the peephole, to the disassembler's goldens, and to any
// stored template.
func EncodeMakeClosure(freeCount, selfSlot int) int32 {
	if freeCount < 0 || freeCount > math.MaxInt16 {
		panic(werr.WrapForeignErrorf(ErrLocalIndexOverflow,
			"EncodeMakeClosure: freeCount %d outside 0..%d", freeCount, math.MaxInt16))
	}
	if selfSlot < -1 || selfSlot >= math.MaxInt16 {
		panic(werr.WrapForeignErrorf(ErrLocalIndexOverflow,
			"EncodeMakeClosure: selfSlot %d outside -1..%d", selfSlot, math.MaxInt16-1))
	}
	return int32(selfSlot+1)<<16 | int32(freeCount)
}

// DecodeMakeClosure unpacks OpMakeClosure's free-variable count and
// self-reference slot. selfSlot is -1 when the closure has none.
func DecodeMakeClosure(arg int32) (freeCount, selfSlot int) {
	freeCount = int(arg & 0xFFFF)
	selfSlot = int(arg>>16) - 1
	return
}

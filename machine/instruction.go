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

	"github.com/aalpar/wile/environment"
)

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
	if instr.Op == OpLoadLocal || instr.Op == OpStoreLocal || instr.Op == OpPushLocal {
		slot, depth := DecodeLocalIndex(instr.Arg)
		return fmt.Sprintf("%s slot=%d depth=%d", instr.Op, slot, depth)
	}
	if instr.Op == OpCallForeignCached || instr.Op == OpCallForeignCachedTail ||
		instr.Op == OpCallForeignCachedVar || instr.Op == OpCallForeignCachedVarTail {
		bindingIdx, paramCount := DecodeForeignCallArg(instr.Arg)
		return fmt.Sprintf("%s binding=%d params=%d", instr.Op, bindingIdx, paramCount)
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
	return int32(li.Up()<<16) | int32(li.Over()&0xFFFF)
}

// DecodeLocalIndex unpacks slot and depth from a bit-packed Instruction.Arg.
func DecodeLocalIndex(arg int32) (slot, depth int) {
	slot = int(int16(arg))
	depth = int(int16(arg >> 16))
	return
}

// EncodeForeignCallArg packs a cachedBindings index and paramCount into
// a single int32 for OpCallForeignCached instructions.
//
//	bits  0-15: cachedBindings index (0..65535)
//	bits 16-23: paramCount (0..255)
//	bits 24-31: reserved
func EncodeForeignCallArg(bindingIdx int32, paramCount int) int32 {
	return (bindingIdx & 0xFFFF) | int32(paramCount&0xFF)<<16
}

// DecodeForeignCallArg unpacks the cachedBindings index and paramCount
// from a bit-packed Instruction.Arg.
func DecodeForeignCallArg(arg int32) (bindingIdx int32, paramCount int) {
	return arg & 0xFFFF, int(arg>>16) & 0xFF
}

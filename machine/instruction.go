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
	if instr.Op == OpLoadLocal || instr.Op == OpStoreLocal {
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
func EncodeLocalIndex(li *environment.LocalIndex) int32 {
	return int32(li.Up()<<16) | int32(li.Over()&0xFFFF)
}

// DecodeLocalIndex unpacks slot and depth from a bit-packed Instruction.Arg.
func DecodeLocalIndex(arg int32) (slot, depth int) {
	slot = int(int16(arg))
	depth = int(int16(arg >> 16))
	return
}

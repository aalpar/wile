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

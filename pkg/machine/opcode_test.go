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

	qt "github.com/frankban/quicktest"
)

func TestOpcodeConstants(t *testing.T) {
	c := qt.New(t)

	// OpInvalid is zero — default value of OpCode is invalid.
	c.Assert(OpInvalid, qt.Equals, OpCode(0))

	// All named opcodes are distinct and sequential.
	seen := make(map[OpCode]string)
	opcodes := []struct {
		op   OpCode
		name string
	}{
		{OpInvalid, "Invalid"},
		{OpPush, "Push"},
		{OpPop, "Pop"},
		{OpPull, "Pull"},
		{OpLoadVoid, "LoadVoid"},
		{OpDrop, "Drop"},
		{OpPopEnv, "PopEnv"},
		{OpApply, "Apply"},
		{OpRestoreContinuation, "RestoreContinuation"},
		{OpBranchOnFalseValue, "BranchOnFalseValue"},
		{OpBranch, "Branch"},
		{OpSaveContinuation, "SaveContinuation"},
		{OpLoadLiteral, "LoadLiteral"},
		{OpLoadGlobal, "LoadGlobal"},
		{OpStoreGlobal, "StoreGlobal"},
		{OpPeekK, "PeekK"},
		{OpLoadLocal, "LoadLocal"},
		{OpStoreLocal, "StoreLocal"},
		{OpCallForeignCached, "CallForeignCached"},
		{OpCallForeignCachedTail, "CallForeignCachedTail"},
		{OpComplex, "Complex"},
	}

	for _, tc := range opcodes {
		prev, dup := seen[tc.op]
		c.Assert(dup, qt.IsFalse, qt.Commentf("OpCode %d used by both %s and %s", tc.op, prev, tc.name))
		seen[tc.op] = tc.name

		c.Assert(tc.op.String(), qt.Equals, tc.name)
	}
}

func TestOpcodeStringUnknown(t *testing.T) {
	c := qt.New(t)
	c.Assert(OpCode(9999).String(), qt.Equals, "Unknown")
}

// TestOpcodeTableComplete ensures every valid opcode has an entry in
// opcodeTable. Adding a new OpCode constant without a table entry
// causes this test to fail.
func TestOpcodeTableComplete(t *testing.T) {
	c := qt.New(t)
	for op := OpCode(1); op < opCount; op++ {
		c.Assert(opcodeTable[op].name, qt.Not(qt.Equals), "",
			qt.Commentf("opcode %d has no name in opcodeTable", op))
	}
}

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

	// Fallback: dispatch to sideTable[Arg]
	OpComplex

	// opCount is not a valid opcode; it marks the end of the enum
	// for use in table sizing and validation.
	opCount
)

// opcodeNames maps each OpCode to its string representation.
// Indexed by OpCode value; must stay in sync with the const block above.
var opcodeNames = [opCount]string{
	OpInvalid:             "Invalid",
	OpPush:                "Push",
	OpPop:                 "Pop",
	OpPull:                "Pull",
	OpLoadVoid:            "LoadVoid",
	OpDrop:                "Drop",
	OpPopEnv:              "PopEnv",
	OpApply:               "Apply",
	OpRestoreContinuation: "RestoreContinuation",
	OpBranchOnFalseValue:  "BranchOnFalseValue",
	OpBranch:              "Branch",
	OpSaveContinuation:    "SaveContinuation",
	OpLoadLiteral:         "LoadLiteral",
	OpLoadGlobal:          "LoadGlobal",
	OpStoreGlobal:         "StoreGlobal",
	OpPeekK:               "PeekK",
	OpLoadLocal:           "LoadLocal",
	OpStoreLocal:          "StoreLocal",
	OpComplex:             "Complex",
}

// String returns the human-readable name of the opcode.
func (op OpCode) String() string {
	if op < opCount {
		return opcodeNames[op]
	}
	return "Unknown"
}

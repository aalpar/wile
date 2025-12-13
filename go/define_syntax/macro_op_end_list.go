// Copyright 2025 Aaron Alpar
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

package define_syntax

import "wile/values"

// MacroOpEndList returns from a nested list during pattern matching.
//
// This operation is the counterpart to MacroOpStartList. After matching
// all elements within a nested list, MacroOpEndList pops the saved position
// from the stack and advances to the next element at the enclosing level.
//
// # Behavior
//
//  1. Get the saved position from the top of the stack
//  2. Get the cdr of the saved position (the element after the nested list)
//  3. Set mm.curr to this cdr (must be a pair)
//  4. Pop the stack
//  5. Advance the program counter
//
// # Example
//
// For pattern ((a b) c) matching ((1 2) 3):
//
//	After matching (a b) against (1 2):
//	  stack = [((1 2) 3)]
//	  curr = () (empty after matching b)
//	MacroOpEndList:
//	  saved = ((1 2) 3)
//	  cdr = (3)
//	  curr = (3)
//	  stack = []
//	Now ready to match c against 3
//
// # Errors
//
// Returns a ForeignError if the cdr of the saved position is not a pair,
// which would indicate a malformed input structure.
type MacroOpEndList struct{}

// NewMacroOpEndList creates a new MacroOpEndList operation.
func NewMacroOpEndList() *MacroOpEndList {
	return &MacroOpEndList{}
}

// Apply returns from a nested list to the enclosing level.
// Pops the saved position and advances to the next element.
// Returns an error if the structure doesn't match expectations.
// Increments the program counter on success.
func (p *MacroOpEndList) Apply(mm *MacroMachine) error {
	cdr := mm.stack[len(mm.stack)-1].Cdr()
	switch v := cdr.(type) {
	case *values.Pair:
		mm.curr = v
	default:
		return values.NewForeignErrorf("macro pattern match failed: expected more elements")
	}
	mm.stack = mm.stack[:len(mm.stack)-1]
	mm.pc++
	return nil
}

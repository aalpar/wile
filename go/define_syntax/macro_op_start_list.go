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

// MacroOpStartList descends into a nested list during pattern matching.
//
// This operation handles nested structure in patterns. When a pattern contains
// a sub-list, MacroOpStartList saves the current position on a stack and
// moves into the nested list to continue matching.
//
// # Behavior
//
//  1. Get the car of the current position (must be a pair)
//  2. Push the current position onto the stack (for later restoration)
//  3. Set mm.curr to the nested pair
//  4. Advance the program counter
//
// The saved position allows MacroOpEndList to resume matching at the
// correct position after the nested pattern is matched.
//
// # Example
//
// For pattern ((a b) c) matching ((1 2) 3):
//
//	Initial: curr = ((1 2) 3)
//	MacroOpStartList: push ((1 2) 3), curr = (1 2)
//	  ... match a, b inside (1 2) ...
//	MacroOpEndList: pop, curr = (3)
//	  ... match c ...
//
// # Errors
//
// Returns values.ErrNotAList if:
//   - mm.curr is nil
//   - mm.curr.Car() is not a pair (trying to descend into a non-list)
type MacroOpStartList struct{}

// NewMacroOpStartList creates a new MacroOpStartList operation.
func NewMacroOpStartList() *MacroOpStartList {
	return &MacroOpStartList{}
}

// Apply descends into the nested list at the current position.
// Pushes the current position onto the stack for later restoration.
// Returns values.ErrNotAList if the current car is not a list.
// Increments the program counter on success.
func (p *MacroOpStartList) Apply(mm *MacroMachine) error {
	curr := mm.curr
	if curr == nil {
		return values.ErrNotAList
	}
	pr, ok := curr.Car().(*values.Pair)
	if !ok {
		return values.ErrNotAList
	}
	mm.stack = append(mm.stack, mm.curr)
	mm.curr = pr
	mm.pc++
	return nil
}

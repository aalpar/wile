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

// MacroOpNext advances the current position to the next element in the list.
//
// This operation moves from the current pair to its cdr (tail). It is used
// to traverse a list sequentially during pattern matching.
//
// # Behavior
//
// Before: mm.curr points to a pair (car . cdr)
// After:  mm.curr points to cdr (which must be a pair)
//
// # Errors
//
// Returns values.ErrNotAPair if the cdr is not a pair. This typically happens
// when reaching an improper list terminator or when the pattern expects more
// elements than the input provides.
//
// # Example
//
// Matching pattern (a b c) against input (1 2 3):
//
//	Initial: curr = (1 2 3)
//	After MacroOpNext: curr = (2 3)
//	After MacroOpNext: curr = (3)
type MacroOpNext struct{}

// NewMacroOpNext creates a new MacroOpNext operation.
func NewMacroOpNext() *MacroOpNext {
	return &MacroOpNext{}
}

// Apply advances to the next element in the current list.
// Increments the program counter on success.
func (p *MacroOpNext) Apply(mm *MacroMachine) error {
	var ok bool
	cdr := mm.curr.Cdr()
	mm.curr, ok = cdr.(*values.Pair)
	if !ok {
		return values.ErrNotAPair
	}
	mm.pc++
	return nil
}

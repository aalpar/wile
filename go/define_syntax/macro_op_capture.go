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

import (
	"wile/match"
	"wile/values"
)

// MacroOpCapture verifies that the current element matches a pattern variable.
//
// This operation is used during pattern matching to confirm that a pattern
// variable symbol appears at the expected position. It differs from MacroOpMatch
// in that it's specifically for pattern variables (which will later be bound
// to captured values) rather than literals.
//
// # Behavior
//
// Compares mm.curr.Car() against the binding symbol. If they match, advances
// the program counter. Otherwise, returns a match error.
//
// Note: The actual value capture/binding is performed by MacroOpBind.
// MacroOpCapture only validates that the expected pattern variable is present.
//
// # Example
//
// For pattern (let ((x e)) body):
//   - 'x' and 'e' and 'body' are pattern variables
//   - MacroOpCapture(x) verifies 'x' appears at the binding position
//   - Later operations capture the actual values bound to these variables
type MacroOpCapture struct {
	binding *values.Symbol
}

// NewMacroOpCapture creates a new MacroOpCapture operation for the given symbol.
// The symbol is the pattern variable that should appear at the current position.
func NewMacroOpCapture(sym *values.Symbol) *MacroOpCapture {
	return &MacroOpCapture{binding: sym}
}

// Apply verifies the current element matches the expected pattern variable.
// Returns match.ErrNotAMatch if the symbols are not equal.
// Increments the program counter on success.
func (p *MacroOpCapture) Apply(mm *MacroMachine) error {
	car := mm.curr.Car()
	if !p.binding.EqualTo(car) {
		return match.ErrNotAMatch
	}
	mm.pc++
	return nil
}

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

// MacroOpBind binds a captured value to a pattern variable.
//
// This operation associates a pattern variable with a value captured during
// matching. The bindings are stored in the MacroMachine's tree structure
// and used during template expansion to substitute pattern variables with
// their captured values.
//
// # Behavior
//
// Currently a placeholder that advances the program counter.
// Full implementation would store the binding in mm.tree.bindings.
//
// # Example
//
// For pattern (let ((name val)) body) matching (let ((x 42)) (+ x 1)):
//   - MacroOpBind(name) would bind 'name' -> 'x'
//   - MacroOpBind(val) would bind 'val' -> 42
//   - MacroOpBind(body) would bind 'body' -> '(+ x 1)'
//
// During template expansion, references to 'name', 'val', and 'body' would
// be replaced with their bound values.
type MacroOpBind struct {
	key *values.Symbol
}

// NewMacroOpBind creates a new MacroOpBind operation.
// The key parameter (currently unused) would specify the pattern variable name.
func NewMacroOpBind() *MacroOpBind {
	return &MacroOpBind{}
}

// Apply binds a value to a pattern variable.
// Currently a placeholder that only advances the program counter.
func (p *MacroOpBind) Apply(mm *MacroMachine) error {
	mm.pc++
	return nil
}

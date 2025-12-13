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

package machine

import (
	"wile/syntax"
	"wile/values"
)

// PrimitiveExpanderFunc is the type for expand-time special form handlers.
// These functions handle macro expansion of primitive forms like
// `if`, `lambda`, `define`, `quote`, etc.
//
// Parameters:
//   - etc: The expander-time continuation (expander state)
//   - ectx: The expand-time call context
//   - sym: The keyword symbol (e.g., 'if', 'lambda')
//   - expr: The expression arguments (everything after the keyword)
//
// Returns the expanded syntax value.
type PrimitiveExpanderFunc func(
	etc *ExpanderTimeContinuation,
	ectx ExpandTimeCallContext,
	sym *syntax.SyntaxSymbol,
	expr syntax.SyntaxValue,
) (syntax.SyntaxValue, error)

// PrimitiveExpander wraps a PrimitiveExpanderFunc as a values.Value so it can
// be stored in the environment.
type PrimitiveExpander struct {
	name string
	fn   PrimitiveExpanderFunc
}

// NewPrimitiveExpander creates a new primitive expander.
func NewPrimitiveExpander(name string, fn PrimitiveExpanderFunc) *PrimitiveExpander {
	return &PrimitiveExpander{name: name, fn: fn}
}

// Name returns the name of this primitive expander.
func (p *PrimitiveExpander) Name() string {
	return p.name
}

// Expand invokes the primitive expander function.
func (p *PrimitiveExpander) Expand(
	etc *ExpanderTimeContinuation,
	ectx ExpandTimeCallContext,
	sym *syntax.SyntaxSymbol,
	expr syntax.SyntaxValue,
) (syntax.SyntaxValue, error) {
	return p.fn(etc, ectx, sym, expr)
}

// SchemeString implements values.Value interface.
func (p *PrimitiveExpander) SchemeString() string {
	return "#<primitive-expander:" + p.name + ">"
}

// IsVoid implements values.Value interface.
func (p *PrimitiveExpander) IsVoid() bool {
	return false
}

// EqualTo implements values.Value interface.
func (p *PrimitiveExpander) EqualTo(other values.Value) bool {
	if other == nil {
		return false
	}
	otherPE, ok := other.(*PrimitiveExpander)
	if !ok {
		return false
	}
	return p.name == otherPE.name
}

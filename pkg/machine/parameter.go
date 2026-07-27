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

import "github.com/aalpar/wile/pkg/values"

var (
	_ values.Value    = (*Parameter)(nil)
	_ values.Callable = (*Parameter)(nil)
)

// Parameter represents an R7RS parameter object.
// Parameters are dynamically-scoped variables that can be temporarily
// rebound using parameterize. They act as procedures:
//   - (param) returns the current value
//   - (param val) sets the parameter's BASE value (after applying converter if
//     present); it does not affect an active parameterize binding, which is
//     carried as a continuation mark and read first by (param)
type Parameter struct {
	value     values.Value // current value
	converter Closure      // optional converter procedure (Closure), or nil
}

// NewParameter creates a new parameter with the given initial value and optional converter.
// The converter should be a procedure that takes one argument and returns the converted value.
// Pass nil for converter if no conversion is needed.
func NewParameter(init values.Value, converter Closure) *Parameter {
	return &Parameter{
		value:     init,
		converter: converter,
	}
}

// Value returns the current value of the parameter.
func (p *Parameter) Value() values.Value {
	return p.value
}

// SetValue sets the current value of the parameter.
// Note: This does NOT apply the converter. The caller is responsible for
// converting the value before calling SetValue.
func (p *Parameter) SetValue(v values.Value) {
	p.value = v
}

// Converter returns the converter procedure, or nil if none.
func (p *Parameter) Converter() Closure {
	return p.converter
}

// HasConverter returns true if the parameter has a converter procedure.
func (p *Parameter) HasConverter() bool {
	return p.converter != nil
}

// AcceptsArity reports whether this parameter can be called with n arguments.
// Parameters accept 0 args (get current value) or 1 arg (set value).
func (p *Parameter) AcceptsArity(n int) bool {
	return n == 0 || n == 1
}

// IsVoid returns true if the parameter is nil.
func (p *Parameter) IsVoid() bool {
	return p == nil
}

// EqualTo uses identity comparison for parameters.
// Two parameters are equal only if they are the same object.
func (p *Parameter) EqualTo(v values.Value) bool {
	other, ok := v.(*Parameter)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the parameter.
func (p *Parameter) SchemeString() string {
	return "#<parameter>"
}

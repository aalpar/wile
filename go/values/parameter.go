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

package values

var (
	_ Value = (*Parameter)(nil)
)

// Parameter represents an R7RS parameter object.
// Parameters are dynamically-scoped variables that can be temporarily
// rebound using parameterize. They act as procedures:
//   - (param) returns the current value
//   - (param val) sets the current value (after applying converter if present)
type Parameter struct {
	value     Value // current value
	converter Value // optional converter procedure (MachineClosure), or nil
}

// NewParameter creates a new parameter with the given initial value and optional converter.
// The converter should be a procedure that takes one argument and returns the converted value.
// Pass nil for converter if no conversion is needed.
func NewParameter(init Value, converter Value) *Parameter {
	return &Parameter{
		value:     init,
		converter: converter,
	}
}

// Value returns the current value of the parameter.
func (p *Parameter) Value() Value {
	return p.value
}

// SetValue sets the current value of the parameter.
// Note: This does NOT apply the converter. The caller is responsible for
// converting the value before calling SetValue.
func (p *Parameter) SetValue(v Value) {
	p.value = v
}

// Converter returns the converter procedure, or nil if none.
func (p *Parameter) Converter() Value {
	return p.converter
}

// HasConverter returns true if the parameter has a converter procedure.
func (p *Parameter) HasConverter() bool {
	return p.converter != nil
}

func (p *Parameter) IsVoid() bool {
	return p == nil
}

// EqualTo uses identity comparison for parameters.
// Two parameters are equal only if they are the same object.
func (p *Parameter) EqualTo(v Value) bool {
	other, ok := v.(*Parameter)
	if !ok {
		return false
	}
	return p == other
}

func (p *Parameter) SchemeString() string {
	return "#<parameter>"
}

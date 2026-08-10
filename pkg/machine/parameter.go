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

// ParameterBase says whether a parameter's base value may be rewritten by
// applying it to one argument. Parameterize is unaffected either way: it binds a
// continuation mark, never the base.
type ParameterBase bool

const (
	// MutableBase is the ordinary R7RS parameter: (param val) rewrites the base.
	MutableBase ParameterBase = false
	// ImmutableBase refuses (param val). Required of any parameter object shared
	// across Engines, whose safety argument rests on the base never changing --
	// exceptionHandlerParam is the one such singleton.
	ImmutableBase ParameterBase = true
)

// Parameter represents an R7RS parameter object.
// Parameters are dynamically-scoped variables that can be temporarily
// rebound using parameterize. They act as procedures:
//   - (param) returns the current value
//   - (param val) sets the parameter's BASE value (after applying converter if
//     present); it does not affect an active parameterize binding, which is
//     carried as a continuation mark and read first by (param)
//
// A parameter constructed with ImmutableBase refuses the one-argument form; see
// ParameterBase.
type Parameter struct {
	value values.Value // current value
	// converter is values.Callable, not Closure: it is only ever applied, via
	// ApplyCallable, which dispatches six types where Closure has two. Assign
	// only untyped nil — HasConverter tests this field against nil, and a typed
	// nil in an interface field is not nil.
	converter values.Callable // optional converter procedure, or nil
	base      ParameterBase   // whether (param val) may rewrite value
}

// NewParameter creates a new parameter with the given initial value and optional converter.
// The converter should be a procedure that takes one argument and returns the converted value.
// Pass nil for converter if no conversion is needed. Pass MutableBase unless the
// object is shared beyond one Engine (see ParameterBase).
func NewParameter(init values.Value, converter values.Callable, base ParameterBase) *Parameter {
	return &Parameter{
		value:     init,
		converter: converter,
		base:      base,
	}
}

// HasImmutableBase reports whether this parameter refuses base-value rewrites.
func (p *Parameter) HasImmutableBase() bool {
	return bool(p.base)
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
func (p *Parameter) Converter() values.Callable {
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

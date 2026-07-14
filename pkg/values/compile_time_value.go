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

package values

import "fmt"

var _ Value = (*CompileTimeValue)(nil)

// CompileTimeValue wraps a value that is stored in the expand phase
// but accessible during macro expansion. This enables compile-time
// computation via define-for-syntax and begin-for-syntax.
type CompileTimeValue struct {
	Value Value
}

// NewCompileTimeValue creates a new compile-time value.
func NewCompileTimeValue(v Value) *CompileTimeValue {
	return &CompileTimeValue{
		Value: v,
	}
}

// Unwrap returns the underlying value.
func (p *CompileTimeValue) Unwrap() Value {
	return p.Value
}

// IsVoid returns true if the compile-time value is nil.
func (p *CompileTimeValue) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the compile-time values are equal.
func (p *CompileTimeValue) EqualTo(v Value) bool {
	return Equal(p, v)
}

// EqualComponents pushes the two wrapped values for Equal to compare.
func (p *CompileTimeValue) EqualComponents(v Value, push func(a, b Value)) bool {
	other, ok := v.(*CompileTimeValue)
	if !ok {
		return false
	}
	if p == nil || other == nil {
		return p == other
	}
	push(p.Value, other.Value)
	return true
}

// SchemeString returns the Scheme representation of the compile-time value.
func (p *CompileTimeValue) SchemeString() string {
	if p == nil {
		return "#<compile-time-value:void>"
	}
	return fmt.Sprintf("#<compile-time-value %s>", p.Value.SchemeString())
}

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

var (
	_ Value    = (*Boolean)(nil)
	_ Hashable = (*Boolean)(nil)

	// FalseValue is the singleton false boolean.
	FalseValue = newBoolean(false)
	// TrueValue is the singleton true boolean.
	TrueValue = newBoolean(true)
)

// Boolean represents a Scheme boolean value.
type Boolean struct {
	Value bool
}

// newBoolean creates a new boolean value. Callers outside this package
// must use TrueValue, FalseValue, or BoolToBoolean to ensure singleton identity.
func newBoolean(v bool) *Boolean {
	q := &Boolean{Value: v}
	return q
}

// HashCode returns a hash of the boolean value.
func (p *Boolean) HashCode() uint64 {
	if p.Value {
		return 1
	}
	return 0
}

// Datum returns the underlying boolean value.
func (p *Boolean) Datum() bool {
	return p.Value
}

// IsVoid returns true if the boolean is nil.
func (p *Boolean) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the values are equal booleans.
func (p *Boolean) EqualTo(v Value) bool {
	other, ok := v.(*Boolean)
	if ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the boolean.
func (p *Boolean) SchemeString() string {
	if !p.Value {
		return "#f"
	}
	return "#t"
}

// BoolToBoolean converts a Go bool to a Scheme boolean value.
func BoolToBoolean(b bool) *Boolean {
	if b {
		return TrueValue
	}
	return FalseValue
}

// BooleanToBool converts a Scheme *Boolean to a Go bool value.
func BooleanToBool(b *Boolean) bool {
	return b.Value
}

// ValueToBool converts a value into a Go bool using Scheme semantics.
// In Scheme, only #f is false; everything else (including 0, "", '()) is true.
func ValueToBool(b Value) bool {
	v, ok := b.(*Boolean)
	if !ok {
		return true
	}
	return v.Datum()
}

// ValueToBoolean converts a value into a Scheme *Boolean using Scheme semantics.
func ValueToBoolean(b Value) *Boolean {
	return BoolToBoolean(ValueToBool(b))
}

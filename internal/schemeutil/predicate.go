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

package schemeutil

import "github.com/aalpar/wile/values"

// BoolToBoolean converts a Go bool to a Scheme boolean value.
func BoolToBoolean(b bool) *values.Boolean {
	if b {
		return values.TrueValue
	}
	return values.FalseValue
}

// BooleanToBool converts a Scheme *Boolean to a Go bool value.
func BooleanToBool(b *values.Boolean) bool {
	return b == values.TrueValue
}

// ValueToBool converts a value into a Go bool using Scheme semantics.
func ValueToBool(b values.Value) bool {
	v, ok := b.(*values.Boolean)
	if !ok {
		return true
	}
	return v.Datum()
}

// ValueToBoolean converts a value into a Scheme *Boolean using Scheme semantics.
func ValueToBoolean(b values.Value) *values.Boolean {
	return BoolToBoolean(ValueToBool(b))
}

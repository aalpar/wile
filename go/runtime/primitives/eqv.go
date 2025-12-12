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


package primitives

import "wile/values"

// Eqv is a helper implementing eqv? semantics for memv and assv.
func Eqv(a, b values.Value) bool {
	if a == b {
		return true
	}
	switch va := a.(type) {
	case *values.Integer:
		if vb, ok := b.(*values.Integer); ok {
			return va.Value == vb.Value
		}
	case *values.Float:
		if vb, ok := b.(*values.Float); ok {
			return va.Value == vb.Value
		}
	case *values.Rational:
		if vb, ok := b.(*values.Rational); ok {
			return va.Rat().Cmp(vb.Rat()) == 0
		}
	case *values.Complex:
		if vb, ok := b.(*values.Complex); ok {
			return va.Value == vb.Value
		}
	case *values.Character:
		if vb, ok := b.(*values.Character); ok {
			return va.Value == vb.Value
		}
	}
	return false
}

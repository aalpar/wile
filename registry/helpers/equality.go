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

package helpers

import "github.com/aalpar/wile/values"

// Eqv is a helper implementing eqv? semantics for memv and assv.
//
// R7RS §6.1: eqv? returns #t for numbers that are = and both exact or both inexact.
// For exact integers (Integer and BigInteger), this means comparing numeric values.
func Eqv(a, b values.Value) bool {
	if a == b {
		return true
	}
	switch va := a.(type) {
	case *values.Integer:
		switch vb := b.(type) {
		case *values.Integer:
			return va.Value == vb.Value
		case *values.BigInteger:
			// Both are exact integers, compare numerically
			return va.Compare(vb) == 0
		}
	case *values.BigInteger:
		switch vb := b.(type) {
		case *values.BigInteger:
			return va.BigInt().Cmp(vb.BigInt()) == 0
		case *values.Integer:
			// Both are exact integers, compare numerically
			return va.Compare(vb) == 0
		}
	case *values.Float:
		if vb, ok := b.(*values.Float); ok { //nolint:gocritic
			return va.Value == vb.Value
		}
	case *values.BigFloat:
		if vb, ok := b.(*values.BigFloat); ok { //nolint:gocritic
			return va.BigFloatValue().Cmp(vb.BigFloatValue()) == 0
		}
	case *values.Rational:
		if vb, ok := b.(*values.Rational); ok { //nolint:gocritic
			return va.Rat().Cmp(vb.Rat()) == 0
		}
	case *values.Complex:
		if vb, ok := b.(*values.Complex); ok { //nolint:gocritic
			return va.Value == vb.Value
		}
	case *values.BigComplex:
		if vb, ok := b.(*values.BigComplex); ok { //nolint:gocritic
			return va.EqualTo(vb)
		}
	case *values.Character:
		if vb, ok := b.(*values.Character); ok { //nolint:gocritic
			return va.Value == vb.Value
		}
	}
	return false
}

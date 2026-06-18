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

import "github.com/aalpar/wile/pkg/values"

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
		vb, ok := b.(*values.Float)
		if ok {
			return va.Value == vb.Value
		}
	case *values.BigFloat:
		vb, ok := b.(*values.BigFloat)
		if ok {
			// NaN is not equal to anything, including itself (IEEE 754).
			// BigFloat stores NaN as a flag with a zero *big.Float; Cmp would
			// incorrectly return 0 (equal) without this guard.
			if va.IsNaN() || vb.IsNaN() {
				return false
			}
			return va.BigFloatValue().Cmp(vb.BigFloatValue()) == 0
		}
	case *values.Rational:
		vb, ok := b.(*values.Rational)
		if ok {
			return va.Rat().Cmp(vb.Rat()) == 0
		}
	case *values.Complex:
		vb, ok := b.(*values.Complex)
		if ok {
			return va.Value == vb.Value
		}
	case *values.BigComplex:
		vb, ok := b.(*values.BigComplex)
		if ok {
			return va.EqualTo(vb)
		}
	case *values.Character:
		vb, ok := b.(*values.Character)
		if ok {
			return va.Value == vb.Value
		}
	case *values.Symbol:
		vb, ok := b.(*values.Symbol)
		if ok {
			return va.Key == vb.Key
		}
	}
	return false
}

// EqIdentity implements eq? semantics: pointer identity for all types except
// symbols, which compare by name (R7RS §6.1, §6.5). Thin re-export of
// values.EqIdentity — the single source of truth — retained for API stability
// (registry/helpers is a public package; memq/assq pass it as a comparator value).
func EqIdentity(a, b values.Value) bool {
	return values.EqIdentity(a, b)
}

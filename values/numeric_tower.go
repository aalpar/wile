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

import (
	"math/big"
)

// Simplify attempts to reduce a number to a simpler type without losing information.
//
// Simplification rules:
// - BigComplex with zero imaginary → real part
// - Complex with zero imaginary → Float → possibly Integer
// - BigFloat that is an integer → BigInteger → possibly Integer
// - Float that is an integer → Integer
// - Rational that is an integer → BigInteger → possibly Integer
// - BigInteger that fits int64 → Integer
func Simplify(n Number) Number {
	switch v := n.(type) {
	case *BigComplex:
		if v.Imag().IsZero() {
			return Simplify(v.Real())
		}
	case *Complex:
		if imag(v.Value) == 0 {
			return Simplify(NewFloat(real(v.Value)))
		}
	case *BigFloat:
		if v.value.IsInt() {
			bi, _ := v.value.Int(nil)
			return Simplify(&BigInteger{value: bi})
		}
	case *Float:
		// Check if float is a whole number that fits in int64
		if v.Value == float64(int64(v.Value)) {
			return NewInteger(int64(v.Value))
		}
	case *Rational:
		if v.IsInteger() {
			return Simplify(&BigInteger{value: new(big.Int).Set(v.Num())})
		}
	case *BigInteger:
		if v.value.IsInt64() {
			return NewInteger(v.value.Int64())
		}
	}
	return n
}

// Exactness represents whether a number is exact or inexact.
//
// R7RS §6.2.2: Numbers are either exact or inexact. A number is exact if it
// was written as an exact constant or derived from exact numbers using only
// exact operations. Otherwise, it is inexact.
type Exactness int

// Exactness constants for R7RS exact/inexact classification.
const (
	Exact Exactness = iota
	Inexact
)

// ExactnessOf returns the exactness of a number.
//
// R7RS §6.2.2:
// - Integer, BigInteger, Rational are exact
// - Float, BigFloat, Complex are inexact
// - BigComplex depends on its components
func ExactnessOf(n Number) Exactness {
	switch v := n.(type) {
	case *Integer, *BigInteger, *Rational:
		return Exact
	case *Float, *BigFloat, *Complex:
		return Inexact
	case *BigComplex:
		if v.IsExact() {
			return Exact
		}
		return Inexact
	}
	panic(ErrNotANumber)
}

// ResultExactness computes the exactness of a binary operation result.
//
// R7RS §6.2.2: exact op exact = exact, otherwise inexact.
func ResultExactness(a, b Number) Exactness {
	if ExactnessOf(a) == Inexact || ExactnessOf(b) == Inexact {
		return Inexact
	}
	return Exact
}

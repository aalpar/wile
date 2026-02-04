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

// NumericRank represents the position of a numeric type in the promotion tower.
//
// The ordering is: Integer < BigInteger < Rational < Float < BigFloat < Complex < BigComplex
//
// This is a design decision for this implementation, not an R7RS requirement.
// R7RS §6.2.1 defines a mathematical subtype tower (number ⊃ complex ⊃ real ⊃ rational ⊃ integer)
// which describes containment, not promotion. R7RS §6.2.3 permits implementations to use
// any internal representations.
//
// We choose this ordering because:
// 1. Promoting to "wider" types preserves information
// 2. Total ordering enables uniform dispatch
// 3. Users can reason about result types
type NumericRank int

// NumericRank constants define the position of each numeric type in the tower.
const (
	RankInteger NumericRank = iota
	RankBigInteger
	RankRational
	RankFloat
	RankBigFloat
	RankComplex
	RankBigComplex
)

// Rank returns the position of a number in the numeric tower.
func Rank(n Number) NumericRank {
	switch n.(type) {
	case *Integer:
		return RankInteger
	case *BigInteger:
		return RankBigInteger
	case *Rational:
		return RankRational
	case *Float:
		return RankFloat
	case *BigFloat:
		return RankBigFloat
	case *Complex:
		return RankComplex
	case *BigComplex:
		return RankBigComplex
	}
	panic(ErrNotANumber)
}

// Promote converts a number to the target rank.
// Returns the same value if already at or above the target rank.
//
// R7RS §6.2.2: Operations involving inexact numbers produce inexact results.
// Promotion from exact to inexact types follows this rule.
func Promote(n Number, target NumericRank) Number {
	current := Rank(n)
	if current >= target {
		return n
	}
	// Chain of promotions
	for current < target {
		n = promoteOnce(n)
		current = Rank(n)
	}
	return n
}

// promoteOnce promotes a number exactly one level up the tower.
func promoteOnce(n Number) Number {
	switch v := n.(type) {
	case *Integer:
		return NewBigIntegerFromInt64(v.Value)
	case *BigInteger:
		return NewRationalFromBigInt(v.value, big.NewInt(1))
	case *Rational:
		f, _ := v.value.Float64()
		return NewFloat(f)
	case *Float:
		return NewBigFloatFromFloat64(v.Value)
	case *BigFloat:
		// Promote to Complex (as real part with zero imaginary)
		return NewComplex(complex(v.Float64(), 0))
	case *Complex:
		return NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
	case *BigComplex:
		return v // Already at top
	}
	panic(ErrNotANumber)
}

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

// CommonRank returns the higher rank of two numbers.
func CommonRank(a, b Number) NumericRank {
	rankA, rankB := Rank(a), Rank(b)
	if rankA > rankB {
		return rankA
	}
	return rankB
}

// PromoteBoth promotes both numbers to their common rank.
func PromoteBoth(a, b Number) (Number, Number) {
	target := CommonRank(a, b)
	return Promote(a, target), Promote(b, target)
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

// BinaryOp performs a binary operation on two numbers using promotion and simplification.
//
// The algorithm:
// 1. Promote both operands to their common rank
// 2. Perform the operation using same-type methods
// 3. Simplify the result
//
// This provides unified dispatch for all numeric operations.
func BinaryOp(a, b Number, op func(Number, Number) Number) Number {
	promotedA, promotedB := PromoteBoth(a, b)
	result := op(promotedA, promotedB)
	return Simplify(result)
}

// addOp performs addition on two numbers of the same type.
func addOp(a, b Number) Number {
	switch va := a.(type) {
	case *Integer:
		return va.addSame(b.(*Integer))
	case *BigInteger:
		return va.addSame(b.(*BigInteger))
	case *Rational:
		return va.addSame(b.(*Rational))
	case *Float:
		return va.addSame(b.(*Float))
	case *BigFloat:
		return va.addSame(b.(*BigFloat))
	case *Complex:
		return va.addSame(b.(*Complex))
	case *BigComplex:
		return va.addSame(b.(*BigComplex))
	}
	panic(ErrNotANumber)
}

// subtractOp performs subtraction on two numbers of the same type.
func subtractOp(a, b Number) Number {
	switch va := a.(type) {
	case *Integer:
		return va.subtractSame(b.(*Integer))
	case *BigInteger:
		return va.subtractSame(b.(*BigInteger))
	case *Rational:
		return va.subtractSame(b.(*Rational))
	case *Float:
		return va.subtractSame(b.(*Float))
	case *BigFloat:
		return va.subtractSame(b.(*BigFloat))
	case *Complex:
		return va.subtractSame(b.(*Complex))
	case *BigComplex:
		return va.subtractSame(b.(*BigComplex))
	}
	panic(ErrNotANumber)
}

// multiplyOp performs multiplication on two numbers of the same type.
func multiplyOp(a, b Number) Number {
	switch va := a.(type) {
	case *Integer:
		return va.multiplySame(b.(*Integer))
	case *BigInteger:
		return va.multiplySame(b.(*BigInteger))
	case *Rational:
		return va.multiplySame(b.(*Rational))
	case *Float:
		return va.multiplySame(b.(*Float))
	case *BigFloat:
		return va.multiplySame(b.(*BigFloat))
	case *Complex:
		return va.multiplySame(b.(*Complex))
	case *BigComplex:
		return va.multiplySame(b.(*BigComplex))
	}
	panic(ErrNotANumber)
}

// divideOp performs division on two numbers of the same type.
func divideOp(a, b Number) Number {
	switch va := a.(type) {
	case *Integer:
		return va.divideSame(b.(*Integer))
	case *BigInteger:
		return va.divideSame(b.(*BigInteger))
	case *Rational:
		return va.divideSame(b.(*Rational))
	case *Float:
		return va.divideSame(b.(*Float))
	case *BigFloat:
		return va.divideSame(b.(*BigFloat))
	case *Complex:
		return va.divideSame(b.(*Complex))
	case *BigComplex:
		return va.divideSame(b.(*BigComplex))
	}
	panic(ErrNotANumber)
}

// compareOp compares two numbers of the same type.
func compareOp(a, b Number) int {
	switch va := a.(type) {
	case *Integer:
		return va.compareSame(b.(*Integer))
	case *BigInteger:
		return va.compareSame(b.(*BigInteger))
	case *Rational:
		return va.compareSame(b.(*Rational))
	case *Float:
		return va.compareSame(b.(*Float))
	case *BigFloat:
		return va.compareSame(b.(*BigFloat))
	case *Complex:
		return va.compareSame(b.(*Complex))
	case *BigComplex:
		return va.compareSame(b.(*BigComplex))
	}
	panic(ErrNotANumber)
}

// TowerAdd adds two numbers using the numeric tower.
func TowerAdd(a, b Number) Number {
	return BinaryOp(a, b, addOp)
}

// TowerSubtract subtracts two numbers using the numeric tower.
func TowerSubtract(a, b Number) Number {
	return BinaryOp(a, b, subtractOp)
}

// TowerMultiply multiplies two numbers using the numeric tower.
func TowerMultiply(a, b Number) Number {
	return BinaryOp(a, b, multiplyOp)
}

// TowerDivide divides two numbers using the numeric tower.
func TowerDivide(a, b Number) Number {
	return BinaryOp(a, b, divideOp)
}

// TowerCompare compares two numbers using the numeric tower.
func TowerCompare(a, b Number) int {
	promotedA, promotedB := PromoteBoth(a, b)
	return compareOp(promotedA, promotedB)
}

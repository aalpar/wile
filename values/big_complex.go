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

import (
	"math"
	"math/big"
)

var (
	_ Value         = (*BigComplex)(nil)
	_ Number        = (*BigComplex)(nil)
	_ ComplexNumber = (*BigComplex)(nil)
	_ Hashable      = (*BigComplex)(nil)
)

// BigComplex represents an arbitrary-precision complex number.
// The real and imaginary parts can be *BigInteger, *Rational (exact), or *BigFloat (inexact).
//
// R7RS §6.2.1: Complex numbers are part of the numeric tower hierarchy:
//
//	number ⊃ complex ⊃ real ⊃ rational ⊃ integer
//
// R7RS §6.2.2: BigComplex is exact if both parts are BigInteger or Rational,
// inexact if either part is BigFloat. Operations follow exactness contagion rules.
type BigComplex struct {
	real Number // *BigInteger, *Rational, or *BigFloat only
	imag Number // *BigInteger, *Rational, or *BigFloat only
}

// NewBigComplex creates a new BigComplex from real and imaginary parts.
// Parts must be *BigInteger, *Rational, or *BigFloat. Other types will panic.
func NewBigComplex(rel, iam Number) *BigComplex {
	validateBigComplexPart(rel)
	validateBigComplexPart(iam)
	q := &BigComplex{real: rel, imag: iam}
	return q
}

// NewBigComplexFromBigIntegers creates an exact BigComplex from BigInteger parts.
func NewBigComplexFromBigIntegers(rel, iam *BigInteger) *BigComplex {
	q := &BigComplex{real: rel, imag: iam}
	return q
}

// NewBigComplexFromBigFloats creates an inexact BigComplex from BigFloat parts.
func NewBigComplexFromBigFloats(rel, iam *BigFloat) *BigComplex {
	q := &BigComplex{real: rel, imag: iam}
	return q
}

// validateBigComplexPart ensures the part is a valid type for BigComplex.
func validateBigComplexPart(n Number) {
	switch n.(type) {
	case *BigInteger, *BigFloat, *Rational:
		return
	default:
		panic(ErrNotANumber)
	}
}

// Real returns the real part of the complex number.
func (p *BigComplex) Real() Number {
	return p.real
}

// Imag returns the imaginary part of the complex number.
func (p *BigComplex) Imag() Number {
	return p.imag
}

// RealAsBigFloat returns the real part converted to BigFloat for calculations.
func (p *BigComplex) RealAsBigFloat() *BigFloat {
	return toBigFloat(p.real)
}

// ImagAsBigFloat returns the imaginary part converted to BigFloat for calculations.
func (p *BigComplex) ImagAsBigFloat() *BigFloat {
	return toBigFloat(p.imag)
}

// toBigFloat converts a Number to BigFloat.
// Handles all five types that can appear as BigComplex parts or intermediate
// arithmetic results: BigFloat, BigInteger, Rational, Integer, and Float.
func toBigFloat(n Number) *BigFloat {
	switch v := n.(type) {
	case *BigFloat:
		return v
	case *BigInteger:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetInt(v.value)
		return &BigFloat{value: bf}
	case *Rational:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(v.Rat())
		return &BigFloat{value: bf}
	case *Integer:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetInt64(v.Value)
		return &BigFloat{value: bf}
	case *Float:
		return NewBigFloatFromFloat64(v.Value)
	}
	panic(ErrNotANumber)
}

// maybeSimplify returns a real number if imag is zero, otherwise returns BigComplex.
func maybeSimplify(rel, iam Number) Number {
	if iam.IsZero() {
		return rel
	}
	return NewBigComplex(rel, iam)
}

// promoteToBigComplexPart converts any Number to a BigComplex-compatible part.
func promoteToBigComplexPart(n Number) Number {
	switch v := n.(type) {
	case *BigInteger:
		return v
	case *BigFloat:
		return v
	case *Rational:
		return v // Preserve exactness
	case *Integer:
		return NewBigIntegerFromInt64(v.Value)
	case *Float:
		return NewBigFloatFromFloat64(v.Value)
	}
	panic(ErrNotANumber)
}

// Kind returns the numeric kind for dispatch table indexing.
func (p *BigComplex) Kind() NumericKind {
	return KindBigComplex
}

// BigComplex has 5 dispatch tables (no bigComplexLessThan).
// Complex ordering is undefined in R7RS §6.2.6 — LessThan delegates to Compare,
// which uses magnitude comparison as a total order for internal use (sorting, etc.)
// but is NOT mathematical ordering. Compare is initialized below like all other types.
var bigComplexAdd [numKinds]func(*BigComplex, Number) Number
var bigComplexSubtract [numKinds]func(*BigComplex, Number) Number
var bigComplexCompare [numKinds]func(*BigComplex, Number) int
var bigComplexMultiply [numKinds]func(*BigComplex, Number) Number
var bigComplexDivide [numKinds]func(*BigComplex, Number) Number

func init() {
	bigComplexAdd = makeAddDispatch(KindBigComplex, func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		newReal := p.real.Add(v.real)
		newImag := p.imag.Add(v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	})

	bigComplexSubtract = makeSubtractDispatch(KindBigComplex, func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		newReal := p.real.Subtract(v.real)
		newImag := p.imag.Subtract(v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	})

	bigComplexCompare = makeCompareDispatch(KindBigComplex, func(p *BigComplex, o Number) int {
		return toBigFloat(p.real).Compare(toBigFloat(o.(*BigComplex).real))
	})

	bigComplexMultiply = makeMultiplyDispatch(KindBigComplex, func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		ac := p.real.Multiply(v.real)
		bd := p.imag.Multiply(v.imag)
		ad := p.real.Multiply(v.imag)
		bc := p.imag.Multiply(v.real)
		newReal := ac.Subtract(bd)
		newImag := ad.Add(bc)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	})

	bigComplexDivide = makeDivideDispatch(KindBigComplex, func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		// Scalar divisor (d=0): divide each part directly to preserve exactness.
		// Scalars promoted from Integer/BigInteger/Rational arrive with BigInteger(0) imag.
		if v.imag.IsZero() {
			newReal := p.real.Divide(v.real)
			newImag := p.imag.Divide(v.real)
			return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
		}
		// General case: (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
		ac := p.real.Multiply(v.real)
		bd := p.imag.Multiply(v.imag)
		bc := p.imag.Multiply(v.real)
		ad := p.real.Multiply(v.imag)
		cc := v.real.Multiply(v.real)
		dd := v.imag.Multiply(v.imag)

		numerReal := ac.Add(bd)
		numerImag := bc.Subtract(ad)
		denom := cc.Add(dd)

		newReal := toBigFloat(numerReal).Divide(toBigFloat(denom))
		newImag := toBigFloat(numerImag).Divide(toBigFloat(denom))
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	})
}

// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
// Inexactness is contagious per R7RS §6.2.2.
func (p *BigComplex) Add(o Number) Number {
	return bigComplexAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this BigComplex and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *BigComplex) Subtract(o Number) Number {
	return bigComplexSubtract[o.Kind()](p, o)
}

// Multiply returns the product of this BigComplex and another number.
// Complex multiplication: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2 Exactness: exact * exact = exact, exact * inexact = inexact.
func (p *BigComplex) Multiply(o Number) Number {
	if o.IsZero() {
		return multiplyResultForZero(o, p)
	}
	if p.IsZero() && o.IsFinite() {
		return multiplyResultForZero(p, o)
	}
	return bigComplexMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this BigComplex and another number.
// Complex division: (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
//
// R7RS §6.2.6: The / procedure returns the quotient of its arguments.
// R7RS §6.2.2 Exactness: exact / exact = exact, exact / inexact = inexact.
func (p *BigComplex) Divide(o Number) Number {
	if o.IsZero() && o.IsExact() {
		panic(ErrDivisionByZero)
	}
	return bigComplexDivide[o.Kind()](p, o)
}

// Negate returns the negation of this BigComplex.
func (p *BigComplex) Negate() Number {
	return NewBigComplex(p.real.Negate(), p.imag.Negate())
}

// IsZero returns true if both real and imaginary parts are zero.
func (p *BigComplex) IsZero() bool {
	return p.real.IsZero() && p.imag.IsZero()
}

// LessThan compares the real parts of complex numbers.
// Following R7RS, < is not mathematically defined for complex numbers,
// but we follow the existing Complex.LessThan pattern of comparing real parts.
func (p *BigComplex) LessThan(o Number) bool {
	return p.Compare(o) < 0
}

// Compare compares this BigComplex with another number by real parts.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value.
// For complex numbers, we compare real parts only (matching Complex behavior).
func (p *BigComplex) Compare(o Number) int {
	v, ok := o.(*BigComplex)
	if ok {
		return toBigFloat(p.real).Compare(toBigFloat(v.real))
	}
	return bigComplexCompare[o.Kind()](p, o)
}

// IsReal returns true if the imaginary part is zero.
func (p *BigComplex) IsReal() bool {
	return p.imag.IsZero()
}

// IsExact returns true if both parts are exact (BigInteger or Rational).
//
// R7RS §6.2.2: A complex number is exact if both real and imaginary parts are exact.
func (p *BigComplex) IsExact() bool {
	return isExactPart(p.real) && isExactPart(p.imag)
}

// IsInteger returns true if the imaginary part is zero and the real part is an integer.
//
// R7RS §6.2.6: integer? returns #t for complex numbers with zero imaginary
// part whose real part is an integer.
func (p *BigComplex) IsInteger() bool {
	return p.IsReal() && p.RealPart().IsInteger()
}

// IsRational returns true if this BigComplex is a real, finite number.
//
// R7RS §6.2.6: rational? returns #t for finite real numbers.
// Inf and NaN are not rational, even when the imaginary part is zero.
func (p *BigComplex) IsRational() bool {
	return p.IsReal() && p.real.IsRational()
}

// IsFinite returns true if both real and imaginary parts are finite.
//
// R7RS §6.2.6: finite? returns #t if neither part is Inf or NaN.
func (p *BigComplex) IsFinite() bool {
	return p.real.IsFinite() && p.imag.IsFinite()
}

// IsNaN returns true if either real or imaginary part is NaN.
//
// R7RS §6.2.6: nan? returns #t for complex numbers with a NaN component.
func (p *BigComplex) IsNaN() bool {
	return p.real.IsNaN() || p.imag.IsNaN()
}

// isExactPart returns true if the number is an exact type (BigInteger or Rational).
func isExactPart(n Number) bool {
	switch n.(type) {
	case *BigInteger, *Rational:
		return true
	}
	return false
}

// ToExact converts this BigComplex to an exact representation.
//
// R7RS §6.2.6: exact returns an exact representation of its argument.
// If already exact, returns itself. Otherwise converts BigFloat parts to BigInteger
// by truncating (may lose precision).
func (p *BigComplex) ToExact() Number {
	if p.IsExact() {
		return p
	}
	realExact := toExactPart(p.real)
	imagExact := toExactPart(p.imag)
	if imagExact.IsZero() {
		return realExact
	}
	return NewBigComplex(realExact, imagExact)
}

// toExactPart converts a BigComplex part to a BigComplex-compatible exact type
// (*BigInteger or *Rational).
//
// R7RS §6.2.6: exact converts to exact representation, preserving value.
// For BigFloat, converts via ToExact() then simplifies integer-valued rationals
// (3/1 → 3). Simplify may return *Integer; we promote it back to *BigInteger
// since NewBigComplex only accepts *BigInteger, *Rational, or *BigFloat.
func toExactPart(n Number) Number {
	switch v := n.(type) {
	case *BigInteger, *Rational:
		return v
	case *BigFloat:
		q := Simplify(v.ToExact())
		i, ok := q.(*Integer)
		if ok {
			return NewBigIntegerFromInt64(i.Value)
		}
		return q
	}
	panic(ErrNotANumber)
}

// ToInexact converts this BigComplex to an inexact representation.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
// Converts BigInteger parts to BigFloat.
func (p *BigComplex) ToInexact() Number {
	if !p.IsExact() {
		return p
	}
	realInexact := toBigFloat(p.real)
	imagInexact := toBigFloat(p.imag)
	if imagInexact.IsZero() {
		return realInexact
	}
	return NewBigComplexFromBigFloats(realInexact, imagInexact)
}

// Abs returns the magnitude of this BigComplex as a Number.
//
// R7RS §6.2.6: For complex numbers, abs returns the magnitude.
func (p *BigComplex) Abs() Number {
	return p.Magnitude()
}

// RealPart returns the real part of this complex number as a Number.
//
// R7RS §6.2.6: real-part returns the real part of a complex number.
func (p *BigComplex) RealPart() Number {
	return p.real
}

// ImagPart returns the imaginary part of this complex number as a Number.
//
// R7RS §6.2.6: imag-part returns the imaginary part of a complex number.
func (p *BigComplex) ImagPart() Number {
	return p.imag
}

// Magnitude returns the absolute value (modulus) of the complex number.
// |a+bi| = sqrt(a² + b²)
func (p *BigComplex) Magnitude() *BigFloat {
	a := toBigFloat(p.real)
	b := toBigFloat(p.imag)
	aa := new(big.Float).Mul(a.value, a.value)
	bb := new(big.Float).Mul(b.value, b.value)
	sum := new(big.Float).Add(aa, bb)
	result := new(big.Float).SetPrec(DefaultBigFloatPrecision).Sqrt(sum)
	return &BigFloat{value: result}
}

// Phase returns the phase (argument) of the complex number in radians.
// Uses atan2(imag, real).
func (p *BigComplex) Phase() *BigFloat {
	// Convert to float64 for atan2 calculation
	r := toBigFloat(p.real).Float64()
	i := toBigFloat(p.imag).Float64()
	phase := math.Atan2(i, r)
	return NewBigFloatFromFloat64(phase)
}

// Conjugate returns the complex conjugate (a-bi for a+bi).
func (p *BigComplex) Conjugate() *BigComplex {
	return NewBigComplex(p.real, p.imag.Negate())
}

// SchemeString returns the Scheme representation of this BigComplex.
func (p *BigComplex) SchemeString() string {
	realStr := p.real.SchemeString()
	imagStr := p.imag.SchemeString()
	// Check if imaginary part is negative
	isNeg := false
	switch v := p.imag.(type) {
	case *BigInteger:
		isNeg = v.IsNegative()
	case *BigFloat:
		isNeg = v.IsNegative()
	case *Rational:
		isNeg = v.IsNegative()
	}
	if isNeg {
		return realStr + imagStr + "i"
	}
	return realStr + "+" + imagStr + "i"
}

// IsVoid returns true if this BigComplex is nil.
func (p *BigComplex) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both complex numbers have equal real and imaginary parts.
//
// R7RS §6.2.6: The = procedure compares numerical values for equality.
func (p *BigComplex) EqualTo(o Value) bool {
	v, ok := o.(*BigComplex)
	if !ok {
		// Check if equal to regular Complex
		c, ok := o.(*Complex)
		if ok {
			pReal := toBigFloat(p.real).Float64()
			pImag := toBigFloat(p.imag).Float64()
			return pReal == real(c.Value) && pImag == imag(c.Value)
		}
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	// NaN is not equal to anything, including itself (IEEE 754).
	if p.IsNaN() || v.IsNaN() {
		return false
	}
	// Compare real parts
	if !p.real.EqualTo(v.real) {
		// Try comparing as BigFloat
		if toBigFloat(p.real).Compare(v.real) != 0 {
			return false
		}
	}
	// Compare imaginary parts
	if !p.imag.EqualTo(v.imag) {
		// Try comparing as BigFloat
		if toBigFloat(p.imag).Compare(v.imag) != 0 {
			return false
		}
	}
	return true
}

// HashCode returns a hash of the complex value.
// Hashes real and imaginary parts independently via hashInexactNumeric
// and combines them with a multiplicative mixing constant.
// Uses the same combining formula as Complex.HashCode for cross-type consistency:
// when BigComplex.EqualTo(*Complex) holds, both produce equal hashes.
func (p *BigComplex) HashCode() uint64 {
	r := hashInexactNumeric(toBigFloat(p.real).value)
	i := hashInexactNumeric(toBigFloat(p.imag).value)
	return r ^ (i * 0x9e3779b97f4a7c15)
}

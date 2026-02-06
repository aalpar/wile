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
	_ Value  = (*BigComplex)(nil)
	_ Number = (*BigComplex)(nil)
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
		panic("BigComplex parts must be BigInteger, Rational, or BigFloat")
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

// toBigFloat converts a BigInteger, Rational, or BigFloat to BigFloat.
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
	}
	panic("toBigFloat: unexpected type")
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
	panic("promoteToBigComplexPart: unexpected type")
}

// addParts adds two BigComplex-compatible parts.
func addParts(a, b Number) Number {
	// Parts are BigInteger, Rational, or BigFloat
	switch va := a.(type) {
	case *BigInteger:
		switch vb := b.(type) {
		case *BigInteger:
			return va.Add(vb)
		case *Rational:
			return vb.Add(va) // Rational + BigInteger = Rational
		case *BigFloat:
			return toBigFloat(va).Add(vb)
		}
	case *Rational:
		switch vb := b.(type) {
		case *Rational:
			return va.Add(vb) // Rational + Rational = Rational
		case *BigInteger:
			return va.Add(vb) // Rational + BigInteger = Rational
		case *BigFloat:
			return toBigFloat(va).Add(vb) // Goes inexact
		}
	case *BigFloat:
		return va.Add(toBigFloat(b))
	}
	panic("addParts: unexpected type")
}

// subtractParts subtracts two BigComplex-compatible parts.
func subtractParts(a, b Number) Number {
	switch va := a.(type) {
	case *BigInteger:
		switch vb := b.(type) {
		case *BigInteger:
			return va.Subtract(vb)
		case *Rational:
			// BigInteger - Rational: convert BigInteger to Rational
			ra := NewRationalFromBigInt(va.value, big.NewInt(1))
			return ra.Subtract(vb)
		case *BigFloat:
			return toBigFloat(va).Subtract(vb)
		}
	case *Rational:
		switch vb := b.(type) {
		case *Rational:
			return va.Subtract(vb)
		case *BigInteger:
			return va.Subtract(vb)
		case *BigFloat:
			return toBigFloat(va).Subtract(vb)
		}
	case *BigFloat:
		return va.Subtract(toBigFloat(b))
	}
	panic("subtractParts: unexpected type")
}

// multiplyParts multiplies two BigComplex-compatible parts.
func multiplyParts(a, b Number) Number {
	switch va := a.(type) {
	case *BigInteger:
		switch vb := b.(type) {
		case *BigInteger:
			return va.Multiply(vb)
		case *Rational:
			return vb.Multiply(va) // Rational * BigInteger = Rational
		case *BigFloat:
			return toBigFloat(va).Multiply(vb)
		}
	case *Rational:
		switch vb := b.(type) {
		case *Rational:
			return va.Multiply(vb)
		case *BigInteger:
			return va.Multiply(vb)
		case *BigFloat:
			return toBigFloat(va).Multiply(vb)
		}
	case *BigFloat:
		return va.Multiply(toBigFloat(b))
	}
	panic("multiplyParts: unexpected type")
}

// divideParts divides two BigComplex-compatible parts, preserving exactness when possible.
func divideParts(a, b Number) Number {
	switch va := a.(type) {
	case *BigInteger:
		switch vb := b.(type) {
		case *BigInteger:
			return va.Divide(vb) // Returns Rational
		case *Rational:
			// BigInteger / Rational = BigInteger * (1/Rational) = Rational
			ra := NewRationalFromBigInt(va.value, big.NewInt(1))
			return ra.Divide(vb)
		case *BigFloat:
			return toBigFloat(va).Divide(vb)
		}
	case *Rational:
		switch vb := b.(type) {
		case *Rational:
			return va.Divide(vb)
		case *BigInteger:
			return va.Divide(vb)
		case *BigFloat:
			return toBigFloat(va).Divide(vb)
		}
	case *BigFloat:
		return va.Divide(toBigFloat(b))
	}
	panic("divideParts: unexpected type")
}

// Add returns the sum of this BigComplex and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigComplex) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *BigComplex:
		newReal := addParts(p.real, v.real)
		newImag := addParts(p.imag, v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *BigInteger:
		newReal := addParts(p.real, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	case *BigFloat:
		newReal := addParts(p.real, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	case *Integer:
		bi := NewBigIntegerFromInt64(v.Value)
		return p.Add(bi)
	case *Float:
		bf := NewBigFloatFromFloat64(v.Value)
		return p.Add(bf)
	case *Rational:
		newReal := addParts(p.real, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	case *Complex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return p.Add(bc)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of this BigComplex and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigComplex) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigComplex:
		newReal := subtractParts(p.real, v.real)
		newImag := subtractParts(p.imag, v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *BigInteger:
		newReal := subtractParts(p.real, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	case *BigFloat:
		newReal := subtractParts(p.real, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	case *Integer:
		bi := NewBigIntegerFromInt64(v.Value)
		return p.Subtract(bi)
	case *Float:
		bf := NewBigFloatFromFloat64(v.Value)
		return p.Subtract(bf)
	case *Rational:
		newReal := subtractParts(p.real, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	case *Complex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return p.Subtract(bc)
	}
	panic(ErrNotANumber)
}

// Multiply returns the product of this BigComplex and another number.
// Complex multiplication: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2 Exactness: exact * exact = exact, exact * inexact = inexact.
func (p *BigComplex) Multiply(o Number) Number {
	if o.IsZero() {
		// Return zero with appropriate exactness
		if p.IsExact() {
			switch v := o.(type) {
			case *BigInteger, *Integer:
				return NewBigIntegerFromInt64(0)
			case *BigComplex:
				if v.IsExact() {
					return NewBigIntegerFromInt64(0)
				}
			}
		}
		return NewBigFloatFromFloat64(0)
	}
	if p.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigComplex:
		// (a+bi)(c+di) = (ac-bd) + (ad+bc)i
		ac := multiplyParts(p.real, v.real)
		bd := multiplyParts(p.imag, v.imag)
		ad := multiplyParts(p.real, v.imag)
		bc := multiplyParts(p.imag, v.real)
		newReal := subtractParts(ac, bd)
		newImag := addParts(ad, bc)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *BigInteger:
		newReal := multiplyParts(p.real, v)
		newImag := multiplyParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *BigFloat:
		newReal := multiplyParts(p.real, v)
		newImag := multiplyParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *Integer:
		bi := NewBigIntegerFromInt64(v.Value)
		return p.Multiply(bi)
	case *Float:
		bf := NewBigFloatFromFloat64(v.Value)
		return p.Multiply(bf)
	case *Rational:
		newReal := multiplyParts(p.real, v)
		newImag := multiplyParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *Complex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return p.Multiply(bc)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of this BigComplex and another number.
// Complex division: (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
//
// R7RS §6.2.6: The / procedure returns the quotient of its arguments.
// R7RS §6.2.2 Exactness: exact / exact = exact, exact / inexact = inexact.
func (p *BigComplex) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *BigComplex:
		// (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
		ac := multiplyParts(p.real, v.real)
		bd := multiplyParts(p.imag, v.imag)
		bc := multiplyParts(p.imag, v.real)
		ad := multiplyParts(p.real, v.imag)
		cc := multiplyParts(v.real, v.real)
		dd := multiplyParts(v.imag, v.imag)

		numerReal := addParts(ac, bd)
		numerImag := subtractParts(bc, ad)
		denom := addParts(cc, dd)

		// Division produces BigFloat results
		newReal := toBigFloat(numerReal).Divide(toBigFloat(denom))
		newImag := toBigFloat(numerImag).Divide(toBigFloat(denom))
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *BigInteger:
		// Use exact division to preserve exactness when possible
		newReal := divideParts(p.real, v)
		newImag := divideParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *BigFloat:
		newReal := toBigFloat(p.real).Divide(v)
		newImag := toBigFloat(p.imag).Divide(v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *Integer:
		bi := NewBigIntegerFromInt64(v.Value)
		return p.Divide(bi)
	case *Float:
		bf := NewBigFloatFromFloat64(v.Value)
		return p.Divide(bf)
	case *Rational:
		newReal := divideParts(p.real, v)
		newImag := divideParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	case *Complex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return p.Divide(bc)
	}
	panic(ErrNotANumber)
}

// Negate returns the negation of this BigComplex.
func (p *BigComplex) Negate() Number {
	return NewBigComplex(negatePart(p.real), negatePart(p.imag))
}

// negatePart negates a BigComplex part (BigInteger, Rational, or BigFloat).
func negatePart(n Number) Number {
	switch v := n.(type) {
	case *BigInteger:
		return v.Negate()
	case *BigFloat:
		return v.Negate()
	case *Rational:
		return v.Negate()
	}
	panic("negatePart: unexpected type")
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
	var otherReal Number
	switch v := o.(type) {
	case *BigComplex:
		otherReal = v.real
	case *BigInteger:
		otherReal = v
	case *BigFloat:
		otherReal = v
	case *Integer:
		otherReal = NewBigIntegerFromInt64(v.Value)
	case *Float:
		otherReal = NewBigFloatFromFloat64(v.Value)
	case *Rational:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(v.Rat())
		otherReal = &BigFloat{value: bf}
	case *Complex:
		otherReal = NewBigFloatFromFloat64(real(v.Value))
	default:
		panic(ErrNotANumber)
	}
	return toBigFloat(p.real).Compare(otherReal)
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

// toExactPart converts a BigComplex part to an exact type (BigInteger or Rational).
func toExactPart(n Number) Number {
	switch v := n.(type) {
	case *BigInteger:
		return v
	case *Rational:
		return v // Already exact
	case *BigFloat:
		// Truncate to integer
		i, _ := v.value.Int(nil)
		if i == nil {
			i = big.NewInt(0)
		}
		return &BigInteger{value: i}
	}
	panic("toExactPart: unexpected type")
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
	return NewBigComplex(p.real, negatePart(p.imag))
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
		isNeg = v.Rat().Sign() < 0
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
		if c, ok := o.(*Complex); ok {
			pReal := toBigFloat(p.real).Float64()
			pImag := toBigFloat(p.imag).Float64()
			return pReal == real(c.Value) && pImag == imag(c.Value)
		}
		return false
	}
	if v == nil || p == nil {
		return p == v
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

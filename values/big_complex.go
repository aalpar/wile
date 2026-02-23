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

// addParts adds two BigComplex-compatible parts.
// Inputs are promoted to BigComplex-compatible types (BigInteger, Rational,
// or BigFloat) to handle results from multiplyParts that may return Integer
// via the exact-zero multiplication optimization.
func addParts(a, b Number) Number {
	a = promoteToBigComplexPart(a)
	b = promoteToBigComplexPart(b)
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
	panic(ErrNotANumber)
}

// subtractParts subtracts two BigComplex-compatible parts.
// Inputs are promoted for the same reason as addParts.
func subtractParts(a, b Number) Number {
	a = promoteToBigComplexPart(a)
	b = promoteToBigComplexPart(b)
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
	panic(ErrNotANumber)
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
	panic(ErrNotANumber)
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
	panic(ErrNotANumber)
}

// Add returns the sum of this BigComplex and another number.
//
// Kind returns the numeric kind for dispatch table indexing.
func (p *BigComplex) Kind() NumericKind {
	return KindBigComplex
}

var bigComplexAdd [numKinds]func(*BigComplex, Number) Number
var bigComplexSubtract [numKinds]func(*BigComplex, Number) Number
var bigComplexCompare [numKinds]func(*BigComplex, Number) int
var bigComplexMultiply [numKinds]func(*BigComplex, Number) Number
var bigComplexDivide [numKinds]func(*BigComplex, Number) Number

func init() {
	bigComplexAdd[KindInteger] = func(p *BigComplex, o Number) Number {
		bi := NewBigIntegerFromInt64(o.(*Integer).Value)
		newReal := addParts(p.real, bi)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexAdd[KindBigInteger] = func(p *BigComplex, o Number) Number {
		newReal := addParts(p.real, o.(*BigInteger))
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexAdd[KindFloat] = func(p *BigComplex, o Number) Number {
		bf := NewBigFloatFromFloat64(o.(*Float).Value)
		newReal := addParts(p.real, bf)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexAdd[KindBigFloat] = func(p *BigComplex, o Number) Number {
		newReal := addParts(p.real, o.(*BigFloat))
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexAdd[KindRational] = func(p *BigComplex, o Number) Number {
		newReal := addParts(p.real, o.(*Rational))
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexAdd[KindComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*Complex)
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		newReal := addParts(p.real, bc.real)
		newImag := addParts(p.imag, bc.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexAdd[KindBigComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		newReal := addParts(p.real, v.real)
		newImag := addParts(p.imag, v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}

	bigComplexSubtract[KindInteger] = func(p *BigComplex, o Number) Number {
		bi := NewBigIntegerFromInt64(o.(*Integer).Value)
		newReal := subtractParts(p.real, bi)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexSubtract[KindBigInteger] = func(p *BigComplex, o Number) Number {
		newReal := subtractParts(p.real, o.(*BigInteger))
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexSubtract[KindFloat] = func(p *BigComplex, o Number) Number {
		bf := NewBigFloatFromFloat64(o.(*Float).Value)
		newReal := subtractParts(p.real, bf)
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexSubtract[KindBigFloat] = func(p *BigComplex, o Number) Number {
		newReal := subtractParts(p.real, o.(*BigFloat))
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexSubtract[KindRational] = func(p *BigComplex, o Number) Number {
		newReal := subtractParts(p.real, o.(*Rational))
		return maybeSimplify(promoteToBigComplexPart(newReal), p.imag)
	}
	bigComplexSubtract[KindComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*Complex)
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		newReal := subtractParts(p.real, bc.real)
		newImag := subtractParts(p.imag, bc.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexSubtract[KindBigComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		newReal := subtractParts(p.real, v.real)
		newImag := subtractParts(p.imag, v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}

	bigComplexCompare[KindInteger] = func(p *BigComplex, o Number) int {
		otherReal := NewBigIntegerFromInt64(o.(*Integer).Value)
		return toBigFloat(p.real).Compare(otherReal)
	}
	bigComplexCompare[KindBigInteger] = func(p *BigComplex, o Number) int {
		return toBigFloat(p.real).Compare(o.(*BigInteger))
	}
	bigComplexCompare[KindFloat] = func(p *BigComplex, o Number) int {
		otherReal := NewBigFloatFromFloat64(o.(*Float).Value)
		return toBigFloat(p.real).Compare(otherReal)
	}
	bigComplexCompare[KindBigFloat] = func(p *BigComplex, o Number) int {
		return toBigFloat(p.real).Compare(o.(*BigFloat))
	}
	bigComplexCompare[KindRational] = func(p *BigComplex, o Number) int {
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(o.(*Rational).Rat())
		otherReal := &BigFloat{value: bf}
		return toBigFloat(p.real).Compare(otherReal)
	}
	bigComplexCompare[KindComplex] = func(p *BigComplex, o Number) int {
		otherReal := NewBigFloatFromFloat64(real(o.(*Complex).Value))
		return toBigFloat(p.real).Compare(otherReal)
	}
	bigComplexCompare[KindBigComplex] = func(p *BigComplex, o Number) int {
		return toBigFloat(p.real).Compare(toBigFloat(o.(*BigComplex).real))
	}

	bigComplexMultiply[KindInteger] = func(p *BigComplex, o Number) Number {
		bi := NewBigIntegerFromInt64(o.(*Integer).Value)
		return p.Multiply(bi)
	}
	bigComplexMultiply[KindBigInteger] = func(p *BigComplex, o Number) Number {
		v := o.(*BigInteger)
		newReal := multiplyParts(p.real, v)
		newImag := multiplyParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexMultiply[KindFloat] = func(p *BigComplex, o Number) Number {
		bf := NewBigFloatFromFloat64(o.(*Float).Value)
		return p.Multiply(bf)
	}
	bigComplexMultiply[KindBigFloat] = func(p *BigComplex, o Number) Number {
		v := o.(*BigFloat)
		newReal := multiplyParts(p.real, v)
		newImag := multiplyParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexMultiply[KindRational] = func(p *BigComplex, o Number) Number {
		v := o.(*Rational)
		newReal := multiplyParts(p.real, v)
		newImag := multiplyParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexMultiply[KindComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*Complex)
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return p.Multiply(bc)
	}
	bigComplexMultiply[KindBigComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		ac := multiplyParts(p.real, v.real)
		bd := multiplyParts(p.imag, v.imag)
		ad := multiplyParts(p.real, v.imag)
		bc := multiplyParts(p.imag, v.real)
		newReal := subtractParts(ac, bd)
		newImag := addParts(ad, bc)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}

	bigComplexDivide[KindInteger] = func(p *BigComplex, o Number) Number {
		bi := NewBigIntegerFromInt64(o.(*Integer).Value)
		newReal := divideParts(p.real, bi)
		newImag := divideParts(p.imag, bi)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexDivide[KindBigInteger] = func(p *BigComplex, o Number) Number {
		v := o.(*BigInteger)
		newReal := divideParts(p.real, v)
		newImag := divideParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexDivide[KindFloat] = func(p *BigComplex, o Number) Number {
		bf := NewBigFloatFromFloat64(o.(*Float).Value)
		newReal := toBigFloat(p.real).Divide(bf)
		newImag := toBigFloat(p.imag).Divide(bf)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexDivide[KindBigFloat] = func(p *BigComplex, o Number) Number {
		v := o.(*BigFloat)
		newReal := toBigFloat(p.real).Divide(v)
		newImag := toBigFloat(p.imag).Divide(v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexDivide[KindRational] = func(p *BigComplex, o Number) Number {
		v := o.(*Rational)
		newReal := divideParts(p.real, v)
		newImag := divideParts(p.imag, v)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	bigComplexDivide[KindComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*Complex)
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return p.Divide(bc)
	}
	bigComplexDivide[KindBigComplex] = func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
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

		newReal := toBigFloat(numerReal).Divide(toBigFloat(denom))
		newImag := toBigFloat(numerImag).Divide(toBigFloat(denom))
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
}

// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
// Inexactness is contagious per R7RS §6.2.2.
func (p *BigComplex) Add(o Number) Number {
	v, ok := o.(*BigComplex)
	if ok {
		newReal := addParts(p.real, v.real)
		newImag := addParts(p.imag, v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	return bigComplexAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this BigComplex and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *BigComplex) Subtract(o Number) Number {
	v, ok := o.(*BigComplex)
	if ok {
		newReal := subtractParts(p.real, v.real)
		newImag := subtractParts(p.imag, v.imag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
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
	v, ok := o.(*BigComplex)
	if ok {
		// (a+bi)(c+di) = (ac-bd) + (ad+bc)i
		ac := multiplyParts(p.real, v.real)
		bd := multiplyParts(p.imag, v.imag)
		ad := multiplyParts(p.real, v.imag)
		bc := multiplyParts(p.imag, v.real)
		newReal := subtractParts(ac, bd)
		newImag := addParts(ad, bc)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	return bigComplexMultiply[o.Kind()](p, o)
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
	v, ok := o.(*BigComplex)
	if ok {
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

		newReal := toBigFloat(numerReal).Divide(toBigFloat(denom))
		newImag := toBigFloat(numerImag).Divide(toBigFloat(denom))
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	}
	return bigComplexDivide[o.Kind()](p, o)
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
	panic(ErrNotANumber)
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

// IsRational returns true if this BigComplex is a real number.
//
// R7RS §6.2.6: rational? returns #t for real BigComplex values since
// BigInteger, Rational, and BigFloat parts are all finite.
func (p *BigComplex) IsRational() bool {
	return p.IsReal()
}

// IsFinite returns true since BigComplex parts are always finite.
//
// R7RS §6.2.6: BigComplex uses BigInteger, Rational, or BigFloat parts,
// none of which support Inf or NaN.
func (p *BigComplex) IsFinite() bool {
	return true
}

// IsNaN returns false since BigComplex parts cannot represent NaN.
//
// R7RS §6.2.6: BigComplex uses BigInteger, Rational, or BigFloat parts,
// none of which support NaN.
func (p *BigComplex) IsNaN() bool {
	return false
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
//
// R7RS §6.2.6: exact converts to exact representation, preserving value.
// For BigFloat, converts via big.Rat (not truncation) to preserve fractional parts.
func toExactPart(n Number) Number {
	switch v := n.(type) {
	case *BigInteger, *Rational:
		return v
	case *BigFloat:
		// Convert via big.Rat to preserve fractional values
		// E.g., 1.5 -> 3/2 (not truncated to 1)
		f, _ := v.value.Float64()
		r := new(big.Rat).SetFloat64(f)
		if r == nil {
			// Non-finite value (should not happen for BigFloat)
			panic(ErrExactnessConversion)
		}
		if r.IsInt() {
			num := r.Num()
			return NewBigInteger(new(big.Int).Set(num))
		}
		return NewRationalFromRat(r)
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

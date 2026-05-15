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
	"math/cmplx"
	"strconv"
	"strings"

	"github.com/aalpar/wile/werr"
)

var (
	_ Value         = (*Complex)(nil)
	_ Number        = (*Complex)(nil)
	_ ComplexNumber = (*Complex)(nil)
	_ Hashable      = (*Complex)(nil)
)

// Complex represents a Scheme complex number.
type Complex struct {
	Value complex128
}

// NewComplex creates a new complex number from a complex128 value.
func NewComplex(v complex128) *Complex {
	return &Complex{Value: v}
}

// NewComplexFromParts creates a new complex number from real and imaginary parts.
func NewComplexFromParts(realPart, imagPart float64) *Complex {
	return &Complex{Value: complex(realPart, imagPart)}
}

// Datum returns the underlying complex128 value.
func (p *Complex) Datum() complex128 {
	return p.Value
}

// Real returns the real part of the complex number.
func (p *Complex) Real() float64 {
	return real(p.Value)
}

// Imag returns the imaginary part of the complex number.
func (p *Complex) Imag() float64 {
	return imag(p.Value)
}

// Kind returns the numeric kind for dispatch table indexing.
func (p *Complex) Kind() NumericKind {
	return KindComplex
}

// complexSimplifyDown is an identity: Complex's cross-kind reduction to a
// real Float (when imag == 0) lives in numeric_tower.go's Simplify(), so
// the per-kind step is a no-op.
func complexSimplifyDown(n Number) Number {
	return n
}

// complexToFloat64WithAccuracy returns the real component when the imaginary
// part is zero (ok=true, Exact since *Complex IS a complex128). Returns
// ok=false when the imaginary part is non-zero — callers must handle this.
func complexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
	v := n.(*Complex)
	return real(v.Value), big.Exact, imag(v.Value) == 0
}

// complexToComplex128WithAccuracy returns the underlying complex128 directly;
// both components are Exact since *Complex IS a complex128 (identity conversion).
func complexToComplex128WithAccuracy(n Number) Complex128Result {
	v := n.(*Complex)
	return Complex128Result{Value: v.Value, RealAcc: big.Exact, ImagAcc: big.Exact}
}

var complexAdd [numKinds]func(*Complex, Number) Number
var complexSubtract [numKinds]func(*Complex, Number) Number
var complexLessThan [numKinds]func(*Complex, Number) bool
var complexCompare [numKinds]func(*Complex, Number) int
var complexMultiply [numKinds]func(*Complex, Number) Number
var complexDivide [numKinds]func(*Complex, Number) (Number, error)

func init() {
	complexAdd = makeAddDispatch(KindComplex, func(p *Complex, o Number) Number {
		return NewComplex(p.Value + o.(*Complex).Value)
	})

	complexSubtract = makeSubtractDispatch(KindComplex, func(p *Complex, o Number) Number {
		return NewComplex(p.Value - o.(*Complex).Value)
	})

	complexLessThan = makeLessThanDispatch(KindComplex, func(p *Complex, o Number) bool {
		return real(p.Value) < real(o.(*Complex).Value)
	})

	complexCompare = makeCompareDispatch(KindComplex, func(p *Complex, o Number) int {
		r1, r2 := real(p.Value), real(o.(*Complex).Value)
		if r1 < r2 {
			return -1
		} else if r1 > r2 {
			return 1
		}
		return 0
	})

	complexMultiply = makeMultiplyDispatch(KindComplex, func(p *Complex, o Number) Number {
		return NewComplex(p.Value * o.(*Complex).Value)
	})

	complexDivide = makeDivideDispatch(KindComplex, func(p *Complex, o Number) (Number, error) {
		return NewComplex(p.Value / o.(*Complex).Value), nil
	})

	registerNumericSpec(KindComplex, NumericTypeSpec{
		schemeName:               "complex",
		simplifyDown:             complexSimplifyDown,
		toFloat64WithAccuracy:    complexToFloat64WithAccuracy,
		toComplex128WithAccuracy: complexToComplex128WithAccuracy,
		isAlwaysExact:            false,
	})
}

// Add returns the sum of this complex number and another number.
// Zero short-circuit: 0+x=x preserves exactness per R7RS §6.2.2.
func (p *Complex) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	v, ok := o.(*Complex)
	if ok {
		return NewComplex(p.Value + v.Value)
	}
	return complexAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this complex number and another number.
func (p *Complex) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	v, ok := o.(*Complex)
	if ok {
		return NewComplex(p.Value - v.Value)
	}
	return complexSubtract[o.Kind()](p, o)
}

// Multiply returns the product of this complex number and another number.
func (p *Complex) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	v, ok := o.(*Complex)
	if ok {
		return NewComplex(p.Value * v.Value)
	}
	return complexMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this complex number and another number.
func (p *Complex) Divide(o Number) (Number, error) {
	if o.IsZero() && o.IsExact() {
		return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "Complex.Divide: division by exact zero")
	}
	v, ok := o.(*Complex)
	if ok {
		return NewComplex(p.Value / v.Value), nil
	}
	return complexDivide[o.Kind()](p, o)
}

// IsZero returns true if this complex number is zero.
func (p *Complex) IsZero() bool {
	return p.Value == 0
}

// LessThan compares the real parts of the complex numbers.
func (p *Complex) LessThan(o Number) bool {
	v, ok := o.(*Complex)
	if ok {
		return real(p.Value) < real(v.Value)
	}
	return complexLessThan[o.Kind()](p, o)
}

// Negate returns the negation of this complex number.
//
// R7RS §6.2.6: The - procedure with one argument returns the additive inverse.
func (p *Complex) Negate() Number {
	return NewComplex(-p.Value)
}

// Abs returns the magnitude of this complex number.
//
// R7RS §6.2.6: For complex numbers, abs returns the magnitude.
func (p *Complex) Abs() Number {
	return NewFloat(cmplx.Abs(p.Value))
}

// ToExact converts this Complex to an exact representation.
//
// R7RS §6.2.6: exact returns an exact representation of its argument.
// Both real and imaginary parts are converted to exact numbers.
func (p *Complex) ToExact() (Number, error) {
	realPart, err := floatToExact(real(p.Value))
	if err != nil {
		return nil, err
	}
	imagPart, err := floatToExact(imag(p.Value))
	if err != nil {
		return nil, err
	}
	return NewBigComplex(realPart, imagPart), nil
}

// ToInexact returns this Complex unchanged since it is already inexact.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
func (p *Complex) ToInexact() Number {
	return p
}

// RealPart returns the real part of this complex number as a Number.
//
// R7RS §6.2.6: real-part returns the real part of a complex number.
func (p *Complex) RealPart() Number {
	return NewFloat(real(p.Value))
}

// ImagPart returns the imaginary part of this complex number as a Number.
//
// R7RS §6.2.6: imag-part returns the imaginary part of a complex number.
func (p *Complex) ImagPart() Number {
	return NewFloat(imag(p.Value))
}

// Compare compares this complex number with another number by real parts.
//
// R7RS §6.2.6: Complex comparison compares real parts only.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Complex) Compare(o Number) int {
	v, ok := o.(*Complex)
	if ok {
		r1, r2 := real(p.Value), real(v.Value)
		if r1 < r2 {
			return -1
		} else if r1 > r2 {
			return 1
		}
		return 0
	}
	return complexCompare[o.Kind()](p, o)
}

// IsExact returns false since Complex is always inexact.
//
// R7RS §6.2.2: Complex numbers with floating-point components are inexact.
func (p *Complex) IsExact() bool {
	return false
}

// IsInteger returns true if this complex has zero imaginary part and an integer real part.
//
// R7RS §6.2.6: integer? returns #t for complex numbers with zero imaginary
// part whose real part is an integer.
func (p *Complex) IsInteger() bool {
	return imag(p.Value) == 0 &&
		real(p.Value) == math.Trunc(real(p.Value)) &&
		!math.IsInf(real(p.Value), 0) &&
		!math.IsNaN(real(p.Value))
}

// IsRational returns true if the imaginary part is zero and the real part is finite.
//
// R7RS §6.2.6: A complex number with zero imaginary part is rational if its real
// part is a finite real number.
func (p *Complex) IsRational() bool {
	return imag(p.Value) == 0.0 &&
		!math.IsInf(real(p.Value), 0) &&
		!math.IsNaN(real(p.Value))
}

// IsFinite returns true if both real and imaginary parts are finite.
//
// R7RS §6.2.6: finite? returns #t if neither part is Inf or NaN.
func (p *Complex) IsFinite() bool {
	return !math.IsInf(real(p.Value), 0) && !math.IsNaN(real(p.Value)) &&
		!math.IsInf(imag(p.Value), 0) && !math.IsNaN(imag(p.Value))
}

// IsNaN returns true if either the real or imaginary part is NaN.
//
// R7RS §6.2.6: nan? returns #t if any component is NaN.
func (p *Complex) IsNaN() bool {
	return math.IsNaN(real(p.Value)) || math.IsNaN(imag(p.Value))
}

// IsReal returns true if the imaginary part is zero.
func (p *Complex) IsReal() bool {
	return imag(p.Value) == 0
}

// Magnitude returns the absolute value (modulus) of the complex number.
func (p *Complex) Magnitude() float64 {
	return cmplx.Abs(p.Value)
}

// Phase returns the phase (argument) of the complex number in radians.
func (p *Complex) Phase() float64 {
	return cmplx.Phase(p.Value)
}

// IsVoid returns true if this complex number is nil.
func (p *Complex) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both complex numbers have the same value.
func (p *Complex) EqualTo(v Value) bool {
	other, ok := v.(*Complex)
	if ok {
		return p.Value == other.Value
	}
	return false
}

// hashComplexComponent hashes a single float64 component of a complex number.
// For finite values it delegates to hashInexactNumeric via big.Float.
// For NaN or ±Inf it hashes the raw IEEE-754 bits to avoid big.Float panics.
func hashComplexComponent(f float64) uint64 {
	if math.IsNaN(f) || math.IsInf(f, 0) {
		return hashUint64(0x5, math.Float64bits(f))
	}
	return hashInexactNumeric(new(big.Float).SetFloat64(f))
}

// HashCode returns a hash of the complex value.
// Hashes real and imaginary parts independently via hashComplexComponent
// and combines them with a multiplicative mixing constant.
// NaN and ±Inf components use bitwise hashing to avoid big.Float panics.
func (p *Complex) HashCode() uint64 {
	r := hashComplexComponent(real(p.Value))
	i := hashComplexComponent(imag(p.Value))
	return r ^ (i * 0x9e3779b97f4a7c15)
}

// SchemeString returns the Scheme representation of this complex number.
// R7RS §6.2.6: Ensures decimal point for inexact values, lowercase inf/nan.
func (p *Complex) SchemeString() string {
	r := real(p.Value)
	i := imag(p.Value)
	realStr := formatComplexComponent(r)
	imagStr := formatComplexComponent(i)
	if len(imagStr) > 0 && imagStr[0] != '-' && imagStr[0] != '+' {
		return realStr + "+" + imagStr + "i"
	}
	return realStr + imagStr + "i"
}

// formatComplexComponent formats a float64 for use as a complex number component.
// Ensures R7RS-compliant output: decimal point for inexact values, lowercase inf/nan.
func formatComplexComponent(f float64) string {
	if math.IsInf(f, 1) {
		return "+inf.0"
	}
	if math.IsInf(f, -1) {
		return "-inf.0"
	}
	if math.IsNaN(f) {
		return "+nan.0"
	}
	s := strconv.FormatFloat(f, 'f', -1, 64)
	if !strings.ContainsRune(s, '.') {
		s += ".0"
	}
	return s
}

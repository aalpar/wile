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

	"github.com/aalpar/wile/pkg/werr"
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
//
// The magnitude-only zero test is DELIBERATE, and is the loss-signal question, not
// the exact-zero rule (exact_zero.go). It asks "did I drop information", not "is
// this number real": a *Complex is always inexact, so its zero imaginary part is
// always an INEXACT zero and (real? 5.0+0.0i) is #f — yet dropping a zero-magnitude
// component from a float64 conversion loses nothing. Contract: conversion.go §isReal.
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
		return cmpFloat64(real(p.Value), real(o.(*Complex).Value))
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

// realPartsOf returns the complex operand's parts and the real operand's float64
// value, plus whether o is a Float at all.
//
// realPartsOf reports whether o is a real operand that will be absorbed into
// complex128, and if so returns its float64 value.
//
// A REAL operand touches only the real part; it has NO imaginary component to
// contribute. Promotion pretends otherwise -- it manufactures an 0.0 imaginary part
// and lets IEEE act on it, which is how (+ 5.0-0.0i 2.0) loses the sign of its
// imaginary part (-0.0 + 0.0 is +0.0) and (* 5.0-0.0i 2.0) loses it too.
//
// The test is the promotion table, not a type: any real kind whose LUB with Complex
// IS Complex gets absorbed into complex128, which cannot represent an exact zero,
// so it must be handled here part-wise before promotion can invent the component.
// A real kind whose LUB is BigComplex is safe to promote and falls through — the
// promoted imaginary part is an EXACT zero there and the exact-zero rules preserve
// the sign for free.
//
// This used to test `o.(*Float)`, on the stated grounds that "every other real kind
// promotes Complex to BigComplex." That stopped being true when exactness contagion
// was fixed (promotion.go, Zone 3): Integer/BigInteger/Rational × Complex now lands
// at Complex, so an exact real reaching complex128 multiplication silently ate the
// sign of a signed-zero component. Keying off the table instead of a type list is
// what keeps the two facts from drifting apart again.
func realPartsOf(o Number) (float64, bool) {
	_, isComplex := o.(ComplexNumber)
	if isComplex {
		return 0, false
	}
	if promotionTable[KindComplex][o.Kind()] != KindComplex {
		return 0, false
	}
	return NumberToFloat64(o), true
}

// Add returns the sum of this complex number and another number.
// Zero short-circuit: 0+x=x preserves exactness per R7RS §6.2.2.
func (p *Complex) Add(o Number) Number {
	if isExactZero(o) {
		return p
	}
	// A real operand adds to the real part and leaves the imaginary part ALONE.
	r, isReal := realPartsOf(o)
	if isReal {
		return NewComplex(complex(real(p.Value)+r, imag(p.Value)))
	}
	v, ok := o.(*Complex)
	if ok {
		return NewComplex(p.Value + v.Value)
	}
	return complexAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this complex number and another number.
func (p *Complex) Subtract(o Number) Number {
	if isExactZero(o) {
		return p
	}
	// A real operand subtracts from the real part and leaves the imaginary part ALONE.
	r, isReal := realPartsOf(o)
	if isReal {
		return NewComplex(complex(real(p.Value)-r, imag(p.Value)))
	}
	v, ok := o.(*Complex)
	if ok {
		return NewComplex(p.Value - v.Value)
	}
	return complexSubtract[o.Kind()](p, o)
}

// Multiply returns the product of this complex number and another number.
func (p *Complex) Multiply(o Number) Number {
	if exactZeroEither(p, o) {
		return NewInteger(0)
	}
	// A real operand SCALES both parts. Going through complex multiplication instead
	// computes the imaginary part as a*d + b*c with a manufactured d = 0.0, and the
	// IEEE addition then swallows the sign: -0.0*2.0 + 0.0*5.0 is +0.0, not -0.0.
	r, isReal := realPartsOf(o)
	if isReal {
		return NewComplex(complex(real(p.Value)*r, imag(p.Value)*r))
	}
	v, ok := o.(*Complex)
	if ok {
		return NewComplex(p.Value * v.Value)
	}
	return complexMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this complex number and another number.
func (p *Complex) Divide(o Number) (Number, error) {
	// The exact-zero rule for division; exactZeroTable[zeroDiv] in exact_zero.go.
	switch exactZeroDivideAction(p, o) {
	case zeroRaise:
		return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "Complex.Divide: division by exact zero")
	case zeroYieldExactZero:
		return NewInteger(0), nil
	}
	// A REAL divisor divides each part directly; a COMPLEX zero divisor yields NaN.
	// The two are NOT interchangeable, and they become indistinguishable the moment
	// dispatch promotes the real into complex128 -- so the question must be asked
	// HERE, on the divisor's own kind, which is the last point that still sees it:
	//
	//	(/ 1.0+2.0i 0.0)       => +inf.0+inf.0i    (real zero divisor)
	//	(/ 1.0+2.0i 0.0+0.0i)  => +nan.0+nan.0i    (complex zero divisor)
	//
	// SCHEME DELIBERATELY DIVERGES FROM C99 HERE, and the divergence is the point.
	// C99 Annex G (G.5.1) mandates INFINITY for a zero divisor with a non-NaN
	// dividend, and Go implements exactly that -- runtime/complex.go says so, citing
	// G.5.1 by name. So Go's Inf is the C99-correct answer, and it is still the wrong
	// answer here: Chez and Racket both give NaN for a COMPLEX zero divisor, and this
	// is a Scheme. Follow the oracles, not C99.
	//
	// Stated explicitly because the next reader who "corrects" this toward C99 will
	// reintroduce the bug it fixes. This is the exact mirror of the bug
	// BigComplex.Divide had: the same erased distinction, the opposite symptom (that
	// one gave NaN for every zero divisor, this one gave Inf).
	//
	// Only a ZERO divisor needs intercepting. A non-zero real divisor is left to
	// normal dispatch, which promotes to the LUB and so preserves both the result
	// type and a BigFloat divisor's precision -- short-circuiting it here would
	// truncate the divisor to float64 and hand back a Complex where the promotion
	// lattice says BigComplex.
	//
	// Dividing the parts by the real scalar is what produces the signed infinity
	// naturally: 1/(-0.0) is -inf under IEEE, with no special case. An exact zero
	// divisor never reaches here -- it raised above.
	if o.IsZero() && !LookupNumericSpec(o.Kind()).IsAlwaysReal() {
		return NewComplex(complex(math.NaN(), math.NaN())), nil
	}
	// A REAL divisor divides both parts. Kept in complex128 only for a Float divisor,
	// whose LUB with Complex IS Complex; every other real kind promotes to BigComplex,
	// where the exact-zero imaginary part makes the general formula sign-correct on its
	// own. Short-circuiting those here would truncate a BigFloat divisor to float64 and
	// return a Complex where the promotion lattice says BigComplex.
	r, isReal := realPartsOf(o)
	if isReal {
		return NewComplex(complex(real(p.Value)/r, imag(p.Value)/r)), nil
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
//
// The result goes through maybeSimplify, because converting the parts to exact is
// precisely what can make the demotion rule apply: an inexact 0.0 imaginary part
// becomes an EXACT zero, and a number with an exact zero imaginary part IS real.
// (exact 5.0+0.0i) is 5, not 5+0i.
//
// This used to return NewBigComplex unconditionally, minting a 5+0i that reported
// real? #t and integer? #t yet was not eqv? to 5 -- while BigComplex.ToExact
// demoted correctly. Two ToExacts, one applying the rule and one not.
func (p *Complex) ToExact() (Number, error) {
	realPart, err := floatToExact(real(p.Value))
	if err != nil {
		return nil, err
	}
	imagPart, err := floatToExact(imag(p.Value))
	if err != nil {
		return nil, err
	}
	return maybeSimplify(promoteToBigComplexPart(realPart), promoteToBigComplexPart(imagPart)), nil
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
		return cmpFloat64(real(p.Value), real(v.Value))
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
// IsInteger reports whether this complex number is an integer.
//
// R7RS §6.2: the predicate hierarchy is integer? ⟹ rational? ⟹ real? ⟹
// complex?. A *Complex always has inexact (float64) components, so even a 0.0
// imaginary part is an *inexact* zero — the value is not real (see IsReal), and
// therefore not rational or integer. (integer? 5.0+0.0i) => #f (Chez/Racket
// agree). Always false; integer-valued reals are represented by *Float/*Integer.
func (*Complex) IsInteger() bool {
	return false
}

// IsRational reports whether this complex number is rational.
//
// R7RS §6.2: rational? ⟹ real?. A *Complex always has inexact components, so its
// zero imaginary part is an inexact zero and the value is not real, hence not
// rational. (rational? 5.0+0.0i) => #f. Always false.
func (*Complex) IsRational() bool {
	return false
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

// IsReal reports whether this complex number is real.
//
// R7RS §6.2: a complex with an *inexact* zero imaginary part is NOT real —
// (real? 5.0+0.0i) => #f, while (real? 5+0i) => #t. A *Complex always stores
// inexact (float64) components, so its imaginary part (even 0.0) is an inexact
// zero. Exact-zero-imaginary complexes are represented by *BigComplex, never
// *Complex, so this is always false. (Chez/Racket agree.)
func (*Complex) IsReal() bool {
	return false
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

// EqualTo implements R7RS equal? for Complex.
//
// R7RS §6.1: equal? "returns the same as eqv? when applied to … numbers" — no
// latitude. So this delegates to EqvNumber (eqv.go), the single authority on
// numeric equivalence, rather than restating the rules. Restating them is what
// let equal? and eqv? drift apart on signed zero and on cross-representation
// inexacts.
func (p *Complex) EqualTo(v Value) bool {
	return eqvNumberValue(p, v)
}

// hashComplexComponent hashes a single float64 component of a complex number.
// For finite values it delegates to hashInexactNumeric via big.Float.
// For NaN or ±Inf it hashes the raw IEEE-754 bits to avoid big.Float panics.
func hashComplexComponent(f float64) uint64 {
	// Canonical for NaN (every NaN is eqv?, so every NaN must hash alike);
	// bit-exact for Inf (+inf.0 and -inf.0 are not eqv?). See hashNaN.
	if math.IsNaN(f) {
		return hashNaN()
	}
	if math.IsInf(f, 0) {
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
	realStr := formatInexactReal(r)
	imagStr := formatInexactReal(i)
	if len(imagStr) > 0 && imagStr[0] != '-' && imagStr[0] != '+' {
		return realStr + "+" + imagStr + "i"
	}
	return realStr + imagStr + "i"
}

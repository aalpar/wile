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
	"math/big"

	"github.com/aalpar/wile/pkg/werr"
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
		panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "validateBigComplexPart: unsupported part type %T", n))
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

// toBigFloat converts a BigComplex part to a BigFloat.
//
// The three cases below are exactly the part types validateBigComplexPart admits,
// and every caller passes a part (p.real or p.imag), so the switch is total. It
// previously also carried *Integer and *Float arms for "intermediate arithmetic
// results" — the general divide path used to funnel its unpromoted numerators
// through here — but that call site now divides the parts directly, and those two
// arms became unreachable. They are dropped rather than left as unpinned dead code;
// the panic below still catches a part type that escapes validation.
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
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "toBigFloat: unsupported type %T", n))
}

// maybeSimplify demotes a complex to its real part when the imaginary part is an
// EXACT zero, and only then.
//
// R7RS §6.2.6 makes exactness, not magnitude, the deciding property:
//
//	(real? 3+0i)       =>  #t     ; exact zero imag: the number IS real
//	(real? -2.5+0.0i)  =>  #f     ; INEXACT zero imag: still complex
//
// An inexact 0.0 is an IEEE value that merely happens to compare equal to zero;
// it carries a sign and it is not a mathematical zero. Demoting on it would
// discard a component the caller can still observe, and would silently turn a
// complex into a real. An exact 0 is a mathematical zero, so a+0i IS a.
func maybeSimplify(rel, iam Number) Number {
	if isExactZero(iam) {
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
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "promoteToBigComplexPart: unsupported type %T", n))
}

// Kind returns the numeric kind for dispatch table indexing.
func (p *BigComplex) Kind() NumericKind {
	return KindBigComplex
}

// BigComplex has 5 dispatch tables (no bigComplexLessThan), each indexed by the
// second operand's kind. Complex ordering is undefined in R7RS §6.2.6 — LessThan
// delegates to Compare, which uses magnitude comparison as a total order for
// internal use (sorting, etc.) but is NOT mathematical ordering. Compare is
// initialized below like all other types.

// bigComplexAdd dispatches BigComplex addition on the second operand's kind.
var bigComplexAdd [numKinds]func(*BigComplex, Number) Number

// bigComplexSubtract dispatches BigComplex subtraction on the second operand's kind.
var bigComplexSubtract [numKinds]func(*BigComplex, Number) Number

// bigComplexCompare dispatches BigComplex magnitude comparison on the second operand's kind.
var bigComplexCompare [numKinds]func(*BigComplex, Number) int

// bigComplexMultiply dispatches BigComplex multiplication on the second operand's kind.
var bigComplexMultiply [numKinds]func(*BigComplex, Number) Number

// bigComplexDivide dispatches BigComplex division on the second operand's kind.
var bigComplexDivide [numKinds]func(*BigComplex, Number) (Number, error)

// bigComplexSimplifyDown is an identity: BigComplex's cross-kind reduction
// to its real part (when imag.IsZero()) lives in numeric_tower.go's
// Simplify(), so the per-kind step is a no-op.
func bigComplexSimplifyDown(n Number) Number {
	return n
}

// bigComplexToFloat64WithAccuracy converts a BigComplex to float64 with a
// per-component loss signal. The accuracy is the BigFloat→float64 rounding
// direction of the real component only; the imaginary component's drop is
// signaled by the isReal bool, which is false iff the imaginary part is
// non-zero. Callers needing both components should use ToComplex128WithAccuracy.
// The bare IsZero() below is DELIBERATE, and is the one place in this package where
// a zero test must NOT consult exactness. It asks "did I drop information", not "is
// this number real" (isExactZero / IsReal). An inexact 0.0 imaginary part is not a
// mathematical zero -- (real? 5.0+0.0i) is #f -- but dropping it from a float64
// conversion loses nothing, because its magnitude is zero. Contract: conversion.go
// §isReal, "false iff n had a NON-ZERO imaginary part".
//
// Routing this through isExactZero would report loss where none occurred.
func bigComplexToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
	p := n.(*BigComplex)
	realF, realAcc := toBigFloat(p.real).Float64WithAccuracy()
	return realF, realAcc, p.imag.IsZero()
}

// bigComplexToComplex128WithAccuracy converts a BigComplex to Complex128Result
// with per-component accuracy. Named fields prevent realAcc/imagAcc swaps.
func bigComplexToComplex128WithAccuracy(n Number) Complex128Result {
	p := n.(*BigComplex)
	realF, realAcc := toBigFloat(p.real).Float64WithAccuracy()
	imagF, imagAcc := toBigFloat(p.imag).Float64WithAccuracy()
	return Complex128Result{Value: complex(realF, imagF), RealAcc: realAcc, ImagAcc: imagAcc}
}

func init() {
	bigComplexAdd = makeAddDispatch(KindBigComplex, func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		newReal := p.real.Add(v.real)
		newImag := p.imag.Add(v.imag)
		// A promoted real arrives with an EXACT zero imaginary part (promotion.go), so
		// the additive identity hands p.imag back UNTOUCHED and a signed zero survives.
		// contagionOverParts then stops that exact zero from leaking into the result.
		newReal, newImag = contagionOverParts(p, v, newReal, newImag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	})

	bigComplexSubtract = makeSubtractDispatch(KindBigComplex, func(p *BigComplex, o Number) Number {
		v := o.(*BigComplex)
		newReal := p.real.Subtract(v.real)
		newImag := p.imag.Subtract(v.imag)
		newReal, newImag = contagionOverParts(p, v, newReal, newImag)
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
		// A promoted real has an EXACT zero imaginary part, so the annihilation rule
		// kills the cross terms (ad and bd become exact 0) and the surviving product
		// keeps its sign: -0.0 * 2.0 stays -0.0 instead of being swallowed by an IEEE
		// addition of +0.0.
		newReal, newImag = contagionOverParts(p, v, newReal, newImag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag))
	})

	bigComplexDivide = makeDivideDispatch(KindBigComplex, func(p *BigComplex, o Number) (Number, error) {
		v := o.(*BigComplex)
		// Scalar divisor (d=0): divide each part directly to preserve exactness.
		// Scalars promoted from Integer/BigInteger/Rational arrive with BigInteger(0) imag.
		//
		// The zero must be EXACT. An inexact 0.0 imaginary part is an IEEE value,
		// not an absent component, and the shortcut is not equivalent to the general
		// formula on it: the general form computes the imaginary numerator as
		// (b*c - a*d), which for an exact b=0 and an inexact d=0.0 is 0 - 0.0, i.e.
		// -0.0 by the exact-zero negation identity. The shortcut computes b/c = +0.0
		// and loses the sign. Chez and Racket both give (/ 10 2.0+0.0i) => 5.0-0.0i.
		if isExactZero(v.imag) {
			newReal, err := p.real.Divide(v.real)
			if err != nil {
				return nil, err
			}
			newImag, err := p.imag.Divide(v.real)
			if err != nil {
				return nil, err
			}
			newReal, newImag = contagionOverParts(p, v, newReal, newImag)
			return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag)), nil
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

		// Divide the parts directly rather than coercing them to BigFloat first.
		// Exactness contagion already gives the right answer for both cases: exact
		// operands divide exactly (Integer/Integer yields a Rational, so 11/5 stays
		// 11/5 instead of collapsing to 2.2), and any inexact operand makes the
		// quotient inexact on its own. Coercing to BigFloat up front would destroy
		// exactness unconditionally, which R7RS §6.2.2 does not license: Chez and
		// Racket both give (/ 3+4i 1+2i) => 11/5-2/5i, exact.
		//
		// This is also what lets maybeSimplify demote correctly below. A quotient
		// with a mathematically-zero imaginary part reaches it as an EXACT zero, so
		// (/ 2+2i 1+1i) => 2 rather than being stranded as a complex 2.0+0.0i.
		//
		// denom CAN be zero here, but only an inexact one: the exact-zero-imag case
		// returned above, so a zero denominator implies the divisor was an inexact
		// zero (0.0+0.0i). Dividing by it yields NaN/infinity under IEEE rather than
		// raising, which is the answer both oracles give: (/ 1+1i 0.0+0.0i) is
		// +nan.0+nan.0i. Division by an EXACT zero still raises, on the path above.
		newReal, err := numerReal.Divide(denom)
		if err != nil {
			return nil, err
		}
		newImag, err := numerImag.Divide(denom)
		if err != nil {
			return nil, err
		}
		// The GENERAL branch needs contagion re-imposed too, and it did not get it --
		// which shipped a leak. A promoted real dividend carries an EXACT zero imaginary
		// part, and BOTH terms of numerReal = a*c + b*d can annihilate to an exact zero
		// when the divisor is exact and pure-imaginary: b because promotion mints it
		// exact, c because it IS zero. The quotient then holds an exact 0 real part
		// inside an inexact number. (/ 2.0 0+1i) came back as 0-2.0i, with
		// (exact? (real-part ...)) => #t; both oracles give 0.0-2.0i.
		newReal, newImag = contagionOverParts(p, v, newReal, newImag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag)), nil
	})

	registerNumericSpec(KindBigComplex, NumericTypeSpec{
		schemeName:               "complex",
		simplifyDown:             bigComplexSimplifyDown,
		toFloat64WithAccuracy:    bigComplexToFloat64WithAccuracy,
		toComplex128WithAccuracy: bigComplexToComplex128WithAccuracy,
		isAlwaysExact:            false,
	})
}

// Add returns the sum of this BigComplex and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
func (p *BigComplex) Add(o Number) Number {
	if isExactZero(o) {
		return p
	}
	if isExactZero(p) {
		return o
	}
	return bigComplexAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this BigComplex and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *BigComplex) Subtract(o Number) Number {
	if isExactZero(o) {
		return p
	}
	if isExactZero(p) {
		return o.Negate()
	}
	return bigComplexSubtract[o.Kind()](p, o)
}

// Multiply returns the product of this BigComplex and another number.
// Complex multiplication: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2 Exactness: exact * exact = exact, exact * inexact = inexact.
func (p *BigComplex) Multiply(o Number) Number {
	if exactZeroEither(p, o) {
		return NewInteger(0)
	}
	return bigComplexMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this BigComplex and another number.
// Complex division: (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
//
// R7RS §6.2.6: The / procedure returns the quotient of its arguments.
// R7RS §6.2.2 Exactness: exact / exact = exact, exact / inexact = inexact.
func (p *BigComplex) Divide(o Number) (Number, error) {
	// The exact-zero rule for division; exactZeroTable[zeroDiv] in exact_zero.go.
	switch exactZeroDivideAction(p, o) {
	case zeroRaise:
		return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "BigComplex.Divide: division by exact zero")
	case zeroYieldExactZero:
		return NewInteger(0), nil
	}
	// A REAL divisor divides part-wise. This MUST be decided here, on o's own
	// kind, because it is the last point that still sees the operand unpromoted.
	//
	// Dispatch promotes both operands to the LUB first, and lifting a real into
	// the complex LUB manufactures a zero imaginary part: a Float 0.0 arrives at
	// the closure as BigComplex{BigFloat(0.0), BigFloat(0.0)}, byte-identical to a
	// user-written 0.0+0.0i. The two are NOT interchangeable — the oracles give
	// (/ 1+2i 0.0) => +inf.0+inf.0i but (/ 1+2i 0.0+0.0i) => +nan.0+nan.0i,
	// because the general formula's denominator (c²+d²) is 0.0 for the complex
	// zero and drives 0/0 => NaN, while a real divisor divides each part by c and
	// yields a signed infinity. Asking about the imaginary part's exactness AFTER
	// promotion cannot recover the distinction: the promoted zero is inexact
	// exactly when the real divisor was.
	if LookupNumericSpec(o.Kind()).IsAlwaysReal() {
		newReal, err := p.real.Divide(o)
		if err != nil {
			return nil, err
		}
		newImag, err := p.imag.Divide(o)
		if err != nil {
			return nil, err
		}
		newReal, newImag = contagionOverParts(p, o, newReal, newImag)
		return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag)), nil
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

// IsReal reports whether this complex number is real.
//
// R7RS §6.2: a complex is real iff its imaginary part is an *exact* zero —
// (real? 5+0i) => #t but (real? 5.0+0.0i) => #f. An inexact zero imaginary
// (a BigFloat 0.0) does not collapse to real. IsInteger/IsRational delegate
// here, so the whole integer? ⟹ rational? ⟹ real? hierarchy stays consistent.
func (p *BigComplex) IsReal() bool {
	return isExactZero(p.imag)
}

// IsExact returns true if both parts are exact.
//
// R7RS §6.2.2: A complex number is exact if both real and imaginary parts are exact.
//
// This used to ask via isExactPart, a type switch on BigInteger|Rational that
// shadowed the parts' own IsExact(). The two agreed for every part type
// validateBigComplexPart admits, so it was not a bug -- but it was a second spelling
// of "is this exact", and it would have diverged silently the moment a part type was
// added. Ask the value, not its type.
func (p *BigComplex) IsExact() bool {
	return p.real.IsExact() && p.imag.IsExact()
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

// ToExact converts this BigComplex to an exact representation.
//
// R7RS §6.2.6: exact returns an exact representation of its argument.
// If already exact, returns itself. Otherwise converts BigFloat parts to BigInteger
// by truncating (may lose precision).
func (p *BigComplex) ToExact() (Number, error) {
	if p.IsExact() {
		return p, nil
	}
	realExact, err := toExactPart(p.real)
	if err != nil {
		return nil, err
	}
	imagExact, err := toExactPart(p.imag)
	if err != nil {
		return nil, err
	}
	if imagExact.IsZero() {
		return realExact, nil
	}
	return NewBigComplex(realExact, imagExact), nil
}

// toExactPart converts a BigComplex part to a BigComplex-compatible exact type
// (*BigInteger or *Rational).
//
// R7RS §6.2.6: exact converts to exact representation, preserving value.
// For BigFloat, converts via ToExact() then simplifies integer-valued rationals
// (3/1 → 3). Simplify may return *Integer; we promote it back to *BigInteger
// since NewBigComplex only accepts *BigInteger, *Rational, or *BigFloat.
func toExactPart(n Number) (Number, error) {
	switch v := n.(type) {
	case *BigInteger, *Rational:
		return v, nil
	case *BigFloat:
		exact, err := v.ToExact()
		if err != nil {
			return nil, err
		}
		q := Simplify(exact)
		i, ok := q.(*Integer)
		if ok {
			return NewBigIntegerFromInt64(i.Value), nil
		}
		return q, nil
	}
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "toExactPart: unsupported type %T", n))
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
// Uses atan2(imag, real) at big.Float precision, so components beyond the
// float64 range (~1.8e308) with a finite ratio keep their true angle instead of
// both saturating to +Inf and collapsing to atan2(+Inf,+Inf)=π/4.
func (p *BigComplex) Phase() *BigFloat {
	r := toBigFloat(p.real).value
	i := toBigFloat(p.imag).value
	return &BigFloat{value: BigAtan2(i, r, DefaultBigFloatPrecision)}
}

// Sqrt returns the principal square root at big.Float precision, honoring the
// R7RS §6.2.6 branch cut along the negative real axis (continuous with
// quadrant II: the negative real axis maps to the positive imaginary axis).
// It uses the numerically stable formulation that derives the smaller component
// from the larger via division, avoiding catastrophic cancellation. Computing
// with big.Float instead of truncating to complex128 keeps components beyond
// the float64 range (~1.8e308) from overflowing the result to +inf.
func (p *BigComplex) Sqrt() *BigComplex {
	prec := uint(DefaultBigFloatPrecision)
	a := new(big.Float).SetPrec(prec).Set(toBigFloat(p.real).value)
	b := new(big.Float).SetPrec(prec).Set(toBigFloat(p.imag).value)
	two := new(big.Float).SetPrec(prec).SetInt64(2)

	// Zero imaginary part: the result stays on an axis. The negative real axis
	// maps to the positive imaginary axis per the R7RS branch cut.
	if b.Sign() == 0 {
		if a.Sign() >= 0 {
			re := new(big.Float).SetPrec(prec).Sqrt(a)
			return NewBigComplexFromBigFloats(&BigFloat{value: re}, NewBigFloatFromFloat64(0))
		}
		negA := new(big.Float).SetPrec(prec).Neg(a)
		im := new(big.Float).SetPrec(prec).Sqrt(negA)
		return NewBigComplexFromBigFloats(NewBigFloatFromFloat64(0), &BigFloat{value: im})
	}

	r := new(big.Float).SetPrec(prec).Set(p.Magnitude().value)

	if a.Sign() >= 0 {
		// re = sqrt((|z| + a)/2); im = b / (2·re).
		sum := new(big.Float).SetPrec(prec).Add(r, a)
		sum.Quo(sum, two)
		re := new(big.Float).SetPrec(prec).Sqrt(sum)
		twoRe := new(big.Float).SetPrec(prec).Mul(two, re)
		im := new(big.Float).SetPrec(prec).Quo(b, twoRe)
		return NewBigComplexFromBigFloats(&BigFloat{value: re}, &BigFloat{value: im})
	}
	// a < 0: im = sign(b)·sqrt((|z| − a)/2); re = b / (2·im).
	diff := new(big.Float).SetPrec(prec).Sub(r, a)
	diff.Quo(diff, two)
	im := new(big.Float).SetPrec(prec).Sqrt(diff)
	if b.Sign() < 0 {
		im.Neg(im)
	}
	twoIm := new(big.Float).SetPrec(prec).Mul(two, im)
	re := new(big.Float).SetPrec(prec).Quo(b, twoIm)
	return NewBigComplexFromBigFloats(&BigFloat{value: re}, &BigFloat{value: im})
}

// Conjugate returns the complex conjugate (a-bi for a+bi).
func (p *BigComplex) Conjugate() *BigComplex {
	return NewBigComplex(p.real, p.imag.Negate())
}

// SchemeString returns the Scheme representation of this BigComplex.
func (p *BigComplex) SchemeString() string {
	realStr := p.real.SchemeString()
	imagStr := p.imag.SchemeString()
	// The separator is driven by the RENDERED sign, not by IsNegative(), matching
	// (*Complex).SchemeString. Three parts are not negative yet still render with a
	// leading sign, and IsNegative() misses every one: -0.0 (a negative zero is not
	// less than zero) printed "5.0+-0.0i", while +inf.0 and +nan.0 carry their own
	// "+" and printed "5.0++inf.0i". A part whose text already opens with a sign
	// supplies the separator itself.
	if len(imagStr) > 0 && imagStr[0] != '-' && imagStr[0] != '+' {
		return realStr + "+" + imagStr + "i"
	}
	return realStr + imagStr + "i"
}

// IsVoid returns true if this BigComplex is nil.
func (p *BigComplex) IsVoid() bool {
	return p == nil
}

// EqualTo implements R7RS equal? for BigComplex.
//
// R7RS §6.1: equal? "returns the same as eqv? when applied to … numbers" — no
// latitude. So this delegates to EqvNumber (eqv.go), the single authority on
// numeric equivalence, rather than restating the rules. Restating them is what
// let equal? and eqv? drift apart on signed zero and on cross-representation
// inexacts.
func (p *BigComplex) EqualTo(v Value) bool {
	return eqvNumberValue(p, v)
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

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
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "toBigFloat: unsupported type %T", n))
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
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "promoteToBigComplexPart: unsupported type %T", n))
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

	bigComplexDivide = makeDivideDispatch(KindBigComplex, func(p *BigComplex, o Number) (Number, error) {
		v := o.(*BigComplex)
		// Scalar divisor (d=0): divide each part directly to preserve exactness.
		// Scalars promoted from Integer/BigInteger/Rational arrive with BigInteger(0) imag.
		if v.imag.IsZero() {
			newReal, err := p.real.Divide(v.real)
			if err != nil {
				return nil, err
			}
			newImag, err := p.imag.Divide(v.real)
			if err != nil {
				return nil, err
			}
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

		newReal, err := toBigFloat(numerReal).Divide(toBigFloat(denom))
		if err != nil {
			return nil, err
		}
		newImag, err := toBigFloat(numerImag).Divide(toBigFloat(denom))
		if err != nil {
			return nil, err
		}
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
func (p *BigComplex) Divide(o Number) (Number, error) {
	if o.IsZero() && o.IsExact() {
		return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "BigComplex.Divide: division by exact zero")
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
	return p.imag.IsZero() && isExactPart(p.imag)
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
			// Promote the Complex's float64 components to big precision rather
			// than truncating this BigComplex, so two values differing only
			// below float64 precision compare unequal.
			cReal := real(c.Value)
			cImag := imag(c.Value)
			// NaN never equals anything; guard here because
			// NewBigFloatFromFloat64 folds NaN into a NaN BigFloat that would
			// otherwise compare via Compare's nan==0 path.
			if math.IsNaN(cReal) || math.IsNaN(cImag) || p.IsNaN() {
				return false
			}
			// big.Float represents ±Inf and Compare (via big.Float.Cmp) orders
			// it correctly, so a finite component never matches an infinite one.
			return toBigFloat(p.real).Compare(NewBigFloatFromFloat64(cReal)) == 0 &&
				toBigFloat(p.imag).Compare(NewBigFloatFromFloat64(cImag)) == 0
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

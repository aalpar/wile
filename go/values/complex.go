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
	"math"
	"math/cmplx"
	"strconv"
	"strings"
)

var (
	_ Value  = (*Complex)(nil)
	_ Number = (*Complex)(nil)
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

// addSame adds two Complex numbers of the same type.
func (p *Complex) addSame(o *Complex) Number {
	return NewComplex(p.Value + o.Value)
}

// subtractSame subtracts two Complex numbers of the same type.
func (p *Complex) subtractSame(o *Complex) Number {
	return NewComplex(p.Value - o.Value)
}

// multiplySame multiplies two Complex numbers of the same type.
func (p *Complex) multiplySame(o *Complex) Number {
	return NewComplex(p.Value * o.Value)
}

// divideSame divides two Complex numbers of the same type.
func (p *Complex) divideSame(o *Complex) Number {
	if o.Value == 0 {
		panic(ErrDivisionByZero)
	}
	return NewComplex(p.Value / o.Value)
}

// compareSame compares two Complex numbers of the same type by real parts.
func (p *Complex) compareSame(o *Complex) int {
	r1, r2 := real(p.Value), real(o.Value)
	if r1 < r2 {
		return -1
	} else if r1 > r2 {
		return 1
	}
	return 0
}

// Add returns the sum of this complex number and another number.
func (p *Complex) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value + v.Value)
	case *Float:
		return NewComplex(p.Value + complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value + complex(float64(v.Value), 0))
	case *BigInteger:
		return NewComplex(p.Value + complex(float64FromBigInt(v.value), 0))
	case *BigFloat:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Add(v)
	case *Rational:
		return NewComplex(p.Value + complex(v.Float64(), 0))
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Add(v)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of this complex number and another number.
func (p *Complex) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value - v.Value)
	case *Float:
		return NewComplex(p.Value - complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value - complex(float64(v.Value), 0))
	case *BigInteger:
		return NewComplex(p.Value - complex(float64FromBigInt(v.value), 0))
	case *BigFloat:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Subtract(v)
	case *Rational:
		return NewComplex(p.Value - complex(v.Float64(), 0))
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Subtract(v)
	}
	panic(ErrNotANumber)
}

// Multiply returns the product of this complex number and another number.
func (p *Complex) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value * v.Value)
	case *Float:
		return NewComplex(p.Value * complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value * complex(float64(v.Value), 0))
	case *BigInteger:
		return NewComplex(p.Value * complex(float64FromBigInt(v.value), 0))
	case *BigFloat:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Multiply(v)
	case *Rational:
		return NewComplex(p.Value * complex(v.Float64(), 0))
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Multiply(v)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of this complex number and another number.
func (p *Complex) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Complex:
		return NewComplex(p.Value / v.Value)
	case *Float:
		return NewComplex(p.Value / complex(v.Value, 0))
	case *Integer:
		return NewComplex(p.Value / complex(float64(v.Value), 0))
	case *BigInteger:
		return NewComplex(p.Value / complex(float64FromBigInt(v.value), 0))
	case *BigFloat:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Divide(v)
	case *Rational:
		return NewComplex(p.Value / complex(v.Float64(), 0))
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
		return bc.Divide(v)
	}
	panic(ErrNotANumber)
}

// IsZero returns true if this complex number is zero.
func (p *Complex) IsZero() bool {
	return p.Value == 0
}

// LessThan compares the real parts of the complex numbers.
func (p *Complex) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Complex:
		return real(p.Value) < real(v.Value)
	case *Float:
		return real(p.Value) < v.Value
	case *Integer:
		return real(p.Value) < float64(v.Value)
	case *BigInteger:
		return real(p.Value) < float64FromBigInt(v.value)
	case *BigFloat:
		self := NewBigFloatFromFloat64(real(p.Value))
		return self.Compare(v) < 0
	case *Rational:
		return real(p.Value) < v.Float64()
	case *BigComplex:
		return NewBigFloatFromFloat64(real(p.Value)).Compare(v.Real()) < 0
	}
	panic(ErrNotANumber)
}

// Negate returns the negation of this complex number.
//
// R7RS §6.2.6: The - procedure with one argument returns the additive inverse.
func (p *Complex) Negate() Number {
	return NewComplex(-p.Value)
}

// Compare compares this complex number with another number by real parts.
//
// R7RS §6.2.6: Complex comparison compares real parts only.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Complex) Compare(o Number) int {
	switch v := o.(type) {
	case *Complex:
		r1, r2 := real(p.Value), real(v.Value)
		if r1 < r2 {
			return -1
		} else if r1 > r2 {
			return 1
		}
		return 0
	case *Float:
		r := real(p.Value)
		if r < v.Value {
			return -1
		} else if r > v.Value {
			return 1
		}
		return 0
	case *Integer:
		r := real(p.Value)
		vf := float64(v.Value)
		if r < vf {
			return -1
		} else if r > vf {
			return 1
		}
		return 0
	case *BigInteger:
		r := real(p.Value)
		vf := float64FromBigInt(v.value)
		if r < vf {
			return -1
		} else if r > vf {
			return 1
		}
		return 0
	case *BigFloat:
		self := NewBigFloatFromFloat64(real(p.Value))
		return self.Compare(v)
	case *Rational:
		r := real(p.Value)
		vf := v.Float64()
		if r < vf {
			return -1
		} else if r > vf {
			return 1
		}
		return 0
	case *BigComplex:
		return NewBigFloatFromFloat64(real(p.Value)).Compare(v.Real())
	}
	panic(ErrNotANumber)
}

// IsExact returns false since Complex is always inexact.
//
// R7RS §6.2.2: Complex numbers with floating-point components are inexact.
func (p *Complex) IsExact() bool {
	return false
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

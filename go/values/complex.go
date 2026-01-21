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
	"fmt"
	"math/cmplx"
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
	case *Rational:
		return real(p.Value) < v.Float64()
	case *BigComplex:
		return NewBigFloatFromFloat64(real(p.Value)).Compare(v.Real()) < 0
	}
	panic(ErrNotANumber)
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
func (p *Complex) SchemeString() string {
	r := real(p.Value)
	i := imag(p.Value)
	if i >= 0 {
		return fmt.Sprintf("%g+%gi", r, i)
	}
	return fmt.Sprintf("%g%gi", r, i)
}

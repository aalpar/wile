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
	"math/big"
	"strconv"
)

var (
	_ Value  = (*Float)(nil)
	_ Number = (*Float)(nil)
	// _ Comparable = (*Float)(nil)
)

// Float represents a Scheme floating-point number.
type Float struct {
	Value float64
}

// NewFloat creates a new float value.
func NewFloat(v float64) *Float {
	q := &Float{Value: v}
	return q
}

// Datum returns the underlying float64 value.
func (p *Float) Datum() float64 {
	return p.Value
}

// addSame adds two Floats of the same type.
func (p *Float) addSame(o *Float) Number {
	return NewFloat(p.Value + o.Value)
}

// subtractSame subtracts two Floats of the same type.
func (p *Float) subtractSame(o *Float) Number {
	return NewFloat(p.Value - o.Value)
}

// multiplySame multiplies two Floats of the same type.
func (p *Float) multiplySame(o *Float) Number {
	return NewFloat(p.Value * o.Value)
}

// divideSame divides two Floats of the same type.
func (p *Float) divideSame(o *Float) Number {
	if o.Value == 0 {
		panic(ErrDivisionByZero)
	}
	return NewFloat(p.Value / o.Value)
}

// compareSame compares two Floats of the same type.
func (p *Float) compareSame(o *Float) int {
	if p.Value < o.Value {
		return -1
	} else if p.Value > o.Value {
		return 1
	}
	return 0
}

// Add returns the sum of two numbers.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Float) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value + float64(v.Value))
	case *BigInteger:
		return NewFloat(p.Value + float64FromBigInt(v.value))
	case *Float:
		return NewFloat(p.Value + v.Value)
	case *BigFloat:
		self := new(big.Float).SetFloat64(p.Value)
		return &BigFloat{value: new(big.Float).Add(self, v.value)}
	case *Rational:
		return NewFloat(p.Value + v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) + v.Value)
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(p.Value), NewBigFloatFromFloat64(0))
		return bc.Add(v)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of two numbers.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Float) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value - float64(v.Value))
	case *BigInteger:
		return NewFloat(p.Value - float64FromBigInt(v.value))
	case *Float:
		return NewFloat(p.Value - v.Value)
	case *BigFloat:
		self := new(big.Float).SetFloat64(p.Value)
		return &BigFloat{value: new(big.Float).Sub(self, v.value)}
	case *Rational:
		return NewFloat(p.Value - v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) - v.Value)
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(p.Value), NewBigFloatFromFloat64(0))
		return bc.Subtract(v)
	}
	panic(ErrNotANumber)
}

// Multiply returns the product of two numbers.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2: Exact zero dominates—(* 0 x) may return exact 0 even when
// x is inexact. Zero is an exact value when the result is mathematically
// unambiguous. This implementation follows Chez Scheme's behavior.
func (p *Float) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value * float64(v.Value))
	case *BigInteger:
		return NewFloat(p.Value * float64FromBigInt(v.value))
	case *BigFloat:
		self := new(big.Float).SetFloat64(p.Value)
		return &BigFloat{value: new(big.Float).Mul(self, v.value)}
	case *Float:
		return NewFloat(p.Value * v.Value)
	case *Rational:
		return NewFloat(p.Value * v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) * v.Value)
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(p.Value), NewBigFloatFromFloat64(0))
		return bc.Multiply(v)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of this float and another number.
func (p *Float) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value / float64(v.Value))
	case *BigInteger:
		return NewFloat(p.Value / float64FromBigInt(v.value))
	case *BigFloat:
		self := new(big.Float).SetFloat64(p.Value)
		return &BigFloat{value: new(big.Float).Quo(self, v.value)}
	case *Float:
		return NewFloat(p.Value / v.Value)
	case *Rational:
		return NewFloat(p.Value / v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) / v.Value)
	case *BigComplex:
		bc := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(p.Value), NewBigFloatFromFloat64(0))
		return bc.Divide(v)
	}
	panic(ErrNotANumber)
}

// IsZero returns true if this float is zero.
func (p *Float) IsZero() bool {
	return p.Value == 0.0
}

// LessThan returns true if this float is less than another number.
func (p *Float) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Integer:
		return p.Value < float64(v.Value)
	case *BigInteger:
		self := new(big.Float).SetFloat64(p.Value)
		other := new(big.Float).SetInt(v.BigInt())
		return self.Cmp(other) < 0
	case *Float:
		return p.Value < v.Value
	case *BigFloat:
		self := new(big.Float).SetFloat64(p.Value)
		return self.Cmp(v.BigFloatValue()) < 0
	case *Rational:
		return p.Value < v.Float64()
	case *Complex:
		return p.Value < real(v.Value)
	case *BigComplex:
		self := new(big.Float).SetFloat64(p.Value)
		return self.Cmp(toBigFloat(v.Real()).BigFloatValue()) < 0
	}
	panic(ErrNotANumber)
}

func (p *Float) Abs() *Float {
	return NewFloat(math.Abs(p.Value))
}

// Negate returns the negation of this float.
//
// R7RS §6.2.6: The - procedure with one argument returns the additive inverse.
func (p *Float) Negate() Number {
	return NewFloat(-p.Value)
}

// Compare compares this float with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of exactness.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Float) Compare(o Number) int {
	pf := new(big.Float).SetFloat64(p.Value)
	switch v := o.(type) {
	case *BigFloat:
		return pf.Cmp(v.value)
	case *BigInteger:
		vf := new(big.Float).SetInt(v.value)
		return pf.Cmp(vf)
	case *Integer:
		vf := new(big.Float).SetInt64(v.Value)
		return pf.Cmp(vf)
	case *Float:
		vf := new(big.Float).SetFloat64(v.Value)
		return pf.Cmp(vf)
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return pf.Cmp(vf)
	case *Complex:
		r := real(v.Value)
		if p.Value < r {
			return -1
		} else if p.Value > r {
			return 1
		}
		return 0
	case *BigComplex:
		return pf.Cmp(toBigFloat(v.Real()).BigFloatValue())
	}
	panic(ErrNotANumber)
}

// IsExact returns false since Float is always inexact.
//
// R7RS §6.2.2: Floating-point numbers are inexact.
func (p *Float) IsExact() bool {
	return false
}

// IsVoid returns true if the float is nil.
func (p *Float) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both floats have the same value.
func (p *Float) EqualTo(v Value) bool {
	if other, ok := v.(*Float); ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the float.
//
// R7RS §6.2.5: +inf.0, -inf.0, and +nan.0 are the written representations
// for positive infinity, negative infinity, and NaN.
// R7RS §7.1.1: Inexact real numbers must contain a decimal point to distinguish
// them from exact integers.
func (p *Float) SchemeString() string {
	if math.IsInf(p.Value, 1) {
		return "+inf.0"
	}
	if math.IsInf(p.Value, -1) {
		return "-inf.0"
	}
	if math.IsNaN(p.Value) {
		return "+nan.0"
	}
	s := strconv.FormatFloat(p.Value, 'f', -1, 64)
	// Ensure inexact integers have a decimal point to distinguish from exact integers
	for i := 0; i < len(s); i++ {
		if s[i] == '.' {
			return s
		}
	}
	return s + ".0"
}

func (p *Float) String() string {
	return strconv.FormatFloat(p.Value, 'f', -1, 64)
}

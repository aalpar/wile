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
	"math/big"
)

var (
	_ Value  = (*BigFloat)(nil)
	_ Number = (*BigFloat)(nil)
)

// DefaultBigFloatPrecision is the default precision for BigFloat values.
const DefaultBigFloatPrecision = 256

// BigFloat represents an arbitrary-precision floating-point number.
// Created with the #z prefix in Scheme (e.g., #z3.14159265358979323846).
type BigFloat struct {
	value *big.Float
}

// NewBigFloat creates a new BigFloat from a big.Float.
func NewBigFloat(v *big.Float) *BigFloat {
	return &BigFloat{value: new(big.Float).Copy(v)}
}

// NewBigFloatFromFloat64 creates a new BigFloat from a float64.
func NewBigFloatFromFloat64(v float64) *BigFloat {
	return &BigFloat{value: big.NewFloat(v).SetPrec(DefaultBigFloatPrecision)}
}

// NewBigFloatFromString creates a new BigFloat from a string.
// Returns nil if the string is not a valid number.
func NewBigFloatFromString(s string) *BigFloat {
	v, _, err := big.ParseFloat(s, 10, DefaultBigFloatPrecision, big.ToNearestEven)
	if err != nil {
		return nil
	}
	return &BigFloat{value: v}
}

// BigFloatValue returns the underlying big.Float value.
func (p *BigFloat) BigFloatValue() *big.Float {
	return p.value
}

// Float64 returns the value as float64 (may lose precision).
func (p *BigFloat) Float64() float64 {
	f, _ := p.value.Float64()
	return f
}

func (p *BigFloat) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Add(p.value, v.value)}
	case *BigInteger:
		vf := new(big.Float).SetInt(v.value)
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Integer:
		vf := new(big.Float).SetInt64(v.Value)
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Float:
		vf := new(big.Float).SetFloat64(v.Value)
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Complex:
		f, _ := p.value.Float64()
		return NewComplex(complex(f, 0) + v.Datum())
	}
	return nil
}

func (p *BigFloat) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Sub(p.value, v.value)}
	case *BigInteger:
		vf := new(big.Float).SetInt(v.value)
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Integer:
		vf := new(big.Float).SetInt64(v.Value)
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Float:
		vf := new(big.Float).SetFloat64(v.Value)
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Complex:
		f, _ := p.value.Float64()
		return NewComplex(complex(f, 0) - v.Datum())
	}
	return nil
}

func (p *BigFloat) Multiply(o Number) Number {
	if o.IsZero() {
		return NewBigFloatFromFloat64(0)
	}
	if p.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Mul(p.value, v.value)}
	case *BigInteger:
		vf := new(big.Float).SetInt(v.value)
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Integer:
		vf := new(big.Float).SetInt64(v.Value)
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Float:
		vf := new(big.Float).SetFloat64(v.Value)
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Complex:
		f, _ := p.value.Float64()
		return NewComplex(complex(f, 0) * v.Datum())
	}
	return nil
}

func (p *BigFloat) Divide(o Number) Number {
	if o.IsZero() {
		return nil // Division by zero
	}
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Quo(p.value, v.value)}
	case *BigInteger:
		vf := new(big.Float).SetInt(v.value)
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Integer:
		vf := new(big.Float).SetInt64(v.Value)
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Float:
		vf := new(big.Float).SetFloat64(v.Value)
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Complex:
		f, _ := p.value.Float64()
		return NewComplex(complex(f, 0) / v.Datum())
	}
	return nil
}

func (p *BigFloat) Negate() Number {
	return &BigFloat{value: new(big.Float).Neg(p.value)}
}

func (p *BigFloat) IsZero() bool {
	return p.value.Sign() == 0
}

func (p *BigFloat) LessThan(o Number) bool {
	return p.Compare(o) < 0
}

func (p *BigFloat) IsNegative() bool {
	return p.value.Sign() < 0
}

func (p *BigFloat) IsPositive() bool {
	return p.value.Sign() > 0
}

func (p *BigFloat) IsExact() bool {
	return false // BigFloat is inexact
}

func (p *BigFloat) ToExact() Number {
	// Convert to Rational for exact representation
	r, _ := p.value.Rat(nil)
	if r == nil {
		return NewRational(0, 1)
	}
	return NewRationalFromRat(r)
}

func (p *BigFloat) ToInexact() Number {
	return p
}

func (p *BigFloat) Compare(o Number) int {
	switch v := o.(type) {
	case *BigFloat:
		return p.value.Cmp(v.value)
	case *BigInteger:
		vf := new(big.Float).SetInt(v.value)
		return p.value.Cmp(vf)
	case *Integer:
		vf := new(big.Float).SetInt64(v.Value)
		return p.value.Cmp(vf)
	case *Float:
		vf := new(big.Float).SetFloat64(v.Value)
		return p.value.Cmp(vf)
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return p.value.Cmp(vf)
	}
	return 0
}

func (p *BigFloat) SchemeString() string {
	return p.value.Text('g', -1)
}

func (p *BigFloat) IsVoid() bool {
	return p == nil
}

func (p *BigFloat) EqualTo(o Value) bool {
	v, ok := o.(*BigFloat)
	if !ok {
		// Also check if equal to regular Float
		if f, ok := o.(*Float); ok {
			vf := new(big.Float).SetFloat64(f.Value)
			return p.value.Cmp(vf) == 0
		}
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	return p.value.Cmp(v.value) == 0
}

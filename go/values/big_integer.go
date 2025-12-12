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
	_ Value  = (*BigInteger)(nil)
	_ Number = (*BigInteger)(nil)
)

// BigInteger represents an arbitrary-precision integer.
// Created with the #m prefix in Scheme (e.g., #m12345678901234567890).
type BigInteger struct {
	value *big.Int
}

// NewBigInteger creates a new BigInteger from a big.Int.
func NewBigInteger(v *big.Int) *BigInteger {
	return &BigInteger{value: new(big.Int).Set(v)}
}

// NewBigIntegerFromInt64 creates a new BigInteger from an int64.
func NewBigIntegerFromInt64(v int64) *BigInteger {
	return &BigInteger{value: big.NewInt(v)}
}

// NewBigIntegerFromString creates a new BigInteger from a string.
// Returns nil if the string is not a valid integer.
func NewBigIntegerFromString(s string, base int) *BigInteger {
	v := new(big.Int)
	_, ok := v.SetString(s, base)
	if !ok {
		return nil
	}
	return &BigInteger{value: v}
}

// BigInt returns the underlying big.Int value.
func (p *BigInteger) BigInt() *big.Int {
	return p.value
}

// Int64 returns the value as int64 (may overflow for large values).
func (p *BigInteger) Int64() int64 {
	return p.value.Int64()
}

func (p *BigInteger) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *BigInteger:
		return &BigInteger{value: new(big.Int).Add(p.value, v.value)}
	case *Integer:
		return &BigInteger{value: new(big.Int).Add(p.value, big.NewInt(v.Value))}
	case *Float:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewFloat(f + v.Value)
	case *Rational:
		// Convert BigInteger to Rational and add
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Add(pRat, v.Rat()))
	case *Complex:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewComplex(complex(f, 0) + v.Datum())
	}
	return nil
}

func (p *BigInteger) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigInteger:
		return &BigInteger{value: new(big.Int).Sub(p.value, v.value)}
	case *Integer:
		return &BigInteger{value: new(big.Int).Sub(p.value, big.NewInt(v.Value))}
	case *Float:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewFloat(f - v.Value)
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Sub(pRat, v.Rat()))
	case *Complex:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewComplex(complex(f, 0) - v.Datum())
	}
	return nil
}

func (p *BigInteger) Multiply(o Number) Number {
	if o.IsZero() {
		return NewBigIntegerFromInt64(0)
	}
	if p.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigInteger:
		return &BigInteger{value: new(big.Int).Mul(p.value, v.value)}
	case *Integer:
		return &BigInteger{value: new(big.Int).Mul(p.value, big.NewInt(v.Value))}
	case *Float:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewFloat(f * v.Value)
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Mul(pRat, v.Rat()))
	case *Complex:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewComplex(complex(f, 0) * v.Datum())
	}
	return nil
}

func (p *BigInteger) Divide(o Number) Number {
	if o.IsZero() {
		return nil // Division by zero
	}
	switch v := o.(type) {
	case *BigInteger:
		// Return rational for exact division
		return NewRationalFromBigInt(p.value, v.value)
	case *Integer:
		return NewRationalFromBigInt(p.value, big.NewInt(v.Value))
	case *Float:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewFloat(f / v.Value)
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Quo(pRat, v.Rat()))
	case *Complex:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		return NewComplex(complex(f, 0) / v.Datum())
	}
	return nil
}

func (p *BigInteger) Negate() Number {
	return &BigInteger{value: new(big.Int).Neg(p.value)}
}

func (p *BigInteger) IsZero() bool {
	return p.value.Sign() == 0
}

func (p *BigInteger) LessThan(o Number) bool {
	return p.Compare(o) < 0
}

func (p *BigInteger) IsNegative() bool {
	return p.value.Sign() < 0
}

func (p *BigInteger) IsPositive() bool {
	return p.value.Sign() > 0
}

func (p *BigInteger) IsExact() bool {
	return true
}

func (p *BigInteger) ToExact() Number {
	return p
}

func (p *BigInteger) ToInexact() Number {
	f, _ := new(big.Float).SetInt(p.value).Float64()
	return NewFloat(f)
}

func (p *BigInteger) Compare(o Number) int {
	switch v := o.(type) {
	case *BigInteger:
		return p.value.Cmp(v.value)
	case *Integer:
		return p.value.Cmp(big.NewInt(v.Value))
	case *Float:
		f, _ := new(big.Float).SetInt(p.value).Float64()
		if f < v.Value {
			return -1
		} else if f > v.Value {
			return 1
		}
		return 0
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return pRat.Cmp(v.Rat())
	}
	return 0
}

func (p *BigInteger) SchemeString() string {
	return p.value.String()
}

func (p *BigInteger) IsVoid() bool {
	return p == nil
}

func (p *BigInteger) EqualTo(o Value) bool {
	v, ok := o.(*BigInteger)
	if !ok {
		// Also check if equal to regular Integer
		if i, ok := o.(*Integer); ok {
			return p.value.Cmp(big.NewInt(i.Value)) == 0
		}
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	return p.value.Cmp(v.value) == 0
}

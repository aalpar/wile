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
	"strconv"
)

var (
	_ Value  = (*Integer)(nil)
	_ Number = (*Integer)(nil)
	// _ Comparable = (*Integer)(nil)
)

// Integer cache for small integers (-32768 to 32767).
// This avoids allocations for commonly used integer values.
// Uses 16-bit range to cover most practical small integers.
const (
	intCacheMin = -32768
	intCacheMax = 32767
)

var intCache [intCacheMax - intCacheMin + 1]*Integer

func init() {
	for i := int64(intCacheMin); i <= intCacheMax; i++ {
		intCache[i-intCacheMin] = &Integer{Value: i}
	}
}

// Integer represents a Scheme integer value.
//
// R7RS §6.2.1: Integers are exact numbers in the numeric tower hierarchy:
//
//	number ⊃ complex ⊃ real ⊃ rational ⊃ integer
//
// R7RS §6.2.2: Integer is always exact. Operations on exact numbers
// produce exact results when mathematically well-defined.
type Integer struct {
	Value int64
}

// NewInteger returns an Integer value. Small integers in the range
// -256 to 255 are cached and return the same pointer for the same value.
func NewInteger(v int64) *Integer {
	if v >= intCacheMin && v <= intCacheMax {
		return intCache[v-intCacheMin]
	}
	return &Integer{Value: v}
}

// Datum returns the underlying int64 value.
func (p *Integer) Datum() int64 {
	return p.Value
}

// addSame adds two integers of the same type.
func (p *Integer) addSame(o *Integer) Number {
	return NewInteger(p.Value + o.Value)
}

// subtractSame subtracts two integers of the same type.
func (p *Integer) subtractSame(o *Integer) Number {
	return NewInteger(p.Value - o.Value)
}

// multiplySame multiplies two integers of the same type.
func (p *Integer) multiplySame(o *Integer) Number {
	return NewInteger(p.Value * o.Value)
}

// divideSame divides two integers of the same type.
// Returns Rational if not evenly divisible.
func (p *Integer) divideSame(o *Integer) Number {
	if o.Value == 0 {
		panic(ErrDivisionByZero)
	}
	result := NewRational(p.Value, o.Value)
	if result.IsInteger() {
		return NewInteger(result.NumInt64())
	}
	return result
}

// compareSame compares two integers of the same type.
func (p *Integer) compareSame(o *Integer) int {
	if p.Value < o.Value {
		return -1
	} else if p.Value > o.Value {
		return 1
	}
	return 0
}

// Add returns the sum of this integer and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
// When adding Integer + BigInteger, result is BigInteger (exact).
// When adding Integer + Float/Complex, result is Float/Complex (inexact).
func (p *Integer) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value + v.Value)
	case *BigInteger:
		result := newBigIntFromOp((*big.Int).Add, big.NewInt(p.Value), v.value)
		return &BigInteger{value: result}
	case *Float:
		return NewFloat(float64(p.Value) + v.Value)
	case *BigFloat:
		self := new(big.Float).SetInt64(p.Value)
		return &BigFloat{value: new(big.Float).Add(self, v.value)}
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Add(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) + v.Value)
	case *BigComplex:
		bc := NewBigComplex(NewBigIntegerFromInt64(p.Value), NewBigIntegerFromInt64(0))
		return bc.Add(v)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of this integer and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *Integer) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value - v.Value)
	case *BigInteger:
		result := newBigIntFromOp((*big.Int).Sub, big.NewInt(p.Value), v.value)
		return &BigInteger{value: result}
	case *Float:
		return NewFloat(float64(p.Value) - v.Value)
	case *BigFloat:
		self := new(big.Float).SetInt64(p.Value)
		return &BigFloat{value: new(big.Float).Sub(self, v.value)}
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Sub(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) - v.Value)
	case *BigComplex:
		bc := NewBigComplex(NewBigIntegerFromInt64(p.Value), NewBigIntegerFromInt64(0))
		return bc.Subtract(v)
	}
	panic(ErrNotANumber)
}

// Multiply returns the product of this integer and another number.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2 Exactness: exact * exact = exact, exact * inexact = inexact.
// Exception: Exact zero dominates—(* 0 x) may return exact 0 even when
// x is inexact. Zero is an exact value when the result is mathematically
// unambiguous. This implementation follows Chez Scheme's behavior.
func (p *Integer) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value * v.Value)
	case *BigInteger:
		result := newBigIntFromOp((*big.Int).Mul, big.NewInt(p.Value), v.value)
		return &BigInteger{value: result}
	case *Float:
		return NewFloat(float64(p.Value) * v.Value)
	case *BigFloat:
		self := new(big.Float).SetInt64(p.Value)
		return &BigFloat{value: new(big.Float).Mul(self, v.value)}
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Mul(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) * v.Value)
	case *BigComplex:
		bc := NewBigComplex(NewBigIntegerFromInt64(p.Value), NewBigIntegerFromInt64(0))
		return bc.Multiply(v)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of this integer and another number.
//
// R7RS §6.2.6: The / procedure returns the quotient of its arguments.
// For exact arguments, / may return a non-integer (Rational) when the
// mathematical result is not an integer. Returns Integer only when
// the division is exact.
//
// R7RS §6.2.2 Exactness: exact / exact = exact (Integer or Rational),
// exact / inexact = inexact (Float or Complex).
func (p *Integer) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Integer:
		result := NewRational(p.Value, v.Value)
		if result.IsInteger() {
			return NewInteger(result.NumInt64())
		}
		return result
	case *BigInteger:
		return NewRationalFromBigInt(big.NewInt(p.Value), v.value)
	case *Float:
		return NewFloat(float64(p.Value) / v.Value)
	case *BigFloat:
		self := new(big.Float).SetInt64(p.Value)
		return &BigFloat{value: new(big.Float).Quo(self, v.value)}
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Quo(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) / v.Value)
	case *BigComplex:
		bc := NewBigComplex(NewBigIntegerFromInt64(p.Value), NewBigIntegerFromInt64(0))
		return bc.Divide(v)
	}
	panic(ErrNotANumber)
}

// IsZero returns true if this integer is zero.
func (p *Integer) IsZero() bool {
	return p.Value == 0
}

// LessThan returns true if this integer is less than another number.
//
// R7RS §6.2.6: The < procedure returns #t if its arguments are monotonically
// increasing. Comparison across numeric types uses mathematical value.
func (p *Integer) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Integer:
		return p.Value < v.Value
	case *BigInteger:
		return big.NewInt(p.Value).Cmp(v.BigInt()) < 0
	case *Float:
		return float64(p.Value) < v.Value
	case *BigFloat:
		self := new(big.Float).SetInt64(p.Value)
		return self.Cmp(v.BigFloatValue()) < 0
	case *Rational:
		self := big.NewRat(p.Value, 1)
		return self.Cmp(v.Rat()) < 0
	case *Complex:
		return float64(p.Value) < real(v.Value)
	case *BigComplex:
		return toBigFloat(NewBigIntegerFromInt64(p.Value)).Compare(v.Real()) < 0
	}
	panic(ErrNotANumber)
}

func (p *Integer) Abs() *Integer {
	if p.Value < 0 {
		return NewInteger(-p.Value)
	}
	return NewInteger(p.Value)
}

// Negate returns the negation of this integer.
//
// R7RS §6.2.6: The - procedure with one argument returns the additive inverse.
func (p *Integer) Negate() Number {
	return NewInteger(-p.Value)
}

// Compare compares this integer with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of exactness.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Integer) Compare(o Number) int {
	switch v := o.(type) {
	case *Integer:
		if p.Value < v.Value {
			return -1
		} else if p.Value > v.Value {
			return 1
		}
		return 0
	case *BigInteger:
		return big.NewInt(p.Value).Cmp(v.value)
	case *Float:
		pf := float64(p.Value)
		if pf < v.Value {
			return -1
		} else if pf > v.Value {
			return 1
		}
		return 0
	case *BigFloat:
		self := new(big.Float).SetInt64(p.Value)
		return self.Cmp(v.BigFloatValue())
	case *Rational:
		self := big.NewRat(p.Value, 1)
		return self.Cmp(v.Rat())
	case *Complex:
		pf := float64(p.Value)
		r := real(v.Value)
		if pf < r {
			return -1
		} else if pf > r {
			return 1
		}
		return 0
	case *BigComplex:
		self := new(big.Float).SetInt64(p.Value)
		return self.Cmp(toBigFloat(v.Real()).BigFloatValue())
	}
	panic(ErrNotANumber)
}

// IsExact returns true since Integer is always exact.
//
// R7RS §6.2.2: Integers are always exact numbers.
func (p *Integer) IsExact() bool {
	return true
}

// IsVoid returns true if this integer is nil.
func (p *Integer) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both integers have the same value.
//
// R7RS §6.2.6: The = procedure compares numerical values for equality.
// This implements structural equality for the Integer type specifically.
// Handles comparison with both Integer and BigInteger types.
func (p *Integer) EqualTo(v Value) bool {
	switch other := v.(type) {
	case *Integer:
		return p.Value == other.Value
	case *BigInteger:
		return other.BigInt().Cmp(big.NewInt(p.Value)) == 0
	}
	return false
}

// SchemeString returns the Scheme representation of this integer.
func (p *Integer) SchemeString() string {
	return strconv.FormatInt(p.Value, 10)
}

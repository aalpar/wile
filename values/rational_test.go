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
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestRational_EqualTo(t *testing.T) {
	r1 := NewRational(1, 2)
	r2 := NewRational(1, 2)
	qt.Assert(t, r1.EqualTo(r2), qt.IsTrue)

	r3 := NewRational(2, 4)
	qt.Assert(t, r1.EqualTo(r3), qt.IsTrue)

	r4 := NewRational(1, 3)
	qt.Assert(t, r1.EqualTo(r4), qt.IsFalse)
}

func TestRational_Add(t *testing.T) {
	r1 := NewRational(1, 2)
	r2 := NewRational(1, 3)
	result := r1.Add(r2)
	qt.Assert(t, result, SchemeEquals, NewRational(5, 6))

	r3 := NewRational(0, 1)
	result = r1.Add(r3)
	qt.Assert(t, result, SchemeEquals, NewRational(1, 2))

	i1 := NewInteger(2)
	result = r1.Add(i1)
	qt.Assert(t, result, SchemeEquals, NewRational(5, 2))

	f1 := NewFloat(2.5)
	result = r1.Add(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(3.0))

	c1 := NewComplex(complex(1, 2))
	result = r1.Add(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(1.5, 2)))
}

func TestRational_Subtract(t *testing.T) {
	r1 := NewRational(1, 2)
	r2 := NewRational(1, 3)
	result := r1.Subtract(r2)
	qt.Assert(t, result, SchemeEquals, NewRational(1, 6))

	r3 := NewRational(0, 1)
	result = r1.Subtract(r3)
	qt.Assert(t, result, SchemeEquals, NewRational(1, 2))

	i1 := NewInteger(2)
	result = r1.Subtract(i1)
	qt.Assert(t, result, SchemeEquals, NewRational(-3, 2))

	f1 := NewFloat(0.5)
	result = r1.Subtract(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(0.0))

	c1 := NewComplex(complex(1, 2))
	result = r1.Subtract(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(-0.5, -2)))
}

func TestRational_Multiply(t *testing.T) {
	r1 := NewRational(1, 2)
	r2 := NewRational(2, 3)
	result := r1.Multiply(r2)
	qt.Assert(t, result, SchemeEquals, NewRational(1, 3))

	r3 := NewRational(0, 1)
	result = r1.Multiply(r3)
	qt.Assert(t, result, SchemeEquals, NewRational(0, 1))

	i1 := NewInteger(3)
	result = r1.Multiply(i1)
	qt.Assert(t, result, SchemeEquals, NewRational(3, 2))

	f1 := NewFloat(2.0)
	result = r1.Multiply(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(1.0))

	c1 := NewComplex(complex(2, 3))
	result = r1.Multiply(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(1, 1.5)))
}

func TestRational_Divide(t *testing.T) {
	r1 := NewRational(1, 2)
	r2 := NewRational(1, 3)
	result := r1.Divide(r2)
	qt.Assert(t, result, SchemeEquals, NewRational(3, 2))

	i1 := NewInteger(2)
	result = r1.Divide(i1)
	qt.Assert(t, result, SchemeEquals, NewRational(1, 4))

	f1 := NewFloat(2.0)
	result = r1.Divide(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(0.25))

	c1 := NewComplex(complex(2, 0))
	result = r1.Divide(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(0.25, 0)))
}

func TestRational_IsZero(t *testing.T) {
	r1 := NewRational(0, 1)
	qt.Assert(t, r1.IsZero(), qt.IsTrue)

	r2 := NewRational(1, 2)
	qt.Assert(t, r2.IsZero(), qt.IsFalse)
}

func TestRational_LessThan(t *testing.T) {
	r1 := NewRational(1, 2)
	r2 := NewRational(2, 3)
	qt.Assert(t, r1.LessThan(r2), qt.IsTrue)
	qt.Assert(t, r2.LessThan(r1), qt.IsFalse)

	i1 := NewInteger(1)
	qt.Assert(t, r1.LessThan(i1), qt.IsTrue)

	f1 := NewFloat(0.6)
	qt.Assert(t, r1.LessThan(f1), qt.IsTrue)

	c1 := NewComplex(complex(1, 0))
	qt.Assert(t, r1.LessThan(c1), qt.IsTrue)
}

func TestRational_IsInteger(t *testing.T) {
	r1 := NewRational(4, 2)
	qt.Assert(t, r1.IsInteger(), qt.IsTrue)

	r2 := NewRational(1, 2)
	qt.Assert(t, r2.IsInteger(), qt.IsFalse)
}

func TestRational_NumInt64(t *testing.T) {
	r := NewRational(5, 3)
	qt.Assert(t, r.NumInt64(), qt.Equals, int64(5))
}

func TestRational_DenomInt64(t *testing.T) {
	r := NewRational(5, 3)
	qt.Assert(t, r.DenomInt64(), qt.Equals, int64(3))
}

func TestRational_Float64(t *testing.T) {
	r := NewRational(1, 2)
	qt.Assert(t, r.Float64(), qt.Equals, 0.5)
}

func TestRational_SchemeString(t *testing.T) {
	r := NewRational(1, 2)
	qt.Assert(t, r.SchemeString(), qt.Equals, "1/2")
}

func TestRational_Rat(t *testing.T) {
	r := NewRational(1, 2)
	rat := r.Rat()
	qt.Assert(t, rat.RatString(), qt.Equals, "1/2")
}

func TestRational_Num(t *testing.T) {
	r := NewRational(5, 3)
	num := r.Num()
	expected := big.NewInt(5)
	qt.Assert(t, num.Cmp(expected), qt.Equals, 0)
}

func TestRational_Denom(t *testing.T) {
	r := NewRational(5, 3)
	denom := r.Denom()
	expected := big.NewInt(3)
	qt.Assert(t, denom.Cmp(expected), qt.Equals, 0)
}

func TestNewRationalFromBigInt(t *testing.T) {
	num := big.NewInt(7)
	denom := big.NewInt(4)
	r := NewRationalFromBigInt(num, denom)
	qt.Assert(t, r.SchemeString(), qt.Equals, "7/4")
}

func TestNewRationalFromRat(t *testing.T) {
	rat := big.NewRat(3, 5)
	r := NewRationalFromRat(rat)
	qt.Assert(t, r.SchemeString(), qt.Equals, "3/5")
}

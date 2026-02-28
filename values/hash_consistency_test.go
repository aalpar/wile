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

package values_test

import (
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// TestHashConsistencyExactFamily verifies that Integer, BigInteger, and
// Rational produce identical hashes for mathematically equal values.
// This is the Hashable contract: if a.EqualTo(b) then a.HashCode() == b.HashCode().
func TestHashConsistencyExactFamily(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		int_ *values.Integer
		big_ *values.BigInteger
		rat_ *values.Rational
	}{
		{"positive", values.NewInteger(5), values.NewBigIntegerFromInt64(5), values.NewRational(5, 1)},
		{"zero", values.NewInteger(0), values.NewBigIntegerFromInt64(0), values.NewRational(0, 1)},
		{"negative", values.NewInteger(-42), values.NewBigIntegerFromInt64(-42), values.NewRational(-42, 1)},
		{"large", values.NewInteger(1000000), values.NewBigIntegerFromInt64(1000000), values.NewRational(1000000, 1)},
	}

	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			// Precondition: all three are EqualTo each other.
			c.Assert(tc.int_.EqualTo(tc.big_), qt.IsTrue)
			c.Assert(tc.int_.EqualTo(tc.rat_), qt.IsTrue)
			c.Assert(tc.big_.EqualTo(tc.int_), qt.IsTrue)
			c.Assert(tc.big_.EqualTo(tc.rat_), qt.IsTrue)
			c.Assert(tc.rat_.EqualTo(tc.int_), qt.IsTrue)
			c.Assert(tc.rat_.EqualTo(tc.big_), qt.IsTrue)

			// Contract: equal values must have equal hashes.
			intHash := tc.int_.HashCode()
			bigHash := tc.big_.HashCode()
			ratHash := tc.rat_.HashCode()
			c.Assert(intHash, qt.Equals, bigHash,
				qt.Commentf("Integer vs BigInteger for %s", tc.name))
			c.Assert(intHash, qt.Equals, ratHash,
				qt.Commentf("Integer vs Rational for %s", tc.name))
		})
	}
}

// TestHashConsistencyExactLargeValues tests values that overflow int64,
// ensuring BigInteger and Rational still agree.
func TestHashConsistencyExactLargeValues(t *testing.T) {
	c := qt.New(t)

	// A value that doesn't fit in int64.
	large := new(big.Int).Exp(big.NewInt(2), big.NewInt(100), nil)
	bi := values.NewBigInteger(large)
	rat := values.NewRationalFromBigInt(large, big.NewInt(1))

	c.Assert(bi.EqualTo(rat), qt.IsTrue)
	c.Assert(bi.HashCode(), qt.Equals, rat.HashCode())
}

// TestHashConsistencyInexactFamily verifies that Float and BigFloat
// produce identical hashes for mathematically equal values.
func TestHashConsistencyInexactFamily(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		f    *values.Float
		bf   *values.BigFloat
	}{
		{"pi-ish", values.NewFloat(3.14), values.NewBigFloatFromFloat64(3.14)},
		{"zero", values.NewFloat(0.0), values.NewBigFloatFromFloat64(0.0)},
		{"negative", values.NewFloat(-2.5), values.NewBigFloatFromFloat64(-2.5)},
		{"one", values.NewFloat(1.0), values.NewBigFloatFromFloat64(1.0)},
		{"small", values.NewFloat(0.001), values.NewBigFloatFromFloat64(0.001)},
	}

	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			// Precondition: EqualTo holds.
			c.Assert(tc.f.EqualTo(tc.bf), qt.IsTrue)
			c.Assert(tc.bf.EqualTo(tc.f), qt.IsTrue)

			// Contract: equal values must have equal hashes.
			c.Assert(tc.f.HashCode(), qt.Equals, tc.bf.HashCode(),
				qt.Commentf("Float vs BigFloat for %s", tc.name))
		})
	}
}

// TestHashConsistencyFloatSpecialValues ensures NaN, +Inf, -Inf
// don't panic and produce self-consistent hashes.
func TestHashConsistencyFloatSpecialValues(t *testing.T) {
	c := qt.New(t)

	specials := []struct {
		name string
		val  float64
	}{
		{"NaN", math.NaN()},
		{"+Inf", math.Inf(1)},
		{"-Inf", math.Inf(-1)},
	}

	for _, tc := range specials {
		c.Run(tc.name, func(c *qt.C) {
			f := values.NewFloat(tc.val)
			// Must not panic.
			h := f.HashCode()
			// Same value must produce same hash (stability).
			c.Assert(h, qt.Equals, values.NewFloat(tc.val).HashCode())
		})
	}
}

// TestHashConsistencyHashtableExact verifies the end-to-end hashtable
// contract: Set with one exact type, Get with another.
func TestHashConsistencyHashtableExact(t *testing.T) {
	c := qt.New(t)

	ht := values.NewEmptyHashtable()
	key := values.NewInteger(42)
	val := values.NewInteger(100)
	err := ht.Set(key, val)
	c.Assert(err, qt.IsNil)

	// Look up with BigInteger.
	got, ok, err := ht.Get(values.NewBigIntegerFromInt64(42))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, valuestest.SchemeEquals, val)

	// Look up with Rational.
	got, ok, err = ht.Get(values.NewRational(42, 1))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, valuestest.SchemeEquals, val)
}

// TestHashConsistencyHashtableInexact verifies the end-to-end hashtable
// contract: Set with Float, Get with BigFloat.
func TestHashConsistencyHashtableInexact(t *testing.T) {
	c := qt.New(t)

	ht := values.NewEmptyHashtable()
	key := values.NewFloat(3.14)
	val := values.NewInteger(999)
	err := ht.Set(key, val)
	c.Assert(err, qt.IsNil)

	// Look up with BigFloat.
	got, ok, err := ht.Get(values.NewBigFloatFromFloat64(3.14))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, valuestest.SchemeEquals, val)
}

// TestHashConsistencyExactInexactBoundary is a regression guard ensuring
// that exact and inexact types don't accidentally hash-collide in a way
// that would break the type boundary. Integer(5) is NOT EqualTo Float(5.0)
// in this implementation (exact != inexact).
func TestHashConsistencyExactInexactBoundary(t *testing.T) {
	c := qt.New(t)

	i := values.NewInteger(5)
	f := values.NewFloat(5.0)
	c.Assert(i.EqualTo(f), qt.IsFalse,
		qt.Commentf("exact Integer must not equal inexact Float"))
}

// TestHashConsistencySameValueStability ensures the same type and value
// always produces the same hash (regression guard against nondeterminism).
func TestHashConsistencySameValueStability(t *testing.T) {
	c := qt.New(t)

	i1 := values.NewInteger(7)
	i2 := values.NewInteger(7)
	c.Assert(i1.HashCode(), qt.Equals, i2.HashCode())

	f1 := values.NewFloat(2.718)
	f2 := values.NewFloat(2.718)
	c.Assert(f1.HashCode(), qt.Equals, f2.HashCode())

	r1 := values.NewRational(3, 7)
	r2 := values.NewRational(3, 7)
	c.Assert(r1.HashCode(), qt.Equals, r2.HashCode())

	bi1 := values.NewBigIntegerFromInt64(99)
	bi2 := values.NewBigIntegerFromInt64(99)
	c.Assert(bi1.HashCode(), qt.Equals, bi2.HashCode())

	bf1 := values.NewBigFloatFromFloat64(1.23)
	bf2 := values.NewBigFloatFromFloat64(1.23)
	c.Assert(bf1.HashCode(), qt.Equals, bf2.HashCode())
}

// TestHashConsistencyComplexFamily verifies that BigComplex (with BigFloat parts)
// and Complex produce the same hash when BigComplex.EqualTo(Complex) holds.
// Contract: if a.EqualTo(b) then a.HashCode() == b.HashCode().
func TestHashConsistencyComplexFamily(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name    string
		complex *values.Complex
		big     *values.BigComplex
	}{
		{
			"3+4i",
			values.NewComplex(complex(3, 4)),
			values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(3), values.NewBigFloatFromFloat64(4)),
		},
		{
			"zero",
			values.NewComplex(complex(0, 0)),
			values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(0), values.NewBigFloatFromFloat64(0)),
		},
		{
			"1.5+2.5i",
			values.NewComplex(complex(1.5, 2.5)),
			values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(1.5), values.NewBigFloatFromFloat64(2.5)),
		},
	}

	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			// Precondition: BigComplex.EqualTo(Complex) holds.
			c.Assert(tc.big.EqualTo(tc.complex), qt.IsTrue,
				qt.Commentf("precondition: BigComplex must equal Complex"))

			// Contract: equal values must produce equal hashes.
			c.Assert(tc.big.HashCode(), qt.Equals, tc.complex.HashCode(),
				qt.Commentf("BigComplex and Complex hash must agree for %s", tc.name))
		})
	}
}

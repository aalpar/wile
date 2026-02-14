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
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestHashConsistencyExactFamily verifies that Integer, BigInteger, and
// Rational produce identical hashes for mathematically equal values.
// This is the Hashable contract: if a.EqualTo(b) then a.HashCode() == b.HashCode().
func TestHashConsistencyExactFamily(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		int_ *Integer
		big_ *BigInteger
		rat_ *Rational
	}{
		{"positive", NewInteger(5), NewBigIntegerFromInt64(5), NewRational(5, 1)},
		{"zero", NewInteger(0), NewBigIntegerFromInt64(0), NewRational(0, 1)},
		{"negative", NewInteger(-42), NewBigIntegerFromInt64(-42), NewRational(-42, 1)},
		{"large", NewInteger(1000000), NewBigIntegerFromInt64(1000000), NewRational(1000000, 1)},
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
	bi := NewBigInteger(large)
	rat := NewRationalFromBigInt(large, big.NewInt(1))

	c.Assert(bi.EqualTo(rat), qt.IsTrue)
	c.Assert(bi.HashCode(), qt.Equals, rat.HashCode())
}

// TestHashConsistencyInexactFamily verifies that Float and BigFloat
// produce identical hashes for mathematically equal values.
func TestHashConsistencyInexactFamily(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		f    *Float
		bf   *BigFloat
	}{
		{"pi-ish", NewFloat(3.14), NewBigFloatFromFloat64(3.14)},
		{"zero", NewFloat(0.0), NewBigFloatFromFloat64(0.0)},
		{"negative", NewFloat(-2.5), NewBigFloatFromFloat64(-2.5)},
		{"one", NewFloat(1.0), NewBigFloatFromFloat64(1.0)},
		{"small", NewFloat(0.001), NewBigFloatFromFloat64(0.001)},
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
			f := NewFloat(tc.val)
			// Must not panic.
			h := f.HashCode()
			// Same value must produce same hash (stability).
			c.Assert(h, qt.Equals, NewFloat(tc.val).HashCode())
		})
	}
}

// TestHashConsistencyHashtableExact verifies the end-to-end hashtable
// contract: Set with one exact type, Get with another.
func TestHashConsistencyHashtableExact(t *testing.T) {
	c := qt.New(t)

	ht := NewEmptyHashtable()
	key := NewInteger(42)
	val := NewInteger(100)
	err := ht.Set(key, val)
	c.Assert(err, qt.IsNil)

	// Look up with BigInteger.
	got, ok, err := ht.Get(NewBigIntegerFromInt64(42))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, SchemeEquals, val)

	// Look up with Rational.
	got, ok, err = ht.Get(NewRational(42, 1))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, SchemeEquals, val)
}

// TestHashConsistencyHashtableInexact verifies the end-to-end hashtable
// contract: Set with Float, Get with BigFloat.
func TestHashConsistencyHashtableInexact(t *testing.T) {
	c := qt.New(t)

	ht := NewEmptyHashtable()
	key := NewFloat(3.14)
	val := NewInteger(999)
	err := ht.Set(key, val)
	c.Assert(err, qt.IsNil)

	// Look up with BigFloat.
	got, ok, err := ht.Get(NewBigFloatFromFloat64(3.14))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, SchemeEquals, val)
}

// TestHashConsistencyExactInexactBoundary is a regression guard ensuring
// that exact and inexact types don't accidentally hash-collide in a way
// that would break the type boundary. Integer(5) is NOT EqualTo Float(5.0)
// in this implementation (exact != inexact).
func TestHashConsistencyExactInexactBoundary(t *testing.T) {
	c := qt.New(t)

	i := NewInteger(5)
	f := NewFloat(5.0)
	c.Assert(i.EqualTo(f), qt.IsFalse,
		qt.Commentf("exact Integer must not equal inexact Float"))
}

// TestHashConsistencySameValueStability ensures the same type and value
// always produces the same hash (regression guard against nondeterminism).
func TestHashConsistencySameValueStability(t *testing.T) {
	c := qt.New(t)

	i1 := NewInteger(7)
	i2 := NewInteger(7)
	c.Assert(i1.HashCode(), qt.Equals, i2.HashCode())

	f1 := NewFloat(2.718)
	f2 := NewFloat(2.718)
	c.Assert(f1.HashCode(), qt.Equals, f2.HashCode())

	r1 := NewRational(3, 7)
	r2 := NewRational(3, 7)
	c.Assert(r1.HashCode(), qt.Equals, r2.HashCode())

	bi1 := NewBigIntegerFromInt64(99)
	bi2 := NewBigIntegerFromInt64(99)
	c.Assert(bi1.HashCode(), qt.Equals, bi2.HashCode())

	bf1 := NewBigFloatFromFloat64(1.23)
	bf2 := NewBigFloatFromFloat64(1.23)
	c.Assert(bf1.HashCode(), qt.Equals, bf2.HashCode())
}

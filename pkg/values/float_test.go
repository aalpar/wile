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
	"errors"
	"math"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

func TestFloat_SchemeString(t *testing.T) {
	tcs := []struct {
		in  values.Value
		out string
	}{
		{
			in:  values.NewFloat(1.1),
			out: "1.1",
		},
		{
			in:  values.NewFloat(1.2),
			out: "1.2",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestFloat_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 values.Value
		in1 values.Value
		out bool
	}{
		{
			in0: values.NewFloat(1.1),
			in1: values.NewFloat(1.1),
			out: true,
		},
		{
			in0: values.NewFloat(1.0),
			in1: values.NewFloat(1.1),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestFloat_Datum(t *testing.T) {
	f := values.NewFloat(3.14)
	qt.Assert(t, f.Value, qt.Equals, 3.14)
}

func TestFloat_String(t *testing.T) {
	f := values.NewFloat(3.14)
	qt.Assert(t, f.String(), qt.Equals, "3.14")
}

func TestFloat_Add(t *testing.T) {
	f1 := values.NewFloat(5.5)
	f2 := values.NewFloat(2.5)
	result := f1.Add(f2)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(8.0))

	f3 := values.NewFloat(0.0)
	result = f1.Add(f3)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(5.5))

	i1 := values.NewInteger(3)
	result = f1.Add(i1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(8.5))

	r1 := values.NewRational(1, 2)
	result = f1.Add(r1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(6.0))

	c1 := values.NewComplex(complex(1, 2))
	result = f1.Add(c1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(6.5, 2)))
}

func TestFloat_Subtract(t *testing.T) {
	f1 := values.NewFloat(10.5)
	f2 := values.NewFloat(2.5)
	result := f1.Subtract(f2)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(8.0))

	f3 := values.NewFloat(0.0)
	result = f1.Subtract(f3)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(10.5))

	i1 := values.NewInteger(3)
	result = f1.Subtract(i1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(7.5))

	r1 := values.NewRational(1, 2)
	result = f1.Subtract(r1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(10.0))

	c1 := values.NewComplex(complex(1, 2))
	result = f1.Subtract(c1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(9.5, -2)))
}

func TestFloat_Multiply(t *testing.T) {
	f1 := values.NewFloat(5.0)
	f2 := values.NewFloat(2.5)
	result := f1.Multiply(f2)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(12.5))

	f3 := values.NewFloat(0.0)
	result = f1.Multiply(f3)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(0.0))

	i1 := values.NewInteger(3)
	result = f1.Multiply(i1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(15.0))

	r1 := values.NewRational(1, 2)
	result = f1.Multiply(r1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(2.5))

	c1 := values.NewComplex(complex(2, 3))
	result = f1.Multiply(c1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(10, 15)))
}

func TestFloat_Divide(t *testing.T) {
	f1 := values.NewFloat(10.0)
	f2 := values.NewFloat(2.0)
	result, err := f1.Divide(f2)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(5.0))

	i1 := values.NewInteger(4)
	result, err = f1.Divide(i1)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(2.5))

	r1 := values.NewRational(1, 2)
	result, err = f1.Divide(r1)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewFloat(20.0))

	// (/ 10.0 2.0+0.0i) => 5.0-0.0i. The imaginary part is NEGATIVE zero: the general
	// formula computes it as (b*c - a*d) = 0.0 - 0.0, and dividing by |z|^2 keeps the
	// sign. Chez and Racket agree — verified against Petite Chez 10.4.1.
	//
	// This asserted complex(5, 0) — a POSITIVE zero — and passed anyway, because
	// Complex.EqualTo compared complex128 with Go's ==, and IEEE-754 says
	// 0.0 == -0.0. The sign was simply invisible to the assertion. Routing equality
	// through values.EqvNumber, which consults SignBit, made it visible; the value
	// itself did not change.
	c1 := values.NewComplex(complex(2, 0))
	result, err = f1.Divide(c1)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "5.0-0.0i")
}

func TestFloat_IsZero(t *testing.T) {
	f1 := values.NewFloat(0.0)
	qt.Assert(t, f1.IsZero(), qt.IsTrue)

	f2 := values.NewFloat(5.5)
	qt.Assert(t, f2.IsZero(), qt.IsFalse)
}

func TestFloat_LessThan(t *testing.T) {
	f1 := values.NewFloat(5.5)
	f2 := values.NewFloat(10.5)
	qt.Assert(t, f1.LessThan(f2), qt.IsTrue)
	qt.Assert(t, f2.LessThan(f1), qt.IsFalse)

	i1 := values.NewInteger(7)
	qt.Assert(t, f1.LessThan(i1), qt.IsTrue)

	r1 := values.NewRational(11, 2)
	qt.Assert(t, f1.LessThan(r1), qt.IsFalse)

	c1 := values.NewComplex(complex(7, 0))
	qt.Assert(t, f1.LessThan(c1), qt.IsTrue)
}

func TestFloat_ToExact(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		input float64
		want  values.Value
	}{
		{
			name:  "integer value",
			input: 5.0,
			want:  values.NewInteger(5),
		},
		{
			name:  "rational value",
			input: 2.5,
			want:  values.NewRational(5, 2),
		},
		{
			name:  "negative integer",
			input: -3.0,
			want:  values.NewInteger(-3),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			f := values.NewFloat(tc.input)
			result, err := f.ToExact()
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestFloat_ToExact_NonFinite(t *testing.T) {
	// Test that ToExact raises an error for infinity and NaN
	// instead of panicking.
	// R7RS: (exact +inf.0) should signal an error.
	tcs := []struct {
		name  string
		input float64
	}{
		{
			name:  "positive infinity",
			input: math.Inf(1),
		},
		{
			name:  "negative infinity",
			input: math.Inf(-1),
		},
		{
			name:  "NaN",
			input: math.NaN(),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			f := values.NewFloat(tc.input)
			_, err := f.ToExact()
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrExactnessConversion), qt.IsTrue)
		})
	}
}

// TestFloat_InfNaNPredicates verifies that IEEE 754 special-value predicates
// are correct and symmetric with BigFloat's behaviour.
func TestFloat_InfNaNPredicates(t *testing.T) {
	c := qt.New(t)
	// Only special values: finite predicate cases are covered by TestFloat_SchemeString.
	tcs := []struct {
		name       string
		v          *values.Float
		isFinite   bool
		isNaN      bool
		isRational bool
		isInteger  bool
		scheme     string
	}{
		{"+inf.0", values.NewFloat(math.Inf(1)), false, false, false, false, "+inf.0"},
		{"-inf.0", values.NewFloat(math.Inf(-1)), false, false, false, false, "-inf.0"},
		{"+nan.0", values.NewFloat(math.NaN()), false, true, false, false, "+nan.0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.v.IsFinite(), qt.Equals, tc.isFinite)
			c.Assert(tc.v.IsNaN(), qt.Equals, tc.isNaN)
			c.Assert(tc.v.IsRational(), qt.Equals, tc.isRational)
			c.Assert(tc.v.IsInteger(), qt.Equals, tc.isInteger)
			c.Assert(tc.v.SchemeString(), qt.Equals, tc.scheme)
		})
	}
}

// TestFloat_NaNEquality pins EqualTo's NaN behavior, which splits along a line
// this test used to blur: EqualTo backs the EQUIVALENCE predicates (eqv?/equal?),
// NOT numeric = (R7RS §6.2.6, which routes through Compare and keeps IEEE-754's
// NaN != NaN — see the assertion at the end).
//
// An equivalence relation must be reflexive, and R7RS §6.1 orders the three by
// coarseness: eq? ⊆ eqv? ⊆ equal?. eqv? settles identity before it inspects the
// value, so it answers #t for a NaN compared with itself — and equal? may not be
// finer than eqv?. This test previously asserted the opposite (nan1.EqualTo(nan1)
// == false), which put equal? BELOW eqv? and made (member x lst) unable to find
// the very object it was handed.
//
// Two DISTINCT NaN objects remain unequal: identity does not hold, and the value
// comparison then applies IEEE-754. That is the case that carries "NaN != NaN".
func TestFloat_NaNEquality(t *testing.T) {
	c := qt.New(t)
	nan1 := values.NewFloat(math.NaN())
	nan2 := values.NewFloat(math.NaN())

	// Reflexive: the same object is equivalent to itself, NaN payload or not.
	c.Assert(nan1.EqualTo(nan1), qt.IsTrue)

	// Distinct objects: no identity, so IEEE-754 decides, and NaN != NaN.
	c.Assert(nan1.EqualTo(nan2), qt.IsFalse)
	c.Assert(nan1.EqualTo(values.NewFloat(0)), qt.IsFalse)
	c.Assert(nan1.EqualTo(values.NewFloat(math.Inf(1))), qt.IsFalse)

	// Numeric = is a DIFFERENT predicate and keeps IEEE-754 even for the same
	// object — reflexivity is a law of equivalence relations, not of =. Pinned at
	// the Scheme level in registry/core (TestEqualIsReflexive), since Float.Compare
	// panics on NaN rather than answering, and so cannot be asserted here.
}

// TestFloat_InfNaNArithmetic verifies IEEE 754 Inf/NaN arithmetic outcomes.
func TestFloat_InfNaNArithmetic(t *testing.T) {
	c := qt.New(t)
	posInf := values.NewFloat(math.Inf(1))
	negInf := values.NewFloat(math.Inf(-1))
	nan := values.NewFloat(math.NaN())
	three := values.NewFloat(3)

	// Inf arithmetic stays non-finite.
	r := posInf.Add(three)
	c.Assert(r.IsFinite(), qt.IsFalse)
	c.Assert(r.IsNaN(), qt.IsFalse)

	r = posInf.Multiply(three)
	c.Assert(r.IsFinite(), qt.IsFalse)

	// Inf + (-Inf) = NaN.
	r = posInf.Add(negInf)
	c.Assert(r.IsNaN(), qt.IsTrue)

	// NaN propagates.
	r = nan.Add(three)
	c.Assert(r.IsNaN(), qt.IsTrue)

	r = three.Add(nan)
	c.Assert(r.IsNaN(), qt.IsTrue)
}

// TestFloat_InfNaNHashConsistency verifies that Float and BigFloat produce
// identical hash codes for the same Inf/NaN value (required by the Hashable contract).
func TestFloat_InfNaNHashConsistency(t *testing.T) {
	c := qt.New(t)
	for _, v := range []float64{math.Inf(1), math.Inf(-1), math.NaN()} {
		f := values.NewFloat(v)
		bf := values.NewBigFloatFromFloat64(v)
		c.Assert(f.HashCode(), qt.Equals, bf.HashCode(),
			qt.Commentf("hash mismatch for %v: Float=%d BigFloat=%d", v, f.HashCode(), bf.HashCode()))
	}
}

// TestFloat_LessThanNaN verifies that NaN comparisons return false (IEEE 754).
func TestFloat_LessThanNaN(t *testing.T) {
	c := qt.New(t)
	nan := values.NewFloat(math.NaN())
	one := values.NewFloat(1)
	c.Assert(nan.LessThan(one), qt.IsFalse)
	c.Assert(one.LessThan(nan), qt.IsFalse)
	c.Assert(nan.LessThan(nan), qt.IsFalse)
}

// TestFloat_EqualTo_NaNVsBigFloat verifies that Float.EqualTo does not panic
// when comparing a NaN Float against a BigFloat (E3). Go's big.Float.SetFloat64(NaN)
// panics; the guard must short-circuit before that conversion.
func TestFloat_EqualTo_NaNVsBigFloat(t *testing.T) {
	tcs := []struct {
		name string
		a    *values.Float
		b    *values.BigFloat
		want bool
	}{
		{"NaN vs finite BigFloat", values.NewFloat(math.NaN()), values.NewBigFloatFromFloat64(1.0), false},
		{"NaN vs BigFloat NaN", values.NewFloat(math.NaN()), values.NewBigFloatNaN(), false},
		{"finite vs BigFloat NaN", values.NewFloat(1.0), values.NewBigFloatNaN(), false},
		// Not equal: different inexact precisions are observable under arithmetic
		// ((+ x 1e-20) tells them apart), so R7RS 6.1's eqv? clause says #f, and
		// equal? must agree. The point of this test is that neither case PANICS.
		{"finite vs equal BigFloat", values.NewFloat(1.0), values.NewBigFloatFromFloat64(1.0), false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Must not panic.
			got := tc.a.EqualTo(tc.b)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

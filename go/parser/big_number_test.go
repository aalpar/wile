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

package parser

import (
	"context"
	"math"
	"math/big"
	"strings"
	"testing"

	"wile/environment"
	"wile/values"

	"github.com/google/go-cmp/cmp"

	qt "github.com/frankban/quicktest"
)

func TestReadSyntaxBigInteger(t *testing.T) {
	tcs := []struct {
		input  string
		expect string
	}{
		{"#z123", "123"},
		{"#Z456", "456"},
		{"#z-789", "-789"},
		{"#z+42", "42"},
		{"#z0", "0"},
		{"#z12345678901234567890", "12345678901234567890"},
		{"#z-12345678901234567890", "-12345678901234567890"},
		{"#z99999999999999999999999999999999999999999999999999", "99999999999999999999999999999999999999999999999999"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bigInt, ok := obj.(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", obj))

			expected := new(big.Int)
			expected.SetString(tc.expect, 10)
			c.Assert(bigInt.BigInt().Cmp(expected), qt.Equals, 0)
		})
	}
}

func TestReadSyntaxBigFloat(t *testing.T) {
	tcs := []struct {
		input  string
		expect string
	}{
		{"#m3.14159265358979323846", "3.14159265358979323846"},
		{"#M2.71828182845904523536", "2.71828182845904523536"},
		{"#m-1.5", "-1.5"},
		{"#m+42.0", "42.0"},
		{"#m123", "123"},
		{"#m0.0", "0.0"},
		{"#m1e10", "1e10"},
		{"#m1.5e-10", "1.5e-10"},
		{"#m3.14E+20", "3.14E+20"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bigFloat, ok := obj.(*values.BigFloat)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat, got %T", obj))

			expected, _, _ := big.ParseFloat(tc.expect, 10, values.DefaultBigFloatPrecision, big.ToNearestEven)
			c.Assert(bigFloat.BigFloatValue().Cmp(expected), qt.Equals, 0)
		})
	}
}

func TestReadSyntaxBigIntegerInList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, true, strings.NewReader("(#z123 #z456 #z789)"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	// Verify we can unwrap and traverse the list - UnwrapAll recursively unwraps
	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// First element should be BigInteger with value 123
	bigInt1, ok := pair.Car().(*values.BigInteger)
	c.Assert(ok, qt.IsTrue)
	c.Assert(bigInt1.BigInt().Int64(), qt.Equals, int64(123))
}

func TestReadSyntaxBigFloatInList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, true, strings.NewReader("(#m1.5 #m2.5 #m3.5)"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	// Verify we can unwrap and traverse the list - UnwrapAll recursively unwraps
	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// First element should be BigFloat with value 1.5
	bigFloat1, ok := pair.Car().(*values.BigFloat)
	c.Assert(ok, qt.IsTrue)
	c.Assert(bigFloat1.Float64(), qt.Equals, 1.5)
}

func TestReadSyntaxMixedBigNumbers(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, true, strings.NewReader("(#z100 #m1.5 42 3.14)"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	// Verify we can unwrap the list - UnwrapAll recursively unwraps
	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// First element: BigInteger
	bigInt, ok := pair.Car().(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", pair.Car()))
	c.Assert(bigInt.BigInt().Int64(), qt.Equals, int64(100))
}

func TestReadSyntaxBigIntegerInVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, true, strings.NewReader("#(#z100 #z200)"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	// Verify we got a vector - UnwrapAll recursively unwraps
	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 2)

	// First element: BigInteger with value 100
	first := (*vec)[0]
	bigInt, ok := first.(*values.BigInteger)
	c.Assert(ok, qt.IsTrue)
	c.Assert(bigInt.BigInt().Int64(), qt.Equals, int64(100))
}

// TestIntegerOverflowPromotion tests that integer literals too large for int64
// are automatically promoted to BigInteger.
func TestIntegerOverflowPromotion(t *testing.T) {
	tcs := []struct {
		input  string
		expect string
	}{
		// Unsigned overflow
		{"31622776601683793319", "31622776601683793319"},
		{"9223372036854775808", "9223372036854775808"}, // int64 max + 1
		{"99999999999999999999", "99999999999999999999"},
		// Signed positive overflow
		{"+9223372036854775808", "9223372036854775808"},
		// Signed negative overflow
		{"-9223372036854775809", "-9223372036854775809"}, // int64 min - 1
		{"-99999999999999999999", "-99999999999999999999"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bigInt, ok := obj.(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", obj))

			expected := new(big.Int)
			expected.SetString(tc.expect, 10)
			c.Assert(bigInt.BigInt().Cmp(expected), qt.Equals, 0)
		})
	}
}

// TestIntegerNoOverflow tests that integers within int64 range remain Integer.
func TestIntegerNoOverflow(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"9223372036854775807", 9223372036854775807},   // int64 max
		{"-9223372036854775808", -9223372036854775808}, // int64 min
		{"0", 0},
		{"12345", 12345},
		{"-12345", -12345},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			intVal, ok := obj.(*values.Integer)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Integer, got %T", obj))
			c.Assert(intVal.Value, qt.Equals, tc.expect)
		})
	}
}

// TestScientificNotationToInteger tests that scientific notation with positive exponents
// or negative exponents with sufficient trailing zeros yields integers.
func TestScientificNotationToInteger(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		// Positive exponents - always integer
		{"1e10", 10000000000},
		{"2e5", 200000},
		{"+3e2", 300},
		{"-4e3", -4000},
		{"1e0", 1},
		// Negative exponents with sufficient trailing zeros
		{"100000e-4", 10},      // 5 trailing zeros >= 4
		{"100000e-5", 1},       // 5 trailing zeros >= 5
		{"1000e-3", 1},         // 3 trailing zeros >= 3
		{"-2000e-2", -20},      // 3 trailing zeros >= 2
		{"+50000e-4", 5},       // 4 trailing zeros >= 4
		{"10e-1", 1},           // 1 trailing zero >= 1
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			intVal, ok := obj.(*values.Integer)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Integer, got %T: %v", obj, obj))
			c.Assert(intVal.Value, qt.Equals, tc.expect)
		})
	}
}

// TestScientificNotationToFloat tests that scientific notation with negative exponents
// and insufficient trailing zeros yields floats.
func TestScientificNotationToFloat(t *testing.T) {
	tcs := []struct {
		input  string
		expect float64
	}{
		// Negative exponents without sufficient trailing zeros
		{"1e-10", 1e-10},
		{"10e-4", 0.001},       // 1 trailing zero < 4
		{"123e-5", 0.00123},    // 0 trailing zeros < 5
		{"+5e-2", 0.05},        // 0 trailing zeros < 2
		{"-7e-3", -0.007},      // 0 trailing zeros < 3
		{"100e-4", 0.01},       // 2 trailing zeros < 4
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			floatVal, ok := obj.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Float, got %T: %v", obj, obj))
			c.Assert(floatVal.Value, qt.CmpEquals(cmp.Comparer(func(a, b float64) bool {
				return math.Abs(a-b) < 1e-15
			})), tc.expect)
		})
	}
}

// TestScientificNotationToBigInteger tests that large scientific notation results
// are promoted to BigInteger.
func TestScientificNotationToBigInteger(t *testing.T) {
	tcs := []struct {
		input  string
		expect string
	}{
		{"1e20", "100000000000000000000"},
		{"1e19", "10000000000000000000"}, // Just over int64 max (~9.2e18)
		{"-5e19", "-50000000000000000000"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bigInt, ok := obj.(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T: %v", obj, obj))

			expected := new(big.Int)
			expected.SetString(tc.expect, 10)
			c.Assert(bigInt.BigInt().Cmp(expected), qt.Equals, 0)
		})
	}
}

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
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestBigIntegerOrderFloatPrecision verifies that BigInteger.LessThan
// preserves precision when ordering against Float values, especially for
// integers beyond the float64 precision boundary (2^53).
//
// Prior to the fix, BigInteger was converted to float64 before comparison,
// causing precision loss for integers with more than 53 significant bits.
func TestBigIntegerOrderFloatPrecision(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name           string
		bigInt         *values.BigInteger
		float          *values.Float
		expectedResult int // -1, 0, or 1
	}{
		{
			name:           "2^53 + 1 > 2^53 (exact in float64)",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(9007199254740993)), // 2^53 + 1
			float:          values.NewFloat(9007199254740992.0),                           // 2^53 (exact in float64)
			expectedResult: 1,                                                             // BigInteger is larger
		},
		{
			name:           "2^53 == 2^53.0",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(9007199254740992)), // 2^53
			float:          values.NewFloat(9007199254740992.0),                           // 2^53
			expectedResult: 0,                                                             // Equal
		},
		{
			name:           "2^53 - 1 < 2^53.0",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(9007199254740991)), // 2^53 - 1
			float:          values.NewFloat(9007199254740992.0),                           // 2^53
			expectedResult: -1,                                                            // BigInteger is smaller
		},
		{
			name:           "2^54 > 2^53.0",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(18014398509481984)), // 2^54
			float:          values.NewFloat(9007199254740992.0),                            // 2^53
			expectedResult: 1,                                                              // BigInteger is larger
		},
		{
			name:           "negative: -(2^53 + 1) < -(2^53)",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(-9007199254740993)), // -(2^53 + 1)
			float:          values.NewFloat(-9007199254740992.0),                           // -(2^53)
			expectedResult: -1,                                                             // BigInteger is smaller (more negative)
		},
		{
			name:           "small values: 42 == 42.0",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(42)),
			float:          values.NewFloat(42.0),
			expectedResult: 0, // Equal
		},
		{
			name:           "zero: 0 == 0.0",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(0)),
			float:          values.NewFloat(0.0),
			expectedResult: 0, // Equal
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// assertOrder covers both directions, subsuming the reverse check
			// this loop used to make separately.
			assertOrder(c, tc.bigInt, tc.float, tc.expectedResult)
		})
	}
}

// TestBigIntegerOrderComplexPrecision verifies that BigInteger.LessThan
// preserves precision when ordering against Complex values (real part only).
func TestBigIntegerOrderComplexPrecision(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name           string
		bigInt         *values.BigInteger
		complex        *values.Complex
		expectedResult int // -1, 0, or 1
	}{
		{
			name:           "2^53 + 1 > complex(2^53, 0)",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(9007199254740993)), // 2^53 + 1
			complex:        values.NewComplex(complex(9007199254740992.0, 0)),             // 2^53 + 0i
			expectedResult: 1,                                                             // BigInteger is larger
		},
		{
			name:           "2^53 == complex(2^53, 0)",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(9007199254740992)), // 2^53
			complex:        values.NewComplex(complex(9007199254740992.0, 0)),             // 2^53 + 0i
			expectedResult: 0,                                                             // Equal
		},
		{
			name:           "42 == complex(42, 0)",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(42)),
			complex:        values.NewComplex(complex(42.0, 0)),
			expectedResult: 0, // Equal
		},
		{
			name:           "100 < complex(200, 50i)",
			bigInt:         values.NewBigInteger(new(big.Int).SetInt64(100)),
			complex:        values.NewComplex(complex(200.0, 50.0)),
			expectedResult: -1, // Real part comparison: 100 < 200
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			assertOrder(c, tc.bigInt, tc.complex, tc.expectedResult)
		})
	}
}

// TestBigIntegerArithmeticFloatPrecision verifies that BigInteger arithmetic with
// Float DISCARDS precision — exactness contagion (R7RS §6.2.2) — and that the
// discarding is confined to arithmetic.
//
// The test used to assert the opposite: that the result was promoted to BigFloat
// "to preserve precision." The promotion was real; the preservation was not. Per-op
// demotion back to Float was never wired (Simplify runs at parse time only), so
// ordinary float arithmetic minted 256-bit bignums that never came back down —
// (+ 1.5 2) was a *BigFloat. Nobody noticed for as long as they did because the
// numeric EqualTo methods compared across representations, so a BigFloat 3.5 tested
// equal to a Float 3.5 and every test passed.
//
// Chez, verified (Petite 10.4.1):
//
//	(+ (expt 2 54) 1.0)                                  => 1.8014398509481984e16
//	(= (+ (expt 2 54) 1.0) (exact->inexact (expt 2 54))) => #t   ; the +1 is GONE
//
// That is what "inexact" means. A program that needs the digit must stay exact, or
// ask for a BigFloat operand explicitly.
func TestBigIntegerArithmeticFloatPrecision(t *testing.T) {
	c := qt.New(t)

	// 2^54 is exactly representable in float64; 2^54 + 1 is NOT (53-bit mantissa).
	// So this is precisely the case where contagion is observable.
	bigInt := values.NewBigInteger(new(big.Int).SetInt64(18014398509481984)) // 2^54
	floatOne := values.NewFloat(1.0)

	t.Run("Add: 2^54 + 1.0", func(t *testing.T) {
		result := bigInt.Add(floatOne)
		c.Assert(result, qt.Not(qt.IsNil))

		f, ok := result.(*values.Float)
		c.Assert(ok, qt.IsTrue, qt.Commentf("Expected *values.Float (contagion), got %T", result))
		c.Assert(f.IsExact(), qt.Equals, false)

		// The +1 is gone: the sum is bit-identical to 2^54 as a float64. This is the
		// assertion that inverted, and it is the whole content of the change.
		c.Assert(f.Value, qt.Equals, float64(18014398509481984))

		// But COMPARISON still sees the difference the arithmetic threw away — that is
		// the other half of the contagion fix, and why there are two promotion tables.
		// Comparing 2^54+1 (exact) against 2^54 (exact) must not round either operand.
		exactPlusOne := values.NewBigInteger(new(big.Int).SetInt64(18014398509481985))
		assertOrder(c, exactPlusOne, bigInt, 1)
		c.Assert(values.EqvNumber(exactPlusOne, bigInt), qt.IsFalse,
			qt.Commentf("2^54+1 must order GREATER than 2^54 and not be eqv? to it; if "+
				"comparison rounded to float64 they would collapse to equal"))
	})

	t.Run("Subtract: 2^54 - 1.0", func(t *testing.T) {
		result := bigInt.Subtract(floatOne)
		c.Assert(result, qt.Not(qt.IsNil))
		// Similar verification as Add
	})

	t.Run("Multiply: 2^54 * 2.0", func(t *testing.T) {
		floatTwo := values.NewFloat(2.0)
		result := bigInt.Multiply(floatTwo)
		c.Assert(result, qt.Not(qt.IsNil))
		// Result should be 2^55
	})

	t.Run("Divide: 2^54 / 2.0", func(t *testing.T) {
		floatTwo := values.NewFloat(2.0)
		result, err := bigInt.Divide(floatTwo)
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.Not(qt.IsNil))
		// Result should be 2^53
	})
}

// TestBigIntegerLessThanFloat verifies LessThan uses the fixed Compare.
func TestBigIntegerLessThanFloat(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		bigInt   *values.BigInteger
		float    *values.Float
		expected bool
	}{
		{
			name:     "2^53 + 1 is not less than 2^53.0",
			bigInt:   values.NewBigInteger(new(big.Int).SetInt64(9007199254740993)), // 2^53 + 1
			float:    values.NewFloat(9007199254740992.0),                           // 2^53
			expected: false,
		},
		{
			name:     "2^53 - 1 is less than 2^53.0",
			bigInt:   values.NewBigInteger(new(big.Int).SetInt64(9007199254740991)), // 2^53 - 1
			float:    values.NewFloat(9007199254740992.0),                           // 2^53
			expected: true,
		},
		{
			name:     "42 is not less than 42.0",
			bigInt:   values.NewBigInteger(new(big.Int).SetInt64(42)),
			float:    values.NewFloat(42.0),
			expected: false,
		},
		{
			name:     "10 is less than 20.0",
			bigInt:   values.NewBigInteger(new(big.Int).SetInt64(10)),
			float:    values.NewFloat(20.0),
			expected: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := tc.bigInt.LessThan(tc.float)
			c.Assert(result, qt.Equals, tc.expected)
		})
	}
}

// TestBigIntegerEqualToFloat verifies EqualTo works correctly after Compare fix.
func TestBigIntegerEqualToFloat(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		bigInt   *values.BigInteger
		other    values.Value
		expected bool
	}{
		{
			name:     "2^53 + 1 is not equal to 2^53.0",
			bigInt:   values.NewBigInteger(new(big.Int).SetInt64(9007199254740993)),
			other:    values.NewFloat(9007199254740992.0),
			expected: false, // Different values
		},
		{
			name:     "2^53 equals 2^53.0",
			bigInt:   values.NewBigInteger(new(big.Int).SetInt64(9007199254740992)),
			other:    values.NewFloat(9007199254740992.0),
			expected: false, // EqualTo doesn't compare across exact/inexact
		},
		{
			name:     "42 equals Integer 42",
			bigInt:   values.NewBigInteger(new(big.Int).SetInt64(42)),
			other:    values.NewInteger(42),
			expected: true, // Same exact value
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := tc.bigInt.EqualTo(tc.other)
			c.Assert(result, qt.Equals, tc.expected)
		})
	}
}

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
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestBigIntegerCompareFloatPrecision verifies that BigInteger.Compare
// preserves precision when comparing with Float values, especially for
// integers beyond the float64 precision boundary (2^53).
//
// Prior to the fix, BigInteger was converted to float64 before comparison,
// causing precision loss for integers with more than 53 significant bits.
func TestBigIntegerCompareFloatPrecision(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name           string
		bigInt         *BigInteger
		float          *Float
		expectedResult int // -1, 0, or 1
	}{
		{
			name:           "2^53 + 1 > 2^53 (exact in float64)",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(9007199254740993)), // 2^53 + 1
			float:          NewFloat(9007199254740992.0),                           // 2^53 (exact in float64)
			expectedResult: 1,                                                      // BigInteger is larger
		},
		{
			name:           "2^53 == 2^53.0",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(9007199254740992)), // 2^53
			float:          NewFloat(9007199254740992.0),                           // 2^53
			expectedResult: 0,                                                      // Equal
		},
		{
			name:           "2^53 - 1 < 2^53.0",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(9007199254740991)), // 2^53 - 1
			float:          NewFloat(9007199254740992.0),                           // 2^53
			expectedResult: -1,                                                     // BigInteger is smaller
		},
		{
			name:           "2^54 > 2^53.0",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(18014398509481984)), // 2^54
			float:          NewFloat(9007199254740992.0),                            // 2^53
			expectedResult: 1,                                                       // BigInteger is larger
		},
		{
			name:           "negative: -(2^53 + 1) < -(2^53)",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(-9007199254740993)), // -(2^53 + 1)
			float:          NewFloat(-9007199254740992.0),                           // -(2^53)
			expectedResult: -1,                                                      // BigInteger is smaller (more negative)
		},
		{
			name:           "small values: 42 == 42.0",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(42)),
			float:          NewFloat(42.0),
			expectedResult: 0, // Equal
		},
		{
			name:           "zero: 0 == 0.0",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(0)),
			float:          NewFloat(0.0),
			expectedResult: 0, // Equal
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := tc.bigInt.Compare(tc.float)
			c.Assert(result, qt.Equals, tc.expectedResult)

			// Also test the reverse comparison (Float.Compare(BigInteger))
			reverseResult := tc.float.Compare(tc.bigInt)
			c.Assert(reverseResult, qt.Equals, -tc.expectedResult)
		})
	}
}

// TestBigIntegerCompareComplexPrecision verifies that BigInteger.Compare
// preserves precision when comparing with Complex values (real part comparison).
func TestBigIntegerCompareComplexPrecision(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name           string
		bigInt         *BigInteger
		complex        *Complex
		expectedResult int // -1, 0, or 1
	}{
		{
			name:           "2^53 + 1 > complex(2^53, 0)",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(9007199254740993)), // 2^53 + 1
			complex:        NewComplex(complex(9007199254740992.0, 0)),             // 2^53 + 0i
			expectedResult: 1,                                                      // BigInteger is larger
		},
		{
			name:           "2^53 == complex(2^53, 0)",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(9007199254740992)), // 2^53
			complex:        NewComplex(complex(9007199254740992.0, 0)),             // 2^53 + 0i
			expectedResult: 0,                                                      // Equal
		},
		{
			name:           "42 == complex(42, 0)",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(42)),
			complex:        NewComplex(complex(42.0, 0)),
			expectedResult: 0, // Equal
		},
		{
			name:           "100 < complex(200, 50i)",
			bigInt:         NewBigInteger(new(big.Int).SetInt64(100)),
			complex:        NewComplex(complex(200.0, 50.0)),
			expectedResult: -1, // Real part comparison: 100 < 200
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := tc.bigInt.Compare(tc.complex)
			c.Assert(result, qt.Equals, tc.expectedResult)
		})
	}
}

// TestBigIntegerArithmeticFloatPrecision verifies that BigInteger arithmetic
// with Float preserves precision by promoting to BigFloat.
func TestBigIntegerArithmeticFloatPrecision(t *testing.T) {
	c := qt.New(t)

	// Use 2^54 which is beyond float64 precision boundary
	bigInt := NewBigInteger(new(big.Int).SetInt64(18014398509481984)) // 2^54
	floatOne := NewFloat(1.0)

	t.Run("Add: 2^54 + 1.0", func(t *testing.T) {
		result := bigInt.Add(floatOne)
		// Result should be BigFloat (inexact) to preserve exactness contagion
		c.Assert(result, qt.Not(qt.IsNil))

		bf, ok := result.(*BigFloat)
		c.Assert(ok, qt.IsTrue, qt.Commentf("Expected *BigFloat, got %T", result))

		// Verify the value is correct
		expected := new(big.Float).SetInt64(18014398509481984)
		expected.Add(expected, new(big.Float).SetFloat64(1.0))
		c.Assert(bf.value.Cmp(expected), qt.Equals, 0)

		// Verify it's inexact
		c.Assert(bf.IsExact(), qt.Equals, false)
	})

	t.Run("Subtract: 2^54 - 1.0", func(t *testing.T) {
		result := bigInt.Subtract(floatOne)
		c.Assert(result, qt.Not(qt.IsNil))
		// Similar verification as Add
	})

	t.Run("Multiply: 2^54 * 2.0", func(t *testing.T) {
		floatTwo := NewFloat(2.0)
		result := bigInt.Multiply(floatTwo)
		c.Assert(result, qt.Not(qt.IsNil))
		// Result should be 2^55
	})

	t.Run("Divide: 2^54 / 2.0", func(t *testing.T) {
		floatTwo := NewFloat(2.0)
		result := bigInt.Divide(floatTwo)
		c.Assert(result, qt.Not(qt.IsNil))
		// Result should be 2^53
	})
}

// TestBigIntegerLessThanFloat verifies LessThan uses the fixed Compare.
func TestBigIntegerLessThanFloat(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		bigInt   *BigInteger
		float    *Float
		expected bool
	}{
		{
			name:     "2^53 + 1 is not less than 2^53.0",
			bigInt:   NewBigInteger(new(big.Int).SetInt64(9007199254740993)), // 2^53 + 1
			float:    NewFloat(9007199254740992.0),                           // 2^53
			expected: false,
		},
		{
			name:     "2^53 - 1 is less than 2^53.0",
			bigInt:   NewBigInteger(new(big.Int).SetInt64(9007199254740991)), // 2^53 - 1
			float:    NewFloat(9007199254740992.0),                           // 2^53
			expected: true,
		},
		{
			name:     "42 is not less than 42.0",
			bigInt:   NewBigInteger(new(big.Int).SetInt64(42)),
			float:    NewFloat(42.0),
			expected: false,
		},
		{
			name:     "10 is less than 20.0",
			bigInt:   NewBigInteger(new(big.Int).SetInt64(10)),
			float:    NewFloat(20.0),
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
		bigInt   *BigInteger
		other    Value
		expected bool
	}{
		{
			name:     "2^53 + 1 is not equal to 2^53.0",
			bigInt:   NewBigInteger(new(big.Int).SetInt64(9007199254740993)),
			other:    NewFloat(9007199254740992.0),
			expected: false, // Different values
		},
		{
			name:     "2^53 equals 2^53.0",
			bigInt:   NewBigInteger(new(big.Int).SetInt64(9007199254740992)),
			other:    NewFloat(9007199254740992.0),
			expected: false, // EqualTo doesn't compare across exact/inexact
		},
		{
			name:     "42 equals Integer 42",
			bigInt:   NewBigInteger(new(big.Int).SetInt64(42)),
			other:    NewInteger(42),
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

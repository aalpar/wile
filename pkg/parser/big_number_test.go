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

package parser

import (
	"context"
	"errors"
	"math"
	"math/big"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

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
			env := environment.NewNamespace().Runtime()
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
			env := environment.NewNamespace().Runtime()
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

// TestIntroducerAcceptsIntertokenSpace decides, rather than inherits, what
// intertoken space means between #z / #m and their datum.
//
// It is a consequence of the introducer model, not a special case: the marker
// and the number are separate tokens, so the whitespace between them is ordinary
// intertoken space. #; and #& behave the same way, and so does every other
// introducer in R7RS §2.1. Only the numeric product (#e#i#b#o#d#x) is glued to
// its operand, and that is because a radix prefix steers a digit *scan*.
//
// Pinned because "#z 5 reads as 5" would otherwise be a side effect nobody chose,
// and the opposite reading — requiring adjacency — is a defensible design that
// this test now rules out.
func TestIntroducerAcceptsIntertokenSpace(t *testing.T) {
	tcs := []struct {
		input string
		big   bool // true: expect BigInteger; false: expect BigFloat
	}{
		{"#z 5", true},
		{"#z\n5", true},
		{"#z  \t 5", true},
		{"#z #x1f", true},
		{"#m 5", false},
		{"#m\n1.5", false},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(tc.input)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			obj := syn.Unwrap()
			if tc.big {
				_, ok := obj.(*values.BigInteger)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T: %v", obj, obj))
				return
			}
			_, ok := obj.(*values.BigFloat)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat, got %T: %v", obj, obj))
		})
	}
}

// TestIntroducerWithoutDatumIsLocated pins the two ways an introducer can be
// left dangling. Both must be located parser errors: a bare marker at end of
// input is malformed input rather than a clean EOF, and a marker before a close
// delimiter must not leak errNoDatum into the enclosing compound reader, which
// would silently read "(#z)" as "()".
//
// Same rule as #e / #i / #d, whose guards this shares (readNumericIntroducer).
func TestIntroducerWithoutDatumIsLocated(t *testing.T) {
	for _, input := range []string{"#z", "#m", "(#z)", "(#m)", "#z)", "#e", "(#e)"} {
		t.Run(input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(input)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil, qt.Commentf("got %v", syn))
			var perr *ParserError
			c.Assert(errors.As(err, &perr), qt.IsTrue,
				qt.Commentf("a dangling introducer must carry a source location, got %T: %v", err, err))
		})
	}
}

// TestBigNumberIntroducerComposition pins the compositions the introducer model
// buys by construction. Each row is a property the third-slot design would have
// had to enumerate an ordering rule for.
func TestBigNumberIntroducerComposition(t *testing.T) {
	tcs := []struct {
		input string
		want  string // SchemeString
	}{
		{"#z#z#z5", "5"},     // coercion, not container: applying it thrice is applying it once
		{"#m#z5", "5.0l0"},   // #m widens the BigInteger #z produced
		{"#m#m1.5", "1.5l0"}, // likewise idempotent; the l marker is how a BigFloat writes
		{"#e#z9", "9"},       // exactness is post-hoc, so it composes the other way too
		{"#z#e#x1f", "31"},
		{"#z#x#e1f", "31"},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(tc.input)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Check(syn.Unwrap().SchemeString(), qt.Equals, tc.want)
		})
	}

	// #z demands an exact integer, so a BigFloat operand is rejected however it
	// was spelled. This is the pair that shows #z and #m are not interchangeable
	// coercions: #m accepts what #z produces, never the reverse.
	for _, bad := range []string{"#z#m5", "#z#m1.5"} {
		t.Run(bad, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(bad)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil, qt.Commentf("got %v", syn))
		})
	}
}

func TestReadSyntaxBigIntegerInList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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
	c.Assert(bigFloat1.Float64Truncated(), qt.Equals, 1.5)
}

func TestReadSyntaxMixedBigNumbers(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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
			env := environment.NewNamespace().Runtime()
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
			env := environment.NewNamespace().Runtime()
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

// TestScientificNotationToFloat tests that all scientific notation produces Float (inexact)
// per R7RS §7.1.1. The exponent marker indicates inexact notation.
func TestScientificNotationToFloat(t *testing.T) {
	tcs := []struct {
		input  string
		expect float64
	}{
		// Positive exponents — still inexact per R7RS
		{"1e10", 1e10},
		{"2e5", 2e5},
		{"+3e2", 3e2},
		{"-4e3", -4e3},
		{"1e0", 1.0},
		// Negative exponents with sufficient trailing zeros
		{"100000e-4", 10.0},
		{"100000e-5", 1.0},
		{"1000e-3", 1.0},
		{"-2000e-2", -20.0},
		{"+50000e-4", 5.0},
		{"10e-1", 1.0},
		// Negative exponents without sufficient trailing zeros
		{"1e-10", 1e-10},
		{"10e-4", 0.001},
		{"123e-5", 0.00123},
		{"+5e-2", 0.05},
		{"-7e-3", -0.007},
		{"100e-4", 0.01},
		// Large values
		{"1e20", 1e20},
		{"1e19", 1e19},
		{"-5e19", -5e19},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
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

// TestScientificNotationExactPrefix tests that #e prefix converts scientific notation
// Float back to exact Integer per R7RS §6.2.6.
func TestScientificNotationExactPrefix(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#e1e2", 100},
		{"#e2e5", 200000},
		{"#e1e0", 1},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
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

// TestScientificNotationBigFloatOverflow tests that scientific notation whose
// magnitude exceeds float64 range promotes to BigFloat rather than failing.
// This makes the reader symmetric with the writer, which renders out-of-range
// bigfloats in scientific notation (e.g. (* 1.0 (expt 10 1000)) -> "1e+1000").
func TestScientificNotationBigFloatOverflow(t *testing.T) {
	tcs := []struct {
		input  string
		expect string
	}{
		{"1e+1000", "1e+1000"},
		{"1e1000", "1e1000"},
		{"1e309", "1e309"},
		{"-2.5e+500", "-2.5e+500"},
		{"1.5e400", "1.5e400"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bigFloat, ok := obj.(*values.BigFloat)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat, got %T: %v", obj, obj))

			expected, _, _ := big.ParseFloat(tc.expect, 10, values.DefaultBigFloatPrecision, big.ToNearestEven)
			c.Assert(bigFloat.BigFloatValue().Cmp(expected), qt.Equals, 0)
		})
	}
}

// TestScientificNotationExactPrefixOverflow tests that the #e exact prefix on
// out-of-range scientific notation yields an exact BigInteger — and that the
// integer is the mathematical 10^1000.
//
// This assertion used to read the other way. It compared against
// exact(BigFloat("1e+1000")) and its comment called that "the exact value of
// that approximation, not the mathematical 10^1000", treating the loss as
// inherent. It was not: #e applies to the number as written (R7RS §7.1.1), so
// the 256-bit BigFloat was never the thing to convert. The test passed for as
// long as it did because it asserted self-consistency with the wrong path
// rather than correctness. See MakeExactFromLiteral.
func TestScientificNotationExactPrefixOverflow(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("#e1e+1000"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	obj := syn.Unwrap()
	bigInt, ok := obj.(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T: %v", obj, obj))

	expected := new(big.Int).Exp(big.NewInt(10), big.NewInt(1000), nil)
	c.Assert(bigInt.BigInt().Cmp(expected), qt.Equals, 0)
}

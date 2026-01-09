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
	"math/big"
	"strings"
	"testing"

	"wile/environment"
	"wile/values"

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

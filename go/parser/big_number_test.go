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
	"math/big"
	"wile/environment"
	"wile/values"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestReadSyntaxBigInteger(t *testing.T) {
	tcs := []struct {
		input  string
		expect string
	}{
		{"#m123", "123"},
		{"#M456", "456"},
		{"#m-789", "-789"},
		{"#m+42", "42"},
		{"#m0", "0"},
		{"#m12345678901234567890", "12345678901234567890"},
		{"#m-12345678901234567890", "-12345678901234567890"},
		{"#m99999999999999999999999999999999999999999999999999", "99999999999999999999999999999999999999999999999999"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(nil)
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
		{"#z3.14159265358979323846", "3.14159265358979323846"},
		{"#Z2.71828182845904523536", "2.71828182845904523536"},
		{"#z-1.5", "-1.5"},
		{"#z+42.0", "42.0"},
		{"#z123", "123"},
		{"#z0.0", "0.0"},
		{"#z1e10", "1e10"},
		{"#z1.5e-10", "1.5e-10"},
		{"#z3.14E+20", "3.14E+20"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(nil)
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
	p := NewParser(env, strings.NewReader("(#m123 #m456 #m789)"))

	syn, err := p.ReadSyntax(nil)
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
	p := NewParser(env, strings.NewReader("(#z1.5 #z2.5 #z3.5)"))

	syn, err := p.ReadSyntax(nil)
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
	p := NewParser(env, strings.NewReader("(#m100 #z1.5 42 3.14)"))

	syn, err := p.ReadSyntax(nil)
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
	p := NewParser(env, strings.NewReader("#(#m100 #m200)"))

	syn, err := p.ReadSyntax(nil)
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

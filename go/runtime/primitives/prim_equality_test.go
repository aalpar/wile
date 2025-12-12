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

package primitives_test

import (
	"testing"

	"wile/runtime/primitives"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestNumericEquality(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "equal integers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "unequal integers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(5), values.NewInteger(3)),
			out:  values.FalseValue,
		},
		{
			name: "zero equals zero",
			prog: values.List(values.NewSymbol("="), values.NewInteger(0), values.NewInteger(0)),
			out:  values.TrueValue,
		},
		{
			name: "negative numbers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(-5), values.NewInteger(-5)),
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestEqQ(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "eq? same symbol",
			prog: values.List(values.NewSymbol("eq?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "eq? different symbols",
			prog: values.List(values.NewSymbol("eq?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("bar"))),
			out: values.FalseValue,
		},
		{
			name: "eq? same integers",
			prog: values.List(values.NewSymbol("eq?"), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "eq? booleans",
			prog: values.List(values.NewSymbol("eq?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestEqualQ(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "equal? same integers",
			prog: values.List(values.NewSymbol("equal?"), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "equal? different integers",
			prog: values.List(values.NewSymbol("equal?"), values.NewInteger(5), values.NewInteger(3)),
			out:  values.FalseValue,
		},
		{
			name: "equal? same booleans",
			prog: values.List(values.NewSymbol("equal?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "equal? different booleans",
			prog: values.List(values.NewSymbol("equal?"), values.TrueValue, values.FalseValue),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestEqQWithDifferentPairs(t *testing.T) {
	// Test eq? with two different pairs that have same contents
	// According to R7RS, eq? should return #f for different objects
	prog := values.List(values.NewSymbol("eq?"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2))),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(3), values.NewInteger(4))))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	// Two different pairs should not be eq?
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestEqualQWithLists(t *testing.T) {
	// Test equal? with two equivalent lists
	prog := values.List(values.NewSymbol("equal?"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	// equal? compares by value, so equivalent lists should be equal?
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEqv(t *testing.T) {
	tcs := []struct {
		name   string
		a      values.Value
		b      values.Value
		expect bool
	}{
		// Same singleton objects
		{
			name:   "true vs true same object",
			a:      values.TrueValue,
			b:      values.TrueValue,
			expect: true,
		},
		{
			name:   "false vs false same object",
			a:      values.FalseValue,
			b:      values.FalseValue,
			expect: true,
		},
		{
			name:   "true vs false",
			a:      values.TrueValue,
			b:      values.FalseValue,
			expect: false,
		},
		{
			name:   "empty list vs empty list",
			a:      values.EmptyList,
			b:      values.EmptyList,
			expect: true,
		},
		// Integer comparisons (different objects with same value)
		{
			name:   "equal integers different objects",
			a:      values.NewInteger(42),
			b:      values.NewInteger(42),
			expect: true,
		},
		{
			name:   "unequal integers",
			a:      values.NewInteger(42),
			b:      values.NewInteger(43),
			expect: false,
		},
		{
			name:   "zero integers",
			a:      values.NewInteger(0),
			b:      values.NewInteger(0),
			expect: true,
		},
		{
			name:   "negative integers equal",
			a:      values.NewInteger(-5),
			b:      values.NewInteger(-5),
			expect: true,
		},
		// Float comparisons
		{
			name:   "equal floats",
			a:      values.NewFloat(3.14),
			b:      values.NewFloat(3.14),
			expect: true,
		},
		{
			name:   "unequal floats",
			a:      values.NewFloat(3.14),
			b:      values.NewFloat(2.71),
			expect: false,
		},
		{
			name:   "zero floats",
			a:      values.NewFloat(0.0),
			b:      values.NewFloat(0.0),
			expect: true,
		},
		// Character comparisons
		{
			name:   "equal characters",
			a:      values.NewCharacter('A'),
			b:      values.NewCharacter('A'),
			expect: true,
		},
		{
			name:   "unequal characters",
			a:      values.NewCharacter('A'),
			b:      values.NewCharacter('B'),
			expect: false,
		},
		{
			name:   "unicode characters equal",
			a:      values.NewCharacter('λ'),
			b:      values.NewCharacter('λ'),
			expect: true,
		},
		// Cross-type comparisons (should always be false)
		{
			name:   "integer vs float",
			a:      values.NewInteger(42),
			b:      values.NewFloat(42.0),
			expect: false,
		},
		{
			name:   "integer vs string",
			a:      values.NewInteger(42),
			b:      values.NewString("42"),
			expect: false,
		},
		{
			name:   "symbol vs string",
			a:      values.NewSymbol("foo"),
			b:      values.NewString("foo"),
			expect: false,
		},
		// Pairs (different objects)
		{
			name:   "different pairs same contents",
			a:      values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			b:      values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			expect: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := primitives.Eqv(tc.a, tc.b)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

// TestEqvWithSamePointer tests eqv with the same pointer values
func TestEqvWithSamePointer(t *testing.T) {
	// When a and b are the same object, eqv should return true
	i := values.NewInteger(42)
	qt.Assert(t, primitives.Eqv(i, i), qt.IsTrue)

	f := values.NewFloat(3.14)
	qt.Assert(t, primitives.Eqv(f, f), qt.IsTrue)

	c := values.NewCharacter('X')
	qt.Assert(t, primitives.Eqv(c, c), qt.IsTrue)

	s := values.NewSymbol("foo")
	qt.Assert(t, primitives.Eqv(s, s), qt.IsTrue)

	str := values.NewString("hello")
	qt.Assert(t, primitives.Eqv(str, str), qt.IsTrue)

	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, primitives.Eqv(pair, pair), qt.IsTrue)
}

// TestEqvWithRational tests eqv with rational numbers
func TestEqvWithRational(t *testing.T) {
	// Same rational value
	r1 := values.NewRational(1, 2)
	r2 := values.NewRational(1, 2)
	qt.Assert(t, primitives.Eqv(r1, r2), qt.IsTrue)

	// Different rational values
	r3 := values.NewRational(1, 3)
	qt.Assert(t, primitives.Eqv(r1, r3), qt.IsFalse)

	// Equivalent rationals (reduced form)
	r4 := values.NewRational(2, 4) // Should reduce to 1/2
	qt.Assert(t, primitives.Eqv(r1, r4), qt.IsTrue)

	// Same object
	qt.Assert(t, primitives.Eqv(r1, r1), qt.IsTrue)
}

// TestEqvWithComplex tests eqv with complex numbers
func TestEqvWithComplex(t *testing.T) {
	// Same complex value
	c1 := values.NewComplex(complex(1, 2))
	c2 := values.NewComplex(complex(1, 2))
	qt.Assert(t, primitives.Eqv(c1, c2), qt.IsTrue)

	// Different complex values
	c3 := values.NewComplex(complex(1, 3))
	qt.Assert(t, primitives.Eqv(c1, c3), qt.IsFalse)

	// Same object
	qt.Assert(t, primitives.Eqv(c1, c1), qt.IsTrue)
}

// TestEqvQPrimitive tests the eqv? primitive through program execution
func TestEqvQPrimitive(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "eqv? same integers",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different integers",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewInteger(43)),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same booleans",
			prog: values.List(values.NewSymbol("eqv?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different booleans",
			prog: values.List(values.NewSymbol("eqv?"), values.TrueValue, values.FalseValue),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same symbols",
			prog: values.List(values.NewSymbol("eqv?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "eqv? different symbols",
			prog: values.List(values.NewSymbol("eqv?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("bar"))),
			out: values.FalseValue,
		},
		{
			name: "eqv? integer vs float (different types)",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewFloat(42.0)),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same characters",
			prog: values.List(values.NewSymbol("eqv?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different characters",
			prog: values.List(values.NewSymbol("eqv?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestAddition(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "add two integers",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2)),
			out:  values.NewInteger(3),
		},
		{
			name: "add three integers",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  values.NewInteger(6),
		},
		{
			name: "add single integer",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
		{
			name: "add no arguments returns 0",
			prog: values.List(values.NewSymbol("+")),
			out:  values.NewInteger(0),
		},
		{
			name: "add negative numbers",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(-5), values.NewInteger(3)),
			out:  values.NewInteger(-2),
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

func TestSubtraction(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "subtract two integers",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(5), values.NewInteger(2)),
			out:  values.NewInteger(3),
		},
		{
			name: "negate single integer",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(5)),
			out:  values.NewInteger(-5),
		},
		{
			name: "subtract multiple integers",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(10), values.NewInteger(3), values.NewInteger(2)),
			out:  values.NewInteger(5),
		},
		{
			name: "subtract negative result",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(1), values.NewInteger(5)),
			out:  values.NewInteger(-4),
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

func TestMultiplication(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "multiply two integers",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(3), values.NewInteger(4)),
			out:  values.NewInteger(12),
		},
		{
			name: "multiply three integers",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4)),
			out:  values.NewInteger(24),
		},
		{
			name: "multiply single integer",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(7)),
			out:  values.NewInteger(7),
		},
		{
			name: "multiply no arguments returns 1",
			prog: values.List(values.NewSymbol("*")),
			out:  values.NewInteger(1),
		},
		{
			name: "multiply by zero",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(5), values.NewInteger(0)),
			out:  values.NewInteger(0),
		},
		{
			name: "multiply negative numbers",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(-3), values.NewInteger(4)),
			out:  values.NewInteger(-12),
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

func TestDivision(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "divide two integers",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(10), values.NewInteger(2)),
			out:  values.NewInteger(5),
		},
		{
			name: "divide multiple integers",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(100), values.NewInteger(5), values.NewInteger(4)),
			out:  values.NewInteger(5),
		},
		{
			name: "divide single integer returns reciprocal",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(5)),
			out:  values.NewRational(1, 5),
		},
		{
			name: "divide single integer 1 returns integer",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(1)),
			out:  values.NewInteger(1),
		},
		{
			name: "divide integers non-evenly returns rational",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(1), values.NewInteger(2)),
			out:  values.NewRational(1, 2),
		},
		{
			name: "divide integers auto-simplifies rational",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(10), values.NewInteger(4)),
			out:  values.NewRational(5, 2),
		},
		{
			name: "divide integers evenly returns integer",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(6), values.NewInteger(3)),
			out:  values.NewInteger(2),
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

func TestAbs(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "abs of positive",
			prog: values.List(values.NewSymbol("abs"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
		{
			name: "abs of negative",
			prog: values.List(values.NewSymbol("abs"), values.NewInteger(-5)),
			out:  values.NewInteger(5),
		},
		{
			name: "abs of zero",
			prog: values.List(values.NewSymbol("abs"), values.NewInteger(0)),
			out:  values.NewInteger(0),
		},
		{
			name: "abs of float",
			prog: values.List(values.NewSymbol("abs"), values.NewFloat(-3.14)),
			out:  values.NewFloat(3.14),
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

func TestFloor(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "floor of positive float",
			prog: values.List(values.NewSymbol("floor"), values.NewFloat(3.7)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "floor of negative float",
			prog: values.List(values.NewSymbol("floor"), values.NewFloat(-3.2)),
			out:  values.NewFloat(-4.0),
		},
		{
			name: "floor of integer",
			prog: values.List(values.NewSymbol("floor"), values.NewInteger(5)),
			out:  values.NewInteger(5),
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

func TestCeiling(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "ceiling of positive float",
			prog: values.List(values.NewSymbol("ceiling"), values.NewFloat(3.2)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "ceiling of negative float",
			prog: values.List(values.NewSymbol("ceiling"), values.NewFloat(-3.7)),
			out:  values.NewFloat(-3.0),
		},
		{
			name: "ceiling of integer",
			prog: values.List(values.NewSymbol("ceiling"), values.NewInteger(5)),
			out:  values.NewInteger(5),
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

func TestRound(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "round down",
			prog: values.List(values.NewSymbol("round"), values.NewFloat(3.2)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "round up",
			prog: values.List(values.NewSymbol("round"), values.NewFloat(3.7)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "round of integer",
			prog: values.List(values.NewSymbol("round"), values.NewInteger(5)),
			out:  values.NewInteger(5),
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

func TestTruncate(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "truncate positive",
			prog: values.List(values.NewSymbol("truncate"), values.NewFloat(3.7)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "truncate negative",
			prog: values.List(values.NewSymbol("truncate"), values.NewFloat(-3.7)),
			out:  values.NewFloat(-3.0),
		},
		{
			name: "truncate of integer",
			prog: values.List(values.NewSymbol("truncate"), values.NewInteger(5)),
			out:  values.NewInteger(5),
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

func TestSqrt(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "sqrt of 4",
			prog: values.List(values.NewSymbol("sqrt"), values.NewInteger(4)),
			out:  values.NewFloat(2.0),
		},
		{
			name: "sqrt of 2",
			prog: values.List(values.NewSymbol("sqrt"), values.NewInteger(2)),
			out:  values.NewFloat(1.4142135623730951),
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

func TestExpt(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "2^3",
			prog: values.List(values.NewSymbol("expt"), values.NewInteger(2), values.NewInteger(3)),
			out:  values.NewInteger(8),
		},
		{
			name: "2^0",
			prog: values.List(values.NewSymbol("expt"), values.NewInteger(2), values.NewInteger(0)),
			out:  values.NewInteger(1),
		},
		{
			name: "10^2",
			prog: values.List(values.NewSymbol("expt"), values.NewInteger(10), values.NewInteger(2)),
			out:  values.NewInteger(100),
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

func TestSquare(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "square of 5",
			prog: values.List(values.NewSymbol("square"), values.NewInteger(5)),
			out:  values.NewInteger(25),
		},
		{
			name: "square of -3",
			prog: values.List(values.NewSymbol("square"), values.NewInteger(-3)),
			out:  values.NewInteger(9),
		},
		{
			name: "square of 0",
			prog: values.List(values.NewSymbol("square"), values.NewInteger(0)),
			out:  values.NewInteger(0),
		},
		{
			name: "square of float",
			prog: values.List(values.NewSymbol("square"), values.NewFloat(2.5)),
			out:  values.NewFloat(6.25),
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

func TestGcd(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "gcd of 12 and 8",
			prog: values.List(values.NewSymbol("gcd"), values.NewInteger(12), values.NewInteger(8)),
			out:  values.NewInteger(4),
		},
		{
			name: "gcd of no args",
			prog: values.List(values.NewSymbol("gcd")),
			out:  values.NewInteger(0),
		},
		{
			name: "gcd of one arg",
			prog: values.List(values.NewSymbol("gcd"), values.NewInteger(5)),
			out:  values.NewInteger(5),
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

func TestLcm(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "lcm of 4 and 6",
			prog: values.List(values.NewSymbol("lcm"), values.NewInteger(4), values.NewInteger(6)),
			out:  values.NewInteger(12),
		},
		{
			name: "lcm of no args",
			prog: values.List(values.NewSymbol("lcm")),
			out:  values.NewInteger(1),
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

func TestQuotient(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "quotient 7/3",
			prog: values.List(values.NewSymbol("quotient"), values.NewInteger(7), values.NewInteger(3)),
			out:  values.NewInteger(2),
		},
		{
			name: "quotient -7/3",
			prog: values.List(values.NewSymbol("quotient"), values.NewInteger(-7), values.NewInteger(3)),
			out:  values.NewInteger(-2),
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

func TestRemainder(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "remainder 7/3",
			prog: values.List(values.NewSymbol("remainder"), values.NewInteger(7), values.NewInteger(3)),
			out:  values.NewInteger(1),
		},
		{
			name: "remainder -7/3",
			prog: values.List(values.NewSymbol("remainder"), values.NewInteger(-7), values.NewInteger(3)),
			out:  values.NewInteger(-1),
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

func TestModulo(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "modulo 7/3",
			prog: values.List(values.NewSymbol("modulo"), values.NewInteger(7), values.NewInteger(3)),
			out:  values.NewInteger(1),
		},
		{
			name: "modulo -7/3",
			prog: values.List(values.NewSymbol("modulo"), values.NewInteger(-7), values.NewInteger(3)),
			out:  values.NewInteger(2),
		},
		{
			name: "modulo 7/-3",
			prog: values.List(values.NewSymbol("modulo"), values.NewInteger(7), values.NewInteger(-3)),
			out:  values.NewInteger(-2),
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

func TestMax(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "max of two",
			prog: values.List(values.NewSymbol("max"), values.NewInteger(3), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
		{
			name: "max of three",
			prog: values.List(values.NewSymbol("max"), values.NewInteger(3), values.NewInteger(5), values.NewInteger(1)),
			out:  values.NewInteger(5),
		},
		{
			name: "max of one",
			prog: values.List(values.NewSymbol("max"), values.NewInteger(7)),
			out:  values.NewInteger(7),
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

func TestMin(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "min of two",
			prog: values.List(values.NewSymbol("min"), values.NewInteger(3), values.NewInteger(5)),
			out:  values.NewInteger(3),
		},
		{
			name: "min of three",
			prog: values.List(values.NewSymbol("min"), values.NewInteger(3), values.NewInteger(5), values.NewInteger(1)),
			out:  values.NewInteger(1),
		},
		{
			name: "min of one",
			prog: values.List(values.NewSymbol("min"), values.NewInteger(7)),
			out:  values.NewInteger(7),
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

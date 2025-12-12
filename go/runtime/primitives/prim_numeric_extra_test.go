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
	"math"
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// Exactness Predicate Tests
// ----------------------------------------------------------------------------

func TestExactnessPredicates(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// exact?
		{
			name: "exact? on integer",
			prog: values.List(values.NewSymbol("exact?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "exact? on float",
			prog: values.List(values.NewSymbol("exact?"), values.NewFloat(3.14)),
			out:  values.FalseValue,
		},
		{
			name: "exact? on rational",
			prog: values.List(values.NewSymbol("exact?"), values.NewRational(1, 2)),
			out:  values.TrueValue,
		},
		// inexact?
		{
			name: "inexact? on float",
			prog: values.List(values.NewSymbol("inexact?"), values.NewFloat(3.14)),
			out:  values.TrueValue,
		},
		{
			name: "inexact? on integer",
			prog: values.List(values.NewSymbol("inexact?"), values.NewInteger(42)),
			out:  values.FalseValue,
		},
		// exact-integer?
		{
			name: "exact-integer? on integer",
			prog: values.List(values.NewSymbol("exact-integer?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "exact-integer? on float",
			prog: values.List(values.NewSymbol("exact-integer?"), values.NewFloat(42.0)),
			out:  values.FalseValue,
		},
		{
			name: "exact-integer? on rational",
			prog: values.List(values.NewSymbol("exact-integer?"), values.NewRational(1, 2)),
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

// ----------------------------------------------------------------------------
// Special Value Predicate Tests
// ----------------------------------------------------------------------------

func TestSpecialValuePredicates(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// finite?
		{
			name: "finite? on normal number",
			prog: values.List(values.NewSymbol("finite?"), values.NewFloat(42.0)),
			out:  values.TrueValue,
		},
		{
			name: "finite? on +inf.0",
			prog: values.List(values.NewSymbol("finite?"), values.NewFloat(math.Inf(1))),
			out:  values.FalseValue,
		},
		{
			name: "finite? on -inf.0",
			prog: values.List(values.NewSymbol("finite?"), values.NewFloat(math.Inf(-1))),
			out:  values.FalseValue,
		},
		{
			name: "finite? on integer",
			prog: values.List(values.NewSymbol("finite?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		// infinite?
		{
			name: "infinite? on +inf.0",
			prog: values.List(values.NewSymbol("infinite?"), values.NewFloat(math.Inf(1))),
			out:  values.TrueValue,
		},
		{
			name: "infinite? on -inf.0",
			prog: values.List(values.NewSymbol("infinite?"), values.NewFloat(math.Inf(-1))),
			out:  values.TrueValue,
		},
		{
			name: "infinite? on normal number",
			prog: values.List(values.NewSymbol("infinite?"), values.NewFloat(42.0)),
			out:  values.FalseValue,
		},
		{
			name: "infinite? on integer",
			prog: values.List(values.NewSymbol("infinite?"), values.NewInteger(42)),
			out:  values.FalseValue,
		},
		// nan?
		{
			name: "nan? on +nan.0",
			prog: values.List(values.NewSymbol("nan?"), values.NewFloat(math.NaN())),
			out:  values.TrueValue,
		},
		{
			name: "nan? on normal number",
			prog: values.List(values.NewSymbol("nan?"), values.NewFloat(42.0)),
			out:  values.FalseValue,
		},
		{
			name: "nan? on integer",
			prog: values.List(values.NewSymbol("nan?"), values.NewInteger(42)),
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

// ----------------------------------------------------------------------------
// Exactness Conversion Tests
// ----------------------------------------------------------------------------

func TestExactnessConversions(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// exact
		{
			name: "exact on inexact float",
			prog: values.List(values.NewSymbol("exact"), values.NewFloat(3.5)),
			out:  values.NewRational(7, 2),
		},
		{
			name: "exact on integer",
			prog: values.List(values.NewSymbol("exact"), values.NewInteger(42)),
			out:  values.NewInteger(42),
		},
		// inexact
		{
			name: "inexact on integer",
			prog: values.List(values.NewSymbol("inexact"), values.NewInteger(42)),
			out:  values.NewFloat(42.0),
		},
		{
			name: "inexact on rational",
			prog: values.List(values.NewSymbol("inexact"), values.NewRational(1, 2)),
			out:  values.NewFloat(0.5),
		},
		{
			name: "inexact on float",
			prog: values.List(values.NewSymbol("inexact"), values.NewFloat(3.14)),
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

// ----------------------------------------------------------------------------
// Exact Integer Sqrt Tests
// ----------------------------------------------------------------------------

func TestExactIntegerSqrt(t *testing.T) {
	tcs := []struct {
		name  string
		prog  values.Value
		out1  values.Value
		out2  values.Value
	}{
		{
			name: "exact-integer-sqrt of 16",
			prog: values.List(values.NewSymbol("exact-integer-sqrt"), values.NewInteger(16)),
			out1: values.NewInteger(4),
			out2: values.NewInteger(0),
		},
		{
			name: "exact-integer-sqrt of 17",
			prog: values.List(values.NewSymbol("exact-integer-sqrt"), values.NewInteger(17)),
			out1: values.NewInteger(4),
			out2: values.NewInteger(1),
		},
		{
			name: "exact-integer-sqrt of 0",
			prog: values.List(values.NewSymbol("exact-integer-sqrt"), values.NewInteger(0)),
			out1: values.NewInteger(0),
			out2: values.NewInteger(0),
		},
		{
			name: "exact-integer-sqrt of 100",
			prog: values.List(values.NewSymbol("exact-integer-sqrt"), values.NewInteger(100)),
			out1: values.NewInteger(10),
			out2: values.NewInteger(0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Wrap in call-with-values to capture multiple return values
			prog := values.List(
				values.NewSymbol("call-with-values"),
				values.List(values.NewSymbol("lambda"), values.EmptyList, tc.prog),
				values.NewSymbol("list"),
			)
			result, err := runProgram(t, prog)
			qt.Assert(t, err, qt.IsNil)
			// Result should be a list of two values
			expected := values.List(tc.out1, tc.out2)
			qt.Assert(t, result, values.SchemeEquals, expected)
		})
	}
}

// ----------------------------------------------------------------------------
// Rationalize Tests
// ----------------------------------------------------------------------------

func TestRationalize(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "rationalize simple - finds 22/7",
			prog: values.List(values.NewSymbol("rationalize"), values.NewFloat(3.14), values.NewFloat(0.01)),
			out:  values.NewFloat(22.0 / 7.0), // rationalize finds simpler 22/7 within tolerance
		},
		{
			name: "rationalize exact inputs",
			prog: values.List(values.NewSymbol("rationalize"), values.NewRational(22, 7), values.NewRational(1, 100)),
			out:  values.NewRational(22, 7),
		},
		{
			name: "rationalize zero tolerance",
			prog: values.List(values.NewSymbol("rationalize"), values.NewInteger(5), values.NewInteger(0)),
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

// ----------------------------------------------------------------------------
// Floor Division Tests
// ----------------------------------------------------------------------------

func TestFloorDivision(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out1 values.Value
		out2 values.Value
	}{
		{
			name: "floor/ positive",
			prog: values.List(values.NewSymbol("floor/"), values.NewInteger(10), values.NewInteger(3)),
			out1: values.NewInteger(3),
			out2: values.NewInteger(1),
		},
		{
			name: "floor/ negative dividend",
			prog: values.List(values.NewSymbol("floor/"), values.NewInteger(-10), values.NewInteger(3)),
			out1: values.NewInteger(-4),
			out2: values.NewInteger(2),
		},
		{
			name: "floor/ negative divisor",
			prog: values.List(values.NewSymbol("floor/"), values.NewInteger(10), values.NewInteger(-3)),
			out1: values.NewInteger(-4),
			out2: values.NewInteger(-2),
		},
		{
			name: "floor/ both negative",
			prog: values.List(values.NewSymbol("floor/"), values.NewInteger(-10), values.NewInteger(-3)),
			out1: values.NewInteger(3),
			out2: values.NewInteger(-1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Wrap in call-with-values to capture multiple return values
			prog := values.List(
				values.NewSymbol("call-with-values"),
				values.List(values.NewSymbol("lambda"), values.EmptyList, tc.prog),
				values.NewSymbol("list"),
			)
			result, err := runProgram(t, prog)
			qt.Assert(t, err, qt.IsNil)
			// Result should be a list of two values
			expected := values.List(tc.out1, tc.out2)
			qt.Assert(t, result, values.SchemeEquals, expected)
		})
	}
}

func TestFloorQuotient(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "floor-quotient positive",
			prog: values.List(values.NewSymbol("floor-quotient"), values.NewInteger(10), values.NewInteger(3)),
			out:  values.NewInteger(3),
		},
		{
			name: "floor-quotient negative dividend",
			prog: values.List(values.NewSymbol("floor-quotient"), values.NewInteger(-10), values.NewInteger(3)),
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

func TestFloorRemainder(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "floor-remainder positive",
			prog: values.List(values.NewSymbol("floor-remainder"), values.NewInteger(10), values.NewInteger(3)),
			out:  values.NewInteger(1),
		},
		{
			name: "floor-remainder negative dividend",
			prog: values.List(values.NewSymbol("floor-remainder"), values.NewInteger(-10), values.NewInteger(3)),
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

// ----------------------------------------------------------------------------
// Truncate Division Tests
// ----------------------------------------------------------------------------

func TestTruncateDivision(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out1 values.Value
		out2 values.Value
	}{
		{
			name: "truncate/ positive",
			prog: values.List(values.NewSymbol("truncate/"), values.NewInteger(10), values.NewInteger(3)),
			out1: values.NewInteger(3),
			out2: values.NewInteger(1),
		},
		{
			name: "truncate/ negative dividend",
			prog: values.List(values.NewSymbol("truncate/"), values.NewInteger(-10), values.NewInteger(3)),
			out1: values.NewInteger(-3),
			out2: values.NewInteger(-1),
		},
		{
			name: "truncate/ negative divisor",
			prog: values.List(values.NewSymbol("truncate/"), values.NewInteger(10), values.NewInteger(-3)),
			out1: values.NewInteger(-3),
			out2: values.NewInteger(1),
		},
		{
			name: "truncate/ both negative",
			prog: values.List(values.NewSymbol("truncate/"), values.NewInteger(-10), values.NewInteger(-3)),
			out1: values.NewInteger(3),
			out2: values.NewInteger(-1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Wrap in call-with-values to capture multiple return values
			prog := values.List(
				values.NewSymbol("call-with-values"),
				values.List(values.NewSymbol("lambda"), values.EmptyList, tc.prog),
				values.NewSymbol("list"),
			)
			result, err := runProgram(t, prog)
			qt.Assert(t, err, qt.IsNil)
			// Result should be a list of two values
			expected := values.List(tc.out1, tc.out2)
			qt.Assert(t, result, values.SchemeEquals, expected)
		})
	}
}

func TestTruncateQuotient(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "truncate-quotient positive",
			prog: values.List(values.NewSymbol("truncate-quotient"), values.NewInteger(10), values.NewInteger(3)),
			out:  values.NewInteger(3),
		},
		{
			name: "truncate-quotient negative dividend",
			prog: values.List(values.NewSymbol("truncate-quotient"), values.NewInteger(-10), values.NewInteger(3)),
			out:  values.NewInteger(-3),
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

func TestTruncateRemainder(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "truncate-remainder positive",
			prog: values.List(values.NewSymbol("truncate-remainder"), values.NewInteger(10), values.NewInteger(3)),
			out:  values.NewInteger(1),
		},
		{
			name: "truncate-remainder negative dividend",
			prog: values.List(values.NewSymbol("truncate-remainder"), values.NewInteger(-10), values.NewInteger(3)),
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

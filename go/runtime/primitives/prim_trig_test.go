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

func withinTolerance(t *testing.T, result, expected values.Value, tolerance float64) bool {
	t.Helper()
	resultFloat, ok1 := result.(*values.Float)
	expectedFloat, ok2 := expected.(*values.Float)
	if !ok1 || !ok2 {
		return false
	}
	diff := math.Abs(resultFloat.Value - expectedFloat.Value)
	return diff < tolerance
}

func TestSin(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "sin(0) = 0",
			prog: values.List(values.NewSymbol("sin"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "sin(π/2) ≈ 1",
			prog:      values.List(values.NewSymbol("sin"), values.NewFloat(math.Pi/2)),
			out:       values.NewFloat(1.0),
			tolerance: 0.0001,
		},
		{
			name:      "sin(π) ≈ 0",
			prog:      values.List(values.NewSymbol("sin"), values.NewFloat(math.Pi)),
			out:       values.NewFloat(0.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestCos(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "cos(0) = 1",
			prog: values.List(values.NewSymbol("cos"), values.NewInteger(0)),
			out:  values.NewFloat(1.0),
		},
		{
			name:      "cos(π) ≈ -1",
			prog:      values.List(values.NewSymbol("cos"), values.NewFloat(math.Pi)),
			out:       values.NewFloat(-1.0),
			tolerance: 0.0001,
		},
		{
			name:      "cos(π/2) ≈ 0",
			prog:      values.List(values.NewSymbol("cos"), values.NewFloat(math.Pi/2)),
			out:       values.NewFloat(0.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestTan(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "tan(0) = 0",
			prog: values.List(values.NewSymbol("tan"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "tan(π/4) ≈ 1",
			prog:      values.List(values.NewSymbol("tan"), values.NewFloat(math.Pi/4)),
			out:       values.NewFloat(1.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestAsin(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "asin(0) = 0",
			prog: values.List(values.NewSymbol("asin"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "asin(1) ≈ π/2",
			prog:      values.List(values.NewSymbol("asin"), values.NewInteger(1)),
			out:       values.NewFloat(math.Pi / 2),
			tolerance: 0.0001,
		},
		{
			name:      "asin(-1) ≈ -π/2",
			prog:      values.List(values.NewSymbol("asin"), values.NewInteger(-1)),
			out:       values.NewFloat(-math.Pi / 2),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestAcos(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "acos(1) = 0",
			prog: values.List(values.NewSymbol("acos"), values.NewInteger(1)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "acos(0) ≈ π/2",
			prog:      values.List(values.NewSymbol("acos"), values.NewInteger(0)),
			out:       values.NewFloat(math.Pi / 2),
			tolerance: 0.0001,
		},
		{
			name:      "acos(-1) ≈ π",
			prog:      values.List(values.NewSymbol("acos"), values.NewInteger(-1)),
			out:       values.NewFloat(math.Pi),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestAtan(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "atan(0) = 0",
			prog: values.List(values.NewSymbol("atan"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "atan(1) ≈ π/4",
			prog:      values.List(values.NewSymbol("atan"), values.NewInteger(1)),
			out:       values.NewFloat(math.Pi / 4),
			tolerance: 0.0001,
		},
		{
			name:      "atan(1, 1) ≈ π/4",
			prog:      values.List(values.NewSymbol("atan"), values.NewInteger(1), values.NewInteger(1)),
			out:       values.NewFloat(math.Pi / 4),
			tolerance: 0.0001,
		},
		{
			name:      "atan(1, 0) ≈ π/2",
			prog:      values.List(values.NewSymbol("atan"), values.NewInteger(1), values.NewInteger(0)),
			out:       values.NewFloat(math.Pi / 2),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestExp(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "exp(0) = 1",
			prog: values.List(values.NewSymbol("exp"), values.NewInteger(0)),
			out:  values.NewFloat(1.0),
		},
		{
			name:      "exp(1) ≈ e",
			prog:      values.List(values.NewSymbol("exp"), values.NewInteger(1)),
			out:       values.NewFloat(math.E),
			tolerance: 0.0001,
		},
		{
			name:      "exp(2) ≈ e^2",
			prog:      values.List(values.NewSymbol("exp"), values.NewInteger(2)),
			out:       values.NewFloat(math.E * math.E),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestLog(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "log(1) = 0",
			prog: values.List(values.NewSymbol("log"), values.NewInteger(1)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "log(e) ≈ 1",
			prog:      values.List(values.NewSymbol("log"), values.NewFloat(math.E)),
			out:       values.NewFloat(1.0),
			tolerance: 0.0001,
		},
		{
			name: "log(8, 2) = 3",
			prog: values.List(values.NewSymbol("log"), values.NewInteger(8), values.NewInteger(2)),
			out:  values.NewFloat(3.0),
		},
		{
			name:      "log(100, 10) = 2",
			prog:      values.List(values.NewSymbol("log"), values.NewInteger(100), values.NewInteger(10)),
			out:       values.NewFloat(2.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

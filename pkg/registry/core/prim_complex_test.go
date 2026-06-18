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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// Complex Number Primitive Tests
// ----------------------------------------------------------------------------

func TestRealPart(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "real-part of complex",
			prog: values.List(values.NewSymbol("real-part"), values.NewComplexFromParts(3.0, 4.0)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "real-part of integer",
			prog: values.List(values.NewSymbol("real-part"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestImagPart(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "imag-part of complex",
			prog: values.List(values.NewSymbol("imag-part"), values.NewComplexFromParts(3.0, 4.0)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "imag-part of integer",
			prog: values.List(values.NewSymbol("imag-part"), values.NewInteger(5)),
			out:  values.NewInteger(0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestMagnitude(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "magnitude of 3+4i",
			prog: values.List(values.NewSymbol("magnitude"), values.NewComplexFromParts(3.0, 4.0)),
			out:  values.NewFloat(5.0),
		},
		{
			name: "magnitude of real number",
			prog: values.List(values.NewSymbol("magnitude"), values.NewInteger(-5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestMakeRectangular(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "make-rectangular from integers",
			prog: values.List(values.NewSymbol("make-rectangular"), values.NewInteger(3), values.NewInteger(4)),
			out:  values.NewComplexFromParts(3.0, 4.0),
		},
		{
			name: "make-rectangular from floats",
			prog: values.List(values.NewSymbol("make-rectangular"), values.NewFloat(1.5), values.NewFloat(2.5)),
			out:  values.NewComplexFromParts(1.5, 2.5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Rational Number Primitive Tests
// ----------------------------------------------------------------------------

func TestNumerator(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "numerator of integer",
			prog: values.List(values.NewSymbol("numerator"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestDenominator(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "denominator of integer",
			prog: values.List(values.NewSymbol("denominator"), values.NewInteger(5)),
			out:  values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestMakeRectangularExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "make-rectangular with integers",
			Code: `(make-rectangular 3 4)`,
		},
		{
			Name: "make-rectangular with floats",
			Code: `(make-rectangular 3.0 4.0)`,
		},
		{
			Name: "make-rectangular with rationals",
			Code: `(make-rectangular 1/2 3/4)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestMagnitudeExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "magnitude of rational",
			Code: `(magnitude 3/4)`,
		},
		{
			Name: "magnitude of negative",
			Code: `(magnitude -5)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

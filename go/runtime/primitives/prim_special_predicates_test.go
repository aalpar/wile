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

// ----------------------------------------------------------------------------
// Special Number Predicate Tests (finite?, infinite?, nan?)
// ----------------------------------------------------------------------------

func TestFiniteQWithRationalAndComplex(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "finite? on rational",
			code: "(finite? 1/2)",
			out:  values.TrueValue,
		},
		{
			name: "finite? on finite complex",
			code: "(finite? 3+4i)",
			out:  values.TrueValue,
		},
		{
			name: "finite? on negative rational",
			code: "(finite? -3/4)",
			out:  values.TrueValue,
		},
		{
			name: "finite? on complex with negative parts",
			code: "(finite? -2-5i)",
			out:  values.TrueValue,
		},
		{
			name: "finite? on complex with real zero",
			code: "(finite? 0+3i)",
			out:  values.TrueValue,
		},
		{
			name: "finite? on complex with imag zero",
			code: "(finite? 5+0i)",
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

func TestInfiniteQWithRationalAndComplex(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "infinite? on rational",
			code: "(infinite? 1/2)",
			out:  values.FalseValue,
		},
		{
			name: "infinite? on complex",
			code: "(infinite? 3+4i)",
			out:  values.FalseValue,
		},
		{
			name: "infinite? on negative rational",
			code: "(infinite? -5/3)",
			out:  values.FalseValue,
		},
		{
			name: "infinite? on complex with negative parts",
			code: "(infinite? -1-2i)",
			out:  values.FalseValue,
		},
		{
			name: "infinite? on complex with large values",
			code: "(infinite? 1000+2000i)",
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

func TestNanQWithRationalAndComplex(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "nan? on rational",
			code: "(nan? 1/2)",
			out:  values.FalseValue,
		},
		{
			name: "nan? on complex",
			code: "(nan? 3+4i)",
			out:  values.FalseValue,
		},
		{
			name: "nan? on negative rational",
			code: "(nan? -7/8)",
			out:  values.FalseValue,
		},
		{
			name: "nan? on complex with negative parts",
			code: "(nan? -6-7i)",
			out:  values.FalseValue,
		},
		{
			name: "nan? on complex with zero parts",
			code: "(nan? 0+0i)",
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Real-Part Tests with Various Number Types
// ----------------------------------------------------------------------------

func TestRealPartWithVariousTypes(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "real-part of complex",
			code: "(real-part 3+4i)",
			out:  values.NewFloat(3.0),
		},
		{
			name: "real-part of integer",
			code: "(real-part 5)",
			out:  values.NewFloat(5.0),
		},
		{
			name: "real-part of float",
			code: "(real-part 2.5)",
			out:  values.NewFloat(2.5),
		},
		{
			name: "real-part of rational",
			code: "(real-part 1/2)",
			out:  values.NewFloat(0.5),
		},
		{
			name: "real-part of negative complex",
			code: "(real-part -3+4i)",
			out:  values.NewFloat(-3.0),
		},
		{
			name: "real-part of complex with negative real",
			code: "(real-part -5-2i)",
			out:  values.NewFloat(-5.0),
		},
		{
			name: "real-part of rational with negative numerator",
			code: "(real-part -3/4)",
			out:  values.NewFloat(-0.75),
		},
		{
			name: "real-part of zero",
			code: "(real-part 0)",
			out:  values.NewFloat(0.0),
		},
		{
			name: "real-part of complex with zero real",
			code: "(real-part 0+5i)",
			out:  values.NewFloat(0.0),
		},
		{
			name: "real-part of purely real complex",
			code: "(real-part 7+0i)",
			out:  values.NewFloat(7.0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

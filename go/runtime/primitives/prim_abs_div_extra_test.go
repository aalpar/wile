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
// Abs Extra Tests
// ----------------------------------------------------------------------------

func TestAbsExtraCoverage(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "abs of negative integer",
			code: "(abs -5)",
			out:  values.NewInteger(5),
		},
		{
			name: "abs of positive integer",
			code: "(abs 5)",
			out:  values.NewInteger(5),
		},
		{
			name: "abs of negative float",
			code: "(abs -3.14)",
			out:  values.NewFloat(3.14),
		},
		{
			name: "abs of negative rational",
			code: "(abs -1/2)",
			out:  values.NewRational(1, 2),
		},
		{
			name: "abs of positive rational",
			code: "(abs 1/2)",
			out:  values.NewRational(1, 2),
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

// ----------------------------------------------------------------------------
// Magnitude Tests (for complex numbers)
// ----------------------------------------------------------------------------

func TestMagnitudeExtraCoverage(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "magnitude of 3+4i",
			code: "(magnitude 3+4i)",
			out:  values.NewFloat(5.0),
		},
		{
			name: "magnitude of real number",
			code: "(magnitude 5)",
			out:  values.NewFloat(5.0),
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

// ----------------------------------------------------------------------------
// Division Extra Tests
// ----------------------------------------------------------------------------

func TestDivisionExtraCoverage(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "divide two integers evenly",
			code: "(/ 10 2)",
			out:  values.NewInteger(5),
		},
		{
			name: "divide integers non-evenly",
			code: "(/ 1 2)",
			out:  values.NewRational(1, 2),
		},
		{
			name: "divide float by integer",
			code: "(/ 10.0 2)",
			out:  values.NewFloat(5.0),
		},
		{
			name: "divide chained",
			code: "(/ 10 2 5)",
			out:  values.NewInteger(1),
		},
		{
			name: "divide single argument reciprocal",
			code: "(/ 5)",
			out:  values.NewRational(1, 5),
		},
		{
			name: "divide complex by real",
			code: "(/ 1+2i 1+0i)",
			out:  values.NewComplexFromParts(1.0, 2.0),
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

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

	qt "github.com/frankban/quicktest"

	"wile/values"
)

func TestMinExtended(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "min of three integers ascending",
			code: "(min 1 2 3)",
			out:  values.NewInteger(1),
		},
		{
			name: "min of three integers descending",
			code: "(min 3 2 1)",
			out:  values.NewInteger(1),
		},
		{
			name: "min of single integer",
			code: "(min 5)",
			out:  values.NewInteger(5),
		},
		{
			name: "min of two floats",
			code: "(min 1.5 2.5)",
			out:  values.NewFloat(1.5),
		},
		{
			name: "min of integer and float",
			code: "(min 1 2.5)",
			out:  values.NewInteger(1),
		},
		{
			name: "min of two rationals",
			code: "(min 1/2 3/4)",
			out:  values.NewRational(1, 2),
		},
		{
			name: "min of negative integers",
			code: "(min -1 -2 -3)",
			out:  values.NewInteger(-3),
		},
		{
			name: "min of mixed positive and negative",
			code: "(min -5 0 5)",
			out:  values.NewInteger(-5),
		},
		{
			name: "min of many integers",
			code: "(min 10 5 8 3 12 1 9)",
			out:  values.NewInteger(1),
		},
		{
			name: "min of negative floats",
			code: "(min -1.5 -2.5 -0.5)",
			out:  values.NewFloat(-2.5),
		},
		{
			name: "min with zero",
			code: "(min 0 1 2)",
			out:  values.NewInteger(0),
		},
		{
			name: "min of float and rational",
			code: "(min 0.4 1/2)",
			out:  values.NewFloat(0.4),
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

func TestMaxExtended(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "max of three integers ascending",
			code: "(max 1 2 3)",
			out:  values.NewInteger(3),
		},
		{
			name: "max of three integers descending",
			code: "(max 3 2 1)",
			out:  values.NewInteger(3),
		},
		{
			name: "max of single integer",
			code: "(max 5)",
			out:  values.NewInteger(5),
		},
		{
			name: "max of two floats",
			code: "(max 1.5 2.5)",
			out:  values.NewFloat(2.5),
		},
		{
			name: "max of integer and float",
			code: "(max 1 2.5)",
			out:  values.NewFloat(2.5),
		},
		{
			name: "max of two rationals",
			code: "(max 1/2 3/4)",
			out:  values.NewRational(3, 4),
		},
		{
			name: "max of negative integers",
			code: "(max -1 -2 -3)",
			out:  values.NewInteger(-1),
		},
		{
			name: "max of mixed positive and negative",
			code: "(max -5 0 5)",
			out:  values.NewInteger(5),
		},
		{
			name: "max of many integers",
			code: "(max 10 5 8 3 12 1 9)",
			out:  values.NewInteger(12),
		},
		{
			name: "max of negative floats",
			code: "(max -1.5 -2.5 -0.5)",
			out:  values.NewFloat(-0.5),
		},
		{
			name: "max with zero",
			code: "(max 0 -1 -2)",
			out:  values.NewInteger(0),
		},
		{
			name: "max of float and rational",
			code: "(max 0.6 1/2)",
			out:  values.NewFloat(0.6),
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

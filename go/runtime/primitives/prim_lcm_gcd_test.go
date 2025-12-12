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

func TestLcmExtended(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "lcm of 12 and 18",
			code: "(lcm 12 18)",
			out:  values.NewInteger(36),
		},
		{
			name: "lcm of three args",
			code: "(lcm 4 6 8)",
			out:  values.NewInteger(24),
		},
		{
			name: "lcm single arg",
			code: "(lcm 5)",
			out:  values.NewInteger(5),
		},
		{
			name: "lcm no args",
			code: "(lcm)",
			out:  values.NewInteger(1),
		},
		{
			name: "lcm with negative",
			code: "(lcm -4 6)",
			out:  values.NewInteger(12),
		},
		{
			name: "lcm with zero",
			code: "(lcm 0 5)",
			out:  values.NewInteger(0),
		},
		{
			name: "lcm of 4 and 6",
			code: "(lcm 4 6)",
			out:  values.NewInteger(12),
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

func TestGcdExtended(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "gcd of 12 and 18",
			code: "(gcd 12 18)",
			out:  values.NewInteger(6),
		},
		{
			name: "gcd of three args",
			code: "(gcd 4 6 8)",
			out:  values.NewInteger(2),
		},
		{
			name: "gcd single arg",
			code: "(gcd 5)",
			out:  values.NewInteger(5),
		},
		{
			name: "gcd no args",
			code: "(gcd)",
			out:  values.NewInteger(0),
		},
		{
			name: "gcd with negative",
			code: "(gcd -4 6)",
			out:  values.NewInteger(2),
		},
		{
			name: "gcd of 12 and 8",
			code: "(gcd 12 8)",
			out:  values.NewInteger(4),
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

func TestNumGe(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: ">= strictly decreasing",
			code: "(>= 3 2 1)",
			out:  values.TrueValue,
		},
		{
			name: ">= all equal",
			code: "(>= 3 3 3)",
			out:  values.TrueValue,
		},
		{
			name: ">= not decreasing",
			code: "(>= 3 2 3)",
			out:  values.FalseValue,
		},
		{
			name: ">= two args greater",
			code: "(>= 2 1)",
			out:  values.TrueValue,
		},
		{
			name: ">= two args less",
			code: "(>= 1 2)",
			out:  values.FalseValue,
		},
		{
			name: ">= two args equal",
			code: "(>= 2 2)",
			out:  values.TrueValue,
		},
		{
			name: ">= floats greater",
			code: "(>= 2.0 1.0)",
			out:  values.TrueValue,
		},
		{
			name: ">= floats equal",
			code: "(>= 1.0 1.0)",
			out:  values.TrueValue,
		},
		{
			name: ">= floats less",
			code: "(>= 1.0 2.0)",
			out:  values.FalseValue,
		},
		{
			name: ">= mixed integer and float",
			code: "(>= 2.0 1)",
			out:  values.TrueValue,
		},
		{
			name: ">= negative numbers",
			code: "(>= -3 -5)",
			out:  values.TrueValue,
		},
		{
			name: ">= non-increasing with equals",
			code: "(>= 3 2 2 1)",
			out:  values.TrueValue,
		},
		{
			name: ">= fails at end",
			code: "(>= 3 2 1 2)",
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

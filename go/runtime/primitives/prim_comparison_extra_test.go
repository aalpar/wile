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

func TestNumLt(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "< two args true",
			code: "(< 1 2)",
			out:  values.TrueValue,
		},
		{
			name: "< two args false",
			code: "(< 2 1)",
			out:  values.FalseValue,
		},
		{
			name: "< three args strictly increasing",
			code: "(< 1 2 3)",
			out:  values.TrueValue,
		},
		{
			name: "< three args not strictly increasing",
			code: "(< 1 3 2)",
			out:  values.FalseValue,
		},
		{
			name: "< equal values",
			code: "(< 1 1)",
			out:  values.FalseValue,
		},
		{
			name: "< floats true",
			code: "(< 1.0 2.0)",
			out:  values.TrueValue,
		},
		{
			name: "< floats false",
			code: "(< 2.0 1.0)",
			out:  values.FalseValue,
		},
		{
			name: "< mixed integer and float",
			code: "(< 1 2.0)",
			out:  values.TrueValue,
		},
		{
			name: "< negative numbers",
			code: "(< -5 -3)",
			out:  values.TrueValue,
		},
		{
			name: "< negative to positive",
			code: "(< -1 1)",
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

func TestNumLe(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "<= two args less",
			code: "(<= 1 2)",
			out:  values.TrueValue,
		},
		{
			name: "<= two args greater",
			code: "(<= 2 1)",
			out:  values.FalseValue,
		},
		{
			name: "<= equal values",
			code: "(<= 1 1)",
			out:  values.TrueValue,
		},
		{
			name: "<= non-decreasing with equals",
			code: "(<= 1 2 2 3)",
			out:  values.TrueValue,
		},
		{
			name: "<= floats less",
			code: "(<= 1.0 2.0)",
			out:  values.TrueValue,
		},
		{
			name: "<= floats equal",
			code: "(<= 1.0 1.0)",
			out:  values.TrueValue,
		},
		{
			name: "<= floats greater",
			code: "(<= 2.0 1.0)",
			out:  values.FalseValue,
		},
		{
			name: "<= mixed integer and float",
			code: "(<= 1 2.0)",
			out:  values.TrueValue,
		},
		{
			name: "<= negative numbers",
			code: "(<= -5 -3)",
			out:  values.TrueValue,
		},
		{
			name: "<= all equal",
			code: "(<= 5 5 5 5)",
			out:  values.TrueValue,
		},
		{
			name: "<= strictly increasing",
			code: "(<= 1 2 3 4)",
			out:  values.TrueValue,
		},
		{
			name: "<= fails at end",
			code: "(<= 1 2 3 2)",
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

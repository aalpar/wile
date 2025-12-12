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

func TestFiniteQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "finite? on integer",
			code: "(finite? 42)",
			out:  values.TrueValue,
		},
		{
			name: "finite? on float",
			code: "(finite? 3.14)",
			out:  values.TrueValue,
		},
		{
			name: "finite? on +inf.0",
			code: "(finite? +inf.0)",
			out:  values.FalseValue,
		},
		{
			name: "finite? on -inf.0",
			code: "(finite? -inf.0)",
			out:  values.FalseValue,
		},
		{
			name: "finite? on +nan.0",
			code: "(finite? +nan.0)",
			out:  values.FalseValue,
		},
		{
			name: "finite? on rational",
			code: "(finite? 1/2)",
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

func TestInfiniteQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "infinite? on integer",
			code: "(infinite? 42)",
			out:  values.FalseValue,
		},
		{
			name: "infinite? on +inf.0",
			code: "(infinite? +inf.0)",
			out:  values.TrueValue,
		},
		{
			name: "infinite? on -inf.0",
			code: "(infinite? -inf.0)",
			out:  values.TrueValue,
		},
		{
			name: "infinite? on +nan.0",
			code: "(infinite? +nan.0)",
			out:  values.FalseValue,
		},
		{
			name: "infinite? on float",
			code: "(infinite? 3.14)",
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

func TestNanQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "nan? on integer",
			code: "(nan? 42)",
			out:  values.FalseValue,
		},
		{
			name: "nan? on +nan.0",
			code: "(nan? +nan.0)",
			out:  values.TrueValue,
		},
		{
			name: "nan? on +inf.0",
			code: "(nan? +inf.0)",
			out:  values.FalseValue,
		},
		{
			name: "nan? on float",
			code: "(nan? 3.14)",
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

func TestNegativeQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "negative? on negative integer",
			code: "(negative? -5)",
			out:  values.TrueValue,
		},
		{
			name: "negative? on positive integer",
			code: "(negative? 5)",
			out:  values.FalseValue,
		},
		{
			name: "negative? on zero",
			code: "(negative? 0)",
			out:  values.FalseValue,
		},
		{
			name: "negative? on negative float",
			code: "(negative? -3.14)",
			out:  values.TrueValue,
		},
		{
			name: "negative? on negative rational",
			code: "(negative? -1/2)",
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

func TestPositiveQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "positive? on positive integer",
			code: "(positive? 5)",
			out:  values.TrueValue,
		},
		{
			name: "positive? on negative integer",
			code: "(positive? -5)",
			out:  values.FalseValue,
		},
		{
			name: "positive? on zero",
			code: "(positive? 0)",
			out:  values.FalseValue,
		},
		{
			name: "positive? on positive float",
			code: "(positive? 3.14)",
			out:  values.TrueValue,
		},
		{
			name: "positive? on positive rational",
			code: "(positive? 1/2)",
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

func TestRationalQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "rational? on integer",
			code: "(rational? 42)",
			out:  values.TrueValue,
		},
		{
			name: "rational? on float",
			code: "(rational? 3.14)",
			out:  values.TrueValue,
		},
		{
			name: "rational? on rational",
			code: "(rational? 1/2)",
			out:  values.TrueValue,
		},
		{
			name: "rational? on +inf.0",
			code: "(rational? +inf.0)",
			out:  values.FalseValue,
		},
		{
			name: "rational? on +nan.0",
			code: "(rational? +nan.0)",
			out:  values.FalseValue,
		},
		{
			name: "rational? on complex",
			code: "(rational? 1+2i)",
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

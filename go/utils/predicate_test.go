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

package utils

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/values"
)

func TestBoolToBoolean(t *testing.T) {
	tcs := []struct {
		name string
		in   bool
		out  *values.Boolean
	}{
		{
			name: "true returns TrueValue",
			in:   true,
			out:  values.TrueValue,
		},
		{
			name: "false returns FalseValue",
			in:   false,
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := BoolToBoolean(tc.in)
			c.Assert(result, qt.Equals, tc.out)
		})
	}
}

func TestValueToBool(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  bool
	}{
		{
			name: "boolean false returns false",
			in:   values.FalseValue,
			out:  false,
		},
		{
			name: "boolean true returns true",
			in:   values.TrueValue,
			out:  true,
		},
		{
			name: "integer returns true (Scheme semantics)",
			in:   values.NewInteger(0),
			out:  true,
		},
		{
			name: "negative integer returns true",
			in:   values.NewInteger(-1),
			out:  true,
		},
		{
			name: "positive integer returns true",
			in:   values.NewInteger(42),
			out:  true,
		},
		{
			name: "empty string returns true",
			in:   values.NewString(""),
			out:  true,
		},
		{
			name: "non-empty string returns true",
			in:   values.NewString("hello"),
			out:  true,
		},
		{
			name: "empty list returns true",
			in:   values.EmptyList,
			out:  true,
		},
		{
			name: "pair returns true",
			in:   values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			out:  true,
		},
		{
			name: "symbol returns true",
			in:   values.NewSymbol("foo"),
			out:  true,
		},
		{
			name: "void returns true",
			in:   values.Void,
			out:  true,
		},
		{
			name: "character returns true",
			in:   values.NewCharacter('a'),
			out:  true,
		},
		{
			name: "float zero returns true",
			in:   values.NewFloat(0.0),
			out:  true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValueToBool(tc.in)
			c.Assert(result, qt.Equals, tc.out)
		})
	}
}

func TestValueToBoolean(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  *values.Boolean
	}{
		{
			name: "boolean false returns FalseValue",
			in:   values.FalseValue,
			out:  values.FalseValue,
		},
		{
			name: "boolean true returns TrueValue",
			in:   values.TrueValue,
			out:  values.TrueValue,
		},
		{
			name: "integer returns TrueValue (Scheme semantics)",
			in:   values.NewInteger(0),
			out:  values.TrueValue,
		},
		{
			name: "string returns TrueValue",
			in:   values.NewString(""),
			out:  values.TrueValue,
		},
		{
			name: "empty list returns TrueValue",
			in:   values.EmptyList,
			out:  values.TrueValue,
		},
		{
			name: "symbol returns TrueValue",
			in:   values.NewSymbol("bar"),
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValueToBoolean(tc.in)
			c.Assert(result, qt.Equals, tc.out)
		})
	}
}

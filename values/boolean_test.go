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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestBoolean_New(t *testing.T) {
	tcs := []struct {
		in  bool
		out values.Value
	}{
		{
			in:  true,
			out: values.NewBoolean(true),
		},
		{
			in:  false,
			out: values.NewBoolean(false),
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			v := values.NewBoolean(tc.in)
			qt.Assert(t, v, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBoolean_SchemeString(t *testing.T) {
	tcs := []struct {
		in  values.Value
		out string
	}{
		{
			in:  values.NewBoolean(true),
			out: "#t",
		},
		{
			in:  values.NewBoolean(false),
			out: "#f",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestBoolean_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 values.Value
		in1 values.Value
		out bool
	}{
		{
			in0: values.NewBoolean(true),
			in1: values.NewBoolean(true),
			out: true,
		},
		{
			in0: values.NewBoolean(true),
			in1: values.NewBoolean(false),
			out: false,
		},
		{
			in0: values.NewBoolean(false),
			in1: values.NewBoolean(false),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestBoolean_Datum(t *testing.T) {
	b := values.NewBoolean(true)
	qt.Assert(t, b.Datum(), qt.Equals, true)

	b2 := values.NewBoolean(false)
	qt.Assert(t, b2.Datum(), qt.Equals, false)
}

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
			result := values.BoolToBoolean(tc.in)
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
			result := values.ValueToBool(tc.in)
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
			result := values.ValueToBoolean(tc.in)
			c.Assert(result, qt.Equals, tc.out)
		})
	}
}

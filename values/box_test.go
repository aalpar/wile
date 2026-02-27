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

func TestBox_SchemeString(t *testing.T) {
	tcs := []struct {
		in  values.Value
		out string
	}{
		{
			in:  values.NewBox(values.TrueValue),
			out: "#&#t",
		},
		{
			in:  values.NewBox(values.FalseValue),
			out: "#&#f",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestBox_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 values.Value
		in1 values.Value
		out bool
	}{
		{
			in0: values.NewBox(values.NewInteger(10)),
			in1: values.NewBox(values.NewInteger(20)),
			out: false,
		},
		{
			in0: values.NewBox(values.NewInteger(10)),
			in1: values.NewBox(values.NewInteger(10)),
			out: true,
		},
		{
			in0: values.NewBox(nil),
			in1: values.NewBox(nil),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestBox_Unbox(t *testing.T) {
	b := values.NewBox(values.NewInteger(42))
	qt.Assert(t, b.Unbox(), valuestest.SchemeEquals, values.NewInteger(42))

	b2 := values.NewBox(values.NewString("test"))
	qt.Assert(t, b2.Unbox(), valuestest.SchemeEquals, values.NewString("test"))
}

func TestBox_Datum(t *testing.T) {
	b := values.NewBox(values.NewInteger(99))
	qt.Assert(t, b.Datum(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestBox_IsVoid(t *testing.T) {
	b := values.NewBox(values.NewInteger(1))
	qt.Assert(t, b.IsVoid(), qt.IsFalse)

	var nilBox *values.Box
	qt.Assert(t, nilBox.IsVoid(), qt.IsTrue)
}

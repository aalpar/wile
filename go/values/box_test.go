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

package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestBox_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewBox(NewBoolean(true)),
			out: "#&#t",
		},
		{
			in:  NewBox(NewBoolean(false)),
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
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewBox(NewInteger(10)),
			in1: NewBox(NewInteger(20)),
			out: false,
		},
		{
			in0: NewBox(NewInteger(10)),
			in1: NewBox(NewInteger(10)),
			out: true,
		},
		{
			in0: NewBox(nil),
			in1: NewBox(nil),
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
	b := NewBox(NewInteger(42))
	qt.Assert(t, b.Unbox(), SchemeEquals, NewInteger(42))

	b2 := NewBox(NewString("test"))
	qt.Assert(t, b2.Unbox(), SchemeEquals, NewString("test"))
}

func TestBox_Datum(t *testing.T) {
	b := NewBox(NewInteger(99))
	qt.Assert(t, b.Datum(), SchemeEquals, NewInteger(99))
}

func TestBox_IsVoid(t *testing.T) {
	b := NewBox(NewInteger(1))
	qt.Assert(t, b.IsVoid(), qt.IsFalse)

	var nilBox *Box
	qt.Assert(t, nilBox.IsVoid(), qt.IsTrue)
}

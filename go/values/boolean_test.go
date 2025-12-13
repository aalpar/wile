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

func TestBoolean_New(t *testing.T) {
	tcs := []struct {
		in  bool
		out Value
	}{
		{
			in:  true,
			out: NewBoolean(true),
		},
		{
			in:  false,
			out: NewBoolean(false),
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			v := NewBoolean(tc.in)
			qt.Assert(t, v, SchemeEquals, tc.out)
		})
	}
}

func TestBoolean_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewBoolean(true),
			out: "#t",
		},
		{
			in:  NewBoolean(false),
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
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewBoolean(true),
			in1: NewBoolean(true),
			out: true,
		},
		{
			in0: NewBoolean(true),
			in1: NewBoolean(false),
			out: false,
		},
		{
			in0: NewBoolean(false),
			in1: NewBoolean(false),
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
	b := NewBoolean(true)
	qt.Assert(t, b.Datum(), qt.Equals, true)

	b2 := NewBoolean(false)
	qt.Assert(t, b2.Datum(), qt.Equals, false)
}

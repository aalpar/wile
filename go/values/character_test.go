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
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestCharacter_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewCharacter('='),
			out: `#\=`,
		},
		{
			in:  NewCharacter('>'),
			out: `#\>`,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestCharacter_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewCharacter('='),
			in1: NewCharacter('='),
			out: true,
		},
		{
			in0: NewCharacter('='),
			in1: NewCharacter('>'),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestCharacter_Datum(t *testing.T) {
	c := NewCharacter('a')
	qt.Assert(t, c.Datum(), qt.Equals, 'a')
}

func TestCharacter_IsVoid(t *testing.T) {
	c := NewCharacter('x')
	qt.Assert(t, c.IsVoid(), qt.IsFalse)

	var nilChar *Character
	qt.Assert(t, nilChar.IsVoid(), qt.IsTrue)
}

func TestCharacter_String(t *testing.T) {
	c := NewCharacter('z')
	qt.Assert(t, c.String(), qt.Equals, "z")
}

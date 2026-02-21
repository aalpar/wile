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
)

func TestCharacter_SchemeString(t *testing.T) {
	tcs := []struct {
		in  values.Value
		out string
	}{
		{
			in:  values.NewCharacter('='),
			out: `#\=`,
		},
		{
			in:  values.NewCharacter('>'),
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
		in0 values.Value
		in1 values.Value
		out bool
	}{
		{
			in0: values.NewCharacter('='),
			in1: values.NewCharacter('='),
			out: true,
		},
		{
			in0: values.NewCharacter('='),
			in1: values.NewCharacter('>'),
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
	c := values.NewCharacter('a')
	qt.Assert(t, c.Datum(), qt.Equals, 'a')
}

func TestCharacter_IsVoid(t *testing.T) {
	c := values.NewCharacter('x')
	qt.Assert(t, c.IsVoid(), qt.IsFalse)

	var nilChar *values.Character
	qt.Assert(t, nilChar.IsVoid(), qt.IsTrue)
}

func TestCharacter_String(t *testing.T) {
	c := values.NewCharacter('z')
	qt.Assert(t, c.String(), qt.Equals, "z")
}

func TestCharacter_CacheIdentity(t *testing.T) {
	tcs := []struct {
		name    string
		r       rune
		samePtr bool
	}{
		{"ASCII low", 0, true},
		{"ASCII letter", 'a', true},
		{"ASCII boundary", 127, true},
		{"above cache", 128, false},
		{"unicode lambda", 'λ', false},
		{"unicode CJK", '日', false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			a := values.NewCharacter(tc.r)
			b := values.NewCharacter(tc.r)
			if tc.samePtr {
				qt.Assert(t, a, qt.Equals, b)
			} else {
				qt.Assert(t, a != b, qt.IsTrue)
			}
			qt.Assert(t, a.Datum(), qt.Equals, tc.r)
		})
	}
}

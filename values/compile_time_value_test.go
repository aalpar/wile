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

func TestCompileTimeValue_NewCompileTimeValue(t *testing.T) {
	inner := values.NewInteger(42)
	ctv := values.NewCompileTimeValue(inner)
	qt.Assert(t, ctv.Value, valuestest.SchemeEquals, inner)
}

func TestCompileTimeValue_Unwrap(t *testing.T) {
	inner := values.NewString("hello")
	ctv := values.NewCompileTimeValue(inner)
	qt.Assert(t, ctv.Unwrap(), valuestest.SchemeEquals, inner)
}

func TestCompileTimeValue_IsVoid(t *testing.T) {
	ctv := values.NewCompileTimeValue(values.NewInteger(1))
	qt.Assert(t, ctv.IsVoid(), qt.IsFalse)

	var nilCTV *values.CompileTimeValue
	qt.Assert(t, nilCTV.IsVoid(), qt.IsTrue)
}

func TestCompileTimeValue_EqualTo(t *testing.T) {
	tcs := []struct {
		name string
		a    values.Value
		b    values.Value
		out  bool
	}{
		{
			name: "same object",
			a:    values.NewCompileTimeValue(values.NewInteger(1)),
			b:    nil, // will be set to a
			out:  true,
		},
		{
			name: "equal wrapped values",
			a:    values.NewCompileTimeValue(values.NewInteger(42)),
			b:    values.NewCompileTimeValue(values.NewInteger(42)),
			out:  true,
		},
		{
			name: "different wrapped values",
			a:    values.NewCompileTimeValue(values.NewInteger(1)),
			b:    values.NewCompileTimeValue(values.NewInteger(2)),
			out:  false,
		},
		{
			name: "type mismatch",
			a:    values.NewCompileTimeValue(values.NewInteger(1)),
			b:    values.NewInteger(1),
			out:  false,
		},
		{
			name: "both nil wrapped",
			a:    values.NewCompileTimeValue(nil),
			b:    values.NewCompileTimeValue(nil),
			out:  true,
		},
		{
			name: "one nil wrapped",
			a:    values.NewCompileTimeValue(nil),
			b:    values.NewCompileTimeValue(values.NewInteger(1)),
			out:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.name == "same object" {
				tc.b = tc.a
			}
			qt.Assert(t, tc.a.EqualTo(tc.b), qt.Equals, tc.out)
		})
	}
}

func TestCompileTimeValue_SchemeString(t *testing.T) {
	ctv := values.NewCompileTimeValue(values.NewInteger(42))
	qt.Assert(t, ctv.SchemeString(), qt.Equals, "#<compile-time-value 42>")

	ctv2 := values.NewCompileTimeValue(values.NewString("hello"))
	qt.Assert(t, ctv2.SchemeString(), qt.Equals, "#<compile-time-value \"hello\">")
}

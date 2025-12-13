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

func TestFloat_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewFloat(1.1),
			out: "1.1",
		},
		{
			in:  NewFloat(1.2),
			out: "1.2",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestFloat_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewFloat(1.1),
			in1: NewFloat(1.1),
			out: true,
		},
		{
			in0: NewFloat(1.0),
			in1: NewFloat(1.1),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestFloat_Datum(t *testing.T) {
	f := NewFloat(3.14)
	qt.Assert(t, f.Datum(), qt.Equals, 3.14)
}

func TestFloat_String(t *testing.T) {
	f := NewFloat(3.14)
	qt.Assert(t, f.String(), qt.Equals, "3.14")
}

func TestFloat_Add(t *testing.T) {
	f1 := NewFloat(5.5)
	f2 := NewFloat(2.5)
	result := f1.Add(f2)
	qt.Assert(t, result, SchemeEquals, NewFloat(8.0))

	f3 := NewFloat(0.0)
	result = f1.Add(f3)
	qt.Assert(t, result, SchemeEquals, NewFloat(5.5))

	i1 := NewInteger(3)
	result = f1.Add(i1)
	qt.Assert(t, result, SchemeEquals, NewFloat(8.5))

	r1 := NewRational(1, 2)
	result = f1.Add(r1)
	qt.Assert(t, result, SchemeEquals, NewFloat(6.0))

	c1 := NewComplex(complex(1, 2))
	result = f1.Add(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(6.5, 2)))
}

func TestFloat_Subtract(t *testing.T) {
	f1 := NewFloat(10.5)
	f2 := NewFloat(2.5)
	result := f1.Subtract(f2)
	qt.Assert(t, result, SchemeEquals, NewFloat(8.0))

	f3 := NewFloat(0.0)
	result = f1.Subtract(f3)
	qt.Assert(t, result, SchemeEquals, NewFloat(10.5))

	i1 := NewInteger(3)
	result = f1.Subtract(i1)
	qt.Assert(t, result, SchemeEquals, NewFloat(7.5))

	r1 := NewRational(1, 2)
	result = f1.Subtract(r1)
	qt.Assert(t, result, SchemeEquals, NewFloat(10.0))

	c1 := NewComplex(complex(1, 2))
	result = f1.Subtract(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(9.5, -2)))
}

func TestFloat_Multiply(t *testing.T) {
	f1 := NewFloat(5.0)
	f2 := NewFloat(2.5)
	result := f1.Multiply(f2)
	qt.Assert(t, result, SchemeEquals, NewFloat(12.5))

	f3 := NewFloat(0.0)
	result = f1.Multiply(f3)
	qt.Assert(t, result, SchemeEquals, NewFloat(0.0))

	i1 := NewInteger(3)
	result = f1.Multiply(i1)
	qt.Assert(t, result, SchemeEquals, NewFloat(15.0))

	r1 := NewRational(1, 2)
	result = f1.Multiply(r1)
	qt.Assert(t, result, SchemeEquals, NewFloat(2.5))

	c1 := NewComplex(complex(2, 3))
	result = f1.Multiply(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(10, 15)))
}

func TestFloat_Divide(t *testing.T) {
	f1 := NewFloat(10.0)
	f2 := NewFloat(2.0)
	result := f1.Divide(f2)
	qt.Assert(t, result, SchemeEquals, NewFloat(5.0))

	i1 := NewInteger(4)
	result = f1.Divide(i1)
	qt.Assert(t, result, SchemeEquals, NewFloat(2.5))

	r1 := NewRational(1, 2)
	result = f1.Divide(r1)
	qt.Assert(t, result, SchemeEquals, NewFloat(20.0))

	c1 := NewComplex(complex(2, 0))
	result = f1.Divide(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(5, 0)))
}

func TestFloat_IsZero(t *testing.T) {
	f1 := NewFloat(0.0)
	qt.Assert(t, f1.IsZero(), qt.IsTrue)

	f2 := NewFloat(5.5)
	qt.Assert(t, f2.IsZero(), qt.IsFalse)
}

func TestFloat_LessThan(t *testing.T) {
	f1 := NewFloat(5.5)
	f2 := NewFloat(10.5)
	qt.Assert(t, f1.LessThan(f2), qt.IsTrue)
	qt.Assert(t, f2.LessThan(f1), qt.IsFalse)

	i1 := NewInteger(7)
	qt.Assert(t, f1.LessThan(i1), qt.IsTrue)

	r1 := NewRational(11, 2)
	qt.Assert(t, f1.LessThan(r1), qt.IsFalse)

	c1 := NewComplex(complex(7, 0))
	qt.Assert(t, f1.LessThan(c1), qt.IsTrue)
}

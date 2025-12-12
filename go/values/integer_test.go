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

func TestInteger_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewInteger(10),
			in1: NewInteger(10),
			out: true,
		},
		{
			in0: NewInteger(10),
			in1: NewInteger(11),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestInteger_Datum(t *testing.T) {
	i := NewInteger(42)
	qt.Assert(t, i.Datum(), qt.Equals, int64(42))
}

func TestInteger_Add(t *testing.T) {
	i1 := NewInteger(5)
	i2 := NewInteger(3)
	result := i1.Add(i2)
	qt.Assert(t, result, SchemeEquals, NewInteger(8))

	i3 := NewInteger(0)
	result = i1.Add(i3)
	qt.Assert(t, result, SchemeEquals, NewInteger(5))

	f1 := NewFloat(2.5)
	result = i1.Add(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(7.5))

	r1 := NewRational(1, 2)
	result = i1.Add(r1)
	qt.Assert(t, result, SchemeEquals, NewRational(11, 2))

	c1 := NewComplex(complex(1, 2))
	result = i1.Add(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(6, 2)))
}

func TestInteger_Subtract(t *testing.T) {
	i1 := NewInteger(10)
	i2 := NewInteger(3)
	result := i1.Subtract(i2)
	qt.Assert(t, result, SchemeEquals, NewInteger(7))

	i3 := NewInteger(0)
	result = i1.Subtract(i3)
	qt.Assert(t, result, SchemeEquals, NewInteger(10))

	f1 := NewFloat(2.5)
	result = i1.Subtract(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(7.5))

	r1 := NewRational(1, 2)
	result = i1.Subtract(r1)
	qt.Assert(t, result, SchemeEquals, NewRational(19, 2))

	c1 := NewComplex(complex(1, 2))
	result = i1.Subtract(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(9, -2)))
}

func TestInteger_Multiply(t *testing.T) {
	i1 := NewInteger(5)
	i2 := NewInteger(3)
	result := i1.Multiply(i2)
	qt.Assert(t, result, SchemeEquals, NewInteger(15))

	i3 := NewInteger(0)
	result = i1.Multiply(i3)
	qt.Assert(t, result, SchemeEquals, NewInteger(0))

	f1 := NewFloat(2.5)
	result = i1.Multiply(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(12.5))

	r1 := NewRational(1, 2)
	result = i1.Multiply(r1)
	qt.Assert(t, result, SchemeEquals, NewRational(5, 2))

	c1 := NewComplex(complex(2, 3))
	result = i1.Multiply(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(10, 15)))
}

func TestInteger_Divide(t *testing.T) {
	i1 := NewInteger(10)
	i2 := NewInteger(2)
	result := i1.Divide(i2)
	qt.Assert(t, result, SchemeEquals, NewInteger(5))

	i3 := NewInteger(3)
	result = i1.Divide(i3)
	qt.Assert(t, result, SchemeEquals, NewRational(10, 3))

	f1 := NewFloat(2.0)
	result = i1.Divide(f1)
	qt.Assert(t, result, SchemeEquals, NewFloat(5.0))

	r1 := NewRational(1, 2)
	result = i1.Divide(r1)
	qt.Assert(t, result, SchemeEquals, NewRational(20, 1))

	c1 := NewComplex(complex(2, 0))
	result = i1.Divide(c1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(5, 0)))
}

func TestInteger_IsZero(t *testing.T) {
	i1 := NewInteger(0)
	qt.Assert(t, i1.IsZero(), qt.IsTrue)

	i2 := NewInteger(5)
	qt.Assert(t, i2.IsZero(), qt.IsFalse)
}

func TestInteger_LessThan(t *testing.T) {
	i1 := NewInteger(5)
	i2 := NewInteger(10)
	qt.Assert(t, i1.LessThan(i2), qt.IsTrue)
	qt.Assert(t, i2.LessThan(i1), qt.IsFalse)

	f1 := NewFloat(7.5)
	qt.Assert(t, i1.LessThan(f1), qt.IsTrue)

	r1 := NewRational(11, 2)
	qt.Assert(t, i1.LessThan(r1), qt.IsTrue)

	c1 := NewComplex(complex(7, 0))
	qt.Assert(t, i1.LessThan(c1), qt.IsTrue)
}

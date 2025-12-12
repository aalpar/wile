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
	"math"
	"testing"
)

func TestComplex_NewComplex(t *testing.T) {
	c := NewComplex(complex(3, 4))
	qt.Assert(t, c.Real(), qt.Equals, 3.0)
	qt.Assert(t, c.Imag(), qt.Equals, 4.0)
}

func TestComplex_NewComplexFromParts(t *testing.T) {
	c := NewComplexFromParts(3, 4)
	qt.Assert(t, c.Real(), qt.Equals, 3.0)
	qt.Assert(t, c.Imag(), qt.Equals, 4.0)
}

func TestComplex_Datum(t *testing.T) {
	c := NewComplex(complex(3, 4))
	qt.Assert(t, c.Datum(), qt.Equals, complex(3, 4))
}

func TestComplex_Real(t *testing.T) {
	c := NewComplex(complex(3, 4))
	qt.Assert(t, c.Real(), qt.Equals, 3.0)
}

func TestComplex_Imag(t *testing.T) {
	c := NewComplex(complex(3, 4))
	qt.Assert(t, c.Imag(), qt.Equals, 4.0)
}

func TestComplex_Add(t *testing.T) {
	c1 := NewComplex(complex(1, 2))
	c2 := NewComplex(complex(3, 4))
	result := c1.Add(c2)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(4, 6)))

	c3 := NewComplex(complex(0, 0))
	result = c1.Add(c3)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(1, 2)))

	i1 := NewInteger(5)
	result = c1.Add(i1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(6, 2)))

	f1 := NewFloat(2.5)
	result = c1.Add(f1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(3.5, 2)))

	r1 := NewRational(1, 2)
	result = c1.Add(r1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(1.5, 2)))
}

func TestComplex_Subtract(t *testing.T) {
	c1 := NewComplex(complex(5, 6))
	c2 := NewComplex(complex(2, 3))
	result := c1.Subtract(c2)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(3, 3)))

	c3 := NewComplex(complex(0, 0))
	result = c1.Subtract(c3)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(5, 6)))

	i1 := NewInteger(2)
	result = c1.Subtract(i1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(3, 6)))

	f1 := NewFloat(1.0)
	result = c1.Subtract(f1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(4, 6)))

	r1 := NewRational(1, 2)
	result = c1.Subtract(r1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(4.5, 6)))
}

func TestComplex_Multiply(t *testing.T) {
	c1 := NewComplex(complex(2, 3))
	c2 := NewComplex(complex(1, 2))
	result := c1.Multiply(c2)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(-4, 7)))

	c3 := NewComplex(complex(0, 0))
	result = c1.Multiply(c3)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(0, 0)))

	i1 := NewInteger(3)
	result = c1.Multiply(i1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(6, 9)))

	f1 := NewFloat(2.0)
	result = c1.Multiply(f1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(4, 6)))

	r1 := NewRational(1, 2)
	result = c1.Multiply(r1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(1, 1.5)))
}

func TestComplex_Divide(t *testing.T) {
	c1 := NewComplex(complex(4, 2))
	c2 := NewComplex(complex(2, 0))
	result := c1.Divide(c2)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(2, 1)))

	i1 := NewInteger(2)
	result = c1.Divide(i1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(2, 1)))

	f1 := NewFloat(2.0)
	result = c1.Divide(f1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(2, 1)))

	r1 := NewRational(1, 2)
	result = c1.Divide(r1)
	qt.Assert(t, result, SchemeEquals, NewComplex(complex(8, 4)))
}

func TestComplex_IsZero(t *testing.T) {
	c1 := NewComplex(complex(0, 0))
	qt.Assert(t, c1.IsZero(), qt.IsTrue)

	c2 := NewComplex(complex(1, 2))
	qt.Assert(t, c2.IsZero(), qt.IsFalse)

	c3 := NewComplex(complex(0, 1))
	qt.Assert(t, c3.IsZero(), qt.IsFalse)
}

func TestComplex_LessThan(t *testing.T) {
	c1 := NewComplex(complex(3, 4))
	c2 := NewComplex(complex(5, 6))
	qt.Assert(t, c1.LessThan(c2), qt.IsTrue)
	qt.Assert(t, c2.LessThan(c1), qt.IsFalse)

	i1 := NewInteger(5)
	qt.Assert(t, c1.LessThan(i1), qt.IsTrue)

	f1 := NewFloat(4.0)
	qt.Assert(t, c1.LessThan(f1), qt.IsTrue)

	r1 := NewRational(7, 2)
	qt.Assert(t, c1.LessThan(r1), qt.IsTrue)
}

func TestComplex_IsReal(t *testing.T) {
	c1 := NewComplex(complex(5, 0))
	qt.Assert(t, c1.IsReal(), qt.IsTrue)

	c2 := NewComplex(complex(5, 1))
	qt.Assert(t, c2.IsReal(), qt.IsFalse)
}

func TestComplex_Magnitude(t *testing.T) {
	c := NewComplex(complex(3, 4))
	qt.Assert(t, c.Magnitude(), qt.Equals, 5.0)
}

func TestComplex_Phase(t *testing.T) {
	c := NewComplex(complex(1, 1))
	expected := math.Pi / 4
	qt.Assert(t, math.Abs(c.Phase()-expected) < 0.0001, qt.IsTrue)
}

func TestComplex_IsVoid(t *testing.T) {
	c := NewComplex(complex(1, 2))
	qt.Assert(t, c.IsVoid(), qt.IsFalse)

	var nilComplex *Complex
	qt.Assert(t, nilComplex.IsVoid(), qt.IsTrue)
}

func TestComplex_EqualTo(t *testing.T) {
	c1 := NewComplex(complex(1, 2))
	c2 := NewComplex(complex(1, 2))
	qt.Assert(t, c1.EqualTo(c2), qt.IsTrue)

	c3 := NewComplex(complex(1, 3))
	qt.Assert(t, c1.EqualTo(c3), qt.IsFalse)

	i1 := NewInteger(5)
	qt.Assert(t, c1.EqualTo(i1), qt.IsFalse)
}

func TestComplex_SchemeString(t *testing.T) {
	c1 := NewComplex(complex(3, 4))
	qt.Assert(t, c1.SchemeString(), qt.Equals, "3+4i")

	c2 := NewComplex(complex(3, -4))
	qt.Assert(t, c2.SchemeString(), qt.Equals, "3-4i")

	c3 := NewComplex(complex(0, 0))
	qt.Assert(t, c3.SchemeString(), qt.Equals, "0+0i")
}

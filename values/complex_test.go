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
	"math"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestComplex_NewComplex(t *testing.T) {
	c := values.NewComplex(complex(3, 4))
	qt.Assert(t, c.Real(), qt.Equals, 3.0)
	qt.Assert(t, c.Imag(), qt.Equals, 4.0)
}

func TestComplex_NewComplexFromParts(t *testing.T) {
	c := values.NewComplexFromParts(3, 4)
	qt.Assert(t, c.Real(), qt.Equals, 3.0)
	qt.Assert(t, c.Imag(), qt.Equals, 4.0)
}

func TestComplex_Datum(t *testing.T) {
	c := values.NewComplex(complex(3, 4))
	qt.Assert(t, c.Datum(), qt.Equals, complex(3, 4))
}

func TestComplex_Real(t *testing.T) {
	c := values.NewComplex(complex(3, 4))
	qt.Assert(t, c.Real(), qt.Equals, 3.0)
}

func TestComplex_Imag(t *testing.T) {
	c := values.NewComplex(complex(3, 4))
	qt.Assert(t, c.Imag(), qt.Equals, 4.0)
}

func TestComplex_Add(t *testing.T) {
	c1 := values.NewComplex(complex(1, 2))
	c2 := values.NewComplex(complex(3, 4))
	result := c1.Add(c2)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(4, 6)))

	c3 := values.NewComplex(complex(0, 0))
	result = c1.Add(c3)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(1, 2)))

	i1 := values.NewInteger(5)
	result = c1.Add(i1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(6, 2)))

	f1 := values.NewFloat(2.5)
	result = c1.Add(f1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(3.5, 2)))

	r1 := values.NewRational(1, 2)
	result = c1.Add(r1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(1.5, 2)))
}

func TestComplex_Subtract(t *testing.T) {
	c1 := values.NewComplex(complex(5, 6))
	c2 := values.NewComplex(complex(2, 3))
	result := c1.Subtract(c2)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(3, 3)))

	c3 := values.NewComplex(complex(0, 0))
	result = c1.Subtract(c3)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(5, 6)))

	i1 := values.NewInteger(2)
	result = c1.Subtract(i1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(3, 6)))

	f1 := values.NewFloat(1.0)
	result = c1.Subtract(f1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(4, 6)))

	r1 := values.NewRational(1, 2)
	result = c1.Subtract(r1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(4.5, 6)))
}

func TestComplex_Multiply(t *testing.T) {
	c1 := values.NewComplex(complex(2, 3))
	c2 := values.NewComplex(complex(1, 2))
	result := c1.Multiply(c2)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(-4, 7)))

	c3 := values.NewComplex(complex(0, 0))
	result = c1.Multiply(c3)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(0, 0)))

	i1 := values.NewInteger(3)
	result = c1.Multiply(i1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(6, 9)))

	f1 := values.NewFloat(2.0)
	result = c1.Multiply(f1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(4, 6)))

	r1 := values.NewRational(1, 2)
	result = c1.Multiply(r1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(1, 1.5)))
}

func TestComplex_Divide(t *testing.T) {
	c1 := values.NewComplex(complex(4, 2))
	c2 := values.NewComplex(complex(2, 0))
	result := c1.Divide(c2)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(2, 1)))

	i1 := values.NewInteger(2)
	result = c1.Divide(i1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(2, 1)))

	f1 := values.NewFloat(2.0)
	result = c1.Divide(f1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(2, 1)))

	r1 := values.NewRational(1, 2)
	result = c1.Divide(r1)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewComplex(complex(8, 4)))
}

func TestComplex_IsZero(t *testing.T) {
	c1 := values.NewComplex(complex(0, 0))
	qt.Assert(t, c1.IsZero(), qt.IsTrue)

	c2 := values.NewComplex(complex(1, 2))
	qt.Assert(t, c2.IsZero(), qt.IsFalse)

	c3 := values.NewComplex(complex(0, 1))
	qt.Assert(t, c3.IsZero(), qt.IsFalse)
}

func TestComplex_LessThan(t *testing.T) {
	c1 := values.NewComplex(complex(3, 4))
	c2 := values.NewComplex(complex(5, 6))
	qt.Assert(t, c1.LessThan(c2), qt.IsTrue)
	qt.Assert(t, c2.LessThan(c1), qt.IsFalse)

	i1 := values.NewInteger(5)
	qt.Assert(t, c1.LessThan(i1), qt.IsTrue)

	f1 := values.NewFloat(4.0)
	qt.Assert(t, c1.LessThan(f1), qt.IsTrue)

	r1 := values.NewRational(7, 2)
	qt.Assert(t, c1.LessThan(r1), qt.IsTrue)
}

func TestComplex_IsReal(t *testing.T) {
	c1 := values.NewComplex(complex(5, 0))
	qt.Assert(t, c1.IsReal(), qt.IsTrue)

	c2 := values.NewComplex(complex(5, 1))
	qt.Assert(t, c2.IsReal(), qt.IsFalse)
}

func TestComplex_Magnitude(t *testing.T) {
	c := values.NewComplex(complex(3, 4))
	qt.Assert(t, c.Magnitude(), qt.Equals, 5.0)
}

func TestComplex_Phase(t *testing.T) {
	c := values.NewComplex(complex(1, 1))
	expected := math.Pi / 4
	qt.Assert(t, math.Abs(c.Phase()-expected) < 0.0001, qt.IsTrue)
}

func TestComplex_IsVoid(t *testing.T) {
	c := values.NewComplex(complex(1, 2))
	qt.Assert(t, c.IsVoid(), qt.IsFalse)

	var nilComplex *values.Complex
	qt.Assert(t, nilComplex.IsVoid(), qt.IsTrue)
}

func TestComplex_EqualTo(t *testing.T) {
	c1 := values.NewComplex(complex(1, 2))
	c2 := values.NewComplex(complex(1, 2))
	qt.Assert(t, c1.EqualTo(c2), qt.IsTrue)

	c3 := values.NewComplex(complex(1, 3))
	qt.Assert(t, c1.EqualTo(c3), qt.IsFalse)

	i1 := values.NewInteger(5)
	qt.Assert(t, c1.EqualTo(i1), qt.IsFalse)
}

func TestComplex_SchemeString(t *testing.T) {
	tests := []struct {
		name   string
		value  complex128
		expect string
	}{
		{"positive_imag", complex(3, 4), "3.0+4.0i"},
		{"negative_imag", complex(3, -4), "3.0-4.0i"},
		{"zero", complex(0, 0), "0.0+0.0i"},
		{"integer_like", complex(100, 1), "100.0+1.0i"},
		{"pos_inf_both", complex(math.Inf(1), math.Inf(1)), "+inf.0+inf.0i"},
		{"neg_inf_pos_inf", complex(math.Inf(-1), math.Inf(1)), "-inf.0+inf.0i"},
		{"neg_inf_both", complex(math.Inf(-1), math.Inf(-1)), "-inf.0-inf.0i"},
		{"pos_inf_neg_inf", complex(math.Inf(1), math.Inf(-1)), "+inf.0-inf.0i"},
		{"nan_both", complex(math.NaN(), math.NaN()), "+nan.0+nan.0i"},
		{"decimal", complex(1.5, 2.5), "1.5+2.5i"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := values.NewComplex(tt.value)
			qt.Assert(t, c.SchemeString(), qt.Equals, tt.expect)
		})
	}
}

func TestComplex_HashCode(t *testing.T) {
	c := qt.New(t)

	// Stability: same value produces same hash.
	c.Assert(values.NewComplex(complex(3, 4)).HashCode(),
		qt.Equals, values.NewComplex(complex(3, 4)).HashCode())

	// Zero.
	c.Assert(values.NewComplex(complex(0, 0)).HashCode(),
		qt.Equals, values.NewComplex(complex(0, 0)).HashCode())

	// Distinctness: different values almost certainly produce different hashes.
	h1 := values.NewComplex(complex(1, 2)).HashCode()
	h2 := values.NewComplex(complex(2, 1)).HashCode()
	c.Assert(h1, qt.Not(qt.Equals), h2,
		qt.Commentf("(1+2i) and (2+1i) should hash differently"))

	// Sign matters: real and imaginary parts are treated distinctly.
	hPos := values.NewComplex(complex(3, 4)).HashCode()
	hSwap := values.NewComplex(complex(4, 3)).HashCode()
	c.Assert(hPos, qt.Not(qt.Equals), hSwap)

	// NaN and ±Inf components must not panic (big.Float cannot represent them).
	// Calling HashCode directly: if it panics the test fails with the panic message.
	_ = values.NewComplex(complex(math.NaN(), 0)).HashCode()
	_ = values.NewComplex(complex(0, math.NaN())).HashCode()
	_ = values.NewComplex(complex(math.Inf(1), math.Inf(-1))).HashCode()

	// Stability: same bit pattern produces same hash (even for NaN).
	nan := math.NaN()
	c.Assert(values.NewComplex(complex(nan, 0)).HashCode(),
		qt.Equals, values.NewComplex(complex(nan, 0)).HashCode())
}

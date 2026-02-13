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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestExact(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Integer - already exact
		{name: "exact on integer", code: `(exact 42)`, expected: values.NewInteger(42)},
		{name: "exact on negative integer", code: `(exact -42)`, expected: values.NewInteger(-42)},
		{name: "exact on zero", code: `(exact 0)`, expected: values.NewInteger(0)},

		// Float to rational
		{name: "exact on float 0.5", code: `(exact 0.5)`, expected: values.NewRational(1, 2)},
		{name: "exact on float 0.25", code: `(exact 0.25)`, expected: values.NewRational(1, 4)},
		{name: "exact on float 1.5", code: `(exact 1.5)`, expected: values.NewRational(3, 2)},
		// R7RS §6.2.6: exact on integer float returns Integer (simpler exact representation)
		{name: "exact on integer float", code: `(exact 3.0)`, expected: values.NewInteger(3)},

		// Rational - already exact
		{name: "exact on rational", code: `(exact 3/4)`, expected: values.NewRational(3, 4)},
		{name: "exact on negative rational", code: `(exact -3/4)`, expected: values.NewRational(-3, 4)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestExact_Complex tests exact on complex numbers.
// R7RS §6.2.6: exact converts both real and imaginary parts.
func TestExact_Complex(t *testing.T) {
	// exact on inexact complex produces exact BigComplex
	t.Run("exact on 1.5+2.5i", func(t *testing.T) {
		result, err := runSchemeCode(t, "(exact 1.5+2.5i)")
		qt.Assert(t, err, qt.IsNil)
		_, ok := result.(*values.BigComplex)
		qt.Assert(t, ok, qt.IsTrue)
	})

	t.Run("exact on 3.0+0.0i", func(t *testing.T) {
		result, err := runSchemeCode(t, "(exact 3.0+0.0i)")
		qt.Assert(t, err, qt.IsNil)
		_, ok := result.(*values.BigComplex)
		qt.Assert(t, ok, qt.IsTrue)
	})

	t.Run("exact? of exact complex", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(exact? (exact 1.5+2.5i))")
	})

	// exact on already-exact complex (BigComplex with integer parts)
	t.Run("exact on exact complex passthrough", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(exact? (exact 1+2i))")
	})
}

// TestExact_ComplexFractionalParts tests H4 regression:
// (exact <complex>) must convert fractional parts to Rational, not truncate to Integer.
//
// Architectural Review H4: toExactPart converts BigFloat to integer by
// truncation instead of Rational. (exact 1.5+0i) produces 1 instead of 3/2.
func TestExact_ComplexFractionalParts(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Fractional real part should become Rational 3/2, not Integer 1
		{
			name:     "fractional real preserves fraction",
			code:     "(= (real-part (exact 1.5+0i)) 3/2)",
			expected: values.TrueValue,
		},
		{
			name:     "fractional imag preserves fraction",
			code:     "(= (imag-part (exact 0+2.5i)) 5/2)",
			expected: values.TrueValue,
		},
		{
			name:     "both fractional parts preserve fractions",
			code:     "(and (= (real-part (exact 1.5+2.5i)) 3/2) (= (imag-part (exact 1.5+2.5i)) 5/2))",
			expected: values.TrueValue,
		},
		// Integer-valued floats should simplify to integers
		{
			name:     "integer-valued real simplifies",
			code:     "(integer? (real-part (exact 3.0+2.5i)))",
			expected: values.TrueValue,
		},
		{
			name:     "integer-valued imag simplifies",
			code:     "(integer? (imag-part (exact 1.5+4.0i)))",
			expected: values.TrueValue,
		},
		// Verify exactness is preserved
		{
			name:     "exact complex with fractional parts is exact",
			code:     "(exact? (exact 1.5+2.5i))",
			expected: values.TrueValue,
		},
		{
			name:     "real part of exact complex is exact",
			code:     "(exact? (real-part (exact 1.5+2.5i)))",
			expected: values.TrueValue,
		},
		{
			name:     "imag part of exact complex is exact",
			code:     "(exact? (imag-part (exact 1.5+2.5i)))",
			expected: values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestExactErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "exact on non-number string", code: `(exact "hello")`},
		{name: "exact on symbol", code: `(exact 'foo)`},
		{name: "exact on list", code: `(exact '(1 2 3))`},
		{name: "exact on +inf.0", code: `(exact +inf.0)`},
		{name: "exact on -inf.0", code: `(exact -inf.0)`},
		{name: "exact on +nan.0", code: `(exact +nan.0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

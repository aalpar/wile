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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestInexact(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Integer to float
		{name: "inexact on integer", code: `(inexact 42)`, expected: values.NewFloat(42.0)},
		{name: "inexact on negative integer", code: `(inexact -42)`, expected: values.NewFloat(-42.0)},
		{name: "inexact on zero", code: `(inexact 0)`, expected: values.NewFloat(0.0)},

		// Float - already inexact
		{name: "inexact on float", code: `(inexact 3.14)`, expected: values.NewFloat(3.14)},

		// Rational to float
		{name: "inexact on rational 1/2", code: `(inexact 1/2)`, expected: values.NewFloat(0.5)},
		{name: "inexact on rational 1/4", code: `(inexact 1/4)`, expected: values.NewFloat(0.25)},
		{name: "inexact on rational 3/4", code: `(inexact 3/4)`, expected: values.NewFloat(0.75)},

		// Complex - already inexact
		{name: "inexact on complex", code: `(inexact 1+2i)`, expected: values.NewComplexFromParts(1.0, 2.0)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestInexactBigNumbers(t *testing.T) {
	// BigInteger to float
	t.Run("inexact on BigInteger", func(t *testing.T) {
		result, err := runSchemeCode(t, `(inexact (expt 2 100))`)
		qt.Assert(t, err, qt.IsNil)
		// 2^100 as float
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Value > 1e30, qt.IsTrue) // 2^100 ≈ 1.27e30
	})

	// BigFloat - already inexact
	t.Run("inexact on BigFloat", func(t *testing.T) {
		result, err := runSchemeCode(t, `(inexact #m3.14159265358979323846)`)
		qt.Assert(t, err, qt.IsNil)
		_, ok := result.(*values.BigFloat)
		qt.Assert(t, ok, qt.IsTrue)
	})

	// BigComplex with exact parts - converts to inexact BigComplex
	t.Run("inexact on exact BigComplex", func(t *testing.T) {
		// Create exact BigComplex via arithmetic on BigIntegers
		result, err := runSchemeCode(t, `(inexact? (inexact (make-rectangular (expt 2 100) (expt 2 50))))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
	})
}

func TestInexactErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "inexact on non-number string", code: `(inexact "hello")`},
		{name: "inexact on symbol", code: `(inexact 'foo)`},
		{name: "inexact on list", code: `(inexact '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestInexact(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Integer to float
		{Name: "inexact on integer", Code: `(inexact 42)`, Expected: values.NewFloat(42.0)},
		{Name: "inexact on negative integer", Code: `(inexact -42)`, Expected: values.NewFloat(-42.0)},
		{Name: "inexact on zero", Code: `(inexact 0)`, Expected: values.NewFloat(0.0)},

		// Float - already inexact
		{Name: "inexact on float", Code: `(inexact 3.14)`, Expected: values.NewFloat(3.14)},

		// Rational to float
		{Name: "inexact on rational 1/2", Code: `(inexact 1/2)`, Expected: values.NewFloat(0.5)},
		{Name: "inexact on rational 1/4", Code: `(inexact 1/4)`, Expected: values.NewFloat(0.25)},
		{Name: "inexact on rational 3/4", Code: `(inexact 3/4)`, Expected: values.NewFloat(0.75)},

		// Complex - already inexact
		{Name: "inexact on complex", Code: `(inexact 1+2i)`, Expected: values.NewComplexFromParts(1.0, 2.0)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestInexactBigNumbers(t *testing.T) {
	// BigInteger to float
	t.Run("inexact on BigInteger", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(inexact (expt 2 100))`)
		qt.Assert(t, err, qt.IsNil)
		// 2^100 as float
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Value > 1e30, qt.IsTrue) // 2^100 ≈ 1.27e30
	})

	// BigFloat - already inexact
	t.Run("inexact on BigFloat", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(inexact #m3.14159265358979323846)`)
		qt.Assert(t, err, qt.IsNil)
		_, ok := result.(*values.BigFloat)
		qt.Assert(t, ok, qt.IsTrue)
	})

	// BigComplex with exact parts - converts to inexact BigComplex
	t.Run("inexact on exact BigComplex", func(t *testing.T) {
		// Create exact BigComplex via arithmetic on BigIntegers
		result, err := testhelpers.RunSchemeCode(t, `(inexact? (inexact (make-rectangular (expt 2 100) (expt 2 50))))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
	})
}

func TestInexactErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "inexact on non-number string", Code: `(inexact "hello")`},
		{Name: "inexact on symbol", Code: `(inexact 'foo)`},
		{Name: "inexact on list", Code: `(inexact '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

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

// eqv? Tests (R7RS §6.1 - Equivalence predicate)

func TestEqvQComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// All eq? cases should also be eqv?
		{Name: "same symbol", Code: `(eqv? 'foo 'foo)`, Expected: values.TrueValue},
		{Name: "different symbols", Code: `(eqv? 'foo 'bar)`, Expected: values.FalseValue},
		{Name: "true eqv? true", Code: `(eqv? #t #t)`, Expected: values.TrueValue},
		{Name: "false eqv? false", Code: `(eqv? #f #f)`, Expected: values.TrueValue},
		{Name: "empty list", Code: `(eqv? '() '())`, Expected: values.TrueValue},

		// Numbers - eqv? compares by value AND exactness
		{Name: "same integers", Code: `(eqv? 42 42)`, Expected: values.TrueValue},
		{Name: "different integers", Code: `(eqv? 42 43)`, Expected: values.FalseValue},
		{Name: "negative integers", Code: `(eqv? -5 -5)`, Expected: values.TrueValue},
		{Name: "same floats", Code: `(eqv? 3.14 3.14)`, Expected: values.TrueValue},
		{Name: "different floats", Code: `(eqv? 3.14 2.71)`, Expected: values.FalseValue},
		{Name: "zero floats", Code: `(eqv? 0.0 0.0)`, Expected: values.TrueValue},

		// Exact vs inexact - different exactness means not eqv?
		{Name: "integer vs float same value", Code: `(eqv? 42 42.0)`, Expected: values.FalseValue},
		{Name: "integer vs float zero", Code: `(eqv? 0 0.0)`, Expected: values.FalseValue},

		// Characters
		{Name: "same characters", Code: `(eqv? #\a #\a)`, Expected: values.TrueValue},
		{Name: "different characters", Code: `(eqv? #\a #\b)`, Expected: values.FalseValue},
		{Name: "unicode characters", Code: `(eqv? #\λ #\λ)`, Expected: values.TrueValue},

		// Rationals
		{Name: "same rationals", Code: `(eqv? 1/2 1/2)`, Expected: values.TrueValue},
		{Name: "equivalent rationals", Code: `(eqv? 2/4 1/2)`, Expected: values.TrueValue},
		{Name: "different rationals", Code: `(eqv? 1/2 1/3)`, Expected: values.FalseValue},

		// Complex numbers
		{Name: "same complex", Code: `(eqv? 1+2i 1+2i)`, Expected: values.TrueValue},
		{Name: "different complex", Code: `(eqv? 1+2i 1+3i)`, Expected: values.FalseValue},

		// BigInteger
		{Name: "same big integer", Code: `(eqv? #z123456789012345678901234567890 #z123456789012345678901234567890)`, Expected: values.TrueValue},
		{Name: "different big integers", Code: `(eqv? #z123456789012345678901234567890 #z123456789012345678901234567891)`, Expected: values.FalseValue},

		// Pairs - literals are interned, so they ARE eqv?
		{Name: "literal pairs interned", Code: `(eqv? '(1 2) '(1 2))`, Expected: values.TrueValue},

		// Strings - literals are interned, so they ARE eqv?
		{Name: "literal strings interned", Code: `(eqv? "hello" "hello")`, Expected: values.TrueValue},

		// Bytevectors - eqv? compares by identity, not contents (R7RS §6.1)
		{Name: "bytevector literal same contents (interned)", Code: `(eqv? #u8(1 2 3) #u8(1 2 3))`, Expected: values.TrueValue},
		{Name: "bytevector different contents", Code: `(eqv? #u8(1 2 3) #u8(4 5 6))`, Expected: values.FalseValue},
		{Name: "bytevector vs non-bytevector", Code: `(eqv? #u8(1 2 3) '(1 2 3))`, Expected: values.FalseValue},
		{Name: "empty bytevectors (interned)", Code: `(eqv? #u8() #u8())`, Expected: values.TrueValue},

		// Ports - eqv? on ports compares identity (R7RS §6.1)
		{Name: "same port via let", Code: `(let ((p (open-input-string "hello"))) (eqv? p p))`, Expected: values.TrueValue},
		{Name: "different ports same content", Code: `(eqv? (open-input-string "hello") (open-input-string "hello"))`, Expected: values.FalseValue},
		{Name: "same output port via let", Code: `(let ((p (open-output-string))) (eqv? p p))`, Expected: values.TrueValue},
		{Name: "different output ports", Code: `(eqv? (open-output-string) (open-output-string))`, Expected: values.FalseValue},

		// Procedures
		{Name: "same procedure", Code: `(eqv? + +)`, Expected: values.TrueValue},
		{Name: "different procedures", Code: `(eqv? + -)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestEqvQSpecialValues(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Infinity
		{Name: "positive infinity", Code: `(eqv? +inf.0 +inf.0)`, Expected: values.TrueValue},
		{Name: "negative infinity", Code: `(eqv? -inf.0 -inf.0)`, Expected: values.TrueValue},
		{Name: "pos inf vs neg inf", Code: `(eqv? +inf.0 -inf.0)`, Expected: values.FalseValue},

		// NaN - per R7RS, NaN is not eqv? to itself
		{Name: "nan vs nan", Code: `(eqv? +nan.0 +nan.0)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

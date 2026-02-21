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
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// eqv? Tests (R7RS §6.1 - Equivalence predicate)

func TestEqvQComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// All eq? cases should also be eqv?
		{name: "same symbol", code: `(eqv? 'foo 'foo)`, expected: values.TrueValue},
		{name: "different symbols", code: `(eqv? 'foo 'bar)`, expected: values.FalseValue},
		{name: "true eqv? true", code: `(eqv? #t #t)`, expected: values.TrueValue},
		{name: "false eqv? false", code: `(eqv? #f #f)`, expected: values.TrueValue},
		{name: "empty list", code: `(eqv? '() '())`, expected: values.TrueValue},

		// Numbers - eqv? compares by value AND exactness
		{name: "same integers", code: `(eqv? 42 42)`, expected: values.TrueValue},
		{name: "different integers", code: `(eqv? 42 43)`, expected: values.FalseValue},
		{name: "negative integers", code: `(eqv? -5 -5)`, expected: values.TrueValue},
		{name: "same floats", code: `(eqv? 3.14 3.14)`, expected: values.TrueValue},
		{name: "different floats", code: `(eqv? 3.14 2.71)`, expected: values.FalseValue},
		{name: "zero floats", code: `(eqv? 0.0 0.0)`, expected: values.TrueValue},

		// Exact vs inexact - different exactness means not eqv?
		{name: "integer vs float same value", code: `(eqv? 42 42.0)`, expected: values.FalseValue},
		{name: "integer vs float zero", code: `(eqv? 0 0.0)`, expected: values.FalseValue},

		// Characters
		{name: "same characters", code: `(eqv? #\a #\a)`, expected: values.TrueValue},
		{name: "different characters", code: `(eqv? #\a #\b)`, expected: values.FalseValue},
		{name: "unicode characters", code: `(eqv? #\λ #\λ)`, expected: values.TrueValue},

		// Rationals
		{name: "same rationals", code: `(eqv? 1/2 1/2)`, expected: values.TrueValue},
		{name: "equivalent rationals", code: `(eqv? 2/4 1/2)`, expected: values.TrueValue},
		{name: "different rationals", code: `(eqv? 1/2 1/3)`, expected: values.FalseValue},

		// Complex numbers
		{name: "same complex", code: `(eqv? 1+2i 1+2i)`, expected: values.TrueValue},
		{name: "different complex", code: `(eqv? 1+2i 1+3i)`, expected: values.FalseValue},

		// BigInteger
		{name: "same big integer", code: `(eqv? #z123456789012345678901234567890 #z123456789012345678901234567890)`, expected: values.TrueValue},
		{name: "different big integers", code: `(eqv? #z123456789012345678901234567890 #z123456789012345678901234567891)`, expected: values.FalseValue},

		// Pairs - literals are interned, so they ARE eqv?
		{name: "literal pairs interned", code: `(eqv? '(1 2) '(1 2))`, expected: values.TrueValue},

		// Strings - literals are interned, so they ARE eqv?
		{name: "literal strings interned", code: `(eqv? "hello" "hello")`, expected: values.TrueValue},

		// Bytevectors - eqv? compares by identity, not contents (R7RS §6.1)
		{name: "bytevector literal same contents (interned)", code: `(eqv? #u8(1 2 3) #u8(1 2 3))`, expected: values.TrueValue},
		{name: "bytevector different contents", code: `(eqv? #u8(1 2 3) #u8(4 5 6))`, expected: values.FalseValue},
		{name: "bytevector vs non-bytevector", code: `(eqv? #u8(1 2 3) '(1 2 3))`, expected: values.FalseValue},
		{name: "empty bytevectors (interned)", code: `(eqv? #u8() #u8())`, expected: values.TrueValue},

		// Ports - eqv? on ports compares identity (R7RS §6.1)
		{name: "same port via let", code: `(let ((p (open-input-string "hello"))) (eqv? p p))`, expected: values.TrueValue},
		{name: "different ports same content", code: `(eqv? (open-input-string "hello") (open-input-string "hello"))`, expected: values.FalseValue},
		{name: "same output port via let", code: `(let ((p (open-output-string))) (eqv? p p))`, expected: values.TrueValue},
		{name: "different output ports", code: `(eqv? (open-output-string) (open-output-string))`, expected: values.FalseValue},

		// Procedures
		{name: "same procedure", code: `(eqv? + +)`, expected: values.TrueValue},
		{name: "different procedures", code: `(eqv? + -)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestEqvQSpecialValues(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Infinity
		{name: "positive infinity", code: `(eqv? +inf.0 +inf.0)`, expected: values.TrueValue},
		{name: "negative infinity", code: `(eqv? -inf.0 -inf.0)`, expected: values.TrueValue},
		{name: "pos inf vs neg inf", code: `(eqv? +inf.0 -inf.0)`, expected: values.FalseValue},

		// NaN - per R7RS, NaN is not eqv? to itself
		{name: "nan vs nan", code: `(eqv? +nan.0 +nan.0)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

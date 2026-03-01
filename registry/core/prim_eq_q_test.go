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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// eq? Tests (R7RS §6.1 - Identity comparison)

func TestEqQComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Symbols - same symbol name should be eq?
		{Name: "same symbol", Code: `(eq? 'foo 'foo)`, Expected: values.TrueValue},
		{Name: "different symbols", Code: `(eq? 'foo 'bar)`, Expected: values.FalseValue},

		// Booleans - singletons
		{Name: "true eq? true", Code: `(eq? #t #t)`, Expected: values.TrueValue},
		{Name: "false eq? false", Code: `(eq? #f #f)`, Expected: values.TrueValue},
		{Name: "true not eq? false", Code: `(eq? #t #f)`, Expected: values.FalseValue},

		// Empty list - singleton
		{Name: "empty list eq? empty list", Code: `(eq? '() '())`, Expected: values.TrueValue},

		// Small integers - implementation may cache these
		{Name: "same small integer", Code: `(eq? 5 5)`, Expected: values.TrueValue},
		{Name: "zero eq? zero", Code: `(eq? 0 0)`, Expected: values.TrueValue},

		// Characters - same character should be eq?
		{Name: "same character", Code: `(eq? #\a #\a)`, Expected: values.TrueValue},
		{Name: "different characters", Code: `(eq? #\a #\b)`, Expected: values.FalseValue},

		// Pairs - literals may be shared per R7RS §4.1.2
		// "The implementation may share storage between constants where appropriate."
		// This implementation interns literal lists, so they ARE eq?
		{Name: "literal pairs same contents (interned)", Code: `(eq? '(1 2) '(1 2))`, Expected: values.TrueValue},
		{Name: "different pairs different contents", Code: `(eq? '(1) '(2))`, Expected: values.FalseValue},

		// Strings - literals may be shared per R7RS §4.1.2
		// This implementation interns literal strings, so they ARE eq?
		{Name: "literal strings same contents (interned)", Code: `(eq? "hello" "hello")`, Expected: values.TrueValue},

		// Vectors - literals are interned like pairs and strings
		{Name: "literal vectors same contents (interned)", Code: `(eq? #(1 2 3) #(1 2 3))`, Expected: values.TrueValue},

		// Procedures - same procedure should be eq?
		{Name: "same primitive", Code: `(eq? + +)`, Expected: values.TrueValue},
		{Name: "different primitives", Code: `(eq? + -)`, Expected: values.FalseValue},

		// Large integers - different literal bignums are distinct objects
		{Name: "large integer literals (interned)", Code: `(eq? #z123456789012345678901234567890 #z123456789012345678901234567890)`, Expected: values.TrueValue},
		{Name: "different large integers", Code: `(eq? #z123456789012345678901234567890 #z123456789012345678901234567891)`, Expected: values.FalseValue},

		// Float identity
		{Name: "same float literal", Code: `(eq? 3.14 3.14)`, Expected: values.TrueValue},
		{Name: "different floats", Code: `(eq? 3.14 2.71)`, Expected: values.FalseValue},
		{Name: "positive zero float", Code: `(eq? 0.0 0.0)`, Expected: values.TrueValue},
		{Name: "positive infinity", Code: `(eq? +inf.0 +inf.0)`, Expected: values.TrueValue},
		{Name: "negative infinity", Code: `(eq? -inf.0 -inf.0)`, Expected: values.TrueValue},

		// Cross-type comparisons
		{Name: "integer vs symbol", Code: `(eq? 1 'one)`, Expected: values.FalseValue},
		{Name: "string vs symbol", Code: `(eq? "foo" 'foo)`, Expected: values.FalseValue},
		{Name: "empty list vs false", Code: `(eq? '() #f)`, Expected: values.FalseValue},
		{Name: "integer vs float same value", Code: `(eq? 42 42.0)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestEqQWithLetBinding(t *testing.T) {
	// Test that eq? works correctly with let bindings (same object)
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "same pair via let",
			Code:     `(let ((x '(1 2 3))) (eq? x x))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "same string via let",
			Code:     `(let ((s "hello")) (eq? s s))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "same vector via let",
			Code:     `(let ((v #(1 2 3))) (eq? v v))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "same lambda via let",
			Code:     `(let ((f (lambda (x) x))) (eq? f f))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

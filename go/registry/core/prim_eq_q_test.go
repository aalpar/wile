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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// eq? Tests (R7RS §6.1 - Identity comparison)

func TestEqQComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Symbols - same symbol name should be eq?
		{name: "same symbol", code: `(eq? 'foo 'foo)`, expected: values.TrueValue},
		{name: "different symbols", code: `(eq? 'foo 'bar)`, expected: values.FalseValue},

		// Booleans - singletons
		{name: "true eq? true", code: `(eq? #t #t)`, expected: values.TrueValue},
		{name: "false eq? false", code: `(eq? #f #f)`, expected: values.TrueValue},
		{name: "true not eq? false", code: `(eq? #t #f)`, expected: values.FalseValue},

		// Empty list - singleton
		{name: "empty list eq? empty list", code: `(eq? '() '())`, expected: values.TrueValue},

		// Small integers - implementation may cache these
		{name: "same small integer", code: `(eq? 5 5)`, expected: values.TrueValue},
		{name: "zero eq? zero", code: `(eq? 0 0)`, expected: values.TrueValue},

		// Characters - same character should be eq?
		{name: "same character", code: `(eq? #\a #\a)`, expected: values.TrueValue},
		{name: "different characters", code: `(eq? #\a #\b)`, expected: values.FalseValue},

		// Pairs - literals may be shared per R7RS §4.1.2
		// "The implementation may share storage between constants where appropriate."
		// This implementation interns literal lists, so they ARE eq?
		{name: "literal pairs same contents (interned)", code: `(eq? '(1 2) '(1 2))`, expected: values.TrueValue},
		{name: "different pairs different contents", code: `(eq? '(1) '(2))`, expected: values.FalseValue},

		// Strings - literals may be shared per R7RS §4.1.2
		// This implementation interns literal strings, so they ARE eq?
		{name: "literal strings same contents (interned)", code: `(eq? "hello" "hello")`, expected: values.TrueValue},

		// Vectors - literals are interned like pairs and strings
		{name: "literal vectors same contents (interned)", code: `(eq? #(1 2 3) #(1 2 3))`, expected: values.TrueValue},

		// Procedures - same procedure should be eq?
		{name: "same primitive", code: `(eq? + +)`, expected: values.TrueValue},
		{name: "different primitives", code: `(eq? + -)`, expected: values.FalseValue},

		// Cross-type comparisons
		{name: "integer vs symbol", code: `(eq? 1 'one)`, expected: values.FalseValue},
		{name: "string vs symbol", code: `(eq? "foo" 'foo)`, expected: values.FalseValue},
		{name: "empty list vs false", code: `(eq? '() #f)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestEqQWithLetBinding(t *testing.T) {
	// Test that eq? works correctly with let bindings (same object)
	tcs := []schemeCodeTestCase{
		{
			name:     "same pair via let",
			code:     `(let ((x '(1 2 3))) (eq? x x))`,
			expected: values.TrueValue,
		},
		{
			name:     "same string via let",
			code:     `(let ((s "hello")) (eq? s s))`,
			expected: values.TrueValue,
		},
		{
			name:     "same vector via let",
			code:     `(let ((v #(1 2 3))) (eq? v v))`,
			expected: values.TrueValue,
		},
		{
			name:     "same lambda via let",
			code:     `(let ((f (lambda (x) x))) (eq? f f))`,
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

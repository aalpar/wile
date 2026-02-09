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

// call-with-values Tests (R7RS §6.4 - Multiple values)

func TestCallWithValuesComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Single value
		{name: "single value", code: `(call-with-values (lambda () 42) (lambda (x) x))`, expected: values.NewInteger(42)},

		// Multiple values
		{name: "two values", code: `(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))`, expected: values.NewInteger(3)},
		{name: "three values", code: `(call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (* a b c)))`, expected: values.NewInteger(6)},
		{name: "five values", code: `(call-with-values (lambda () (values 1 2 3 4 5)) (lambda (a b c d e) (+ a b c d e)))`, expected: values.NewInteger(15)},

		// Zero values
		{name: "zero values", code: `(call-with-values (lambda () (values)) (lambda () 'done))`, expected: values.NewSymbol("done")},

		// Consumer uses list
		{name: "consumer builds list", code: `(call-with-values (lambda () (values 'a 'b 'c)) list)`, expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},

		// Values from arithmetic
		{name: "floor/ values", code: `(call-with-values (lambda () (floor/ 13 4)) (lambda (q r) (+ (* q 10) r)))`, expected: values.NewInteger(31)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCallWithValuesErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "producer not procedure", code: `(call-with-values 5 (lambda (x) x))`},
		{name: "consumer not procedure", code: `(call-with-values (lambda () 1) 5)`},
		{name: "arity mismatch", code: `(call-with-values (lambda () (values 1 2)) (lambda (x) x))`},
		{name: "exception in producer", code: `(call-with-values (lambda () (error "boom")) list)`},
		{name: "exception in consumer", code: `(call-with-values (lambda () (values 1 2)) (lambda (x y) (error "boom")))`},
		{name: "arity mismatch three to two", code: `(call-with-values (lambda () (values 1 2 3)) (lambda (x y) (+ x y)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

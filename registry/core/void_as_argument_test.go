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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// TestVoidReturningExpressionAsArgument is a regression test for a bug where
// void-returning primitives (display, newline, vector-set!, etc.) used as
// function arguments caused arity errors. The root cause: mc.SetValues()
// with zero arguments set the value register to an empty slice, and
// OperationPush's PushAll pushed nothing — silently dropping the argument
// slot from the eval stack. The fix: all void-returning primitives use
// mc.SetValue(values.Void) to push exactly one value.
func TestVoidReturningExpressionAsArgument(t *testing.T) {
	c := qt.New(t)

	tcs := []schemeCodeTestCase{
		// display returns void; passing it as an argument should work
		{
			name:     "display as argument",
			code:     `((lambda (x) x) (display ""))`,
			expected: values.Void,
		},
		// newline returns void
		{
			name:     "newline as argument",
			code:     `((lambda (x) x) (newline))`,
			expected: values.Void,
		},
		// write returns void
		{
			name:     "write as argument",
			code:     `((lambda (x) x) (write ""))`,
			expected: values.Void,
		},
		// vector-set! returns void
		{
			name:     "vector-set! as argument",
			code:     `((lambda (x) x) (vector-set! (vector 1 2 3) 0 99))`,
			expected: values.Void,
		},
		// bytevector-u8-set! returns void
		{
			name:     "bytevector-u8-set! as argument",
			code:     `((lambda (x) x) (bytevector-u8-set! (bytevector 1 2 3) 0 99))`,
			expected: values.Void,
		},
		// close-port returns void
		{
			name:     "close-port as argument",
			code:     `((lambda (x) x) (close-port (open-input-string "x")))`,
			expected: values.Void,
		},
		// void as argument in a multi-argument call
		{
			name:     "void among multiple arguments",
			code:     `((lambda (x y) y) (display "") 42)`,
			expected: values.NewInteger(42),
		},
		// void as first of two arguments
		{
			name:     "void as first argument",
			code:     `((lambda (x y) x) (display "") 42)`,
			expected: values.Void,
		},
		// guard with void-producing expression (the original symptom)
		{
			name:     "guard with void body",
			code:     `(guard (exn (#t "caught")) (display ""))`,
			expected: values.Void,
		},
		// guard catching a raise after void-producing code
		{
			name: "guard catching raise",
			code: `(guard (exn (#t exn))
				(raise "test"))`,
			expected: values.NewString("test"),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

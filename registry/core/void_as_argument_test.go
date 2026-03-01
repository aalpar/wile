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

	"github.com/aalpar/wile/registry/testhelpers"
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

	tcs := []testhelpers.SchemeCodeTestCase{
		// display returns void; passing it as an argument should work
		{
			Name:     "display as argument",
			Code:     `((lambda (x) x) (display ""))`,
			Expected: values.Void,
		},
		// newline returns void
		{
			Name:     "newline as argument",
			Code:     `((lambda (x) x) (newline))`,
			Expected: values.Void,
		},
		// write returns void
		{
			Name:     "write as argument",
			Code:     `((lambda (x) x) (write ""))`,
			Expected: values.Void,
		},
		// vector-set! returns void
		{
			Name:     "vector-set! as argument",
			Code:     `((lambda (x) x) (vector-set! (vector 1 2 3) 0 99))`,
			Expected: values.Void,
		},
		// bytevector-u8-set! returns void
		{
			Name:     "bytevector-u8-set! as argument",
			Code:     `((lambda (x) x) (bytevector-u8-set! (bytevector 1 2 3) 0 99))`,
			Expected: values.Void,
		},
		// close-port returns void
		{
			Name:     "close-port as argument",
			Code:     `((lambda (x) x) (close-port (open-input-string "x")))`,
			Expected: values.Void,
		},
		// void as argument in a multi-argument call
		{
			Name:     "void among multiple arguments",
			Code:     `((lambda (x y) y) (display "") 42)`,
			Expected: values.NewInteger(42),
		},
		// void as first of two arguments
		{
			Name:     "void as first argument",
			Code:     `((lambda (x y) x) (display "") 42)`,
			Expected: values.Void,
		},
		// guard with void-producing expression (the original symptom)
		{
			Name:     "guard with void body",
			Code:     `(guard (exn (#t "caught")) (display ""))`,
			Expected: values.Void,
		},
		// guard catching a raise after void-producing code
		{
			Name: "guard catching raise",
			Code: `(guard (exn (#t exn))
				(raise "test"))`,
			Expected: values.NewString("test"),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

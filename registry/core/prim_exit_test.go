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

func TestCallWithExit_Success(t *testing.T) {
	c := qt.New(t)

	tcs := []schemeCodeTestCase{
		{
			name:     "normal return",
			code:     `(call-with-exit (lambda (exit) 42))`,
			expected: values.NewInteger(42),
		},
		{
			name:     "exit with value",
			code:     `(call-with-exit (lambda (exit) (exit 99) 42))`,
			expected: values.NewInteger(99),
		},
		{
			name: "exit from nested call",
			code: `(call-with-exit (lambda (exit)
			  (map (lambda (x) (if (> x 3) (exit 'found) x))
			       '(1 2 3 4 5))))`,
			expected: values.NewSymbol("found"),
		},
		{
			name: "dynamic-wind after thunk runs on exit",
			code: `(let ((log '()))
			  (call-with-exit (lambda (exit)
			    (dynamic-wind
			      (lambda () (set! log (cons 'before log)))
			      (lambda () (exit 'done))
			      (lambda () (set! log (cons 'after log))))))
			  (reverse log))`,
			expected: values.NewCons(values.NewSymbol("before"), values.NewCons(values.NewSymbol("after"), values.EmptyList)),
		},
		{
			name: "nested call-with-exit inner exit does not escape outer",
			code: `(call-with-exit (lambda (outer)
			  (call-with-exit (lambda (inner)
			    (inner 'inner-val)))
			  'outer-val))`,
			expected: values.NewSymbol("outer-val"),
		},
		{
			name:     "exit skips remaining computation",
			code:     `(+ 1 (call-with-exit (lambda (exit) (+ 100 (exit 10)))))`,
			expected: values.NewInteger(11),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCallWithExit_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "exit procedure invalid after return",
			code: `(let ((saved #f))
			  (call-with-exit (lambda (exit) (set! saved exit) 42))
			  (saved 99))`,
		},
		{
			name: "non-procedure argument",
			code: `(call-with-exit 42)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

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

func TestCallWithExit_Success(t *testing.T) {
	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "normal return",
			Code:     `(call-with-exit (lambda (exit) 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "exit with value",
			Code:     `(call-with-exit (lambda (exit) (exit 99) 42))`,
			Expected: values.NewInteger(99),
		},
		{
			Name: "exit from nested call",
			Code: `(call-with-exit (lambda (exit)
			  (map (lambda (x) (if (> x 3) (exit 'found) x))
			       '(1 2 3 4 5))))`,
			Expected: values.NewSymbol("found"),
		},
		{
			Name: "dynamic-wind after thunk runs on exit",
			Code: `(let ((log '()))
			  (call-with-exit (lambda (exit)
			    (dynamic-wind
			      (lambda () (set! log (cons 'before log)))
			      (lambda () (exit 'done))
			      (lambda () (set! log (cons 'after log))))))
			  (reverse log))`,
			Expected: values.NewCons(values.NewSymbol("before"), values.NewCons(values.NewSymbol("after"), values.EmptyList)),
		},
		{
			Name: "nested call-with-exit inner exit does not escape outer",
			Code: `(call-with-exit (lambda (outer)
			  (call-with-exit (lambda (inner)
			    (inner 'inner-val)))
			  'outer-val))`,
			Expected: values.NewSymbol("outer-val"),
		},
		{
			Name:     "exit skips remaining computation",
			Code:     `(+ 1 (call-with-exit (lambda (exit) (+ 100 (exit 10)))))`,
			Expected: values.NewInteger(11),
		},
		{
			Name: "exit from dynamic-wind before thunk does not run after",
			Code: `(let ((log '()))
			  (call-with-exit (lambda (exit)
			    (dynamic-wind
			      (lambda () (exit 'from-before))
			      (lambda () (set! log (cons 'body log)))
			      (lambda () (set! log (cons 'after log))))))
			  (reverse log))`,
			Expected: values.EmptyList, // winding frame never installed, after thunk never runs
		},
		{
			Name: "exit through continuation barrier",
			Code: `(call-with-exit (lambda (exit)
			  (with-continuation-barrier (exit 42))))`,
			Expected: values.NewInteger(42), // ErrExitEscape propagates through barriers
		},
		{
			Name: "exit from guard handler body",
			Code: `(call-with-exit (lambda (exit)
			  (guard (e (#t (exit 'caught)))
			    (error "boom"))))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name: "exit from within call-with-continuation-prompt",
			Code: `(let ((tag (make-continuation-prompt-tag 'test)))
			  (call-with-exit (lambda (exit)
			    (call-with-continuation-prompt
			      (lambda () (exit 'escaped))
			      tag
			      (lambda (v) 'not-reached)))))`,
			Expected: values.NewSymbol("escaped"),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCallWithExit_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "exit procedure invalid after return",
			Code: `(let ((saved #f))
			  (call-with-exit (lambda (exit) (set! saved exit) 42))
			  (saved 99))`,
		},
		{
			Name: "non-procedure argument",
			Code: `(call-with-exit 42)`,
		},
		{
			Name: "exit called with zero arguments",
			Code: `(call-with-exit (lambda (exit) (exit)))`,
		},
		{
			Name: "exit called with too many arguments",
			Code: `(call-with-exit (lambda (exit) (exit 1 2)))`,
		},
		{
			Name: "cross-thread exit invocation during dynamic extent",
			Code: `(call-with-exit (lambda (exit)
			  (let ((th (make-thread (lambda () (exit 99)))))
			    (thread-start! th)
			    (thread-join! th))))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

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

func TestWithContinuationBarrier_Success(t *testing.T) {
	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "normal return last value",
			Code:     `(with-continuation-barrier 1 2 3)`,
			Expected: values.NewInteger(3),
		},
		{
			Name: "call-with-exit inside barrier works",
			Code: `(with-continuation-barrier
			  (call-with-exit (lambda (exit) (exit 42))))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "dynamic-wind inside barrier works",
			Code: `(let ((log '()))
			  (with-continuation-barrier
			    (dynamic-wind
			      (lambda () (set! log (cons 'before log)))
			      (lambda () 42)
			      (lambda () (set! log (cons 'after log)))))
			  (reverse log))`,
			Expected: values.NewCons(values.NewSymbol("before"), values.NewCons(values.NewSymbol("after"), values.EmptyList)),
		},
		{
			Name:     "nested barriers return correctly",
			Code:     `(with-continuation-barrier (with-continuation-barrier 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "composable continuation entirely inside barrier works",
			Code: `(with-continuation-barrier
			  (let ((tag (make-continuation-prompt-tag 'test)))
			    (call-with-continuation-prompt
			      (lambda ()
			        (+ 1 (call-with-composable-continuation
			                (lambda (k) (k (k 10)))
			                tag)))
			      tag
			      (lambda (v) v))))`,
			Expected: values.NewInteger(12),
		},
		{
			Name: "prompt abort passes through barrier",
			Code: `(let ((tag (make-continuation-prompt-tag 'test)))
			  (call-with-continuation-prompt
			    (lambda ()
			      (with-continuation-barrier
			        (abort-current-continuation tag 42)))
			    tag
			    (lambda (v) v)))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "with-baffle alias works",
			Code:     `(with-baffle 42)`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "exit propagates through barrier from outside",
			Code: `(call-with-exit (lambda (exit)
			  (with-continuation-barrier (exit 42))))`,
			Expected: values.NewInteger(42), // ErrExitEscape is upward-only, not blocked by barriers
		},
		{
			Name: "raise-continuable propagates through barrier",
			Code: `(with-exception-handler
			  (lambda (e) (+ e 100))
			  (lambda ()
			    (with-continuation-barrier
			      (raise-continuable 5))))`,
			Expected: values.NewInteger(105),
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

func TestWithContinuationBarrier_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "call/cc escape from inside to outside blocked",
			Code: `(call/cc (lambda (k)
			  (with-continuation-barrier (k 42))))`,
		},
		{
			Name: "call/cc inside barrier invoked outside blocked",
			Code: `(let ((k #f))
			  (with-continuation-barrier
			    (call/cc (lambda (c) (set! k c) 42)))
			  (k 99))`,
		},
		{
			Name: "continuation crosses between distinct barriers",
			Code: `(let ((k #f))
			  (with-continuation-barrier
			    (call/cc (lambda (c) (set! k c) 42)))
			  (with-continuation-barrier (k 99)))`,
		},
		{
			Name: "composable continuation blocked at barrier boundary",
			Code: `(let ((k #f)
			       (tag (make-continuation-prompt-tag 'test)))
			  (with-continuation-barrier
			    (call-with-continuation-prompt
			      (lambda ()
			        (call-with-composable-continuation
			          (lambda (c) (set! k c) 42)
			          tag))
			      tag
			      (lambda (v) v)))
			  (k 99))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

func TestWithContinuationBarrier_ExceptionPassthrough(t *testing.T) {
	c := qt.New(t)

	result, err := testhelpers.RunSchemeCode(t, `(guard (e (#t 'caught))
	  (with-continuation-barrier (error "boom")))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("caught"))
}

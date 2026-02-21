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

func TestWithContinuationBarrier_Success(t *testing.T) {
	c := qt.New(t)

	tcs := []schemeCodeTestCase{
		{
			name:     "normal return last value",
			code:     `(with-continuation-barrier 1 2 3)`,
			expected: values.NewInteger(3),
		},
		{
			name: "call-with-exit inside barrier works",
			code: `(with-continuation-barrier
			  (call-with-exit (lambda (exit) (exit 42))))`,
			expected: values.NewInteger(42),
		},
		{
			name: "dynamic-wind inside barrier works",
			code: `(let ((log '()))
			  (with-continuation-barrier
			    (dynamic-wind
			      (lambda () (set! log (cons 'before log)))
			      (lambda () 42)
			      (lambda () (set! log (cons 'after log)))))
			  (reverse log))`,
			expected: values.NewCons(values.NewSymbol("before"), values.NewCons(values.NewSymbol("after"), values.EmptyList)),
		},
		{
			name:     "nested barriers return correctly",
			code:     `(with-continuation-barrier (with-continuation-barrier 42))`,
			expected: values.NewInteger(42),
		},
		{
			name: "composable continuation entirely inside barrier works",
			code: `(with-continuation-barrier
			  (let ((tag (make-continuation-prompt-tag 'test)))
			    (call-with-continuation-prompt
			      (lambda ()
			        (+ 1 (call-with-composable-continuation
			                (lambda (k) (k (k 10)))
			                tag)))
			      tag
			      (lambda (v) v))))`,
			expected: values.NewInteger(12),
		},
		{
			name: "prompt abort passes through barrier",
			code: `(let ((tag (make-continuation-prompt-tag 'test)))
			  (call-with-continuation-prompt
			    (lambda ()
			      (with-continuation-barrier
			        (abort-current-continuation tag 42)))
			    tag
			    (lambda (v) v)))`,
			expected: values.NewInteger(42),
		},
		{
			name:     "with-baffle alias works",
			code:     `(with-baffle 42)`,
			expected: values.NewInteger(42),
		},
		{
			name: "exit propagates through barrier from outside",
			code: `(call-with-exit (lambda (exit)
			  (with-continuation-barrier (exit 42))))`,
			expected: values.NewInteger(42), // ErrExitEscape is upward-only, not blocked by barriers
		},
		{
			name: "raise-continuable propagates through barrier",
			code: `(with-exception-handler
			  (lambda (e) (+ e 100))
			  (lambda ()
			    (with-continuation-barrier
			      (raise-continuable 5))))`,
			expected: values.NewInteger(105),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestWithContinuationBarrier_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "call/cc escape from inside to outside blocked",
			code: `(call/cc (lambda (k)
			  (with-continuation-barrier (k 42))))`,
		},
		{
			name: "call/cc inside barrier invoked outside blocked",
			code: `(let ((k #f))
			  (with-continuation-barrier
			    (call/cc (lambda (c) (set! k c) 42)))
			  (k 99))`,
		},
		{
			name: "continuation crosses between distinct barriers",
			code: `(let ((k #f))
			  (with-continuation-barrier
			    (call/cc (lambda (c) (set! k c) 42)))
			  (with-continuation-barrier (k 99)))`,
		},
		{
			name: "composable continuation blocked at barrier boundary",
			code: `(let ((k #f)
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
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestWithContinuationBarrier_ExceptionPassthrough(t *testing.T) {
	c := qt.New(t)

	result, err := runSchemeCode(t, `(guard (e (#t 'caught))
	  (with-continuation-barrier (error "boom")))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("caught"))
}

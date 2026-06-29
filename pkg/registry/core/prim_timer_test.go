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
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestWithTimeout(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "normal completion returns thunk result",
			Code:     `(with-timeout 5000 (lambda (k) 'timeout) (lambda () 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "handler return value when timeout fires",
			Code:     `(with-timeout 1 (lambda (k) 'expired) (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("expired"),
		},
		{
			Name: "handler receives composable continuation",
			Code: `(with-timeout 1
                     (lambda (k) (procedure? k))
                     (lambda () (let loop () (loop))))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestWithTimeoutErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "ms not integer", Code: `(with-timeout "bad" (lambda (k) k) (lambda () 1))`},
		{Name: "handler not procedure", Code: `(with-timeout 100 42 (lambda () 1))`},
		{Name: "thunk not procedure", Code: `(with-timeout 100 (lambda (k) k) 42)`},
		{Name: "negative ms", Code: `(with-timeout -1 (lambda (k) k) (lambda () 1))`},
		{Name: "wrong arity", Code: `(with-timeout 100 (lambda (k) k))`},
		{Name: "ms overflow", Code: `(with-timeout 9999999999999999 (lambda (k) k) (lambda () 1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestWithTimeoutNesting(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "inner fires before outer",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (with-timeout 1 (lambda (k) 'inner-fired)
                         (lambda () (let loop () (loop))))))`,
			Expected: values.NewSymbol("inner-fired"),
		},
		{
			Name: "inner completes normally within outer",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (with-timeout 5000 (lambda (k) 'inner-fired)
                         (lambda () 42))))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "nested normal completion propagates",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (+ 1 (with-timeout 5000 (lambda (k) 0)
                               (lambda () 41)))))`,
			Expected: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestWithTimeoutResumption(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "handler discards continuation",
			Code:     `(with-timeout 1 (lambda (k) 'discarded) (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("discarded"),
		},
		{
			Name: "handler can inspect continuation type",
			Code: `(with-timeout 1
                     (lambda (k) (and (procedure? k) 'ok))
                     (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestWithTimeoutDynamicWind(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "normal completion fires dynamic-wind after thunk",
			Code: `(let ((log '()))
                     (with-timeout 5000
                       (lambda (k) 'timeout)
                       (lambda ()
                         (dynamic-wind
                           (lambda () (set! log (cons 'before log)))
                           (lambda () 42)
                           (lambda () (set! log (cons 'after log))))))
                     log)`,
			Expected: values.List(
				values.NewSymbol("after"),
				values.NewSymbol("before"),
			),
		},
		{
			Name: "handler discards continuation — after thunks NOT called",
			Code: `(let ((after-called #f))
                     (with-timeout 1
                       (lambda (k) after-called)
                       (lambda ()
                         (dynamic-wind
                           (lambda () #f)
                           (lambda () (let loop () (loop)))
                           (lambda () (set! after-called #t))))))`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestWithTimeoutInSubContext is the regression guard for the timer-arm symmetry: a
// with-timeout whose timer FIRES while it runs inside a SURVIVING sub-context (here an
// eval sub-context, driven by RunWithinBoundary) must be resolved on that sub's OWN chain
// by RunWithinBoundary's timer arm — not re-raised to the top driver, whose FindPrompt
// would walk the wrong chain and find no boundary. The reified design moved timer routing
// from a primitive-local Go catch to FindPrompt(tag), so without the symmetric
// resolveTimerInterrupt arm in RunWithinBoundary this truncates or crashes ("no boundary
// frame found"). If the loop is not interrupted, the 10s test timeout fails it loudly.
func TestWithTimeoutInSubContext(t *testing.T) {
	code := `(eval '(with-timeout 1 (lambda (k) 'expired) (lambda () (let loop () (loop))))
	               (interaction-environment))`
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 10*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("expired"))
}

func TestWithTimeoutThreadIsolation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "timeout in one thread does not affect another",
			Code: `(let ((result #f)
                          (m (make-mutex)))
                      (mutex-lock! m)
                      (thread-start!
                        (make-thread
                          (lambda ()
                            (set! result
                              (with-timeout 5000
                                (lambda (k) 'timeout)
                                (lambda () 'ok)))
                            (mutex-unlock! m))))
                      (mutex-lock! m)
                      result)`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

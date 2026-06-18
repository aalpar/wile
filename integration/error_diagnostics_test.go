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

package integration_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestErrorDiagnostics(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "guard captures error with source",
			Code: `(guard (e ((error-object? e)
					(let ((src (error-object-source e)))
						(or (string? src) (not src)))))
				(error "test"))`,
			Expected: values.TrueValue,
		},
		{
			Name: "nested handlers get innermost context",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e1)
							(k 'outer))
						(lambda ()
							(with-exception-handler
								(lambda (e2)
									(k (error-context?
										(current-error-context))))
								(lambda () (raise "inner")))))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "non-error raise has context via mark",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(let ((ctx (current-error-context)))
								(k (and (error-context? ctx)
										(eqv? e 42)))))
						(lambda () (raise 42)))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "stack trace has frames from nested calls",
			Code: `(begin
				(define (f) (error "boom"))
				(define (g) (f))
				(guard (e (#t
					(let ((frames (error-object-stack-trace e)))
						(and (list? frames)
							(> (length frames) 0)))))
					(g)))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-source is #f for non-error",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(k (not (error-object? e))))
						(lambda () (raise 42)))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "stack frame alist has expected keys",
			Code: `(begin
				(define (f) (error "boom"))
				(guard (e (#t
					(let* ((frames (error-object-stack-trace e))
						   (first-frame (car frames)))
						(and (pair? (assq 'name first-frame))
							(pair? first-frame)))))
					(f)))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-message still works with enrichment",
			Code: `(guard (e ((error-object? e)
					(string=? (error-object-message e) "hello world")))
				(error "hello world"))`,
			Expected: values.TrueValue,
		},
		{
			Name: "raise-continuable has context",
			Code: `(with-exception-handler
				(lambda (e)
					(error-context? (current-error-context)))
				(lambda () (raise-continuable "note")))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-irritants preserved after enrichment",
			Code: `(guard (e ((error-object? e)
					(equal? (error-object-irritants e) '(1 2 3))))
				(error "test" 1 2 3))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-stack-trace returns list for simple error",
			Code: `(guard (e ((error-object? e)
					(list? (error-object-stack-trace e))))
				(error "simple"))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "current-error-context is #f outside handler",
			Code:     `(current-error-context)`,
			Expected: values.FalseValue,
		},
		{
			Name: "error-context-source returns string or #f",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(let* ((ctx (current-error-context))
								   (src (error-context-source ctx)))
								(k (or (string? src) (not src)))))
						(lambda () (raise "boom")))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-context-stack-trace returns list",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(let ((ctx (current-error-context)))
								(k (list? (error-context-stack-trace ctx)))))
						(lambda () (raise "boom")))))`,
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

	// Error cases: primitives applied to wrong types
	errorTcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "error-object-source on non-error",
			Code: `(error-object-source 42)`,
		},
		{
			Name: "error-object-stack-trace on non-error",
			Code: `(error-object-stack-trace "not-an-error")`,
		},
	}

	for _, tc := range errorTcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

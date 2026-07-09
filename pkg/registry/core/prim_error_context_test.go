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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNativeErrorEnrichment(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "error-object-source returns string or false",
			Code: `(guard (e ((error-object? e)
					(let ((src (error-object-source e)))
						(or (string? src) (not src)))))
				(error "test error"))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-stack-trace is list via error",
			Code: `(guard (e ((error-object? e) (list? (error-object-stack-trace e))))
				(error "test error"))`,
			Expected: values.TrueValue,
		},
		{
			Name: "stack trace has frames",
			Code: `(begin (define (f) (error "boom"))
				(define (g) (f))
				(guard (e (#t
					(let ((frames (error-object-stack-trace e)))
						(and (list? frames) (> (length frames) 0)))))
					(g)))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-source from nested call returns string or false",
			Code: `(begin (define (f) (error "inner"))
				(guard (e ((error-object? e)
					(let ((src (error-object-source e)))
						(or (string? src) (not src)))))
					(f)))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-stack-trace non-empty when enriched",
			Code: `(guard (e ((error-object? e)
					(let ((st (error-object-stack-trace e)))
						(and (list? st) (> (length st) 0)))))
				(error "enriched"))`,
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

	// Error cases
	errorTcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "error-object-source wrong type",
			Code: `(error-object-source 42)`,
		},
		{
			Name: "error-object-stack-trace wrong type",
			Code: `(error-object-stack-trace "not-an-error")`,
		},
		{
			Name: "error-context-source wrong type",
			Code: `(error-context-source 42)`,
		},
		{
			Name: "error-context-stack-trace wrong type",
			Code: `(error-context-stack-trace "nope")`,
		},
		{
			Name: "error-context-marks wrong type",
			Code: `(error-context-marks #t)`,
		},
	}
	for _, tc := range errorTcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestErrorContext_Primitives(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "current-error-context outside handler",
			Code:     `(current-error-context)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "error-context? on non-context",
			Code:     `(error-context? 42)`,
			Expected: values.FalseValue,
		},
		{
			Name: "error-context available in handler",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e) (k (error-context? (current-error-context))))
						(lambda () (raise "boom")))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-context-source returns string or false",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(let ((ctx (current-error-context)))
								(k (let ((src (error-context-source ctx)))
									(or (string? src) (not src))))))
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
		{
			Name: "non-error raise has context",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(k (and (error-context? (current-error-context))
									(not (error-object? e)))))
						(lambda () (raise 42)))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-context with raise-continuable",
			Code: `(with-exception-handler
				(lambda (e) (error-context? (current-error-context)))
				(lambda () (raise-continuable "note")))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-context-marks captures a mark set at the raise site",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(let ((ctx (current-error-context)))
								(k (continuation-mark-set? (error-context-marks ctx)))))
						(lambda () (raise "boom")))))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-context-marks round-trips a value set before the raise",
			Code: `(call/cc
				(lambda (k)
					(with-exception-handler
						(lambda (e)
							(let ((ctx (current-error-context)))
								(k (continuation-mark-set-first
										(error-context-marks ctx) 'my-key 'missing))))
						(lambda ()
							(with-continuation-mark 'my-key 'found
								(raise "boom"))))))`,
			Expected: values.NewSymbol("found"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

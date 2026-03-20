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

// =============================================================================
// Parameterize × Composable Continuation Regression Tests
//
// These test the interaction between parameterize and composable continuations
// (call-with-composable-continuation). With dynamic-wind-based parameterize,
// the after-thunk captures the "old" value at definition time, causing stale
// restores when composable continuations are invoked in different dynamic
// contexts. Marks-based parameterize fixes this by storing parameter bindings
// as continuation marks that ride on the continuation frames.
// =============================================================================

func TestParameterizeComposableContinuation(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			// Basic: capture inside parameterize, invoke outside.
			// The resumed continuation should see the parameterized value.
			name: "capture inside parameterize invoke outside",
			code: `
				(let ((p (make-parameter 'outer))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p 'inner))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (p))))
				             tag #f)))
				    (call-with-continuation-prompt
				      (lambda () (k 'go))
				      tag #f)))`,
			out: values.NewSymbol("inner"),
		},
		{
			// Parameter is restored after composable continuation returns.
			name: "parameter restored after CC invoke",
			code: `
				(let ((p (make-parameter 'outer))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p 'inner))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (p))))
				             tag #f)))
				    (call-with-continuation-prompt
				      (lambda () (k 'go))
				      tag #f)
				    (p)))`,
			out: values.NewSymbol("outer"),
		},
		{
			// Multiple invocations of same composable continuation.
			// Each should see 'inner and restore correctly.
			name: "multiple invocations restore correctly",
			code: `
				(let ((p (make-parameter 'outer))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p 'inner))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (p))))
				             tag #f)))
				    (let* ((r1 (call-with-continuation-prompt
				                 (lambda () (k 'a)) tag #f))
				           (b1 (p))
				           (r2 (call-with-continuation-prompt
				                 (lambda () (k 'b)) tag #f))
				           (b2 (p)))
				      (list r1 b1 r2 b2))))`,
			out: values.List(
				values.NewSymbol("inner"), values.NewSymbol("outer"),
				values.NewSymbol("inner"), values.NewSymbol("outer"),
			),
		},
		{
			// Nested parameterize: both parameters restored.
			name: "nested parameterize with CC",
			code: `
				(let ((p1 (make-parameter 'p1-outer))
				      (p2 (make-parameter 'p2-outer))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p1 'p1-inner))
				                 (parameterize ((p2 'p2-inner))
				                   (let ((val (call-with-composable-continuation
				                                (lambda (k) k) tag)))
				                     (list (p1) (p2))))))
				             tag #f)))
				    (let ((result (call-with-continuation-prompt
				                    (lambda () (k 'go)) tag #f)))
				      (list result (p1) (p2)))))`,
			out: values.List(
				values.List(values.NewSymbol("p1-inner"), values.NewSymbol("p2-inner")),
				values.NewSymbol("p1-outer"),
				values.NewSymbol("p2-outer"),
			),
		},
		{
			// Converter applied correctly with composable continuation.
			name: "converter applied correctly with CC",
			code: `
				(let ((p (make-parameter 0 (lambda (x) (* x 2))))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p 10))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (p))))
				             tag #f)))
				    (let ((result (call-with-continuation-prompt
				                    (lambda () (k 'go)) tag #f)))
				      (list result (p)))))`,
			out: values.List(values.NewInteger(20), values.NewInteger(0)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestParameterizeMutationClobbering tests that parameter mutations between
// capture and invocation of a composable continuation are not clobbered.
//
// Bug: with dynamic-wind parameterize, the after-thunk captures 'old' at
// definition time. Mutating the parameter between capture and invocation
// means the after-thunk restores the stale captured value, not the mutation.
func TestParameterizeMutationClobbering(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "mutation preserved after CC invoke",
			code: `
				(let ((p (make-parameter 'original))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p 'inner))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (p))))
				             tag #f)))
				    (p 'mutated)
				    (call-with-continuation-prompt
				      (lambda () (k 'go)) tag #f)
				    (p)))`,
			out: values.NewSymbol("mutated"),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestParameterizeOuterDestruction tests that invoking a composable
// continuation inside a different parameterize does not destroy the
// outer parameterize binding.
//
// Bug: with dynamic-wind parameterize, RestoreWithWindingFrom unwinds the
// outer parameterize (firing its after-thunk with a stale 'old' value),
// and the outer's before-thunk never re-fires after the CC finishes.
func TestParameterizeOuterDestruction(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			// Invoke CC captured inside (parameterize ((p 'A)))
			// from within (parameterize ((p 'B))).
			// After CC finishes, p should still be 'B.
			name: "outer parameterize preserved after CC invoke",
			code: `
				(let ((p (make-parameter 'base))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p 'A))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (p))))
				             tag #f)))
				    (parameterize ((p 'B))
				      (let ((result (call-with-continuation-prompt
				                      (lambda () (k 'go)) tag #f)))
				        (list result (p))))))`,
			out: values.List(values.NewSymbol("A"), values.NewSymbol("B")),
		},
		{
			// Reverse: capture inside B, invoke inside A.
			name: "reverse direction preserves outer",
			code: `
				(let ((p (make-parameter 'base))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p 'B))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (p))))
				             tag #f)))
				    (parameterize ((p 'A))
				      (let ((result (call-with-continuation-prompt
				                      (lambda () (k 'go)) tag #f)))
				        (list result (p))))))`,
			out: values.List(values.NewSymbol("B"), values.NewSymbol("A")),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestParameterizeCollateralDamage tests that invoking a composable
// continuation does not corrupt unrelated parameters that are parameterized
// in the invocation context but not in the captured context.
//
// Bug: with dynamic-wind parameterize, RestoreWithWindingFrom unwinds ALL
// current wind frames (including unrelated parameterize frames) to reach
// the captured context, firing their after-thunks and clobbering their values.
func TestParameterizeCollateralDamage(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			// CC captured inside parameterize((p1 'inner)), invoked inside
			// parameterize((p1 'X) (p2 'Y)). p2 should be unaffected
			// inside the CC body, and both should be correct after.
			name: "unrelated parameter not corrupted",
			code: `
				(let ((p1 (make-parameter 'p1-base))
				      (p2 (make-parameter 'p2-base))
				      (tag (make-continuation-prompt-tag 'test)))
				  (let ((k (call-with-continuation-prompt
				             (lambda ()
				               (parameterize ((p1 'p1-inner))
				                 (let ((val (call-with-composable-continuation
				                              (lambda (k) k) tag)))
				                   (list (p1) (p2)))))
				             tag #f)))
				    (parameterize ((p1 'p1-X) (p2 'p2-Y))
				      (let ((result (call-with-continuation-prompt
				                      (lambda () (k 'go)) tag #f)))
				        (list result (p1) (p2))))))`,
			out: values.List(
				values.List(values.NewSymbol("p1-inner"), values.NewSymbol("p2-Y")),
				values.NewSymbol("p1-X"),
				values.NewSymbol("p2-Y"),
			),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

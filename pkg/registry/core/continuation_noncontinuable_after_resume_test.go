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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// REGRESSION (crosscheck 2026-06-28, machine.raiseToHandlers): the non-continuable
// escalation once forwarded a handler's return instead of raising the R7RS §6.11
// secondary exception, gated first on the sticky context-global `isolatedMarks`, then
// on a per-driver resume counter, then on a per-arm "revived" flag that still
// forwarded a guard miss and any re-entry after the guard's exit (decided 2026-10-25:
// match Chez). A return from the handler raises the secondary however its
// continuation was reached; the rows below pin each route that used to forward.
//
// A raise handled inside a sub-context (every dynamic-wind before/after thunk is one)
// arms its finalizer frame on the SUB's chain while the call/cc trampoline reinstates
// on the top driver's; the dynamic-wind rows pin that route.

// TestNonContinuableHandlerReturnErrors: a handler that RETURNS from a
// non-continuable raise must always raise the §6.11 secondary exception:
// whether an unrelated continuation was resumed earlier on the same driver, the
// handler itself takes a call/cc jump inside its own body, or a guard whose
// clauses miss re-raises to a handler that returns. Chez raises in every row.
func TestNonContinuableHandlerReturnErrors(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "no prior resume (baseline control)",
			Code: `(with-exception-handler (lambda (e) 'recovered) (lambda () (raise 'oops)))`,
		},
		{
			// The regression: an unrelated call/cc is captured and re-invoked
			// (setting the sticky isolatedMarks), THEN a non-continuable raise's
			// handler returns normally. Must still escalate, not forward 'recovered.
			Name: "after unrelated call/cc resume on same driver",
			Code: `
(let ((saved #f) (n 0))
  (call-with-current-continuation (lambda (k) (set! saved k) #f))
  (set! n (+ n 1))
  (if (< n 2) (saved #f))
  (with-exception-handler (lambda (e) 'recovered) (lambda () (raise 'oops))))`,
		},
		{
			// The resume happens INSIDE the handler's own dynamic extent; the
			// handler still returns normally, so §6.11 must still fire.
			Name: "call/cc resumed INSIDE the handler",
			Code: `(with-exception-handler
                     (lambda (e) (call/cc (lambda (k) (k 1))) 42)
                     (lambda () (list 'a (error "boom") 'b)))`,
		},
		{
			// Same shape one call deeper. ONE datum: RunSchemeCode reads a single
			// form, so a leading (define (helper) …) would be the whole program and
			// the row would pass vacuously. letrec keeps it one expression.
			Name: "call/cc resumed inside a procedure the handler calls",
			Code: `(letrec ((helper (lambda () (call/cc (lambda (k) (k 'x))))))
                     (with-exception-handler
                       (lambda (e) (helper) 42)
                       (lambda () (list 'a (error "boom") 'b))))`,
		},
		{
			// The condition comes from a primitive's Go error through
			// applyCallableError rather than from `error`, which reaches
			// raiseToHandlers by a different route.
			Name: "call/cc resumed INSIDE the handler, primitive-raised condition",
			Code: `(with-exception-handler
                     (lambda (e) (call/cc (lambda (k) (k 1))) 42)
                     (lambda () (car 5)))`,
		},
		{
			// The raise is handled inside a dynamic-wind AFTER thunk, i.e. on a
			// sub-context's chain, and the (k 1) trampolines out of that sub-context
			// to the top driver, a chain that never held the finalizer frame.
			Name: "call/cc INSIDE a handler running in a dynamic-wind after thunk",
			Code: `(call/cc
                     (lambda (esc)
                       (dynamic-wind
                         (lambda () #f)
                         (lambda () (esc 'ok))
                         (lambda ()
                           (with-exception-handler
                             (lambda (e) (call/cc (lambda (k) (k 1))) 42)
                             (lambda () (error "boom")))))))`,
		},
		{
			// The other winding sub-context: a BEFORE thunk, run by the rewind half
			// of the reconcile when a continuation re-enters the extent from outside.
			Name: "call/cc INSIDE a handler running in a dynamic-wind before thunk",
			Code: `(let ((saved #f) (n 0))
                     (dynamic-wind
                       (lambda ()
                         (set! n (+ n 1))
                         (if (> n 1)
                             (with-exception-handler
                               (lambda (e) (call/cc (lambda (k) (k 1))) 42)
                               (lambda () (error "boom")))))
                       (lambda () (call/cc (lambda (k) (set! saved k))) 'inside)
                       (lambda () #f))
                     (if (< n 2) (saved #f))
                     'done)`,
		},
		{
			// A guard whose clauses miss re-raises with raise-continuable in the
			// dynamic environment of the raise (R7RS 4.2.7); the outer handler's
			// value returns from guard's handler, which raises the secondary.
			// This program forwarded (outer 99) before 2026-10-25.
			Name: "guard miss re-raises continuable to a returning outer handler",
			Code: `(with-exception-handler
                     (lambda (e) (list 'outer e))
                     (lambda () (guard (x ((string? x) 'str)) (raise 99))))`,
		},
		{
			// The same route under an expression context: it answered 43.
			Name: "guard miss under an enclosing expression",
			Code: `(with-exception-handler
                     (lambda (e) 42)
                     (lambda () (+ 1 (guard (e (#f 0)) (raise 'x)))))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, strings.Contains(err.Error(), "non-continuable"), qt.IsTrue,
				qt.Commentf("expected a non-continuable secondary exception, got: %v", err))
		})
	}
}

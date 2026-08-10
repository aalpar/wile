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

// REGRESSION (crosscheck 2026-06-28, exception_raise.go:133): the non-continuable
// escalation in raiseToHandlers gated the R7RS §6.11 secondary exception on the
// CONTEXT-GLOBAL, sticky `isolatedMarks` flag. Once ANY call/cc continuation had
// been resumed on the driver, isolatedMarks stayed true, so a later non-continuable
// `raise` whose handler returned NORMALLY took the "forward the value" branch — the
// mandatory secondary exception was silently swallowed and the illegal handler
// return was converted into a normal value.
//
// The replacement, a per-driver `resumeGeneration` counter, fixed that and was
// still too coarse: it answered "did any resume happen between arm and fire", so
// a call/cc escape taken INSIDE the handler — which never leaves the escalator
// frame's extent — cleared the gate too. The rows below pin that second failure.
//
// The signal must be per-arm and directional: forward ONLY when a reinstatement
// REVIVED this escalator frame (the segment carried it, the live chain did not),
// which is the guard re-raise-continuable case.

// TestNonContinuableHandlerReturnErrors: a handler that RETURNS from a
// non-continuable raise must always raise the §6.11 secondary exception —
// whether an unrelated continuation was resumed earlier on the same driver, or
// the handler itself takes a call/cc jump inside its own body.
func TestNonContinuableHandlerReturnErrors(t *testing.T) {
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
			// The resume happens INSIDE the handler's own dynamic extent, so the
			// escalator frame is on the live chain when the segment is reinstated:
			// a within-extent jump, not a revival. The handler still returns
			// normally, so §6.11 must still fire.
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

// TestNonContinuableForwardBranchPreserved: the load-bearing forward branch must
// survive the fix. A guard whose clauses miss re-raises via raise-continuable to an
// outer handler; that handler's value RESUMES guard's handler-k back THROUGH the
// escalator frame and must be forwarded, not escalated. (Same program as the golden
// corpus cell `guard-reraise-to-weh`, pinned here next to the regression it must not
// trade against.)
func TestNonContinuableForwardBranchPreserved(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "guard miss re-raises continuable to outer weh",
			Code: `(with-exception-handler
                     (lambda (e) (list 'outer e))
                     (lambda () (guard (x ((string? x) 'str)) (raise 99))))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, "(outer 99)")
		})
	}
}

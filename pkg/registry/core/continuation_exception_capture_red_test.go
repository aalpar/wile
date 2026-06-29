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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// TestExceptionHandlerCapturedByContinuation is a GREEN regression guard for the
// exception-handler facet of the sub-context continuation fix (claim 3). See
// memory/2026-06-26-subcontext-continuation-the-open-problem.local.md.
//
// The current exception handler is part of the dynamic environment (R7RS §6.11), so a
// continuation captured inside (with-exception-handler H ...) must resume with H
// current. The handler now rides the %exception-handlers parameter (a continuation
// mark), so call/cc captures it and re-entry restores it. (Before claim 3 the handler
// lived on an off-chain MachineContext field that call/cc never copied; this probe was
// RED and t.Skip-guarded. The field is gone and the test now passes.)
//
// The probe captures k under an inner handler, exits the with-exception-handler, then
// re-invokes k under a different (sentinel) handler and raises; the CAPTURED inner
// handler must fire:
//
//	-> (inner-result inner)   [not the invocation-site (sentinel-result sentinel)]
func TestExceptionHandlerCapturedByContinuation(t *testing.T) {
	const code = `
(call/cc (lambda (done)
  (let ((k #f) (log '()))
    ;; Phase 1: capture k under the INNER handler, then escape out normally
    ;; (return-1 'captured) so the with-exception-handler is exited and INNER popped.
    (call/cc (lambda (return-1)
      (with-exception-handler
        (lambda (e) (set! log (cons 'inner log))
                    (done (cons 'inner-result (reverse log))))
        (lambda ()
          (call/cc (lambda (c) (set! k c) (return-1 'captured)))
          (raise 'boom)))))            ; only reached on re-entry of k
    ;; Phase 2: re-invoke k under a SENTINEL handler. k resumes after its capture
    ;; and evaluates (raise 'boom). Whichever handler is in force fires.
    (with-exception-handler
      (lambda (e) (set! log (cons 'sentinel log))
                  (done (cons 'sentinel-result (reverse log))))
      (lambda () (k 'go)))
    (done (cons 'no-raise (reverse log))))))`

	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	// The captured inner handler must fire on re-entry.
	qt.Assert(t, result.SchemeString(), qt.Equals, "(inner-result inner)")
}

// --- step-0 sibling probes (scaffold). Colors confirmed by running un-skipped;
// red ones are t.Skip-guarded, green ones are regression guards the handler-as-mark
// redesign must preserve. See the plan's "Next steps for the fix".

// TestExceptionHandlerSelfReRaiseEscalates (a): a re-raise inside a handler must
// reach the OUTER handler, not re-enter the same one (R7RS §6.11). HEAD pops the
// handler before calling it (prim_exceptions.go:145), so this is GREEN today — a
// regression guard the mark-based design must preserve (raise must re-mark to the
// cdr so the handler runs with the parent current).
func TestExceptionHandlerSelfReRaiseEscalates(t *testing.T) {
	const code = `
(call/cc (lambda (done)
  (with-exception-handler
    (lambda (e) (done (list 'outer e)))
    (lambda ()
      (with-exception-handler
        (lambda (e) (raise (list 'reraised e)))
        (lambda () (raise 'original)))))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(outer (reraised original))")
}

// TestRaiseContinuableThroughReinvokedContinuation (b): a continuation captured under
// a continuable handler, re-invoked, resumes (raise-continuable ...) with the CAPTURED
// handler's value, not the invocation-site handler's. The raise-continuable analogue
// of the headline guard; GREEN since claim 3 (was RED/skipped on HEAD 9540d515:
// got "(resumed-with sentinel-value)", want "(resumed-with inner-value)").
func TestRaiseContinuableThroughReinvokedContinuation(t *testing.T) {
	const code = `
(call/cc (lambda (done)
  (let ((k #f) (first #t))
    (with-exception-handler
      (lambda (e) 'inner-value)
      (lambda ()
        (call/cc (lambda (c) (set! k c)))
        (if first
            'first-pass
            (done (list 'resumed-with (raise-continuable 'x))))))
    (set! first #f)
    (with-exception-handler
      (lambda (e) 'sentinel-value)
      (lambda () (k 'go)))
    (done 'no-raise))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(resumed-with inner-value)")
}

// TestGoPrimitiveErrorCrossesPromptToHandler (c): a Go-level primitive error
// ((car 5)) raised inside a call/cc prompt must reach the enclosing guard handler.
// Exercises the bridge the mark-based redesign must keep working (step 3): the VM's
// foreign-error path must find the handler across an intervening prompt.
func TestGoPrimitiveErrorCrossesPromptToHandler(t *testing.T) {
	const code = `
(guard (e (#t 'caught))
  (call/cc (lambda (escape)
    (car 5))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "caught")
}

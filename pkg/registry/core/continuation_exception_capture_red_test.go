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

// TestExceptionHandlerNotCapturedByContinuation is a RED characterization of the
// open sub-context continuation bug, exception-handler facet. See
// plans/2026-06-26-subcontext-continuation-the-open-problem.local.md
// ("The two windings, in struct fields" + "Fact 2").
//
// The current exception handler is part of the dynamic environment (R7RS §6.11),
// so a continuation captured inside (with-exception-handler H ...) must resume
// with H current. Wile stores the handler on a context field
// (MachineContext.exceptionHandler, machine_context.go:71) that call/cc never
// copies into the continuation value — so on re-invocation the live handler is
// the INVOKER's, not the captured one.
//
// The probe captures k under an inner handler, exits the with-exception-handler,
// then re-invokes k under a different (sentinel) handler and raises:
//
//	TARGET (green): the captured inner handler fires  -> (inner-result inner)
//	CURRENT (red):  the invocation-site sentinel fires -> (sentinel-result sentinel)
//
// SKIP until the handler is reified into the captured continuation (handler-as-
// mark; see the plan's "The target" diagram and "Next steps").
func TestExceptionHandlerNotCapturedByContinuation(t *testing.T) {
	t.Skip("RED: handler not captured by continuation; unskip when handler-as-mark lands (plans/2026-06-26-subcontext-continuation-the-open-problem.local.md)")

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

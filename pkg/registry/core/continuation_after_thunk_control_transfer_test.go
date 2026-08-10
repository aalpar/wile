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
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// Defect 27 (review wave 3, item 18). The driver's two RECONCILE paths returned
// their error unclassified while every other arm routed control signals through
// the errors.As chain. An after-thunk fired during a winding reconcile can itself
// transfer control, and that transfer was handed to the embedder verbatim:
//
//	Error: runtime error: abort to prompt #<continuation-prompt-tag:exit>
//	exit 1
//
// The two sites are ReinstallSegment's error in the resume arm and
// resolveAbort's, both in machine_context.go; both now re-enter the driver's own
// dispatch loop via isControlSignal.
//
// The table pairs the two leaking shapes with two that already worked, in one
// function, so the gate is shown to DISCRIMINATE rather than merely to pass. The
// no-exception row is why the fix classifies by control-signal-ness rather than
// by exception provenance: a fix that re-routed only exception-derived aborts
// would leave it broken.
func TestAfterThunkControlTransferReachesDriver(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			// LEAKED. No exception anywhere: the after-thunk escapes to an OUTER
			// call-with-exit while the reconcile is running it.
			Name: "no exception, after-thunk escapes to an outer exit",
			Code: `
(call-with-exit
  (lambda (outer)
    (call/cc
      (lambda (k)
        (dynamic-wind (lambda () #f)
                      (lambda () (k 'escaped))
                      (lambda () (outer "from-after")))))))`,
			Expected: values.NewString("from-after"),
		},
		{
			// LEAKED, and this is the commoner shape: guard's own escape is
			// call-with-exit, so a raise in the after-thunk caught by a guard
			// placed AROUND the call/cc produces the identical abort.
			Name: "guard around the call/cc, raise inside the after-thunk",
			Code: `
(guard (e (#t (string-append "caught:" (symbol->string e))))
  (call/cc
    (lambda (k)
      (dynamic-wind (lambda () #f)
                    (lambda () (k 'escaped))
                    (lambda () (raise 'boom))))))`,
			Expected: values.NewString("caught:boom"),
		},
		{
			// CONTROL, already worked: no call/cc escape, so the after-thunk runs
			// on the ordinary unwind path rather than through a reconcile.
			Name: "control: non-call/cc escape catches correctly",
			Code: `
(guard (e (#t (string-append "caught:" (symbol->string e))))
  (dynamic-wind (lambda () #f)
                (lambda () 'body)
                (lambda () (raise 'boom))))`,
			Expected: values.NewString("caught:boom"),
		},
		{
			// CONTROL, already worked: the guard is written ENTIRELY inside the
			// after-thunk, so nothing escapes the reconcile.
			Name: "control: guard entirely inside the after-thunk",
			Code: `
(let ((log "none"))
  (call/cc
    (lambda (k)
      (dynamic-wind (lambda () #f)
                    (lambda () (k 'escaped))
                    (lambda () (set! log (guard (e (#t "caught")) (raise 'boom)))))))
  log)`,
			Expected: values.NewString("caught"),
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

// Defect 26 (review wave 3, item 18). abort-current-continuation was the one
// ErrPromptAbort emitter that did not stamp SourceWinding — call-with-exit
// (prim_exit.go) and the value-delivery abort both do. Without it the driver
// reconciles dynamic-wind from its OWN winding, so after-thunks pushed inside a
// deeper foreign sub-context never run.
//
// CORRECTION to the filed repro: the design plan's vehicle is a PARAMETER
// CONVERTER, and that no longer reproduces. prim_parameters.go now applies the
// converter "on the LIVE chain (not a sub-context)" — the fix for the
// parameterize-converter case of continuation_subcontext_truncation_red_test.go
// — so the converter's winding is the driver's winding and there is nothing to
// lose. `eval` is the vehicle that still reaches a deeper sub-context, and it
// reproduces the filed trace exactly: (in out in) instead of (in out in out).
func TestAbortCurrentContinuationRunsDeepSubAfterThunks(t *testing.T) {
	// Both runs abort from inside the same dynamic-wind. The first runs it in
	// ordinary code (the control, which already worked); the second runs it under
	// eval, one foreign sub-context deeper. Each must contribute in AND out.
	const code = `
(begin
  (define trace '())
  (define (note! x) (set! trace (append trace (list x))))
  (define tag (make-continuation-prompt-tag))
  (define (deep-abort)
    (dynamic-wind (lambda () (note! 'in))
                  (lambda () (abort-current-continuation tag 'deep))
                  (lambda () (note! 'out))))
  (define (under-prompt thunk)
    (call-with-continuation-prompt thunk tag (lambda args 'aborted)))
  (under-prompt deep-abort)
  (under-prompt (lambda () (eval '(deep-abort) (interaction-environment))))
  trace)`

	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(in out in out)")
}

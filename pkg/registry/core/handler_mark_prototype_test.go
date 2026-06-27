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

// Step-1 falsification (plan "Next steps for the fix"): prototype the
// handler-as-mark model in PURE SCHEME on the existing mark primitives — no VM
// change — and run the probes against it. If self-catch loops or the captured
// handler is still lost, the candidate dies here, cheaply, before any Go work.
//
// The model: the current exception-handler stack is an IMMUTABLE list stored in a
// single continuation mark. with-exception-handler re-marks with (cons H stack);
// raise reads the innermost mark (the whole stack), and re-marks to its cdr for the
// handler call so a re-raise escalates to the parent. No mutation, no Go field.
const handlerMarkPrototype = `
  (define exc-key (list 'exc))
  (define (default-h obj) (error "uncaught-exception" obj))
  (define (handlers)
    (continuation-mark-set-first (current-continuation-marks) exc-key (list default-h)))
  (define (p-weh handler thunk)
    (with-continuation-mark exc-key (cons handler (handlers)) (thunk)))
  (define (p-raise obj)
    (let* ((hs (handlers)) (h (car hs)) (parent (cdr hs)))
      (with-continuation-mark exc-key parent (h obj))
      (error "non-continuable-raise-handler-returned" obj)))
  (define (p-raise-continuable obj)
    (let* ((hs (handlers)) (h (car hs)) (parent (cdr hs)))
      (with-continuation-mark exc-key parent (h obj))))
`

func runProto(t *testing.T, probe string) string {
	t.Helper()
	code := "(let ()\n" + handlerMarkPrototype + "\n" + probe + ")"
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	return result.SchemeString()
}

// Probe 1 (headline capture): a continuation captured under the mark-based inner
// handler, re-invoked under a sentinel, must resume with the CAPTURED handler.
func TestHandlerAsMarkPrototype_Capture(t *testing.T) {
	got := runProto(t, `
(call/cc (lambda (done)
  (let ((k #f) (log '()))
    (call/cc (lambda (return-1)
      (p-weh
        (lambda (e) (set! log (cons 'inner log)) (done (cons 'inner-result (reverse log))))
        (lambda ()
          (call/cc (lambda (c) (set! k c) (return-1 'captured)))
          (p-raise 'boom)))))
    (p-weh
      (lambda (e) (set! log (cons 'sentinel log)) (done (cons 'sentinel-result (reverse log))))
      (lambda () (k 'go)))
    (done (cons 'no-raise (reverse log))))))`)
	qt.Assert(t, got, qt.Equals, "(inner-result inner)")
}

// Probe 2 (self-catch escalation): a re-raise inside a handler must reach the OUTER
// handler, not loop on itself.
func TestHandlerAsMarkPrototype_SelfReRaiseEscalates(t *testing.T) {
	got := runProto(t, `
(call/cc (lambda (done)
  (p-weh
    (lambda (e) (done (list 'outer e)))
    (lambda ()
      (p-weh
        (lambda (e) (p-raise (list 'reraised e)))
        (lambda () (p-raise 'original)))))))`)
	qt.Assert(t, got, qt.Equals, "(outer (reraised original))")
}

// Probe 3 (raise-continuable through re-invoke): resume returns the captured
// handler's value at the raise site.
func TestHandlerAsMarkPrototype_RaiseContinuableThroughReinvoke(t *testing.T) {
	got := runProto(t, `
(call/cc (lambda (done)
  (let ((k #f) (first #t))
    (p-weh
      (lambda (e) 'inner-value)
      (lambda ()
        (call/cc (lambda (c) (set! k c)))
        (if first 'first-pass (done (list 'resumed-with (p-raise-continuable 'x))))))
    (set! first #f)
    (p-weh
      (lambda (e) 'sentinel-value)
      (lambda () (k 'go)))
    (done 'no-raise))))`)
	qt.Assert(t, got, qt.Equals, "(resumed-with inner-value)")
}

// Probe 4 (step 2: prompt-bounded mark lookup): the handler is installed OUTSIDE an
// explicit continuation prompt; a raise INSIDE the prompt must still find it. This
// is the failure mode flagged as most likely in the plan -- current-continuation-marks
// is documented as prompt-bounded (cont_marks.go:28). If the lookup stops at the
// inner prompt, the handler is missed and default-h errors "uncaught-exception".
func TestHandlerAsMarkPrototype_HandlerVisibleAcrossPrompt(t *testing.T) {
	t.Skip("OPEN obstacle (step 2): a handler installed above an explicit " +
		"call-with-continuation-prompt is NOT visible to a raise inside it -- " +
		"current-continuation-marks is prompt-bounded (cont_marks.go:28), and the " +
		"default-prompt-tag lookup variant does not fix it either (verified). Needs " +
		"VM-level design (likely the call-with-continuation-prompt sub-context/mark " +
		"interaction), not a Scheme one-liner. NOTE: guard escapes via call/cc to " +
		"DefaultPromptTag and DOES cross (see the Capture probe), so this bites only " +
		"explicit nested prompts -- scope the real fix accordingly.")

	got := runProto(t, `
(let ((tag (make-continuation-prompt-tag 'p)))
  (call/cc (lambda (done)
    (p-weh
      (lambda (e) (done (list 'found e)))
      (lambda ()
        (call-with-continuation-prompt
          (lambda () (p-raise 'inside))
          tag
          (lambda args 'aborted)))))))`)
	qt.Assert(t, got, qt.Equals, "(found inside)")
}

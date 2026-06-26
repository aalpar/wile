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

package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// These tests pin the fix for the C1 continuation re-entry bug. A continuation
// captured inside a procedure whose activation frame is later returned to the
// env-frame pool must survive re-invocation: the captured continuation aliases
// that frame (MachineContinuation.Copy shares env pointers), so the live chain
// must not recycle it. Before the fix, re-invoking such a continuation raised
// "no such local binding 0:1" because the frame had been pooled out from under
// the captured segment on the procedure's first normal return.
//
// The fix marks the live continuation chain shared at capture
// (SliceContinuationAt -> MarkChainShared) so a normal return through a captured
// frame keeps the old env for GC instead of pooling it. The bug was PRE-EXISTING
// (predates the frame-reclaim / immutable-top-level arc) and is NOT a
// frame-reclaim classifier defect — the classifier correctly gives the inner
// procedure no frame reuse.

// reEntrantCounterBody is the canonical C1 shape: a procedure captures a
// continuation, then a later re-invocation resumes inside that procedure, which
// must still reach an enclosing binding it set!s and loop until n == 3.
const reEntrantCounterBody = `(step)
   (if (< n 3) (k* #f))
   n`

func evalCounter(t *testing.T, src string) (string, error) {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()
	result, evalErr := eng.EvalMultiple(ctx, src)
	if evalErr != nil {
		return "", evalErr
	}
	return result.SchemeString(), nil
}

// TestContinuationReentryInternalDefine is the regression test for C1: a
// continuation captured inside an INTERNAL define in a let body must reach the
// enclosing let frame on re-entry. The defining procedure's activation frame is
// pooled on its first normal return, but the captured continuation re-enters it,
// so the frame must not be recycled. The counter must run to n == 3.
func TestContinuationReentryInternalDefine(t *testing.T) {
	src := `(let ((n 0) (k* #f))
              (define (step)
                (call/cc (lambda (k) (set! k* k)))
                (set! n (+ n 1)))
              ` + reEntrantCounterBody + `)`
	got, err := evalCounter(t, src)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "3")
}

// TestContinuationReentryVariantsConverge pins the sibling shapes that always
// worked (the bug was specific to the internal-define-in-let frame layout) plus
// two deeper shapes the fix must also keep correct, so a future change to the
// env-frame pool keeps every re-entrant-counter form correct, not just the one
// that previously failed.
func TestContinuationReentryVariantsConverge(t *testing.T) {
	tcs := []struct {
		name string
		src  string
	}{
		{
			name: "letrec",
			src: `(letrec ((n 0) (k* #f)
                          (step (lambda ()
                                  (call/cc (lambda (k) (set! k* k)))
                                  (set! n (+ n 1)))))
                   ` + reEntrantCounterBody + `)`,
		},
		{
			name: "nested let with lambda",
			src: `(let ((n 0) (k* #f))
                   (let ((step (lambda ()
                                 (call/cc (lambda (k) (set! k* k)))
                                 (set! n (+ n 1)))))
                     ` + reEntrantCounterBody + `))`,
		},
		{
			name: "direct let body, no procedure",
			src: `(let ((n 0) (k* #f))
                   (call/cc (lambda (k) (set! k* k)))
                   (set! n (+ n 1))
                   (if (< n 3) (k* #f))
                   n)`,
		},
		{
			// Internal define one frame deeper: the re-entered binding lives in
			// a transitive lexical parent, exercising the parent-chain reach.
			name: "internal define under inner let",
			src: `(let ((n 0) (k* #f))
                   (let ((bump 1))
                     (define (step)
                       (call/cc (lambda (k) (set! k* k)))
                       (set! n (+ n bump)))
                     ` + reEntrantCounterBody + `))`,
		},
		{
			// Internal define inside a lambda body (not a let): the proc
			// activation frame is still pooled on return while the captured
			// continuation re-enters it.
			name: "internal define in lambda body",
			src: `((lambda ()
                    (define n 0)
                    (define k* #f)
                    (define (step)
                      (call/cc (lambda (k) (set! k* k)))
                      (set! n (+ n 1)))
                    ` + reEntrantCounterBody + `))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalCounter(t, tc.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "3")
		})
	}
}

// TestContinuationReentryCoroutinePingPong exercises two continuations captured
// in distinct internal-define procedures that resume each other — the stress
// shape the prior frame-recycling reverts crashed on (use-after-release). Each
// ping/pong frame is pooled on its normal return yet re-entered by the other's
// captured continuation, so both activation frames must stay live.
func TestContinuationReentryCoroutinePingPong(t *testing.T) {
	src := `(let ((out '()) (kp #f) (kq #f) (turns 0))
            (define (ping)
              (call/cc (lambda (k) (set! kp k)))
              (set! out (cons 'ping out)))
            (define (pong)
              (call/cc (lambda (k) (set! kq k)))
              (set! out (cons 'pong out)))
            (ping)
            (pong)
            (set! turns (+ turns 1))
            (if (< turns 3)
                (if (even? turns) (kp #f) (kq #f)))
            (reverse out))`
	got, err := evalCounter(t, src)
	qt.Assert(t, err, qt.IsNil)
	// Each re-entry resumes inside a captured ping/pong frame and continues the
	// rest of the let body. The exact ordered trace pins that every resume runs
	// once — no dropped or double-run resume (the reverted recyclers' failure
	// modes) and no use-after-release on the pooled-then-re-entered frames.
	qt.Assert(t, got, qt.Equals, "(ping pong pong ping pong)")
}

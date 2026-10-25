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

// The value-level gate for review-wave-1 item 5: a raise primitive without
// InvokesProcedure is stamped capture-safe, so a self-tail loop calling it arms
// OpSelfTailCall, whose in-place slot rebind corrupts a continuation captured
// over that activation's parameter frame. The stamps on the raise family
// (pkg/registry/core/exceptions.go) close it.
//
// THE PROGRAM USES raise-continuable. The gate needs a handler whose return
// resumes the loop at the raise, so a replayed continuation re-enters an
// activation the opcode rebinds in place. Only raise-continuable sanctions that
// return (R7RS 6.11): a handler returning from `error` or `raise` raises a
// secondary exception, so no program can resume a loop through them (decided
// 2026-10-25), and their stamps are held by the arming rows of
// TestSelfTailArmingCountRatchet instead. With the stamp on raise-continuable
// removed the loop arms one site and this program answers the corrupt value
// below.
//
// WHY IT IS HERE AND NOT IN continuation_escape_past_oracle_test.go. That file
// runs every probe through testhelpers.RunSchemeCode, a MUTABLE top level, where
// no global is Stable, bodyCalleesAllCaptureSafe refuses, and NOTHING arms
// OpSelfTailCall: a gate there is green for the wrong reason. The Engine
// (WithProfile, immutable top level by default) is the harness that arms it.
//
// The raise happens at a LATER iteration (n=3 of a loop entered at n=5), so the
// raising activation is one the opcode already rebound in place and will rebind
// again on resume; a raise on the first iteration abandons the activation before
// pc reaches the self-tail call. The escape is a full call/cc continuation
// (`home`), not guard/call-with-exit, because a guard's exit procedure expires
// across a replay and the driver must be re-enterable.

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/wile"
)

// raiseSelfTailProgram is the gate program.
//
// `visited` is the per-pass list of loop iterations; `raised` is the
// (raised-at n) observable, one entry per raise, in raise order across all
// three passes, so the assertion pins the exact re-raise ordering.
//
// Pass 0 runs the loop 5,4,3 and raises at n=3; the handler records
// (raised-at 3), captures k, and escapes to `home`. Each replay of k resumes the
// handler, which returns #f as the value of the n=3 raise-continuable, so the
// loop continues from n=3 with `(lp (- n 1))`. Replaying the SAME continuation
// twice must continue it the same way twice.
const raiseSelfTailProgram = `
(let ((k #f) (home #f) (visited '()) (raised '()) (shots 0) (results '()))
  (define (h n)
    (set! raised (cons (list 'raised-at n) raised))
    (if (not k)
        (if (call/cc (lambda (c) (set! k c) #t))
            (home 'captured)
            #f)
        (home 'other)))
  (define (body)
    (with-exception-handler h
      (lambda ()
        (let lp ((n 5))
          (if (<= n 0)
              'done
              (begin (set! visited (cons n visited))
                     (if (or (= n 3) (= n 1)) (raise-continuable n) #f)
                     (lp (- n 1))))))))
  (call/cc (lambda (c) (set! home c) (body)))
  (set! results (cons (reverse visited) results))
  (set! visited '())
  (if (< shots 2)
      (begin (set! shots (+ shots 1)) (k #f))
      (list (reverse results) (reverse raised))))
`

// raiseSelfTailCorrect is what a capture-safe raise produces: the two replays
// of one continuation continue the loop identically (2 then 1), and each replay
// reaches the n=1 raise, so (raised-at 1) appears twice.
//
// Without the stamp the program answers
// `(((5 4 3) (2 1) ()) ((raised-at 3) (raised-at 1)))`: OpSelfTailCall rebinds
// slot 0 of the CAPTURED activation to 1 during the first replay, so the second
// replay of the SAME continuation computes (- 1 1) = 0, terminates immediately,
// and never reaches the n=1 raise.
const raiseSelfTailCorrect = `(((5 4 3) (2 1) (2 1)) ((raised-at 3) (raised-at 1) (raised-at 1)))`

// runGateProgram evaluates src on a fresh KitchenSink engine and returns the
// printed result. KitchenSink (not the testhelpers namespace) is load-bearing:
// see the file header on why a mutable top level never arms the opcode.
func runGateProgram(t *testing.T, src string) string {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	v, err := eng.EvalMultiple(ctx, src)
	qt.Assert(t, err, qt.IsNil)
	return v.SchemeString()
}

// TestRaiseSelfTailCaptureGate_Target is the acceptance criterion for
// review-wave-1 item 5: replaying one continuation captured in a
// raise-continuable handler inside a self-tail loop continues the loop
// identically each time. Removing InvokesProcedure from raise-continuable turns
// it red (the corrupt value above).
func TestRaiseSelfTailCaptureGate_Target(t *testing.T) {
	t.Parallel()

	got := runGateProgram(t, raiseSelfTailProgram)
	qt.Assert(t, got, qt.Equals, raiseSelfTailCorrect,
		qt.Commentf("replaying one continuation twice must continue the loop identically; "+
			"a differing third pass means OpSelfTailCall rebound the captured activation's "+
			"parameter frame in place"))
}

// TestRaiseSelfTailCaptureGate_HarnessNonBlindness pins the reason this gate is
// not in continuation_escape_past_oracle_test.go, so a later "simplification"
// onto testhelpers.RunSchemeCode cannot silently turn it green.
//
// Hold the program fixed and vary only the top level's mutability. Under
// WithMutableTopLevel no global is Stable, bodyCalleesAllCaptureSafe refuses,
// and the loop arms ZERO OpSelfTailCall sites, so the program reports the
// correct value FOR THE WRONG REASON: the opcode was never executed, stamp or no
// stamp.
func TestRaiseSelfTailCaptureGate_HarnessNonBlindness(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(ctx,
		`(define (loopc n) (if (= n 0) 'done (begin (raise-continuable n) (loopc (- n 1)))))`)
	qt.Assert(t, err, qt.IsNil)
	v, ok := eng.Get("loopc")
	qt.Assert(t, ok, qt.IsTrue)
	closure, ok := v.Internal().(*machine.MachineClosure)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, selfTailSites(closure.Template()), qt.Equals, 0,
		qt.Commentf("a mutable top level leaves every global non-Stable, so the loop must not arm"))

	got, err := eng.EvalMultiple(ctx, raiseSelfTailProgram)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, raiseSelfTailCorrect,
		qt.Commentf("unarmed, the same program reports the correct value, which is exactly "+
			"why a harness with a mutable top level cannot gate this defect"))
}

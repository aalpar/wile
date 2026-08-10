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

// The value-level gate for review-wave-1 item 5: `error` lacks InvokesProcedure,
// so a self-tail loop that calls it is stamped capture-safe and armed for
// OpSelfTailCall, whose in-place slot rebind corrupts a continuation captured
// over that activation's parameter frame.
//
// WHY IT IS HERE AND NOT IN continuation_escape_past_oracle_test.go. The plan
// (§5.4/§8 Q7) says to build this on that file, whose capture-and-replay idiom
// this program reuses. It cannot live there: that file runs every probe through
// testhelpers.RunSchemeCode -> bootstrap.NewNamespaceFrame, which is a MUTABLE
// top level. bodyCalleesAllCaptureSafe demands each callee be CaptureSafe AND
// Stable (Imported || Stable), and under a mutable top level no global is
// Stable, so NOTHING in that harness ever arms OpSelfTailCall. Measured: the
// identical program run through testhelpers executes the opcode zero times and
// returns the CORRECT value. A gate written there would be green for the wrong
// reason. The Engine (WithProfile, immutable top level by default) is the only
// harness that arms it, which is why this file is in pkg/wile.
//
// WHY THE §5.4 PROGRAM VERBATIM DOES NOT DISCRIMINATE. §5.4 proposes
//
//	(define (loop n) (if (= n 0) 'done (begin (error "tick" n) (loop (- n 1)))))
//
// with k captured in the handler at n=3. That program ARMS the opcode (1 site,
// against 0 for the `raise` twin) but never EXECUTES it: `error` raises on the
// FIRST iteration, so the activation is abandoned at the raise, before pc ever
// reaches the self-tail call, and every resume re-enters `loop` through a fresh
// application with a fresh frame. Measured: OpSelfTailCall executes 0 times and
// the observable is correct on master. The raise must therefore happen at a
// LATER iteration, so the raising activation is one the opcode already rebound
// in place and will rebind again on resume. That is the single change below:
// (if (= n 3) (error "tick" n) #f) inside a loop entered at n=5.
//
// THE CONTINUABLE SUBSTITUTE IS `error` ITSELF, NOT raise-continuable.
// raise-continuable carries InvokesProcedure: true, so a loop calling it deopts
// to PullApply (0 sites) and cannot exercise the defect at all. What makes the
// non-continuable raise resume here is RaiseInPlace's escalator generation
// check (exception_raise.go): replaying a continuation captured inside the
// handler bumps the driver's resume generation, so the finalizer FORWARDS the
// handler's return value as the value of the `error` expression instead of
// escalating. That is the continuable behaviour §5.4 asks for, obtained from the
// primitive under test rather than from a substitute that deopts.
//
// The one-shot replay guard is the oracle file's idiom (`(set! k …)` captured
// once, `(not k)` gating re-capture); the escape is a full call/cc continuation
// (`home`) and NOT guard/call-with-exit, because a guard's exit procedure
// expires across a replay ("expired escape procedure") and the driver must be
// re-enterable.

import (
	"context"
	"os"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/wile"
)

// errorSelfTailRaisedAtProgram is the §5.4 gate program.
//
// `visited` is the per-pass list of loop iterations; `raised` is the
// (raised-at n) observable — one entry per condition raise, in raise order
// across all three passes, so the assertion pins the exact re-raise ordering.
//
// Pass 0 runs the loop 5,4,3 and raises at n=3; the handler records
// (raised-at 3), captures k, and escapes to `home`. Each replay of k resumes the
// handler, returns #f, and the escalator forwards it as the value of the n=3
// `error` — so the loop continues from n=3 with `(lp (- n 1))`. Replaying the
// SAME continuation twice must continue it the same way twice.
const errorSelfTailRaisedAtProgram = `
(let ((k #f) (home #f) (visited '()) (raised '()) (shots 0) (results '()))
  (define (h e)
    (let ((n (car (error-object-irritants e))))
      (set! raised (cons (list 'raised-at n) raised))
      (if (not k)
          (if (call/cc (lambda (c) (set! k c) #t))
              (home 'captured)
              #f)
          (home 'other))))
  (define (body)
    (with-exception-handler h
      (lambda ()
        (let lp ((n 5))
          (if (<= n 0)
              'done
              (begin (set! visited (cons n visited))
                     (if (or (= n 3) (= n 1)) (error "tick" n) #f)
                     (lp (- n 1))))))))
  (call/cc (lambda (c) (set! home c) (body)))
  (set! results (cons (reverse visited) results))
  (set! visited '())
  (if (< shots 2)
      (begin (set! shots (+ shots 1)) (k #f))
      (list (reverse results) (reverse raised))))
`

const (
	// errorSelfTailCorrect is what a capture-safe `error` produces: the two
	// replays of one continuation continue the loop identically (2 then 1), and
	// each replay reaches the n=1 raise, so (raised-at 1) appears twice.
	errorSelfTailCorrect = `(((5 4 3) (2 1) (2 1)) ((raised-at 3) (raised-at 1) (raised-at 1)))`
	// errorSelfTailCorrupt is master's answer. OpSelfTailCall rebound slot 0 of
	// the captured activation to 1 during the first replay, so the second replay
	// of the SAME continuation computes (- 1 1) = 0, terminates immediately, and
	// never reaches the n=1 raise: the third pass visits nothing and the third
	// (raised-at 1) is missing.
	errorSelfTailCorrupt = `(((5 4 3) (2 1) ()) ((raised-at 3) (raised-at 1)))`
)

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

// TestErrorSelfTailCaptureGate_Target is the ACCEPTANCE CRITERION for review-wave-1
// item 5 change 1 (InvokesProcedure: true on the `error` spec,
// pkg/registry/core/exceptions.go). It asserts the CORRECT value and is RED on
// master today, so it is gated behind WILE_RUN_RED_CONTINUATION=1 — the idiom
// continuation_escape_past_oracle_test.go used for its own RED targets. When the
// annotation lands: drop the gate, and delete the two
// DocumentsCurrentMasterBug tests below (their failure then signals the fix).
func TestErrorSelfTailCaptureGate_Target(t *testing.T) {
	if os.Getenv("WILE_RUN_RED_CONTINUATION") == "" {
		t.Skip("RED target: set WILE_RUN_RED_CONTINUATION=1 (acceptance criterion for InvokesProcedure on `error`)")
	}
	got := runGateProgram(t, errorSelfTailRaisedAtProgram)
	qt.Assert(t, got, qt.Equals, errorSelfTailCorrect,
		qt.Commentf("replaying one continuation twice must continue the loop identically; "+
			"a differing third pass means OpSelfTailCall rebound the captured activation's "+
			"parameter frame in place"))
}

// TestErrorSelfTailCaptureGate_DocumentsCurrentMasterBug is the always-on tripwire
// proving the defect is real on master right now, so the skipped target above is
// not mistaken for a hypothetical. It asserts the WRONG value and MUST be deleted
// when the annotation lands.
func TestErrorSelfTailCaptureGate_DocumentsCurrentMasterBug(t *testing.T) {
	got := runGateProgram(t, errorSelfTailRaisedAtProgram)
	qt.Assert(t, got, qt.Equals, errorSelfTailCorrupt,
		qt.Commentf("if this FAILS with %s the defect is fixed — delete this test and "+
			"un-gate TestErrorSelfTailCaptureGate_Target", errorSelfTailCorrect))
}

// TestErrorSelfTailCaptureGate_HarnessNonBlindness pins the reason this gate is
// not in continuation_escape_past_oracle_test.go, so a later "simplification"
// onto testhelpers.RunSchemeCode cannot silently turn it green.
//
// Hold the program fixed and vary only the top level's mutability. Under
// WithMutableTopLevel no global is Stable, bodyCalleesAllCaptureSafe refuses,
// the loop arms ZERO OpSelfTailCall sites, and the program therefore reports the
// CORRECT value — on the very build where the immutable-top-level run reports
// the corrupt one. The measured value is "correct" here for the wrong reason:
// the defect was never executed. Unlike its neighbours this test survives the
// fix (it is a statement about the harness, not about the defect) and must NOT
// be deleted with them.
func TestErrorSelfTailCaptureGate_HarnessNonBlindness(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(ctx,
		`(define (loope n) (if (= n 0) 'done (begin (error "tick" n) (loope (- n 1)))))`)
	qt.Assert(t, err, qt.IsNil)
	v, ok := eng.Get("loope")
	qt.Assert(t, ok, qt.IsTrue)
	closure, ok := v.Internal().(*machine.MachineClosure)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, selfTailSites(closure.Template()), qt.Equals, 0,
		qt.Commentf("a mutable top level leaves every global non-Stable, so the loop must not arm"))

	got, err := eng.EvalMultiple(ctx, errorSelfTailRaisedAtProgram)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, errorSelfTailCorrect,
		qt.Commentf("unarmed, the same program reports the correct value — which is exactly "+
			"why a harness with a mutable top level cannot gate this defect"))
}

// TestErrorSelfTailArming_DocumentsCurrentMasterBug is §5.4's disassembly-level
// companion, and it is the structural half the value assertion cannot supply: it
// pins WHY the program above misbehaves rather than that it does. `error` and
// `raise` are the same shape and differ only in the InvokesProcedure stamp, so
// the pair is a discriminator — a change that armed or deopted every loop would
// fail one row. Delete with the two above when the annotation lands.
func TestErrorSelfTailArming_DocumentsCurrentMasterBug(t *testing.T) {
	cases := []struct {
		name  string
		src   string
		proc  string
		sites int
	}{
		{
			name:  "error arms in-place reuse (the defect)",
			src:   `(define (loope n) (if (= n 0) 'done (begin (error "tick" n) (loope (- n 1)))))`,
			proc:  "loope",
			sites: 1,
		},
		{
			name:  "raise deopts to PullApply (the annotated twin)",
			src:   `(define (loopr n) (if (= n 0) 'done (begin (raise n) (loopr (- n 1)))))`,
			proc:  "loopr",
			sites: 0,
		},
		{
			name:  "raise-continuable deopts too",
			src:   `(define (loopc n) (if (= n 0) 'done (begin (raise-continuable n) (loopc (- n 1)))))`,
			proc:  "loopc",
			sites: 0,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			_, err = eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNil)
			v, ok := eng.Get(tc.proc)
			qt.Assert(t, ok, qt.IsTrue)
			closure, ok := v.Internal().(*machine.MachineClosure)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, selfTailSites(closure.Template()), qt.Equals, tc.sites)
		})
	}
}

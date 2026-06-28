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

// The non-blind escape-past oracle for continuation resume.
//
// WHY THIS FILE EXISTS. The 2026-06-27 resume-side-only trampoline flip
// (reinstall-at-nearest) shipped with `make ci` GREEN and was then FALSIFIED by a
// multi-agent crosscheck: an outer continuation invoked to ESCAPE PAST a
// `call-with-values` producer double-executed the consumer. The whole suite was
// STRUCTURALLY BLIND to that class — NO committed test captured `k` OUTSIDE a
// sub-context boundary (call-with-values producer / call-with-exit /
// call-with-continuation-prompt) and invoked it INSIDE, asserting whether the
// boundary's post-thunk work wrongly ran. This file is that missing oracle.
// See plans/2026-06-27-continuation-resume-design-archaeology.local.md,
// plans/2026-06-27-continuation-resume-trampoline-coupled-fix-design.local.md, and
// memory continuation-resume-trampoline-falsified.
//
// HOW TO READ IT.
//   - The GREEN guards below pin master's CORRECT escape-past behavior. They run in
//     CI and FAIL on reinstall-at-nearest (the falsified flip) — they are the exact
//     regression net whose absence let the flip pass. A correct trampoline keeps
//     them GREEN.
//   - The RED cells document two OPEN defects on master: `call-with-values`
//     truncation and the `dynamic-wind` after-thunk double-fire on escape-out. Each
//     has (a) an always-on characterization that PROVES the bug is real on master
//     today (the tripwire), and (b) an env-gated target test (set
//     WILE_RUN_RED_CONTINUATION=1) that asserts the CORRECT behavior — the
//     acceptance criterion for the coupled boundary-reification fix. When the fix
//     lands: the target tests go GREEN, and each "DocumentsCurrentMasterBug"
//     characterization must be deleted (its failure then signals the bug is fixed).
//
// All probes are SINGLE expressions (RunSchemeCode reads exactly one form) and were
// validated on a fresh GOWORK=off build of feat/continuation-resume-trampoline
// (HEAD 77d4e8d5) before being encoded here; the want/skip strings record that run.
//
// NON-BLINDNESS PROVEN. Run against the falsified flip itself (git tag
// attempt-resume-aware-catches-falsified), TestContinuationEscapePastBoundary FAILS —
// the call-with-values row returns "CONSUMER-WRONGLY-RAN" instead of
// "escaped-past-consumer" — i.e. this oracle catches the exact regression that shipped
// `make ci`-green and was only caught by a crosscheck. (The exit/prompt rows pass on
// that flip because their post-sub.Run() work is ABORT-conditional, not unconditional
// like the consumer; they stand as escape-past parity guards for the full fix.)

import (
	"os"
	"strings"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// requireRedContinuation skips a RED acceptance test unless the caller opts in via
// WILE_RUN_RED_CONTINUATION=1. The cells assert the TARGET (post-fix) behavior, so
// they are RED on master; gating keeps CI green while leaving them one env var away
// from running as the coupled fix's acceptance criteria.
func requireRedContinuation(t *testing.T) {
	t.Helper()
	if os.Getenv("WILE_RUN_RED_CONTINUATION") == "" {
		t.Skip("RED acceptance test for the continuation coupled-fix (truncation / " +
			"dynamic-wind double-fire). Set WILE_RUN_RED_CONTINUATION=1 to run; it asserts " +
			"the post-fix TARGET behavior and is RED on master by design.")
	}
}

// TestContinuationEscapePastBoundary is THE escape-past regression net.
//
// Shape: capture `k` with an outer call/cc, then INSIDE a sub-context boundary
// invoke `(k 1)` to escape PAST that boundary. Because `k` is the full
// continuation, invoking it MUST abandon the boundary entirely — the boundary's
// post-thunk work (consumer / handler / exit-body tail) MUST NOT run. The marker
// returned on re-entry proves the escape skipped that work.
//
// On master every row is GREEN (validated). reinstall-at-nearest (the falsified
// flip) turns the sub-context rows RED by running the segment INSIDE the boundary
// and then letting the boundary's pending Go work run anyway — e.g. the
// call-with-values row returned the consumer marker. This is the test that was
// missing.
func TestContinuationEscapePastBoundary(t *testing.T) {
	cases := []struct {
		name string
		code string
		want string
	}{
		{
			// boundary = call-with-values producer (a sub-context; the consumer
			// runs in Go control flow after sub.Run()). The exact class that
			// falsified the flip.
			name: "call-with-values producer",
			code: `
(let ((k #f) (entered #f))
  (call/cc (lambda (c) (set! k c)))
  (if entered
      'escaped-past-consumer
      (begin
        (set! entered #t)
        (call-with-values (lambda () (k 1))
                          (lambda args 'CONSUMER-WRONGLY-RAN)))))`,
			want: "escaped-past-consumer",
		},
		{
			// boundary = call-with-continuation-prompt (a sub-context with a
			// context-level prompt tag + Go-stack errors.As catch).
			name: "call-with-continuation-prompt",
			code: `
(let ((k #f) (entered #f) (tag (make-continuation-prompt-tag)))
  (call/cc (lambda (c) (set! k c)))
  (if entered
      'escaped-past-handler
      (begin
        (set! entered #t)
        (call-with-continuation-prompt
          (lambda () (k 1)) tag
          (lambda vals 'HANDLER-WRONGLY-RAN)))))`,
			want: "escaped-past-handler",
		},
		{
			// boundary = call-with-exit (a sub-context with a custom-tag Go-stack
			// catch). The body after (k 1) must not continue.
			name: "call-with-exit",
			code: `
(let ((k #f) (entered #f))
  (call/cc (lambda (c) (set! k c)))
  (if entered
      'escaped-past-exit-body
      (begin
        (set! entered #t)
        (call-with-exit
          (lambda (exit) (k 1) 'EXIT-BODY-WRONGLY-CONTINUED)))))`,
			want: "escaped-past-exit-body",
		},
		{
			// control: apply runs its callee INLINE on mc (no sub-context), so a
			// continuation captured outside and invoked inside already rides the
			// live chain. GREEN on master AND on the flip — it isolates "is the
			// boundary inline?" from "does the flip break escape-past?".
			name: "apply (inline control)",
			code: `
(let ((k #f) (entered #f))
  (call/cc (lambda (c) (set! k c)))
  (if entered
      'escaped-past-apply
      (begin
        (set! entered #t)
        (apply (lambda (x) (k 1) 'APPLY-BODY-WRONGLY-CONTINUED) (list 99)))))`,
			want: "escaped-past-apply",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestContinuationNestedGuardReRaise pins the case that the flip's DriveResume was
// SPECIFICALLY built to preserve (and did) — but which it preserved by
// reinstall-at-nearest, the same choice that broke escape-past. Keeping this GREEN
// alongside TestContinuationEscapePastBoundary is the contradiction the coupled fix
// must resolve: a correct trampoline keeps BOTH green, where every resume-side-only
// shape keeps at most one. An inner guard misses (number? vs a symbol) and re-raises
// to the outer guard.
func TestContinuationNestedGuardReRaise(t *testing.T) {
	const code = `
(guard (outer ((symbol? outer) (list 'outer-caught outer)))
  (guard (inner ((number? inner) (list 'inner-caught inner)))
    (raise 'a-symbol)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(outer-caught a-symbol)")
}

// TestContinuationNormalCompletionThroughCallWithValues is the no-escape control:
// a producer that simply returns values, consumed normally. The boundary's
// post-thunk work SHOULD run exactly once. Guards against a fix that "solves"
// escape-past by dropping the consumer unconditionally.
func TestContinuationNormalCompletionThroughCallWithValues(t *testing.T) {
	const code = `(call-with-values (lambda () (values 3 4)) (lambda (a b) (+ a b)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "7")
}

// TestContinuationPlainMultiShot is the multi-shot control on the inline path (no
// sub-context boundary): a continuation re-invoked until a counter converges. Pins
// that re-invocation works and that the segment is not corrupted across shots
// (use-after-release would crash or miscount under -race).
func TestContinuationPlainMultiShot(t *testing.T) {
	const code = `
(let ((k #f) (n 0))
  (call/cc (lambda (c) (set! k c)))
  (set! n (+ n 1))
  (if (< n 3) (k #f) n))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "3")
}

// TestContinuationConsumerCapturedReentry pins that a continuation captured INSIDE
// the call-with-values CONSUMER re-enters correctly. The consumer already runs inline
// on mc (prim_control.go:295), so this is GREEN on master — and it is the parity
// guard the coupled fix must NOT break when it reifies the producer: the consumer
// stays re-enterable. (Surfaced by the design's adversarial review, gap #5: the
// producer side was guarded but the consumer side was not.)
func TestContinuationConsumerCapturedReentry(t *testing.T) {
	const code = `
(let ((k #f) (n 0) (trace '()))
  (call-with-values
    (lambda () (values 10 20))
    (lambda (a b)
      (call/cc (lambda (c) (set! k c)))
      (set! n (+ n 1))
      (set! trace (cons (+ a b n) trace))
      (if (< n 3) (k #f) (reverse trace)))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(31 32 33)")
}

// TestContinuationEscapePastMultiShotWithMutation is the KC-7 escape-past variant the
// design named but the first oracle cut lacked: an outer continuation invoked to
// escape PAST a call-with-values producer, MULTIPLE times, with state mutation
// between shots. The consumer must never run on any shot; the counter must converge.
// GREEN on master (validated -> 3); a fix that corrupts the segment across shots
// (use-after-release / shared-bit underflow) would miscount or crash under -race.
func TestContinuationEscapePastMultiShotWithMutation(t *testing.T) {
	const code = `
(let ((k #f) (n 0))
  (call/cc (lambda (c) (set! k c)))
  (set! n (+ n 1))
  (if (< n 3)
      (call-with-values (lambda () (k #f)) (lambda args 'CONSUMER-WRONGLY-RAN))
      n))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "3")
}

// --- RED cells: the two OPEN defects the coupled boundary-reification fix targets.
// Each has a DocumentsCurrentMasterBug tripwire (always on, proves the bug real)
// and a Target acceptance test (WILE_RUN_RED_CONTINUATION-gated, asserts the fix).

// TestCallWithValuesTruncation_DocumentsCurrentMasterBug proves the truncation
// defect is live on master: a generator captures `k` INSIDE the call-with-values
// producer (a sub-context) and re-invokes it after call-with-values returns. The
// consumer + the rest of the program live as Go control flow after sub.Run(), are
// not on the captured chain, and are DROPPED on re-entry — the program truncates,
// surfacing the intermediate value `2` instead of the full trace `(1 2 3)`.
//
// BUG CHARACTERIZATION — when the coupled fix lands this assertion must be DELETED;
// its target counterpart (below) becomes the GREEN acceptance test.
func TestCallWithValuesTruncation_DocumentsCurrentMasterBug(t *testing.T) {
	const code = `
(let ((k #f) (count 0) (trace '()))
  (call-with-values
    (lambda () (+ 1 (call/cc (lambda (c) (set! k c) 0))))
    (lambda (x) (set! trace (cons x trace))))
  (set! count (+ count 1))
  (if (< count 3) (k count) (reverse trace)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	// Master truncates the generator: the re-invoked k (captured inside the producer
	// sub-context) aborts to DefaultPromptTag delivering the intermediate value,
	// dropping the consumer and the (reverse trace) tail. Correct is "(1 2 3)".
	qt.Assert(t, result.SchemeString(), qt.Equals, "2")
}

// TestCallWithValuesTruncation_TargetReachesFullTrace is the acceptance criterion:
// the coupled fix (producer inline / boundary reified as a chain frame) keeps the
// consumer + tail on the captured chain, so re-entry replays them and the generator
// counts to three.
func TestCallWithValuesTruncation_TargetReachesFullTrace(t *testing.T) {
	requireRedContinuation(t)
	const code = `
(let ((k #f) (count 0) (trace '()))
  (call-with-values
    (lambda () (+ 1 (call/cc (lambda (c) (set! k c) 0))))
    (lambda (x) (set! trace (cons x trace))))
  (set! count (+ count 1))
  (if (< count 3) (k count) (reverse trace)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(1 2 3)")
}

// TestDynamicWindDoubleFire_DocumentsCurrentMasterBug proves the after-thunk
// double-fire is live on master. An after-thunk that errors on its SECOND
// invocation turns the latent double-fire into a surfaced error (the trailing fire
// happens at the top-level abort catch, AFTER the program's value is fixed — which
// is exactly why counter/constant assertions are blind to it).
//
// Root cause: two winding reconciles on the call/cc-escape path — one in
// ReinstallSegment, one in RunResumable's abort catch (archaeology §7a). The
// coupled fix collapses them to a single reconcile site.
//
// BUG CHARACTERIZATION — delete when the fix lands; the target counterpart asserts
// the single fire.
func TestDynamicWindDoubleFire_DocumentsCurrentMasterBug(t *testing.T) {
	const code = `
(let ((fired #f) (k #f))
  (call/cc (lambda (c) (set! k c)))
  (cond (k (let ((kk k)) (set! k #f)
            (dynamic-wind (lambda () #f)
                          (lambda () (kk #f))
                          (lambda () (if fired (error "DOUBLE-FIRE") (set! fired #t))))))
        (else 'done)))`
	_, err := testhelpers.RunSchemeCode(t, code)
	// Master double-fires: the after-thunk runs a second time at the top-level
	// catch, where fired is already #t, raising DOUBLE-FIRE. A correct single-fire
	// completes with 'done and no error.
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, strings.Contains(err.Error(), "DOUBLE-FIRE"), qt.IsTrue)
}

// TestDynamicWindDoubleFire_TargetFiresOnce is the acceptance criterion: the
// after-thunk fires exactly once on escape-out, so the error-on-second sentinel
// never trips and the program completes with 'done.
func TestDynamicWindDoubleFire_TargetFiresOnce(t *testing.T) {
	requireRedContinuation(t)
	const code = `
(let ((fired #f) (k #f))
  (call/cc (lambda (c) (set! k c)))
  (cond (k (let ((kk k)) (set! k #f)
            (dynamic-wind (lambda () #f)
                          (lambda () (kk #f))
                          (lambda () (if fired (error "DOUBLE-FIRE") (set! fired #t))))))
        (else 'done)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "done")
}

// ============================================================================
// C1–C4: the four CRITICAL regressions the 2026-06-28 full-cluster reification
// attempt introduced (reifying all six boundaries WITHOUT the winding-aware resume
// flip). Every one was invisible to `go test ./...`, `make lint`, `-race`, AND the
// escape-past guards above; only an A/B crosscheck against clean HEAD caught them —
// and two also turned `make ci` RED via checked-in Scheme suites that `go test ./...`
// does not run. These are REGRESSION GUARDS: each pins CORRECT base behavior that is
// GREEN on clean HEAD (9ee6d38c) today and that the UNIFIED reification+flip change
// MUST preserve. They are the net the suite lacked for the four CRITICALs; their
// common root is that reification makes a boundary's abort depend on driver routing +
// winding-from-the-escape-point, so any path still running boundary-bearing code under
// a plain Run()/non-reconciling re-raise breaks — which is why the reification and the
// resume flip are ONE atomic change. Spec:
// plans/2026-06-28-continuation-cluster-reification-impl.md §7; memory
// continuation-cwv-reification-validated-coupling-mapped.
// ============================================================================

// criticalGuardTimeout bounds the C1/C4 guards: the buggy reification-without-flip
// makes a boundary reached after a resume (C1) or a re-raising handler that ran a
// reified boundary (C4) recur unboundedly. Cooperative ctx cancellation surfaces that
// hang as a timeout error (a clean FAIL) instead of wedging the test binary.
const criticalGuardTimeout = 15 * time.Second

// TestContinuationBoundaryReachedAfterResume_C1 guards C1: a delimiting boundary
// (guard / escape) nested inside a continuation-prompt, or reached in code AFTER a
// call/cc resume, must resolve normally. The reification-without-flip left
// applyCapturedContinuation's resume on a plain sub.Run() (routing it overflows ctak
// under nest-then-abort), so a boundary in resumed code crashed ("no prompt found for
// tag exit") or silently returned a #<machine-closure>. The flip (resume on the
// driver, O(1) Go frames) is what keeps these GREEN once boundaries are reified.
func TestContinuationBoundaryReachedAfterResume_C1(t *testing.T) {
	cases := []struct {
		name string
		code string
		want string
	}{
		{
			// guard inside a continuation-prompt (plan §3 G-C1 row 1).
			name: "guard inside continuation-prompt",
			code: `
(call-with-continuation-prompt
  (lambda () (guard (e (#t (list 'caught e))) (raise 'boom)))
  (make-continuation-prompt-tag)
  (lambda v 'h))`,
			want: "(caught boom)",
		},
		{
			// an escape (call-with-exit) inside a continuation-prompt — the base-only
			// analog of plan §3 G-C1 row 2, (reset (* 2 (call/ec (lambda (k) (k 21)))))
			// => 42, using core primitives instead of the (wile control) library.
			name: "escape inside continuation-prompt",
			code: `
(call-with-continuation-prompt
  (lambda () (* 2 (call-with-exit (lambda (k) (k 21)))))
  (make-continuation-prompt-tag)
  (lambda v v))`,
			want: "42",
		},
		{
			// the §7-C1 face: a boundary (guard) reached in code AFTER a call/cc
			// resume. The resume must land on the live chain so the subsequent guard
			// resolves; the buggy resume-sub truncated/crashed here.
			name: "guard reached after call/cc resume",
			code: `
(let ((k #f) (n 0))
  (call/cc (lambda (c) (set! k c)))
  (set! n (+ n 1))
  (if (< n 2)
      (k #f)
      (guard (e (#t 'caught-after-resume)) (raise 'boom))))`,
			want: "caught-after-resume",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.code, criticalGuardTimeout)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestBoundaryInsideWindThunkOnForwardEscape_C2 guards C2: a boundary that aborts
// INSIDE a dynamic-wind thunk reached on the FORWARD-escape path must route, not
// crash. The reification-without-flip ran the winding thunks
// (machine_context_winding.go) under a plain sub.Run() reached from
// RestoreWithWindingFrom, so the inner escape's abort had no driver to route it and
// crashed ("no prompt found"). Here the after-thunk runs its own call-with-exit while
// the outer escape unwinds the wind; clean HEAD completes the outer escape => 'done.
func TestBoundaryInsideWindThunkOnForwardEscape_C2(t *testing.T) {
	const code = `
(call-with-exit
  (lambda (escape)
    (dynamic-wind
      (lambda () #f)
      (lambda () (escape 'done))
      (lambda () (call-with-exit (lambda (k) (k 'x)))))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "done")
}

// TestAfterThunkThroughDeeperSubFiresOnce_C3 guards C3: a dynamic-wind after-thunk
// must fire (exactly once) when an exception escapes through a DEEPER surviving
// sub-context — here force/delay; barrier/timeout/eval are the same class. The
// reification-without-flip's RunWithinBoundary re-raise reconciled only the frame's
// own chain, not the deeper sub's winding, so the after-thunk was SILENTLY SKIPPED (a
// resource leak). The counter proves it ran on the escape.
func TestAfterThunkThroughDeeperSubFiresOnce_C3(t *testing.T) {
	const code = `
(let ((c 0))
  (guard (e (#t 'caught))
    (force (delay (dynamic-wind
                    (lambda () #f)
                    (lambda () (raise 'x))
                    (lambda () (set! c (+ c 1)))))))
  c)`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "1")
}

// TestHandlerRunsBoundaryThenReraise_C4 guards C4: a with-exception-handler handler
// that runs a (reifiable) boundary and then re-raises must escalate to the PARENT
// handler exactly once, not loop. The reification-without-flip's RunBodyUnderFrame set
// p.cont = frame and dropped the exceptionHandlerParam=parent mark RaiseInPlace had
// installed on the handler sub, so the secondary raise re-resolved the FULL handler
// list — re-invoking the same handler unboundedly (a hang). The outer guard must catch
// the re-raised 'again exactly once.
func TestHandlerRunsBoundaryThenReraise_C4(t *testing.T) {
	const code = `
(guard (outer (#t (list 'outer outer)))
  (with-exception-handler
    (lambda (e)
      (call-with-exit (lambda (k) 'ran-boundary))
      (raise 'again))
    (lambda () (raise 'first))))`
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, criticalGuardTimeout)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(outer again)")
}

// TestComposableEscapePastReifiedBoundary_DocumentsCurrentMasterBug proves KC-9 is
// live on master: a call-with-composable-continuation captured to the default prompt
// tag from inside a call-with-exit cannot see the outer default prompt, because the
// call-with-exit SUB-CONTEXT blocks FindPrompt — so the capture ERRORS ("no prompt
// found for tag"). Reifying call-with-exit as a chain frame removes that block; the
// target counterpart asserts the capture then succeeds. This cell only asserts
// TARGET-relevant behavior indirectly (it pins the current limitation); DELETE it when
// the unified change lands and its target goes GREEN.
func TestComposableEscapePastReifiedBoundary_DocumentsCurrentMasterBug(t *testing.T) {
	const code = `
(call-with-continuation-prompt
  (lambda ()
    (call-with-exit
      (lambda (exit)
        (+ 1 (call-with-composable-continuation
               (lambda (k) (k 10))
               (default-continuation-prompt-tag))))))
  (default-continuation-prompt-tag)
  (lambda (v) v))`
	_, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, strings.Contains(err.Error(), "no prompt found"), qt.IsTrue)
}

// TestComposableEscapePastReifiedBoundary_Target is the KC-9 acceptance criterion:
// once call-with-exit is a chain frame, FindPrompt reaches the outer default prompt,
// so the composable capture succeeds (no error). The exact value is pinned when the
// reification lands; the load-bearing property here is that capture no longer errors.
// Env-gated like the other RED targets.
func TestComposableEscapePastReifiedBoundary_Target(t *testing.T) {
	requireRedContinuation(t)
	const code = `
(call-with-continuation-prompt
  (lambda ()
    (call-with-exit
      (lambda (exit)
        (+ 1 (call-with-composable-continuation
               (lambda (k) (k 10))
               (default-continuation-prompt-tag))))))
  (default-continuation-prompt-tag)
  (lambda (v) v))`
	_, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
}

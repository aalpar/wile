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

// Continuation invariants over a combinatorial matrix (item 2 of
// plans/2026-06-28-continuation-testing-enhancement.md).
//
// Every regression in the reification+flip arc lived in a COMBINATION — guard-over-cwv,
// call/cc-in-force-in-dynamic-wind, tail-promoted-op-in-proc-under-guard. These tests
// enumerate the combinations from templates and assert a LAW that must hold for ALL of
// them, not a memorized per-case answer, so they keep biting as boundaries are added:
//
//   1. escape-past: an outer full k invoked inside any boundary never runs that
//      boundary's post-work.
//   2. dynamic-wind balance: after-thunk fires exactly as many times as before-thunk on
//      every exit path.
//   3. multi-shot convergence: a counter loop driven by re-invoking a continuation
//      captured inside any boundary converges to the target.
//
// criticalGuardTimeout (continuation_escape_past_oracle_test.go) bounds each probe so a
// hang surfaces as a clean FAIL.

import (
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// TestEscapePastInvariant: for every boundary B, an outer full continuation k invoked
// INSIDE B must abandon B entirely — B's post-work (consumer / handler / body-tail /
// after-thunk) must NOT run. The program reaches 'escaped-ok on re-entry; a regression
// that runs the boundary's pending work yields its MARKER instead.
func TestEscapePastInvariant(t *testing.T) {
	boundaries := []struct {
		name string
		wrap string // an expression containing (k 1); its non-escape value is a *-MARKER
	}{
		{"call-with-values consumer", `(call-with-values (lambda () (k 1)) (lambda args 'CONSUMER-MARKER))`},
		{"call-with-exit body tail", `(call-with-exit (lambda (e) (k 1) 'EXIT-MARKER))`},
		{"continuation-prompt handler", `(call-with-continuation-prompt (lambda () (k 1)) (make-continuation-prompt-tag) (lambda v 'HANDLER-MARKER))`},
		{"apply body tail", `(apply (lambda (x) (k 1) 'APPLY-MARKER) (list 9))`},
		{"dynamic-wind body", `(dynamic-wind (lambda () #f) (lambda () (k 1) 'DW-MARKER) (lambda () #f))`},
		{"guard body", `(guard (e (#t 'GUARD-MARKER)) (k 1) 'BODY-MARKER)`},
		{"cwv inside call-with-exit", `(call-with-exit (lambda (e) (call-with-values (lambda () (k 1)) (lambda a 'INNER-MARKER))))`},
		{"prompt inside dynamic-wind", `(dynamic-wind (lambda () #f) (lambda () (call-with-continuation-prompt (lambda () (k 1)) (make-continuation-prompt-tag) (lambda v 'PH-MARKER))) (lambda () #f))`},
	}
	for _, b := range boundaries {
		t.Run(b.name, func(t *testing.T) {
			code := `(let ((k #f) (entered #f))
  (call/cc (lambda (c) (set! k c)))
  (if entered
      'escaped-ok
      (begin (set! entered #t) ` + b.wrap + `)))`
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, criticalGuardTimeout)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, "escaped-ok")
		})
	}
}

// TestDynamicWindBalanceInvariant: on every exit path, the dynamic-wind after-thunk
// fires exactly as many times as the before-thunk (and at least once). The program
// returns (and (= before after) (> before 0)) => #t; the various exit mechanisms
// (normal, exception, escape-procedure, call/cc escape) are the matrix axis.
func TestDynamicWindBalanceInvariant(t *testing.T) {
	const tmpl = `(let ((b 0) (a 0) (esc #f) (top #f))
  %s
  (and (= b a) (> b 0)))`
	exits := []struct {
		name string
		body string // a form that enters the dynamic-wind and exits via the named path
	}{
		{"normal return", `(dynamic-wind (lambda () (set! b (+ b 1))) (lambda () 'v) (lambda () (set! a (+ a 1))))`},
		{"exception caught by outer guard", `(guard (e (#t #f)) (dynamic-wind (lambda () (set! b (+ b 1))) (lambda () (raise 'x)) (lambda () (set! a (+ a 1)))))`},
		{"call-with-exit escape", `(call-with-exit (lambda (e) (dynamic-wind (lambda () (set! b (+ b 1))) (lambda () (e 'v)) (lambda () (set! a (+ a 1))))))`},
		{"callcc escape-out", `(call/cc (lambda (out) (dynamic-wind (lambda () (set! b (+ b 1))) (lambda () (out 'gone)) (lambda () (set! a (+ a 1))))))`},
		{"nested winds, exception through both", `(guard (e (#t #f)) (dynamic-wind (lambda () (set! b (+ b 1))) (lambda () (dynamic-wind (lambda () (set! b (+ b 1))) (lambda () (raise 'x)) (lambda () (set! a (+ a 1))))) (lambda () (set! a (+ a 1)))))`},
	}
	for _, e := range exits {
		t.Run(e.name, func(t *testing.T) {
			code := fmt.Sprintf(tmpl, e.body)
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, criticalGuardTimeout)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
		})
	}
}

// TestMultiShotConvergenceInvariant: a counter loop driven by re-invoking a continuation
// captured inside each boundary must converge to the target (5). A truncated or runaway
// resume would loop (-> timeout) or return the wrong count. Confirms the resume
// trampoline replays whatever boundary the capture sits inside.
func TestMultiShotConvergenceInvariant(t *testing.T) {
	positions := []struct {
		name    string
		capture string // an expression that runs (call/cc (lambda (c) (set! k c))) in some boundary
	}{
		{"top-level", `(call/cc (lambda (c) (set! k c)))`},
		{"in call-with-values producer", `(call-with-values (lambda () (call/cc (lambda (c) (set! k c)))) (lambda (v) v))`},
		{"in dynamic-wind body", `(dynamic-wind (lambda () #f) (lambda () (call/cc (lambda (c) (set! k c)))) (lambda () #f))`},
		{"in guard body", `(guard (e (#t 'never)) (call/cc (lambda (c) (set! k c))))`},
		{"in apply", `(apply (lambda () (call/cc (lambda (c) (set! k c)))) '())`},
		{"in call-with-exit body", `(call-with-exit (lambda (e) (call/cc (lambda (c) (set! k c)))))`},
		{"in nested dynamic-wind", `(dynamic-wind (lambda () #f) (lambda () (dynamic-wind (lambda () #f) (lambda () (call/cc (lambda (c) (set! k c)))) (lambda () #f))) (lambda () #f))`},
	}
	for _, p := range positions {
		t.Run(p.name, func(t *testing.T) {
			code := `(let ((k #f) (n 0))
  ` + p.capture + `
  (set! n (+ n 1))
  (if (< n 5) (k #f) n))`
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, criticalGuardTimeout)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, "5")
		})
	}
}

// TestDynamicWindEntryMarkInvariant: across every exit path, a parameter rebound by a
// parameterize INSIDE a dynamic-wind body must be restored to its entry value when the
// dynamic-wind after-thunk runs (R7RS §6.10 — after-thunk runs in the dynamic-wind's
// entry environment). This is the invariant family the matrix originally LACKED: the A/B
// crosscheck caught a regression here that escaped every other layer. Each program
// returns the parameter value observed by the after-thunk; it must equal the entry value.
func TestDynamicWindEntryMarkInvariant(t *testing.T) {
	const tmpl = `(let ((p (make-parameter 7)) (seen #f))
  %s
  seen)`
	exits := []struct {
		name string
		body string // rebinds p to 99 inside the dynamic-wind body, then exits via the named path
	}{
		{"normal", `(dynamic-wind (lambda () #f) (lambda () (parameterize ((p 99)) 'v)) (lambda () (set! seen (p))))`},
		{"call/cc escape", `(call/cc (lambda (k) (dynamic-wind (lambda () #f) (lambda () (parameterize ((p 99)) (k 'v))) (lambda () (set! seen (p))))))`},
		{"call-with-exit escape", `(call-with-exit (lambda (k) (dynamic-wind (lambda () #f) (lambda () (parameterize ((p 99)) (k 'v))) (lambda () (set! seen (p))))))`},
		{"exception via guard", `(guard (e (#t #f)) (dynamic-wind (lambda () #f) (lambda () (parameterize ((p 99)) (raise 'x))) (lambda () (set! seen (p)))))`},
	}
	for _, e := range exits {
		t.Run(e.name, func(t *testing.T) {
			code := fmt.Sprintf(tmpl, e.body)
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, criticalGuardTimeout)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, "7") // entry value, NOT 99
		})
	}
}

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

// captureEscape is a call/cc escape in NON-TAIL position: it is the value
// expression of a `set!`, so the compiler emits a SaveContinuation frame above
// the call/cc site (tail-position escapes do not, and do not trip the bug).
// Invoking the escape `(r 42)` resumes the captured segment, which runs the
// `set!` (binding `captured` to 42) and then continues.
//
// Wrapping captureEscape inside a sub-context primitive and then evaluating
// `(list 'END captured)` is a STRONG observable: it checks both that the
// continuation after the primitive survives (the list runs at all) AND that the
// escape delivered the correct value (captured == 42). A correct VM returns
// `(END 42)`; the truncation bug drops the `(list ...)` at the sub-context
// boundary, so the program value is the truncated remainder instead.
//
// See plans/2026-06-25-overnight-execution-roadmap.local.md -> "BLAST RADIUS".
const captureEscape = `(set! captured (call/cc (lambda (r) (r 42))))`

// redExpected is the correct post-fix program value: a proper list (END 42).
func redExpected() values.Value {
	return values.List(values.NewSymbol("END"), values.NewInteger(42))
}

// TestSubContextNonTailEscapeTruncation is a GREEN regression guard for the
// sub-context continuation-truncation fix. A non-tail call/cc escape inside a
// sub-context primitive that does not catch DefaultPromptTag used to truncate the
// continuation after that primitive: the program value was the truncated remainder
// instead of the resumed program tail. The resume-trampoline + boundary-reification
// arc fixed it — applyCapturedContinuation now bounces through RunResumable, and
// each of the ten victim primitives reifies its boundary as a chain frame, so a
// captured segment spans the boundary and re-entry replays the tail.
//
// Each case wraps captureEscape in one such primitive; a correct VM resumes the
// continuation AND delivers the escape value, so the program value is `(END 42)`.
// A regression turns one of these RED.
//
// History: committed RED + WILE_RUN_RED_CONTINUATION-gated in a99d39ac; the fix
// landed across 6d8e7ee5..7acf4797 (dfc3504b claim-3 handler reify + the five
// boundary reifications 71032eb8/9fa2817a/c7e50def/98c9da72/7acf4797); the gate was
// removed once all ten passed (2026-06-29). See the design archive doc
// 2026-06-26-subcontext-continuation-the-open-problem.local.md for the
// falsification arc that produced the fix.
func TestSubContextNonTailEscapeTruncation(t *testing.T) {
	cases := []struct {
		name string
		// form is the sub-context primitive call whose thunk contains
		// captureEscape. It is wrapped as
		// `(let ((captured 'unset)) <form> (list 'END captured))`.
		form string
	}{
		{"call-with-values", `(call-with-values (lambda () ` + captureEscape + `) list)`},
		{"with-exception-handler", `(with-exception-handler (lambda (e) #f) (lambda () ` + captureEscape + `))`},
		{"guard", `(guard (e (#t #f)) ` + captureEscape + `)`},
		{"parameterize-converter", `(let ((p (make-parameter 0 (lambda (x) ` + captureEscape + ` x)))) (parameterize ((p 9)) #f))`},
		{"call-with-composable-continuation", `(call-with-composable-continuation (lambda (k) ` + captureEscape + `) (default-continuation-prompt-tag))`},
		{"call-with-exit", `(call-with-exit (lambda (e) ` + captureEscape + `))`},
		{"call-with-continuation-barrier", `(call-with-continuation-barrier (lambda () ` + captureEscape + `))`},
		{"with-timeout", `(with-timeout 100000 (lambda (k) k) (lambda () ` + captureEscape + `))`},
		{"call-with-immediate-continuation-mark", `(call-with-immediate-continuation-mark 'k (lambda (v) ` + captureEscape + `) #f)`},
		{"exception-handler-dispatch", `(with-exception-handler (lambda (e) ` + captureEscape + `) (lambda () (raise-continuable 'x)))`},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			code := `(let ((captured 'unset)) ` + tc.form + ` (list 'END captured))`
			result, err := testhelpers.RunSchemeCode(t, code)
			qt.Assert(t, err, qt.IsNil)
			// Post-fix the continuation resumes with the escape value, so the
			// program value is (END 42). The bug drops the trailing (list ...)
			// at the sub-context boundary and yields the truncated remainder.
			qt.Assert(t, result, valuestest.SchemeEquals, redExpected())
		})
	}
}

// TestSubContextEscapeControls pins the IMMUNE behavior and proves the RED
// harness is valid (not always-failing). Two families, both green on current
// code:
//
//   - tail-position survival: the same escape in TAIL position survives in every
//     tail-immune sub-context primitive (returns 'END). Guards against a fix
//     regressing tail-position escapes. call-with-composable-continuation is
//     deliberately ABSENT: it truncates even in tail position (a stronger
//     victim), so it has no green tail behavior to pin — its non-tail case is
//     covered by the RED suite above.
//   - value-capture validity: the (list 'END captured) == (END 42) observable
//     returns the correct value when the continuation is NOT truncated (a
//     continuation prompt that catches the abort, dynamic-wind which is not a
//     sub.Run primitive, and the bare top-level case). This proves the RED
//     assertion is not vacuously failing.
func TestSubContextEscapeControls(t *testing.T) {
	endSym := func() values.Value {
		return values.NewSymbol("END")
	}

	tailSurvival := []testhelpers.SchemeCodeTestCase{
		{Name: "call-with-values tail", Code: `(begin (call-with-values (lambda () (call/cc (lambda (r) (r 0)))) list) 'END)`, Expected: endSym()},
		{Name: "with-exception-handler tail", Code: `(begin (with-exception-handler (lambda (e) #f) (lambda () (call/cc (lambda (r) (r 0))))) 'END)`, Expected: endSym()},
		{Name: "guard tail", Code: `(begin (guard (e (#t #f)) (call/cc (lambda (r) (r 0)))) 'END)`, Expected: endSym()},
		{Name: "parameterize-converter tail", Code: `(begin (let ((p (make-parameter 0 (lambda (x) (call/cc (lambda (r) (r x))))))) (parameterize ((p 9)) #f)) 'END)`, Expected: endSym()},
		{Name: "call-with-exit tail", Code: `(begin (call-with-exit (lambda (e) (call/cc (lambda (r) (r 0))))) 'END)`, Expected: endSym()},
		{Name: "call-with-continuation-barrier tail", Code: `(begin (call-with-continuation-barrier (lambda () (call/cc (lambda (r) (r 0))))) 'END)`, Expected: endSym()},
		{Name: "with-timeout tail", Code: `(begin (with-timeout 100000 (lambda (k) k) (lambda () (call/cc (lambda (r) (r 0))))) 'END)`, Expected: endSym()},
		{Name: "call-with-immediate-continuation-mark tail", Code: `(begin (call-with-immediate-continuation-mark 'k (lambda (v) (call/cc (lambda (r) (r 0)))) #f) 'END)`, Expected: endSym()},
		{Name: "exception-handler-dispatch tail", Code: `(begin (with-exception-handler (lambda (e) (call/cc (lambda (r) (r 0)))) (lambda () (raise-continuable 'x))) 'END)`, Expected: endSym()},
	}

	captureValid := []testhelpers.SchemeCodeTestCase{
		{Name: "dynamic-wind value-capture", Code: `(let ((captured 'unset)) (dynamic-wind (lambda () #f) (lambda () ` + captureEscape + `) (lambda () #f)) (list 'END captured))`, Expected: redExpected()},
		{Name: "call-with-continuation-prompt value-capture", Code: `(let ((captured 'unset)) (call-with-continuation-prompt (lambda () ` + captureEscape + `) (default-continuation-prompt-tag) (lambda (x) x)) (list 'END captured))`, Expected: redExpected()},
		{Name: "bare top-level value-capture", Code: `(let ((captured 'unset)) ` + captureEscape + ` (list 'END captured))`, Expected: redExpected()},
	}

	run := func(t *testing.T, tc testhelpers.SchemeCodeTestCase) {
		result, err := testhelpers.RunSchemeCode(t, tc.Code)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
	}

	for _, tc := range tailSurvival {
		t.Run("tail/"+tc.Name, func(t *testing.T) {
			run(t, tc)
		})
	}
	for _, tc := range captureValid {
		t.Run("capture-valid/"+tc.Name, func(t *testing.T) {
			run(t, tc)
		})
	}
}

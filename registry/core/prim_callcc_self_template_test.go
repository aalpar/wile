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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// TestCallCCSelfTemplate pins a correctness bug in call/cc inline mode.
//
// call/cc inline mode (PrimCallCC, mc.Parent() != nil) reconfigures the VM in
// place via mc.ApplyCallable(proc). The foreign-call dispatcher (applyForeign /
// callForeignCached) decides whether the primitive reconfigured the VM by
// testing `mc.template != savedTemplate`. That inference is a FALSE NEGATIVE
// when proc shares the caller's template — i.e. when the procedure passed to
// call/cc is the very function currently executing (self-application). In that
// case mc.template == savedTemplate after Apply, the dispatcher concludes
// "nothing changed", and wrongly restores the continuation instead of running
// proc. The observable symptom: (call/cc self) yields self's continuation
// object instead of calling (self k).
//
// The control case (distinct template) proves the logic is otherwise correct:
// only template identity differs between the two sub-tests.
func TestCallCCSelfTemplate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			// proc passed to call/cc IS the enclosing function `self`, so the
			// call site's template and proc's template are the same object.
			// Correct R7RS behavior: call/cc invokes (self k); self ignores k,
			// recurses via (call/cc self) until n reaches 3, returning 3.
			Name: "self-template: proc is the enclosing function",
			Code: `
				(let ()
				  (define n 0)
				  (define (self k)
				    (set! n (+ n 1))
				    (if (>= n 3) n (call/cc self)))
				  (self 'start))`,
			Expected: values.NewInteger(3),
		},
		{
			// Identical logic, but proc is a fresh lambda whose template differs
			// from the call site. This is the control: it already passes today.
			Name: "distinct-template: proc is a fresh lambda (control)",
			Code: `
				(let ()
				  (define m 0)
				  (define (distinct k)
				    (set! m (+ m 1))
				    (if (>= m 3) m (call/cc (lambda (k2) (distinct k2)))))
				  (distinct 'start))`,
			Expected: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			// Compare on the printed form. Asserting SchemeEquals directly
			// against a non-Integer result would deep-print the closure's
			// (cyclic) captured namespace on failure — slow and unreadable.
			// The buggy path returns the call/cc closure, so its SchemeString
			// makes the failure self-explanatory.
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.Expected.SchemeString())
		})
	}
}

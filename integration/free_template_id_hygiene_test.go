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

package integration_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// TestFreeTemplateIdHygiene exercises R7RS §4.3.2: a free identifier in a macro
// template resolves to the binding in effect at the macro's DEFINITION site, and a
// same-named binding introduced at the USE site does not capture it.
//
// Each case is a sequence of top-level forms; EvalMultiple runs them in order and
// returns the last value. A fresh engine per case isolates the #1/#3 mutations (they
// mutate an installed slot and would cross-contaminate a shared engine).
func TestFreeTemplateIdHygiene(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
		closedBy string // the phase that flips this GREEN; "" = green today (guard)
	}{
		{
			name: "exposure1_toplevel_guard_aux_hijack",
			// #1: a top-level (define-syntax guard-aux ...) must NOT capture guard's
			// private helper. guard(else) catches the raise and yields 'x.
			code: `(define-syntax guard-aux (syntax-rules () ((_ r ...) 'PWNED)))
			       (guard (e (else 'x)) (raise 'y))`,
			expected: "x",
			closedBy: "Phase 3 (D0+D1+D2)",
		},
		{
			name: "guard2_letsyntax_local_no_capture",
			// Filed as #2 but does NOT reproduce on b96f8b53: a use-site
			// (let-syntax ((guard-aux ...))) carries its own intro scope, which is not a
			// subset of guard's template free-id def-site scope, so arm 1 already refuses
			// it (design §1.2). GREEN today; a standing guard that D2 must not regress.
			code: `(let-syntax ((guard-aux (syntax-rules () ((_ . a) 'hijacked))))
			         (guard (e (#t 'c)) (raise 'b)))`,
			expected: "c",
			closedBy: "", // green guard: already correct, never quarantined
		},
		{
			name: "exposure3_letsyntax_special_form_corruption",
			// #3: redefining the let-syntax special form must SHADOW cleanly, not brick
			// it. Before D3 the user define-syntax reuses the installed slot
			// (CreateGlobalBinding dedups ignoring BindingType) and SetOwnGlobalValue
			// overwrites the Primitive-typed slot in place, so every subsequent
			// (let-syntax ...) is rejected by lookup and ERRORS (err != nil). After D3 the
			// redefine lands in the mutable child as a BindingTypeSyntax shadow and the
			// user's transformer runs.
			code: `(define-syntax let-syntax (syntax-rules () ((_ bindings body ...) 'shadowed)))
			       (let-syntax ((tmp (syntax-rules () ((_) 42)))) (tmp))`,
			expected: "shadowed",
			closedBy: "Phase 1 (D3)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.closedBy != "" {
				t.Skipf("RED until %s — free-template-id-hygiene (%s)", tc.closedBy, tc.name)
			}
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestFreeTemplateIdHygiene_CoIntroducedLetSyntaxBeatsGlobalPin is the R1-analog on
// the macro dispatch path: a co-introduced let-syntax helper must win over a
// same-named definition-site GLOBAL macro pin. Mirrors the value-path R1 fix
// (25d832c0). If this ever fails after D2, the pin was inserted BEFORE the local
// let-syntax arm — the R1 "jumps the queue" mistake. Green today (no pin consulted →
// co-introduced wins via arm 1); must stay green after D2.
func TestFreeTemplateIdHygiene_CoIntroducedLetSyntaxBeatsGlobalPin(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	code := `
	  ;; a def-time GLOBAL macro named helper
	  (define-syntax helper (syntax-rules () ((_) 'GLOBAL)))
	  ;; a macro whose template co-introduces its OWN let-syntax helper of the same name
	  (define-syntax uses-own-helper
	    (syntax-rules ()
	      ((_) (let-syntax ((helper (syntax-rules () ((_) 'LOCAL))))
	             (helper)))))
	  (uses-own-helper)`
	result, err := engine.EvalMultiple(context.Background(), code)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "LOCAL") // the co-introduced helper, not 'GLOBAL
}

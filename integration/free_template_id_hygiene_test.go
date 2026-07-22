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
			closedBy: "", // closed by Phase 2′ (D0 pin population + D2 pin consultation)
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
			closedBy: "", // closed by Phase 1′ (D1+D3): special-form expanders now in the sealed expand base
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

// TestFreeTemplateIdHygiene_Section6Cases covers the design's §6 soundness cases for the
// D2 pin consultation beyond #1 (which is in TestFreeTemplateIdHygiene). Each is green after
// Phase 2′ (D0+D2) and must stay green.
func TestFreeTemplateIdHygiene_Section6Cases(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			// §6 case 1: a macro that GENERATES an inner macro referencing a define the
			// outer expansion introduces. The generated reference has no stable def-time
			// pin (nil), so D2 must fall THROUGH to the current use-site walk to reach the
			// introduced define. In-process analogue of march-hare/jabberwocky.
			name: "case1_generated_macro_nil_pin_fallthrough",
			code: `(define-syntax make-getter
			         (syntax-rules ()
			           ((_ name)
			            (begin
			              (define secret 42)
			              (define-syntax name (syntax-rules () ((_) secret)))))))
			       (make-getter get-secret)
			       (get-secret)`,
			expected: "42",
		},
		{
			// §6 case 2 (hygiene direction): a USE-SITE local variable named like a
			// template free-id (a macro) must NOT capture it. guard's template references
			// guard-aux; a use-site (let ((guard-aux …))) variable of a different scope does
			// not shadow it — hasLocalVariableBinding is false, the pin fires, guard works.
			name: "case2b_use_site_local_var_no_capture",
			code: `(let ((guard-aux 'i-am-a-variable))
			         (guard (e (else 'caught)) (raise 'x)))`,
			expected: "caught",
		},
		{
			// §6 case 4: a pinned template reference and a bare use-site reference of the
			// same name resolve to DIFFERENT bindings. guard's template ref to guard-aux is
			// pinned to the sealed bootstrap guard-aux (so guard still works); a direct
			// (guard-aux …) call reaches the user's top-level redefinition. present/absent
			// pin IS the def-site/use-site split.
			name: "case4_pinned_vs_bare_different_bindings",
			code: `(define-syntax guard-aux (syntax-rules () ((_ . r) 'USER-GUARD-AUX)))
			       (list (guard (e (else 'guard-works)) (raise 'x))
			             (guard-aux ignored))`,
			expected: "(guard-works USER-GUARD-AUX)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestFreeTemplateIdHygiene_Adversarial exercises additional use-site capture vectors that
// must NOT hijack a bootstrap macro's private helper. They are standing guards that the D1/D2
// changes must not regress. Clauses use (#t …) rather than (else …): the else auxiliary-syntax
// literal does not resolve inside a let-syntax scope (a pre-existing limitation, present on
// master b96f8b53 and unrelated to this arc — the #2 guard uses the same workaround), so an
// (else …) clause here would fail for a reason orthogonal to guard-aux capture. The
// library-import vector never reproduced (TODO "No sealed base above phase 0") and needs a
// library registry the in-process EvalMultiple harness does not wire up; it is left uncovered.
func TestFreeTemplateIdHygiene_Adversarial(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			// A use-site nested let-syntax binding guard-aux must not capture (a variant of
			// the #2 guard: the binder's intro scope is not a subset of guard's def-site scope).
			name: "nested_let_syntax_no_capture",
			code: `(let ()
			         (let-syntax ((guard-aux (syntax-rules () ((_ . r) 'HIJACK))))
			           (guard (e (#t 'safe)) (raise 'x))))`,
			expected: "safe",
		},
		{
			// Same via letrec-syntax.
			name: "letrec_syntax_no_capture",
			code: `(letrec-syntax ((guard-aux (syntax-rules () ((_ . r) 'HIJACK))))
			         (guard (e (#t 'safe)) (raise 'x)))`,
			expected: "safe",
		},
		{
			// A macro-generating macro that introduces a helper of a colliding name in its
			// output. guard used in the generated body still resolves guard-aux def-site.
			name: "macro_generating_macro_colliding_helper",
			code: `(define-syntax with-evil-guard-aux
			         (syntax-rules ()
			           ((_ body)
			            (let-syntax ((guard-aux (syntax-rules () ((_ . r) 'HIJACK))))
			              body))))
			       (with-evil-guard-aux (guard (e (#t 'safe)) (raise 'x)))`,
			expected: "safe",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

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

package environment_test

import (
	"context"
	"sort"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	valuestest "github.com/aalpar/wile/pkg/values/valuestest"
)

// runSteps evaluates each step as a SEPARATE compilation unit against one shared
// environment, returning the last step's value. Separate units (rather than one
// (begin ...) wrapper) are essential for the redefine-visibility characterization:
// a closure compiled in an earlier unit pins its global references at compile time,
// and a later unit's redefine is what exposes the sealed-base shadow semantics. This
// mirrors how EvalMultiple and real REPL input compile top-level forms.
func runSteps(t *testing.T, steps ...string) values.Value {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	qt.Assert(t, err, qt.IsNil)
	var result values.Value
	for _, step := range steps {
		result, err = testhelpers.RunSchemeCodeWithEnv(t, env, step)
		qt.Assert(t, err, qt.IsNil)
	}
	return result
}

// TestCharacterization_UserDefineShadowsPrimitive pins the post-carve sealed-base
// (Chez two-environment) semantics.
//
// Case 1: a direct (car ...) call compiled AFTER a (define car ...) resolves to the
// user shadow — a redefine still takes effect for code compiled after it.
//
// Cases 2-3: use-car is compiled BEFORE the shadow, so its `car` reference is pinned to
// the sealed base at compile time (GetGlobalIndex sets gi.Env to the sealed-base frame).
// It keeps calling the REAL car even after the shadow, so (use-car '(7 8)) = 7. This is
// the documented R7RS deviation: an already-compiled closure does NOT observe a later
// redefine of a sealed-base name. PRE-carve both cases were 99 (the redefine mutated the
// single shared merged-frame slot in place).
func TestCharacterization_UserDefineShadowsPrimitive(t *testing.T) {
	tcs := []struct {
		name     string
		steps    []string
		expected values.Value
	}{
		{
			name:     "shadow car with user define",
			steps:    []string{`(define car (lambda (x) 42))`, `(car '(1 2))`},
			expected: values.NewInteger(42),
		},
		{
			name:     "closure keeps sealed binding through define-then-shadow (POST-carve)",
			steps:    []string{`(define (use-car p) (car p))`, `(define car (lambda (x) 99))`, `(use-car '(7 8))`},
			expected: values.NewInteger(7),
		},
		// Redefine-visibility: call the closure FIRST, then shadow, then call again.
		// use-car keeps the sealed car (compile-time pinned), so the final call = 7.
		{
			name:     "closure keeps sealed binding through redefine (POST-carve)",
			steps:    []string{`(define (use-car p) (car p))`, `(use-car '(1 2))`, `(define car (lambda (x) 99))`, `(use-car '(7 8))`},
			expected: values.NewInteger(7),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := runSteps(t, tc.steps...)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestCharacterization_RuntimeFrameTopology pins the post-carve topology: Runtime()
// is the mutable child; SealedBase() is the structural root (parent nil). This is the
// inversion of the pre-carve assertion (Runtime() was the root) — the carve is a
// visible, deliberate topology change.
func TestCharacterization_RuntimeFrameTopology(t *testing.T) {
	ns := environment.NewNamespace()
	qt.Assert(t, ns.Runtime().IsTopLevel(), qt.IsFalse)      // parent is the sealed base now
	qt.Assert(t, ns.SealedBase().IsTopLevel(), qt.IsTrue)    // sealed base is the true root
	qt.Assert(t, ns.Runtime() != ns.SealedBase(), qt.IsTrue) // distinct frames
}

// TestSealedBase_PrimitivesLiveInSealedBase verifies a core Go primitive binding is
// OWNED by the sealed base (present in its OWN global frame) and ABSENT from the mutable
// runtime's OWN frame — so a user (define car ...) shadows via created=true. Uses the
// non-walking, non-mutating GlobalEnvironmentFrame.GetGlobalIndex own-frame probe.
func TestSealedBase_PrimitivesLiveInSealedBase(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	carSym := values.NewSymbol("car")

	inRuntime := ns.Runtime().GlobalEnvironment().GetGlobalIndex(carSym)
	inSealed := ns.SealedBase().GlobalEnvironment().GetGlobalIndex(carSym)

	qt.Assert(t, inRuntime, qt.IsNil)   // absent from the mutable runtime's own frame → a define here shadows
	qt.Assert(t, inSealed, qt.IsNotNil) // present in the sealed base's own frame
}

// TestSealedBase_BootstrapProcedureInSealedBase verifies a Scheme-defined bootstrap
// procedure (caar) lands in the sealed base, not the mutable runtime — so a later
// (import (scheme cxr)) re-export or user redefine is a shadow, not a Stable rebind.
func TestSealedBase_BootstrapProcedureInSealedBase(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	caarSym := values.NewSymbol("caar")

	inRuntime := ns.Runtime().GlobalEnvironment().GetGlobalIndex(caarSym)
	inSealed := ns.SealedBase().GlobalEnvironment().GetGlobalIndex(caarSym)

	qt.Assert(t, inRuntime, qt.IsNil)   // shadowable (absent from mutable runtime's own frame)
	qt.Assert(t, inSealed, qt.IsNotNil) // owned by the sealed base
}

// TestSealedExpandBase_BootstrapMacroLivesInSealedExpandBase verifies a Scheme-defined
// bootstrap macro (cond) lands in the sealed EXPAND base (phase 1), not the mutable expand
// child — the phase-1 analogue of TestSealedBase_BootstrapProcedureInSealedBase (D1). A
// later top-level (define-syntax cond …) is thus a shadow in the mutable child, not an
// in-place overwrite of the pinned bootstrap macro (closes the #1 stability leg).
func TestSealedExpandBase_BootstrapMacroLivesInSealedExpandBase(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	condSym := values.NewSymbol("cond")

	// Own-frame probe (no parent walk): the mutable expand child vs. the sealed expand base.
	inMutableExpand := ns.Runtime().Expand().GlobalEnvironment().GetGlobalIndex(condSym)
	inSealedExpand := ns.SealedExpandBase().GlobalEnvironment().GetGlobalIndex(condSym)

	qt.Assert(t, inMutableExpand, qt.IsNil)   // shadowable (absent from the mutable expand child's own frame)
	qt.Assert(t, inSealedExpand, qt.IsNotNil) // owned by the sealed expand base
}

// TestSealedExpandBase_SpecialFormExpanderLivesInSealedExpandBase verifies a special-form
// primitive expander (let-syntax) lands in the sealed EXPAND base after the D3 retarget, not
// the mutable expand child. This is what makes a user (define-syntax let-syntax …) a clean
// shadow instead of an in-place corruption of the installed expander (closes #3), while
// keeping the expander — a compile-time handler — off the phase-0 value frame (so it cannot
// leak into runtime value resolution).
func TestSealedExpandBase_SpecialFormExpanderLivesInSealedExpandBase(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	letSyntaxSym := values.NewSymbol("let-syntax")

	inMutableExpand := ns.Runtime().Expand().GlobalEnvironment().GetGlobalIndex(letSyntaxSym)
	inSealedExpand := ns.SealedExpandBase().GlobalEnvironment().GetGlobalIndex(letSyntaxSym)
	inSealedBase := ns.SealedBase().GlobalEnvironment().GetGlobalIndex(letSyntaxSym)

	qt.Assert(t, inMutableExpand, qt.IsNil)   // shadowable (absent from the mutable expand child)
	qt.Assert(t, inSealedExpand, qt.IsNotNil) // owned by the sealed expand base (phase 1)
	qt.Assert(t, inSealedBase, qt.IsNil)      // NOT on the phase-0 value frame (no runtime-value leak)
}

// TestBoundNamesAcrossPhases verifies the all-phases name walk that backs
// Engine.BoundNames and REPL completion: it must span the sealed base (so
// primitives like car and bootstrap procedures like caar appear, not just the
// mutable runtime's own frame), return sorted output, and deduplicate names that
// occur in more than one frame.
func TestBoundNamesAcrossPhases(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	names := ns.BoundNamesAcrossPhases()

	qt.Assert(t, len(names) > 0, qt.IsTrue)
	qt.Assert(t, sort.StringsAreSorted(names), qt.IsTrue,
		qt.Commentf("BoundNamesAcrossPhases must return sorted output"))

	seen := map[string]bool{}
	for _, n := range names {
		qt.Assert(t, seen[n], qt.IsFalse, qt.Commentf("duplicate name %q", n))
		seen[n] = true
	}

	// Sealed-base coverage: car is a Go primitive, caar a Scheme bootstrap
	// procedure — both live in the sealed base, not the mutable runtime's own
	// frame, so a runtime-only walk would miss them.
	qt.Assert(t, seen["car"], qt.IsTrue, qt.Commentf("primitive car must appear"))
	qt.Assert(t, seen["caar"], qt.IsTrue, qt.Commentf("bootstrap caar must appear"))
}

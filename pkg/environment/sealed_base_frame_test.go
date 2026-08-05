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
	"slices"
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

// TestSealedExpandBaseConstructionInvariant asserts every namespace-building entry point
// wires a non-nil sealedExpandBase parented to the sealed base at phase 1. newPhaseRegistry
// panics rather than degrading when a declared row has no frame, so a builder that forgets
// it must fail HERE — loudly — not silently degrade the expand-phase seal to the mutable
// frame (the WithStableBasePrimitives
// profile-child divergence is the cautionary precedent for an unenforced construction step).
func TestSealedExpandBaseConstructionInvariant(t *testing.T) {
	root := environment.NewNamespace()
	builders := []struct {
		name string
		ns   *environment.Namespace
	}{
		{"NewNamespace", root},
		{"NewChildNamespace", root.NewChildNamespace()},
		{"NewSchemeReportNamespace", root.NewSchemeReportNamespace()},
	}
	for _, b := range builders {
		t.Run(b.name, func(t *testing.T) {
			seb := b.ns.SealedExpandBase()
			qt.Assert(t, seb, qt.IsNotNil)
			qt.Assert(t, seb.Parent(), qt.Equals, b.ns.SealedBase())
			qt.Assert(t, seb.PhaseLevel(), qt.Equals, environment.PhaseExpand)
		})
	}
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
	qt.Assert(t, slices.IsSorted(names), qt.IsTrue,
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

// TestSealedAxisTable pins the (phase, kind) model that SealedAt states: sealing is a
// property of the PAIR, not of the phase. The phase-1 row is the one that carries the
// asymmetry — a handler there is sealed, a value is not, which is why registry
// expand-phase primitives land in the mutable expand child while special-form expanders
// land in the sealed one. Change a cell here only alongside a deliberate model change.
func TestSealedAxisTable(t *testing.T) {
	ns := environment.NewNamespace()
	cases := []struct {
		name   string
		phase  environment.Phase
		kind   environment.SealKind
		want   *environment.EnvironmentFrame
		sealed bool
	}{
		{"runtime value", environment.PhaseRuntime, environment.SealKindValue, ns.SealedBase(), true},
		{"runtime handler", environment.PhaseRuntime, environment.SealKindHandler, ns.SealedBase(), true},
		{"expand handler", environment.PhaseExpand, environment.SealKindHandler, ns.SealedExpandBase(), true},
		{"expand value", environment.PhaseExpand, environment.SealKindValue, nil, false},
		{"compile value", environment.PhaseCompile, environment.SealKindValue, nil, false},
		{"compile handler", environment.PhaseCompile, environment.SealKindHandler, nil, false},
		{"template handler", environment.PhaseTemplate, environment.SealKindHandler, nil, false},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			frame, sealed := ns.SealedAt(c.phase, c.kind)
			qt.Assert(t, sealed, qt.Equals, c.sealed)
			qt.Assert(t, frame, qt.Equals, c.want)
		})
	}
}

// TestSealedAxisIsEnumerableAndReachable pins the two structural properties the sealed
// axis has to hold at once, because a seal that fails either is invisible in a way
// nothing else reports:
//
//   - ENUMERABLE. No sealed frame is a PhaseRegistry entry, so a walk over phase frames
//     misses it. A seal absent from SealedFrames vanishes from ,apropos and REPL
//     completion rather than failing.
//   - REACHABLE. A seal nothing parents to resolves nothing. phaseParent must point the
//     mutable frame at each phase's seal, and that link is kind-independent, so a row
//     whose kinds do not happen to include the kind a caller guessed must still be wired.
//
// The sweep spans well past the phases that have seals today so a new row cannot satisfy
// one property and quietly miss the other. It does NOT pin non-nil-ness: SealedAt and
// SealedFrames read the same rows, so a nil frame would make them agree —
// newPhaseRegistry's construction-time panic is what makes that loud, and
// TestSealedExpandBaseConstructionInvariant is what pins it across builders.
func TestSealedAxisIsEnumerableAndReachable(t *testing.T) {
	ns := environment.NewNamespace()
	enumerated := make(map[*environment.EnvironmentFrame]bool)
	for _, frame := range ns.SealedFrames() {
		enumerated[frame] = true
	}
	kinds := []environment.SealKind{environment.SealKindValue, environment.SealKindHandler}
	for phase := environment.Phase(-2); phase <= 8; phase++ {
		for _, kind := range kinds {
			sealed, ok := ns.SealedAt(phase, kind)
			if !ok {
				continue
			}
			qt.Assert(t, enumerated[sealed], qt.IsTrue,
				qt.Commentf("seal at (phase %s, %s) is routable but not enumerated", phase, kind))
			qt.Assert(t, ns.IsSealed(sealed), qt.IsTrue,
				qt.Commentf("seal at (phase %s, %s) is not recognized by IsSealed", phase, kind))
			qt.Assert(t, ns.AtPhase(phase).Parent(), qt.Equals, sealed,
				qt.Commentf("the mutable frame at phase %s does not parent to its seal", phase))
		}
	}
	qt.Assert(t, ns.IsSealed(nil), qt.IsFalse) // a frame that does not exist is not sealed
}

// TestSealedClimbStopsAboveExpand pins the depth of the sealed axis: a climb rooted at a
// sealed frame stays sealed while a seal exists, and leaves it where none does. A
// bootstrap define-syntax (env == the sealed base) climbs into the sealed expand base, but
// a define-syntax inside a transformer body (env == the sealed expand base) climbs into
// the MUTABLE compile frame, because phase 2 has no seal. That is the shipped behavior, not
// an aspiration: it means the seal is exactly one level deep, and the exit is silent.
func TestSealedClimbStopsAboveExpand(t *testing.T) {
	ns := environment.NewNamespace()

	// Phase 0 seal climbing to phase 1: stays sealed.
	qt.Assert(t, ns.SealedBase().AtPhase(environment.PhaseExpand), qt.Equals, ns.SealedExpandBase())

	// Phase 1 seal climbing to phase 2: leaves the sealed axis.
	compile := ns.SealedExpandBase().AtPhase(environment.PhaseCompile)
	qt.Assert(t, ns.IsSealed(compile), qt.IsFalse)
	qt.Assert(t, compile, qt.Equals, ns.Compile())

	// The redirect is a CLIMB: it never rewrites a lookup at or below the receiver's
	// own level, so the sealed base still reaches the mutable runtime at phase 0.
	qt.Assert(t, ns.SealedBase().AtPhase(environment.PhaseRuntime), qt.Equals, ns.Runtime())
}

// TestSealedTargetAtFallbacks pins SealedTargetAt's answers for the two shapes that
// own a sealed axis, and for the (phase, kind) pairs the axis leaves unsealed. Both
// shapes answer the same way — a library env routes to its OWN seals, never the
// namespace's — because an owner does not pick a subset of the axis. The expand-phase
// VALUE cell, where registry expand primitives are registered, reaches the mutable
// expand child on both, since phase 1 seals handlers only.
//
// The library assertions live here in the EXTERNAL test package on purpose: they use
// nothing but exported API, so they would catch a subset creeping back in even if the
// internal shape tests were deleted.
func TestSealedTargetAtFallbacks(t *testing.T) {
	ns := environment.NewNamespace()
	lib := ns.NewChildRuntime()
	libBase := lib.Parent()
	libExpandBase := lib.Expand().Parent()

	qt.Assert(t, libBase, qt.Not(qt.Equals), ns.SealedBase())
	qt.Assert(t, libExpandBase, qt.Not(qt.Equals), ns.SealedExpandBase())
	qt.Assert(t, libExpandBase.Parent(), qt.Equals, libBase)
	qt.Assert(t, lib.SealedTargetAt(environment.PhaseRuntime, environment.SealKindValue), qt.Equals, libBase)
	qt.Assert(t, lib.SealedTargetAt(environment.PhaseRuntime, environment.SealKindHandler), qt.Equals, libBase)
	qt.Assert(t, lib.SealedTargetAt(environment.PhaseExpand, environment.SealKindHandler), qt.Equals, libExpandBase)
	qt.Assert(t, lib.SealedTargetAt(environment.PhaseExpand, environment.SealKindValue), qt.Equals, lib.Expand())
	qt.Assert(t, lib.SealedTargetAt(environment.PhaseCompile, environment.SealKindHandler), qt.Equals, lib.Compile())

	runtime := ns.Runtime()
	qt.Assert(t, runtime.SealedTargetAt(environment.PhaseRuntime, environment.SealKindValue), qt.Equals, ns.SealedBase())
	qt.Assert(t, runtime.SealedTargetAt(environment.PhaseExpand, environment.SealKindHandler), qt.Equals, ns.SealedExpandBase())
	qt.Assert(t, runtime.SealedTargetAt(environment.PhaseExpand, environment.SealKindValue), qt.Equals, ns.Expand())
	qt.Assert(t, runtime.SealedTargetAt(environment.PhaseCompile, environment.SealKindHandler), qt.Equals, ns.Compile())
}

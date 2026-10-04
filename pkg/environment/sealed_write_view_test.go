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
	"fmt"
	"slices"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	valuestest "github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
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

// TestCharacterization_OwnerViewTopology pins the post-fold topology: Runtime() is
// the owner's ROOT VIEW, a structural root with no lexical parent, and the
// sealed-write root is a DISTINCT view over the SAME store. This is the second
// inversion of this assertion — pre-carve Runtime() was the root, post-carve the
// sealed base was, and post-fold there is one store with two roles over it.
func TestCharacterization_OwnerViewTopology(t *testing.T) {
	ns := environment.NewNamespace()
	sealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)

	qt.Assert(t, ns.Runtime().IsTopLevel(), qt.IsTrue)
	qt.Assert(t, ns.Runtime().Parent(), qt.IsNil)
	qt.Assert(t, sealedRoot.IsTopLevel(), qt.IsTrue)
	qt.Assert(t, ns.Runtime() != sealedRoot, qt.IsTrue)
	qt.Assert(t, sealedRoot.GlobalEnvironment(), qt.Equals, ns.Store())
	qt.Assert(t, ns.Runtime().GlobalEnvironment(), qt.Equals, ns.Store())

	// Both are the namespace's own root views, which is the predicate the
	// immutable-top-level define gate keys on.
	qt.Assert(t, ns.Runtime().IsOwnerRoot(), qt.IsTrue)
	qt.Assert(t, sealedRoot.IsOwnerRoot(), qt.IsTrue)
	qt.Assert(t, ns.Expand().IsOwnerRoot(), qt.IsFalse)
}

// TestSealedTier_PrimitivesAreSealedNotMutable verifies a core Go primitive binding
// is owned by the SEALED tier and absent from the mutable tier at phase 0 — so a
// user (define car ...) creates a new slot (created=true) that shadows it rather
// than rebinding it in place. OwnGlobalIndex is the coordinate-exact probe: it asks
// each view for a binding at that view's OWN coordinates, with no tier ranking.
func TestSealedTier_PrimitivesAreSealedNotMutable(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	carSym := values.NewSymbol("car")

	inMutable := ns.Runtime().OwnGlobalIndex(carSym, values.EmptyScopes())
	sealed := ns.Store().IsSealedBindingAt(carSym, values.EmptyScopes(), environment.PhaseRuntime)

	qt.Assert(t, inMutable, qt.IsNil) // nothing at (0, mutable) → a define here shadows
	qt.Assert(t, sealed, qt.IsTrue)   // an ambient read at phase 0 lands on a sealed slot
}

// TestSealedTier_BootstrapProcedureIsSealed verifies a Scheme-defined bootstrap
// procedure (caar) lands in the sealed tier, not the mutable one — so a later
// (import (scheme cxr)) re-export or user redefine is a shadow, not a Stable rebind.
func TestSealedTier_BootstrapProcedureIsSealed(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	caarSym := values.NewSymbol("caar")

	inMutable := ns.Runtime().OwnGlobalIndex(caarSym, values.EmptyScopes())
	sealed := ns.Store().IsSealedBindingAt(caarSym, values.EmptyScopes(), environment.PhaseRuntime)

	qt.Assert(t, inMutable, qt.IsNil)
	qt.Assert(t, sealed, qt.IsTrue)
}

// TestSealedTier_BootstrapMacroIsSealedAtPhaseOne verifies a Scheme-defined bootstrap
// macro (cond) lands at (1, sealed), not (1, mutable) — the phase-1 analogue of
// TestSealedTier_BootstrapProcedureIsSealed (D1). A later top-level (define-syntax
// cond …) is thus a new slot in the mutable tier, not an in-place overwrite of the
// pinned bootstrap macro (closes the #1 stability leg).
func TestSealedTier_BootstrapMacroIsSealedAtPhaseOne(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	condSym := values.NewSymbol("cond")

	mutableExpand := ns.Runtime().Expand()
	sealedExpand := ns.Runtime().SealedWriteViewAt(environment.PhaseExpand)

	qt.Assert(t, mutableExpand.OwnGlobalIndex(condSym, values.EmptyScopes()), qt.IsNil)
	qt.Assert(t, sealedExpand.OwnGlobalIndex(condSym, values.EmptyScopes()), qt.IsNotNil)
}

// TestSealedTier_SpecialFormExpanderIsSealedAtPhaseOne verifies a special-form
// primitive expander (let-syntax) lands at (1, sealed) after the D3 retarget. That is
// what makes a user (define-syntax let-syntax …) a clean shadow instead of an in-place
// corruption of the installed expander (closes #3), while keeping the expander — a
// compile-time handler — out of the AMBIENT tier, where runtime value resolution would
// reach it.
func TestSealedTier_SpecialFormExpanderIsSealedAtPhaseOne(t *testing.T) {
	ns := testhelpers.NewBootstrappedNamespace(t)
	letSyntaxSym := values.NewSymbol("let-syntax")

	mutableExpand := ns.Runtime().Expand()
	sealedExpand := ns.Runtime().SealedWriteViewAt(environment.PhaseExpand)
	sealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)

	qt.Assert(t, mutableExpand.OwnGlobalIndex(letSyntaxSym, values.EmptyScopes()), qt.IsNil)
	qt.Assert(t, sealedExpand.OwnGlobalIndex(letSyntaxSym, values.EmptyScopes()), qt.IsNotNil)
	qt.Assert(t, sealedRoot.OwnGlobalIndex(letSyntaxSym, values.EmptyScopes()), qt.IsNil)
}

// TestSealedWriteViewConstructionInvariant asserts every namespace-building entry
// point mints the phase-1 sealed-write view over its own store. A builder that
// forgot it would silently degrade the expand-phase seal to the mutable tier (the
// WithStableBasePrimitives profile-child divergence is the cautionary precedent for
// an unenforced construction step).
func TestSealedWriteViewConstructionInvariant(t *testing.T) {
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
			view := b.ns.Runtime().SealedWriteViewAt(environment.PhaseExpand)
			qt.Assert(t, view, qt.IsNotNil)
			qt.Assert(t, view.GlobalEnvironment(), qt.Equals, b.ns.Store())
			qt.Assert(t, view.PhaseLevel(), qt.Equals, environment.PhaseExpand)
			qt.Assert(t, view, qt.Not(qt.Equals), b.ns.Expand())
		})
	}
}

// TestBoundNamesAcrossPhases verifies the all-phases name walk that backs
// Engine.BoundNames and REPL completion: it must span the sealed tier (so
// primitives like car and bootstrap procedures like caar appear, not just user
// defines), return sorted output, and deduplicate names that occur at more than one
// coordinate.
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

	// Sealed-tier coverage: car is a Go primitive, caar a Scheme bootstrap
	// procedure — both live in the sealed tier, so a mutable-only walk would miss
	// them.
	qt.Assert(t, seen["car"], qt.IsTrue, qt.Commentf("primitive car must appear"))
	qt.Assert(t, seen["caar"], qt.IsTrue, qt.Commentf("bootstrap caar must appear"))
}

// TestSealedClimbStopsAboveExpand pins where AtPhase leaves the seal. It stays
// sealed only when the receiver IS a sealed-write view and the target is above it,
// on a row: a top-level bootstrap define-syntax (env == the sealed-write root)
// climbs into the phase-1 sealed-write view. Every other case falls through to the
// MUTABLE view, silently, and three are pinned here as shipped behavior, not
// endorsed (TODO.md, "The seal is exactly one level deep"):
//
//   - the phase-1 view climbing to phase 2, which has no row;
//   - a lexical child of a sealed view, which does not inherit `sealed`, even
//     onto a row: a define-syntax in a bootstrap procedure body runs in one (a
//     transformer body is a lexical child at phase 1, so its climb to phase 2
//     takes this exit and the off-axis one);
//   - the phase-1 view asked for phase 1 itself, which is meta's route
//     (Expand(), an absolute phase) and no climb at all.
//
// Each is asserted through AtPhase and through the writer's own entry (NextPhase,
// or Expand for meta), so a fix at either layer moves this test; NextPhase also
// serves macro lookup. A fix at the compiler's write sites would not.
func TestSealedClimbStopsAboveExpand(t *testing.T) {
	ns := environment.NewNamespace()
	sealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)
	sealedExpand := ns.Runtime().SealedWriteViewAt(environment.PhaseExpand)

	// Phase 0 sealed-write view climbing to phase 1: stays sealed.
	qt.Assert(t, sealedRoot.AtPhase(environment.PhaseExpand), qt.Equals, sealedExpand)

	// Phase 1 sealed-write view climbing to phase 2: leaves the sealed axis.
	qt.Assert(t, sealedExpand.AtPhase(environment.Phase(2)), qt.Equals, ns.AtPhase(environment.Phase(2)))
	qt.Assert(t, sealedExpand.NextPhase(), qt.Equals, ns.AtPhase(environment.Phase(2)))

	// A lexical child of the sealed root climbs UNSEALED, even onto a row.
	inner := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), sealedRoot)
	qt.Assert(t, inner.AtPhase(environment.PhaseExpand), qt.Equals, ns.AtPhase(environment.PhaseExpand))
	qt.Assert(t, inner.NextPhase(), qt.Equals, ns.AtPhase(environment.PhaseExpand))

	// A transformer body: a lexical child of the phase-1 view, climbing to phase 2.
	// It takes the lexical-child and off-axis exits at once, so this goes red only
	// when both are fixed.
	transformerBody := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), sealedExpand)
	qt.Assert(t, transformerBody.NextPhase(), qt.Equals, ns.AtPhase(environment.Phase(2)))

	// meta's route from the phase-1 view: an absolute phase 1, not a climb, so the
	// mutable phase-1 view.
	qt.Assert(t, sealedExpand.Expand(), qt.Equals, ns.AtPhase(environment.PhaseExpand))

	// The redirect is a CLIMB: it never rewrites a lookup at or below the receiver's
	// own level, so the sealed-write root still reaches the ordinary view at phase 0.
	qt.Assert(t, sealedRoot.AtPhase(environment.PhaseRuntime), qt.Equals, ns.Runtime())
}

// TestSealedWriteViewAtFallbacks pins SealedWriteViewAt's answers for the two shapes
// that own a store, and for phase 2, which has no sealedAxis row
// (TestSealedWriteOffTheAxisIsRefused covers the other off-axis phases).
// Both shapes answer the same way — a library env routes to its OWN views, never the
// namespace's — because an owner does not pick a subset of the axis. Whether an
// expand-phase primitive lands sealed or mutable is not this call's question any
// more: that placement is registry.Apply's phaseTargets (apply.go).
//
// These live in the EXTERNAL test package on purpose: they use nothing but exported
// API, so they would catch a subset creeping back in even if the internal shape tests
// were deleted.
func TestSealedWriteViewAtFallbacks(t *testing.T) {
	ns := environment.NewNamespace()
	lib := ns.NewChildRuntime()
	libSealedRoot := lib.SealedWriteViewAt(environment.PhaseRuntime)
	libSealedExpand := lib.SealedWriteViewAt(environment.PhaseExpand)
	nsSealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)
	nsSealedExpand := ns.Runtime().SealedWriteViewAt(environment.PhaseExpand)

	qt.Assert(t, libSealedRoot, qt.Not(qt.Equals), nsSealedRoot)
	qt.Assert(t, libSealedExpand, qt.Not(qt.Equals), nsSealedExpand)
	qt.Assert(t, libSealedRoot.GlobalEnvironment(), qt.Equals, lib.GlobalEnvironment())
	qt.Assert(t, libSealedExpand.GlobalEnvironment(), qt.Equals, lib.GlobalEnvironment())

	qt.Assert(t, nsSealedRoot.GlobalEnvironment(), qt.Equals, ns.Store())

	// Off the axis: an owner root is REFUSED rather than quietly handed the
	// ordinary view. Both of these asserted the fall-through until 2026-10-03;
	// see TestSealedWriteOffTheAxisIsRefused for the whole rule.
	qt.Assert(t, sealedWriteRefusal(func() {
		_ = lib.SealedWriteViewAt(environment.Phase(2))
	}), qt.ErrorIs, werr.ErrNoSealedWriteView)
	qt.Assert(t, sealedWriteRefusal(func() {
		_ = ns.Runtime().SealedWriteViewAt(environment.Phase(2))
	}), qt.ErrorIs, werr.ErrNoSealedWriteView)
}

// sealedWriteRefusal runs f and returns the error it panicked with, or nil if it
// returned.
//
// A non-error payload goes through werr.RecoverAsError — the production mechanism
// for exactly this conversion, whose doc names ErrPanicRecovery as a recovering
// site's sentinel — rather than a hand-rolled wrap, so an unrelated panic still
// surfaces as a non-nil error and fails the sentinel assertion loudly instead of
// reading as "did not panic".
func sealedWriteRefusal(f func()) (q error) {
	defer func() {
		q = werr.RecoverAsError(recover(), werr.ErrPanicRecovery, "sealedWriteRefusal")
	}()
	f()
	return nil
}

// TestSealedWriteOffTheAxisIsRefused pins which phases own a sealed-write view, and
// which receivers are refused off the axis.
//
// RED on master: every owner and sealed-view off-axis row, and the phase-1 sealed
// view's phase-0 answer. Each comes back as the ordinary MUTABLE view and nothing says so, so a
// registration aimed at the seal lands where user code can overwrite it — the one
// call whose entire job is to name a sealed target, silently handing back an
// unsealed one.
//
// OFF THE AXIS IS BOTH DIRECTIONS, and the PhaseTemplate row is the reason this
// test is not a table of 2-and-up. The first cut of the fix compared against a
// derived ceiling, which answers for 2 and up and falls straight through at -1 —
// a named, exported phase (wile.PhaseTemplate) with no sealed view, so the
// comparison left the whole defect in place at a constant the package publishes.
// The condition is the MISS.
//
// A SEALED-WRITE VIEW is the other sealed receiver, and it is refused like its
// owner. It shares the owner's registry but is not an owner, so a refusal guarded
// by ownership alone sent it through AtPhase, which stays sealed only climbing
// UP onto a row: off the axis it was handed the mutable view, and asked for
// phase 0 from phase 1 it was handed the mutable ROOT. The second is a downgrade
// ON the axis, so its row asserts the sealed answer positively.
//
// What is NOT covered, stated so it is not read as an oversight:
//
//   - An UNSEALED non-owner (an inner lexical frame, including one under a
//     sealed-write view) still falls through at every phase, by the documented
//     precondition; refusing it too is filed in TODO.md. Asserted below as "does
//     not panic" off the axis, and at both axis phases as a non-nil frame at the
//     asked phase that is not the owner's sealed view: `sealed` must not be
//     inherited, or every body under a sealed view would be admitted. Asserting WHICH frame it
//     returns would restate unsealedTargetAt's implementation.
//   - AtPhase still falls through, silently, and it is the compile-time writers'
//     route (see sealedAxis). TestSealedClimbStopsAboveExpand pins its exits and
//     TODO.md keeps them open.
//
// Unreachable from production: all five non-test SealedWriteViewAt callers pass a
// literal PhaseRuntime or PhaseExpand (bootstrap_core.go, syntax_compilers_registry.go,
// primitive_expanders_registry.go, apply.go twice), which is what makes this a
// ratchet rather than a behaviour change.
func TestSealedWriteOffTheAxisIsRefused(t *testing.T) {
	ns := environment.NewNamespace()
	lib := ns.NewChildRuntime()

	// ON the axis: the owner's own sealed view, asserted positively. Inequality
	// against the ordinary view is not enough — nil satisfies that too.
	for _, owner := range []struct {
		name  string
		frame *environment.EnvironmentFrame
		store *environment.GlobalEnvironmentFrame
	}{
		{name: "namespace runtime", frame: ns.Runtime(), store: ns.Store()},
		{name: "library env", frame: lib, store: lib.GlobalEnvironment()},
	} {
		for _, phase := range []environment.Phase{environment.PhaseRuntime, environment.PhaseExpand} {
			t.Run(fmt.Sprintf("%s/on-axis phase %d", owner.name, phase), func(t *testing.T) {
				view := owner.frame.SealedWriteViewAt(phase)
				qt.Assert(t, view, qt.IsNotNil)
				qt.Assert(t, view.PhaseLevel(), qt.Equals, phase)
				qt.Assert(t, view.GlobalEnvironment(), qt.Equals, owner.store)
				qt.Assert(t, view, qt.Not(qt.Equals), owner.frame.AtPhase(phase))
				// Cached, not minted per call: the same view every time.
				qt.Assert(t, owner.frame.SealedWriteViewAt(phase), qt.Equals, view)
			})
		}
	}

	// OFF it, in BOTH directions, for every owner root.
	for _, owner := range []struct {
		name  string
		frame *environment.EnvironmentFrame
	}{
		{name: "namespace runtime", frame: ns.Runtime()},
		{name: "library env", frame: lib},
	} {
		for _, phase := range []environment.Phase{environment.PhaseTemplate, -5, 2, 3, 7} {
			t.Run(fmt.Sprintf("%s/off-axis phase %d", owner.name, phase), func(t *testing.T) {
				err := sealedWriteRefusal(func() {
					_ = owner.frame.SealedWriteViewAt(phase)
				})
				qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSealedWriteView,
					qt.Commentf("SealedWriteViewAt(%d) returned a mutable view silently; want a refusal", phase))
			})
		}
	}

	// A sealed-write view: the owner's sealed answer on the axis, in both
	// directions, and the same refusal off it.
	for _, owner := range []struct {
		name  string
		frame *environment.EnvironmentFrame
	}{
		{name: "namespace runtime", frame: ns.Runtime()},
		{name: "library env", frame: lib},
	} {
		sealedRoot := owner.frame.SealedWriteViewAt(environment.PhaseRuntime)
		sealedExpand := owner.frame.SealedWriteViewAt(environment.PhaseExpand)
		for _, receiver := range []struct {
			name  string
			frame *environment.EnvironmentFrame
		}{
			{name: "sealed phase 0", frame: sealedRoot},
			{name: "sealed phase 1", frame: sealedExpand},
		} {
			t.Run(fmt.Sprintf("%s/%s/on-axis", owner.name, receiver.name), func(t *testing.T) {
				qt.Assert(t, receiver.frame.SealedWriteViewAt(environment.PhaseRuntime), qt.Equals, sealedRoot)
				qt.Assert(t, receiver.frame.SealedWriteViewAt(environment.PhaseExpand), qt.Equals, sealedExpand)
			})
			for _, phase := range []environment.Phase{environment.PhaseTemplate, 2} {
				t.Run(fmt.Sprintf("%s/%s/off-axis phase %d", owner.name, receiver.name, phase), func(t *testing.T) {
					err := sealedWriteRefusal(func() {
						_ = receiver.frame.SealedWriteViewAt(phase)
					})
					qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSealedWriteView,
						qt.Commentf("sealed receiver: SealedWriteViewAt(%d) returned a mutable view silently; want a refusal", phase))
				})
			}
		}
	}

	// An unsealed non-owner receiver is NOT refused, at any phase off the axis,
	// including one whose parent is a sealed-write view. Under a sealed view it is
	// not ADMITTED either: it gets its own view, not the owner's sealed one.
	sealedViews := map[environment.Phase]*environment.EnvironmentFrame{
		environment.PhaseRuntime: ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime),
		environment.PhaseExpand:  ns.Runtime().SealedWriteViewAt(environment.PhaseExpand),
	}
	for _, parent := range []struct {
		name  string
		frame *environment.EnvironmentFrame
	}{
		{name: "under the runtime", frame: ns.Runtime()},
		{name: "under the sealed root", frame: sealedViews[environment.PhaseRuntime]},
		{name: "under the sealed expand view", frame: sealedViews[environment.PhaseExpand]},
	} {
		inner := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), parent.frame)
		for _, phase := range []environment.Phase{environment.PhaseRuntime, environment.PhaseExpand} {
			t.Run(fmt.Sprintf("non-owner %s/not admitted at phase %d", parent.name, phase), func(t *testing.T) {
				// Recovered, so a refusal reports here instead of aborting the binary;
				// non-nil and at the asked phase, since nil satisfies the inequality.
				var got *environment.EnvironmentFrame
				qt.Assert(t, sealedWriteRefusal(func() {
					got = inner.SealedWriteViewAt(phase)
				}), qt.IsNil)
				qt.Assert(t, got, qt.IsNotNil)
				qt.Assert(t, got.PhaseLevel(), qt.Equals, phase)
				qt.Assert(t, got, qt.Not(qt.Equals), sealedViews[phase])
			})
		}
		for _, phase := range []environment.Phase{environment.PhaseTemplate, 2} {
			t.Run(fmt.Sprintf("non-owner %s/phase %d", parent.name, phase), func(t *testing.T) {
				qt.Assert(t, sealedWriteRefusal(func() {
					_ = inner.SealedWriteViewAt(phase)
				}), qt.IsNil)
			})
		}
	}
}

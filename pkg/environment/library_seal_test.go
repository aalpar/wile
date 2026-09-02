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

package environment

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// A library env is a full OWNER: its own store, its own registry, its own views.
// It shares the caller's Namespace (that is what keeps syntax interning
// consistent) and nothing else, which is what gives a library body its own
// bindings while still reaching primitives.
//
// See memory/2026-08-04-library-phase-isolation-impl.local.md Task 3 and
// memory/2026-08-05-flat-binding-model-design.local.md §4.6.
func TestChildRuntimeOwnsItsOwnStore(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()

	// A view, so a structural root: no lexical parent to walk.
	c.Assert(lib.IsTopLevel(), qt.IsTrue)
	c.Assert(lib.Parent(), qt.IsNil)
	c.Assert(lib.PhaseLevel(), qt.Equals, PhaseRuntime)

	// The store is the LIBRARY's, not the shared namespace's.
	c.Assert(lib.GlobalEnvironment(), qt.Not(qt.Equals), ns.Store())
	c.Assert(lib.Namespace(), qt.Equals, ns)

	// It is not one of the namespace's root views, which is what keeps a library
	// body's cross-form define/set! mutable (R2).
	c.Assert(lib.IsOwnerRoot(), qt.IsFalse)
	c.Assert(ns.Runtime().IsOwnerRoot(), qt.IsTrue)

	// Its own phase registry, so AtPhase resolves to the LIBRARY's views.
	c.Assert(lib.AtPhase(PhaseExpand), qt.Not(qt.Equals), ns.AtPhase(PhaseExpand))
	c.Assert(lib.AtPhase(PhaseExpand).phases, qt.Equals, lib.phases)
}

// Every library phase view shares the library's ONE store, exactly as a
// namespace's do. Hermeticity is key disjointness in that store — a phase-N read
// admits only phase-N and ambient slots — not a parent link that skips the
// mutable frame, so there is no per-phase global to diverge from the namespace's
// shape.
func TestChildRuntimePhaseViewsShareItsStore(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()
	store := lib.GlobalEnvironment()

	for _, phase := range []Phase{PhaseExpand, Phase(2), Phase(3)} {
		view := lib.AtPhase(phase)
		c.Assert(view.GlobalEnvironment(), qt.Equals, store, qt.Commentf("phase %s", phase))
		c.Assert(view.Parent(), qt.IsNil, qt.Commentf("phase %s", phase))
		c.Assert(view.PhaseLevel(), qt.Equals, phase, qt.Commentf("phase %s", phase))
	}

	// Same shape on the namespace side, so a divergence shows up here rather than
	// as a library-only behaviour difference nobody thought to test for.
	c.Assert(ns.AtPhase(PhaseExpand).GlobalEnvironment(), qt.Equals, ns.Store())
	c.Assert(ns.AtPhase(Phase(2)).GlobalEnvironment(), qt.Equals, ns.Store())
}

// A library env mints the WHOLE sealed axis of sealed-write views, not a subset.
// An owner choosing which rows to build would make sealedAxis describe only some
// owners, so that "does this phase have a sealed-write view?" needed a "for
// whom?". The answers below are structurally identical to a namespace's,
// differing only in WHICH views they name.
func TestChildRuntimeMirrorsTheWholeSealedAxis(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()

	for _, phase := range sealedAxis {
		libView, libOK := lib.phases.sealedViewAt(phase)
		nsView, nsOK := ns.phases.sealedViewAt(phase)
		c.Assert(libOK, qt.Equals, nsOK, qt.Commentf("phase %s", phase))
		c.Assert(libOK, qt.IsTrue, qt.Commentf("phase %s", phase))
		c.Assert(libView, qt.Not(qt.Equals), nsView, qt.Commentf("phase %s", phase))
		// A sealed-write view is the same store at the same phase, differing only
		// in whether its writes stamp the sealed tier.
		c.Assert(libView.GlobalEnvironment(), qt.Equals, lib.GlobalEnvironment(), qt.Commentf("phase %s", phase))
		c.Assert(libView.sealed, qt.IsTrue, qt.Commentf("phase %s", phase))
	}

	base, _ := lib.phases.sealedViewAt(PhaseRuntime)
	expandBase, _ := lib.phases.sealedViewAt(PhaseExpand)
	c.Assert(lib.SealedWriteViewAt(PhaseRuntime), qt.Equals, base)
	c.Assert(lib.SealedWriteViewAt(PhaseExpand), qt.Equals, expandBase)
	// Whether an expand-phase primitive lands sealed or mutable is not askable
	// here — that placement is registry.Apply's phaseTargets (apply.go), which
	// writes through the ordinary expand view.
	c.Assert(lib.SealedWriteViewAt(Phase(2)), qt.Equals, lib.AtPhase(Phase(2)))
}

// A sealed write at phase 0 lands at the AMBIENT coordinate — visible from every
// phase — while every other sealed write is exact-phase. That single branch in
// writeCoordinates is where the pre-fold topology went: the phase-0 seal's global
// was ambient because every phase frame's parent chain ran through it, and the
// phase-1 seal's was exact because none ran through that one.
func TestSealedWriteCoordinates(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()

	phase, sealed := ns.Runtime().writeCoordinates()
	c.Assert(sealed, qt.IsFalse)
	c.Assert(phase, qt.Equals, ExactPhase(PhaseRuntime))

	phase, sealed = ns.Runtime().SealedWriteViewAt(PhaseRuntime).writeCoordinates()
	c.Assert(sealed, qt.IsTrue)
	c.Assert(phase, qt.Equals, AnyPhase())

	phase, sealed = ns.Runtime().SealedWriteViewAt(PhaseExpand).writeCoordinates()
	c.Assert(sealed, qt.IsTrue)
	c.Assert(phase, qt.Equals, ExactPhase(PhaseExpand))

	phase, sealed = ns.AtPhase(PhaseExpand).writeCoordinates()
	c.Assert(sealed, qt.IsFalse)
	c.Assert(phase, qt.Equals, ExactPhase(PhaseExpand))
}

// A bootstrap macro compiled against a library's phase-0 sealed-write view must
// land in that library's phase-1 sealed-write view, by the same AtPhase climb a
// namespace uses. This is the path loadBootstrapMacros takes (LoadBootstrapCore
// hands it the SealedWriteViewAt result as its target), so getting it wrong puts
// a library's bootstrap macros where a library-body define-syntax shares a slot
// with them rather than shadowing.
func TestChildRuntimeSealedClimbReachesItsExpandView(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()
	base, _ := lib.phases.sealedViewAt(PhaseRuntime)
	expandBase, _ := lib.phases.sealedViewAt(PhaseExpand)

	c.Assert(base.NextPhase(), qt.Equals, expandBase)
	// The climb never rewrites a lookup at or below the receiver's own level.
	c.Assert(base.AtPhase(PhaseRuntime), qt.Equals, lib)
	// Above the axis there is no sealed-write view, so it falls through to the
	// ordinary phase view.
	c.Assert(expandBase.AtPhase(Phase(2)), qt.Equals, lib.AtPhase(Phase(2)))
}

// SealedWriteViewAt answers only an OWNER ROOT — the phase-0 entry of its own
// registry. A namespace's runtime and a library env's each own one; an inner
// lexical frame merely inherits the `phases` pointer and must NOT be able to
// obtain a sealed writer through it. Testing `phases != nil` alone made every
// lambda body a route to the sealed tier.
func TestSealedWriteViewAtRequiresAnOwnerRoot(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	sealedRoot, _ := ns.phases.sealedViewAt(PhaseRuntime)

	c.Assert(ns.Runtime().SealedWriteViewAt(PhaseRuntime), qt.Equals, sealedRoot)
	c.Assert(ns.NewChildRuntime().SealedWriteViewAt(PhaseRuntime).sealed, qt.IsTrue)

	// A lexical child shares the registry and is answered with ITSELF: its own
	// view at phase 0, mutable, not the owner's sealed writer.
	inner := NewEnvironmentFrameWithParent(NewLocalEnvironment(1), ns.Runtime())
	c.Assert(inner.SealedWriteViewAt(PhaseRuntime), qt.Equals, inner)
	c.Assert(inner.SealedWriteViewAt(PhaseRuntime).sealed, qt.IsFalse)

	// The reach a sealed-write view has by climbing is unaffected: AtPhase from a
	// sealed view stays sealed wherever the axis has a row, so the guard costs
	// this path nothing.
	sealedExpand, _ := ns.phases.sealedViewAt(PhaseExpand)
	c.Assert(sealedRoot.SealedWriteViewAt(PhaseExpand), qt.Equals, sealedExpand)

	// A frame with no registry at all has nothing to ask, so it falls back to its
	// own view at that phase.
	detached := newEnvironmentFrame(nil, NewGlobalEnvironmentFrame())
	c.Assert(detached.SealedWriteViewAt(PhaseRuntime), qt.Equals, detached)
}

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

// A library env is a TWO-LEVEL phase-0 stack, the same shape a namespace has:
// an immutable base (registry apply) parented by a mutable child (the library's
// own defines). The split is what lets a library phase frame reach primitives
// without reaching phase-0 user defines.
//
// See plans/2026-08-04-library-phase-isolation-impl.local.md Task 3.
func TestChildRuntimeOwnsItsOwnSealedAxis(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()

	// The mutable child is no longer the structural root; its base is.
	c.Assert(lib.IsTopLevel(), qt.IsFalse)
	c.Assert(lib.Parent(), qt.IsNotNil)
	c.Assert(lib.Parent().IsTopLevel(), qt.IsTrue)
	c.Assert(lib.Parent().PhaseLevel(), qt.Equals, PhaseRuntime)

	// The base is the LIBRARY's, not the shared namespace's.
	c.Assert(lib.Parent(), qt.Not(qt.Equals), ns.SealedBase())
	c.Assert(lib.Namespace(), qt.Equals, ns)

	// The base shares the library's phase registry, so AtPhase — which enters
	// through TopLevel(), now the base — resolves to the LIBRARY's phases and
	// not the shared root's.
	c.Assert(lib.Parent().phases, qt.Equals, lib.phases)
	c.Assert(lib.AtPhase(PhaseExpand), qt.Not(qt.Equals), ns.AtPhase(PhaseExpand))
}

// Every library phase frame parents onto the library's SEALED axis — never onto the
// mutable frame below it. That is the hermeticity cut; before it, a library's phase-1
// frame parented to its own phase-0 frame, which is why a for-syntax body could see
// the library's runtime defines.
//
// A phase with a seal of its own parents to that seal; a phase without one parents to
// the phase-0 seal rather than to the phase below it, which is the climbing-tower
// invariant that the mutable axis introduces no phase->phase edge. Both rules are the
// namespace's, unchanged — that is the point.
func TestChildRuntimePhaseFramesParentToItsSealedAxis(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()
	base, _ := lib.phases.sealAt(PhaseRuntime)
	expandBase, _ := lib.phases.sealAt(PhaseExpand)

	c.Assert(lib.AtPhase(PhaseExpand).Parent(), qt.Equals, expandBase)
	c.Assert(lib.AtPhase(PhaseCompile).Parent(), qt.Equals, base)
	c.Assert(lib.AtPhase(Phase(3)).Parent(), qt.Equals, base)

	// Same shape on the namespace side, so a divergence shows up here rather than as
	// a library-only behaviour difference nobody thought to test for.
	c.Assert(ns.AtPhase(PhaseExpand).Parent(), qt.Equals, ns.SealedExpandBase())
	c.Assert(ns.AtPhase(PhaseCompile).Parent(), qt.Equals, ns.SealedBase())
}

// A library env declares the WHOLE sealed axis, not a subset of it. An owner
// choosing which rows to build would make sealedAxis describe only some owners, so
// that "is this phase sealed?" needed a "for whom?". The routing answers
// below are therefore structurally identical to a namespace's, differing only in
// WHICH frames they name — which is the assertion the loop makes directly.
func TestChildRuntimeMirrorsTheWholeSealedAxis(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()

	for _, phase := range sealedAxis {
		libSeal, libOK := lib.phases.sealAt(phase)
		nsSeal, nsOK := ns.phases.sealAt(phase)
		c.Assert(libOK, qt.Equals, nsOK, qt.Commentf("phase %s", phase))
		c.Assert(libOK, qt.IsTrue, qt.Commentf("phase %s", phase))
		c.Assert(libSeal, qt.Not(qt.Equals), nsSeal, qt.Commentf("phase %s", phase))
	}

	base, _ := lib.phases.sealAt(PhaseRuntime)
	expandBase, _ := lib.phases.sealAt(PhaseExpand)
	c.Assert(base, qt.Equals, lib.Parent())
	c.Assert(expandBase.Parent(), qt.Equals, base)

	c.Assert(lib.SealedTargetAt(PhaseRuntime), qt.Equals, base)
	c.Assert(lib.SealedTargetAt(PhaseExpand), qt.Equals, expandBase)
	// Whether an expand-phase primitive lands in the seal or the mutable expand
	// child is no longer askable here — that placement is registry.Apply's
	// phaseTargets (apply.go), not the sealed axis.
	c.Assert(lib.SealedTargetAt(PhaseCompile), qt.Equals, lib.Compile())
}

// A bootstrap macro compiled against a library's phase-0 seal must land in that
// library's phase-1 seal, by the same AtPhase climb a namespace uses. This is the
// path loadBootstrapMacros takes (LoadBootstrapCore hands it the SealedTargetAt
// result as its target), so getting it wrong puts a library's bootstrap macros in a
// frame that a library-body define-syntax shares rather than shadows.
func TestChildRuntimeSealedClimbReachesItsExpandSeal(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()
	lib := ns.NewChildRuntime()
	base, _ := lib.phases.sealAt(PhaseRuntime)
	expandBase, _ := lib.phases.sealAt(PhaseExpand)

	c.Assert(base.NextPhase(), qt.Equals, expandBase)
	// The climb never rewrites a lookup at or below the receiver's own level.
	c.Assert(base.AtPhase(PhaseRuntime), qt.Equals, lib)
	// Above the axis there is no seal, so it falls through to the mutable frame.
	c.Assert(expandBase.AtPhase(PhaseCompile), qt.Equals, lib.Compile())
}

// The sealed-routing discriminator is "does this frame own its registry's phase-0
// slot", not "is it its namespace's runtime". An inner lexical frame shares its
// parent's registry and must NOT route, or a binding registered against a lambda
// body would land in the seal.
func TestOwnsSealedAxisDiscriminates(t *testing.T) {
	c := qt.New(t)
	ns := NewNamespace()

	c.Assert(ns.Runtime().ownsSealedAxis(), qt.IsTrue)
	c.Assert(ns.NewChildRuntime().ownsSealedAxis(), qt.IsTrue)
	c.Assert(ns.SealedBase().ownsSealedAxis(), qt.IsFalse)

	inner := NewEnvironmentFrameWithParent(NewLocalEnvironment(1), ns.Runtime())
	c.Assert(inner.ownsSealedAxis(), qt.IsFalse)
	c.Assert(inner.SealedTargetAt(PhaseRuntime), qt.Equals, inner)
}

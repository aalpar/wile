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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// TestMustSealPanicsOnDeclaredButMissingSeal is the discriminating check for the whole
// sealed-axis model: "this pair has no seal" and "the seal this row declares does not
// exist" must not be the same answer. If a missing frame merely read as unsealed, every
// routing caller would take its mutable-frame fallback and land a bootstrap macro or a
// special-form expander somewhere a user can overwrite in place — surfacing arbitrarily
// far away as a dead let-syntax, with nothing naming the namespace-construction bug.
//
// Only reachable from inside the package: a namespace built through any exported
// constructor always wires both seals (TestSealedExpandBaseConstructionInvariant).
func TestMustSealPanicsOnDeclaredButMissingSeal(t *testing.T) {
	assertPanics := func(t *testing.T, phase Phase, run func(ns *Namespace)) {
		t.Helper()
		ns := NewNamespace()
		ns.sealedExpandBase = nil
		defer func() {
			r := recover()
			qt.Assert(t, r, qt.IsNotNil, qt.Commentf("phase %s must panic, not degrade", phase))
			err, ok := r.(error)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, errors.Is(err, werr.ErrUnexpectedNil), qt.IsTrue)
		}()
		run(ns)
	}

	t.Run("SealedAt", func(t *testing.T) {
		assertPanics(t, PhaseExpand, func(ns *Namespace) {
			ns.SealedAt(PhaseExpand, SealKindHandler) //nolint:errcheck
		})
	})
	t.Run("sealedFrameAt", func(t *testing.T) {
		assertPanics(t, PhaseExpand, func(ns *Namespace) {
			ns.sealedFrameAt(PhaseExpand) //nolint:errcheck
		})
	})
	t.Run("SealedFrames", func(t *testing.T) {
		assertPanics(t, PhaseExpand, func(ns *Namespace) {
			ns.SealedFrames()
		})
	})
	t.Run("AtPhase climb", func(t *testing.T) {
		assertPanics(t, PhaseExpand, func(ns *Namespace) {
			ns.SealedBase().AtPhase(PhaseExpand)
		})
	})
	t.Run("SealedTargetAt", func(t *testing.T) {
		assertPanics(t, PhaseExpand, func(ns *Namespace) {
			ns.Runtime().SealedTargetAt(PhaseExpand, SealKindHandler)
		})
	})
	t.Run("phaseParent", func(t *testing.T) {
		assertPanics(t, PhaseExpand, func(ns *Namespace) {
			ns.AtPhase(PhaseExpand)
		})
	})
}

// TestIsSealedRejectsNilFrame guards the amplifier: without the nil check IsSealed would
// match a nil row frame and answer "yes, that is one of my seals" about a frame that does
// not exist — on exactly the broken-namespace path where the panic above fires.
func TestIsSealedRejectsNilFrame(t *testing.T) {
	ns := NewNamespace()
	ns.sealedExpandBase = nil
	qt.Assert(t, ns.IsSealed(nil), qt.IsFalse)
}

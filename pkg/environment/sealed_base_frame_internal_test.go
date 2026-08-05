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

// TestSealedAxisRejectsAnIncompleteAxis is the discriminating check for the whole
// sealed-axis model: "this phase has no sealedAxis row" and "this owner did not build
// the row" must not be the same answer. If a missing frame merely read as unsealed,
// every routing caller would take its mutable-frame fallback and land a bootstrap macro
// or a special-form expander somewhere a user can overwrite in place — surfacing
// arbitrarily far away as a dead let-syntax, with nothing naming the construction bug.
//
// The check is at CONSTRUCTION, not at each read: an owner does not choose a subset of
// the axis, so an incomplete map is a builder bug and nothing else. Only reachable from
// inside the package — every exported constructor goes through newSealedAxisFrames
// (TestSealedExpandBaseConstructionInvariant).
func TestSealedAxisRejectsAnIncompleteAxis(t *testing.T) {
	ns := NewNamespace()
	for _, tc := range []struct {
		name  string
		seals map[Phase]*EnvironmentFrame
	}{
		{"declared row present but nil", map[Phase]*EnvironmentFrame{
			PhaseRuntime: ns.sealedBase,
			PhaseExpand:  nil,
		}},
		{"declared row missing entirely", map[Phase]*EnvironmentFrame{
			PhaseRuntime: ns.sealedBase,
		}},
		{"missing the phase-0 root", map[Phase]*EnvironmentFrame{
			PhaseExpand: ns.sealedExpandBase,
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			defer func() {
				r := recover()
				qt.Assert(t, r, qt.IsNotNil, qt.Commentf("an incomplete axis must panic, not degrade"))
				err, ok := r.(error)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, errors.Is(err, werr.ErrUnexpectedNil), qt.IsTrue)
			}()
			newPhaseRegistry(ns, ns.runtime, tc.seals)
		})
	}
}

// TestIsSealedRejectsNilFrame guards the amplifier: without the nil check IsSealed would
// match a nil map entry and answer "yes, that is one of my seals" about a frame that does
// not exist.
func TestIsSealedRejectsNilFrame(t *testing.T) {
	ns := NewNamespace()
	qt.Assert(t, ns.IsSealed(nil), qt.IsFalse)
}

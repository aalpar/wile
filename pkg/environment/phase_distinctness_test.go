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
	"fmt"
	"sort"
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// This file is the RED half of design §6.1's fifth row, written before any
// Stage A mechanism (plans/2026-09-08-flatt-binding-model-a-impl, Task 2).
// Task 7 deletes the skip.
//
// It is INTERNAL (package environment, not environment_test) for the reason
// A7/A17 gives: PhaseKey, ExactPhase, AnyPhase, AmbientBinding and AmbientKeysAt
// are all exported, but PhaseKey's FIELDS, slotRef, exactPhases and
// GlobalIndex.phase are not, so no exported accessor surfaces a slot's phase
// coordinate. The behavioural twin that expresses the same property through
// public API — and therefore survives the PhaseKey deletion — is
// TestPhase1DoesNotReachPhase0Sealed in pkg/wile/phase_distinctness_test.go.

// ambientSlotCensus lists every LIVE slot in the store whose phase coordinate is
// the ANY wildcard, rendered "name@(phase,sealed)" and sorted. A test-file method
// rather than a production accessor: nothing outside this package needs the
// coordinate, and Task 7 deletes the field this reads.
func (p *GlobalEnvironmentFrame) ambientSlotCensus() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()

	q := []string{}
	for k, slots := range p.keys {
		for _, s := range slots {
			if !s.phase.wildcard {
				continue
			}
			if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
				continue
			}
			q = append(q, fmt.Sprintf("%s@(%s,sealed=%t)", k.Key, s.phase.String(), s.sealed))
		}
	}
	sort.Strings(q)
	return q
}

// TestNoAmbientCoordinateExists pins that no slot in a populated store carries
// the ANY phase coordinate. RED on master; GREEN after Task 7 deletes the
// ambient tier and PhaseKey becomes a bare Phase.
//
// A fully BOOTSTRAPPED store is out of reach here: bootstrapping runs through
// pkg/internal/bootstrap and pkg/wile, both of which import this package, so a
// full Engine cannot be built from inside it. The store is instead populated
// through the SAME production write path the bootstrap uses —
// SealedWriteViewAt(phase).MaybeCreateOwnGlobalBinding, which is verbatim what
// registry.Apply's registerCompileTimeBinding (pkg/registry/apply.go) calls, and
// what LoadBootstrapCore reaches through WithRuntimeTarget. writeCoordinates
// (environment_frame.go) is the single function that decides the coordinate, so
// exercising both sealedAxis rows through it covers every way an ambient slot is
// minted. The 309-slot engine-root census is the twin's business, not this
// test's.
//
// Every owner KIND is covered because SealedWriteViewAt is per-owner: a fix that
// only re-coordinated the namespace root would leave library envs ambient.
func TestNoAmbientCoordinateExists(t *testing.T) {
	t.Skip("RED pin for plans/2026-09-08-flatt-binding-model-a-impl Task 2; Task 7 deletes this skip")

	ns := NewNamespace()
	owners := map[string]*EnvironmentFrame{
		"namespace":   ns.Runtime(),
		"child ns":    ns.NewChildNamespace().Runtime(),
		"report ns":   ns.NewSchemeReportNamespace().Runtime(),
		"library env": ns.NewChildRuntime(),
	}
	for name, owner := range owners {
		t.Run(name, func(t *testing.T) {
			// The sealed registration path, both rows: phase 0 is what
			// registry.Apply's WithRuntimeTarget leg and LoadBootstrapCore write
			// through, phase 1 what the primitive expanders and the phase-1
			// primitive copies write through.
			for _, phase := range sealedAxis {
				view := owner.SealedWriteViewAt(phase)
				sym := values.NewSymbol(fmt.Sprintf("sealed-at-%d", phase))
				_, created := view.MaybeCreateOwnGlobalBinding(sym, BindingTypePrimitive, nil)
				qt.Assert(t, created, qt.IsTrue, qt.Commentf("phase %s", phase))
			}
			// An ordinary top-level define, for contrast: it must not be ambient
			// either, and it is the case that would mask a degenerate "census is
			// empty because nothing was written" pass.
			_, err := owner.DefineOwnGlobal(values.NewSymbol("user-define"), BindingTypeVariable, nil, values.Void)
			qt.Assert(t, err, qt.IsNil)

			census := owner.GlobalEnvironment().ambientSlotCensus()
			qt.Assert(t, census, qt.HasLen, 0,
				qt.Commentf("store still mints slots at the ANY phase coordinate: %v", census))
		})
	}
}

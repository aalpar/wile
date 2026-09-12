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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// This file is the successor to phase_distinctness_test.go, which commit
// ce0ffe88 deleted whole.
//
// WHAT IT RESTORES: the SHAPE and the invariant. That file censused every live
// slot carrying the ANY phase coordinate and asserted the census empty — a
// coordinate value that means nothing must not exist in a populated store. This
// file censuses every installed bulk row carrying BulkOriginUnknown and asserts
// the same thing about the same kind of value.
//
// WHAT IT CANNOT RESTORE: the subject. The wildcard coordinate is gone for good
// — Stage B collapsed PhaseKey to a bare Phase, so there is no wildcard field
// to read and ambientSlotCensus cannot be written at all. Nor is the original
// assertion merely relocated: a slot census and a row census walk different
// structures. This is the successor invariant, and the old one has no live
// subject to be weakened into.
//
// It stays INTERNAL (package environment) for the reason the deleted file gave
// for its own placement, one step milder: EachBulkRow is exported, so an
// external census is possible, but bulkRef and bulkRows are not, and the twin
// that DOES run over a fully bootstrapped engine is TestEveryOriginRowIsLanguage-
// Declared in pkg/wile. That twin asserts something strictly stronger — every
// row is BulkOriginLanguage — over exactly one store. This one asserts the
// weaker, longer-lived property over every OWNER KIND, and it is the one that
// survives the day a genuine BulkOriginImport row ships and sends the stronger
// ratchet red by design.

// unknownOriginRowCensus lists every installed bulk row whose origin is the
// zero value, rendered "(phase,sealed)" and sorted.
//
// A test-file method rather than a production accessor, as ambientSlotCensus
// was: nothing outside this package needs to ask, and the answer is supposed to
// be the empty list forever.
func (p *GlobalEnvironmentFrame) unknownOriginRowCensus() []string {
	q := []string{}
	p.EachBulkRow(func(_ []*syntax.Scope, phase Phase, sealed bool, origin BulkOrigin) bool {
		if origin != BulkOriginUnknown {
			return true
		}
		q = append(q, fmt.Sprintf("(phase=%s,sealed=%t)", phase, sealed))
		return true
	})
	sort.Strings(q)
	return q
}

// TestNoInstalledRowHasAnUnknownOrigin pins that no row in a populated store
// carries an origin nothing can rank.
//
// GUARD, not a pin: with the installers refusing BulkOriginUnknown the property
// holds by construction, and TestBulkRowInstallersRefuseUnknownOrigin is what
// goes red if that refusal is removed. What this defends is the OTHER way in —
// a future install path that appends a bulkRef directly, or an origin field
// added to a struct literal and left unset, neither of which passes through an
// installer for the refusal to catch.
//
// Every owner KIND is censused because the row installers are per-store: a
// change that only wired the namespace root correctly would leave library envs
// and report environments holding unranked rows.
//
// The store is populated through the production install paths before the
// census, including the macro-phase TEMPLATE path with a phase view minted
// after it, so installMacroRowLocked's copy of the template is enumerated too.
// Without that the test would have the degenerate pass the deleted one warned
// about: a census that is empty because nothing was ever installed.
func TestNoInstalledRowHasAnUnknownOrigin(t *testing.T) {
	owners := []struct {
		name string
		make func(ns *Namespace) *EnvironmentFrame
	}{
		{
			name: "namespace",
			make: func(ns *Namespace) *EnvironmentFrame {
				return ns.Runtime()
			},
		},
		{
			name: "child ns",
			make: func(ns *Namespace) *EnvironmentFrame {
				return ns.NewChildNamespace().Runtime()
			},
		},
		{
			name: "report ns",
			make: func(ns *Namespace) *EnvironmentFrame {
				return ns.NewSchemeReportNamespace().Runtime()
			},
		},
		{
			name: "library env",
			make: func(ns *Namespace) *EnvironmentFrame {
				return ns.NewChildRuntime()
			},
		},
	}
	for _, tc := range owners {
		t.Run(tc.name, func(t *testing.T) {
			// A fresh namespace per subtest: NewSchemeReportNamespace copies the
			// parent's store, so a shared namespace would make one subtest's rows
			// another's input and the census order-dependent.
			owner := tc.make(NewNamespace())
			store := owner.GlobalEnvironment()
			before := store.BulkRowCount()

			// Both install paths, over the two phases installInitialImports uses.
			for _, phase := range sealedAxis {
				src := NewSealedStoreBulkSource(store, PhaseRuntime, BaseSourceName())
				store.InstallBulkRow(src, nil, phase, true, BulkOriginLanguage)
			}
			vocab := NewFilteredBulkSource(
				NewSealedStoreBulkSource(store, PhaseRuntime, values.NewSymbol("#%vocab")),
				func(string) bool {
					return true
				},
				map[string]struct{}{},
				values.NewSymbol("#%vocab"),
			)
			store.InstallMacroPhaseRow(vocab, nil, true, BulkOriginLanguage)
			// Minting the phase view is what turns the template into a row; the
			// template alone is not in bulkRows and EachBulkRow would not see it.
			store.EnsureMacroPhaseRows(PhaseExpand)

			qt.Assert(t, store.BulkRowCount(), qt.Equals, before+3,
				qt.Commentf("the census must have rows to walk: two direct installs plus one materialized macro-phase template"))

			census := store.unknownOriginRowCensus()
			qt.Assert(t, census, qt.HasLen, 0,
				qt.Commentf("store holds bulk rows nothing can rank: %v", census))
		})
	}
}

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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// SamePin is EqualTo plus the coordinates and query a pin will RE-RESOLVE with.
// The two predicates are asserted together in every row, because the whole
// point of adding one is that it answers differently from the other.
//
// This test lives here rather than beside its consumer because Go attributes
// coverage per test binary: a pkg/machine test cannot cover new exported
// surface in pkg/environment, and covercheck's 80% floor would move against
// this package.
func TestSamePinDiscriminatesTheHealQuery(t *testing.T) {
	scope := syntax.NewScope()
	sym := values.NewSymbol("x")

	ns := NewNamespace().Runtime()
	_, created := ns.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable, syntax.Scopes{})
	qt.Assert(t, created, qt.IsTrue)

	empty := ns.GetGlobalIndexWithScopes(sym, syntax.EmptyScopes())
	all := ns.GetGlobalIndexWithScopes(sym, syntax.AllScopes())
	scoped := ns.GetGlobalIndexWithScopes(sym, syntax.ScopesOf(syntax.Scopes{}.Add(scope)))
	againEmpty := ns.GetGlobalIndexWithScopes(sym, syntax.EmptyScopes())

	for _, row := range []struct {
		name    string
		a, b    *GlobalIndex
		equalTo bool
		samePin bool
	}{
		{
			// The pair the pool was merging. Identical present denotation,
			// different future one — and their scope MEMBERS are identical, so a
			// members-only comparison still merges them. IsAll() is the only
			// discriminator, which is why it is compared separately.
			name: "wildcard against empty: same denotation, different heal",
			a:    empty, b: all, equalTo: true, samePin: false,
		},
		{
			name: "a scoped query is a different heal too",
			a:    empty, b: scoped, equalTo: true, samePin: false,
		},
		{
			// The non-vacuity row: two DISTINCT pin objects agreeing on
			// everything must still be one pin, or SamePin has stopped
			// deduplicating rather than started discriminating.
			name: "two pins of one reference are one pin",
			a:    empty, b: againEmpty, equalTo: true, samePin: true,
		},
		{
			name: "a pin is itself",
			a:    empty, b: empty, equalTo: true, samePin: true,
		},
		{
			name: "nil against a pin",
			a:    nil, b: empty, equalTo: false, samePin: false,
		},
		{
			// equalTo is FALSE here, and that is EqualTo's pre-existing
			// typed-nil behaviour rather than anything this task changed: its
			// parameter is values.Value, so a nil *GlobalIndex boxed into the
			// interface makes `value == nil` false and the nil-nil arm never
			// fires. SamePin takes a *GlobalIndex, so its nil check is a real
			// one. Asserted rather than avoided, because this is the typed-nil
			// hazard and a reader comparing the two columns deserves to see
			// where it bites.
			name: "nil against nil",
			a:    nil, b: nil, equalTo: false, samePin: true,
		},
	} {
		t.Run(row.name, func(t *testing.T) {
			qt.Assert(t, row.a.SamePin(row.b), qt.Equals, row.samePin)
			qt.Assert(t, row.b.SamePin(row.a), qt.Equals, row.samePin,
				qt.Commentf("SamePin must be symmetric"))
			qt.Assert(t, row.a.EqualTo(row.b), qt.Equals, row.equalTo,
				qt.Commentf("EqualTo is unchanged by this task and asserted here to prove it"))
		})
	}
}

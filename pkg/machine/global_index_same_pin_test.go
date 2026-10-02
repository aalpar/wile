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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// Two pins that will HEAL to different slots must not merge in the literal
// pool. I097.
//
// GlobalIndex.EqualTo is the store's PRESENT-denotation predicate and stays
// that: (Index, Env, Slot) is what the VM reads today, and two pins agreeing
// there do denote the same variable now. The literal pool needs a different
// question. `query` is what healReadLocked and healWriteLocked re-resolve with
// after a delete nils the slot (reachable from Scheme through
// namespace-undefine!), so two pins differing only in their heal query have the
// same present denotation and different FUTURE ones. Merging them keeps
// whichever was appended first and silently gives the other that one's heal.
//
// Red on master, and it needs no new method to be written first: the merge is
// observable through MaybeAppendLiteral alone.
func TestW2PinsDifferingOnlyInQueryDoNotMerge(t *testing.T) {
	ns := environment.NewNamespace().Runtime()
	symX := values.NewSymbol("x")
	_, created := ns.MaybeCreateOwnGlobalBinding(symX, environment.BindingTypeVariable, syntax.Scopes{})
	qt.Assert(t, created, qt.IsTrue)

	// The cheapest pair that differs ONLY in the heal query: these two scope
	// sets differ on IsAll() alone, both resolve to the same slot, and neither
	// needs a scope to be minted. ScopeSet.Scopes() returns the empty set for
	// the wildcard, so their scope MEMBERS are identical and a members-only
	// comparison would still merge them.
	giEmpty := ns.GetGlobalIndexWithScopes(symX, syntax.EmptyScopes())
	giAll := ns.GetGlobalIndexWithScopes(symX, syntax.AllScopes())
	qt.Assert(t, giEmpty, qt.IsNotNil)
	qt.Assert(t, giAll, qt.IsNotNil)

	// Precondition, and the reason EqualTo is left alone: today's denotation
	// predicate says these ARE the same variable, and it is right.
	qt.Assert(t, giEmpty.EqualTo(giAll), qt.IsTrue,
		qt.Commentf("present denotation must still agree; EqualTo is not what changes"))

	tpl := machine.NewNativeTemplate(0, 0, false)
	idxEmpty := tpl.MaybeAppendLiteral(giEmpty)
	idxAll := tpl.MaybeAppendLiteral(giAll)

	qt.Assert(t, idxAll, qt.Not(qt.Equals), idxEmpty,
		qt.Commentf("two pins with different heal queries merged into literal index %d", idxEmpty))
}

// The non-vacuity guard: the pool must still dedup. A pin appended twice is one
// literal, or the arm has stopped deduplicating rather than started
// discriminating.
func TestW2SamePinStillDedups(t *testing.T) {
	ns := environment.NewNamespace().Runtime()
	symX := values.NewSymbol("x")
	_, created := ns.MaybeCreateOwnGlobalBinding(symX, environment.BindingTypeVariable, syntax.Scopes{})
	qt.Assert(t, created, qt.IsTrue)

	tpl := machine.NewNativeTemplate(0, 0, false)

	// The same pin object twice.
	gi := ns.GetGlobalIndexWithScopes(symX, syntax.EmptyScopes())
	qt.Assert(t, tpl.MaybeAppendLiteral(gi), qt.Equals, tpl.MaybeAppendLiteral(gi))

	// And two DISTINCT pin objects that agree on every coordinate, which is the
	// case the compiler actually generates: one reference per occurrence of the
	// same identifier.
	again := ns.GetGlobalIndexWithScopes(symX, syntax.EmptyScopes())
	qt.Assert(t, again, qt.Not(qt.Equals), gi,
		qt.Commentf("precondition: distinct pin objects, so this is not pointer dedup"))
	qt.Assert(t, tpl.MaybeAppendLiteral(again), qt.Equals, tpl.MaybeAppendLiteral(gi),
		qt.Commentf("two occurrences of one identifier must share a literal"))
}

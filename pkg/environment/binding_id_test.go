// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//	http://www.apache.org/licenses/LICENSE-2.0
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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ResolveBindingID and GetLocalIndex must name the same binder for every query:
// identity decided by one must agree with the slot the other emits.
func TestResolveBindingIDAgreesWithGetLocalIndex(t *testing.T) {
	a := syntax.NewScope()
	b := syntax.NewScope()
	ab := syntax.Scopes{}.Add(a).Add(b)
	onlyA := syntax.Scopes{}.Add(a)
	sym := values.NewSymbol("x")
	type layer [][]syntax.Scopes // outer..inner frames, each a list of binder scopes
	cases := []struct {
		name   string
		frames layer
		q      syntax.ScopeSet
		up     int // expected frames above the innermost; -1 for a miss
		over   int // expected slot in that frame
	}{
		{"same-frame maximal first", layer{{ab, onlyA}}, syntax.ScopesOf(ab), 0, 0},
		{"same-frame maximal last", layer{{onlyA, ab}}, syntax.ScopesOf(ab), 0, 1},
		{"inner maximal, outer smaller", layer{{onlyA}, {ab}}, syntax.ScopesOf(ab), 0, 0},
		{"outer maximal, inner smaller", layer{{ab}, {onlyA}}, syntax.ScopesOf(ab), 1, 0},
		{"perfect match shadowed by identical outer", layer{{ab}, {ab}}, syntax.ScopesOf(ab), 0, 0},
		{"scoped miss", layer{{ab}}, syntax.ScopesOf(onlyA), -1, 0},
		{"wildcard first not maximal", layer{{onlyA, ab}}, syntax.AllScopes(), 0, 0},
		{"wildcard reaches outer", layer{{onlyA}, {}}, syntax.AllScopes(), 1, 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := NewNamespaceFrame()
			for _, binders := range tc.frames {
				env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
				for _, sc := range binders {
					env.MaybeCreateLocalBinding(sym, BindingTypeVariable, sc, nil)
				}
				if len(binders) == 0 {
					env.MaybeCreateLocalBinding(values.NewSymbol("pad"), BindingTypeVariable, syntax.Scopes{}, nil)
				}
			}
			li := env.GetLocalIndex(sym, tc.q)
			bid, ok := env.ResolveBindingID(sym, tc.q)
			if tc.up < 0 {
				qt.Assert(t, li, qt.IsNil)
			} else {
				qt.Assert(t, li, qt.IsNotNil)
				qt.Assert(t, li.Up(), qt.Equals, tc.up)
				qt.Assert(t, li.Over(), qt.Equals, tc.over)
			}
			qt.Assert(t, ok, qt.Equals, li != nil)
			if li == nil {
				qt.Assert(t, bid, qt.Equals, BindingID{})
				return
			}
			frame := env
			for range li.Up() {
				frame = frame.parent
			}
			qt.Assert(t, bid, qt.Equals, BindingID{Frame: frame.LocalEnvironment(), Slot: li.Over()})
		})
	}
}

// tiedFrame binds x under {a,b} and {a,c}: equal cardinality, incomparable.
func tiedFrame() (env *EnvironmentFrame, sym *values.Symbol, q syntax.ScopeSet) {
	a := syntax.NewScope()
	b := syntax.NewScope()
	c := syntax.NewScope()
	sym = values.NewSymbol("x")
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), NewNamespaceFrame())
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, syntax.Scopes{}.Add(a).Add(b), nil)
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, syntax.Scopes{}.Add(a).Add(c), nil)
	return env, sym, syntax.ScopesOf(syntax.Scopes{}.Add(a).Add(b).Add(c))
}

// The identity authority refuses a tie, as GetLocalIndex does.
func TestResolveBindingIDRaisesOnTie(t *testing.T) {
	env, sym, q := tiedFrame()

	r := capturePanic(func() {
		env.ResolveBindingID(sym, q)
	})

	qt.Assert(t, r, qt.IsNotNil)
	err, _ := r.(error)
	qt.Assert(t, errors.Is(err, werr.ErrAmbiguousBinding), qt.IsTrue)
}

// The reporting form returns the tie instead of raising, and ResolveBindingRef,
// whose callers are over-approximating analyses, never raises on it.
func TestTryResolveBindingIDReportsTie(t *testing.T) {
	env, sym, q := tiedFrame()

	_, ok, tied := env.TryResolveBindingID(sym, q)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, tied, qt.IsTrue)

	ref := env.ResolveBindingRef(sym, q)
	qt.Assert(t, ref.IsLocal(), qt.IsTrue)
}

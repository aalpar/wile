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
	"go/ast"
	"go/parser"
	"go/token"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestNilCreateIsNotWildcard pins that binder CREATION reads a nil scope set as
// the EMPTY set, never as "match any".
//
// Creation and lookup are opposite polarities and the distinction is the whole
// point. On the reference/query side a caller that means "any binding of this
// name" asks for it explicitly with values.AllScopes, because nil there once
// meant two contradictory things (pkg/values/scope_set.go). On the CREATION side
// "all" is meaningless: a binder's own scope set is its identity, so nil can only
// mean the empty set. MaybeCreateLocalBinding held the last surviving
// nil-as-wildcard read, and it was a fail-open one — a nil create silently reused
// whichever slot happened to come first, whatever scope set that slot carried.
//
// The channel was real but dead: an instrumented counter over the whole Go suite
// plus ./integration/... and ./test/... recorded zero reuses that happen ONLY
// because scopes == nil, i.e. zero cases where scopeSetsEqual would have refused.
// That is why the suite cannot supply this ratchet and this test exists.
func TestNilCreateIsNotWildcard(t *testing.T) {
	c := qt.New(t)

	scope := syntax.NewScope()
	env := NewLocalEnvironment(0)
	sym := values.NewSymbol("x")

	scopedIdx, scopedCreated := env.MaybeCreateLocalBinding(
		sym, BindingTypeVariable, syntax.ScopesFromSlice([]*syntax.Scope{scope}), nil,
	)
	t.Logf("scoped slot=%d created=%v", scopedIdx.Over(), scopedCreated)
	c.Assert(scopedCreated, qt.IsTrue, qt.Commentf("the first create must allocate a slot"))

	nilIdx, nilCreated := env.MaybeCreateLocalBinding(
		sym, BindingTypeVariable, syntax.Scopes{}, nil,
	)
	t.Logf("nil slot=%d created=%v", nilIdx.Over(), nilCreated)

	// The load-bearing assertion. Under nil-as-wildcard this reports
	// created=false and hands back slot 0 — the {scope} binder — so a
	// scope-less binder and a scoped one of the same name collapse onto one
	// slot. ∅ is not {scope}, so they are different variables.
	c.Assert(nilCreated, qt.IsTrue,
		qt.Commentf("a nil (empty) scope set must not reuse a slot whose scope set is {scope}"))
	c.Assert(nilIdx.Over(), qt.Not(qt.Equals), scopedIdx.Over(),
		qt.Commentf("the two binders must occupy distinct slots"))
}

// TestNilCreateReusesAnEmptyScopedSlot is the other half, and it is what keeps
// the fix from being a behaviour change rather than a bug fix. Deleting the
// wildcard leaves the loop reusing a slot only under scopeSetsEqual, which for a
// nil scopes argument against a meta-less slot is len 0 == len 0 plus two vacuous
// ScopesMatch calls — still true. Every reuse measured across the suite was of
// exactly this shape (x, testVar0, test-var, dup, all against a length-0 slot),
// so all four keep their answer.
func TestNilCreateReusesAnEmptyScopedSlot(t *testing.T) {
	c := qt.New(t)

	env := NewLocalEnvironment(0)
	sym := values.NewSymbol("x")

	firstIdx, firstCreated := env.MaybeCreateLocalBinding(sym, BindingTypeVariable, syntax.Scopes{}, nil)
	c.Assert(firstCreated, qt.IsTrue)

	secondIdx, secondCreated := env.MaybeCreateLocalBinding(sym, BindingTypeVariable, syntax.Scopes{}, nil)
	c.Assert(secondCreated, qt.IsFalse,
		qt.Commentf("two ∅-scoped creates of one name are the same variable"))
	c.Assert(secondIdx.Over(), qt.Equals, firstIdx.Over())
}

// TestLocalBindingCreateHasNoWildcard is the site-count half of the ratchet: the
// identifier `matchAny` must not appear in local_environment_frame.go at all.
//
// A behavioural test alone is not enough here. The wildcard could be reintroduced
// under another spelling while this one function's observable answer on the probe
// above stays the same, and the failure mode is a silently reused or split slot
// rather than a red suite — the shape CLAUDE.local.md's [Binding Identity] list
// names. Pinning the identifier out of the file is cheap and it names the rule.
func TestLocalBindingCreateHasNoWildcard(t *testing.T) {
	c := qt.New(t)

	const path = "local_environment_frame.go"

	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, parser.ParseComments)
	c.Assert(err, qt.IsNil)

	var found []string
	ast.Inspect(file, func(n ast.Node) bool {
		id, ok := n.(*ast.Ident)
		if !ok {
			return true
		}
		if id.Name == "matchAny" {
			found = append(found, fset.Position(id.Pos()).String())
		}
		return true
	})

	c.Assert(found, qt.HasLen, 0,
		qt.Commentf("binder creation must not carry a nil-as-wildcard read; sites: %v", found))
}

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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// capturePanic runs fn and returns the recovered panic value (nil if fn did not
// panic). Lets a test assert an ambiguous-binding raise without a raw crash.
func capturePanic(fn func()) (r any) {
	defer func() {
		r = recover()
	}()
	fn()
	return nil
}

// TestGetLocalIndex_AmbiguousIncomparableScopesRaises pins Flatt ambiguity
// detection. Two same-name local bindings carry {A,B} and {A,C} — equal
// cardinality, mutually incomparable (neither a subset of the other), both proper
// subsets of the reference {A,B,C}. No single binding is the maximal subset, so
// Racket raises "ambiguous binding". Wile must raise too, not silently resolve to
// the first-seen (innermost) candidate.
func TestGetLocalIndex_AmbiguousIncomparableScopesRaises(t *testing.T) {
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	topEnv := NewNamespaceFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")

	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeB}, nil)
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeC}, nil)

	ref := syntax.ScopesOf([]*syntax.Scope{scopeA, scopeB, scopeC})
	r := capturePanic(func() {
		env.GetLocalIndex(sym, ref)
	})

	qt.Assert(t, r, qt.IsNotNil, qt.Commentf(
		"an ambiguous reference must raise, not resolve silently to the innermost"))
	err, _ := r.(error)
	qt.Assert(t, errors.Is(err, werr.ErrAmbiguousBinding), qt.IsTrue,
		qt.Commentf("panic must carry ErrAmbiguousBinding, got %v", r))
}

// TestGetBinding_AmbiguousIncomparableScopesRaises is the GetBinding sibling of the
// above. GetBinding's local phase must raise on the ambiguity rather than fall
// through to the global phase (which would mask the ambiguity behind an unrelated
// global of the same name).
func TestGetBinding_AmbiguousIncomparableScopesRaises(t *testing.T) {
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	topEnv := NewNamespaceFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")

	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeB}, nil)
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeC}, nil)

	ref := syntax.ScopesOf([]*syntax.Scope{scopeA, scopeB, scopeC})
	r := capturePanic(func() {
		env.GetBinding(sym, ref)
	})

	qt.Assert(t, r, qt.IsNotNil, qt.Commentf(
		"an ambiguous reference must raise from GetBinding's local phase"))
	err, _ := r.(error)
	qt.Assert(t, errors.Is(err, werr.ErrAmbiguousBinding), qt.IsTrue,
		qt.Commentf("panic must carry ErrAmbiguousBinding, got %v", r))
}

// TestGetLocalIndex_ComparableScopesNotAmbiguous is the control that proves the
// detector fires on incomparability, not merely on multiple candidates. Bindings
// {A} and {A,B} are comparable ({A} ⊂ {A,B}); {A,B} is the unique maximal subset of
// the reference {A,B,C}, so resolution is unambiguous and must NOT raise.
func TestGetLocalIndex_ComparableScopesNotAmbiguous(t *testing.T) {
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	topEnv := NewNamespaceFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")

	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA}, nil)         // slot 0
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeB}, nil) // slot 1

	ref := syntax.ScopesOf([]*syntax.Scope{scopeA, scopeB, scopeC})
	var idx *LocalIndex
	r := capturePanic(func() {
		idx = env.GetLocalIndex(sym, ref)
	})

	qt.Assert(t, r, qt.IsNil, qt.Commentf("comparable scope sets are unambiguous; must not raise"))
	qt.Assert(t, idx, qt.IsNotNil)
	qt.Assert(t, idx[0], qt.Equals, 1, qt.Commentf(
		"the larger comparable subset {A,B} (slot 1) is the maximal resolution"))
}

// TestGetLocalIndex_PerfectMatchNotAmbiguous proves the perfect-match short-circuit
// wins before ambiguity can arise: a binding whose scope set equals the reference is
// selected immediately, even when a smaller incomparable binding is also present.
func TestGetLocalIndex_PerfectMatchNotAmbiguous(t *testing.T) {
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	topEnv := NewNamespaceFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")

	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeC}, nil)         // slot 0
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeB, scopeC}, nil) // slot 1 (perfect)

	ref := syntax.ScopesOf([]*syntax.Scope{scopeA, scopeB, scopeC})
	var idx *LocalIndex
	r := capturePanic(func() {
		idx = env.GetLocalIndex(sym, ref)
	})

	qt.Assert(t, r, qt.IsNil, qt.Commentf("a perfect match resolves before any ambiguity"))
	qt.Assert(t, idx, qt.IsNotNil)
	qt.Assert(t, idx[0], qt.Equals, 1, qt.Commentf("the exact-match binding {A,B,C} (slot 1) wins"))
}

// ExactBinding is GetBinding's lexical phase with the tie returned instead of
// raised, followed by the store's exact-phase tiers at the frame's own phase. The
// three tests below are its three answers.
func TestExactBinding_LocalTieIsReportedNotRaised(t *testing.T) {
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	topEnv := NewNamespaceFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeB}, nil)
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeC}, nil)

	ref := syntax.ScopesOf([]*syntax.Scope{scopeA, scopeB, scopeC})
	var bnd *Binding
	var ambiguous bool
	r := capturePanic(func() {
		bnd, ambiguous = env.ExactBinding(sym, ref)
	})
	qt.Assert(t, r, qt.IsNil, qt.Commentf("ExactBinding must not raise: %v", r))
	qt.Assert(t, ambiguous, qt.IsTrue)
	qt.Assert(t, bnd, qt.IsNil)
}

func TestExactBinding_LocalWinsOverTheStore(t *testing.T) {
	ns := NewNamespace()
	sym := values.NewSymbol("x")
	_, err := ns.Runtime().DefineOwnGlobal(sym, BindingTypeVariable, nil, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), ns.Runtime())
	_, ok := env.EnsureLocalBinding(sym, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)

	bnd, ambiguous := env.ExactBinding(sym, values.EmptyScopes())
	qt.Assert(t, ambiguous, qt.IsFalse)
	qt.Assert(t, bnd, qt.IsNotNil)
	// The same answer the raising form gives: the local, not the global.
	qt.Assert(t, bnd, qt.Equals, env.GetBinding(sym, values.EmptyScopes()))
	qt.Assert(t, bnd, qt.Not(qt.Equals), ns.Runtime().GetBinding(sym, values.EmptyScopes()))
}

func TestExactBinding_ReachesTheStoreButNotTheAmbientTier(t *testing.T) {
	ns := NewNamespace()
	sym := values.NewSymbol("else")
	ambientIdx, created := ns.Runtime().SealedWriteViewAt(PhaseRuntime).
		MaybeCreateOwnGlobalBinding(sym, BindingTypePrimitive, nil)
	qt.Assert(t, created, qt.IsTrue)
	ambient := ns.Store().GetOwnGlobalBinding(ambientIdx)

	// GetBinding reaches the ambient keyword at T3; ExactBinding does not.
	qt.Assert(t, ns.Runtime().GetBinding(sym, values.EmptyScopes()), qt.Equals, ambient)
	bnd, ambiguous := ns.Runtime().ExactBinding(sym, values.EmptyScopes())
	qt.Assert(t, ambiguous, qt.IsFalse)
	qt.Assert(t, bnd, qt.IsNil)

	// A phase-0 mutable slot is exact, and wins.
	idx, err := ns.Runtime().DefineOwnGlobal(sym, BindingTypeVariable, nil, values.NewInteger(5))
	qt.Assert(t, err, qt.IsNil)
	bnd, _ = ns.Runtime().ExactBinding(sym, values.EmptyScopes())
	qt.Assert(t, bnd, qt.Equals, ns.Store().GetOwnGlobalBinding(idx))
}

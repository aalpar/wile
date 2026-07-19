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
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// Helper to create a GlobalEnvironmentFrame with proper Namespace
func newTestGlobalEnvFrame() *GlobalEnvironmentFrame {
	return NewNamespaceFrame().GlobalEnvironment()
}

func TestGlobalEnvironment(t *testing.T) {
	// Create a new environment via NamespaceFrame
	env := newTestGlobalEnvFrame()

	// Check if the environment is initialized correctly
	if env == nil {
		t.Fatal("Expected a non-nil environment")
	}

	value0 := values.NewInteger(42)
	value1 := values.NewInteger(43)

	sym0 := values.NewSymbol("testVar0")
	sym1 := values.NewSymbol("testVar1")
	// variable has not been added yet, so GetGlobalIndex should return nil
	gi0 := env.GetGlobalIndex(sym0)
	qt.Assert(t, gi0, qt.IsNil)

	gi1 := env.GetGlobalIndex(sym1)
	qt.Assert(t, gi1, qt.IsNil)

	// Test adding a binding
	gi0, ok := env.CreateGlobalBinding(sym0, BindingTypeVariable, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi0.Index.EqualTo(values.NewSymbol("testVar0")), qt.IsTrue)

	// Set the initial value of the new binding
	err := env.SetOwnGlobalValue(gi0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Adding a new binding should create a new index
	gi1, ok = env.CreateGlobalBinding(sym1, BindingTypeVariable, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi1.Index.EqualTo(values.NewSymbol("testVar1")), qt.IsTrue)

	// Set the initial value of the new binding
	err = env.SetOwnGlobalValue(gi1, value1)
	qt.Assert(t, err, qt.IsNil)

	gb2 := env.GetOwnGlobalBinding(gi0).Value()
	qt.Assert(t, gb2, valuestest.SchemeEquals, value0)

	gb3 := env.GetOwnGlobalBinding(gi1).Value()
	qt.Assert(t, gb3, valuestest.SchemeEquals, value1)
}

func TestGlobalEnvironmentFrame_Keys(t *testing.T) {
	env := newTestGlobalEnvFrame()

	sym1 := values.NewSymbol("var1")
	sym2 := values.NewSymbol("var2")

	env.CreateGlobalBinding(sym1, BindingTypeVariable, nil)
	env.CreateGlobalBinding(sym2, BindingTypeVariable, nil)

	keys := env.Keys()
	qt.Assert(t, keys, qt.HasLen, 2)
}

func TestGlobalEnvironmentFrame_Copy(t *testing.T) {
	env := newTestGlobalEnvFrame()

	sym := values.NewSymbol("test")
	env.CreateGlobalBinding(sym, BindingTypeVariable, nil)

	copied := env.Copy()
	qt.Assert(t, copied, qt.Not(qt.IsNil))

	// Verify bindings were copied
	qt.Assert(t, len(copied.Bindings()), qt.Equals, len(env.Bindings()))
}

func TestGlobalEnvironmentFrame_GetGlobalIndex_NotFound(t *testing.T) {
	env := newTestGlobalEnvFrame()

	// Get index for symbol that doesn't exist
	sym := values.NewSymbol("nonexistent")
	gi := env.GetGlobalIndex(sym)
	qt.Assert(t, gi, qt.IsNil)
}

func TestGlobalEnvironmentFrame_DeleteBinding(t *testing.T) {
	c := qt.New(t)

	ns := NewNamespace()
	env := ns.Runtime()

	sym := values.NewSymbol("x")
	_, created := env.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable, nil)
	c.Assert(created, qt.IsTrue)

	// Verify binding exists
	b := env.GetBinding(sym, nil)
	c.Assert(b, qt.IsNotNil)

	// Delete it
	deleted := env.GlobalEnvironment().DeleteBinding(sym)
	c.Assert(deleted, qt.IsTrue)

	// Verify binding is gone via key lookup
	gi := env.GlobalEnvironment().GetGlobalIndex(sym)
	c.Assert(gi, qt.IsNil)

	// Deleting non-existent binding returns false
	deleted = env.GlobalEnvironment().DeleteBinding(values.NewSymbol("nonexistent"))
	c.Assert(deleted, qt.IsFalse)
}

func TestGlobalEnvironmentFrame_SymbolEquality(t *testing.T) {
	// Symbols with the same key are structurally equal
	sym1 := values.NewSymbol("foo")
	sym2 := values.NewSymbol("foo")

	qt.Assert(t, sym1.EqualTo(sym2), qt.IsTrue)
	qt.Assert(t, sym1.Key, qt.Equals, sym2.Key)
}

// TestGlobalIndex_EqualTo_DistinguishesEnv asserts that two GlobalIndex values
// naming the same symbol but resolving in different global frames are NOT equal.
//
// Env is not decoration: the VM branches on gi.Env != nil to pick the resolving
// frame (machine_context.go:519,1190). Two GlobalIndex that differ only in Env
// therefore denote different bindings, and any equality that ignores Env lets a
// library-pinned macro reference and a user top-level store be treated as one.
func TestGlobalIndex_EqualTo_DistinguishesEnv(t *testing.T) {
	sym := values.NewSymbol("helper")
	libraryFrame := newTestGlobalEnvFrame()

	userStore := NewGlobalIndex(sym)
	libraryLoad := &GlobalIndex{Index: sym, Env: libraryFrame}

	qt.Assert(t, userStore.Env, qt.IsNil)
	qt.Assert(t, libraryLoad.Env, qt.Not(qt.IsNil))

	qt.Assert(t, libraryLoad.EqualTo(userStore), qt.IsFalse,
		qt.Commentf("library-pinned load must not equal an Env==nil store of the same symbol"))
	qt.Assert(t, userStore.EqualTo(libraryLoad), qt.IsFalse,
		qt.Commentf("equality must be symmetric"))
}

// TestGlobalIndex_EqualTo_SameEnv_IsEqual pins the other direction, so a fix that
// simply always returns false cannot pass.
func TestGlobalIndex_EqualTo_SameEnv_IsEqual(t *testing.T) {
	sym := values.NewSymbol("helper")
	frame := newTestGlobalEnvFrame()

	a := &GlobalIndex{Index: sym, Env: frame}
	b := &GlobalIndex{Index: values.NewSymbol("helper"), Env: frame}

	qt.Assert(t, a.EqualTo(b), qt.IsTrue,
		qt.Commentf("same symbol key, same resolving frame: one binding"))
	qt.Assert(t, NewGlobalIndex(sym).EqualTo(NewGlobalIndex(values.NewSymbol("helper"))), qt.IsTrue,
		qt.Commentf("same symbol key, both unpinned: one binding"))
}

// TestGlobalFrame_VacuousScopesAreSingleSlot is the Stage A proof obligation for
// the scope-keyed global frame: while every creation site passes an empty scope
// set, the multi-slot store must be indistinguishable from the single-slot store
// it replaced. One slot per name, and resolution equal to first-hit.
//
// This is asserted directly rather than inferred from a green suite, because the
// whole staging argument rests on it.
func TestGlobalFrame_VacuousScopesAreSingleSlot(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()

	x := values.NewSymbol("x")
	_, created := ge.CreateGlobalBinding(x, BindingTypeVariable, nil)
	c.Assert(created, qt.IsTrue)

	// Redefinition of the same variable reuses the slot — R7RS §5.3.1.
	_, created = ge.CreateGlobalBinding(x, BindingTypeVariable, nil)
	c.Assert(created, qt.IsFalse)

	c.Assert(len(ge.Keys()[*x]), qt.Equals, 1)
	c.Assert(ge.GetGlobalIndex(x), qt.IsNotNil)
	c.Assert(ge.GetGlobalIndexWithScopes(x, nil), qt.IsNotNil)
}

// TestGlobalFrame_ScopeSetsSeparateBindings pins the behavior Stage B turns on:
// a macro-introduced binder and a user-written one of the same name are distinct
// variables, and neither is reachable from the other's scope set.
func TestGlobalFrame_ScopeSetsSeparateBindings(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()

	x := values.NewSymbol("x")
	m := syntax.NewScope()
	n := syntax.NewScope()

	// user-written binder: empty scope set
	_, created := ge.CreateGlobalBinding(x, BindingTypeVariable, nil)
	c.Assert(created, qt.IsTrue)

	// macro-introduced binder: scope set {m}. Creation compares scope sets by
	// EXACT equality, so this must NOT reuse the user's slot — compatibility
	// would have, since an empty binding scope set matches anything.
	_, created = ge.CreateGlobalBinding(x, BindingTypeVariable, []*syntax.Scope{m})
	c.Assert(created, qt.IsTrue)
	c.Assert(len(ge.Keys()[*x]), qt.Equals, 2)

	userSlot := ge.Keys()[*x][0]
	macroSlot := ge.Keys()[*x][1]

	// A reference written outside any expansion sees only the user's binding:
	// {m} is not a subset of {}. This is the leak, closed.
	gi := ge.GetGlobalIndexWithScopes(x, nil)
	c.Assert(gi, qt.IsNotNil)
	c.Assert(gi.Slot, qt.Equals, userSlot)

	// A reference carrying {m} resolves maximally to the macro's binding.
	gi = ge.GetGlobalIndexWithScopes(x, []*syntax.Scope{m})
	c.Assert(gi, qt.IsNotNil)
	c.Assert(gi.Slot, qt.Equals, macroSlot)

	// A reference from a DIFFERENT expansion {n} cannot see {m}: this is the
	// collision, closed.
	gi = ge.GetGlobalIndexWithScopes(x, []*syntax.Scope{n})
	c.Assert(gi, qt.IsNotNil)
	c.Assert(gi.Slot, qt.Equals, userSlot)

	// Wildcard still reaches the name for introspection callers.
	c.Assert(ge.GetGlobalIndex(x), qt.IsNotNil)
}

// TestGlobalIndex_EqualToDiscriminatesSlot guards the literal-pool identity:
// once one symbol can name several bindings in a frame, (Index, Env) no longer
// identifies a variable.
func TestGlobalIndex_EqualToDiscriminatesSlot(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	x := values.NewSymbol("x")
	m := syntax.NewScope()

	ge.CreateGlobalBinding(x, BindingTypeVariable, nil)
	ge.CreateGlobalBinding(x, BindingTypeVariable, []*syntax.Scope{m})

	user := ge.GetGlobalIndexWithScopes(x, nil)
	macro := ge.GetGlobalIndexWithScopes(x, []*syntax.Scope{m})

	c.Assert(user.EqualTo(macro), qt.IsFalse)
	c.Assert(user.EqualTo(ge.GetGlobalIndexWithScopes(x, nil)), qt.IsTrue)
}

// TestGlobalFrame_PinnedIndexSurvivesDelete pins the F1/F2 regressions found by
// the Stage A crosscheck. Slot-addressing replaced name-resolution on these
// paths, and a pinned index must not outlive the binding it names:
//
//   - SetOwnGlobalValue must report ErrNoSuchBinding, not dereference a nil slot
//   - GetOwnGlobalBinding must miss rather than return the emptied slot
//   - after a redefine, a stale pinned index must find the NEW binding, which is
//     the self-healing the name lookup used to provide for free
func TestGlobalFrame_PinnedIndexSurvivesDelete(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("x")

	ge.CreateGlobalBinding(sym, BindingTypeVariable, nil)
	gi := ge.GetGlobalIndexWithScopes(sym, nil)
	c.Assert(gi, qt.IsNotNil)
	c.Assert(gi.Env, qt.Equals, ge)

	c.Assert(ge.DeleteBinding(sym), qt.IsTrue)

	// No panic, and the same error master produced.
	err := ge.SetOwnGlobalValue(gi, values.NewInteger(5))
	c.Assert(err, qt.IsNotNil)
	c.Assert(ge.GetOwnGlobalBinding(gi), qt.IsNil)

	// Redefine: the stale pinned index must re-resolve onto the new binding.
	ge.CreateGlobalBinding(sym, BindingTypeVariable, nil)
	c.Assert(ge.GetOwnGlobalBinding(gi), qt.IsNotNil)
	c.Assert(ge.SetOwnGlobalValue(gi, values.NewInteger(7)), qt.IsNil)
	c.Assert(ge.GetOwnGlobalBinding(gi).Value(), valuestest.SchemeEquals, values.NewInteger(7))
}

// TestGlobalFrame_WildcardSkipsDeletedSlot covers the matchAny branch, which had
// no nil-slot guard while the scoped branch did (crosscheck N1).
func TestGlobalFrame_WildcardSkipsDeletedSlot(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("x")

	ge.CreateGlobalBinding(sym, BindingTypeVariable, nil)
	c.Assert(ge.DeleteBinding(sym), qt.IsTrue)
	c.Assert(ge.GetGlobalIndex(sym), qt.IsNil)
}

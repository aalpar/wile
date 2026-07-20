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
	deleted := env.GlobalEnvironment().DeleteBinding(sym, AmbientScopes())
	c.Assert(deleted, qt.IsTrue)

	// Verify binding is gone via key lookup
	gi := env.GlobalEnvironment().GetGlobalIndex(sym)
	c.Assert(gi, qt.IsNil)

	// Deleting non-existent binding returns false
	deleted = env.GlobalEnvironment().DeleteBinding(values.NewSymbol("nonexistent"), AmbientScopes())
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
// it replaced. One slot per name, and the wildcard and scoped entry points must
// agree on which slot that is.
//
// This is asserted directly rather than inferred from a green suite, because the
// whole staging argument rests on it.
//
// Scope note: the "resolution equals first-hit ACROSS THE PARENT CHAIN" half of
// the invariant cannot be asserted here. A GlobalEnvironmentFrame has no parent
// — the hierarchy is managed by EnvironmentFrame (see the type's doc comment) —
// so the cross-frame ordering is covered at that layer, not this one. This test
// pins the within-frame half. The doc comment formerly claimed the chain half
// too, which it never asserted.
func TestGlobalFrame_VacuousScopesAreSingleSlot(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()

	// Several distinct names, so a bug that collapses or crosses keys is visible.
	// A single-name test cannot distinguish "one slot per name" from "one slot".
	names := []string{"x", "y", "z"}
	for _, n := range names {
		sym := values.NewSymbol(n)
		_, created := ge.CreateGlobalBinding(sym, BindingTypeVariable, nil)
		c.Assert(created, qt.IsTrue, qt.Commentf("first create of %s", n))

		// Redefinition of the same variable reuses the slot — R7RS §5.3.1.
		_, created = ge.CreateGlobalBinding(sym, BindingTypeVariable, nil)
		c.Assert(created, qt.IsFalse, qt.Commentf("redefine of %s must reuse", n))
	}

	for _, n := range names {
		sym := values.NewSymbol(n)
		c.Assert(len(ge.Keys()[*sym]), qt.Equals, 1, qt.Commentf("slots for %s", n))

		// The two entry points return different KINDS of index by design, and the
		// difference is load-bearing rather than incidental: the wildcard one is
		// DEFERRED (Env nil, resolved against whatever environment is live when
		// the instruction executes), the scoped one is PINNED to this frame and
		// slot. EqualTo deliberately reports them unequal — see its doc comment.
		// Asserting only IsNotNil would not distinguish these at all.
		wildcard := ge.GetGlobalIndex(sym)
		scoped := ge.GetGlobalIndexWithScopes(sym, nil)
		c.Assert(wildcard, qt.IsNotNil)
		c.Assert(scoped, qt.IsNotNil)

		c.Assert(wildcard.Env, qt.IsNil,
			qt.Commentf("wildcard lookup must stay deferred for %s", n))
		c.Assert(scoped.Env, qt.Equals, ge,
			qt.Commentf("scoped lookup must pin to this frame for %s", n))
		c.Assert(scoped.Slot, qt.Equals, ge.Keys()[*sym][0],
			qt.Commentf("scoped lookup must pin the name's only slot for %s", n))
		c.Assert(wildcard.EqualTo(scoped), qt.IsFalse,
			qt.Commentf("deferred and pinned are different operations for %s", n))
	}
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

	c.Assert(ge.DeleteBinding(sym, AmbientScopes()), qt.IsTrue)

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
	c.Assert(ge.DeleteBinding(sym, AmbientScopes()), qt.IsTrue)
	c.Assert(ge.GetGlobalIndex(sym), qt.IsNil)
}

// Clearing a name that owns several hygiene-distinct slots takes one delete per
// scope set: each resolves its own binding, and the name leaves the frame only
// when the last slot goes.
//
// INVERTED for issue #805. This test previously pinned the opposite contract —
// one delete removing EVERY slot the name owned — which was coherent only while
// delete was name-level and the read surface was not. Under it,
// `(namespace-undefine! ns 'x)` destroyed a macro-introduced `x` that
// `(namespace-ref ns 'x)` reported as unbound. What is worth keeping from the
// original is its multi-slot coverage: every other delete test builds exactly
// one slot, so nothing else exercises a name with more than one.
//
// Multi-slot names are ordinary since scope-keyed global storage — a
// macro-introduced binder carries the expansion's intro scope and a user-written
// one the empty set.
func TestGlobalFrame_DeleteClearsMultiSlotNameOneScopeSetAtATime(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("counter")
	introScopes := []*syntax.Scope{syntax.NewScope()}

	// A user-written binder (empty set) and a macro-introduced one (intro scope)
	// are distinct bindings sharing a name.
	ge.CreateGlobalBinding(sym, BindingTypeVariable, nil)
	ge.CreateGlobalBinding(sym, BindingTypeVariable, introScopes)

	ambient := ge.GetGlobalIndexWithScopes(sym, AmbientScopes())
	introduced := ge.GetGlobalIndexWithScopes(sym, introScopes)
	c.Assert(ambient, qt.IsNotNil)
	c.Assert(introduced, qt.IsNotNil)
	c.Assert(ambient.Slot, qt.Not(qt.Equals), introduced.Slot)

	// The ambient delete takes its own slot and leaves the name in the frame.
	c.Assert(ge.DeleteBinding(sym, AmbientScopes()), qt.IsTrue)
	c.Assert(ge.GetOwnGlobalBinding(ambient), qt.IsNil)
	c.Assert(ge.GetGlobalIndexWithScopes(sym, AmbientScopes()), qt.IsNil)
	c.Assert(ge.GetGlobalIndex(sym), qt.IsNotNil)

	// Deleting under the intro scope set takes the last slot, and only now does
	// the name stop being reported at all.
	c.Assert(ge.DeleteBinding(sym, introScopes), qt.IsTrue)
	c.Assert(ge.GetOwnGlobalBinding(introduced), qt.IsNil)
	c.Assert(ge.GetGlobalIndexWithScopes(sym, introScopes), qt.IsNil)
	c.Assert(ge.GetGlobalIndex(sym), qt.IsNil)
}

// Delete resolves through the same call the read makes, so it removes exactly
// the binding namespace-ref would have returned and leaves every other slot the
// name owns alone. Issue #805: while delete was name-level and the read surface
// scope-exact, `(namespace-undefine! ns 'x)` destroyed a macro-introduced `x`
// that `(namespace-ref ns 'x)` reported as unbound — you could destroy a binding
// you could not read.
func TestGlobalFrame_DeleteRemovesOnlyTheScopeMatchedSlot(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("counter")
	introScopes := []*syntax.Scope{syntax.NewScope()}

	ge.CreateGlobalBinding(sym, BindingTypeVariable, nil)
	ge.CreateGlobalBinding(sym, BindingTypeVariable, introScopes)

	introduced := ge.GetGlobalIndexWithScopes(sym, introScopes)
	c.Assert(introduced, qt.IsNotNil)

	// Delete under the ambient (empty) scope set, which is what the namespace
	// read surface resolves under.
	c.Assert(ge.DeleteBinding(sym, AmbientScopes()), qt.IsTrue)

	// The ambient binding is gone...
	c.Assert(ge.GetGlobalIndexWithScopes(sym, nil), qt.IsNil)
	// ...and the hygiene-distinct one is untouched, still readable under its
	// own scope set.
	c.Assert(ge.GetGlobalIndexWithScopes(sym, introScopes), qt.IsNotNil)
	c.Assert(ge.GetOwnGlobalBinding(introduced), qt.IsNotNil)
	// The name still exists in the frame, so Keys must keep it. Dropping the
	// map entry here would strand the consumers that treat slots[0] as the
	// name's representative.
	c.Assert(ge.GetGlobalIndex(sym), qt.IsNotNil)
}

// Deleting a name that only a macro-introduced binder owns is a no-op under the
// ambient scope set: there is nothing the read surface can see, so there is
// nothing to remove. This is the behavior change issue #805 turns on.
func TestGlobalFrame_DeleteOfMacroOnlyNameUnderAmbientScopesIsNoOp(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("counter")
	introScopes := []*syntax.Scope{syntax.NewScope()}

	ge.CreateGlobalBinding(sym, BindingTypeVariable, introScopes)
	introduced := ge.GetGlobalIndexWithScopes(sym, introScopes)
	c.Assert(introduced, qt.IsNotNil)

	c.Assert(ge.DeleteBinding(sym, AmbientScopes()), qt.IsFalse)
	c.Assert(ge.GetOwnGlobalBinding(introduced), qt.IsNotNil)
}

// A stale resolved index whose slot was deleted must not re-resolve onto a
// HYGIENE-DISTINCT binding that later takes the name.
//
// TestGlobalFrame_PinnedIndexSurvivesDelete above pins
// stale-index re-resolution as intended, but it redefines under the SAME scope set,
// which is the case where re-resolution is harmless. It is not harmless across
// scope sets: newResolvedGlobalIndex never sets Scopes, so every compiler-resolved
// index carries Scopes == nil, and both SetOwnGlobalValue and GetOwnGlobalBinding
// fall through pinnedSlotLocked to bestSlotLocked(key, gi.Scopes, gi.Scopes == nil)
// — matchAny TRUE, i.e. the name's first live slot, with no scope check at all.
//
// Reachable from Scheme, measured on HEAD via namespace-undefine!: after macro A's
// binding of a name is deleted and macro B introduces its own, A's setter writes
// B's binding. Its control (identical minus the undefine) keeps them isolated, so
// the delete is the necessary ingredient.
//
// Distinct from the C4/#805 axis, which asked delete-all vs scoped-delete and
// settled on scoped. This is the D4 pinned-index axis: whether a pinned index
// whose slot died fails closed rather than re-resolving onto whatever later
// takes the name. The two interact only in that scoped delete is what lets this
// test name A's binding directly.
func TestGlobalFrame_StaleIndexMustNotCrossScopeSets(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("counter")
	aScopes := []*syntax.Scope{syntax.NewScope()}
	bScopes := []*syntax.Scope{syntax.NewScope()}

	ge.CreateGlobalBinding(sym, BindingTypeVariable, aScopes)
	aIndex := ge.GetGlobalIndexWithScopes(sym, aScopes)
	c.Assert(aIndex, qt.IsNotNil)

	// Delete under A's own scope set: the name has no ambient binding, so an
	// ambient delete would correctly be a no-op (#805) and leave A's slot alive.
	c.Assert(ge.DeleteBinding(sym, aScopes), qt.IsTrue)

	// A different binder, whose scope set is incompatible with A's, takes the name.
	ge.CreateGlobalBinding(sym, BindingTypeVariable, bScopes)
	bBinding := ge.GetOwnGlobalBinding(ge.GetGlobalIndexWithScopes(sym, bScopes))
	c.Assert(bBinding, qt.IsNotNil)

	// A's dead index must not address B's binding.
	c.Assert(ge.GetOwnGlobalBinding(aIndex), qt.IsNil)
	err := ge.SetOwnGlobalValue(aIndex, values.NewInteger(42))
	c.Assert(err, qt.IsNotNil)
	c.Assert(bBinding.Value(), qt.Not(valuestest.SchemeEquals), values.NewInteger(42))
}

// TestGlobalFrame_AmbientKeysExcludesMacroIntroducedBinders pins what separates
// AmbientKeys from Keys. Keys answers "what names does this frame hold"; the
// bound-names primitives need "what names can a reference reach", and a binder a
// macro template introduced is reachable from neither source nor any scoped read.
// The mixed name is the discriminating case: dropping the name outright would be
// as wrong as listing it, since the user's own binding of it is reachable.
func TestGlobalFrame_AmbientKeysExcludesMacroIntroducedBinders(t *testing.T) {
	c := qt.New(t)
	ge := NewGlobalEnvironmentFrame()
	m := []*syntax.Scope{syntax.NewScope()}

	ambient := values.NewSymbol("ambient")
	macroOnly := values.NewSymbol("macro-only")
	mixed := values.NewSymbol("mixed")
	deleted := values.NewSymbol("deleted")

	ge.CreateGlobalBinding(ambient, BindingTypeVariable, nil)
	ge.CreateGlobalBinding(macroOnly, BindingTypeVariable, m)
	ge.CreateGlobalBinding(mixed, BindingTypeVariable, nil)
	ge.CreateGlobalBinding(mixed, BindingTypeVariable, m)
	ge.CreateGlobalBinding(deleted, BindingTypeVariable, nil)
	c.Assert(ge.DeleteBinding(deleted, AmbientScopes()), qt.IsTrue)

	names := values.StringSet{}
	for _, k := range ge.AmbientKeys() {
		_, dup := names[k.Key]
		c.Assert(dup, qt.IsFalse)
		names[k.Key] = struct{}{}
	}

	// The delta between the two accessors is macro-only: DeleteBinding drops the
	// key itself, so a deleted name is already absent from Keys.
	c.Assert(ge.Keys(), qt.HasLen, 3)
	c.Assert(names, qt.HasLen, 2)

	_, ok := names["ambient"]
	c.Assert(ok, qt.IsTrue)
	_, ok = names["mixed"]
	c.Assert(ok, qt.IsTrue)
	_, ok = names["macro-only"]
	c.Assert(ok, qt.IsFalse)
	_, ok = names["deleted"]
	c.Assert(ok, qt.IsFalse)
}

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

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNewEnvironmentFrame(t *testing.T) {
	q := NewTopLevelEnvironmentFrame()
	qt.Assert(t, q, qt.Not(qt.IsNil))
	qt.Assert(t, q.GlobalEnvironment(), qt.IsNotNil)
	qt.Assert(t, q.LocalEnvironment(), qt.IsNil)
}

func TestNewTopLevelEnvironmentFrame(t *testing.T) {
	q := NewTopLevelEnvironmentFrame()
	qt.Assert(t, q, qt.Not(qt.IsNil))
	qt.Assert(t, q.GlobalEnvironment(), qt.IsNotNil)
	qt.Assert(t, q.LocalEnvironment(), qt.IsNil)
}

func TestEnvironmentFrame_Locals(t *testing.T) {
	// Create a new Local environment
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	// Check if the environment is initialized correctly
	if env == nil {
		t.Fatal("Expected a non-nil Local environment")
	}

	value0 := values.NewInteger(42)
	value1 := values.NewInteger(43)

	tv0 := values.NewSymbol("testVar0")
	// variable has not been added yet, so GetLocalIndex should return nil
	li0 := env.GetLocalIndex(tv0)
	qt.Assert(t, li0, qt.IsNil)

	// Test adding a binding
	li0, ok := env.MaybeCreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err := env.SetLocalValue(li0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Re-adding the same binding should not change the index
	li0, ok = env.MaybeCreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Adding a new binding should create a new index
	tv1 := values.NewSymbol("testVar1")
	li1, ok := env.MaybeCreateLocalBinding(tv1, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li1[0], qt.Equals, 1)
	qt.Assert(t, li1[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err = env.SetLocalValue(li1, value1)
	qt.Assert(t, err, qt.IsNil)

	lb := env.GetLocalBinding(li0)
	qt.Assert(t, lb.value, valuestest.SchemeEquals, value0)
	lb = env.GetLocalBinding(li1)
	qt.Assert(t, lb.value, valuestest.SchemeEquals, value1)
}

func TestEnvironmentFrame_Globals(t *testing.T) {
	// Create a new Local environment
	env := NewTopLevelEnvironmentFrame()

	// Check if the environment is initialized correctly
	if env == nil {
		t.Fatal("Expected a non-nil Local environment")
	}

	value0 := values.NewInteger(42)
	value1 := values.NewInteger(43)

	// variable has not been added yet, so GetLocalIndex should return nil
	tv0 := env.InternSymbol(values.NewSymbol("testVar0"))
	gi0 := env.GetGlobalIndex(tv0)
	qt.Assert(t, gi0, qt.IsNil)

	// Test adding a binding
	gi0, ok := env.MaybeCreateOwnGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi0, valuestest.SchemeEquals, NewGlobalIndex(tv0))

	// Set the initial value of the new binding
	err := env.SetOwnGlobalValue(gi0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Re-adding the same binding should not change the index
	tv0 = env.InternSymbol(values.NewSymbol("testVar0"))
	gi0, ok = env.MaybeCreateOwnGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, gi0.Index, valuestest.SchemeEquals, tv0)

	// Adding a new binding should create a new index
	tv1 := values.NewSymbol("testVar1")
	gi1, ok := env.MaybeCreateOwnGlobalBinding(tv1, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi1.Index, valuestest.SchemeEquals, tv1)

	// Set the initial value of the new binding
	err = env.SetOwnGlobalValue(gi1, value1)
	qt.Assert(t, err, qt.IsNil)

	bd := env.GetGlobalBinding(gi0)
	qt.Assert(t, bd.value, valuestest.SchemeEquals, value0)
	bd = env.GetGlobalBinding(gi1)
	qt.Assert(t, bd.value, valuestest.SchemeEquals, value1)
}

func TestEnvironmentFrame_Bindings(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	// check global environment
	tv0 := env.InternSymbol(values.NewSymbol("testVar0"))
	qt.Assert(t, env, qt.Not(qt.IsNil))
	_, ok := env.MaybeCreateOwnGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	_, ok = env.MaybeCreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)

	tv0 = env.InternSymbol(values.NewSymbol("testVar0"))
	gi := env.GetGlobalIndex(tv0)
	qt.Assert(t, ok, qt.IsTrue)
	gb := env.GetGlobalBinding(gi)
	qt.Assert(t, gb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, gb.value, valuestest.SchemeEquals, values.Void)

	// check local environment
	li0 := env.GetLocalIndex(tv0)
	qt.Assert(t, li0, qt.IsNotNil)
	lb := env.GetLocalBinding(li0)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, valuestest.SchemeEquals, values.Void)

	err := env.SetLocalValue(li0, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)

	lb = env.GetLocalBinding(li0)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, valuestest.SchemeEquals, values.NewInteger(42))

	err = env.SetOwnGlobalValue(gi, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)

	gb = env.GetGlobalBinding(gi)
	qt.Assert(t, gb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, gb.value, valuestest.SchemeEquals, values.NewInteger(42))

	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
	li1, ok := env.EnsureLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li1[0], qt.Equals, 0)
	qt.Assert(t, li1[1], qt.Equals, 0)

	qt.Assert(t, env.local.bindings, qt.HasLen, 1)
	qt.Assert(t, env.parent.local.bindings, qt.HasLen, 1)

	err = env.SetLocalValue(li1, values.NewInteger(43))
	qt.Assert(t, err, qt.IsNil)
	lb = env.GetLocalBinding(li1)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, valuestest.SchemeEquals, values.NewInteger(43))

	lb = env.parent.GetLocalBinding(li0)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestEnvironmentFrame_Hierarchy(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()

	tv0 := values.NewSymbol("testVar0")
	gi, ok := env.MaybeCreateOwnGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)

	_, ok = env.EnsureLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)

	gb := env.GetGlobalBinding(gi)
	qt.Assert(t, gb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, gb.value, valuestest.SchemeEquals, values.Void)

	lenv := NewLocalEnvironment(0)
	env = NewEnvironmentFrameWithParent(lenv, env)

	li, ok := env.EnsureLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li, qt.IsNotNil)

	lb := env.GetLocalBinding(li)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, valuestest.SchemeEquals, values.Void)
}

func TestEnvironmentFrame_ExpandHierarchy(t *testing.T) {
	// Expand() returns the phase 1 environment
	env := NewTopLevelEnvironmentFrame()
	qt.Assert(t, env.Expand(), qt.IsNotNil)
	qt.Assert(t, env.Expand(), qt.Not(qt.Equals), env)
	qt.Assert(t, env.Expand().LocalEnvironment(), qt.IsNil)

	// Expand should be cached (same pointer)
	qt.Assert(t, env.Expand(), qt.Equals, env.Expand())

	// Expand().Expand() returns the same Expand environment
	expand2 := env.Expand().Expand()
	qt.Assert(t, expand2, qt.IsNotNil)
	qt.Assert(t, expand2, qt.Equals, env.Expand()) // Same expand environment

	// Compile is a different phase than Expand
	qt.Assert(t, env.Compile(), qt.Not(qt.Equals), env.Expand())
}

func TestEnvironmentFrame_PhaseHierarchy(t *testing.T) {
	// Test the indexed phase hierarchy:
	// TopLevel is phase 0, Expand is phase 1, Compile is phase 2
	topLevel := NewTopLevelEnvironmentFrame()

	// TopLevel is phase 0 (runtime)
	qt.Assert(t, topLevel.PhaseLevel(), qt.Equals, PhaseRuntime)

	// Runtime returns phase 0 environment (same as TopLevel for phase 0)
	runtime := topLevel.Runtime()
	qt.Assert(t, runtime, qt.IsNotNil)
	qt.Assert(t, runtime.PhaseLevel(), qt.Equals, PhaseRuntime)

	// Expand is phase 1
	expand := topLevel.Expand()
	qt.Assert(t, expand, qt.IsNotNil)
	qt.Assert(t, expand.PhaseLevel(), qt.Equals, PhaseExpand)

	// Compile is phase 2
	compile := topLevel.Compile()
	qt.Assert(t, compile, qt.IsNotNil)
	qt.Assert(t, compile.PhaseLevel(), qt.Equals, PhaseCompile)

	// Each phase should have its own environment
	qt.Assert(t, runtime, qt.Not(qt.Equals), expand)
	qt.Assert(t, runtime, qt.Not(qt.Equals), compile)
	qt.Assert(t, expand, qt.Not(qt.Equals), compile)

	// Expand and Compile have their own GlobalEnvironmentFrame
	qt.Assert(t, expand.GlobalEnvironment(), qt.Not(qt.Equals), runtime.GlobalEnvironment())
	qt.Assert(t, compile.GlobalEnvironment(), qt.Not(qt.Equals), expand.GlobalEnvironment())

	// Phase environments parent to TopLevel for interning access
	qt.Assert(t, expand.Parent(), qt.Equals, topLevel)
	qt.Assert(t, compile.Parent(), qt.Equals, topLevel)

	// TopLevel() should return the root from any frame
	qt.Assert(t, runtime.TopLevel(), qt.Equals, topLevel)
	qt.Assert(t, expand.TopLevel(), qt.Equals, topLevel)
	qt.Assert(t, compile.TopLevel(), qt.Equals, topLevel)

	// Phase accessors should be cached (same instance returned)
	qt.Assert(t, topLevel.Runtime(), qt.Equals, runtime)
	qt.Assert(t, topLevel.Expand(), qt.Equals, expand)
	qt.Assert(t, topLevel.Compile(), qt.Equals, compile)

	// AtPhase provides direct indexed access
	qt.Assert(t, topLevel.AtPhase(0), qt.Equals, topLevel)
	qt.Assert(t, topLevel.AtPhase(1), qt.Equals, expand)
	qt.Assert(t, topLevel.AtPhase(2), qt.Equals, compile)

	// Arbitrary phases can be created
	phase3 := topLevel.AtPhase(3)
	qt.Assert(t, phase3, qt.IsNotNil)
	qt.Assert(t, phase3.PhaseLevel(), qt.Equals, 3)
	qt.Assert(t, topLevel.AtPhase(3), qt.Equals, phase3) // Same instance

	// Negative phases (for future for-template support)
	phaseMinus1 := topLevel.AtPhase(-1)
	qt.Assert(t, phaseMinus1, qt.IsNotNil)
	qt.Assert(t, phaseMinus1.PhaseLevel(), qt.Equals, -1)
}

func TestEnvironmentFrame_SharedInterning(t *testing.T) {
	// Test that symbol interning is shared across phases
	tipTop := NewTopLevelEnvironmentFrame()
	runtime := tipTop.Runtime()
	expand := tipTop.Expand()

	// Intern a symbol from runtime
	sym1 := &values.Symbol{Key: "test-symbol"}
	interned1 := runtime.InternSymbol(sym1)

	// Intern the same symbol from expand - should get the same pointer
	sym2 := &values.Symbol{Key: "test-symbol"}
	interned2 := expand.InternSymbol(sym2)

	qt.Assert(t, interned1, qt.Equals, interned2)

	// Also verify from tip-top
	sym3 := &values.Symbol{Key: "test-symbol"}
	interned3 := tipTop.InternSymbol(sym3)
	qt.Assert(t, interned1, qt.Equals, interned3)
}

func TestEnvironmentFrame_GetBinding(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	// Create global binding
	globalSym := env.InternSymbol(values.NewSymbol("global-var"))
	env.MaybeCreateOwnGlobalBinding(globalSym, BindingTypeVariable)

	// Create local binding
	localSym := values.NewSymbol("local-var")
	env.MaybeCreateLocalBinding(localSym, BindingTypeVariable)

	// Test GetBinding for global
	gb := env.GetBinding(globalSym)
	qt.Assert(t, gb, qt.Not(qt.IsNil))
	qt.Assert(t, gb.BindingType(), qt.Equals, BindingTypeVariable)

	// Test GetBinding for local
	lb := env.GetBinding(localSym)
	qt.Assert(t, lb, qt.Not(qt.IsNil))
	qt.Assert(t, lb.BindingType(), qt.Equals, BindingTypeVariable)

	// Test GetBinding for non-existent
	nonexistent := values.NewSymbol("nonexistent")
	nb := env.GetBinding(nonexistent)
	qt.Assert(t, nb, qt.IsNil)
}

func TestEnvironmentFrame_GetBindingWithScopes(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	// Create a binding without scopes
	sym1 := values.NewSymbol("var1")
	li1, _ := env.EnsureLocalBinding(sym1, BindingTypeVariable)
	env.SetLocalValue(li1, values.NewInteger(42)) //nolint:errcheck

	// GetBindingWithScopes should return it (no scopes = always matches)
	b1 := env.GetBindingWithScopes(sym1, nil)
	qt.Assert(t, b1, qt.Not(qt.IsNil))
	qt.Assert(t, b1.Value(), valuestest.SchemeEquals, values.NewInteger(42))

	// Test with non-existent symbol
	sym2 := values.NewSymbol("nonexistent")
	b2 := env.GetBindingWithScopes(sym2, nil)
	qt.Assert(t, b2, qt.IsNil)
}

func TestEnvironmentFrame_MaybeCreateLocalBindingWithScopes(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	sym := values.NewSymbol("test-var")

	// Create binding with scopes
	li, created := env.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, nil)
	qt.Assert(t, created, qt.IsTrue)
	qt.Assert(t, li, qt.Not(qt.IsNil))

	// Try to create again - should return existing
	li2, created2 := env.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, nil)
	qt.Assert(t, created2, qt.IsFalse)
	qt.Assert(t, li2, qt.DeepEquals, li)

	// Test on environment with no local
	topEnv := NewTopLevelEnvironmentFrame()
	li3, created3 := topEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, nil)
	qt.Assert(t, created3, qt.IsFalse)
	qt.Assert(t, li3, qt.IsNil)
}

func TestEnvironmentFrame_GetLocalBindingByIndex(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	sym := values.NewSymbol("test-var")
	li, _ := env.EnsureLocalBinding(sym, BindingTypeVariable)
	val := values.NewInteger(42)
	env.SetLocalValue(li, val) //nolint:errcheck

	// GetLocalBindingByIndex takes an int (the index), not a LocalIndex
	binding := env.GetLocalBindingByIndex(li[0])
	qt.Assert(t, binding, qt.Not(qt.IsNil))
	qt.Assert(t, binding.Value(), valuestest.SchemeEquals, val)
}

func TestEnvironmentFrame_SetGlobalBindingByIndex(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()

	sym := env.InternSymbol(values.NewSymbol("test-global"))
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)

	// SetGlobalBindingByIndex takes an int and a binding
	newBinding := NewBinding(values.NewInteger(99), BindingTypeVariable)

	// Get the index from the global environment's keys map
	idx := env.global.keys[*gi.Index]
	env.SetGlobalBindingByIndex(idx, newBinding)

	binding := env.GetGlobalBinding(gi)
	qt.Assert(t, binding.Value(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestEnvironmentFrame_LibraryRegistry(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()

	// Initially nil
	qt.Assert(t, env.LibraryRegistry(), qt.IsNil)
}

func TestEnvironmentFrame_SetLibraryRegistry(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()

	// Test is minimal since LibraryRegistry type is in machine package
	// Just verify we can call SetLibraryRegistry without panic
	env.SetLibraryRegistry(nil)
	qt.Assert(t, env.LibraryRegistry(), qt.IsNil)
}

func TestEnvironmentFrame_SchemeString(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	str := env.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<environment>")
}

func TestEnvironmentFrame_IsVoid(t *testing.T) {
	var env *EnvironmentFrame
	qt.Assert(t, env.IsVoid(), qt.IsTrue)

	env2 := NewTopLevelEnvironmentFrame()
	qt.Assert(t, env2.IsVoid(), qt.IsFalse)
}

func TestEnvironmentFrame_EqualTo(t *testing.T) {
	env1 := NewTopLevelEnvironmentFrame()
	env2 := NewTopLevelEnvironmentFrame()

	// Two fresh top-level environments are equal (same structure)
	qt.Assert(t, env1.EqualTo(env2), qt.IsTrue)

	// Same environment is equal to itself
	qt.Assert(t, env1.EqualTo(env1), qt.IsTrue)

	// After adding different bindings, they should not be equal
	sym := env1.InternSymbol(values.NewSymbol("test"))
	env1.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
	qt.Assert(t, env1.EqualTo(env2), qt.IsFalse)

	// Non-EnvironmentFrame comparison
	qt.Assert(t, env1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestEnvironmentFrame_Copy(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	sym := values.NewSymbol("test")
	env.EnsureLocalBinding(sym, BindingTypeVariable)

	copied := env.Copy()
	qt.Assert(t, copied, qt.Not(qt.IsNil))
	qt.Assert(t, copied.LocalEnvironment(), qt.Not(qt.IsNil))
}

func TestEnvironmentFrame_InternSyntax(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()

	// Create a syntax value to intern
	sym := values.NewSymbol("test")
	// InternSyntax takes (key, syntaxValue) - first call with nil returns nil
	interned1 := env.InternSyntax(sym, nil)
	qt.Assert(t, interned1, qt.IsNil)
}

func TestEnvironmentFrame_GetLocalIndex_NotFound(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	// GetLocalIndex for non-existent should return nil
	idx := env.GetLocalIndex(values.NewSymbol("nonexistent"))
	qt.Assert(t, idx, qt.IsNil)
}

func TestEnvironmentFrame_GetLocalBinding_NotFound(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(2), env)

	// GetLocalBinding with invalid index - should return nil or handle gracefully
	idx := NewLocalIndex(0, 0)
	bnd := env.GetLocalBinding(idx)
	qt.Assert(t, bnd, qt.IsNotNil) // Returns binding at that index
}

func TestEnvironmentFrame_SetLocalValue_NoLocal(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()

	// SetLocalValue without local env
	idx := NewLocalIndex(0, 0)
	err := env.SetLocalValue(idx, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
}

func TestEnvironmentFrame_GetLocalBindingBySlotDepth(t *testing.T) {
	parent := NewTopLevelEnvironmentFrame()
	parent = NewEnvironmentFrameWithParent(NewLocalEnvironment(2), parent)
	parent.local.bindings[0] = Binding{value: values.NewInteger(10), bindingType: BindingTypeVariable}
	parent.local.bindings[1] = Binding{value: values.NewInteger(20), bindingType: BindingTypeVariable}

	child := NewEnvironmentFrameWithParent(NewLocalEnvironment(1), parent)
	child.local.bindings[0] = Binding{value: values.NewInteger(30), bindingType: BindingTypeVariable}

	// depth=0, slot=0 in child
	bd := child.GetLocalBindingBySlotDepth(0, 0)
	qt.Assert(t, bd, qt.IsNotNil)
	qt.Assert(t, bd.Value().EqualTo(values.NewInteger(30)), qt.IsTrue)

	// depth=1, slot=1 in parent
	bd = child.GetLocalBindingBySlotDepth(1, 1)
	qt.Assert(t, bd, qt.IsNotNil)
	qt.Assert(t, bd.Value().EqualTo(values.NewInteger(20)), qt.IsTrue)

	// depth exactly one past last frame -> nil (off-by-one boundary)
	// Chain is: child(0) -> parent(1) -> topLevel(2, no local)
	// depth=3 walks past topLevel to nil
	bd = child.GetLocalBindingBySlotDepth(0, 3)
	qt.Assert(t, bd, qt.IsNil)

	// depth well beyond parent chain -> nil
	bd = child.GetLocalBindingBySlotDepth(0, 5)
	qt.Assert(t, bd, qt.IsNil)
}

func TestEnvironmentFrame_SetLocalValueBySlotDepth(t *testing.T) {
	parent := NewTopLevelEnvironmentFrame()
	parent = NewEnvironmentFrameWithParent(NewLocalEnvironment(1), parent)
	parent.local.bindings[0] = Binding{value: values.NewInteger(10), bindingType: BindingTypeVariable}

	child := NewEnvironmentFrameWithParent(NewLocalEnvironment(1), parent)
	child.local.bindings[0] = Binding{value: values.NewInteger(30), bindingType: BindingTypeVariable}

	// Set in child (depth=0)
	err := child.SetLocalValueBySlotDepth(0, 0, values.NewInteger(99))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, child.local.bindings[0].Value().EqualTo(values.NewInteger(99)), qt.IsTrue)

	// Set in parent (depth=1)
	err = child.SetLocalValueBySlotDepth(0, 1, values.NewInteger(77))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, parent.local.bindings[0].Value().EqualTo(values.NewInteger(77)), qt.IsTrue)

	// No local frame -> error
	topOnly := NewTopLevelEnvironmentFrame()
	err = topOnly.SetLocalValueBySlotDepth(0, 0, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNotNil)

	// depth exactly one past last frame -> error (off-by-one boundary)
	// Chain is: child(0) -> parent(1) -> topLevel(2, no local)
	// depth=3 walks past topLevel to nil
	err = child.SetLocalValueBySlotDepth(0, 3, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNotNil)
}

func TestEnvironmentFrame_EqualTo_NilAndDifferent(t *testing.T) {
	var env1 *EnvironmentFrame
	env2 := NewTopLevelEnvironmentFrame()

	// Nil equals nil
	qt.Assert(t, env1.EqualTo(env1), qt.IsTrue)

	// Nil not equal to non-nil
	qt.Assert(t, env2.EqualTo(env1), qt.IsFalse)

	// Different type
	qt.Assert(t, env2.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestEnvironmentFrame_PanicSentinels(t *testing.T) {
	tcs := []struct {
		name     string
		trigger  func()
		sentinel error
	}{
		{
			"nil parent panics with ErrNilParentEnvironment",
			func() {
				NewEnvironmentFrameWithParent(nil, nil)
			},
			values.ErrNilParentEnvironment,
		},
		{
			"AtPhase without PhaseRegistry panics with ErrMissingPhaseRegistry",
			func() {
				env := NewEnvironmentFrame(nil, NewGlobalEnvironmentFrame())
				env.AtPhase(0)
			},
			values.ErrMissingPhaseRegistry,
		},
		{
			"InternSyntax without TopLevel panics with ErrMissingTopLevelEnvironment",
			func() {
				env := NewEnvironmentFrame(nil, NewGlobalEnvironmentFrame())
				env.InternSyntax(values.NewInteger(1), nil)
			},
			values.ErrMissingTopLevelEnvironment,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			defer func() {
				r := recover()
				if r == nil {
					t.Fatal("expected panic")
				}
				err, ok := r.(error)
				if !ok {
					t.Fatalf("panic value is not error: %T", r)
				}
				if !errors.Is(err, tc.sentinel) {
					t.Errorf("expected sentinel %v, got: %v", tc.sentinel, err)
				}
			}()
			tc.trigger()
		})
	}
}

func TestMaybeCreateLocalBindingWithScopes_Source(t *testing.T) {
	c := qt.New(t)

	src := syntax.NewSourceContext("x", "test.scm",
		syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(1, 1, 1))

	topEnv := NewTopLevelEnvironmentFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")

	li, created := env.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, src)
	c.Assert(created, qt.IsTrue)
	c.Assert(li, qt.IsNotNil)

	binding := env.GetLocalBindingByIndex(li[0])
	c.Assert(binding.Source(), qt.IsNotNil)
	c.Assert(binding.Source().File, qt.Equals, "test.scm")
}

func TestMaybeCreateLocalBindingWithScopes_NilSource(t *testing.T) {
	c := qt.New(t)

	topEnv := NewTopLevelEnvironmentFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")

	li, created := env.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, nil)
	c.Assert(created, qt.IsTrue)
	binding := env.GetLocalBindingByIndex(li[0])
	c.Assert(binding.Source(), qt.IsNil)
}

func TestMaybeCreateLocalBindingWithScopes_SourceWithOrigin(t *testing.T) {
	c := qt.New(t)

	origin := &syntax.OriginInfo{
		Identifier: "my-macro",
		Location: syntax.NewSourceContext("(my-macro x)", "user.scm",
			syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(12, 12, 1)),
	}
	src := &syntax.SourceContext{
		Text:   "temp",
		File:   "stdlib.scm",
		Origin: origin,
	}

	topEnv := NewTopLevelEnvironmentFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("temp")

	li, created := env.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, src)
	c.Assert(created, qt.IsTrue)

	binding := env.GetLocalBindingByIndex(li[0])
	c.Assert(binding.Source(), qt.IsNotNil)
	c.Assert(binding.Source().Origin, qt.IsNotNil)
	c.Assert(binding.Source().Origin.Identifier, qt.Equals, "my-macro")
	c.Assert(binding.Source().Origin.Location.File, qt.Equals, "user.scm")
}

func TestGlobalBinding_SetSource(t *testing.T) {
	c := qt.New(t)

	src := syntax.NewSourceContext("x", "global.scm",
		syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(1, 1, 1))

	topEnv := NewTopLevelEnvironmentFrame()
	sym := values.NewSymbol("x")

	gi, created := topEnv.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
	c.Assert(created, qt.IsTrue)
	c.Assert(gi, qt.IsNotNil)

	binding := topEnv.GetGlobalBinding(gi)
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.Source(), qt.IsNil)

	binding.SetSource(src)
	c.Assert(binding.Source(), qt.IsNotNil)
	c.Assert(binding.Source().File, qt.Equals, "global.scm")
}

func TestMaybeCreateLocalBindingWithScopes_ExistingBindingGetsSource(t *testing.T) {
	c := qt.New(t)

	topEnv := NewTopLevelEnvironmentFrame()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), topEnv)
	sym := values.NewSymbol("x")

	// First creation: no source
	li, created := env.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, nil)
	c.Assert(created, qt.IsTrue)
	c.Assert(env.GetLocalBindingByIndex(li[0]).Source(), qt.IsNil)

	// Second call with source: should update
	src := syntax.NewSourceContext("x", "updated.scm",
		syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(1, 1, 1))
	li2, created2 := env.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, src)
	c.Assert(created2, qt.IsFalse)
	c.Assert(li2[0], qt.Equals, li[0])

	binding := env.GetLocalBindingByIndex(li[0])
	c.Assert(binding.Source(), qt.IsNotNil)
	c.Assert(binding.Source().File, qt.Equals, "updated.scm")
}

// ---------------------------------------------------------------------------
// InitApplyFrame
// ---------------------------------------------------------------------------

func TestInitApplyFrame_PopulatesExistingFrame(t *testing.T) {
	// Set up source env with bindings (simulates closure env).
	tl := NewTopLevelEnvironment()
	parent := tl.Runtime()
	local := NewLocalEnvironment(2)
	src := NewEnvironmentFrameWithParent(local, parent)
	li0 := &LocalIndex{0, 0}
	li1 := &LocalIndex{1, 0}
	src.SetLocalValue(li0, values.NewInteger(10))
	src.SetLocalValue(li1, values.NewInteger(20))

	// Populate dst from src.
	var dst EnvironmentFrame
	src.InitApplyFrame(&dst)

	// Parent chain should match.
	qt.Assert(t, dst.Parent(), qt.Equals, parent)

	// Bindings should be independent copies.
	dstBindings := dst.LocalEnvironment().Bindings()
	qt.Assert(t, len(dstBindings), qt.Equals, 2)
	qt.Assert(t, dstBindings[0].Value(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, dstBindings[1].Value(), valuestest.SchemeEquals, values.NewInteger(20))

	// Mutating dst should not affect src.
	dstBindings[0].SetValue(values.NewInteger(99))
	qt.Assert(t, src.GetLocalBindingByIndex(0).Value(), valuestest.SchemeEquals, values.NewInteger(10))
}

func TestInitApplyFrame_MatchesNewApplyFrame(t *testing.T) {
	// InitApplyFrame must produce the same result as NewApplyFrame.
	tl := NewTopLevelEnvironment()
	parent := tl.Runtime()
	local := NewLocalEnvironment(3)
	src := NewEnvironmentFrameWithParent(local, parent)
	for i := range 3 {
		src.SetLocalValue(&LocalIndex{i, 0}, values.NewInteger(int64(i*10)))
	}

	fromNew := src.NewApplyFrame()

	var fromInit EnvironmentFrame
	src.InitApplyFrame(&fromInit)

	// Same parent.
	qt.Assert(t, fromInit.Parent(), qt.Equals, fromNew.Parent())

	// Same bindings.
	newBindings := fromNew.LocalEnvironment().Bindings()
	initBindings := fromInit.LocalEnvironment().Bindings()
	qt.Assert(t, len(initBindings), qt.Equals, len(newBindings))
	for i := range newBindings {
		qt.Assert(t, initBindings[i].Value(), valuestest.SchemeEquals, newBindings[i].Value())
	}
}

func TestInitApplyFrame_PanicsOnNilParent(t *testing.T) {
	// A frame with no parent should panic, same as NewApplyFrame.
	src := &EnvironmentFrame{}
	var dst EnvironmentFrame

	defer func() {
		r := recover()
		qt.Assert(t, r, qt.IsNotNil)
		err, ok := r.(error)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, errors.Is(err, values.ErrNilParentEnvironment), qt.IsTrue)
	}()
	src.InitApplyFrame(&dst)
}

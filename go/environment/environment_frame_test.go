package environment

import (
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNewEnvironmentFrame(t *testing.T) {
	q := NewEnvironmentFrame(nil, nil)
	qt.Assert(t, q, qt.Not(qt.IsNil))
	qt.Assert(t, q.GlobalEnvironment(), qt.IsNil)
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
	qt.Assert(t, lb.value, values.SchemeEquals, value0)
	lb = env.GetLocalBinding(li1)
	qt.Assert(t, lb.value, values.SchemeEquals, value1)
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
	gi0, ok := env.MaybeCreateGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi0, values.SchemeEquals, NewGlobalIndex(tv0))

	// Set the initial value of the new binding
	err := env.SetGlobalValue(gi0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Re-adding the same binding should not change the index
	tv0 = env.InternSymbol(values.NewSymbol("testVar0"))
	gi0, ok = env.MaybeCreateGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, gi0.Index, values.SchemeEquals, tv0)

	// Adding a new binding should create a new index
	tv1 := values.NewSymbol("testVar1")
	gi1, ok := env.MaybeCreateGlobalBinding(tv1, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi1.Index, values.SchemeEquals, tv1)

	// Set the initial value of the new binding
	err = env.SetGlobalValue(gi1, value1)
	qt.Assert(t, err, qt.IsNil)

	bd := env.GetGlobalBinding(gi0)
	qt.Assert(t, bd.value, values.SchemeEquals, value0)
	bd = env.GetGlobalBinding(gi1)
	qt.Assert(t, bd.value, values.SchemeEquals, value1)
}

func TestEnvironmentFrame_Bindings(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	// check global environment
	tv0 := env.InternSymbol(values.NewSymbol("testVar0"))
	qt.Assert(t, env, qt.Not(qt.IsNil))
	_, ok := env.MaybeCreateGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	_, ok = env.MaybeCreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)

	tv0 = env.InternSymbol(values.NewSymbol("testVar0"))
	gi := env.GetGlobalIndex(tv0)
	qt.Assert(t, ok, qt.IsTrue)
	gb := env.GetGlobalBinding(gi)
	qt.Assert(t, gb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, gb.value, qt.Equals, values.Void)

	// check local environment
	li0 := env.GetLocalIndex(tv0)
	qt.Assert(t, li0, qt.IsNotNil)
	lb := env.GetLocalBinding(li0)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, qt.Equals, values.Void)

	err := env.SetLocalValue(li0, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)

	lb = env.GetLocalBinding(li0)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, values.SchemeEquals, values.NewInteger(42))

	err = env.SetGlobalValue(gi, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)

	gb = env.GetGlobalBinding(gi)
	qt.Assert(t, gb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, gb.value, values.SchemeEquals, values.NewInteger(42))

	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
	li1, ok := env.CreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li1[0], qt.Equals, 0)
	qt.Assert(t, li1[1], qt.Equals, 0)

	qt.Assert(t, env.local.bindings, qt.HasLen, 1)
	qt.Assert(t, env.parent.local.bindings, qt.HasLen, 1)

	err = env.SetLocalValue(li1, values.NewInteger(43))
	qt.Assert(t, err, qt.IsNil)
	lb = env.GetLocalBinding(li1)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, values.SchemeEquals, values.NewInteger(43))

	lb = env.parent.GetLocalBinding(li0)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, values.SchemeEquals, values.NewInteger(42))
}

func TestEnvironmentFrame_Hierarchy(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()

	tv0 := values.NewSymbol("testVar0")
	gi, ok := env.CreateGlobalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)

	_, ok = env.CreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)

	gb := env.GetGlobalBinding(gi)
	qt.Assert(t, gb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, gb.value, qt.Equals, values.Void)

	lenv := NewLocalEnvironment(0)
	env = NewEnvironmentFrameWithParent(lenv, env)

	li, ok := env.CreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li, qt.IsNotNil)

	lb := env.GetLocalBinding(li)
	qt.Assert(t, lb.bindingType, qt.Equals, BindingTypeVariable)
	qt.Assert(t, lb.value, qt.Equals, values.Void)
}

func TestEnvironmentFrame_MetaHierarchy(t *testing.T) {
	// Meta() returns Expand() for backward compatibility
	env := NewTopLevelEnvironmentFrame()
	qt.Assert(t, env.Meta(), qt.IsNotNil)
	qt.Assert(t, env.Meta(), qt.Not(qt.Equals), env)
	qt.Assert(t, env.Meta().LocalEnvironment(), qt.IsNil)

	// Meta (Expand) should be cached (same pointer)
	qt.Assert(t, env.Meta(), qt.Equals, env.Meta())

	// Meta().Meta() returns the same Expand (Meta/Expand always returns TopLevel.meta)
	meta2 := env.Meta().Meta()
	qt.Assert(t, meta2, qt.IsNotNil)
	qt.Assert(t, meta2, qt.Equals, env.Meta()) // Same expand environment

	// To get Compile, call Compile() directly
	qt.Assert(t, env.Compile(), qt.Not(qt.Equals), env.Expand())
}

func TestEnvironmentFrame_PhaseHierarchy(t *testing.T) {
	// Test the new chain-based phase hierarchy:
	// TopLevel (= Runtime) → meta → Expand → meta → Compile
	topLevel := NewTipTopEnvironmentFrame()

	// Runtime IS the top-level
	runtime := topLevel.Runtime()
	qt.Assert(t, runtime, qt.Equals, topLevel)

	// Expand is created lazily via meta
	qt.Assert(t, topLevel.meta, qt.IsNil)
	expand := topLevel.Expand()
	qt.Assert(t, expand, qt.IsNotNil)
	qt.Assert(t, topLevel.meta, qt.Equals, expand)

	// Compile chains from expand
	qt.Assert(t, expand.meta, qt.IsNil)
	compile := topLevel.Compile()
	qt.Assert(t, compile, qt.IsNotNil)
	qt.Assert(t, expand.meta, qt.Equals, compile)

	// Each phase should have its own environment
	qt.Assert(t, runtime, qt.Not(qt.Equals), expand)
	qt.Assert(t, runtime, qt.Not(qt.Equals), compile)
	qt.Assert(t, expand, qt.Not(qt.Equals), compile)

	// Expand and Compile have their own GlobalEnvironmentFrame
	qt.Assert(t, expand.GlobalEnvironment(), qt.Not(qt.Equals), runtime.GlobalEnvironment())
	qt.Assert(t, compile.GlobalEnvironment(), qt.Not(qt.Equals), expand.GlobalEnvironment())

	// Chain structure: expand parents to runtime, compile parents to expand
	qt.Assert(t, expand.Parent(), qt.Equals, runtime)
	qt.Assert(t, compile.Parent(), qt.Equals, expand)

	// TopLevel() should return the root from any frame
	qt.Assert(t, runtime.TopLevel(), qt.Equals, topLevel)
	qt.Assert(t, expand.TopLevel(), qt.Equals, topLevel)
	qt.Assert(t, compile.TopLevel(), qt.Equals, topLevel)

	// Phase accessors should be cached
	qt.Assert(t, topLevel.Runtime(), qt.Equals, runtime)
	qt.Assert(t, topLevel.Expand(), qt.Equals, expand)
	qt.Assert(t, topLevel.Compile(), qt.Equals, compile)
}

func TestEnvironmentFrame_SharedInterning(t *testing.T) {
	// Test that symbol interning is shared across phases
	tipTop := NewTipTopEnvironmentFrame()
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

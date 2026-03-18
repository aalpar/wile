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
	"reflect"
	"testing"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestLocalEnvironment(t *testing.T) {
	// Create a new Local environment
	env := NewLocalEnvironment(0)

	// Check if the environment is initialized correctly
	if env == nil {
		t.Fatal("Expected a non-nil Local environment")
	}

	value0 := values.NewInteger(42)
	value1 := values.NewInteger(43)

	// variable has not been added yet, so GetLocalIndex should return nil
	tv0 := values.NewSymbol("testVar0")
	li0 := env.GetLocalIndex(tv0)
	qt.Assert(t, li0, qt.IsNil)

	// Test adding a binding
	li0, ok := env.EnsureLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err := env.SetLocalValue(li0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Re-adding the same binding should not change the index
	li0, ok = env.EnsureLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Adding a new binding should create a new index
	tv1 := values.NewSymbol("testVar1")
	li1, ok := env.EnsureLocalBinding(tv1, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li1[0], qt.Equals, 1)
	qt.Assert(t, li1[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err = env.SetLocalValue(li1, value1)
	qt.Assert(t, err, qt.IsNil)

	v := env.GetLocalBinding(li0)
	qt.Assert(t, v.value, valuestest.SchemeEquals, value0)
	v = env.GetLocalBinding(li1)
	qt.Assert(t, v.value, valuestest.SchemeEquals, value1)
}

func TestLocalEnvironmentFrame_Bindings(t *testing.T) {
	le := NewLocalEnvironment(0)
	qt.Assert(t, le, qt.Not(qt.IsNil))
	le.EnsureLocalBinding(values.NewSymbol("testVar0"), BindingTypeVariable)

	bindings := le.Bindings()
	qt.Assert(t, bindings, qt.HasLen, 1)
}

func TestLocalEnvironmentFrame_SetBindings(t *testing.T) {
	le := NewLocalEnvironment(0)

	sym := values.NewSymbol("test")
	le.EnsureLocalBinding(sym, BindingTypeVariable)

	// Create new bindings
	newBindings := []Binding{
		{value: values.NewInteger(1), bindingType: BindingTypeVariable},
		{value: values.NewInteger(2), bindingType: BindingTypeVariable},
	}

	le.SetBindings(newBindings)
	qt.Assert(t, le.Bindings(), qt.HasLen, 2)
}

func TestLocalEnvironmentFrame_Keys(t *testing.T) {
	le := NewLocalEnvironment(0)

	sym1 := values.NewSymbol("var1")
	sym2 := values.NewSymbol("var2")

	le.EnsureLocalBinding(sym1, BindingTypeVariable)
	le.EnsureLocalBinding(sym2, BindingTypeVariable)

	keys := le.Keys()
	qt.Assert(t, keys, qt.HasLen, 2)
}

func TestLocalEnvironmentFrame_Keys_DefensiveCopy(t *testing.T) {
	le := NewLocalEnvironment(0)
	sym1 := values.NewSymbol("var1")
	le.EnsureLocalBinding(sym1, BindingTypeVariable)

	keys := le.Keys()
	// Mutating the returned map must not affect internal state.
	bogus := values.NewSymbol("bogus")
	keys[*bogus] = 99

	qt.Assert(t, le.Keys(), qt.HasLen, 1)
	qt.Assert(t, le.GetLocalIndex(bogus), qt.IsNil)
}

func TestLocalEnvironmentFrame_SchemeString(t *testing.T) {
	le := NewLocalEnvironment(0)
	str := le.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<Local-environment>")
}

func TestLocalEnvironmentFrame_IsVoid(t *testing.T) {
	var le *LocalEnvironmentFrame
	qt.Assert(t, le.IsVoid(), qt.IsTrue)

	le2 := NewLocalEnvironment(0)
	qt.Assert(t, le2.IsVoid(), qt.IsFalse)
}

func TestLocalEnvironmentFrame_EqualTo(t *testing.T) {
	le1 := NewLocalEnvironment(0)
	le2 := NewLocalEnvironment(0)

	// Two fresh local environments are equal (same structure)
	qt.Assert(t, le1.EqualTo(le2), qt.IsTrue)

	// Same environment is equal to itself
	qt.Assert(t, le1.EqualTo(le1), qt.IsTrue)

	// After adding different bindings, they should not be equal
	sym := values.NewSymbol("test")
	le1.EnsureLocalBinding(sym, BindingTypeVariable)
	qt.Assert(t, le1.EqualTo(le2), qt.IsFalse)

	// Non-LocalEnvironmentFrame comparison
	qt.Assert(t, le1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestLocalEnvironmentFrame_Copy(t *testing.T) {
	le := NewLocalEnvironment(0)

	sym := values.NewSymbol("test")
	le.EnsureLocalBinding(sym, BindingTypeVariable)

	copied := le.Copy()
	leCopy, ok := copied.(*LocalEnvironmentFrame)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, leCopy, qt.Not(qt.IsNil))

	// Verify bindings were copied
	qt.Assert(t, len(leCopy.Bindings()), qt.Equals, len(le.Bindings()))
}

func TestCopyForApply_SharesKeys(t *testing.T) {
	le := NewLocalEnvironment(0)
	sym := values.NewSymbol("x")
	le.EnsureLocalBinding(sym, BindingTypeVariable)

	copied := le.CopyForApply()

	// Internal keys map is shared (CoW); Keys() returns a defensive copy
	qt.Assert(t, reflect.ValueOf(copied.keys).Pointer(), qt.Equals, reflect.ValueOf(le.keys).Pointer())
}

func TestCopyForApply_IndependentBindings(t *testing.T) {
	le := NewLocalEnvironment(2)
	li0 := &LocalIndex{0, 0}
	li1 := &LocalIndex{1, 0}
	le.SetLocalValue(li0, values.NewInteger(10))
	le.SetLocalValue(li1, values.NewInteger(20))

	copied := le.CopyForApply()

	// SetValue on copy does not affect original
	copied.SetLocalValue(li0, values.NewInteger(99))
	qt.Assert(t, le.GetLocalBinding(li0).Value(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, copied.GetLocalBinding(li0).Value(), valuestest.SchemeEquals, values.NewInteger(99))

	// Original still intact
	qt.Assert(t, le.GetLocalBinding(li1).Value(), valuestest.SchemeEquals, values.NewInteger(20))
}

func TestCopyForApply_NilSafe(t *testing.T) {
	var le *LocalEnvironmentFrame
	result := le.CopyForApply()
	qt.Assert(t, result, qt.IsNil)
}

func TestCopyForApply_EnsureLocalBindingCowsKeys(t *testing.T) {
	le := NewLocalEnvironment(0)
	sym1 := values.NewSymbol("x")
	le.EnsureLocalBinding(sym1, BindingTypeVariable)

	// CopyForApply sets keysShared on both
	copied := le.CopyForApply()

	// EnsureLocalBinding on the copy should COW the keys map
	sym2 := values.NewSymbol("y")
	copied.EnsureLocalBinding(sym2, BindingTypeVariable)

	// Copy now has "y", original does not
	qt.Assert(t, copied.GetLocalIndex(sym2), qt.Not(qt.IsNil))
	qt.Assert(t, le.GetLocalIndex(sym2), qt.IsNil)

	// Original keys unaffected
	qt.Assert(t, len(le.Keys()), qt.Equals, 1)
	qt.Assert(t, len(copied.Keys()), qt.Equals, 2)
}

func TestCopyInto_CopiesBindings(t *testing.T) {
	le := NewLocalEnvironment(2)
	li0 := &LocalIndex{0, 0}
	li1 := &LocalIndex{1, 0}
	le.SetLocalValue(li0, values.NewInteger(10))
	le.SetLocalValue(li1, values.NewInteger(20))

	var dst LocalEnvironmentFrame
	le.copyInto(&dst)

	// Bindings are independent copies
	qt.Assert(t, len(dst.bindings), qt.Equals, 2)
	qt.Assert(t, dst.bindings[0].Value(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, dst.bindings[1].Value(), valuestest.SchemeEquals, values.NewInteger(20))

	// Internal keys map is shared (CoW); Keys() returns a defensive copy
	qt.Assert(t, reflect.ValueOf(dst.keys).Pointer(), qt.Equals, reflect.ValueOf(le.keys).Pointer())
	qt.Assert(t, dst.keysShared, qt.IsTrue)

	// Mutating dst does not affect source
	dst.bindings[0].SetValue(values.NewInteger(99))
	qt.Assert(t, le.GetLocalBinding(li0).Value(), valuestest.SchemeEquals, values.NewInteger(10))
}

func TestCopyForApplyInto_MarksBothShared(t *testing.T) {
	le := NewLocalEnvironment(1)
	li0 := &LocalIndex{0, 0}
	le.SetLocalValue(li0, values.NewInteger(42))

	var dst LocalEnvironmentFrame
	le.copyForApplyInto(&dst)

	// Both source and dest have keysShared set
	qt.Assert(t, le.keysShared, qt.IsTrue)
	qt.Assert(t, dst.keysShared, qt.IsTrue)

	// Bindings are independent
	qt.Assert(t, dst.bindings[0].Value(), valuestest.SchemeEquals, values.NewInteger(42))
	dst.bindings[0].SetValue(values.NewInteger(99))
	qt.Assert(t, le.GetLocalBinding(li0).Value(), valuestest.SchemeEquals, values.NewInteger(42))
}

// Copyright 2025 Aaron Alpar
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
	"wile/values"
	"testing"

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
	li0, ok := env.CreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err := env.SetLocalValue(li0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Re-adding the same binding should not change the index
	li0, ok = env.CreateLocalBinding(tv0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, li0[0], qt.Equals, 0)
	qt.Assert(t, li0[1], qt.Equals, 0)

	// Adding a new binding should create a new index
	tv1 := values.NewSymbol("testVar1")
	li1, ok := env.CreateLocalBinding(tv1, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, li1[0], qt.Equals, 1)
	qt.Assert(t, li1[1], qt.Equals, 0)

	// Set the initial value of the new binding
	err = env.SetLocalValue(li1, value1)
	qt.Assert(t, err, qt.IsNil)

	v := env.GetLocalBinding(li0)
	qt.Assert(t, v.value, values.SchemeEquals, value0)
	v = env.GetLocalBinding(li1)
	qt.Assert(t, v.value, values.SchemeEquals, value1)
}

func TestLocalEnvironmentFrame_Bindings(t *testing.T) {
	le := NewLocalEnvironment(0)
	qt.Assert(t, le, qt.Not(qt.IsNil))
	le.CreateLocalBinding(values.NewSymbol("testVar0"), BindingTypeVariable)

	bindings := le.Bindings()
	qt.Assert(t, bindings, qt.HasLen, 1)
}

func TestLocalEnvironmentFrame_SetBindings(t *testing.T) {
	le := NewLocalEnvironment(0)

	sym := values.NewSymbol("test")
	le.CreateLocalBinding(sym, BindingTypeVariable)

	// Create new bindings
	newBindings := []*Binding{
		NewBinding(values.NewInteger(1), BindingTypeVariable),
		NewBinding(values.NewInteger(2), BindingTypeVariable),
	}

	le.SetBindings(newBindings)
	qt.Assert(t, le.Bindings(), qt.HasLen, 2)
}

func TestLocalEnvironmentFrame_Keys(t *testing.T) {
	le := NewLocalEnvironment(0)

	sym1 := values.NewSymbol("var1")
	sym2 := values.NewSymbol("var2")

	le.CreateLocalBinding(sym1, BindingTypeVariable)
	le.CreateLocalBinding(sym2, BindingTypeVariable)

	keys := le.Keys()
	qt.Assert(t, keys, qt.HasLen, 2)
}

func TestLocalEnvironmentFrame_SchemeString(t *testing.T) {
	le := NewLocalEnvironment(0)
	str := le.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<Local-environment>")
}

func TestLocalEnvironmentFrame_IsVoid(t *testing.T) {
	var le *LocalEnvironmentFrame = nil
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
	le1.CreateLocalBinding(sym, BindingTypeVariable)
	qt.Assert(t, le1.EqualTo(le2), qt.IsFalse)

	// Non-LocalEnvironmentFrame comparison
	qt.Assert(t, le1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestLocalEnvironmentFrame_Copy(t *testing.T) {
	le := NewLocalEnvironment(0)

	sym := values.NewSymbol("test")
	le.CreateLocalBinding(sym, BindingTypeVariable)

	copied := le.Copy()
	leCopy, ok := copied.(*LocalEnvironmentFrame)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, leCopy, qt.Not(qt.IsNil))

	// Verify bindings were copied
	qt.Assert(t, len(leCopy.Bindings()), qt.Equals, len(le.Bindings()))
}

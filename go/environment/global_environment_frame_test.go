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

func TestGlobalEnvironment(t *testing.T) {
	// Create a new Local environment
	env := NewTopLevelGlobalEnvironmentFrame()

	// Check if the environment is initialized correctly
	if env == nil {
		t.Fatal("Expected a non-nil Local environment")
	}

	value0 := values.NewInteger(42)
	value1 := values.NewInteger(43)

	sym0 := values.NewSymbol("testVar0")
	sym1 := values.NewSymbol("testVar1")
	// variable has not been added yet, so GetLocalIndex should return nil
	gi0 := env.GetGlobalIndex(sym0)
	qt.Assert(t, gi0, qt.IsNil)

	gi1 := env.GetGlobalIndex(sym1)
	qt.Assert(t, gi1, qt.IsNil)

	// Test adding a binding
	gi0, ok := env.CreateGlobalBinding(sym0, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi0.Index.EqualTo(values.NewSymbol("testVar0")), qt.IsTrue)

	// Set the initial value of the new binding
	err := env.SetGlobalValue(gi0, value0)
	qt.Assert(t, err, qt.IsNil)

	// Adding a new binding should create a new index
	gi1, ok = env.CreateGlobalBinding(sym1, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, gi1.Index.EqualTo(values.NewSymbol("testVar1")), qt.IsTrue)

	// Set the initial value of the new binding
	err = env.SetGlobalValue(gi1, value1)
	qt.Assert(t, err, qt.IsNil)

	gb2 := env.GetOwnGlobalBinding(gi0).Value()
	qt.Assert(t, gb2, values.SchemeEquals, value0)

	gb3 := env.GetOwnGlobalBinding(gi1).Value()
	qt.Assert(t, gb3, values.SchemeEquals, value1)
}

func TestGlobalEnvironmentFrame_Keys(t *testing.T) {
	env := NewTopLevelGlobalEnvironmentFrame()

	sym1 := values.NewSymbol("var1")
	sym2 := values.NewSymbol("var2")

	env.CreateGlobalBinding(sym1, BindingTypeVariable)
	env.CreateGlobalBinding(sym2, BindingTypeVariable)

	keys := env.Keys()
	qt.Assert(t, keys, qt.HasLen, 2)
}

func TestGlobalEnvironmentFrame_Copy(t *testing.T) {
	env := NewTopLevelGlobalEnvironmentFrame()

	sym := values.NewSymbol("test")
	env.CreateGlobalBinding(sym, BindingTypeVariable)

	copied := env.Copy()
	envCopy, ok := copied.(*GlobalEnvironmentFrame)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, envCopy, qt.Not(qt.IsNil))

	// Verify bindings were copied
	qt.Assert(t, len(envCopy.Bindings()), qt.Equals, len(env.Bindings()))
}

func TestGlobalEnvironmentFrame_IsVoid(t *testing.T) {
	var env *GlobalEnvironmentFrame = nil
	qt.Assert(t, env.IsVoid(), qt.IsTrue)

	env2 := NewTopLevelGlobalEnvironmentFrame()
	qt.Assert(t, env2.IsVoid(), qt.IsFalse)
}

func TestGlobalEnvironmentFrame_SchemeString(t *testing.T) {
	env := NewTopLevelGlobalEnvironmentFrame()
	str := env.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<global-environment>")
}

func TestGlobalEnvironmentFrame_EqualTo(t *testing.T) {
	env1 := NewTopLevelGlobalEnvironmentFrame()
	env2 := NewTopLevelGlobalEnvironmentFrame()

	// Two fresh global environments are equal (same structure)
	qt.Assert(t, env1.EqualTo(env2), qt.IsTrue)

	// Same environment is equal to itself
	qt.Assert(t, env1.EqualTo(env1), qt.IsTrue)

	// After adding different bindings, they should not be equal
	sym := values.NewSymbol("test")
	env1.CreateGlobalBinding(sym, BindingTypeVariable)
	qt.Assert(t, env1.EqualTo(env2), qt.IsFalse)

	// Non-GlobalEnvironmentFrame comparison
	qt.Assert(t, env1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestGlobalEnvironmentFrame_LibraryRegistry(t *testing.T) {
	env := NewTopLevelGlobalEnvironmentFrame()

	// Initially nil
	qt.Assert(t, env.LibraryRegistry(), qt.IsNil)
}

func TestGlobalEnvironmentFrame_SetLibraryRegistry(t *testing.T) {
	env := NewTopLevelGlobalEnvironmentFrame()

	// Test is minimal since LibraryRegistry type is in machine package
	// Just verify we can call SetLibraryRegistry without panic
	env.SetLibraryRegistry(nil)
	qt.Assert(t, env.LibraryRegistry(), qt.IsNil)
}

func TestGlobalEnvironmentFrame_InternSyntax(t *testing.T) {
	env := NewTopLevelGlobalEnvironmentFrame()

	// Create a syntax value to intern
	sym := values.NewSymbol("test")
	// InternSyntax takes key and syntax value
	interned1 := env.InternSyntax(sym, nil)
	// First call with nil value returns nil
	qt.Assert(t, interned1, qt.IsNil)
}

func TestGlobalEnvironmentFrame_GetGlobalIndex_NotFound(t *testing.T) {
	env := NewTopLevelGlobalEnvironmentFrame()

	// Get index for symbol that doesn't exist
	sym := values.NewSymbol("nonexistent")
	gi := env.GetGlobalIndex(sym)
	qt.Assert(t, gi, qt.IsNil)
}

func TestGlobalEnvironmentFrame_EqualTo_NilCases(t *testing.T) {
	env2 := NewTopLevelGlobalEnvironmentFrame()

	// Non-nil equals nil literal returns false
	qt.Assert(t, env2.EqualTo(nil), qt.IsFalse)

	// Different binding counts
	sym := values.NewSymbol("test")
	env3 := NewTopLevelGlobalEnvironmentFrame()
	env3.CreateGlobalBinding(sym, BindingTypeVariable)
	qt.Assert(t, env2.EqualTo(env3), qt.IsFalse)
}

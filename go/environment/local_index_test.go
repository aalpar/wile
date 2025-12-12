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

func TestLocalIndex_Over(t *testing.T) {
	li := NewLocalIndex(3, 2)
	qt.Assert(t, li.Over(), qt.Equals, 3)
}

func TestLocalIndex_Up(t *testing.T) {
	li := NewLocalIndex(3, 2)
	qt.Assert(t, li.Up(), qt.Equals, 2)
}

func TestLocalIndex_GetBinding(t *testing.T) {
	env := NewTopLevelEnvironmentFrame()
	env = NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)

	tv := values.NewSymbol("test")
	li, ok := env.CreateLocalBinding(tv, BindingTypeVariable)
	qt.Assert(t, ok, qt.IsTrue)

	val := values.NewInteger(42)
	err := env.SetLocalValue(li, val)
	qt.Assert(t, err, qt.IsNil)

	// Use LocalIndex.GetBinding method
	binding := li.GetBinding(env)
	qt.Assert(t, binding, qt.Not(qt.IsNil))
	qt.Assert(t, binding.Value(), values.SchemeEquals, val)
}

func TestLocalIndex_String(t *testing.T) {
	li := NewLocalIndex(5, 3)
	qt.Assert(t, li.String(), qt.Equals, "5:3")
}

func TestLocalIndex_SchemeString(t *testing.T) {
	li := NewLocalIndex(2, 1)
	qt.Assert(t, li.SchemeString(), qt.Equals, "<local-index 2:1>")
}

func TestLocalIndex_EqualTo(t *testing.T) {
	li1 := NewLocalIndex(2, 3)
	li2 := NewLocalIndex(2, 3)
	li3 := NewLocalIndex(2, 4)
	li4 := NewLocalIndex(3, 3)

	qt.Assert(t, li1.EqualTo(li2), qt.IsTrue)
	qt.Assert(t, li1.EqualTo(li3), qt.IsFalse)
	qt.Assert(t, li1.EqualTo(li4), qt.IsFalse)

	// Test nil cases
	var nilIndex *LocalIndex = nil
	qt.Assert(t, nilIndex.EqualTo(nilIndex), qt.IsTrue)
	qt.Assert(t, li1.EqualTo(nilIndex), qt.IsFalse)
	qt.Assert(t, nilIndex.EqualTo(li1), qt.IsFalse)
}

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

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// NewEnvironmentFrame (isolated constructor) — uses different name to avoid conflict

func TestNewEnvironmentFrame_Isolated(t *testing.T) {
	c := qt.New(t)
	local := NewLocalEnvironment(2)
	global := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol]int{},
	}
	env := NewEnvironmentFrame(local, global)
	c.Assert(env, qt.IsNotNil)
	c.Assert(env.local, qt.Equals, local)
	c.Assert(env.global, qt.Equals, global)
	c.Assert(env.phaseLevel, qt.Equals, PhaseRuntime)
	c.Assert(env.phases, qt.IsNil)
}

// TopLevelEnvironment value interface methods

func TestTopLevelEnvironment_SchemeString_Coverage(t *testing.T) {
	c := qt.New(t)

	t.Run("with name", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		topLevel.Name = "interaction-environment"
		c.Assert(topLevel.SchemeString(), qt.Equals, "#<environment interaction-environment>")
	})

	t.Run("without name", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		c.Assert(topLevel.SchemeString(), qt.Equals, "#<environment>")
	})
}

func TestTopLevelEnvironment_IsVoid_Coverage(t *testing.T) {
	c := qt.New(t)
	topLevel := NewTopLevelEnvironment()
	c.Assert(topLevel.IsVoid(), qt.IsFalse)

	var nilTLE *TopLevelEnvironment
	c.Assert(nilTLE.IsVoid(), qt.IsTrue)
}

func TestTopLevelEnvironment_EqualTo_Coverage(t *testing.T) {
	c := qt.New(t)
	a := NewTopLevelEnvironment()
	b := NewTopLevelEnvironment()

	c.Assert(a.EqualTo(a), qt.IsTrue)
	c.Assert(a.EqualTo(b), qt.IsFalse)
	c.Assert(a.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestTopLevelEnvironment_SyntaxInternCount_Coverage(t *testing.T) {
	c := qt.New(t)
	topLevel := NewTopLevelEnvironment()
	c.Assert(topLevel.SyntaxInternCount(), qt.Equals, 0)
}

// NewChildTopLevelEnvironment

func TestTopLevelEnvironment_NewChildTopLevelEnvironment_Coverage(t *testing.T) {
	c := qt.New(t)
	parent := NewTopLevelEnvironment()

	child := parent.NewChildTopLevelEnvironment()
	c.Assert(child, qt.IsNotNil)
	c.Assert(child.Runtime(), qt.IsNotNil)
	c.Assert(child.Phases(), qt.IsNotNil)

	// Interning delegates to parent
	sym := values.NewSymbol("test-sym")
	interned1 := parent.InternSymbol(sym)
	interned2 := child.InternSymbol(values.NewSymbol("test-sym"))
	c.Assert(interned1, qt.Equals, interned2)
}

// NewChildRuntime

func TestTopLevelEnvironment_NewChildRuntime_Coverage(t *testing.T) {
	c := qt.New(t)
	parent := NewTopLevelEnvironment()

	childEnv := parent.NewChildRuntime()
	c.Assert(childEnv, qt.IsNotNil)
	c.Assert(childEnv.TopLevelEnv(), qt.Equals, parent)

	// Shares interning with parent
	sym := values.NewSymbol("shared-sym")
	interned1 := parent.InternSymbol(sym)
	interned2 := childEnv.InternSymbol(values.NewSymbol("shared-sym"))
	c.Assert(interned1, qt.Equals, interned2)
}

// GlobalEnvironmentFrame.SchemeString — different name

func TestGlobalEnvironmentFrame_SchemeString_Coverage(t *testing.T) {
	c := qt.New(t)
	topLevel := NewTopLevelEnvironment()
	env := topLevel.Runtime()
	c.Assert(env.global.SchemeString(), qt.Equals, "#<global-environment>")
}

// PhaseRegistry.TopLevelEnv

func TestPhaseRegistry_TopLevelEnv_Coverage(t *testing.T) {
	c := qt.New(t)
	topLevel := NewTopLevelEnvironment()
	c.Assert(topLevel.Phases().TopLevelEnv(), qt.Equals, topLevel)
}

// GetLocalIndexWithScopes

func TestGetLocalIndexWithScopes_Coverage(t *testing.T) {
	c := qt.New(t)

	t.Run("nil environment", func(t *testing.T) {
		var env *EnvironmentFrame
		result := env.GetLocalIndexWithScopes(values.NewSymbol("x"), nil)
		c.Assert(result, qt.IsNil)
	})

	t.Run("no local frame", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()
		result := env.GetLocalIndexWithScopes(values.NewSymbol("x"), nil)
		c.Assert(result, qt.IsNil)
	})

	t.Run("finds binding without scopes", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		local := NewLocalEnvironment(0)
		sym := env.InternSymbol(values.NewSymbol("x"))
		local.EnsureLocalBinding(sym, BindingTypeVariable)

		childEnv := NewEnvironmentFrameWithParent(local, env)
		result := childEnv.GetLocalIndexWithScopes(sym, nil)
		c.Assert(result, qt.IsNotNil)
	})

	t.Run("finds binding with matching scopes", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		local := NewLocalEnvironment(0)
		sym := env.InternSymbol(values.NewSymbol("y"))
		scope := syntax.NewScope()

		childEnv := NewEnvironmentFrameWithParent(local, env)
		childEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope})

		result := childEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scope})
		c.Assert(result, qt.IsNotNil)
	})

	t.Run("no match when scopes incompatible", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		local := NewLocalEnvironment(0)
		sym := env.InternSymbol(values.NewSymbol("z"))
		scope1 := syntax.NewScope()
		scope2 := syntax.NewScope()

		childEnv := NewEnvironmentFrameWithParent(local, env)
		childEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope1, scope2})

		// Reference only has scope1, but binding requires both scope1 and scope2
		result := childEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scope1})
		c.Assert(result, qt.IsNil)
	})
}

// GetBindingWithScopes partial coverage improvement

func TestGetBindingWithScopes_GlobalPhase_Coverage(t *testing.T) {
	c := qt.New(t)
	topLevel := NewTopLevelEnvironment()
	env := topLevel.Runtime()

	sym := env.InternSymbol(values.NewSymbol("global-var"))
	env.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
	err := env.SetOwnGlobalValue(NewGlobalIndex(sym), values.NewInteger(42))
	c.Assert(err, qt.IsNil)

	binding := env.GetBindingWithScopes(sym, nil)
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.Value().SchemeString(), qt.Equals, "42")
}

// MaybeCreateLocalBinding

func TestMaybeCreateLocalBinding_Existing_Coverage(t *testing.T) {
	c := qt.New(t)
	topLevel := NewTopLevelEnvironment()
	env := topLevel.Runtime()

	local := NewLocalEnvironment(0)
	sym := env.InternSymbol(values.NewSymbol("dup"))
	childEnv := NewEnvironmentFrameWithParent(local, env)

	idx1, created1 := childEnv.MaybeCreateLocalBinding(sym, BindingTypeVariable)
	c.Assert(created1, qt.IsTrue)
	c.Assert(idx1, qt.IsNotNil)

	idx2, created2 := childEnv.MaybeCreateLocalBinding(sym, BindingTypeVariable)
	c.Assert(created2, qt.IsFalse)
	c.Assert(idx2, qt.IsNotNil)
}

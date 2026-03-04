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
	env := newEnvironmentFrame(local, global)
	c.Assert(env, qt.IsNotNil)
	c.Assert(env.LocalEnvironment(), qt.IsNotNil)
	c.Assert(len(env.LocalEnvironment().Bindings()), qt.Equals, len(local.Bindings()))
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
		childEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope}, nil)

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
		childEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope1, scope2}, nil)

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

// GetLocalIndexWithScopes — maximality algorithm

func TestGetLocalIndexWithScopes_Maximality(t *testing.T) {
	c := qt.New(t)

	t.Run("maximal scope count wins among competing candidates", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		scope1 := syntax.NewScope()
		scope2 := syntax.NewScope()
		scope3 := syntax.NewScope()

		sym := env.InternSymbol(values.NewSymbol("x"))

		// Build 3-level chain: parentEnv ← middleEnv ← innerEnv
		parentEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
		parentEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, nil) // 0 scopes

		middleEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), parentEnv)
		middleEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope1}, nil) // 1 scope

		innerEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), middleEnv)
		innerEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope1, scope2}, nil) // 2 scopes

		// Reference has all 3 scopes — all bindings match, but inner (2 scopes) is maximal
		result := innerEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scope1, scope2, scope3})
		c.Assert(result, qt.IsNotNil)
		// Inner binding is at depth 0 (the frame we call from)
		c.Assert(result[1], qt.Equals, 0, qt.Commentf("should select innermost binding (depth 0)"))
	})

	t.Run("same scope count tie-break favors innermost", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		scopeA := syntax.NewScope()
		scopeB := syntax.NewScope()

		sym := env.InternSymbol(values.NewSymbol("x"))

		// Parent: binding with [scopeA] (1 scope)
		parentEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
		parentEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scopeA}, nil)

		// Child: binding with [scopeB] (1 scope)
		childEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), parentEnv)
		childEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scopeB}, nil)

		// Reference has both — both candidates match with scopeCount=1
		// First candidate collected is depth 0 (child), wins by first-encountered
		result := childEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scopeA, scopeB})
		c.Assert(result, qt.IsNotNil)
		c.Assert(result[1], qt.Equals, 0, qt.Commentf("should select child binding (depth 0) on tie"))
	})

	t.Run("perfect match returns immediately", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		scope1 := syntax.NewScope()
		scope2 := syntax.NewScope()

		sym := env.InternSymbol(values.NewSymbol("x"))

		childEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
		childEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope1, scope2}, nil)

		// Reference exactly matches binding scopes — triggers fast path
		result := childEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scope1, scope2})
		c.Assert(result, qt.IsNotNil)
		c.Assert(result[1], qt.Equals, 0)
	})

	t.Run("non-nested overlapping scope sets resolved by count", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		scopeA := syntax.NewScope()
		scopeB := syntax.NewScope()
		scopeC := syntax.NewScope()
		scopeD := syntax.NewScope()

		sym := env.InternSymbol(values.NewSymbol("x"))

		// Build 3-level chain with overlapping, non-nested scope sets:
		//   outer:  {A, B}     (2 scopes)
		//   middle: {A, C, D}  (3 scopes) — different set, more scopes
		//   inner:  {B}        (1 scope)
		// Reference: {A, B, C, D} — all four match as subsets
		outerEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
		outerEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeB}, nil)

		middleEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), outerEnv)
		middleEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scopeA, scopeC, scopeD}, nil)

		innerEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), middleEnv)
		innerEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scopeB}, nil)

		// Middle binding (3 scopes) should win despite being at depth 1, not depth 0.
		// This is the core maximality property: scope count trumps position.
		result := innerEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scopeA, scopeB, scopeC, scopeD})
		c.Assert(result, qt.IsNotNil)
		c.Assert(result[1], qt.Equals, 1, qt.Commentf("middle binding (3 scopes, depth 1) should beat inner (1 scope, depth 0)"))
	})

	t.Run("scopeless candidate loses to scoped candidate", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		scope1 := syntax.NewScope()

		sym := env.InternSymbol(values.NewSymbol("x"))

		// Inner: no scopes (scopeCount=0), Outer: 1 scope (scopeCount=1)
		outerEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
		outerEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope1}, nil)

		innerEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), outerEnv)
		innerEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, nil, nil)

		// Outer binding (1 scope) should win over inner (0 scopes)
		result := innerEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scope1})
		c.Assert(result, qt.IsNotNil)
		c.Assert(result[1], qt.Equals, 1, qt.Commentf("scoped binding at depth 1 should beat scopeless at depth 0"))
	})

	t.Run("superset binding rejected", func(t *testing.T) {
		topLevel := NewTopLevelEnvironment()
		env := topLevel.Runtime()

		scope1 := syntax.NewScope()
		scope2 := syntax.NewScope()
		scope3 := syntax.NewScope()

		sym := env.InternSymbol(values.NewSymbol("x"))

		childEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
		// Binding has 3 scopes, but reference only has 2
		childEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scope1, scope2, scope3}, nil)

		// Reference is a strict subset of binding scopes — NOT a match
		result := childEnv.GetLocalIndexWithScopes(sym, []*syntax.Scope{scope1, scope2})
		c.Assert(result, qt.IsNil)
	})
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

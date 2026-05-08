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

package registry

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func noopImpl(mc machine.CallContext) error {
	mc.SetValue(values.Void)
	return nil
}

// Apply with runtime primitives

func TestApply_RuntimePrimitive(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "test-prim",
		ParamCount: 0,
		Impl:       noopImpl,
	}, PhaseSetRuntime)

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Verify the primitive is bound in the runtime environment
	sym := values.NewSymbol("test-prim")
	binding := env.GetBinding(sym, nil)
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.Value(), qt.IsNotNil)
}

// Apply with expand-time primitives

func TestApply_ExpandTimePrimitive(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "expand-prim",
		ParamCount: 1,
		Impl:       noopImpl,
	}, PhaseSetExpand)

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Verify the primitive is bound in the expand environment
	expandEnv := env.Expand()
	sym := values.NewSymbol("expand-prim")
	binding := expandEnv.GetBinding(sym, nil)
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.Value(), qt.IsNotNil)
}

// Apply with compile-time bindings

func TestApply_CompileTimeBinding(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddBinding("special-form")

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Verify the binding exists in the compile environment
	compileEnv := env.Compile()
	sym := values.NewSymbol("special-form")
	binding := compileEnv.GetBinding(sym, nil)
	c.Assert(binding, qt.IsNotNil)
}

// Apply with compile-only primitives (PhaseSetCompile without PhaseSetRuntime)

func TestApply_CompileOnlyPrimitive(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "compile-only",
		ParamCount: 0,
		Impl:       noopImpl,
	}, PhaseSetCompile)

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Should have a compile-time binding
	compileEnv := env.Compile()
	sym := values.NewSymbol("compile-only")
	binding := compileEnv.GetBinding(sym, nil)
	c.Assert(binding, qt.IsNotNil)
}

// Apply with init functions

func TestApply_InitFunc(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()

	called := false
	reg.AddInitFunc(func() error {
		called = true
		return nil
	})

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)
	c.Assert(called, qt.IsTrue)
}

// Apply with multi-phase primitive

func TestApply_MultiPhasePrimitive(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "multi-phase",
		ParamCount: 0,
		Impl:       noopImpl,
	}, PhaseSetRuntime|PhaseSetExpand)

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Should exist in both runtime and expand environments
	sym := values.NewSymbol("multi-phase")
	runtimeBinding := env.GetBinding(sym, nil)
	c.Assert(runtimeBinding, qt.IsNotNil)

	expandEnv := env.Expand()
	expandSym := values.NewSymbol("multi-phase")
	expandBinding := expandEnv.GetBinding(expandSym, nil)
	c.Assert(expandBinding, qt.IsNotNil)
}

// Apply on empty registry

func TestApply_EmptyRegistry(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)
}

// ApplyDocs

func TestApplyDocs(t *testing.T) {
	c := qt.New(t)

	reg := NewRegistry()
	reg.AddBindingSpecs([]BindingSpec{
		{Name: "if", Doc: "Conditional expression."},
		{Name: "else"}, // no doc
	})
	reg.AddDocumentation("and", "Short-circuit conjunction.")

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Simulate bootstrap macro: create a binding for "and" in compile phase
	compileEnv := env.Compile()
	compileEnv.MaybeCreateOwnGlobalBinding(
		values.NewSymbol("and"), environment.BindingTypeSyntax,
	)

	reg.ApplyDocs(env)

	tcs := []struct {
		name    string
		sym     string
		wantDoc string
	}{
		{"BindingSpec with doc", "if", "Conditional expression."},
		{"DocEntry on pre-existing binding", "and", "Short-circuit conjunction."},
		{"BindingSpec without doc unchanged", "else", ""},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			bnd := compileEnv.GetBinding(values.NewSymbol(tc.sym), nil)
			c.Assert(bnd, qt.IsNotNil)
			c.Assert(bnd.Doc(), qt.Equals, tc.wantDoc)
		})
	}
}

func TestApplyDocs_MultiPhase(t *testing.T) {
	c := qt.New(t)

	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "multi",
		ParamCount: 0,
		Impl:       noopImpl,
	}, PhaseSetRuntime|PhaseSetExpand)
	reg.AddDocumentation("multi", "Documented across phases.")

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	reg.ApplyDocs(env)

	sym := values.NewSymbol("multi")

	runtimeBnd := env.GetBinding(sym, nil)
	c.Assert(runtimeBnd, qt.IsNotNil)
	c.Assert(runtimeBnd.Doc(), qt.Equals, "Documented across phases.")

	expandBnd := env.Expand().GetBinding(sym, nil)
	c.Assert(expandBnd, qt.IsNotNil)
	c.Assert(expandBnd.Doc(), qt.Equals, "Documented across phases.")
}

func TestApplyDocs_NonexistentBinding(t *testing.T) {
	c := qt.New(t)

	reg := NewRegistry()
	reg.AddDocumentation("nonexistent", "Should be silently skipped.")

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Must not panic
	reg.ApplyDocs(env)
	c.Assert(true, qt.IsTrue)
}

// TestApply_ContractEnforcement verifies that WithContractEnforcement
// installs a validator on each primitive that declares ParamTypes, and
// leaves ForeignClosure.validate nil otherwise. This covers both the
// enforcement-on and enforcement-off branches of registerRuntimePrimitive.
func TestApply_ContractEnforcement(t *testing.T) {
	spec := PrimitiveSpec{
		Name:       "test-enforced",
		ParamCount: 1,
		Impl:       noopImpl,
		ParamTypes: []values.TypeConstraint{values.TypeString},
	}

	tcs := []struct {
		name          string
		opts          []ApplyOption
		wantValidator bool
	}{
		{"enforcement off by default", nil, false},
		{"enforcement on", []ApplyOption{WithContractEnforcement()}, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			reg := NewRegistry()
			reg.AddPrimitive(spec, PhaseSetRuntime)

			topLevel := environment.NewNamespace()
			env := topLevel.Runtime()
			err := reg.Apply(context.Background(), env, tc.opts...)
			c.Assert(err, qt.IsNil)

			sym := values.NewSymbol("test-enforced")
			binding := env.GetBinding(sym, nil)
			c.Assert(binding, qt.IsNotNil)

			fcls, ok := binding.Value().(*machine.ForeignClosure)
			c.Assert(ok, qt.IsTrue)
			if tc.wantValidator {
				c.Assert(fcls.Validator(), qt.IsNotNil)
			} else {
				c.Assert(fcls.Validator(), qt.IsNil)
			}
		})
	}
}

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

	"github.com/aalpar/wile/go/environment"
	"github.com/aalpar/wile/go/machine"
	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

func noopImpl(_ context.Context, mc *machine.MachineContext) error {
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
	}, PhaseRuntime)

	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Verify the primitive is bound in the runtime environment
	sym := env.InternSymbol(values.NewSymbol("test-prim"))
	binding := env.GetBinding(sym)
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
	}, PhaseExpand)

	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Verify the primitive is bound in the expand environment
	expandEnv := env.Expand()
	sym := expandEnv.InternSymbol(values.NewSymbol("expand-prim"))
	binding := expandEnv.GetBinding(sym)
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.Value(), qt.IsNotNil)
}

// Apply with compile-time bindings

func TestApply_CompileTimeBinding(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddBinding("special-form")

	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Verify the binding exists in the compile environment
	compileEnv := env.Compile()
	sym := compileEnv.InternSymbol(values.NewSymbol("special-form"))
	binding := compileEnv.GetBinding(sym)
	c.Assert(binding, qt.IsNotNil)
}

// Apply with compile-only primitives (PhaseCompile without PhaseRuntime)

func TestApply_CompileOnlyPrimitive(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "compile-only",
		ParamCount: 0,
		Impl:       noopImpl,
	}, PhaseCompile)

	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Should have a compile-time binding
	compileEnv := env.Compile()
	sym := compileEnv.InternSymbol(values.NewSymbol("compile-only"))
	binding := compileEnv.GetBinding(sym)
	c.Assert(binding, qt.IsNotNil)
}

// Apply with init functions

func TestApply_InitFunc(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()

	var calledCtx context.Context
	var calledEnv *environment.EnvironmentFrame
	reg.AddInitFunc(func(actx ApplyContext) error {
		calledCtx = actx.Context()
		calledEnv = actx.Environment()
		return nil
	})

	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()
	ctx := context.Background()
	err := reg.Apply(ctx, env)
	c.Assert(err, qt.IsNil)
	c.Assert(calledCtx, qt.Equals, ctx)
	c.Assert(calledEnv, qt.Equals, env)
}

// Apply with multi-phase primitive

func TestApply_MultiPhasePrimitive(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "multi-phase",
		ParamCount: 0,
		Impl:       noopImpl,
	}, PhaseRuntime|PhaseExpand)

	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	// Should exist in both runtime and expand environments
	sym := env.InternSymbol(values.NewSymbol("multi-phase"))
	runtimeBinding := env.GetBinding(sym)
	c.Assert(runtimeBinding, qt.IsNotNil)

	expandEnv := env.Expand()
	expandSym := expandEnv.InternSymbol(values.NewSymbol("multi-phase"))
	expandBinding := expandEnv.GetBinding(expandSym)
	c.Assert(expandBinding, qt.IsNotNil)
}

// Apply on empty registry

func TestApply_EmptyRegistry(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)
}

// applyContext methods

func TestApplyContext_Methods(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()

	actx := &applyContext{ctx: ctx, env: env}
	c.Assert(actx.Context(), qt.Equals, ctx)
	c.Assert(actx.Environment(), qt.Equals, env)
}

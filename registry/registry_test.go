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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/machine"
)

func TestRegistry_NewRegistry(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	c.Assert(r, qt.IsNotNil)
	c.Assert(r.PrimitiveCount(), qt.Equals, 0)
	c.Assert(r.BindingCount(), qt.Equals, 0)
}

func TestRegistry_AddPrimitive(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	spec := PrimitiveSpec{
		Name:       "test-prim",
		ParamCount: 1,
		IsVariadic: false,
		Impl:       func(_ context.Context, _ *machine.MachineContext) error { return nil },
	}

	r.AddPrimitive(spec, PhaseRuntime)
	c.Assert(r.PrimitiveCount(), qt.Equals, 1)

	prims := r.Primitives()
	c.Assert(len(prims), qt.Equals, 1)
	c.Assert(prims[0].Spec.Name, qt.Equals, "test-prim")
	c.Assert(prims[0].Phases, qt.Equals, PhaseRuntime)
}

func TestRegistry_AddPrimitives(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	specs := []PrimitiveSpec{
		{"prim1", 1, false, nil},
		{"prim2", 2, true, nil},
	}

	r.AddPrimitives(specs, PhaseRuntime|PhaseExpand)
	c.Assert(r.PrimitiveCount(), qt.Equals, 2)

	prims := r.Primitives()
	c.Assert(prims[0].Spec.Name, qt.Equals, "prim1")
	c.Assert(prims[1].Spec.Name, qt.Equals, "prim2")
	c.Assert(prims[0].Phases, qt.Equals, PhaseRuntime|PhaseExpand)
}

func TestRegistry_AddBinding(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddBinding("if")
	c.Assert(r.BindingCount(), qt.Equals, 1)

	bindings := r.Bindings()
	c.Assert(len(bindings), qt.Equals, 1)
	c.Assert(bindings[0], qt.Equals, "if")
}

func TestRegistry_AddBindings(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddBindings([]string{"if", "lambda", "define"})
	c.Assert(r.BindingCount(), qt.Equals, 3)

	bindings := r.Bindings()
	c.Assert(bindings, qt.DeepEquals, []string{"if", "lambda", "define"})
}

func TestRegistry_AddMacroSource(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddMacroSource("(define-syntax and ...)")
	r.AddMacroSource("(define-syntax or ...)")

	sources := r.MacroSources()
	c.Assert(len(sources), qt.Equals, 2)
	c.Assert(sources[0], qt.Equals, "(define-syntax and ...)")
}

func TestRegistry_AddInitFunc(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddInitFunc(func(_ ApplyContext) error {
		return nil
	})

	funcs := r.InitFuncs()
	c.Assert(len(funcs), qt.Equals, 1)
}

func TestRegistry_Clone(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitive(PrimitiveSpec{Name: "test", ParamCount: 1}, PhaseRuntime)
	r.AddBinding("if")
	r.AddMacroSource("source")

	clone := r.Clone()
	c.Assert(clone.PrimitiveCount(), qt.Equals, 1)
	c.Assert(clone.BindingCount(), qt.Equals, 1)
	c.Assert(len(clone.MacroSources()), qt.Equals, 1)

	// Ensure clone is independent
	r.AddBinding("lambda")
	c.Assert(r.BindingCount(), qt.Equals, 2)
	c.Assert(clone.BindingCount(), qt.Equals, 1)
}

func TestPhase_String(t *testing.T) {
	tests := []struct {
		phase Phase
		want  string
	}{
		{PhaseRuntime, "runtime"},
		{PhaseExpand, "expand"},
		{PhaseCompile, "compile"},
		{PhaseRuntime | PhaseExpand, "runtime|expand"},
		{PhaseRuntime | PhaseExpand | PhaseCompile, "runtime|expand|compile"},
		{0, "none"},
	}

	c := qt.New(t)
	for _, tt := range tests {
		c.Run(tt.want, func(c *qt.C) {
			c.Assert(tt.phase.String(), qt.Equals, tt.want)
		})
	}
}

func TestPhase_Has(t *testing.T) {
	c := qt.New(t)

	phase := PhaseRuntime | PhaseExpand
	c.Assert(phase.HasRuntime(), qt.IsTrue)
	c.Assert(phase.HasExpand(), qt.IsTrue)
	c.Assert(phase.HasCompile(), qt.IsFalse)
}

func TestRegistryBuilder_AddToRegistry(t *testing.T) {
	c := qt.New(t)

	builder := NewRegistryBuilder(
		func(r *Registry) error {
			r.AddPrimitive(PrimitiveSpec{Name: "prim1"}, PhaseRuntime)
			return nil
		},
		func(r *Registry) error {
			r.AddPrimitive(PrimitiveSpec{Name: "prim2"}, PhaseExpand)
			return nil
		},
	)

	r := NewRegistry()
	err := builder.AddToRegistry(r)
	c.Assert(err, qt.IsNil)
	c.Assert(r.PrimitiveCount(), qt.Equals, 2)
}

func TestRegistryBuilder_Build(t *testing.T) {
	c := qt.New(t)

	builder := NewRegistryBuilder(
		func(r *Registry) error {
			r.AddBinding("test")
			return nil
		},
	)

	r, err := builder.Build()
	c.Assert(err, qt.IsNil)
	c.Assert(r.BindingCount(), qt.Equals, 1)
}

func TestExtension(t *testing.T) {
	c := qt.New(t)

	ext := NewExtension("test-ext", func(r *Registry) error {
		r.AddPrimitive(PrimitiveSpec{Name: "ext-prim"}, PhaseRuntime)
		return nil
	})

	c.Assert(ext.Name(), qt.Equals, "test-ext")

	r := NewRegistry()
	err := ext.AddToRegistry(r)
	c.Assert(err, qt.IsNil)
	c.Assert(r.PrimitiveCount(), qt.Equals, 1)
}

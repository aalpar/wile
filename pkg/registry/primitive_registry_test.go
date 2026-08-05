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
	"errors"
	"reflect"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// testValue creates a simple string value for testing.
func testValue(s string) values.Value {
	return values.NewString(s)
}

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
		Impl:       func(_ machine.CallContext) error { return nil },
	}

	r.AddPrimitive(spec, PhaseSetRuntime)
	c.Assert(r.PrimitiveCount(), qt.Equals, 1)

	prims := r.Primitives()
	c.Assert(len(prims), qt.Equals, 1)
	c.Assert(prims[0].Spec.Name, qt.Equals, "test-prim")
	c.Assert(prims[0].Phases, qt.Equals, PhaseSetRuntime)
}

func TestRegistry_AddPrimitives(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	specs := []PrimitiveSpec{
		{Name: "prim1", ParamCount: 1, Impl: noopImpl},
		{Name: "prim2", ParamCount: 2, IsVariadic: true, Impl: noopImpl},
	}

	r.AddPrimitives(specs, PhaseSetRuntime|PhaseSetExpand)
	c.Assert(r.PrimitiveCount(), qt.Equals, 2)

	prims := r.Primitives()
	c.Assert(prims[0].Spec.Name, qt.Equals, "prim1")
	c.Assert(prims[1].Spec.Name, qt.Equals, "prim2")
	c.Assert(prims[0].Phases, qt.Equals, PhaseSetRuntime|PhaseSetExpand)
}

func TestRegistry_FindPrimitive(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitive(PrimitiveSpec{Name: "runtime-only", ParamCount: 1, Impl: noopImpl}, PhaseSetRuntime)
	r.AddPrimitive(PrimitiveSpec{Name: "expand-only", ParamCount: 0, Impl: noopImpl}, PhaseSetExpand)
	r.AddPrimitive(PrimitiveSpec{Name: "both", ParamCount: 2, Impl: noopImpl}, PhaseSetRuntime|PhaseSetExpand)

	tcs := []struct {
		name  string
		prim  string
		phase PhaseSet
		found bool
	}{
		{"runtime by name zero phase", "runtime-only", 0, true},
		{"runtime in runtime phase", "runtime-only", PhaseSetRuntime, true},
		{"runtime not in expand phase", "runtime-only", PhaseSetExpand, false},
		{"expand in expand phase", "expand-only", PhaseSetExpand, true},
		{"expand not in runtime phase", "expand-only", PhaseSetRuntime, false},
		{"both in runtime phase", "both", PhaseSetRuntime, true},
		{"both in expand phase", "both", PhaseSetExpand, true},
		{"nonexistent", "no-such-prim", 0, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			reg, ok := r.FindPrimitive(tc.prim, tc.phase)
			c.Assert(ok, qt.Equals, tc.found)
			if tc.found {
				c.Assert(reg.Spec.Name, qt.Equals, tc.prim)
			}
		})
	}
}

func TestRegistry_HasPrimitive(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitive(PrimitiveSpec{Name: "test-prim", Impl: noopImpl}, PhaseSetRuntime)

	c.Assert(r.HasPrimitive("test-prim", 0), qt.IsTrue)
	c.Assert(r.HasPrimitive("test-prim", PhaseSetRuntime), qt.IsTrue)
	c.Assert(r.HasPrimitive("test-prim", PhaseSetExpand), qt.IsFalse)
	c.Assert(r.HasPrimitive("no-such", 0), qt.IsFalse)
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
	r.AddInitFunc(func() error {
		return nil
	})

	funcs := r.InitFuncs()
	c.Assert(len(funcs), qt.Equals, 1)
}

func TestRegistry_Clone(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitive(PrimitiveSpec{Name: "test", ParamCount: 1, Impl: noopImpl}, PhaseSetRuntime)
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

func TestPhaseSet_String(t *testing.T) {
	tests := []struct {
		phase PhaseSet
		want  string
	}{
		{PhaseSetRuntime, "runtime"},
		{PhaseSetExpand, "expand"},
		{PhaseSetCompile, "compile"},
		{PhaseSetRuntime | PhaseSetExpand, "runtime|expand"},
		{PhaseSetRuntime | PhaseSetExpand | PhaseSetCompile, "runtime|expand|compile"},
		{0, "none"},
	}

	c := qt.New(t)
	for _, tt := range tests {
		c.Run(tt.want, func(c *qt.C) {
			c.Assert(tt.phase.String(), qt.Equals, tt.want)
		})
	}
}

func TestPhaseSet_Has(t *testing.T) {
	c := qt.New(t)

	phase := PhaseSetRuntime | PhaseSetExpand
	c.Assert(phase.Has(environment.PhaseRuntime), qt.IsTrue)
	c.Assert(phase.Has(environment.PhaseExpand), qt.IsTrue)
	c.Assert(phase.Has(environment.PhaseCompile), qt.IsFalse)
	c.Assert(phase.Has(environment.PhaseTemplate), qt.IsFalse)
}

func TestPhaseSet_With(t *testing.T) {
	c := qt.New(t)

	s := PhaseSet(0)
	s = s.With(environment.PhaseRuntime)
	s = s.With(environment.PhaseExpand)
	c.Assert(s, qt.Equals, PhaseSetRuntime|PhaseSetExpand)

	// With is idempotent.
	s = s.With(environment.PhaseRuntime)
	c.Assert(s, qt.Equals, PhaseSetRuntime|PhaseSetExpand)

	// PhaseTemplate cannot be added.
	c.Assert(func() {
		_ = s.With(environment.PhaseTemplate)
	}, qt.PanicMatches, ".*phase -1 not representable.*")
}

func TestRegistryBuilder_AddToRegistry(t *testing.T) {
	c := qt.New(t)

	builder := NewRegistryBuilder(
		func(r *PrimitiveRegistry) error {
			r.AddPrimitive(PrimitiveSpec{Name: "prim1", Impl: noopImpl}, PhaseSetRuntime)
			return nil
		},
		func(r *PrimitiveRegistry) error {
			r.AddPrimitive(PrimitiveSpec{Name: "prim2", Impl: noopImpl}, PhaseSetExpand)
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
		func(r *PrimitiveRegistry) error {
			r.AddBinding("test")
			return nil
		},
	)

	r, err := builder.Build()
	c.Assert(err, qt.IsNil)
	c.Assert(r.BindingCount(), qt.Equals, 1)
}

func TestRegistry_VariadicParamCountZeroPanics(t *testing.T) {
	c := qt.New(t)

	// A variadic primitive's rest parameter occupies slot paramCount-1, so
	// ParamCount must be >= 1. {ParamCount:0, IsVariadic:true} would make
	// bindArgs index bnds[:-1] and panic on first call; the registration
	// chokepoint must reject it up front instead.
	r := NewRegistry()
	c.Assert(func() {
		r.AddPrimitives([]PrimitiveSpec{
			{Name: "bad-variadic", ParamCount: 0, IsVariadic: true, Impl: noopImpl},
		}, PhaseSetRuntime)
	}, qt.PanicMatches, ".*variadic.*ParamCount.*")
}

func TestRegistry_PrimitiveByName(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "car", ParamCount: 1, Impl: noopImpl, Doc: "Returns the car.", ParamNames: []string{"pair"}, Category: "pairs"},
		{Name: "cdr", ParamCount: 1, Impl: noopImpl, Doc: "Returns the cdr.", ParamNames: []string{"pair"}, Category: "pairs"},
		{Name: "+", ParamCount: 1, IsVariadic: true, Impl: noopImpl, Doc: "Returns the sum.", ParamNames: []string{"z"}, Category: "arithmetic"},
	}, PhaseSetRuntime)

	tcs := []struct {
		name  string
		query string
		found bool
		doc   string
		cat   string
	}{
		{"existing", "car", true, "Returns the car.", "pairs"},
		{"existing variadic", "+", true, "Returns the sum.", "arithmetic"},
		{"missing", "nonexistent", false, "", ""},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			reg, ok := r.PrimitiveByName(tc.query)
			c.Assert(ok, qt.Equals, tc.found)
			if tc.found {
				c.Assert(reg.Spec.Doc, qt.Equals, tc.doc)
				c.Assert(reg.Spec.Category, qt.Equals, tc.cat)
			}
		})
	}
}

func TestRegistry_PrimitiveNames(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "alpha", Impl: noopImpl},
		{Name: "beta", ParamCount: 1, Impl: noopImpl},
		{Name: "gamma", ParamCount: 2, Impl: noopImpl},
	}, PhaseSetRuntime)

	names := r.PrimitiveNames()
	c.Assert(names, qt.DeepEquals, []string{"alpha", "beta", "gamma"})
}

func TestRegistry_PrimitivesByCategory(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "car", ParamCount: 1, Impl: noopImpl, Category: "pairs"},
		{Name: "cdr", ParamCount: 1, Impl: noopImpl, Category: "pairs"},
		{Name: "+", Impl: noopImpl, Category: "arithmetic"},
		{Name: "display", ParamCount: 1, Impl: noopImpl},
	}, PhaseSetRuntime)

	byCategory := r.PrimitivesByCategory()

	// pairs category has 2 entries
	c.Assert(len(byCategory["pairs"]), qt.Equals, 2)
	c.Assert(byCategory["pairs"][0].Spec.Name, qt.Equals, "car")
	c.Assert(byCategory["pairs"][1].Spec.Name, qt.Equals, "cdr")

	// arithmetic category has 1 entry
	c.Assert(len(byCategory["arithmetic"]), qt.Equals, 1)
	c.Assert(byCategory["arithmetic"][0].Spec.Name, qt.Equals, "+")

	// no-category primitives are under empty string
	c.Assert(len(byCategory[""]), qt.Equals, 1)
	c.Assert(byCategory[""][0].Spec.Name, qt.Equals, "display")
}

func TestRegistry_ClonePreservesMetadata(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitive(PrimitiveSpec{
		Name: "test", ParamCount: 1, Impl: noopImpl,
		Doc: "A test prim.", ParamNames: []string{"x"}, Category: "testing",
	}, PhaseSetRuntime)

	clone := r.Clone()
	reg, ok := clone.PrimitiveByName("test")
	c.Assert(ok, qt.IsTrue)
	c.Assert(reg.Spec.Doc, qt.Equals, "A test prim.")
	c.Assert(reg.Spec.ParamNames, qt.DeepEquals, []string{"x"})
	c.Assert(reg.Spec.Category, qt.Equals, "testing")
}

func TestRegistry_AddGlobalValue(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddGlobalValue("my-var", testValue("hello"))
	r.AddGlobalValue("other-var", testValue("world"))

	gvs := r.GlobalValues()
	c.Assert(len(gvs), qt.Equals, 2)
	c.Assert(gvs[0].Name, qt.Equals, "my-var")
	c.Assert(gvs[1].Name, qt.Equals, "other-var")
}

func TestRegistry_GlobalValues_DefensiveCopy(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddGlobalValue("a", testValue("x"))

	gvs := r.GlobalValues()
	c.Assert(len(gvs), qt.Equals, 1)

	// Mutating the returned slice should not affect the registry.
	gvs[0].Name = "mutated"
	gvs2 := r.GlobalValues()
	c.Assert(gvs2[0].Name, qt.Equals, "a")
}

func TestRegistry_CloneGlobalValues(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddGlobalValue("x", testValue("original"))

	clone := r.Clone()
	c.Assert(len(clone.GlobalValues()), qt.Equals, 1)
	c.Assert(clone.GlobalValues()[0].Name, qt.Equals, "x")

	// Adding to original should not affect clone.
	r.AddGlobalValue("y", testValue("extra"))
	c.Assert(len(r.GlobalValues()), qt.Equals, 2)
	c.Assert(len(clone.GlobalValues()), qt.Equals, 1)
}

func TestRegistry_Without(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "car", Category: "pairs", Impl: noopImpl},
		{Name: "cdr", Category: "pairs", Impl: noopImpl},
		{Name: "set-car!", Category: "pairs", Impl: noopImpl},
		{Name: "+", Category: "arithmetic", Impl: noopImpl},
		{Name: "vector-set!", Category: "vectors", Impl: noopImpl},
	}, PhaseSetRuntime)
	r.AddBinding("if")
	r.AddDocumentation("if", "Conditional.")
	r.AddInitFunc(func() error { return nil })
	r.AddMacroSource("(define-syntax and ...)")
	r.AddGlobalValue("gv", testValue("x"))

	tcs := []struct {
		name    string
		exclude []string
		want    []string
	}{
		{"remove one", []string{"set-car!"}, []string{"car", "cdr", "+", "vector-set!"}},
		{"remove multiple", []string{"set-car!", "vector-set!"}, []string{"car", "cdr", "+"}},
		{"remove nonexistent silently", []string{"nonexistent"}, []string{"car", "cdr", "set-car!", "+", "vector-set!"}},
		{"remove nothing", nil, []string{"car", "cdr", "set-car!", "+", "vector-set!"}},
		{"remove all", []string{"car", "cdr", "set-car!", "+", "vector-set!"}, []string{}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			filtered := r.Without(tc.exclude...)
			c.Assert(filtered.PrimitiveNames(), qt.DeepEquals, tc.want)
			// Non-primitive fields are copied unchanged (docstring contract).
			c.Assert(filtered.Bindings(), qt.DeepEquals, []string{"if"})
			c.Assert(len(filtered.Docs()), qt.Equals, 1)
			c.Assert(len(filtered.InitFuncs()), qt.Equals, 1)
			c.Assert(len(filtered.MacroSources()), qt.Equals, 1)
			c.Assert(len(filtered.GlobalValues()), qt.Equals, 1)
		})
	}

	// Original is unmodified.
	c.Assert(r.PrimitiveCount(), qt.Equals, 5)
}

func TestRegistry_WithoutCategory(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "car", Category: "pairs", Impl: noopImpl},
		{Name: "cdr", Category: "pairs", Impl: noopImpl},
		{Name: "+", Category: "arithmetic", Impl: noopImpl},
		{Name: "display", Category: "io", Impl: noopImpl},
		{Name: "uncategorized", Impl: noopImpl},
	}, PhaseSetRuntime)
	r.AddBinding("lambda")
	r.AddDocumentation("lambda", "Procedure.")
	r.AddInitFunc(func() error { return nil })
	r.AddMacroSource("(define-syntax when ...)")
	r.AddGlobalValue("gv", testValue("y"))

	tcs := []struct {
		name    string
		exclude []string
		want    []string
	}{
		{"remove one category", []string{"pairs"}, []string{"+", "display", "uncategorized"}},
		{"remove multiple categories", []string{"pairs", "io"}, []string{"+", "uncategorized"}},
		{"remove nonexistent category", []string{"nonexistent"}, []string{"car", "cdr", "+", "display", "uncategorized"}},
		{"remove empty-string category", []string{""}, []string{"car", "cdr", "+", "display"}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			filtered := r.WithoutCategory(tc.exclude...)
			c.Assert(filtered.PrimitiveNames(), qt.DeepEquals, tc.want)
			// Non-primitive fields are copied unchanged (docstring contract).
			c.Assert(filtered.Bindings(), qt.DeepEquals, []string{"lambda"})
			c.Assert(len(filtered.Docs()), qt.Equals, 1)
			c.Assert(len(filtered.InitFuncs()), qt.Equals, 1)
			c.Assert(len(filtered.MacroSources()), qt.Equals, 1)
			c.Assert(len(filtered.GlobalValues()), qt.Equals, 1)
		})
	}

	// Original is unmodified.
	c.Assert(r.PrimitiveCount(), qt.Equals, 5)
}

func TestRegistry_WithoutBindings(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "set!", Category: "special", Impl: noopImpl},
		{Name: "+", Category: "arithmetic", Impl: noopImpl},
	}, PhaseSetRuntime)
	r.AddBindings([]string{"if", "set!", "lambda", "define"})
	r.AddDocumentation("set!", "Mutation.")
	r.AddInitFunc(func() error { return nil })
	r.AddMacroSource("(define-syntax unless ...)")
	r.AddGlobalValue("gv", testValue("z"))

	tcs := []struct {
		name    string
		exclude []string
		want    []string
	}{
		{"remove one binding", []string{"set!"}, []string{"if", "lambda", "define"}},
		{"remove multiple", []string{"set!", "if"}, []string{"lambda", "define"}},
		{"remove nonexistent", []string{"nonexistent"}, []string{"if", "set!", "lambda", "define"}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			filtered := r.WithoutBindings(tc.exclude...)
			c.Assert(filtered.Bindings(), qt.DeepEquals, tc.want)
			// Primitives, init funcs, macro sources, and global values
			// are copied unchanged (docstring contract).
			c.Assert(filtered.PrimitiveCount(), qt.Equals, 2)
			c.Assert(len(filtered.Docs()), qt.Equals, 1)
			c.Assert(len(filtered.InitFuncs()), qt.Equals, 1)
			c.Assert(len(filtered.MacroSources()), qt.Equals, 1)
			c.Assert(len(filtered.GlobalValues()), qt.Equals, 1)
		})
	}
}

func TestRegistry_PrimitiveSpecWithContract(t *testing.T) {
	c := qt.New(t)
	r := NewRegistry()
	spec := PrimitiveSpec{
		Name:       "test-contracted",
		ParamCount: 2,
		Impl: func(_ machine.CallContext) error {
			return nil
		},
		ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeInteger},
		ReturnType: values.TypeCharacter,
		Doc:        "Test primitive with contract.",
		ParamNames: []string{"s", "k"},
		Category:   "test",
	}
	r.AddPrimitive(spec, PhaseSetRuntime)
	prims := r.Primitives()
	c.Assert(len(prims), qt.Equals, 1)
	c.Assert(prims[0].Spec.ParamTypes, qt.HasLen, 2)
	c.Assert(prims[0].Spec.ParamTypes[0], qt.Equals, values.TypeString)
	c.Assert(prims[0].Spec.ParamTypes[1], qt.Equals, values.TypeInteger)
	c.Assert(prims[0].Spec.ReturnType, qt.Equals, values.TypeCharacter)
}

func TestRegistry_AddBindingSpecs(t *testing.T) {
	c := qt.New(t)
	r := NewRegistry()
	r.AddBindingSpecs([]BindingSpec{
		{Name: "if", Doc: "Conditional expression."},
		{Name: "lambda", Doc: "Anonymous procedure."},
		{Name: "else"},
	})
	c.Assert(r.BindingCount(), qt.Equals, 3)
	bindings := r.Bindings()
	c.Assert(bindings, qt.DeepEquals, []string{"if", "lambda", "else"})
	specs := r.BindingSpecs()
	c.Assert(specs, qt.HasLen, 3)
	c.Assert(specs[0].Name, qt.Equals, "if")
	c.Assert(specs[0].Doc, qt.Equals, "Conditional expression.")
	c.Assert(specs[2].Doc, qt.Equals, "")
}

func TestRegistry_AddBindings_BackwardCompat(t *testing.T) {
	c := qt.New(t)
	r := NewRegistry()
	r.AddBindings([]string{"if", "lambda"})
	specs := r.BindingSpecs()
	c.Assert(specs, qt.HasLen, 2)
	c.Assert(specs[0].Name, qt.Equals, "if")
	c.Assert(specs[0].Doc, qt.Equals, "")
}

func TestRegistry_AddDocumentation(t *testing.T) {
	c := qt.New(t)
	r := NewRegistry()
	r.AddDocumentation("and", "Short-circuit conjunction.")
	r.AddDocumentation("or", "Short-circuit disjunction.")
	docs := r.Docs()
	c.Assert(docs, qt.HasLen, 2)
	c.Assert(docs[0].Name, qt.Equals, "and")
	c.Assert(docs[0].Doc, qt.Equals, "Short-circuit conjunction.")
}

func TestRegistry_Clone_IncludesDocs(t *testing.T) {
	c := qt.New(t)
	r := NewRegistry()
	r.AddDocumentation("and", "Short-circuit conjunction.")
	r.AddBindingSpecs([]BindingSpec{{Name: "if", Doc: "Conditional."}})
	r2 := r.Clone()
	// Post-Phase-1: BindingSpecs() returns both real bindings and DocOnly
	// entries (they live in the same slice). Docs() filters to DocOnly=true.
	c.Assert(r2.BindingSpecs(), qt.HasLen, 2) // "and" (DocOnly) + "if" (real binding)
	c.Assert(r2.Docs(), qt.HasLen, 1)         // only "and"
	r2.AddDocumentation("or", "Disjunction.")
	c.Assert(r.Docs(), qt.HasLen, 1) // original unaffected
	c.Assert(r2.Docs(), qt.HasLen, 2)
}

func TestRegistry_AddDocOnlyPrimitive(t *testing.T) {
	c := qt.New(t)

	// Post-Phase-1: AddDocOnlyPrimitive routes into the dedicated
	// docPrimitives slice (per Q-b of plans/2026-05-18-registry-structural-reduction.md
	// — "separate tier for documentation"). The full PrimitiveSpec metadata
	// is preserved. FindPrimitive / PrimitiveByName fall back to docPrimitives
	// when no real primitive with that name exists, so existing callers
	// (and the import-registers-docstrings tests in the root package) keep
	// working transparently.
	tcs := []struct {
		name           string
		setup          func(r *PrimitiveRegistry)
		queryName      string
		wantInDocPrims bool     // expect to find via DocPrimitives()
		wantFound      bool     // expect FindPrimitive / PrimitiveByName to succeed
		wantPhases     PhaseSet // expected Phases on the found PrimitiveRegistration
		wantDocText    string   // doc text expected on the found entry
		wantCategory   string   // category expected on the found entry (metadata preservation)
	}{
		{
			name: "doc-only entry lands in DocPrimitives with full metadata",
			setup: func(r *PrimitiveRegistry) {
				r.AddDocOnlyPrimitive(PrimitiveSpec{
					Name:     "map",
					Doc:      "Apply proc to each element.",
					Category: "lists",
				})
			},
			queryName:      "map",
			wantInDocPrims: true,
			wantFound:      true,
			wantPhases:     0,
			wantDocText:    "Apply proc to each element.",
			wantCategory:   "lists",
		},
		{
			name: "Go primitive takes precedence; doc-only entry is skipped at registration",
			setup: func(r *PrimitiveRegistry) {
				r.AddPrimitive(PrimitiveSpec{
					Name: "car",
					Doc:  "Go car.",
					Impl: func(_ machine.CallContext) error { return nil },
				}, PhaseSetRuntime)
				r.AddDocOnlyPrimitive(PrimitiveSpec{
					Name: "car",
					Doc:  "Scheme car.",
				})
			},
			queryName:      "car",
			wantInDocPrims: false, // skipped because primitive with same name exists
			wantFound:      true,
			wantPhases:     PhaseSetRuntime,
			wantDocText:    "Go car.",
		},
		{
			name: "doc-only entry has zero phases (fallback lookup)",
			setup: func(r *PrimitiveRegistry) {
				r.AddDocOnlyPrimitive(PrimitiveSpec{
					Name:       "for-each",
					Doc:        "Apply proc for side effects.",
					ParamCount: 2,
				})
			},
			queryName:      "for-each",
			wantInDocPrims: true,
			wantFound:      true,
			wantPhases:     0,
			wantDocText:    "Apply proc for side effects.",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			r := NewRegistry()
			tc.setup(r)

			// Check fallback lookup via PrimitiveByName.
			reg, found := r.PrimitiveByName(tc.queryName)
			c.Assert(found, qt.Equals, tc.wantFound)
			if tc.wantFound {
				c.Assert(reg.Phases, qt.Equals, tc.wantPhases)
				c.Assert(reg.Spec.Doc, qt.Equals, tc.wantDocText)
			}

			// Check dedicated DocPrimitives() tier.
			var inDocPrims bool
			for _, dp := range r.DocPrimitives() {
				if dp.Name == tc.queryName {
					inDocPrims = true
					c.Assert(dp.Doc, qt.Equals, tc.wantDocText)
					if tc.wantCategory != "" {
						c.Assert(dp.Category, qt.Equals, tc.wantCategory)
					}
					break
				}
			}
			c.Assert(inDocPrims, qt.Equals, tc.wantInDocPrims)
		})
	}
}

func TestExtension(t *testing.T) {
	c := qt.New(t)

	ext := NewExtension("test-ext", func(r *PrimitiveRegistry) error {
		r.AddPrimitive(PrimitiveSpec{Name: "ext-prim", Impl: noopImpl}, PhaseSetRuntime)
		return nil
	})

	c.Assert(ext.Name(), qt.Equals, "test-ext")

	r := NewRegistry()
	err := ext.AddToRegistry(r)
	c.Assert(err, qt.IsNil)
	c.Assert(r.PrimitiveCount(), qt.Equals, 1)
}

func TestExtensionOptions(t *testing.T) {
	noop := func(*PrimitiveRegistry) error {
		return nil
	}

	tcs := []struct {
		name           string
		opts           []ExtensionOption
		wantDesc       string
		wantLibName    []string
		wantHasCloseFn bool
	}{
		{
			name: "no options",
		},
		{
			name:     "with description",
			opts:     []ExtensionOption{WithDescription("a description")},
			wantDesc: "a description",
		},
		{
			name:        "with library name",
			opts:        []ExtensionOption{WithLibraryName("scheme", "base")},
			wantLibName: []string{"scheme", "base"},
		},
		{
			name: "with close",
			opts: []ExtensionOption{WithClose(func() error {
				return nil
			})},
			wantHasCloseFn: true,
		},
		{
			name: "all options",
			opts: []ExtensionOption{
				WithDescription("everything"),
				WithLibraryName("my", "ext"),
				WithClose(func() error {
					return nil
				}),
			},
			wantDesc:       "everything",
			wantLibName:    []string{"my", "ext"},
			wantHasCloseFn: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)

			ext := NewExtension("t", noop, tc.opts...)
			c.Assert(ext.Name(), qt.Equals, "t")

			describer, ok := ext.(Describer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(describer.Description(), qt.Equals, tc.wantDesc)

			namer, ok := ext.(LibraryNamer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(namer.LibraryName(), qt.DeepEquals, tc.wantLibName)

			closer, ok := ext.(Closeable)
			c.Assert(ok, qt.IsTrue)
			c.Assert(closer.Close(), qt.IsNil)

			ef, ok := ext.(*ExtensionFunc)
			c.Assert(ok, qt.IsTrue)
			c.Assert(ef.closeFn != nil, qt.Equals, tc.wantHasCloseFn)
		})
	}
}

func TestExtensionWithCloseInvokesFn(t *testing.T) {
	c := qt.New(t)

	var called int
	ext := NewExtension("t", func(*PrimitiveRegistry) error {
		return nil
	}, WithClose(func() error {
		called++
		return nil
	}))

	closer, ok := ext.(Closeable)
	c.Assert(ok, qt.IsTrue)
	c.Assert(closer.Close(), qt.IsNil)
	c.Assert(called, qt.Equals, 1)
	c.Assert(closer.Close(), qt.IsNil)
	c.Assert(called, qt.Equals, 2)
}

func TestNewDescribedExtensionForwardsToOptions(t *testing.T) {
	c := qt.New(t)

	ext := NewDescribedExtension("t", "desc", func(*PrimitiveRegistry) error {
		return nil
	})

	c.Assert(ext.(Describer).Description(), qt.Equals, "desc")
	c.Assert(ext.(LibraryNamer).LibraryName(), qt.IsNil)
}

// TestDeepCopyTouchesEverySliceField is the drift-guard for the "ADD A NEW
// REGISTRY CATEGORY" ritual documented on the PrimitiveRegistry type. deepCopy builds an
// explicit struct literal, so a forgotten field fails closed (nil on the copy →
// silent data loss through Clone/Without*), while a future switch to a
// struct-copy idiom would fail open (shared backing array → cross-engine
// aliasing). This test catches both, generically over every slice field:
//
//   - completeness: every []T field on PrimitiveRegistry must be populated below, so a
//     newly added category forces this test (hence deepCopy) to be updated in
//     lockstep — the ritual is no longer self-enforcing by memory alone (the
//     doc comment count had already drifted 7→8 before this guard existed);
//   - fail-closed: the copy's length must match the source (field was copied);
//   - fail-open: the copy's backing array must differ (no aliasing).
//
// It generalizes the single-field TestClone_PreservesNamespaceInits.
func TestDeepCopyTouchesEverySliceField(t *testing.T) {
	p := NewRegistry()
	// Populate every category slice with one element so aliasing is observable
	// (Pointer() on an empty slice is not a reliable identity). If a new slice
	// field is added to Registry, the completeness check below fails until it is
	// populated here.
	p.primitives = append(p.primitives, PrimitiveRegistration{})
	p.bindingSpecs = append(p.bindingSpecs, BindingSpec{})
	p.docPrimitives = append(p.docPrimitives, PrimitiveSpec{})
	p.initFuncs = append(p.initFuncs, nil)
	p.macroSources = append(p.macroSources, "m")
	p.procedureSources = append(p.procedureSources, "s")
	p.globalValues = append(p.globalValues, GlobalValue{})
	p.namespaceInits = append(p.namespaceInits, nil)

	pv := reflect.ValueOf(p).Elem()
	pt := pv.Type()

	// Completeness precondition: every slice field must be populated above.
	for i := 0; i < pv.NumField(); i++ {
		f := pv.Field(i)
		if f.Kind() != reflect.Slice {
			continue
		}
		name := pt.Field(i).Name
		if f.Len() == 0 {
			t.Fatalf("Registry slice field %q is not populated in this test — a new "+
				"category was likely added; populate it here and extend deepCopy", name)
		}
	}

	q := p.deepCopy()
	qv := reflect.ValueOf(q).Elem()

	for i := 0; i < pv.NumField(); i++ {
		f := pv.Field(i)
		if f.Kind() != reflect.Slice {
			continue
		}
		name := pt.Field(i).Name
		qf := qv.Field(i)
		// Fail-closed: deepCopy omitted the field → nil/short on the copy.
		if qf.Len() != f.Len() {
			t.Errorf("deepCopy: field %q length %d != source %d (field not copied)", name, qf.Len(), f.Len())
			continue
		}
		// Fail-open: deepCopy shared the backing array with the source.
		if qf.Pointer() == f.Pointer() {
			t.Errorf("deepCopy: field %q shares its backing array with the source (aliasing)", name)
		}
	}
}

// TestPrimitiveSpec_Validate pins the embedder pre-flight path: a malformed spec
// yields an ErrInvalidArgument-matchable error rather than a panic, so a host
// assembling specs from config can reject them without crashing.
func TestPrimitiveSpec_Validate(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name    string
		spec    PrimitiveSpec
		wantErr bool
	}{
		{
			name:    "variadic with ParamCount 0 would index bnds[:-1]",
			spec:    PrimitiveSpec{Impl: noopImpl, Name: "bad-variadic", ParamCount: 0, IsVariadic: true},
			wantErr: true,
		},
		{
			name: "non-variadic ParamTypes shorter than ParamCount",
			spec: PrimitiveSpec{Impl: noopImpl, Name: "short-types", ParamCount: 2,
				ParamTypes: []values.TypeConstraint{values.TypeNumber}},
			wantErr: true,
		},
		{
			name: "non-variadic ParamTypes longer than ParamCount",
			spec: PrimitiveSpec{Impl: noopImpl, Name: "long-types", ParamCount: 1,
				ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber}},
			wantErr: true,
		},
		{
			name: "variadic ParamTypes longer than ParamCount",
			spec: PrimitiveSpec{Impl: noopImpl, Name: "long-variadic", ParamCount: 1, IsVariadic: true,
				ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber}},
			wantErr: true,
		},
		{
			name:    "variadic with no ParamTypes is unconstrained",
			spec:    PrimitiveSpec{Impl: noopImpl, Name: "ok-variadic", ParamCount: 1, IsVariadic: true},
			wantErr: false,
		},
		{
			name: "variadic short ParamTypes is the declared catch-all shape",
			spec: PrimitiveSpec{Impl: noopImpl, Name: "ok-short-variadic", ParamCount: 2, IsVariadic: true,
				ParamTypes: []values.TypeConstraint{values.TypeNumber}},
			wantErr: false,
		},
		{
			name: "non-variadic exact ParamTypes",
			spec: PrimitiveSpec{Impl: noopImpl, Name: "ok-exact", ParamCount: 1,
				ParamTypes: []values.TypeConstraint{values.TypeNumber}},
			wantErr: false,
		},
		{
			name:    "zero-arg non-variadic",
			spec:    PrimitiveSpec{Impl: noopImpl, Name: "ok-thunk", ParamCount: 0},
			wantErr: false,
		},
		{
			name:    "nil Impl is rejected",
			spec:    PrimitiveSpec{Name: "no-impl", ParamCount: 1},
			wantErr: true,
		},
		{
			name:    "empty Name is rejected",
			spec:    PrimitiveSpec{Impl: noopImpl, ParamCount: 1},
			wantErr: true,
		},
	}
	for _, test := range tests {
		c.Run(test.name, func(c *qt.C) {
			err := test.spec.Validate()
			if !test.wantErr {
				c.Assert(err, qt.IsNil)
				return
			}
			c.Assert(err, qt.IsNotNil)
			// Sentinel identity, not message text: the embedder branches on this.
			c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
				qt.Commentf("got %v", err))
		})
	}
}

// TestAddPrimitives_PanicsOnInvalidSpec pins the other half of the contract:
// registration is the Must path, so a spec that fails Validate still panics
// rather than being silently accepted or silently dropped.
func TestAddPrimitives_PanicsOnInvalidSpec(t *testing.T) {
	c := qt.New(t)
	reg := NewRegistry()
	bad := PrimitiveSpec{Impl: noopImpl, Name: "bad-variadic", ParamCount: 0, IsVariadic: true}

	// Pre-flight sees it, so an embedder had a way to avoid the panic below.
	c.Assert(bad.Validate(), qt.IsNotNil)

	var recovered any
	func() {
		defer func() {
			recovered = recover()
		}()
		reg.AddPrimitives([]PrimitiveSpec{bad}, PhaseSetRuntime)
	}()
	c.Assert(recovered, qt.IsNotNil)

	err, ok := recovered.(error)
	c.Assert(ok, qt.IsTrue, qt.Commentf("panic value %T is not an error", recovered))
	c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)
	// The registration context survives the wrap, and so does the cause chain.
	c.Assert(err.Error(), qt.Contains, "AddPrimitives")
	c.Assert(err.Error(), qt.Contains, "bad-variadic")

	// The failed spec left no partial registration behind.
	_, found := reg.FindPrimitive("bad-variadic", PhaseSetRuntime)
	c.Assert(found, qt.IsFalse)
}

// TestDuplicateRegistration_FirstWins pins the registry's duplicate-name
// precedence, and pins it at the two places that must not disagree: what
// FindPrimitive reports (the source of ,doc metadata) and what Apply actually
// binds. Before this, Apply ended in SetOwnGlobalValue and was last-wins while
// FindPrimitive was first-match, so doc and runtime described different procedures.
func TestDuplicateRegistration_FirstWins(t *testing.T) {
	c := qt.New(t)

	reg := NewRegistry()
	first := PrimitiveSpec{
		Name: "dup", ParamCount: 0, Doc: "the first one",
		Impl: func(mc machine.CallContext) error {
			mc.SetValue(testValue("first"))
			return nil
		},
	}
	second := PrimitiveSpec{
		Name: "dup", ParamCount: 0, Doc: "the second one",
		Impl: func(mc machine.CallContext) error {
			mc.SetValue(testValue("second"))
			return nil
		},
	}
	reg.AddPrimitive(first, PhaseSetRuntime)
	reg.AddPrimitive(second, PhaseSetRuntime)

	// Both registrations are retained; the duplicate is inert, not dropped.
	c.Assert(reg.PrimitiveCount(), qt.Equals, 2)

	// Lookup: first wins.
	found, ok := reg.FindPrimitive("dup", PhaseSetRuntime)
	c.Assert(ok, qt.IsTrue)
	c.Assert(found.Spec.Doc, qt.Equals, "the first one")

	// Apply: the bound closure must be the SAME registration lookup reported.
	// Compare through the closure's doc, which registerPhasePrimitive copies from
	// the spec it bound — the observable that would differ under last-wins.
	ns := environment.NewNamespace()
	env := ns.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	binding := env.GetBinding(values.NewSymbol("dup"), values.AllScopes())
	c.Assert(binding, qt.IsNotNil)
	closure, ok := binding.Value().(*machine.ForeignClosure)
	c.Assert(ok, qt.IsTrue, qt.Commentf("bound value is %T", binding.Value()))
	c.Assert(closure.Doc(), qt.Equals, "the first one",
		qt.Commentf("Apply bound a different registration than FindPrimitive reports"))

	// The agreement itself, stated as the invariant rather than as two constants.
	c.Assert(closure.Doc(), qt.Equals, found.Spec.Doc)
}

// TestDuplicateRegistration_PhasesAreIndependent guards the scope of first-wins:
// one name at two phases is two bindings, not a duplicate, so the expand
// registration must not be suppressed by the runtime one.
func TestDuplicateRegistration_PhasesAreIndependent(t *testing.T) {
	c := qt.New(t)

	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name: "cross", ParamCount: 0, Doc: "runtime flavour",
		Impl: func(_ machine.CallContext) error { return nil },
	}, PhaseSetRuntime)
	reg.AddPrimitive(PrimitiveSpec{
		Name: "cross", ParamCount: 0, Doc: "expand flavour",
		Impl: func(_ machine.CallContext) error { return nil },
	}, PhaseSetExpand)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	err := reg.Apply(context.Background(), env)
	c.Assert(err, qt.IsNil)

	runtimeBinding := env.GetBinding(values.NewSymbol("cross"), values.AllScopes())
	c.Assert(runtimeBinding, qt.IsNotNil)
	runtimeClosure, ok := runtimeBinding.Value().(*machine.ForeignClosure)
	c.Assert(ok, qt.IsTrue)
	c.Assert(runtimeClosure.Doc(), qt.Equals, "runtime flavour")

	expandBinding := env.Expand().GetBinding(values.NewSymbol("cross"), values.AllScopes())
	c.Assert(expandBinding, qt.IsNotNil)
	expandClosure, ok := expandBinding.Value().(*machine.ForeignClosure)
	c.Assert(ok, qt.IsTrue)
	c.Assert(expandClosure.Doc(), qt.Equals, "expand flavour",
		qt.Commentf("per-phase precedence collapsed: the expand registration was suppressed"))
}

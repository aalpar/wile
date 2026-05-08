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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
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
		{Name: "prim1", ParamCount: 1, Impl: nil},
		{Name: "prim2", ParamCount: 2, IsVariadic: true, Impl: nil},
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
	r.AddPrimitive(PrimitiveSpec{Name: "runtime-only", ParamCount: 1}, PhaseSetRuntime)
	r.AddPrimitive(PrimitiveSpec{Name: "expand-only", ParamCount: 0}, PhaseSetExpand)
	r.AddPrimitive(PrimitiveSpec{Name: "both", ParamCount: 2}, PhaseSetRuntime|PhaseSetExpand)

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
	r.AddPrimitive(PrimitiveSpec{Name: "test-prim"}, PhaseSetRuntime)

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
	r.AddPrimitive(PrimitiveSpec{Name: "test", ParamCount: 1}, PhaseSetRuntime)
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
		func(r *Registry) error {
			r.AddPrimitive(PrimitiveSpec{Name: "prim1"}, PhaseSetRuntime)
			return nil
		},
		func(r *Registry) error {
			r.AddPrimitive(PrimitiveSpec{Name: "prim2"}, PhaseSetExpand)
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

func TestRegistry_PrimitiveByName(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "car", ParamCount: 1, Impl: nil, Doc: "Returns the car.", ParamNames: []string{"pair"}, Category: "pairs"},
		{Name: "cdr", ParamCount: 1, Impl: nil, Doc: "Returns the cdr.", ParamNames: []string{"pair"}, Category: "pairs"},
		{Name: "+", ParamCount: 0, IsVariadic: true, Impl: nil, Doc: "Returns the sum.", ParamNames: []string{"z"}, Category: "arithmetic"},
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
		{Name: "alpha", Impl: nil},
		{Name: "beta", ParamCount: 1, Impl: nil},
		{Name: "gamma", ParamCount: 2, Impl: nil},
	}, PhaseSetRuntime)

	names := r.PrimitiveNames()
	c.Assert(names, qt.DeepEquals, []string{"alpha", "beta", "gamma"})
}

func TestRegistry_PrimitivesByCategory(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "car", ParamCount: 1, Impl: nil, Category: "pairs"},
		{Name: "cdr", ParamCount: 1, Impl: nil, Category: "pairs"},
		{Name: "+", Impl: nil, Category: "arithmetic"},
		{Name: "display", ParamCount: 1, Impl: nil},
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
		Name: "test", ParamCount: 1, Impl: nil,
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
		{Name: "car", Category: "pairs"},
		{Name: "cdr", Category: "pairs"},
		{Name: "set-car!", Category: "pairs"},
		{Name: "+", Category: "arithmetic"},
		{Name: "vector-set!", Category: "vectors"},
	}, PhaseSetRuntime)
	r.AddBinding("if")
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
			// Non-primitive fields are copied unchanged.
			c.Assert(filtered.Bindings(), qt.DeepEquals, []string{"if"})
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
		{Name: "car", Category: "pairs"},
		{Name: "cdr", Category: "pairs"},
		{Name: "+", Category: "arithmetic"},
		{Name: "display", Category: "io"},
		{Name: "uncategorized"},
	}, PhaseSetRuntime)
	r.AddBinding("lambda")

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
			c.Assert(filtered.Bindings(), qt.DeepEquals, []string{"lambda"})
		})
	}

	// Original is unmodified.
	c.Assert(r.PrimitiveCount(), qt.Equals, 5)
}

func TestRegistry_WithoutBindings(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	r.AddPrimitives([]PrimitiveSpec{
		{Name: "set!", Category: "special"},
		{Name: "+", Category: "arithmetic"},
	}, PhaseSetRuntime)
	r.AddBindings([]string{"if", "set!", "lambda", "define"})

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
			// Primitives are unchanged.
			c.Assert(filtered.PrimitiveCount(), qt.Equals, 2)
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
	c.Assert(r2.Docs(), qt.HasLen, 1)
	c.Assert(r2.BindingSpecs(), qt.HasLen, 1)
	r2.AddDocumentation("or", "Disjunction.")
	c.Assert(r.Docs(), qt.HasLen, 1)
}

func TestRegistry_AddDocOnlyPrimitive(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		setup      func(r *Registry)
		query      string
		found      bool
		wantPhases PhaseSet
		wantDoc    string
	}{
		{
			name: "doc-only primitive is findable",
			setup: func(r *Registry) {
				r.AddDocOnlyPrimitive(PrimitiveSpec{
					Name:     "map",
					Doc:      "Apply proc to each element.",
					Category: "lists",
				})
			},
			query:      "map",
			found:      true,
			wantPhases: 0,
			wantDoc:    "Apply proc to each element.",
		},
		{
			name: "Go primitive takes precedence",
			setup: func(r *Registry) {
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
			query:      "car",
			found:      true,
			wantPhases: PhaseSetRuntime,
			wantDoc:    "Go car.",
		},
		{
			name: "doc-only entry has zero phases",
			setup: func(r *Registry) {
				r.AddDocOnlyPrimitive(PrimitiveSpec{
					Name:       "for-each",
					Doc:        "Apply proc for side effects.",
					ParamCount: 2,
				})
			},
			query:      "for-each",
			found:      true,
			wantPhases: 0,
			wantDoc:    "Apply proc for side effects.",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			r := NewRegistry()
			tc.setup(r)
			reg, ok := r.PrimitiveByName(tc.query)
			c.Assert(ok, qt.Equals, tc.found)
			if tc.found {
				c.Assert(reg.Phases, qt.Equals, tc.wantPhases)
				c.Assert(reg.Spec.Doc, qt.Equals, tc.wantDoc)
			}
		})
	}
}

func TestExtension(t *testing.T) {
	c := qt.New(t)

	ext := NewExtension("test-ext", func(r *Registry) error {
		r.AddPrimitive(PrimitiveSpec{Name: "ext-prim"}, PhaseSetRuntime)
		return nil
	})

	c.Assert(ext.Name(), qt.Equals, "test-ext")

	r := NewRegistry()
	err := ext.AddToRegistry(r)
	c.Assert(err, qt.IsNil)
	c.Assert(r.PrimitiveCount(), qt.Equals, 1)
}

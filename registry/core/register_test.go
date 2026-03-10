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

package core

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry"
)

func TestBuilder_AddToRegistry(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := Builder.AddToRegistry(r)
	c.Assert(err, qt.IsNil)

	// Verify primitives were registered
	c.Assert(r.PrimitiveCount() > 0, qt.IsTrue, qt.Commentf("expected primitives to be registered"))
}

func TestBuilder_Build(t *testing.T) {
	c := qt.New(t)

	r, err := Builder.Build()
	c.Assert(err, qt.IsNil)
	c.Assert(r, qt.IsNotNil)
	c.Assert(r.PrimitiveCount() > 0, qt.IsTrue)
}

func TestExtension(t *testing.T) {
	c := qt.New(t)

	c.Assert(Extension.Name(), qt.Equals, "core")

	r := registry.NewRegistry()
	err := Extension.AddToRegistry(r)
	c.Assert(err, qt.IsNil)
	c.Assert(r.PrimitiveCount() > 0, qt.IsTrue)
}

func TestAddToRegistry_RegistersBindings(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := AddToRegistry(r)
	c.Assert(err, qt.IsNil)

	// Should have compile-time bindings for special forms
	bindings := r.Bindings()
	c.Assert(len(bindings) > 0, qt.IsTrue, qt.Commentf("expected compile-time bindings"))

	// Check for expected special forms
	bindingSet := make(map[string]bool)
	for _, b := range bindings {
		bindingSet[b] = true
	}

	expectedBindings := []string{"if", "lambda", "define", "quote", "set!"}
	for _, name := range expectedBindings {
		c.Assert(bindingSet[name], qt.IsTrue, qt.Commentf("expected binding %q", name))
	}
}

func TestAddToRegistry_RegistersPredicates(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addPredicates(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	// Check for expected predicates
	expectedPrims := []string{"null?", "pair?", "number?", "string?", "procedure?"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected predicate %q", name))
	}
}

func TestAddToRegistry_RegistersEquality(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addEquality(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"eq?", "eqv?", "equal?"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected equality primitive %q", name))
	}
}

func TestAddToRegistry_RegistersBoolean(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addBoolean(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"not"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected boolean primitive %q", name))
	}
}

func TestAddToRegistry_RegistersPairs(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addPairs(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"cons", "car", "cdr", "set-car!", "set-cdr!"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected pair primitive %q", name))
	}
}

func TestAddToRegistry_RegistersArithmetic(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addArithmetic(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"+", "-", "*", "/", "=", "<", ">", "<=", ">="}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected arithmetic primitive %q", name))
	}
}

func TestAddToRegistry_RegistersLists(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addLists(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"list", "length", "append", "reverse", "memq", "assq"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected list primitive %q", name))
	}
}

func TestAddToRegistry_RegistersControl(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addControl(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"apply", "call/cc", "call-with-current-continuation", "values", "call-with-values", "call-with-exit", "call-with-continuation-barrier"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected control primitive %q", name))
	}
}

func TestAddToRegistry_RegistersVectors(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addVectors(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"make-vector", "vector", "vector-length", "vector-ref", "vector-set!"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected vector primitive %q", name))
	}
}

func TestAddToRegistry_RegistersStrings(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addStrings(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"make-string", "string", "string-length", "string-ref", "string-set!"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected string primitive %q", name))
	}
}

func TestAddToRegistry_RegistersCharacters(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addCharacters(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"char->integer", "integer->char", "char=?", "char<?"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected character primitive %q", name))
	}
}

func TestAddToRegistry_RegistersBytevectors(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addBytevectors(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"make-bytevector", "bytevector", "bytevector-length", "bytevector-u8-ref"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected bytevector primitive %q", name))
	}
}

func TestAddToRegistry_RegistersSyntax(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addSyntax(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"identifier?", "syntax->datum", "datum->syntax"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected syntax primitive %q", name))
	}
}

func TestAddToRegistry_RegistersParameters(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := addParameters(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()
	primNames := make(map[string]bool)
	for _, p := range prims {
		primNames[p.Spec.Name] = true
	}

	expectedPrims := []string{"make-parameter", "parameter?"}
	for _, name := range expectedPrims {
		c.Assert(primNames[name], qt.IsTrue, qt.Commentf("expected parameter primitive %q", name))
	}
}

func TestAddToRegistry_HasMacroSources(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := AddToRegistry(r)
	c.Assert(err, qt.IsNil)

	sources := r.MacroSources()
	c.Assert(len(sources), qt.Equals, 2, qt.Commentf("expected bootstrap macros + bootstrap procedures"))
}

func TestPrimitivePhases(t *testing.T) {
	c := qt.New(t)

	r := registry.NewRegistry()
	err := AddToRegistry(r)
	c.Assert(err, qt.IsNil)

	prims := r.Primitives()

	// Find a specific primitive and check its phases
	var carPrim *registry.PrimitiveRegistration
	for i := range prims {
		if prims[i].Spec.Name == "car" {
			carPrim = &prims[i]
			break
		}
	}

	c.Assert(carPrim, qt.IsNotNil, qt.Commentf("expected car primitive"))
	c.Assert(carPrim.Phases.HasRuntime(), qt.IsTrue, qt.Commentf("car should be available at runtime"))
	c.Assert(carPrim.Phases.HasExpand(), qt.IsTrue, qt.Commentf("car should be available at expand time"))
}

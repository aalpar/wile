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

package forms

import (
	"context"
	"maps"
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"

	qt "github.com/frankban/quicktest"
)

// testExpr is a minimal ValidatedExpr for testing dispatch.
type testExpr struct {
	tag string
}

func (p *testExpr) SetFormName(_ string) {}
func (p *testExpr) FormName() string {
	return p.tag
}
func (p *testExpr) Source() *syntax.SourceContext {
	return nil
}

// saveRegistry snapshots the defaultRegistry before a test and restores it after,
// so mutations don't leak between tests.
func saveRegistry(t *testing.T) {
	t.Helper()
	saved := maps.Clone(defaultRegistry.specs)
	// Replace with empty registry so tests only see what they register.
	defaultRegistry.specs = make(map[string]*FormSpec)
	t.Cleanup(func() {
		defaultRegistry.specs = saved
	})
}

func TestLookup_Miss(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)
	c.Assert(Lookup("nonexistent-form"), qt.IsNil)
}

func TestRegister_And_Lookup(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	validator := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "validated"}
	}

	Register(&FormSpec{
		Name:     "test-register",
		Validate: validator,
	})

	spec := Lookup("test-register")
	c.Assert(spec, qt.IsNotNil)
	c.Assert(spec.Name, qt.Equals, "test-register")
	c.Assert(spec.Validate, qt.IsNotNil)
}

func TestRegister_Replaces_Existing(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	first := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "first"}
	}
	second := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "second"}
	}

	Register(&FormSpec{Name: "test-replace", Validate: first})
	Register(&FormSpec{Name: "test-replace", Validate: second})

	spec := Lookup("test-replace")
	c.Assert(spec, qt.IsNotNil)

	// The second registration replaced the first.
	result := spec.Validate(context.Background(), nil, nil, nil)
	c.Assert(result.FormName(), qt.Equals, "second")
}

func TestRegisterValidator_Creates_New_Entry(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	validator := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "v"}
	}
	RegisterValidator("test-val-new", validator)

	spec := Lookup("test-val-new")
	c.Assert(spec, qt.IsNotNil)
	c.Assert(spec.Name, qt.Equals, "test-val-new")
	c.Assert(spec.Validate, qt.IsNotNil)
}

func TestRegisterValidator_Sets_On_Existing(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	Register(&FormSpec{Name: "test-val-existing"})

	validator := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "added"}
	}
	RegisterValidator("test-val-existing", validator)

	spec := Lookup("test-val-existing")
	c.Assert(spec.Validate, qt.IsNotNil)
}

func TestNames_Returns_All_Registered(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	Register(&FormSpec{Name: "test-names-a"})
	Register(&FormSpec{Name: "test-names-b"})

	names := Names()
	c.Assert(slices.Contains(names, "test-names-a"), qt.IsTrue)
	c.Assert(slices.Contains(names, "test-names-b"), qt.IsTrue)
}

func TestVerify_AllPaired(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	validator := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "v"}
	}
	Register(&FormSpec{Name: "test-verify-ok", Validate: validator})

	err := Verify()
	c.Assert(err, qt.IsNil)
}

func TestVerify_MissingValidator(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	Register(&FormSpec{Name: "test-verify-no-validator"})

	err := Verify()
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "test-verify-no-validator: missing validator")
}

func TestFormRegistry_Register_Lookup_Remove(t *testing.T) {
	c := qt.New(t)

	fn := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "r"}
	}
	r := NewFormRegistry()
	r.Register(&FormSpec{Name: "x", Validate: fn})

	spec := r.Lookup("x")
	c.Assert(spec, qt.IsNotNil)
	c.Assert(spec.Name, qt.Equals, "x")

	// Remove then Lookup returns nil.
	r.Remove("x")
	c.Assert(r.Lookup("x"), qt.IsNil)

	// Remove of an absent name is a no-op (must not panic).
	r.Remove("x")
}

func TestFormRegistry_Clone_COW_Isolation(t *testing.T) {
	c := qt.New(t)

	origFn := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "orig"}
	}
	otherFn := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "other"}
	}

	r := NewFormRegistry()
	r.RegisterValidator("x", origFn)
	origSpec := r.Lookup("x")

	clone := r.Clone()

	// Override "x" in the clone. Must not affect r.
	clone.RegisterValidator("x", otherFn)
	c.Assert(r.Lookup("x"), qt.Equals, origSpec) // pointer identity: original spec unchanged

	// Remove "x" from the clone. Must not affect r.
	clone.Remove("x")
	c.Assert(r.Lookup("x"), qt.IsNotNil)
}

func TestFormRegistry_DefaultRegistry_Identity(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	fn := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "t"}
	}
	Register(&FormSpec{Name: "t", Validate: fn})
	c.Assert(DefaultRegistry().Lookup("t"), qt.IsNotNil)
}

func TestFormRegistry_Verify(t *testing.T) {
	c := qt.New(t)

	fn := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "ok"}
	}

	// Nil-validate spec produces an error naming the form.
	r := NewFormRegistry()
	r.Register(&FormSpec{Name: "z"})
	err := r.Verify()
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "z")

	// Fully-populated registry returns nil.
	r2 := NewFormRegistry()
	r2.RegisterValidator("z", fn)
	c.Assert(r2.Verify(), qt.IsNil)
}

func TestRegistryFor_NilEnv_ReturnsDefault(t *testing.T) {
	c := qt.New(t)
	got := RegistryFor(nil)
	c.Assert(got, qt.Equals, defaultRegistry)
}

func TestRegistryFor_NoRegistrySet_ReturnsDefault(t *testing.T) {
	c := qt.New(t)
	// Fresh namespace has no formRegistry set; FormRegistry() returns nil.
	env := environment.NewNamespace().Runtime()
	got := RegistryFor(env)
	c.Assert(got, qt.Equals, defaultRegistry)
}

func TestRegistryFor_WrongType_ReturnsDefault(t *testing.T) {
	c := qt.New(t)
	ns := environment.NewNamespace()
	// Store a non-*FormRegistry value; type assertion must fail gracefully.
	ns.SetFormRegistry("not-a-registry")
	got := RegistryFor(ns.Runtime())
	c.Assert(got, qt.Equals, defaultRegistry)
}

func TestRegistryFor_RegistrySet_ReturnsIt(t *testing.T) {
	c := qt.New(t)
	ns := environment.NewNamespace()
	fr := NewFormRegistry()
	ns.SetFormRegistry(fr)
	got := RegistryFor(ns.Runtime())
	c.Assert(got, qt.Equals, fr)
}

// noopValidator is a do-nothing ValidatorFunc for registry-preservation tests.
func noopValidator() ValidatorFunc {
	return func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return nil
	}
}

func TestRegisterCompiler_Preserves_Validator(t *testing.T) {
	c := qt.New(t)
	r := NewFormRegistry()
	r.RegisterValidator("f", noopValidator())
	r.RegisterCompiler("f", "COMPILER")
	spec := r.Lookup("f")
	c.Assert(spec.Validate, qt.IsNotNil)
	c.Assert(spec.Compile, qt.Equals, "COMPILER")
}

func TestRegisterValidator_Preserves_Compiler(t *testing.T) {
	c := qt.New(t)
	r := NewFormRegistry()
	r.RegisterCompiler("f", "COMPILER")
	r.RegisterValidator("f", noopValidator())
	spec := r.Lookup("f")
	c.Assert(spec.Compile, qt.Equals, "COMPILER")
	c.Assert(spec.Validate, qt.IsNotNil)
}

func TestFormRegistry_Clone_COW_Compile(t *testing.T) {
	c := qt.New(t)
	r := NewFormRegistry()
	r.RegisterCompiler("f", "C1")
	clone := r.Clone()
	c.Assert(clone.Lookup("f").Compile, qt.Equals, "C1")
	clone.RegisterCompiler("f", "C2")
	c.Assert(r.Lookup("f").Compile, qt.Equals, "C1")
	c.Assert(clone.Lookup("f").Compile, qt.Equals, "C2")
}

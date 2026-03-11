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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"

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

// saveRegistry snapshots the registry before a test and restores it after,
// so mutations don't leak between tests.
func saveRegistry(t *testing.T) {
	t.Helper()
	saved := make(map[string]*FormSpec, len(registry))
	maps.Copy(saved, registry)
	t.Cleanup(func() {
		registry = saved
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
	compiler := func(_ any, _ any, _ ValidatedExpr) error { return nil }

	Register(&FormSpec{
		Name:     "test-register",
		Validate: validator,
		Compile:  compiler,
	})

	spec := Lookup("test-register")
	c.Assert(spec, qt.IsNotNil)
	c.Assert(spec.Name, qt.Equals, "test-register")
	c.Assert(spec.Validate, qt.IsNotNil)
	c.Assert(spec.Compile, qt.IsNotNil)
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
	c.Assert(spec.Compile, qt.IsNil)
}

func TestRegisterCompiler_Creates_New_Entry(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	compiler := func(_ any, _ any, _ ValidatedExpr) error { return nil }
	RegisterCompiler("test-comp-new", compiler)

	spec := Lookup("test-comp-new")
	c.Assert(spec, qt.IsNotNil)
	c.Assert(spec.Name, qt.Equals, "test-comp-new")
	c.Assert(spec.Validate, qt.IsNil)
	c.Assert(spec.Compile, qt.IsNotNil)
}

func TestRegisterValidator_Sets_On_Existing(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	compiler := func(_ any, _ any, _ ValidatedExpr) error { return nil }
	Register(&FormSpec{Name: "test-val-existing", Compile: compiler})

	validator := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "added"}
	}
	RegisterValidator("test-val-existing", validator)

	spec := Lookup("test-val-existing")
	c.Assert(spec.Validate, qt.IsNotNil)
	c.Assert(spec.Compile, qt.IsNotNil)
}

func TestRegisterCompiler_Sets_On_Existing(t *testing.T) {
	saveRegistry(t)
	c := qt.New(t)

	validator := func(_ context.Context, _ *environment.EnvironmentFrame, _ *syntax.SyntaxPair, _ any) ValidatedExpr {
		return &testExpr{tag: "v"}
	}
	Register(&FormSpec{Name: "test-comp-existing", Validate: validator})

	compiler := func(_ any, _ any, _ ValidatedExpr) error { return nil }
	RegisterCompiler("test-comp-existing", compiler)

	spec := Lookup("test-comp-existing")
	c.Assert(spec.Validate, qt.IsNotNil)
	c.Assert(spec.Compile, qt.IsNotNil)
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

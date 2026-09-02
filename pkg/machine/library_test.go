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

package machine_test

import (
	"context"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	"github.com/aalpar/wile/pkg/internal/bootstrap"

	qt "github.com/frankban/quicktest"
)

// getTestdataPath returns the path to the testdata directory for library tests
func getTestdataPath() string {
	_, filename, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(filename), "testdata", "lib")
}

// parseLibrarySyntax is a helper to parse a string into syntax for library tests
func parseLibrarySyntax(t *testing.T, env *environment.EnvironmentFrame, input string) syntax.SyntaxValue {
	reader := strings.NewReader(input)
	p := parser.NewParser(env, true, reader)
	stx, err := p.ReadSyntax(context.TODO())
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	return stx
}

func TestLibraryName(t *testing.T) {
	c := qt.New(t)

	// Test compilation.LibraryName creation and methods
	name := compilation.NewLibraryName("scheme", "base")
	c.Assert(name.String(), qt.Equals, "scheme/base")
	c.Assert(name.SchemeString(), qt.Equals, "(scheme base)")
	c.Assert(name.Key(), qt.Equals, "scheme/base")
	c.Assert(name.ToFSPath(), qt.Equals, "scheme/base.sld")

	// Test multi-part name
	name2 := compilation.NewLibraryName("my", "lib", "utils")
	c.Assert(name2.String(), qt.Equals, "my/lib/utils")
	c.Assert(name2.SchemeString(), qt.Equals, "(my lib utils)")
}

func TestLibraryRegistry(t *testing.T) {
	c := qt.New(t)

	registry := compilation.NewLibraryRegistry()

	// Test that default search paths are set. Only "." is searched by default;
	// the stdlib is served by the embedded FileResolver chain, not a dev-tree path
	// (the former "./pkg/stdlib/lib" entry was dropped in Phase 8 Task 8F).
	paths := registry.GetSearchPaths()
	c.Assert(len(paths), qt.Equals, 1)
	c.Assert(paths[0], qt.Equals, ".")

	// Test SetSearchPaths
	registry.SetSearchPaths([]string{"/custom/path"})
	c.Assert(registry.GetSearchPaths(), qt.DeepEquals, []string{"/custom/path"})

	// Test PrependSearchPath (prepends)
	registry.PrependSearchPath("/another/path")
	c.Assert(registry.GetSearchPaths()[0], qt.Equals, "/another/path")
}

func TestCompiledLibrary(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	name := compilation.NewLibraryName("test", "lib")
	lib := compilation.NewCompiledLibrary(name, env)

	// Test empty exports initially
	c.Assert(lib.IsExported("bindSymbolWithScopes"), qt.IsFalse)

	// Test AddExport with same internal/external name
	lib.AddExport("bindSymbolWithScopes", "")
	c.Assert(lib.IsExported("bindSymbolWithScopes"), qt.IsTrue)
	c.Assert(lib.GetInternalName("bindSymbolWithScopes"), qt.Equals, "bindSymbolWithScopes")

	// Test AddExport with rename
	lib.AddExport("bar", "internal-bar")
	c.Assert(lib.IsExported("bar"), qt.IsTrue)
	c.Assert(lib.GetInternalName("bar"), qt.Equals, "internal-bar")
}

func TestImportSet(t *testing.T) {
	c := qt.New(t)

	// Create a library with exports
	env := environment.NewNamespace().Runtime()
	name := compilation.NewLibraryName("test", "lib")
	lib := compilation.NewCompiledLibrary(name, env)
	lib.AddExport("bindSymbolWithScopes", "")
	lib.AddExport("bar", "")
	lib.AddExport("baz", "")

	// Test basic import set (all exports)
	importSet := compilation.NewImportSet(name)
	bindings, err := importSet.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(len(bindings), qt.Equals, 3)
	c.Assert(bindings["bindSymbolWithScopes"], qt.Equals, "bindSymbolWithScopes")
	c.Assert(bindings["bar"], qt.Equals, "bar")
	c.Assert(bindings["baz"], qt.Equals, "baz")

	// Test 'only' filter
	importSet2 := compilation.NewImportSet(name)
	importSet2.AddOnly(values.StringSet{"bindSymbolWithScopes": {}, "bar": {}})
	bindings2, err := importSet2.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(len(bindings2), qt.Equals, 2)
	c.Assert(bindings2["bindSymbolWithScopes"], qt.Equals, "bindSymbolWithScopes")
	c.Assert(bindings2["bar"], qt.Equals, "bar")

	// Test 'except' filter
	importSet3 := compilation.NewImportSet(name)
	importSet3.AddExcept(values.StringSet{"baz": {}})
	bindings3, err := importSet3.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(len(bindings3), qt.Equals, 2)

	// Test 'prefix' modifier
	importSet4 := compilation.NewImportSet(name)
	importSet4.AddPrefix("my:")
	bindings4, err := importSet4.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(bindings4["my:bindSymbolWithScopes"], qt.Equals, "bindSymbolWithScopes")
	c.Assert(bindings4["my:bar"], qt.Equals, "bar")

	// Test 'rename' modifier
	importSet5 := compilation.NewImportSet(name)
	importSet5.AddRename(map[string]string{"bindSymbolWithScopes": "renamed-bindSymbolWithScopes"})
	bindings5, err := importSet5.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(bindings5["renamed-bindSymbolWithScopes"], qt.Equals, "bindSymbolWithScopes")
	c.Assert(bindings5["bar"], qt.Equals, "bar")
}

func TestImportSetErrors(t *testing.T) {
	c := qt.New(t)

	// Create a library with limited exports
	env := environment.NewNamespace().Runtime()
	name := compilation.NewLibraryName("test", "lib")
	lib := compilation.NewCompiledLibrary(name, env)
	lib.AddExport("bindSymbolWithScopes", "")

	// Test 'only' with non-existent identifier
	importSet := compilation.NewImportSet(name)
	importSet.AddOnly(values.StringSet{"nonexistent": {}})
	_, err := importSet.ApplyToExports(lib)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "nonexistent")

	// Test 'except' with non-existent identifier
	importSet2 := compilation.NewImportSet(name)
	importSet2.AddExcept(values.StringSet{"nonexistent": {}})
	_, err2 := importSet2.ApplyToExports(lib)
	c.Assert(err2, qt.IsNotNil)
}

func TestCompileDefineLibrary_Basic(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	// Parse a simple library definition
	libDef := parseLibrarySyntax(t, env, `
		(define-library (my test lib)
		  (export bindSymbolWithScopes)
		  (begin
		    (define bindSymbolWithScopes 42)))
	`)

	// Extract args after 'define-library' keyword
	libPair := libDef.(*syntax.SyntaxPair)
	args := libPair.Cdr().(*syntax.SyntaxPair)

	// Compile the library
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileDefineLibrary(ctctx, args)
	qt.Assert(t, err, qt.IsNil)
}

func TestCompileDefineLibrary_Empty(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	// Parse an empty library definition
	libDef := parseLibrarySyntax(t, env, `(define-library (empty lib))`)

	// Extract args
	libPair := libDef.(*syntax.SyntaxPair)
	args := libPair.Cdr().(*syntax.SyntaxPair)

	// Compile
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileDefineLibrary(ctctx, args)
	qt.Assert(t, err, qt.IsNil)
}

func TestLibraryDescription(t *testing.T) {
	tests := []struct {
		name        string
		input       string
		wantDesc    string
		wantErr     bool
		errContains string
	}{
		{
			name:     "basic description",
			input:    `(define-library (test desc) (description "A test library.") (export))`,
			wantDesc: "A test library.",
		},
		{
			name:     "no description",
			input:    `(define-library (test nodesc) (export))`,
			wantDesc: "",
		},
		{
			name:        "non-string argument",
			input:       `(define-library (test bad) (description 42))`,
			wantErr:     true,
			errContains: "description: argument must be a string",
		},
		{
			name:        "extra arguments rejected",
			input:       `(define-library (test extra) (description "hello" "world"))`,
			wantErr:     true,
			errContains: "description: expected exactly 1 element(s), got 2",
		},
		{
			name:     "last description wins",
			input:    `(define-library (test multi) (description "first") (description "second") (export))`,
			wantDesc: "second",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)

			env := environment.NewNamespace().Runtime()
			libDef := parseLibrarySyntax(t, env, tt.input)

			libPair := libDef.(*syntax.SyntaxPair)
			args := libPair.Cdr().(*syntax.SyntaxPair)

			tpl := machine.NewNativeTemplate(0, 0, false)
			ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

			var compiledLib *compilation.CompiledLibrary
			ctc.SetLibraryCallback(func(lib *compilation.CompiledLibrary) {
				compiledLib = lib
			})

			ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
			err := ctc.CompileDefineLibrary(ctctx, args)

			if tt.wantErr {
				c.Assert(err, qt.IsNotNil)
				c.Assert(err.Error(), qt.Contains, tt.errContains)
				return
			}

			c.Assert(err, qt.IsNil)
			c.Assert(compiledLib, qt.IsNotNil, qt.Commentf("library callback should have been called"))
			c.Assert(compiledLib.Description, qt.Equals, tt.wantDesc)
		})
	}
}

func TestCompileImport_LibraryNotFound(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	// Set up a library registry (required for imports)
	registry := compilation.NewLibraryRegistry()
	env.SetLibraryRegistry(registry)
	env.SetFileResolver(compilation.NewOSFileResolver(env))

	// Parse an import declaration
	importDef := parseLibrarySyntax(t, env, `(import (scheme base))`)

	// Extract args
	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	// Compile - should fail because (scheme base) doesn't exist
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileImport(ctctx, args)
	qt.Assert(t, err, qt.IsNotNil) // Library not found
	qt.Assert(t, err.Error(), qt.Contains, "not found")
}

func TestCompileImport_NoRegistry(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	// Intentionally NOT setting up a library registry

	// Parse an import declaration
	importDef := parseLibrarySyntax(t, env, `(import (scheme base))`)

	// Extract args
	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	// Compile - should fail because no registry is configured
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileImport(ctctx, args)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no library registry")
}

func TestCompileExport_TopLevelError(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	// Parse an export declaration
	exportDef := parseLibrarySyntax(t, env, `(export bindSymbolWithScopes bar)`)

	// Extract args
	exportPair := exportDef.(*syntax.SyntaxPair)
	args := exportPair.Cdr().(syntax.SyntaxValue)

	// Compile - should error at top level
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileExport(ctctx, args)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "only valid within define-library")
}

// ============================================================================
// Integration tests for library loading
// ============================================================================

// setupLibraryTest sets up a test environment with library loading capability
func setupLibraryTest(t *testing.T) *environment.EnvironmentFrame {
	t.Helper()

	// Create the top-level environment
	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	if err != nil {
		t.Fatalf("failed to create environment: %v", err)
	}

	// Set up the factory for creating library environments
	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	// Create and configure the library registry
	registry := compilation.NewLibraryRegistry()
	registry.SetSearchPaths([]string{getTestdataPath()})
	env.SetLibraryRegistry(registry)

	return env
}

func TestLoadLibrary_Simple(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Load the simple library
	name := compilation.NewLibraryName("test", "simple")
	lib, err := compilation.LoadLibrary(context.Background(), name, env, machine.NewVMMacroEvaluator())
	c.Assert(err, qt.IsNil)
	c.Assert(lib, qt.IsNotNil)

	// Verify exports
	c.Assert(lib.IsExported("make-adder"), qt.IsTrue)
	c.Assert(lib.IsExported("double"), qt.IsTrue)
	c.Assert(lib.IsExported("secret-value"), qt.IsTrue)
	c.Assert(lib.IsExported("not-exported"), qt.IsFalse)

	// Verify library name matches
	c.Assert(lib.Name.Key(), qt.Equals, "test/simple")
}

func TestLoadLibrary_Cached(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Load the same library twice
	name := compilation.NewLibraryName("test", "simple")
	lib1, err := compilation.LoadLibrary(context.Background(), name, env, machine.NewVMMacroEvaluator())
	c.Assert(err, qt.IsNil)

	lib2, err := compilation.LoadLibrary(context.Background(), name, env, machine.NewVMMacroEvaluator())
	c.Assert(err, qt.IsNil)

	// Should return the same cached library
	c.Assert(lib1, qt.Equals, lib2)
}

func TestLoadLibrary_WithImports(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Load the importer library (which imports test/simple)
	name := compilation.NewLibraryName("test", "importer")
	lib, err := compilation.LoadLibrary(context.Background(), name, env, machine.NewVMMacroEvaluator())
	c.Assert(err, qt.IsNil)
	c.Assert(lib, qt.IsNotNil)

	// Verify exports
	c.Assert(lib.IsExported("quadruple"), qt.IsTrue)
	c.Assert(lib.IsExported("get-secret"), qt.IsTrue)

	// Verify that the dependency was also loaded
	registry := env.LibraryRegistry().(*compilation.LibraryRegistry)
	simpleName := compilation.NewLibraryName("test", "simple")
	simpleLib := registry.Lookup(simpleName)
	c.Assert(simpleLib, qt.IsNotNil)
}

func TestLoadLibrary_CircularDependency(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Try to load a library with circular dependency
	name := compilation.NewLibraryName("test", "circular-a")
	_, err := compilation.LoadLibrary(context.Background(), name, env, machine.NewVMMacroEvaluator())
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "circular")
}

func TestLoadLibrary_NotFound(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Try to load a non-existent library
	name := compilation.NewLibraryName("nonexistent", "lib")
	_, err := compilation.LoadLibrary(context.Background(), name, env, machine.NewVMMacroEvaluator())
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "not found")
}

func TestImport_Simple(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Parse and compile an import statement
	importDef := parseLibrarySyntax(t, env, `(import (test simple))`)

	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify that the exported names are now bound in the environment
	makeAdder := values.NewSymbol("make-adder")
	c.Assert(env.GetBinding(makeAdder, values.AllScopes()), qt.IsNotNil)

	double := values.NewSymbol("double")
	c.Assert(env.GetBinding(double, values.AllScopes()), qt.IsNotNil)

	secretValue := values.NewSymbol("secret-value")
	c.Assert(env.GetBinding(secretValue, values.AllScopes()), qt.IsNotNil)
}

func TestImport_OnlyModifier(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Parse and compile an import with 'only' modifier
	importDef := parseLibrarySyntax(t, env, `(import (only (test simple) double))`)

	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify only 'double' is bound
	double := values.NewSymbol("double")
	c.Assert(env.GetBinding(double, values.AllScopes()), qt.IsNotNil)

	makeAdder := values.NewSymbol("make-adder")
	c.Assert(env.GetBinding(makeAdder, values.AllScopes()), qt.IsNil)
}

func TestImport_PrefixModifier(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Parse and compile an import with 'prefix' modifier
	importDef := parseLibrarySyntax(t, env, `(import (prefix (test simple) test:))`)

	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify prefixed names are bound
	testDouble := values.NewSymbol("test:double")
	c.Assert(env.GetBinding(testDouble, values.AllScopes()), qt.IsNotNil)

	// Verify unprefixed names are NOT bound
	double := values.NewSymbol("double")
	c.Assert(env.GetBinding(double, values.AllScopes()), qt.IsNil)
}

func TestCopyLibraryBindingsToEnv(t *testing.T) {
	c := qt.New(t)

	// Create source library with runtime and syntax bindings
	srcEnv := environment.NewNamespace().Runtime()
	libName := compilation.NewLibraryName("test", "copylib")
	lib := compilation.NewCompiledLibrary(libName, srcEnv)

	// Add runtime binding (variable)
	foSym := values.NewSymbol("bindSymbolWithScopes")
	_, _ = srcEnv.MaybeCreateOwnGlobalBinding(foSym, environment.BindingTypeVariable, nil)
	fooIdx := srcEnv.GetGlobalIndex(foSym)
	_ = srcEnv.SetOwnGlobalValue(fooIdx, values.NewInteger(42))
	lib.AddExport("bindSymbolWithScopes", "")

	// Add syntax binding (macro)
	barSym := values.NewSymbol("bar")
	expandEnv := srcEnv.Expand()
	_, _ = expandEnv.MaybeCreateOwnGlobalBinding(barSym, environment.BindingTypeSyntax, nil)
	barIdx := expandEnv.GetGlobalIndex(barSym)
	mockMacro := values.NewSymbol("mock-macro")
	_ = expandEnv.SetOwnGlobalValue(barIdx, mockMacro)
	lib.AddExport("bar", "")

	// Create target environment
	targetEnv := environment.NewNamespace().Runtime()

	// Create bindings map (localName -> externalName)
	bindings := map[string]string{
		"bindSymbolWithScopes": "bindSymbolWithScopes",
		"bar":                  "bar",
	}

	// Copy bindings
	err := compilation.CopyLibraryBindingsToEnv(lib, bindings, targetEnv)
	c.Assert(err, qt.IsNil)

	// Verify runtime binding was copied
	fooTarget := values.NewSymbol("bindSymbolWithScopes")
	fooBinding := targetEnv.GetBinding(fooTarget, values.AllScopes())
	c.Assert(fooBinding, qt.IsNotNil)
	c.Assert(fooBinding.Value(), valuestest.SchemeEquals, values.NewInteger(42))

	// Verify syntax binding was copied
	barTarget := values.NewSymbol("bar")
	barBinding := targetEnv.Expand().GetBinding(barTarget, values.AllScopes())
	c.Assert(barBinding, qt.IsNotNil)
	c.Assert(barBinding.BindingType(), qt.Equals, environment.BindingTypeSyntax)
	c.Assert(barBinding.Value(), valuestest.SchemeEquals, mockMacro)
}

func TestCopyLibraryBindingsToEnv_WithRename(t *testing.T) {
	c := qt.New(t)

	// Create source library
	srcEnv := environment.NewNamespace().Runtime()
	libName := compilation.NewLibraryName("test", "renamelib")
	lib := compilation.NewCompiledLibrary(libName, srcEnv)

	// Add binding with internal name different from external
	internalSym := values.NewSymbol("internal-bindSymbolWithScopes")
	_, _ = srcEnv.MaybeCreateOwnGlobalBinding(internalSym, environment.BindingTypeVariable, nil)
	idx := srcEnv.GetGlobalIndex(internalSym)
	_ = srcEnv.SetOwnGlobalValue(idx, values.NewInteger(99))
	lib.AddExport("bindSymbolWithScopes", "internal-bindSymbolWithScopes")

	// Create target environment
	targetEnv := environment.NewNamespace().Runtime()

	// Rename on import: "my-bindSymbolWithScopes" -> "bindSymbolWithScopes"
	bindings := map[string]string{
		"my-bindSymbolWithScopes": "bindSymbolWithScopes",
	}

	err := compilation.CopyLibraryBindingsToEnv(lib, bindings, targetEnv)
	c.Assert(err, qt.IsNil)

	// Verify binding is accessible with local name
	myFooSym := values.NewSymbol("my-bindSymbolWithScopes")
	binding := targetEnv.GetBinding(myFooSym, values.AllScopes())
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.Value(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestCopyLibraryBindingsToEnv_MissingBinding(t *testing.T) {
	c := qt.New(t)

	// Create library with no bindings
	srcEnv := environment.NewNamespace().Runtime()
	libName := compilation.NewLibraryName("test", "empty")
	lib := compilation.NewCompiledLibrary(libName, srcEnv)
	lib.AddExport("missing", "")

	targetEnv := environment.NewNamespace().Runtime()

	bindings := map[string]string{
		"missing": "missing",
	}

	// Should error because binding doesn't exist
	err := compilation.CopyLibraryBindingsToEnv(lib, bindings, targetEnv)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "binding not found")
}

// Tests moved from coverage_additional_test.go
// TestLibraryNameMethodsAdditional tests compilation.LibraryName methods
func TestLibraryNameMethodsAdditional(t *testing.T) {
	ln := compilation.NewLibraryName("scheme", "base")

	qt.Assert(t, ln.String(), qt.Equals, "scheme/base")
	qt.Assert(t, ln.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, ln.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, ln.ToFSPath(), qt.Contains, "scheme")
}

// TestLibraryNameMethods tests compilation.LibraryName methods
func TestLibraryNameMethods(t *testing.T) {
	name := compilation.NewLibraryName("scheme", "base")

	qt.Assert(t, name.String(), qt.Equals, "scheme/base")
	qt.Assert(t, name.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, name.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, name.ToFSPath(), qt.Equals, "scheme/base.sld")
}

// TestLibraryRegistryLookupNotFound tests looking up non-existent library
func TestLibraryRegistryLookupNotFound(t *testing.T) {
	registry := compilation.NewLibraryRegistry()
	lib := registry.Lookup(compilation.NewLibraryName("nonexistent", "lib"))
	qt.Assert(t, lib, qt.IsNil)
}

// TestLibraryRegistryRegister tests LibraryRegistry.Register
func TestLibraryRegistryRegister(t *testing.T) {
	registry := compilation.NewLibraryRegistry()
	name := compilation.NewLibraryName("my", "lib")
	lib := &compilation.CompiledLibrary{Name: name}

	registry.Register(lib) //nolint:errcheck
	result := registry.Lookup(name)
	qt.Assert(t, result, qt.IsNotNil)
	qt.Assert(t, result.Name.Key(), qt.Equals, name.Key())
}

// TestLibraryNamePathConversion tests compilation.LibraryName ToFSPath method
func TestLibraryNamePathConversion(t *testing.T) {
	name1 := compilation.NewLibraryName("scheme", "base")
	name2 := compilation.NewLibraryName("scheme", "base")
	name3 := compilation.NewLibraryName("scheme", "write")

	qt.Assert(t, name1.Key(), qt.Equals, name2.Key())
	qt.Assert(t, name1.Key(), qt.Not(qt.Equals), name3.Key())
	qt.Assert(t, name1.String(), qt.Equals, "scheme/base")
	qt.Assert(t, name1.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, strings.Contains(name1.ToFSPath(), "scheme"), qt.IsTrue)
}

// TestImportSetFields tests ImportSet construction via the modifier builders. Each
// builder appends one ordered modifier; the modifier internals are package-private, so
// this external test asserts the library name and the modifier count (behavioral
// coverage of composition lives in pkg/machine/compilation and pkg/wile).
func TestImportSetFields(t *testing.T) {
	is := compilation.NewImportSet(compilation.NewLibraryName("scheme", "base"))
	is.AddOnly(values.StringSet{"car": {}, "cdr": {}})
	is.AddExcept(values.StringSet{"cons": {}})
	is.AddPrefix("my-")
	is.AddRename(map[string]string{"old": "new"})

	qt.Assert(t, is.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, is.Modifiers, qt.HasLen, 4)
}

// TestLibraryNameToFSPath tests LibraryName.ToFSPath method
func TestLibraryNameToFSPath(t *testing.T) {
	ln := compilation.NewLibraryName("scheme", "base")
	qt.Assert(t, strings.Contains(ln.ToFSPath(), "scheme"), qt.IsTrue)
}

// TestLibraryRegistryRegisterAndLookupAdditional tests Register and Lookup
func TestLibraryRegistryRegisterAndLookupAdditional(t *testing.T) {
	reg := compilation.NewLibraryRegistry()
	env := environment.NewNamespace().Runtime()
	lib := compilation.NewCompiledLibrary(compilation.NewLibraryName("test", "mylib"), env)
	reg.Register(lib) //nolint:errcheck

	// Lookup existing
	found := reg.Lookup(compilation.NewLibraryName("test", "mylib"))
	qt.Assert(t, found, qt.IsNotNil)
	qt.Assert(t, found.Name.String(), qt.Equals, "test/mylib")

	// Lookup non-existing
	notFound := reg.Lookup(compilation.NewLibraryName("nonexistent"))
	qt.Assert(t, notFound, qt.IsNil)
}

// TestCopyLibraryBindingsToEnv_AmbientKeyword: an auxiliary keyword a library
// exports (else, =>) lives at the library env's AMBIENT coordinate, which is
// where registry apply installs it. findLibraryBinding finds it at phase 0
// (ambient is a candidate at every phase and the walk is ascending), so the import
// installs it once, at the importer's phase 0, and propagates nothing: the
// importer gains no phase 2. Before the relocation the source sat at phase 2 and
// the import mirrored it there.
//
// A PIN, not a gate: the ambient `else` is built by hand, so this passes before
// the relocation too. It records the import path's shape for a keyword found at
// phase 0; TestApply_CompileTimeBinding is what goes red without the move.
func TestCopyLibraryBindingsToEnv_AmbientKeyword(t *testing.T) {
	c := qt.New(t)

	srcEnv := environment.NewNamespace().Runtime()
	libName := compilation.NewLibraryName("test", "auxlib")
	lib := compilation.NewCompiledLibrary(libName, srcEnv)

	elseSym := values.NewSymbol("else")
	ambient := srcEnv.SealedWriteViewAt(environment.PhaseRuntime)
	_, _ = ambient.MaybeCreateOwnGlobalBinding(elseSym, environment.BindingTypePrimitive, nil)
	lib.AddExport("else", "")

	targetEnv := environment.NewNamespace().Runtime()
	err := compilation.CopyLibraryBindingsToEnv(lib, map[string]string{"else": "else"}, targetEnv)
	c.Assert(err, qt.IsNil)

	got := targetEnv.GetBinding(elseSym, values.AllScopes())
	c.Assert(got, qt.IsNotNil)
	c.Assert(got.BindingType(), qt.Equals, environment.BindingTypePrimitive)
	c.Assert(got.IsImported(), qt.IsTrue)
	c.Assert(targetEnv.PresentPhases(), qt.DeepEquals, []environment.Phase{environment.PhaseRuntime})
}

// TestLibraryForwardReferences tests that library bodies support forward references
// per R7RS §5.3.2: Internal definitions use letrec* semantics.
//
// Prior to the fix, this would fail with:
// "no such local or global binding \"callee\"" because caller references callee
// before callee is defined.
func TestLibraryForwardReferences(t *testing.T) {
	c := qt.New(t)

	// Create a library with forward references in the begin body:
	// 'caller' references 'callee' before 'callee' is defined
	// This uses only primitives available in the test environment (no external imports)
	libraryCode := `
	(define-library (test forward-refs)
	  (export caller callee)
	  (begin
	    (define (caller x)
	      (callee x))
	    (define (callee y)
	      y)))
	`

	// Set up environment with library registry
	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)
	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	// Set up library registry
	registry := compilation.NewLibraryRegistry()
	env.SetLibraryRegistry(registry)

	// Parse the library definition
	stx := parseLibrarySyntax(t, env, libraryCode)

	// Create compiler and expand the library definition
	ectx := context.Background()
	expanded, err := compilation.NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
	c.Assert(err, qt.IsNil)

	// Compile the library - this should succeed with forward references
	// Before the letrec* semantics fix, this would fail with:
	// "no such local or global binding \"callee\""
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	tpl := machine.NewNativeTemplate(0, 0, false)
	compiler := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	err = compiler.CompileExpression(ctctx, expanded)
	c.Assert(err, qt.IsNil, qt.Commentf("Forward references should work with letrec* semantics"))
}

// TestLoadLibrary_IncludeStampsLibraryScope verifies that forms loaded via
// (include ...) inside define-library receive the library scope (Flatt §3.3).
// The library defines base-value in an included file and a macro double-base
// in (begin ...) that references it. This exercises the invariant that include
// is semantically equivalent to textual insertion into the library body.
func TestLoadLibrary_IncludeStampsLibraryScope(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Load the library that uses (include "include-body.scm")
	name := compilation.NewLibraryName("test", "include-lib")
	lib, err := compilation.LoadLibrary(context.Background(), name, env, machine.NewVMMacroEvaluator())
	c.Assert(err, qt.IsNil)
	c.Assert(lib, qt.IsNotNil)

	// Verify exports exist
	c.Assert(lib.IsExported("double-base"), qt.IsTrue)
	c.Assert(lib.IsExported("base-value"), qt.IsTrue)

	// Import and call: (double-base) should return 14 (* 2 7).
	// double-base is a macro defined in (begin ...) whose template references
	// base-value from the included file. The macro template's free identifier
	// base-value gets resolved during compilation with the library scope.
	// Without scope stamping on included forms, base-value's binding has
	// empty scopes, and the scoped free-identifier reference fails to find it.
	importDef := parseLibrarySyntax(t, env, `(import (test include-lib))`)
	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err = ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Compile and run (double-base)
	callExpr := parseLibrarySyntax(t, env, `(double-base)`)
	callTpl := machine.NewNativeTemplate(0, 0, false)
	callCtc := compilation.NewCompileTimeContinuation(callTpl, env, machine.NewVMMacroEvaluator())
	callCtctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	expanded, err := compilation.NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator()).ExpandExpression(callExpr)
	c.Assert(err, qt.IsNil)

	err = callCtc.CompileExpression(callCtctx, expanded)
	c.Assert(err, qt.IsNil)

	mc := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, callTpl, env),
	)
	err = mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(14))
}

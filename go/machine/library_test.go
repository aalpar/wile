// Copyright 2025 Aaron Alpar
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

	"wile/environment"
	"wile/machine"
	"wile/parser"
	schemertime "wile/runtime"
	"wile/syntax"
	"wile/values"

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
	p := parser.NewParser(env, reader)
	stx, err := p.ReadSyntax(nil)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	return stx
}

func TestLibraryName(t *testing.T) {
	c := qt.New(t)

	// Test LibraryName creation and methods
	name := machine.NewLibraryName("scheme", "base")
	c.Assert(name.String(), qt.Equals, "scheme/base")
	c.Assert(name.SchemeString(), qt.Equals, "(scheme base)")
	c.Assert(name.Key(), qt.Equals, "scheme/base")
	c.Assert(name.ToFilePath(), qt.Equals, "scheme/base.sld")

	// Test multi-part name
	name2 := machine.NewLibraryName("my", "lib", "utils")
	c.Assert(name2.String(), qt.Equals, "my/lib/utils")
	c.Assert(name2.SchemeString(), qt.Equals, "(my lib utils)")
}

func TestLibraryRegistry(t *testing.T) {
	c := qt.New(t)

	registry := machine.NewLibraryRegistry()

	// Test that default search paths are set
	paths := registry.GetSearchPaths()
	c.Assert(len(paths), qt.Equals, 2)
	c.Assert(paths[0], qt.Equals, ".")
	c.Assert(paths[1], qt.Equals, "./lib")

	// Test SetSearchPaths
	registry.SetSearchPaths([]string{"/custom/path"})
	c.Assert(registry.GetSearchPaths(), qt.DeepEquals, []string{"/custom/path"})

	// Test AddSearchPath (prepends)
	registry.AddSearchPath("/another/path")
	c.Assert(registry.GetSearchPaths()[0], qt.Equals, "/another/path")
}

func TestCompiledLibrary(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironmentFrame()
	name := machine.NewLibraryName("test", "lib")
	lib := machine.NewCompiledLibrary(name, env)

	// Test empty exports initially
	c.Assert(lib.IsExported("foo"), qt.IsFalse)

	// Test AddExport with same internal/external name
	lib.AddExport("foo", "")
	c.Assert(lib.IsExported("foo"), qt.IsTrue)
	c.Assert(lib.GetInternalName("foo"), qt.Equals, "foo")

	// Test AddExport with rename
	lib.AddExport("bar", "internal-bar")
	c.Assert(lib.IsExported("bar"), qt.IsTrue)
	c.Assert(lib.GetInternalName("bar"), qt.Equals, "internal-bar")
}

func TestImportSet(t *testing.T) {
	c := qt.New(t)

	// Create a library with exports
	env := environment.NewTopLevelEnvironmentFrame()
	name := machine.NewLibraryName("test", "lib")
	lib := machine.NewCompiledLibrary(name, env)
	lib.AddExport("foo", "")
	lib.AddExport("bar", "")
	lib.AddExport("baz", "")

	// Test basic import set (all exports)
	importSet := machine.NewImportSet(name)
	bindings, err := importSet.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(len(bindings), qt.Equals, 3)
	c.Assert(bindings["foo"], qt.Equals, "foo")
	c.Assert(bindings["bar"], qt.Equals, "bar")
	c.Assert(bindings["baz"], qt.Equals, "baz")

	// Test 'only' filter
	importSet2 := machine.NewImportSet(name)
	importSet2.Only = []string{"foo", "bar"}
	bindings2, err := importSet2.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(len(bindings2), qt.Equals, 2)
	c.Assert(bindings2["foo"], qt.Equals, "foo")
	c.Assert(bindings2["bar"], qt.Equals, "bar")

	// Test 'except' filter
	importSet3 := machine.NewImportSet(name)
	importSet3.Except = []string{"baz"}
	bindings3, err := importSet3.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(len(bindings3), qt.Equals, 2)

	// Test 'prefix' modifier
	importSet4 := machine.NewImportSet(name)
	importSet4.Prefix = "my:"
	bindings4, err := importSet4.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(bindings4["my:foo"], qt.Equals, "foo")
	c.Assert(bindings4["my:bar"], qt.Equals, "bar")

	// Test 'rename' modifier
	importSet5 := machine.NewImportSet(name)
	importSet5.Renames = map[string]string{"foo": "renamed-foo"}
	bindings5, err := importSet5.ApplyToExports(lib)
	c.Assert(err, qt.IsNil)
	c.Assert(bindings5["renamed-foo"], qt.Equals, "foo")
	c.Assert(bindings5["bar"], qt.Equals, "bar")
}

func TestImportSetErrors(t *testing.T) {
	c := qt.New(t)

	// Create a library with limited exports
	env := environment.NewTopLevelEnvironmentFrame()
	name := machine.NewLibraryName("test", "lib")
	lib := machine.NewCompiledLibrary(name, env)
	lib.AddExport("foo", "")

	// Test 'only' with non-existent identifier
	importSet := machine.NewImportSet(name)
	importSet.Only = []string{"nonexistent"}
	_, err := importSet.ApplyToExports(lib)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "nonexistent")

	// Test 'except' with non-existent identifier
	importSet2 := machine.NewImportSet(name)
	importSet2.Except = []string{"nonexistent"}
	_, err2 := importSet2.ApplyToExports(lib)
	c.Assert(err2, qt.IsNotNil)
}

func TestCompileDefineLibrary_Basic(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Parse a simple library definition
	libDef := parseLibrarySyntax(t, env, `
		(define-library (my test lib)
		  (export foo)
		  (begin
		    (define foo 42)))
	`)

	// Extract args after 'define-library' keyword
	libPair := libDef.(*syntax.SyntaxPair)
	args := libPair.Cdr().(*syntax.SyntaxPair)

	// Compile the library
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileDefineLibrary(ctctx, args)
	qt.Assert(t, err, qt.IsNil)
}

func TestCompileDefineLibrary_Empty(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Parse an empty library definition
	libDef := parseLibrarySyntax(t, env, `(define-library (empty lib))`)

	// Extract args
	libPair := libDef.(*syntax.SyntaxPair)
	args := libPair.Cdr().(*syntax.SyntaxPair)

	// Compile
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileDefineLibrary(ctctx, args)
	qt.Assert(t, err, qt.IsNil)
}

func TestCompileImport_LibraryNotFound(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Set up a library registry (required for imports)
	registry := machine.NewLibraryRegistry()
	env.SetLibraryRegistry(registry)

	// Parse an import declaration
	importDef := parseLibrarySyntax(t, env, `(import (scheme base))`)

	// Extract args
	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	// Compile - should fail because (scheme base) doesn't exist
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ctctx, args)
	qt.Assert(t, err, qt.IsNotNil) // Library not found
	qt.Assert(t, err.Error(), qt.Contains, "not found")
}

func TestCompileImport_NoRegistry(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	// Intentionally NOT setting up a library registry

	// Parse an import declaration
	importDef := parseLibrarySyntax(t, env, `(import (scheme base))`)

	// Extract args
	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	// Compile - should fail because no registry is configured
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ctctx, args)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no library registry")
}

func TestCompileExport_TopLevelError(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Parse an export declaration
	exportDef := parseLibrarySyntax(t, env, `(export foo bar)`)

	// Extract args
	exportPair := exportDef.(*syntax.SyntaxPair)
	args := exportPair.Cdr().(syntax.SyntaxValue)

	// Compile - should error at top level
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

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

	// Set up the factory for creating library environments
	machine.LibraryEnvFactory = schemertime.NewTopLevelEnvironmentFrameTiny

	// Create the top-level environment
	env, err := schemertime.NewTopLevelEnvironmentFrameTiny()
	if err != nil {
		t.Fatalf("failed to create environment: %v", err)
	}

	// Create and configure the library registry
	registry := machine.NewLibraryRegistry()
	registry.SetSearchPaths([]string{getTestdataPath()})
	env.SetLibraryRegistry(registry)

	return env
}

func TestLoadLibrary_Simple(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Load the simple library
	name := machine.NewLibraryName("test", "simple")
	lib, err := machine.LoadLibrary(context.Background(), name, env)
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
	name := machine.NewLibraryName("test", "simple")
	lib1, err := machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNil)

	lib2, err := machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNil)

	// Should return the same cached library
	c.Assert(lib1, qt.Equals, lib2)
}

func TestLoadLibrary_WithImports(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Load the importer library (which imports test/simple)
	name := machine.NewLibraryName("test", "importer")
	lib, err := machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNil)
	c.Assert(lib, qt.IsNotNil)

	// Verify exports
	c.Assert(lib.IsExported("quadruple"), qt.IsTrue)
	c.Assert(lib.IsExported("get-secret"), qt.IsTrue)

	// Verify that the dependency was also loaded
	registry := env.LibraryRegistry().(*machine.LibraryRegistry)
	simpleName := machine.NewLibraryName("test", "simple")
	simpleLib := registry.Lookup(simpleName)
	c.Assert(simpleLib, qt.IsNotNil)
}

func TestLoadLibrary_CircularDependency(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Try to load a library with circular dependency
	name := machine.NewLibraryName("test", "circular-a")
	_, err := machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "circular")
}

func TestLoadLibrary_NotFound(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Try to load a non-existent library
	name := machine.NewLibraryName("nonexistent", "lib")
	_, err := machine.LoadLibrary(context.Background(), name, env)
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
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify that the exported names are now bound in the environment
	makeAdder := env.InternSymbol(values.NewSymbol("make-adder"))
	c.Assert(env.GetBinding(makeAdder), qt.IsNotNil)

	double := env.InternSymbol(values.NewSymbol("double"))
	c.Assert(env.GetBinding(double), qt.IsNotNil)

	secretValue := env.InternSymbol(values.NewSymbol("secret-value"))
	c.Assert(env.GetBinding(secretValue), qt.IsNotNil)
}

func TestImport_OnlyModifier(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Parse and compile an import with 'only' modifier
	importDef := parseLibrarySyntax(t, env, `(import (only (test simple) double))`)

	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify only 'double' is bound
	double := env.InternSymbol(values.NewSymbol("double"))
	c.Assert(env.GetBinding(double), qt.IsNotNil)

	makeAdder := env.InternSymbol(values.NewSymbol("make-adder"))
	c.Assert(env.GetBinding(makeAdder), qt.IsNil)
}

func TestImport_PrefixModifier(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	// Parse and compile an import with 'prefix' modifier
	importDef := parseLibrarySyntax(t, env, `(import (prefix (test simple) test:))`)

	importPair := importDef.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify prefixed names are bound
	testDouble := env.InternSymbol(values.NewSymbol("test:double"))
	c.Assert(env.GetBinding(testDouble), qt.IsNotNil)

	// Verify unprefixed names are NOT bound
	double := env.InternSymbol(values.NewSymbol("double"))
	c.Assert(env.GetBinding(double), qt.IsNil)
}

func TestCopyLibraryBindingsToEnv(t *testing.T) {
	c := qt.New(t)

	// Create source library with runtime and syntax bindings
	srcEnv := environment.NewTopLevelEnvironmentFrame()
	libName := machine.NewLibraryName("test", "copylib")
	lib := machine.NewCompiledLibrary(libName, srcEnv)

	// Add runtime binding (variable)
	foSym := srcEnv.InternSymbol(values.NewSymbol("foo"))
	_, _ = srcEnv.MaybeCreateOwnGlobalBinding(foSym, environment.BindingTypeVariable)
	fooIdx := srcEnv.GetGlobalIndex(foSym)
	_ = srcEnv.SetOwnGlobalValue(fooIdx, values.NewInteger(42))
	lib.AddExport("foo", "")

	// Add syntax binding (macro)
	barSym := srcEnv.InternSymbol(values.NewSymbol("bar"))
	expandEnv := srcEnv.Expand()
	_, _ = expandEnv.MaybeCreateOwnGlobalBinding(barSym, environment.BindingTypeSyntax)
	barIdx := expandEnv.GetGlobalIndex(barSym)
	mockMacro := values.NewSymbol("mock-macro")
	_ = expandEnv.SetOwnGlobalValue(barIdx, mockMacro)
	lib.AddExport("bar", "")

	// Create target environment
	targetEnv := environment.NewTopLevelEnvironmentFrame()

	// Create bindings map (localName -> externalName)
	bindings := map[string]string{
		"foo": "foo",
		"bar": "bar",
	}

	// Copy bindings
	err := machine.CopyLibraryBindingsToEnv(lib, bindings, targetEnv)
	c.Assert(err, qt.IsNil)

	// Verify runtime binding was copied
	fooTarget := targetEnv.InternSymbol(values.NewSymbol("foo"))
	fooBinding := targetEnv.GetBinding(fooTarget)
	c.Assert(fooBinding, qt.IsNotNil)
	c.Assert(fooBinding.Value(), values.SchemeEquals, values.NewInteger(42))

	// Verify syntax binding was copied
	barTarget := targetEnv.InternSymbol(values.NewSymbol("bar"))
	barBinding := targetEnv.Expand().GetBinding(barTarget)
	c.Assert(barBinding, qt.IsNotNil)
	c.Assert(barBinding.BindingType(), qt.Equals, environment.BindingTypeSyntax)
	c.Assert(barBinding.Value(), values.SchemeEquals, mockMacro)
}

func TestCopyLibraryBindingsToEnv_WithRename(t *testing.T) {
	c := qt.New(t)

	// Create source library
	srcEnv := environment.NewTopLevelEnvironmentFrame()
	libName := machine.NewLibraryName("test", "renamelib")
	lib := machine.NewCompiledLibrary(libName, srcEnv)

	// Add binding with internal name different from external
	internalSym := srcEnv.InternSymbol(values.NewSymbol("internal-foo"))
	_, _ = srcEnv.MaybeCreateOwnGlobalBinding(internalSym, environment.BindingTypeVariable)
	idx := srcEnv.GetGlobalIndex(internalSym)
	_ = srcEnv.SetOwnGlobalValue(idx, values.NewInteger(99))
	lib.AddExport("foo", "internal-foo")

	// Create target environment
	targetEnv := environment.NewTopLevelEnvironmentFrame()

	// Rename on import: "my-foo" -> "foo"
	bindings := map[string]string{
		"my-foo": "foo",
	}

	err := machine.CopyLibraryBindingsToEnv(lib, bindings, targetEnv)
	c.Assert(err, qt.IsNil)

	// Verify binding is accessible with local name
	myFooSym := targetEnv.InternSymbol(values.NewSymbol("my-foo"))
	binding := targetEnv.GetBinding(myFooSym)
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.Value(), values.SchemeEquals, values.NewInteger(99))
}

func TestCopyLibraryBindingsToEnv_MissingBinding(t *testing.T) {
	c := qt.New(t)

	// Create library with no bindings
	srcEnv := environment.NewTopLevelEnvironmentFrame()
	libName := machine.NewLibraryName("test", "empty")
	lib := machine.NewCompiledLibrary(libName, srcEnv)
	lib.AddExport("missing", "")

	targetEnv := environment.NewTopLevelEnvironmentFrame()

	bindings := map[string]string{
		"missing": "missing",
	}

	// Should error because binding doesn't exist
	err := machine.CopyLibraryBindingsToEnv(lib, bindings, targetEnv)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "binding not found")
}

// Tests moved from coverage_additional_test.go
// TestLibraryNameMethodsAdditional tests LibraryName methods
func TestLibraryNameMethodsAdditional(t *testing.T) {
	ln := machine.NewLibraryName("scheme", "base")

	qt.Assert(t, ln.String(), qt.Equals, "scheme/base")
	qt.Assert(t, ln.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, ln.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, ln.ToFilePath(), qt.Contains, "scheme")
}

// TestLibraryNameMethods tests LibraryName methods
func TestLibraryNameMethods(t *testing.T) {
	name := machine.NewLibraryName("scheme", "base")

	qt.Assert(t, name.String(), qt.Equals, "scheme/base")
	qt.Assert(t, name.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, name.Key(), qt.Equals, "scheme/base")            // Key uses "/" separator
	qt.Assert(t, name.ToFilePath(), qt.Equals, "scheme/base.sld") // Includes .sld extension
}

// TestLibraryRegistryLookupNotFound tests looking up non-existent library
func TestLibraryRegistryLookupNotFound(t *testing.T) {
	registry := machine.NewLibraryRegistry()
	lib := registry.Lookup(machine.NewLibraryName("nonexistent", "lib"))
	qt.Assert(t, lib, qt.IsNil)
}

// TestLibraryRegistryRegister tests LibraryRegistry.Register
func TestLibraryRegistryRegister(t *testing.T) {
	registry := machine.NewLibraryRegistry()
	name := machine.NewLibraryName("my", "lib")
	lib := &machine.CompiledLibrary{Name: name}

	registry.Register(lib) //nolint:errcheck
	result := registry.Lookup(name)
	qt.Assert(t, result, qt.IsNotNil)
	qt.Assert(t, result.Name.Key(), qt.Equals, name.Key())
}

// TestLibraryNamePathConversion tests LibraryName ToFilePath method
func TestLibraryNamePathConversion(t *testing.T) {
	name1 := machine.NewLibraryName("scheme", "base")
	name2 := machine.NewLibraryName("scheme", "base")
	name3 := machine.NewLibraryName("scheme", "write")

	qt.Assert(t, name1.Key(), qt.Equals, name2.Key())
	qt.Assert(t, name1.Key(), qt.Not(qt.Equals), name3.Key())
	qt.Assert(t, name1.String(), qt.Equals, "scheme/base")
	qt.Assert(t, name1.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, strings.Contains(name1.ToFilePath(), "scheme"), qt.IsTrue)
}

// TestImportSetFields tests ImportSet fields
func TestImportSetFields(t *testing.T) {
	is := &machine.ImportSet{
		LibraryName: machine.NewLibraryName("scheme", "base"),
		Only:        []string{"car", "cdr"},
		Except:      []string{"cons"},
		Prefix:      "my-",
		Renames:     map[string]string{"old": "new"},
	}
	qt.Assert(t, is.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, len(is.Only), qt.Equals, 2)
	qt.Assert(t, len(is.Except), qt.Equals, 1)
	qt.Assert(t, is.Prefix, qt.Equals, "my-")
	qt.Assert(t, is.Renames["old"], qt.Equals, "new")
}

// TestLibraryNameToFilePath tests LibraryName.ToFilePath method
func TestLibraryNameToFilePath(t *testing.T) {
	ln := machine.NewLibraryName("scheme", "base")
	qt.Assert(t, strings.Contains(ln.ToFilePath(), "scheme"), qt.IsTrue)
}

// TestLibraryRegistryRegisterAndLookupAdditional tests Register and Lookup
func TestLibraryRegistryRegisterAndLookupAdditional(t *testing.T) {
	reg := machine.NewLibraryRegistry()
	env := environment.NewTopLevelEnvironmentFrame()
	lib := machine.NewCompiledLibrary(machine.NewLibraryName("test", "mylib"), env)
	reg.Register(lib) //nolint:errcheck

	// Lookup existing
	found := reg.Lookup(machine.NewLibraryName("test", "mylib"))
	qt.Assert(t, found, qt.IsNotNil)
	qt.Assert(t, found.Name.String(), qt.Equals, "test/mylib")

	// Lookup non-existing
	notFound := reg.Lookup(machine.NewLibraryName("nonexistent"))
	qt.Assert(t, notFound, qt.IsNil)
}

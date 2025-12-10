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
	stx, err := p.ReadSyntax()
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileDefineLibrary(ccnt, args)
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileDefineLibrary(ccnt, args)
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ccnt, args)
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ccnt, args)
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileExport(ccnt, args)
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ccnt, args)
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ccnt, args)
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
	ccnt := machine.NewCompileTimeCallContext(false, false, env)

	err := ctc.CompileImport(ccnt, args)
	c.Assert(err, qt.IsNil)

	// Verify prefixed names are bound
	testDouble := env.InternSymbol(values.NewSymbol("test:double"))
	c.Assert(env.GetBinding(testDouble), qt.IsNotNil)

	// Verify unprefixed names are NOT bound
	double := env.InternSymbol(values.NewSymbol("double"))
	c.Assert(env.GetBinding(double), qt.IsNil)
}

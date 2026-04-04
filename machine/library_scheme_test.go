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
	"bufio"
	"context"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// getSchemeLibPath returns the path to the stdlib/lib/ directory containing scheme libraries
func getSchemeLibPath() string {
	_, filename, _, _ := runtime.Caller(0)
	// machine/library_scheme_test.go -> stdlib/lib/
	return filepath.Join(filepath.Dir(filename), "..", "stdlib", "lib")
}

// setupSchemeLibraryTest sets up a test environment with access to scheme libraries
func setupSchemeLibraryTest(t *testing.T) *environment.EnvironmentFrame {
	t.Helper()

	// Create the top-level environment
	env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
	if err != nil {
		t.Fatalf("failed to create environment: %v", err)
	}

	// Set up the factory for creating library environments
	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	// Create and configure the library registry with the scheme lib path
	registry := compilation.NewLibraryRegistry()
	registry.SetSearchPaths([]string{getSchemeLibPath()})
	env.SetLibraryRegistry(registry)

	return env
}

// parseSchemeExpr parses a Scheme expression from a string
func parseSchemeExpr(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	t.Helper()
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, true, reader)
	sv, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	return sv
}

// compileAndRun compiles and runs a Scheme expression
func compileAndRun(t *testing.T, env *environment.EnvironmentFrame, sv syntax.SyntaxValue) (values.Value, error) {
	t.Helper()

	// Expand the expression
	econt := compilation.NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	expanded, err := econt.ExpandExpression(sv)
	if err != nil {
		return nil, err
	}

	// Compile the expanded expression
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	err = ctc.CompileExpression(ctctx, expanded)
	if err != nil {
		return nil, err
	}

	// Run
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}

// TestSchemeLibraryImports tests importing all 13 R7RS scheme libraries in a single import expression.
// This simulates running a Scheme source file with:
//
//	(import (scheme base) (scheme char) (scheme lazy)
//	        (scheme inexact) (scheme complex) (scheme time)
//	        (scheme file) (scheme read) (scheme write)
//	        (scheme eval) (scheme process-context) (scheme case-lambda)
//	        (scheme r5rs))
func TestSchemeLibraryImports(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	// Import all 13 scheme libraries in a single import expression
	importCode := `(import (scheme base) (scheme char) (scheme lazy)
	        (scheme inexact) (scheme complex) (scheme time)
	        (scheme file) (scheme read) (scheme write)
	        (scheme eval) (scheme process-context) (scheme case-lambda)
	        (scheme r5rs))`

	sv := parseSchemeExpr(t, env, importCode)

	// Extract args after 'import' keyword
	importPair := sv.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	// Compile the import
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)

	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil, qt.Commentf("import of all scheme libraries failed"))

	// Verify some key bindings from each library are available

	// (scheme base) - car, cdr, cons, list, etc.
	car := values.NewSymbol("car")
	c.Assert(env.GetBinding(car, nil), qt.IsNotNil, qt.Commentf("car not found from (scheme base)"))

	// (scheme char) - char-upcase, char-downcase, etc.
	charUpcase := values.NewSymbol("char-upcase")
	c.Assert(env.GetBinding(charUpcase, nil), qt.IsNotNil, qt.Commentf("char-upcase not found from (scheme char)"))

	// (scheme lazy) - delay, force, promise?
	force := values.NewSymbol("force")
	c.Assert(env.GetBinding(force, nil), qt.IsNotNil, qt.Commentf("force not found from (scheme lazy)"))

	// (scheme inexact) - exp, log, sin, cos, etc.
	exp := values.NewSymbol("exp")
	c.Assert(env.GetBinding(exp, nil), qt.IsNotNil, qt.Commentf("exp not found from (scheme inexact)"))

	// (scheme complex) - make-rectangular, make-polar, etc.
	makeRectangular := values.NewSymbol("make-rectangular")
	c.Assert(env.GetBinding(makeRectangular, nil), qt.IsNotNil, qt.Commentf("make-rectangular not found from (scheme complex)"))

	// (scheme time) - current-second, current-jiffy, etc.
	currentSecond := values.NewSymbol("current-second")
	c.Assert(env.GetBinding(currentSecond, nil), qt.IsNotNil, qt.Commentf("current-second not found from (scheme time)"))

	// (scheme file) - open-input-file, open-output-file, etc.
	openInputFile := values.NewSymbol("open-input-file")
	c.Assert(env.GetBinding(openInputFile, nil), qt.IsNotNil, qt.Commentf("open-input-file not found from (scheme file)"))

	// (scheme read) - read
	read := values.NewSymbol("read")
	c.Assert(env.GetBinding(read, nil), qt.IsNotNil, qt.Commentf("read not found from (scheme read)"))

	// (scheme write) - write, display
	write := values.NewSymbol("write")
	c.Assert(env.GetBinding(write, nil), qt.IsNotNil, qt.Commentf("write not found from (scheme write)"))

	// (scheme eval) - eval, environment
	eval := values.NewSymbol("eval")
	c.Assert(env.GetBinding(eval, nil), qt.IsNotNil, qt.Commentf("eval not found from (scheme eval)"))

	// (scheme process-context) - command-line, exit, get-environment-variable
	commandLine := values.NewSymbol("command-line")
	c.Assert(env.GetBinding(commandLine, nil), qt.IsNotNil, qt.Commentf("command-line not found from (scheme process-context)"))

	// (scheme case-lambda) - case-lambda (syntax)
	// Note: case-lambda is a syntax binding, so check expand environment
	caseLambda := values.NewSymbol("case-lambda")
	caseLambdaBinding := env.Expand().GetBinding(caseLambda, nil)
	c.Assert(caseLambdaBinding, qt.IsNotNil, qt.Commentf("case-lambda not found from (scheme case-lambda)"))

	// (scheme r5rs) - provides R5RS compatibility
	// Check for null-environment which is R5RS-specific
	nullEnvironment := values.NewSymbol("null-environment")
	c.Assert(env.GetBinding(nullEnvironment, nil), qt.IsNotNil, qt.Commentf("null-environment not found from (scheme r5rs)"))
}

// TestSchemeLibraryImportsWithUsage tests that imported bindings actually work
func TestSchemeLibraryImportsWithUsage(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	// Import scheme base and char
	importCode := `(import (scheme base) (scheme char) (scheme inexact))`
	sv := parseSchemeExpr(t, env, importCode)
	importPair := sv.(*syntax.SyntaxPair)
	args := importPair.Cdr().(syntax.SyntaxValue)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	err := ctc.CompileImport(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Test (scheme base) - list operations
	sv = parseSchemeExpr(t, env, "(car '(1 2 3))")
	result, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(1))

	// Test (scheme char) - char-upcase
	sv = parseSchemeExpr(t, env, "(char-upcase #\\a)")
	result, err = compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewCharacter('A'))

	// Test (scheme inexact) - exp
	sv = parseSchemeExpr(t, env, "(exp 0)")
	result, err = compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewFloat(1.0))
}

// TestLibraryInternalMacroHygiene tests that macros defined in a library can
// reference non-exported helper functions. The macro's free identifier references
// should resolve in the library's definition-site environment, not the use-site
// environment where the helper doesn't exist.
//
// This is the core cross-library hygiene scenario: GlobalIndex.Env records
// the definition-site global frame so the VM can look up the helper binding
// directly in the library environment.
func TestLibraryInternalMacroHygiene(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	// Step 1: Compile and execute a library with a non-exported helper
	// and a macro that references it. We replicate what loadLibraryFromFile
	// does: compile with a library callback, execute the template, register.
	libCode := `(define-library (test hygiene-lib)
	  (export my-macro)
	  (import (scheme base))
	  (begin
	    (define (helper x) (+ x 1))
	    (define-syntax my-macro
	      (syntax-rules ()
	        ((my-macro x) (helper x))))))`

	sv := parseSchemeExpr(t, env, libCode)

	// Expand
	ectx := context.Background()
	expanded, err := compilation.NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator()).ExpandExpression(sv)
	c.Assert(err, qt.IsNil, qt.Commentf("library expansion should succeed"))

	// Compile with library callback to capture the compilation.CompiledLibrary
	tpl := machine.NewNativeTemplate(0, 0, false)
	compiler := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	var compiledLib *compilation.CompiledLibrary
	compiler.SetLibraryCallback(func(lib *compilation.CompiledLibrary) {
		compiledLib = lib
	})
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	err = compiler.CompileExpression(ctctx, expanded)
	c.Assert(err, qt.IsNil, qt.Commentf("library compilation should succeed"))
	c.Assert(compiledLib, qt.IsNotNil, qt.Commentf("library callback should have been called"))

	// Execute the library template to populate bindings
	if compiledLib.Template != nil {
		cont := machine.NewMachineContinuation(nil, compiledLib.Template, compiledLib.Env)
		mc := machine.NewMachineContext(context.Background(), cont)
		err = mc.Run()
		c.Assert(err, qt.IsNil, qt.Commentf("library execution should succeed"))
	}

	// Register the library so import can find it
	registryAny := env.LibraryRegistry()
	registry := registryAny.(*compilation.LibraryRegistry)
	err = registry.Register(compiledLib)
	c.Assert(err, qt.IsNil)

	// Step 2: Import the library into the caller environment
	importCode := `(import (test hygiene-lib))`
	sv = parseSchemeExpr(t, env, importCode)
	_, err = compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil, qt.Commentf("import should succeed"))

	// Step 3: Use the macro — helper is not exported, but the macro's
	// GlobalIndex.Env should point to the library's global frame
	useCode := `(my-macro 5)`
	sv = parseSchemeExpr(t, env, useCode)
	result, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil, qt.Commentf("macro using non-exported helper should work"))
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(6))
}

// TestIndividualSchemeLibraries tests each scheme library can be imported individually
func TestIndividualSchemeLibraries(t *testing.T) {
	libraries := []struct {
		name   string
		verify string // a binding to check
	}{
		{"base", "car"},
		{"char", "char-upcase"},
		{"lazy", "force"},
		{"inexact", "exp"},
		{"complex", "make-rectangular"},
		{"time", "current-second"},
		{"file", "open-input-file"},
		{"read", "read"},
		{"write", "write"},
		{"eval", "eval"},
		{"process-context", "command-line"},
		{"case-lambda", "case-lambda"}, // syntax binding
		{"r5rs", "null-environment"},
	}

	for _, lib := range libraries {
		t.Run(lib.name, func(t *testing.T) {
			c := qt.New(t)
			env := setupSchemeLibraryTest(t)

			importCode := "(import (scheme " + lib.name + "))"
			sv := parseSchemeExpr(t, env, importCode)
			importPair := sv.(*syntax.SyntaxPair)
			args := importPair.Cdr().(syntax.SyntaxValue)

			tpl := machine.NewNativeTemplate(0, 0, false)
			ctc := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
			ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
			err := ctc.CompileImport(ctctx, args)
			c.Assert(err, qt.IsNil, qt.Commentf("failed to import (scheme %s)", lib.name))

			// Verify the expected binding exists
			sym := values.NewSymbol(lib.verify)

			// Check both runtime and expand environments
			binding := env.GetBinding(sym, nil)
			if binding == nil {
				binding = env.Expand().GetBinding(sym, nil)
			}
			c.Assert(binding, qt.IsNotNil,
				qt.Commentf("%s not found after importing (scheme %s)", lib.verify, lib.name))
		})
	}
}

// compileAndRegisterLibrary is a test helper that compiles a library definition,
// executes its template to populate bindings, and registers it in the registry.
func compileAndRegisterLibrary(t *testing.T, env *environment.EnvironmentFrame, libCode string) *compilation.CompiledLibrary {
	t.Helper()
	c := qt.New(t)

	sv := parseSchemeExpr(t, env, libCode)
	expanded, err := compilation.NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator()).ExpandExpression(sv)
	c.Assert(err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	compiler := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	var compiledLib *compilation.CompiledLibrary
	compiler.SetLibraryCallback(func(lib *compilation.CompiledLibrary) {
		compiledLib = lib
	})
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	err = compiler.CompileExpression(ctctx, expanded)
	c.Assert(err, qt.IsNil)
	c.Assert(compiledLib, qt.IsNotNil)

	if compiledLib.Template != nil {
		cont := machine.NewMachineContinuation(nil, compiledLib.Template, compiledLib.Env)
		mc := machine.NewMachineContext(context.Background(), cont)
		err = mc.Run()
		c.Assert(err, qt.IsNil)
	}

	registryAny := env.LibraryRegistry()
	registry := registryAny.(*compilation.LibraryRegistry)
	err = registry.Register(compiledLib)
	c.Assert(err, qt.IsNil)

	return compiledLib
}

// TestLibraryInternalMacroToMacroHygiene tests that an exported macro can
// reference an unexported helper MACRO. This is the scenario from issue #433:
// the helper macro's binding lives in the library's expand phase, invisible
// to the use-site environment. Library scopes enable the compiler to redirect
// lookup to the library env via the TLE scope registry.
func TestLibraryInternalMacroToMacroHygiene(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	libCode := `(define-library (test macro-to-macro)
	  (export public-macro)
	  (import (scheme base))
	  (begin
	    (define-syntax helper-macro
	      (syntax-rules ()
	        ((helper-macro x) (+ x 10))))
	    (define-syntax public-macro
	      (syntax-rules ()
	        ((public-macro x) (helper-macro x))))))`

	compileAndRegisterLibrary(t, env, libCode)

	// Import into the caller environment
	sv := parseSchemeExpr(t, env, `(import (test macro-to-macro))`)
	_, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)

	// Use the public macro — helper-macro is NOT exported
	sv = parseSchemeExpr(t, env, `(public-macro 5)`)
	result, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil, qt.Commentf("exported macro using unexported helper macro should work"))
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(15))
}

// TestLibraryChainedMacroHygiene tests chained macro references similar to
// the miniKanren pattern: an exported macro uses an unexported intermediate
// macro which in turn uses another unexported helper. All three must resolve
// through the library's environment.
func TestLibraryChainedMacroHygiene(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	libCode := `(define-library (test chained-macros)
	  (export outer-macro)
	  (import (scheme base))
	  (begin
	    (define-syntax inner-macro
	      (syntax-rules ()
	        ((inner-macro x) (+ x 100))))
	    (define-syntax middle-macro
	      (syntax-rules ()
	        ((middle-macro x) (inner-macro x))))
	    (define-syntax outer-macro
	      (syntax-rules ()
	        ((outer-macro x) (middle-macro x))))))`

	compileAndRegisterLibrary(t, env, libCode)

	sv := parseSchemeExpr(t, env, `(import (test chained-macros))`)
	_, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)

	sv = parseSchemeExpr(t, env, `(outer-macro 5)`)
	result, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil, qt.Commentf("chained macro references should resolve through library scopes"))
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(105))
}

// TestLibraryTwoLibrarySameHelperName verifies that two libraries can each
// define an internal (unexported) macro named "helper" without collision.
// Each library's exported macro should resolve its own "helper" via its
// unique library scope, even when both are imported into the same environment.
func TestLibraryTwoLibrarySameHelperName(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	// Library A: helper adds 10
	libA := `(define-library (test scope-a)
	  (export macro-a)
	  (import (scheme base))
	  (begin
	    (define-syntax helper
	      (syntax-rules ()
	        ((helper x) (+ x 10))))
	    (define-syntax macro-a
	      (syntax-rules ()
	        ((macro-a x) (helper x))))))`

	// Library B: helper multiplies by 2
	libB := `(define-library (test scope-b)
	  (export macro-b)
	  (import (scheme base))
	  (begin
	    (define-syntax helper
	      (syntax-rules ()
	        ((helper x) (* x 2))))
	    (define-syntax macro-b
	      (syntax-rules ()
	        ((macro-b x) (helper x))))))`

	compileAndRegisterLibrary(t, env, libA)
	compileAndRegisterLibrary(t, env, libB)

	sv := parseSchemeExpr(t, env, `(import (test scope-a))`)
	_, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)

	sv = parseSchemeExpr(t, env, `(import (test scope-b))`)
	_, err = compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)

	// macro-a should use A's helper (+ x 10)
	sv = parseSchemeExpr(t, env, `(macro-a 5)`)
	result, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil, qt.Commentf("macro-a should resolve to library A's helper"))
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(15))

	// macro-b should use B's helper (* x 2)
	sv = parseSchemeExpr(t, env, `(macro-b 5)`)
	result, err = compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil, qt.Commentf("macro-b should resolve to library B's helper"))
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(10))
}

// TestLibraryReExportChain verifies that a re-exported macro still resolves
// its free identifiers in the defining library's environment, not the
// re-exporting library's. Library A defines helper + my-macro, library B
// re-exports my-macro, and the test imports from B.
func TestLibraryReExportChain(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	libA := `(define-library (test reexport-source)
	  (export my-macro)
	  (import (scheme base))
	  (begin
	    (define-syntax helper
	      (syntax-rules ()
	        ((helper x) (+ x 42))))
	    (define-syntax my-macro
	      (syntax-rules ()
	        ((my-macro x) (helper x))))))`

	libB := `(define-library (test reexport-relay)
	  (export my-macro)
	  (import (test reexport-source)))`

	compileAndRegisterLibrary(t, env, libA)
	compileAndRegisterLibrary(t, env, libB)

	sv := parseSchemeExpr(t, env, `(import (test reexport-relay))`)
	_, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil)

	// my-macro was re-exported through B, but helper lives in A
	sv = parseSchemeExpr(t, env, `(my-macro 8)`)
	result, err := compileAndRun(t, env, sv)
	c.Assert(err, qt.IsNil, qt.Commentf("re-exported macro should resolve helper in defining library"))
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(50))
}

// TestLibraryBindingsCarryLibraryScope verifies that after Flatt §3.3
// stamping, bindings created in a library environment carry the library
// scope in their scope set. This is the observable effect of stamping
// library body forms with the library scope before expansion.
func TestLibraryBindingsCarryLibraryScope(t *testing.T) {
	c := qt.New(t)
	env := setupSchemeLibraryTest(t)

	libCode := `(define-library (test scoped-bindings)
	  (export my-fn)
	  (import (scheme base))
	  (begin
	    (define (my-fn x) (+ x 1))))`

	lib := compileAndRegisterLibrary(t, env, libCode)

	// The binding in the library's env should carry the library scope
	myFnSym := values.NewSymbol("my-fn")
	binding := lib.Env.GetBinding(myFnSym, nil)
	c.Assert(binding, qt.IsNotNil, qt.Commentf("my-fn should exist in library env"))
	c.Assert(len(binding.Scopes()) > 0, qt.IsTrue,
		qt.Commentf("library binding should carry at least the library scope"))

	// Also check a define-syntax binding in the expand phase
	libCode2 := `(define-library (test scoped-syntax)
	  (export my-macro)
	  (import (scheme base))
	  (begin
	    (define-syntax my-macro
	      (syntax-rules ()
	        ((my-macro x) x)))))`

	lib2 := compileAndRegisterLibrary(t, env, libCode2)

	myMacroSym := values.NewSymbol("my-macro")
	syntaxBinding := lib2.Env.Expand().GetBinding(myMacroSym, nil)
	c.Assert(syntaxBinding, qt.IsNotNil, qt.Commentf("my-macro should exist in library expand env"))
	c.Assert(len(syntaxBinding.Scopes()) > 0, qt.IsTrue,
		qt.Commentf("library syntax binding should carry at least the library scope"))
}

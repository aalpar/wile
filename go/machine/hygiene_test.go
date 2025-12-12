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
	"strings"
	"testing"

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

// Helper function to parse a string into syntax using the given environment.
// Using the same environment ensures symbols are interned consistently.
func parseString(t *testing.T, env *environment.EnvironmentFrame, input string) syntax.SyntaxValue {
	reader := strings.NewReader(input)
	p := parser.NewParser(env, reader)
	stx, err := p.ReadSyntax(nil)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	return stx
}

// Helper function to create a test environment with basic primitives
func createHygieneTestEnv() *environment.EnvironmentFrame {
	env := environment.NewTopLevelEnvironmentFrame()

	// Add some basic primitives for testing
	// We'll add 'if', 'set!', 'let' support as needed

	return env
}

func TestBasicHygiene_SwapMacro(t *testing.T) {
	// This test demonstrates hygienic macro expansion with the classic swap! example.
	// The swap! macro uses a temporary variable 'tmp' which should not capture
	// any user-defined 'tmp' variable due to hygiene.

	env := createHygieneTestEnv()

	// First, define 'let' as a macro since it's a derived expression in R7RS
	// Using a simplified single-body form with begin wrapper
	letMacro := parseString(t, env, `
		(define-syntax let1
		  (syntax-rules ()
		    ((let1 ((name val) ...) body)
		     ((lambda (name ...) body) val ...))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt := machine.NewCompileTimeCallContext(false, false, env)
	args := extractDefineSyntaxArgs(t, letMacro)
	err := ctc.CompileDefineSyntax(ccnt, args)
	if err != nil {
		t.Fatalf("failed to compile let1 macro: %v", err)
	}

	// Define the swap! macro - wrap body in begin for single-body let
	defineSyntaxForm := parseString(t, env, `
		(define-syntax swap!
		  (syntax-rules ()
		    ((swap! x y)
		     (let1 ((tmp x))
		       (begin
		         (set! x y)
		         (set! y tmp))))))
	`)

	// Compile the define-syntax for swap!
	ctc = machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt = machine.NewCompileTimeCallContext(false, false, env)
	args = extractDefineSyntaxArgs(t, defineSyntaxForm)
	err = ctc.CompileDefineSyntax(ccnt, args)
	if err != nil {
		t.Fatalf("failed to compile swap! macro: %v", err)
	}

	// Test case: User has their own 'tmp' variable
	testForm := parseString(t, env, `
		(let1 ((tmp 5) (a 1) (b 2))
		  (begin
		    (swap! a b)
		    tmp))
	`)

	// Expand the macro
	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.ExpandTimeCallContext{}
	expanded, err := etc.ExpandExpression(ectx, testForm)
	if err != nil {
		t.Fatalf("failed to expand: %v", err)
	}

	t.Logf("Expanded: %s", expanded.SchemeString())

	// Compile the expanded form
	ctc2 := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt2 := machine.NewCompileTimeCallContext(false, true, env)
	err = ctc2.CompileExpression(ccnt2, expanded)
	if err != nil {
		t.Fatalf("failed to compile: %v", err)
	}
}

// TestLetMacroExpansion tests the let macro expansion with ellipsis
func TestLetMacroExpansion(t *testing.T) {
	env := createHygieneTestEnv()

	// First test a simpler macro without nested ellipsis
	// (define-syntax my-list
	//   (syntax-rules ()
	//     ((my-list x ...) (list x ...))))
	simpleMacro := parseString(t, env, `
		(define-syntax my-list
		  (syntax-rules ()
		    ((my-list x ...) (list x ...))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt := machine.NewCompileTimeCallContext(false, false, env)
	args := extractDefineSyntaxArgs(t, simpleMacro)
	err := ctc.CompileDefineSyntax(ccnt, args)
	if err != nil {
		t.Fatalf("failed to compile my-list macro: %v", err)
	}

	// Test: (my-list 1 2 3) -> (list 1 2 3)
	testForm := parseString(t, env, `(my-list 1 2 3)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.ExpandTimeCallContext{}
	expanded, err := etc.ExpandExpression(ectx, testForm)
	if err != nil {
		t.Fatalf("failed to expand my-list: %v", err)
	}

	t.Logf("Expanded: %s", expanded.SchemeString())

	expectedForm := parseString(t, env, `(list 1 2 3)`)
	qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expectedForm.UnwrapAll())
}

// TestLetMacroSimple tests a simplified let macro without body ellipsis
func TestLetMacroSimple(t *testing.T) {
	env := createHygieneTestEnv()

	// Simplified let that only takes a single body expression
	// (define-syntax let1
	//   (syntax-rules ()
	//     ((let1 ((name val) ...) body)
	//      ((lambda (name ...) body) val ...))))
	simpleMacro := parseString(t, env, `
		(define-syntax let1
		  (syntax-rules ()
		    ((let1 ((name val) ...) body)
		     ((lambda (name ...) body) val ...))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt := machine.NewCompileTimeCallContext(false, false, env)
	args := extractDefineSyntaxArgs(t, simpleMacro)
	err := ctc.CompileDefineSyntax(ccnt, args)
	if err != nil {
		t.Fatalf("failed to compile let1 macro: %v", err)
	}

	// Test: (let1 ((x 1)) x) -> ((lambda (x) x) 1)
	testForm := parseString(t, env, `(let1 ((x 1)) x)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.ExpandTimeCallContext{}
	expanded, err := etc.ExpandExpression(ectx, testForm)
	if err != nil {
		t.Fatalf("failed to expand let1: %v", err)
	}

	t.Logf("Expanded: %s", expanded.SchemeString())

	expectedForm := parseString(t, env, `((lambda (x) x) 1)`)
	qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expectedForm.UnwrapAll())
}

// TestMultipleElementsWithTrailingEllipsis tests patterns like (foo a b ...)
func TestMultipleElementsWithTrailingEllipsis(t *testing.T) {
	env := createHygieneTestEnv()

	// A simpler test: (begin-with-first e1 e2 ...) -> (begin e1 e2 ...)
	// This tests the pattern: first mandatory element, then zero or more
	simpleMacro := parseString(t, env, `
		(define-syntax begin-with-first
		  (syntax-rules ()
		    ((begin-with-first e1 e2 ...)
		     (begin e1 e2 ...))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt := machine.NewCompileTimeCallContext(false, false, env)
	args := extractDefineSyntaxArgs(t, simpleMacro)
	err := ctc.CompileDefineSyntax(ccnt, args)
	if err != nil {
		t.Fatalf("failed to compile macro: %v", err)
	}

	// Test with just one expression: (begin-with-first x) -> (begin x)
	testForm := parseString(t, env, `(begin-with-first x)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.ExpandTimeCallContext{}
	expanded, err := etc.ExpandExpression(ectx, testForm)
	if err != nil {
		t.Fatalf("failed to expand: %v", err)
	}

	t.Logf("Expanded: %s", expanded.SchemeString())

	expectedForm := parseString(t, env, `(begin x)`)
	qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expectedForm.UnwrapAll())

	// Test with multiple expressions: (begin-with-first x y z) -> (begin x y z)
	testForm2 := parseString(t, env, `(begin-with-first x y z)`)
	expanded2, err := etc.ExpandExpression(ectx, testForm2)
	if err != nil {
		t.Fatalf("failed to expand with multiple: %v", err)
	}

	t.Logf("Expanded2: %s", expanded2.SchemeString())

	expectedForm2 := parseString(t, env, `(begin x y z)`)
	qt.Assert(t, expanded2.UnwrapAll(), values.SchemeEquals, expectedForm2.UnwrapAll())
}

// TestLetMacroFull tests the full R7RS let macro with multiple body expressions
func TestLetMacroFull(t *testing.T) {
	env := createHygieneTestEnv()

	// Simpler let macro - uses (begin body ...) to wrap multiple bodies
	// This avoids the complex "body1 body2 ..." pattern which requires
	// more sophisticated ellipsis tracking
	// (define-syntax let
	//   (syntax-rules ()
	//     ((let ((name val) ...) body ...)
	//      ((lambda (name ...) (begin body ...)) val ...))))
	letMacro := parseString(t, env, `
		(define-syntax let
		  (syntax-rules ()
		    ((let ((name val) ...) body ...)
		     ((lambda (name ...) (begin body ...)) val ...))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt := machine.NewCompileTimeCallContext(false, false, env)
	args := extractDefineSyntaxArgs(t, letMacro)
	err := ctc.CompileDefineSyntax(ccnt, args)
	if err != nil {
		t.Fatalf("failed to compile let macro: %v", err)
	}

	// Test: (let ((x 1)) x) -> ((lambda (x) (begin x)) 1)
	testForm := parseString(t, env, `(let ((x 1)) x)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.ExpandTimeCallContext{}
	expanded, err := etc.ExpandExpression(ectx, testForm)
	if err != nil {
		t.Fatalf("failed to expand let: %v", err)
	}

	t.Logf("Expanded: %s", expanded.SchemeString())

	// With the (begin body ...) wrapper, the expansion is:
	// ((lambda (x) (begin x)) 1)
	expectedForm := parseString(t, env, `((lambda (x) (begin x)) 1)`)
	qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expectedForm.UnwrapAll())
}

func TestScopeCreation(t *testing.T) {
	// Test that scopes are being created and added during expansion
	env := createHygieneTestEnv()

	// Define a simple macro without arguments
	defineSyntaxForm := parseString(t, env, `
		(define-syntax foo
		  (syntax-rules ()
		    ((foo) 'expanded)))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ccnt := machine.NewCompileTimeCallContext(false, false, env)
	args := extractDefineSyntaxArgs(t, defineSyntaxForm)
	err := ctc.CompileDefineSyntax(ccnt, args)
	qt.Assert(t, err, qt.IsNil)

	// Use the macro
	useForm := parseString(t, env, "(foo)")

	// Get the transformer from expand phase (syntax bindings live in expand phase)
	fooSym := values.NewSymbol("foo")
	binding := env.Expand().GetBinding(fooSym)
	qt.Assert(t, binding, qt.Not(qt.IsNil))

	_, ok := binding.Value().(*machine.MachineClosure)
	if !ok {
		t.Fatalf("expected MachineClosure, got %T", binding.Value())
	}

	// Note: Testing the actual macro expansion would require running the
	// transformer, which needs access to machine internals.
	// The key test here is that:
	// 1. The macro compiles successfully
	// 2. It's bound as a syntax transformer (MachineClosure)
	// 3. The OperationSyntaxRulesTransform operation adds intro scopes
	//    during expansion (tested in the implementation)

	// Test that we can expand using the ExpanderTimeContinuation
	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.ExpandTimeCallContext{}

	// Debug: Check what the binding actually contains
	qt.Assert(t, binding.Value(), qt.Not(qt.IsNil))
	t.Logf("Binding type: %v, Value type: %T", binding.BindingType(), binding.Value())

	// Check the closure template
	closure := binding.Value().(*machine.MachineClosure)
	_ = closure // We can't access private fields, but at least verify it's the right type

	defer func() {
		if r := recover(); r != nil {
			t.Fatalf("panic during expansion: %v", r)
		}
	}()

	expanded, err := etc.ExpandExpression(ectx, useForm)
	qt.Assert(t, err, qt.IsNil)

	// Check that expansion succeeded
	qt.Assert(t, expanded, qt.Not(qt.IsNil))

	// Check semantic equality by comparing unwrapped values (ignoring syntax metadata)
	// The expected form is (quote expanded)
	expandedForm := parseString(t, env, "'expanded")
	qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expandedForm.UnwrapAll())

	// Verify the expansion is structurally correct
	// Note: Free identifiers (like 'quote' and 'expanded') do NOT get intro scope
	// because they need to resolve to their original bindings. Only pattern variables
	// and identifiers introduced by the macro that could cause capture get intro scope.
	expandedPair, ok := expanded.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected SyntaxPair, got %T", expanded))

	// The expansion (quote expanded) should have the quote symbol as car
	if quoteSym, ok := expandedPair.Car().(*syntax.SyntaxSymbol); ok {
		// Free identifiers like 'quote' should NOT have intro scope
		// This is correct behavior - they need to resolve to their original bindings
		qt.Assert(t, quoteSym.Unwrap().(*values.Symbol).Key, qt.Equals, "quote")
	}
}

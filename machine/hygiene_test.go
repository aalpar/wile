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
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// Helper function to parse a string into syntax using the given environment.
// Using the same environment ensures symbols are interned consistently.
func parseString(t *testing.T, env *environment.EnvironmentFrame, input string) syntax.SyntaxValue {
	reader := strings.NewReader(input)
	p := parser.NewParser(env, true, reader)
	stx, err := p.ReadSyntax(context.TODO())
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	return stx
}

// Helper function to create a test environment with basic primitives
func createHygieneTestEnv() *environment.EnvironmentFrame {
	env := environment.NewTopLevelEnvironment().Runtime()

	// Register primitive expanders (for let-syntax, quote, if, etc.)
	err := machine.RegisterPrimitiveExpanders(env)
	if err != nil {
		panic("failed to register primitive expanders: " + err.Error())
	}

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
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args := extractDefineSyntaxArgs(t, letMacro)
	err := ctc.CompileDefineSyntax(ctctx, args)
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
	ctctx = machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args = extractDefineSyntaxArgs(t, defineSyntaxForm)
	err = ctc.CompileDefineSyntax(ctctx, args)
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
	ectx := machine.NewExpandTimeCallContext(context.Background())
	expanded, err := etc.ExpandExpression(ectx, testForm)
	if err != nil {
		t.Fatalf("failed to expand: %v", err)
	}

	t.Logf("Expanded: %s", expanded.SchemeString())

	// Compile the expanded form
	ctc2 := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx2 := machine.NewCompileTimeCallContext(context.Background(), false, true, env)
	err = ctc2.CompileExpression(ctctx2, expanded)
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
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args := extractDefineSyntaxArgs(t, simpleMacro)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile my-list macro: %v", err)
	}

	// Test: (my-list 1 2 3) -> (list 1 2 3)
	testForm := parseString(t, env, `(my-list 1 2 3)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext(context.Background())
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
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args := extractDefineSyntaxArgs(t, simpleMacro)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile let1 macro: %v", err)
	}

	// Test: (let1 ((x 1)) x) -> ((lambda (x) x) 1)
	testForm := parseString(t, env, `(let1 ((x 1)) x)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext(context.Background())
	expanded, err := etc.ExpandExpression(ectx, testForm)
	if err != nil {
		t.Fatalf("failed to expand let1: %v", err)
	}

	t.Logf("Expanded: %s", expanded.SchemeString())

	expectedForm := parseString(t, env, `((lambda (x) x) 1)`)
	qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expectedForm.UnwrapAll())
}

// TestMultipleElementsWithTrailingEllipsis tests patterns like (bindSymbolWithScopes a b ...)
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
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args := extractDefineSyntaxArgs(t, simpleMacro)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile macro: %v", err)
	}

	// Test with just one expression: (begin-with-first x) -> (begin x)
	testForm := parseString(t, env, `(begin-with-first x)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext(context.Background())
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
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args := extractDefineSyntaxArgs(t, letMacro)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile let macro: %v", err)
	}

	// Test: (let ((x 1)) x) -> ((lambda (x) (begin x)) 1)
	testForm := parseString(t, env, `(let ((x 1)) x)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext(context.Background())
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
		(define-syntax bindSymbolWithScopes
		  (syntax-rules ()
		    ((bindSymbolWithScopes) 'expanded)))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args := extractDefineSyntaxArgs(t, defineSyntaxForm)
	err := ctc.CompileDefineSyntax(ctctx, args)
	qt.Assert(t, err, qt.IsNil)

	// Use the macro
	useForm := parseString(t, env, "(bindSymbolWithScopes)")

	// Get the transformer from expand phase (syntax bindings live in expand phase)
	fooSym := values.NewSymbol("bindSymbolWithScopes")
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
	ectx := machine.NewExpandTimeCallContext(context.Background())

	// Debug: Check what the binding actually contains
	qt.Assert(t, binding.Value(), qt.Not(qt.IsNil))
	t.Logf("Binding type: %v, wrt type: %T", binding.BindingType(), binding.Value())

	// Check the closure template
	closure := binding.Value().(*machine.MachineClosure)
	_ = closure // We can't access private fields, but at least verify it's the right type

	defer func() {
		r := recover()
		if r != nil {
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

// TestAuxiliarySyntaxShadowing tests R7RS auxiliary syntax hygiene.
// Per R7RS §4.3.2, literals like => and else in syntax-rules should be
// hygienic - if locally shadowed by let-syntax, they should be treated
// as regular expressions, not as the special auxiliary syntax.
//
// Note: We use let-syntax for shadowing because it properly adds scopes
// to the body (implementing Flatt's "sets of scopes" model). Regular let
// is a runtime binding that doesn't affect compile-time scope sets.
func TestAuxiliarySyntaxShadowing(t *testing.T) {
	tests := []struct {
		name     string
		setup    string // Optional setup code (macros to define before test)
		code     string
		expected string
	}{
		{
			name: "shadowed => via let-syntax treated as expression",
			// When => is shadowed via let-syntax, it gets a new scope.
			// The cond pattern's => has different scopes, so it doesn't match.
			// The clause falls through to (test result1 result2 ...) pattern.
			setup: `
				(define-syntax my-cond
				  (syntax-rules (else =>)
				    ((my-cond (else result1 result2 ...))
				     (begin result1 result2 ...))
				    ((my-cond (test => result))
				     (let ((temp test))
				       (if temp (result temp))))
				    ((my-cond (test result1 result2 ...))
				     (if test (begin result1 result2 ...)))))
			`,
			// With => shadowed, (test => 'ok) doesn't match the arrow pattern
			// because the => has an extra scope from let-syntax.
			// let-syntax wraps its body in (begin ...).
			code:     "(let-syntax ((=> (syntax-rules () ((_) #f)))) (my-cond (#t => 'ok)))",
			expected: "(begin (if #t (begin => (quote ok))))", // => doesn't match arrow, falls through
		},
		{
			name: "unshadowed => still works as arrow",
			setup: `
				(define-syntax my-cond
				  (syntax-rules (else =>)
				    ((my-cond (else result1 result2 ...))
				     (begin result1 result2 ...))
				    ((my-cond (test => result))
				     (let ((temp test))
				       (if temp (result temp))))
				    ((my-cond (test result1 result2 ...))
				     (if test (begin result1 result2 ...)))))
			`,
			code:     "(my-cond (#t => (lambda (x) 'yes)))",
			expected: "(let ((temp #t)) (if temp ((lambda (x) (quote yes)) temp)))",
		},
		{
			name: "shadowed else via let-syntax not treated as else clause",
			setup: `
				(define-syntax my-cond
				  (syntax-rules (else =>)
				    ((my-cond (else result1 result2 ...))
				     (begin result1 result2 ...))
				    ((my-cond (test result1 result2 ...))
				     (if test (begin result1 result2 ...)))))
			`,
			// With else shadowed via let-syntax, it has a new scope.
			// (else 'matched) doesn't match the else pattern, treated as regular test.
			// let-syntax wraps its body in (begin ...).
			code:     "(let-syntax ((else (syntax-rules () ((_) #f)))) (my-cond (else 'matched)))",
			expected: "(begin (if else (begin (quote matched))))", // else is the test expression
		},
		// R7RS §4.3.2: Regular let binding also shadows auxiliary syntax.
		// When => is bound via let (which expands to lambda), the lambda scope
		// is added to the body, making => have different scopes than the pattern literal.
		{
			name: "shadowed => via regular let treated as expression",
			setup: `
				(define-syntax let
				  (syntax-rules ()
				    ((let ((name val) ...) body ...)
				     (with-binding-scope (name ...)
				       ((lambda (name ...) (begin body ...)) val ...)))))
				(define-syntax my-cond
				  (syntax-rules (else =>)
				    ((my-cond (else result1 result2 ...))
				     (begin result1 result2 ...))
				    ((my-cond (test => result))
				     (let ((temp test))
				       (if temp (result temp))))
				    ((my-cond (test result1 result2 ...))
				     (if test (begin result1 result2 ...)))))
			`,
			// With => bound via let, it has the lambda scope.
			// (test => 'ok) doesn't match the arrow pattern, falls through to (test result1 result2 ...).
			// let expands to ((lambda (=>) ...) #f)
			code:     "(let ((=> #f)) (my-cond (#t => 'ok)))",
			expected: "((lambda (=>) (begin (if #t (begin => (quote ok))))) #f)",
		},
		{
			name: "shadowed else via regular let not treated as else clause",
			setup: `
				(define-syntax let
				  (syntax-rules ()
				    ((let ((name val) ...) body ...)
				     (with-binding-scope (name ...)
				       ((lambda (name ...) (begin body ...)) val ...)))))
				(define-syntax my-cond
				  (syntax-rules (else =>)
				    ((my-cond (else result1 result2 ...))
				     (begin result1 result2 ...))
				    ((my-cond (test result1 result2 ...))
				     (if test (begin result1 result2 ...)))))
			`,
			// With else bound via let, it has the lambda scope.
			// (else 'matched) doesn't match the else pattern.
			code:     "(let ((else #f)) (my-cond (else 'matched)))",
			expected: "((lambda (else) (begin (if else (begin (quote matched))))) #f)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			env := createHygieneTestEnv()

			// Setup: compile any macro definitions
			if tt.setup != "" {
				setupForms := parseMultipleForms(t, env, tt.setup)
				for _, form := range setupForms {
					// Skip nil forms (from empty strings between forms)
					if form == nil {
						continue
					}

					// Expand and compile each setup form
					etc := machine.NewExpanderTimeContinuation(env)
					ectx := machine.NewExpandTimeCallContext(context.Background())
					expanded, err := etc.ExpandExpression(ectx, form)
					if err != nil {
						t.Fatalf("failed to expand setup: %v", err)
					}

					// If it's a define-syntax, compile it
					if pair, ok := expanded.(*syntax.SyntaxPair); ok {
						car := pair.Car()
						if car != nil {
							if sym, ok := car.(*syntax.SyntaxSymbol); ok && sym.Sym.Key == "define-syntax" {
								ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
								ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
								args := extractDefineSyntaxArgs(t, expanded)
								err := ctc.CompileDefineSyntax(ctctx, args)
								if err != nil {
									t.Fatalf("failed to compile setup define-syntax: %v", err)
								}
							}
						}
					}
				}
			}

			// Parse the test code
			testForm := parseString(t, env, tt.code)

			// Expand the test form
			etc := machine.NewExpanderTimeContinuation(env)
			ectx := machine.NewExpandTimeCallContext(context.Background())
			expanded, err := etc.ExpandExpression(ectx, testForm)
			if err != nil {
				t.Fatalf("failed to expand: %v", err)
			}

			t.Logf("Expanded: %s", expanded.SchemeString())

			// Parse expected form and compare
			expectedForm := parseString(t, env, tt.expected)
			qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expectedForm.UnwrapAll(),
				qt.Commentf("expanded: %s, expected: %s", expanded.SchemeString(), expectedForm.SchemeString()))
		})
	}
}

// parseMultipleForms parses a string containing multiple Scheme forms
func parseMultipleForms(t *testing.T, env *environment.EnvironmentFrame, input string) []syntax.SyntaxValue {
	reader := strings.NewReader(input)
	p := parser.NewParser(env, true, reader)
	var forms []syntax.SyntaxValue
	for {
		stx, err := p.ReadSyntax(context.TODO())
		if err != nil {
			// EOF or other error
			break
		}
		if stx != nil {
			forms = append(forms, stx)
		}
	}
	return forms
}

// TestBoundIdentifierHygieneInNestedSyntaxRules tests R7RS conformance for
// bound-identifier=? semantics in nested syntax-rules. This corresponds to
// r7rs-tests.scm lines 585-592.
//
// R7RS §4.3.2 requires that identifiers appearing in patterns be compared to
// literals using bound-identifier=?, not just by name. When a pattern variable
// from an outer macro is substituted into an inner macro's pattern, and the
// substituted identifier has the same name as a literal in the inner macro,
// they should only match if they have the same scopes.
func TestBoundIdentifierHygieneInNestedSyntaxRules(t *testing.T) {
	env := createHygieneTestEnv()

	// The outer macro 'm' captures input 'x' and substitutes it into the inner
	// macro 'n'. The inner macro has 'k' as a literal. When (m k) is called:
	// - 'x' captures 'k' from the use site (with use-site scopes)
	// - In the template, 'x' in pattern (n x) is substituted with the captured 'k'
	// - The literal 'k' in (syntax-rules (k) ...) has template scopes
	// - These are different scopes, so they're NOT bound-identifier=?
	// - Therefore 'k' in (n k) is treated as a pattern variable
	// - Pattern (n x) matches input (n z), returning 'bound-identifier=?
	outerMacro := parseString(t, env, `
		(define-syntax m
		  (syntax-rules ()
		    ((m x) (let-syntax
		               ((n (syntax-rules (k)
		                     ((n x) 'bound-identifier=?)
		                     ((n y) 'free-identifier=?))))
		             (n z)))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, false, env)
	args := extractDefineSyntaxArgs(t, outerMacro)
	err := ctc.CompileDefineSyntax(ctctx, args)
	qt.Assert(t, err, qt.IsNil, qt.Commentf("failed to compile outer macro"))

	// Test: (m k) should expand to 'bound-identifier=?
	testForm := parseString(t, env, `(m k)`)

	etc := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext(context.Background())
	expanded, err := etc.ExpandExpression(ectx, testForm)
	qt.Assert(t, err, qt.IsNil, qt.Commentf("failed to expand (m k)"))

	t.Logf("Expanded: %s", expanded.SchemeString())

	// The expected result is (begin (quote bound-identifier=?))
	// The outer begin comes from let-syntax body wrapping
	expectedForm := parseString(t, env, `(begin (quote bound-identifier=?))`)
	qt.Assert(t, expanded.UnwrapAll(), values.SchemeEquals, expectedForm.UnwrapAll(),
		qt.Commentf("expected (begin (quote bound-identifier=?)), got: %s", expanded.SchemeString()))
}

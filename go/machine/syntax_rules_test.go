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

	"github.com/aalpar/wile/go/environment"
	"github.com/aalpar/wile/go/machine"
	"github.com/aalpar/wile/go/parser"
	"github.com/aalpar/wile/go/syntax"
	"github.com/aalpar/wile/go/values"
)

// Helper function to parse a string into syntax
func parseSyntax(t *testing.T, env *environment.EnvironmentFrame, input string) syntax.SyntaxValue {
	// Parse the input string
	stx, err := parser.NewParser(env, true, strings.NewReader(input)).ReadSyntax(context.TODO())
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	return stx
}

// Helper function to create a test environment
func createTestEnv() *environment.EnvironmentFrame {
	return environment.NewTopLevelEnvironmentFrame()
}

// Helper function to extract the args from a define-syntax form
// (define-syntax keyword transformer) -> (keyword transformer)
func extractDefineSyntaxArgs(t *testing.T, form syntax.SyntaxValue) syntax.SyntaxValue {
	pair, ok := form.(*syntax.SyntaxPair)
	if !ok {
		t.Fatalf("expected SyntaxPair, got %T", form)
	}
	cdr := pair.Cdr()
	if cdr == nil {
		t.Fatal("expected cdr, got nil")
	}
	args, ok := cdr.(syntax.SyntaxValue)
	if !ok {
		t.Fatalf("expected SyntaxValue, got %T", cdr)
	}
	return args
}

func TestSyntaxRulesSimpleVariable(t *testing.T) {
	// Test: (define-syntax foo (syntax-rules () ((foo x) x)))
	// Usage: (foo 42) => 42

	env := createTestEnv()

	// Parse the define-syntax form and extract args
	defineSyntaxForm := parseSyntax(t, env, "(define-syntax foo (syntax-rules () ((foo x) x)))")
	args := extractDefineSyntaxArgs(t, defineSyntaxForm)

	// Compile define-syntax
	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile define-syntax: %v", err)
	}

	// Check that the transformer was stored in the expand phase environment
	fooSym := values.NewSymbol("foo")
	binding := env.Expand().GetBinding(fooSym)
	if binding == nil {
		t.Fatal("foo not bound in expand phase environment")
	}

	if binding.BindingType() != environment.BindingTypeSyntax {
		t.Fatalf("foo binding type is %v, expected BindingTypeSyntax", binding.BindingType())
	}

	// Get the transformer closure
	closure, ok := binding.Value().(*machine.MachineClosure)
	if !ok {
		t.Fatalf("foo binding value is %T, expected MachineClosure", binding.Value())
	}

	// Test passes if we got this far - the syntax-rules macro was successfully compiled
	// TODO: Add test for actually invoking the transformer once the API supports it
	_ = closure // Suppress unused variable warning
}

func TestSyntaxRulesWithLiteral(t *testing.T) {
	// Test: (define-syntax my-if (syntax-rules (then else)
	//         ((my-if test then expr1 else expr2) (if test expr1 expr2))))
	// Usage: (my-if #t then 1 else 2) => (if #t 1 2)

	env := createTestEnv()

	// Parse the define-syntax form and extract args
	defineSyntaxForm := parseSyntax(t, env,
		`(define-syntax my-if
		   (syntax-rules (then else)
		     ((my-if test then expr1 else expr2) (if test expr1 expr2))))`)
	args := extractDefineSyntaxArgs(t, defineSyntaxForm)

	// Compile define-syntax
	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile define-syntax: %v", err)
	}

	// Get the transformer from expand phase (syntax bindings live in expand phase)
	myIfSym := values.NewSymbol("my-if")
	binding := env.Expand().GetBinding(myIfSym)
	if binding == nil {
		t.Fatal("my-if not bound in expand phase environment")
	}

	closure, ok := binding.Value().(*machine.MachineClosure)
	if !ok {
		t.Fatalf("my-if binding value is %T, expected MachineClosure", binding.Value())
	}

	// Test passes if we got this far - the syntax-rules macro with literals was successfully compiled
	// TODO: Add test for actually invoking the transformer once the API supports it
	_ = closure // Suppress unused variable warning
}

func TestSyntaxRulesWithEllipsis(t *testing.T) {
	// Test: (define-syntax list* (syntax-rules () ((list* x ...) (list x ...))))
	// Usage: (list* 1 2 3) => (list 1 2 3)

	env := createTestEnv()

	// Parse the define-syntax form and extract args
	defineSyntaxForm := parseSyntax(t, env,
		`(define-syntax list*
		   (syntax-rules ()
		     ((list* x ...) (list x ...))))`)
	args := extractDefineSyntaxArgs(t, defineSyntaxForm)

	// Compile define-syntax
	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile define-syntax: %v", err)
	}

	// Get the transformer from expand phase (syntax bindings live in expand phase)
	listStarSym := values.NewSymbol("list*")
	binding := env.Expand().GetBinding(listStarSym)
	if binding == nil {
		t.Fatal("list* not bound in expand phase environment")
	}

	closure, ok := binding.Value().(*machine.MachineClosure)
	if !ok {
		t.Fatalf("list* binding value is %T, expected MachineClosure", binding.Value())
	}

	// Test passes if we got this far - the syntax-rules macro with ellipsis was successfully compiled
	// TODO: Add test for actually invoking the transformer once the API supports it
	_ = closure // Suppress unused variable warning
}

// TestSyntaxRulesWithCustomEllipsis tests R7RS §4.3.2 custom ellipsis identifier.
// The syntax (syntax-rules <ellipsis> (<literal> ...) <clause> ...) allows
// specifying an alternative identifier for the ellipsis, which is useful for
// macros that need to generate code containing literal "..." identifiers.
func TestSyntaxRulesWithCustomEllipsis(t *testing.T) {
	// Test: (define-syntax my-list (syntax-rules ::: () ((my-list x :::) (list x :::))))
	// Usage: (my-list 1 2 3) => (list 1 2 3)
	// The ::: is used as the ellipsis instead of ...

	env := createTestEnv()

	// Parse the define-syntax form with custom ellipsis :::
	defineSyntaxForm := parseSyntax(t, env,
		`(define-syntax my-list
		   (syntax-rules ::: ()
		     ((my-list x :::) (list x :::))))`)
	args := extractDefineSyntaxArgs(t, defineSyntaxForm)

	// Compile define-syntax
	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile define-syntax with custom ellipsis: %v", err)
	}

	// Get the transformer from expand phase
	myListSym := values.NewSymbol("my-list")
	binding := env.Expand().GetBinding(myListSym)
	if binding == nil {
		t.Fatal("my-list not bound in expand phase environment")
	}

	_, ok := binding.Value().(*machine.MachineClosure)
	if !ok {
		t.Fatalf("my-list binding value is %T, expected MachineClosure", binding.Value())
	}

	// Test passes if we got this far - the syntax-rules macro with custom ellipsis was successfully compiled
	t.Log("Custom ellipsis syntax-rules compiled successfully")
}

// TestSyntaxRulesWithUnderscoreInLiterals tests that _ can be matched literally
// when it appears in the literals list, per R7RS §4.3.2.
func TestSyntaxRulesWithUnderscoreInLiterals(t *testing.T) {
	// Test: (define-syntax test-underscore (syntax-rules (_) ((test-underscore _ x) x)))
	// The _ is in the literals list, so it should be matched literally, not as a wildcard.
	// Usage: (test-underscore _ 42) => 42
	//        (test-underscore foo 42) => no match (foo doesn't match literal _)

	env := createTestEnv()

	// Parse the define-syntax form with _ in literals list
	defineSyntaxForm := parseSyntax(t, env,
		`(define-syntax test-underscore
		   (syntax-rules (_)
		     ((test-underscore _ x) x)))`)
	args := extractDefineSyntaxArgs(t, defineSyntaxForm)

	// Compile define-syntax
	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(false, false, env)
	err := ctc.CompileDefineSyntax(ctctx, args)
	if err != nil {
		t.Fatalf("failed to compile define-syntax with _ in literals: %v", err)
	}

	// Get the transformer from expand phase
	testUnderscoreSym := values.NewSymbol("test-underscore")
	binding := env.Expand().GetBinding(testUnderscoreSym)
	if binding == nil {
		t.Fatal("test-underscore not bound in expand phase environment")
	}

	_, ok := binding.Value().(*machine.MachineClosure)
	if !ok {
		t.Fatalf("test-underscore binding value is %T, expected MachineClosure", binding.Value())
	}

	// Test passes if we got this far - the syntax-rules macro with _ in literals was successfully compiled
	t.Log("Underscore in literals syntax-rules compiled successfully")
}

// TestSyntaxRulesEllipsisInLiteralsAccepted tests that ellipsis in literals list is accepted.
// R7RS §4.3.2: If <ellipsis> is specified (appears in <literals>), it is treated as a
// literal and ellipsis functionality is disabled for this syntax-rules form.
func TestSyntaxRulesEllipsisInLiteralsAccepted(t *testing.T) {
	t.Run("Default ellipsis in literals compiles", func(t *testing.T) {
		env := createTestEnv()

		// Parse a syntax-rules form with ... in the literals list (valid per R7RS)
		defineSyntaxForm := parseSyntax(t, env,
			`(define-syntax elli-macro
			   (syntax-rules (...)
			     ((elli-macro x) x)))`)
		args := extractDefineSyntaxArgs(t, defineSyntaxForm)

		// Compile should succeed - ellipsis in literals disables ellipsis functionality
		ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
		ctctx := machine.NewCompileTimeCallContext(false, false, env)
		err := ctc.CompileDefineSyntax(ctctx, args)
		if err != nil {
			t.Fatalf("expected ellipsis in literals to compile, got: %v", err)
		}
		t.Log("Ellipsis in literals syntax-rules compiled successfully")
	})

	t.Run("Custom ellipsis in literals compiles", func(t *testing.T) {
		env := createTestEnv()

		// Parse a syntax-rules form with custom ellipsis ::: in the literals list (valid per R7RS)
		defineSyntaxForm := parseSyntax(t, env,
			`(define-syntax elli-macro
			   (syntax-rules ::: (:::)
			     ((elli-macro x) x)))`)
		args := extractDefineSyntaxArgs(t, defineSyntaxForm)

		// Compile should succeed - ellipsis in literals disables ellipsis functionality
		ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
		ctctx := machine.NewCompileTimeCallContext(false, false, env)
		err := ctc.CompileDefineSyntax(ctctx, args)
		if err != nil {
			t.Fatalf("expected custom ellipsis in literals to compile, got: %v", err)
		}
		t.Log("Custom ellipsis in literals syntax-rules compiled successfully")
	})
}

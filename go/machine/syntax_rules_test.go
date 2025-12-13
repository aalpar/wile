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
)

// Helper function to parse a string into syntax
func parseSyntax(t *testing.T, env *environment.EnvironmentFrame, input string) syntax.SyntaxValue {
	// Parse the input string
	stx, err := parser.Parse(env, strings.NewReader(input))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	return stx
}

// Helper function to create a test environment
func createTestEnv() *environment.EnvironmentFrame {
	// Create a minimal environment
	globalEnv := environment.NewTopLevelGlobalEnvironmentFrame()
	return environment.NewEnvironmentFrame(nil, globalEnv)
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

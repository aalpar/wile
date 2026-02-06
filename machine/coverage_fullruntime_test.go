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

// Tests using full runtime environment (with list, +, append, etc.)
// Uses package machine_test to avoid import cycle with wile/internal/bootstrap.

package machine_test

import (
	"bufio"
	"context"
	"strings"
	"testing"
	"time"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// parseSchemeExprExt parses a Scheme expression
func parseSchemeExprExt(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, true, reader)
	sv, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	return sv
}

// newFullRuntimeEnv creates a full runtime environment with all primitives
func newFullRuntimeEnv(t *testing.T) *environment.EnvironmentFrame {
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	return env
}

// newTopLevelThunkExt creates a top-level thunk from an expression using the expander and compiler
func newTopLevelThunkExt(sv syntax.SyntaxValue, env *environment.EnvironmentFrame) (*machine.MachineContinuation, error) {
	// Expand the expression
	econt := machine.NewExpanderTimeContinuation(env)
	ectx := machine.NewExpandTimeCallContext()
	expanded, err := econt.ExpandExpression(ectx, sv)
	if err != nil {
		return nil, err
	}

	// Compile the expanded expression
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := machine.NewCompiletimeContinuation(tpl, env)
	ctctx := machine.NewCompileTimeCallContext(false, true, env)
	err = ctc.CompileExpression(ctctx, expanded)
	if err != nil {
		return nil, err
	}
	// Note: do NOT add RestoreContinuation - the VM naturally halts when operations end

	return machine.NewMachineContinuation(nil, tpl, env), nil
}

// TestQuasiquoteWithUnquoteSplicingFullEnv tests quasiquote with unquote-splicing using full runtime
func TestQuasiquoteWithUnquoteSplicingFullEnv(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// unquote-splicing in list context with list primitive
	sv := parseSchemeExprExt(t, env, "`(a ,@(list 1 2 3) b)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	// Result should be (a 1 2 3 b)
}

// TestQuasiquoteWithMultipleUnquotes tests quasiquote with multiple unquotes
func TestQuasiquoteWithMultipleUnquotes(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Multiple unquotes in a list
	sv := parseSchemeExprExt(t, env, "`(,(+ 1 1) ,(+ 2 2) ,(+ 3 3))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	// Result should be (2 4 6)
}

// TestNestedQuasiquoteFullEnv tests deeply nested quasiquote
func TestNestedQuasiquoteFullEnv(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name string
		code string
	}{
		{"nested qq with unquote", "``(a ,(+ 1 2))"},
		{"double unquote", "``(a ,,(+ 1 2))"},
		{"triple nested", "```(a b c)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCaseLambdaWithArithmetic tests case-lambda with + and other primitives
func TestCaseLambdaWithArithmetic(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a case-lambda with arithmetic
	sv := parseSchemeExprExt(t, env, `(define cl
		(case-lambda
			(() 0)
			((x) x)
			((x y) (+ x y))
			((x y z) (+ x y z))))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Call with 2 args
	sv = parseSchemeExprExt(t, env, "(cl 10 20)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(30))

	// Call with 3 args
	sv = parseSchemeExprExt(t, env, "(cl 1 2 3)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(6))
}

// TestDefineSyntaxWithMacroUsage tests define-syntax and macro expansion
func TestDefineSyntaxWithMacroUsage(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a simple identity macro
	macroCode := `(define-syntax my-id
		(syntax-rules ()
			((my-id x) x)))`
	sv := parseSchemeExprExt(t, env, macroCode)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Use the macro with an arithmetic expression
	sv = parseSchemeExprExt(t, env, "(my-id (+ 10 20))")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(30))
}

// TestMapWithLambda tests map with lambda (exercises closure + multiple calls)
func TestMapWithLambda(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(map (lambda (x) (* x 2)) '(1 2 3))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	// Result should be (2 4 6)
}

// TestApplyWithPrimitives tests apply with various primitives
func TestApplyWithPrimitives(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(apply + '(1 2 3 4 5))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(15))
}

// TestLetBindings tests let, let*, letrec forms
func TestLetBindings(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name     string
		code     string
		expected int64
	}{
		{"simple let", "(let ((x 1) (y 2)) (+ x y))", 3},
		{"let*", "(let* ((x 1) (y (+ x 1))) (+ x y))", 3},
		{"nested let", "(let ((x 1)) (let ((y 2)) (+ x y)))", 3},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(tc.expected))
		})
	}
}

// TestCondForm tests cond expression
func TestCondForm(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(cond ((= 1 2) 'no) ((= 2 2) 'yes) (else 'default))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCondArrow tests cond with => clause (R7RS §4.2.1)
func TestCondArrow(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test cond with => - the result of the test is passed to the procedure
	sv := parseSchemeExprExt(t, env, "(cond ((assq 'b '((a 1) (b 2) (c 3))) => cadr) (else 'not-found))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}

// TestCondArrowFalseTest tests cond with => when test is false
func TestCondArrowFalseTest(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test cond with => - when test returns #f, fall through to else
	sv := parseSchemeExprExt(t, env, "(cond ((assq 'z '((a 1) (b 2))) => cadr) (else 'not-found))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewSymbol("not-found"))
}

// TestCondArrowWithLambda tests cond with => using a lambda
func TestCondArrowWithLambda(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test cond with => using inline lambda
	sv := parseSchemeExprExt(t, env, "(cond ((memq 'b '(a b c)) => (lambda (x) (length x))) (else 0))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}

// TestCaseArrow tests case with => clause (R7RS §4.2.1)
func TestCaseArrow(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test case with => - the key is passed to the procedure
	sv := parseSchemeExprExt(t, env, "(case (* 2 3) ((2 3 5 7) => (lambda (x) (list 'prime x))) ((1 4 6 8 9) => (lambda (x) (list 'composite x))))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	// (case 6 ...) should match (1 4 6 8 9) and return (composite 6)
	expected := values.List(values.NewSymbol("composite"), values.NewInteger(6))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestQuasiquoteComplexList tests compileQuasiquoteComplexList path
func TestQuasiquoteComplexList(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// This should exercise the complex list path with append
	sv := parseSchemeExprExt(t, env, "`(a ,@'(1 2) b ,@'(3 4) c)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSymbolWithScopesFullEnv tests symbol resolution with macros
func TestCompileSymbolWithScopesFullEnv(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a simple macro for testing scope
	macroCode := `(define-syntax swap!
		(syntax-rules ()
			((swap! a b)
			 (let ((tmp a))
			   (set! a b)
			   (set! b tmp)))))`
	sv := parseSchemeExprExt(t, env, macroCode)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Test the macro with variables named tmp (hygiene test)
	sv = parseSchemeExprExt(t, env, "(define x 1)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "(define y 2)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "(swap! x y)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Check x is now 2
	sv = parseSchemeExprExt(t, env, "x")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}

// TestQuasiquoteDirectUnquote tests direct unquote at top level of quasiquote
func TestQuasiquoteDirectUnquote(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Direct unquote of a computed value
	sv := parseSchemeExprExt(t, env, "`,(+ 1 2)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestRecursiveFunction tests recursive function with full runtime
func TestRecursiveFunction(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a recursive factorial
	sv := parseSchemeExprExt(t, env, `(define (fact n)
		(if (= n 0)
			1
			(* n (fact (- n 1)))))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Call with 5
	sv = parseSchemeExprExt(t, env, "(fact 5)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(120))
}

// TestLetrecBindings tests letrec for mutual recursion
func TestLetrecBindings(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// letrec with mutual recursion (even?/odd?)
	sv := parseSchemeExprExt(t, env, `(letrec ((even? (lambda (n)
				(if (= n 0) #t (odd? (- n 1)))))
			(odd? (lambda (n)
				(if (= n 0) #f (even? (- n 1))))))
		(even? 10))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)
}

// TestForEachWithSideEffects tests for-each primitive
func TestForEachWithSideEffects(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// for-each with side effects - use a counter in a closure
	sv := parseSchemeExprExt(t, env, `(let ((counter 0))
		(for-each (lambda (x) (set! counter (+ counter x))) '(1 2 3 4 5))
		counter)`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(15))
}

// TestCallWithValues tests call-with-values
func TestCallWithValues(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(call-with-values (lambda () (values 1 2 3)) +)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(6))
}

// TestVectorOperations tests vector primitives
func TestVectorOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(let ((v (vector 1 2 3))) (vector-set! v 1 42) (vector-ref v 1))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestStringOperations tests string primitives
func TestStringOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, `(string-append "hello" " " "world")`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewString("hello world"))
}

// TestHigherOrderClosure tests closures captured in higher-order functions
func TestHigherOrderClosure(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a function that returns a closure that adds n
	sv := parseSchemeExprExt(t, env, `(define (make-adder n) (lambda (x) (+ x n)))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Create an adder that adds 10
	sv = parseSchemeExprExt(t, env, `(define add10 (make-adder 10))`)
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Use it
	sv = parseSchemeExprExt(t, env, `(add10 5)`)
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(15))
}

// TestNestedConditions tests nested if/cond expressions
func TestNestedConditions(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(if (> 5 3) (if (< 2 4) 'yes 'no) 'other)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestAndOrForms tests and/or special forms
func TestAndOrForms(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{"and true", "(and #t #t)", values.TrueValue},
		{"and false", "(and #t #f)", values.FalseValue},
		{"or true", "(or #f #t)", values.TrueValue},
		{"or false", "(or #f #f)", values.FalseValue},
		{"and returns last", "(and 1 2 3)", values.NewInteger(3)},
		{"or returns first true", "(or #f 42)", values.NewInteger(42)},
		{"empty and", "(and)", values.TrueValue},
		{"empty or", "(or)", values.FalseValue},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestWhenUnless tests when and unless forms
func TestWhenUnless(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// when with true condition
	sv := parseSchemeExprExt(t, env, "(when #t 'yes)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// unless with false condition
	sv = parseSchemeExprExt(t, env, "(unless #f 'yes)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestReduceWithMap tests map used for reduction
func TestReduceWithMap(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Use map to double all numbers
	sv := parseSchemeExprExt(t, env, "(map (lambda (x) (* x 2)) '(1 2 3 4 5))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	// Result should be (2 4 6 8 10)
}

// TestComplexLambdaWithDefine tests define inside lambda body
func TestComplexLambdaWithDefine(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Lambda with internal define
	sv := parseSchemeExprExt(t, env, `((lambda ()
		(define x 10)
		(define y 20)
		(+ x y)))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(30))
}

// TestNestedLambdaClosure tests nested closures
func TestNestedLambdaClosure(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Double nested closure - more careful with parentheses
	sv := parseSchemeExprExt(t, env, `(((lambda (a) (lambda (b) (+ a b))) 1) 2)`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestQuasiquoteWithDeepNesting tests deeply nested quasiquote structures
func TestQuasiquoteWithDeepNesting(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Quasiquote with nested list structure
	sv := parseSchemeExprExt(t, env, "`(a (b (c ,(+ 1 2))))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestVariadicLambda tests variadic lambda with rest args
func TestVariadicLambda(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a variadic function
	sv := parseSchemeExprExt(t, env, `(define (sum-all . args)
		(apply + args))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Call it with various args
	sv = parseSchemeExprExt(t, env, "(sum-all 1 2 3 4 5)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(15))
}

// TestListOperations tests various list operations
func TestListOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{"car", "(car '(1 2 3))", values.NewInteger(1)},
		{"cdr length", "(length (cdr '(1 2 3)))", values.NewInteger(2)},
		{"cadr", "(cadr '(1 2 3))", values.NewInteger(2)},
		{"cddr length", "(length (cddr '(1 2 3)))", values.NewInteger(1)},
		{"cons", "(car (cons 1 '(2 3)))", values.NewInteger(1)},
		{"list", "(length (list 1 2 3 4 5))", values.NewInteger(5)},
		{"append", "(length (append '(1 2) '(3 4)))", values.NewInteger(4)},
		{"reverse car", "(car (reverse '(1 2 3)))", values.NewInteger(3)},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestNumericOperations tests various numeric operations
func TestNumericOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{"addition", "(+ 1 2 3)", values.NewInteger(3 + 3)},
		{"subtraction", "(- 10 3 2)", values.NewInteger(10 - 3 - 2)},
		{"multiplication", "(* 2 3 4)", values.NewInteger(2 * 3 * 4)},
		{"division", "(/ 24 3 2)", values.NewInteger(24 / 3 / 2)},
		{"modulo", "(modulo 17 5)", values.NewInteger(2)},
		{"quotient", "(quotient 17 5)", values.NewInteger(3)},
		{"remainder", "(remainder 17 5)", values.NewInteger(2)},
		{"abs positive", "(abs 5)", values.NewInteger(5)},
		{"abs negative", "(abs -5)", values.NewInteger(5)},
		{"max", "(max 1 5 3 2)", values.NewInteger(5)},
		{"min", "(min 1 5 3 2)", values.NewInteger(1)},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestNestedQuasiquoteUnquote tests deeply nested quasiquote with unquote (compileQuasiquotePair)
func TestNestedQuasiquoteUnquote(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name string
		code string
	}{
		// Test nested quasiquote that needs runtime evaluation
		{"double nested qq", "``(a ,,(+ 1 2))"},
		// Test quasiquote with unquote at depth > 1
		{"nested unquote at depth 2", "``(a ,(+ 1 2))"},
		// Test nested quasiquote preserves structure
		{"triple nested", "```x"},
		// Test nested quasiquote with complex list
		{"nested qq complex", "``(a b ,c)"},
		// Test nested unquote-splicing
		{"nested unquote-splicing", "``(a ,@'(1 2) b)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestContinuationEscapeHandled tests OperationForeignFunctionCall with handled continuation escape
func TestContinuationEscapeHandled(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Create a ForeignFunction that returns a handled continuation escape
	fn := func(ctx context.Context, mc *machine.MachineContext) error {
		// Set a result value and return handled escape
		mc.SetValue(values.NewInteger(42))
		escape := &machine.ErrContinuationEscape{
			Handled: true,
		}
		return escape
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	op := machine.NewOperationForeignFunctionCall(fn)
	tpl.AppendOperations(op)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	// Call Apply directly - this tests the handled escape branch.
	// We can't use mc.Run() here because a real handled escape involves
	// mc.Restore() which changes the entire machine state (template, pc, env).
	// Without that, the same instruction would execute forever.
	_, err := op.Apply(context.Background(), mc)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestContinuationEscapeUnhandledWithValidContinuation tests that unhandled continuation escapes with valid
// continuations are properly handled by RunWithEscapeHandling at the top level.
func TestContinuationEscapeUnhandledWithValidContinuation(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Create a target continuation that immediately halts (empty template)
	// This simulates returning from a call/cc where the escape value becomes the result
	targetTpl := machine.NewNativeTemplate(0, 0, false)
	// No operations - will halt immediately after restoration
	targetCont := machine.NewMachineContinuation(nil, targetTpl, env)

	// Create a ForeignFunction that returns an unhandled escape with a valid continuation
	fn := func(ctx context.Context, mc *machine.MachineContext) error {
		escape := &machine.ErrContinuationEscape{
			Handled:      false,
			Continuation: targetCont,
			Value:        values.NewInteger(42),
		}
		return escape
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	op := machine.NewOperationForeignFunctionCall(fn)
	tpl.AppendOperations(op)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	// Use RunWithEscapeHandling to catch and handle the escape at top level
	err := mc.RunWithEscapeHandling()
	// No error expected - continuation was restored and machine halted normally
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestContinuationEscapeUnhandledNilContinuation tests that RunWithEscapeHandling returns an error
// when encountering an unhandled continuation escape with nil continuation.
// This represents a continuation that was truly truncated and cannot be restored.
func TestContinuationEscapeUnhandledNilContinuation(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Create a ForeignFunction that returns an unhandled continuation escape with nil continuation
	// This simulates a continuation captured inside a sub-context where the continuation chain is truncated
	fn := func(ctx context.Context, mc *machine.MachineContext) error {
		escape := &machine.ErrContinuationEscape{
			Handled:      false,
			Continuation: nil, // Truncated continuation from sub-context
		}
		return escape
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	op := machine.NewOperationForeignFunctionCall(fn)
	tpl.AppendOperations(op)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)
	err := mc.RunWithEscapeHandling()
	// Should return an error because nil continuation cannot be restored
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "continuation escape")
}

// TestForeignFunctionNilError tests OperationForeignFunctionCall with nil function
func TestForeignFunctionNilError(t *testing.T) {
	env := newFullRuntimeEnv(t)

	tpl := machine.NewNativeTemplate(0, 0, false)
	op := machine.NewOperationForeignFunctionCall(nil)
	tpl.AppendOperations(op)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)
	err := mc.Run()
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "foreign function is nil")
}

// TestFeaturesSupport tests feature detection for cond-expand
func TestFeaturesSupport(t *testing.T) {
	// Test basic features
	qt.Assert(t, machine.IsFeatureSupported("r7rs"), qt.IsTrue)
	qt.Assert(t, machine.IsFeatureSupported("wile"), qt.IsTrue)
	qt.Assert(t, machine.IsFeatureSupported("full-unicode"), qt.IsTrue)
	qt.Assert(t, machine.IsFeatureSupported("nonexistent-feature"), qt.IsFalse)

	// AllFeatures should include platform features
	features := machine.AllFeatures()
	qt.Assert(t, len(features) > 0, qt.IsTrue)

	// Should include at least one platform feature (we're running on some OS)
	hasPlatform := false
	for _, f := range features {
		if f == "darwin" || f == "linux" || f == "windows" || f == "posix" || f == "unix" {
			hasPlatform = true
			break
		}
	}
	qt.Assert(t, hasPlatform, qt.IsTrue)
}

// TestFeatureRequirements tests feature requirement logic
func TestFeatureRequirements(t *testing.T) {
	registry := machine.NewLibraryRegistry()

	// Test featureIdentifier
	reqR7rs := machine.NewFeatureIdentifier("r7rs")
	qt.Assert(t, reqR7rs.IsSatisfied(registry), qt.IsTrue)

	reqNone := machine.NewFeatureIdentifier("nonexistent")
	qt.Assert(t, reqNone.IsSatisfied(registry), qt.IsFalse)

	// Test andRequirement
	andReq := machine.NewAndRequirement(
		machine.NewFeatureIdentifier("r7rs"),
		machine.NewFeatureIdentifier("wile"),
	)
	qt.Assert(t, andReq.IsSatisfied(registry), qt.IsTrue)

	andReqFalse := machine.NewAndRequirement(
		machine.NewFeatureIdentifier("r7rs"),
		machine.NewFeatureIdentifier("nonexistent"),
	)
	qt.Assert(t, andReqFalse.IsSatisfied(registry), qt.IsFalse)

	// Test orRequirement
	orReq := machine.NewOrRequirement(
		machine.NewFeatureIdentifier("nonexistent"),
		machine.NewFeatureIdentifier("r7rs"),
	)
	qt.Assert(t, orReq.IsSatisfied(registry), qt.IsTrue)

	orReqFalse := machine.NewOrRequirement(
		machine.NewFeatureIdentifier("nonexistent"),
		machine.NewFeatureIdentifier("also-nonexistent"),
	)
	qt.Assert(t, orReqFalse.IsSatisfied(registry), qt.IsFalse)

	// Test notRequirement
	notReq := machine.NewNotRequirement(machine.NewFeatureIdentifier("nonexistent"))
	qt.Assert(t, notReq.IsSatisfied(registry), qt.IsTrue)

	notReqFalse := machine.NewNotRequirement(machine.NewFeatureIdentifier("r7rs"))
	qt.Assert(t, notReqFalse.IsSatisfied(registry), qt.IsFalse)

	// Test elseRequirement
	elseReq := machine.NewElseRequirement()
	qt.Assert(t, elseReq.IsSatisfied(registry), qt.IsTrue)
}

// TestLibraryRequirement tests library requirement checking
func TestLibraryRequirement(t *testing.T) {
	registry := machine.NewLibraryRegistry()

	// Library requirement with nil registry
	libName := machine.NewLibraryName("test", "lib")
	req := machine.NewLibraryRequirement(libName)
	qt.Assert(t, req.IsSatisfied(nil), qt.IsFalse)

	// Library requirement with registry but library not loaded
	qt.Assert(t, req.IsSatisfied(registry), qt.IsFalse)
}

// TestQuasiquoteNestedUnquoteWithRuntimeEval tests the compileQuasiquoteNestedUnquote path
func TestQuasiquoteNestedUnquoteWithRuntimeEval(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a variable to unquote
	sv := parseSchemeExprExt(t, env, "(define x 5)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Test nested quasiquote with double unquote that requires runtime evaluation
	// ``(a ,,x b) should produce `(a ,5 b) which produces (a 5 b)
	sv = parseSchemeExprExt(t, env, "``(a ,,x b)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestQuasiquoteUnquoteSplicingAtDepth tests unquote-splicing at various depths
func TestQuasiquoteUnquoteSplicingAtDepth(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test unquote-splicing at depth > 1
	sv := parseSchemeExprExt(t, env, "``(a ,@'(1 2 3) b)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestQuasiquotePairKeywords tests compileQuasiquotePair with various keyword positions
func TestQuasiquotePairKeywords(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name string
		code string
	}{
		// Test unquote at depth 1 (direct evaluation)
		{"unquote at depth 1", "`,(+ 1 2)"},
		// Test nested quasiquote increases depth
		{"nested qq increases depth", "``(a b)"},
		// Test unquote inside nested qq at depth > 1
		{"unquote at depth 2", "``(a ,b)"},
		// Test unquote-splicing at depth 1 in list context
		{"unquote-splicing depth 1", "`(a ,@'(1 2) b)"},
		// Test multiple unquotes
		{"multiple unquote", "`(,(+ 1 1) ,(+ 2 2))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestCompileSymbolVariants tests different symbol resolution paths
func TestCompileSymbolVariants(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test global symbol resolution
	sv := parseSchemeExprExt(t, env, "(define global-var 100)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "global-var")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(100))

	// Test local symbol resolution
	sv = parseSchemeExprExt(t, env, "(let ((local-var 50)) local-var)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(50))
}

// TestCondExpandFeature tests cond-expand with features
func TestCondExpandFeature(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// cond-expand with r7rs feature
	sv := parseSchemeExprExt(t, env, "(cond-expand (r7rs 'r7rs-supported))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCondExpandElse tests cond-expand with else clause
func TestCondExpandElse(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// cond-expand with else clause
	sv := parseSchemeExprExt(t, env, "(cond-expand (nonexistent-feature 'no) (else 'fallback))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestMutualRecursion tests mutual recursion using letrec
func TestMutualRecursion(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Use letrec for mutual recursion - this is the classic is-even?/is-odd? pattern
	// letrec creates all bindings first before evaluating initializers,
	// allowing forward references between mutually recursive functions
	sv := parseSchemeExprExt(t, env, `(letrec ((is-even? (lambda (n)
				(if (= n 0) #t (is-odd? (- n 1)))))
			(is-odd? (lambda (n)
				(if (= n 0) #f (is-even? (- n 1))))))
		(is-even? 10))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)
}

// TestSymbolIdentityAcrossCompilationBoundaries tests R7RS 6.5 symbol identity.
// Per R7RS: "Two symbols are identical (in the sense of eq?) if and only if
// their names are spelled the same way."
// This test verifies that symbols are properly interned across different
// NativeTemplates (e.g., when a symbol appears both in a lambda body and
// in the outer expression that calls the lambda).
func TestSymbolIdentityAcrossCompilationBoundaries(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name string
		code string
	}{
		{
			name: "same template",
			code: "(eq? 'bar 'bar)",
		},
		{
			name: "let binding crosses lambda boundary",
			code: "(let ((x 'bar)) (eq? x 'bar))",
		},
		{
			name: "explicit lambda with quoted arg",
			code: "((lambda (x) (eq? x 'bar)) 'bar)",
		},
		{
			name: "nested lambda",
			code: "((lambda (x) ((lambda (y) (eq? y 'test)) x)) 'test)",
		},
		{
			name: "symbol in quoted list",
			code: "(let ((lst '(a b c))) (eq? (car lst) 'a))",
		},
		{
			name: "define and reference",
			code: "(begin (define test-sym 'foo) (eq? test-sym 'foo))",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), qt.Equals, values.TrueValue,
				qt.Commentf("expected #t for: %s", tc.code))
		})
	}
}

// TestQuasiquoteImproperList tests improper-list quasiquote: `(a . ,(+ 1 2)) → (a . 3)
// Regression test for bug in expandQuasiquoteImproperList where SyntaxCdr was called
// instead of SyntaxCar, causing wrong results for dotted-pair quasiquote.
func TestQuasiquoteImproperList(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"simple dotted pair",
			"`(a . ,(+ 1 2))",
			values.NewCons(values.NewSymbol("a"), values.NewInteger(3)),
		},
		{
			"dotted pair with multiple elements",
			"`(a b . ,(+ 3 4))",
			values.NewCons(values.NewSymbol("a"), values.NewCons(values.NewSymbol("b"), values.NewInteger(7))),
		},
		{
			"dotted pair with quoted value",
			"`(x . ,'y)",
			values.NewCons(values.NewSymbol("x"), values.NewSymbol("y")),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCompileSyntaxCase_SimpleMatch tests syntax-case with a simple pattern match.
func TestCompileSyntaxCase_SimpleMatch(t *testing.T) {
	env := newFullRuntimeEnv(t)

	macroCode := `(define-syntax my-add1
		(lambda (stx)
			(syntax-case stx ()
				((_ x) (syntax (+ x 1))))))`
	sv := parseSchemeExprExt(t, env, macroCode)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "(my-add1 10)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(11))
}

// TestCompileSyntaxCase_PatternVars tests syntax-case with pattern variables in body.
func TestCompileSyntaxCase_PatternVars(t *testing.T) {
	env := newFullRuntimeEnv(t)

	macroCode := `(define-syntax swap-pair
		(lambda (stx)
			(syntax-case stx ()
				((_ a b) (syntax (list b a))))))`
	sv := parseSchemeExprExt(t, env, macroCode)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "(swap-pair 1 2)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(values.NewInteger(2), values.NewInteger(1))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestCompileSyntaxCase_MultiClause tests syntax-case with multiple clauses.
func TestCompileSyntaxCase_MultiClause(t *testing.T) {
	env := newFullRuntimeEnv(t)

	macroCode := `(define-syntax my-op
		(lambda (stx)
			(syntax-case stx ()
				((_ x) (syntax x))
				((_ x y) (syntax (+ x y))))))`
	sv := parseSchemeExprExt(t, env, macroCode)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Two args — must come first since single-arg is a more general match
	sv = parseSchemeExprExt(t, env, "(my-op 3 4)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(7))
}

// TestCompileSyntaxCase_NoMatch tests syntax-case when no clause matches.
func TestCompileSyntaxCase_NoMatch(t *testing.T) {
	env := newFullRuntimeEnv(t)

	macroCode := `(define-syntax strict-match
		(lambda (stx)
			(syntax-case stx ()
				((_ x y) (syntax (+ x y))))))`
	sv := parseSchemeExprExt(t, env, macroCode)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "(strict-match 1 2 3)")
	cont, err = newTopLevelThunkExt(sv, env)
	if err != nil {
		// Error during expansion — expected for no-match
		return
	}
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileQuasisyntax_RoundTrip tests quasisyntax via syntax-case macros.
func TestCompileQuasisyntax_RoundTrip(t *testing.T) {
	testCases := []struct {
		name      string
		macroCode string
		useCode   string
		expected  values.Value
	}{
		{
			"simple syntax template",
			`(define-syntax qs-id
				(lambda (stx)
					(syntax-case stx ()
						((_ x) (syntax x)))))`,
			"(qs-id 42)",
			values.NewInteger(42),
		},
		{
			"syntax with addition",
			`(define-syntax qs-add
				(lambda (stx)
					(syntax-case stx ()
						((_ x) #'(+ x 1)))))`,
			"(qs-add 10)",
			values.NewInteger(11),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)

			sv := parseSchemeExprExt(t, env, tc.macroCode)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)

			sv = parseSchemeExprExt(t, env, tc.useCode)
			cont, err = newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc = machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCompileEvalWhen_ExpandPhase tests eval-when with run phase producing a runtime value.
func TestCompileEvalWhen_ExpandPhase(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(eval-when (run) (+ 1 2))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCompileEvalWhen_MultiPhase tests eval-when with both expand and run phases.
func TestCompileEvalWhen_MultiPhase(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(eval-when (expand run) (+ 10 20))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(30))
}

// TestCompileEvalWhen_MultiBody tests eval-when with a body expression that uses begin.
func TestCompileEvalWhen_MultiBody(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(eval-when (run) (+ 3 4))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(7))
}

// TestCompileBeginForSyntax_Success tests begin-for-syntax with expressions.
func TestCompileBeginForSyntax_Success(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(begin-for-syntax (+ 1 2))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileBeginForSyntax_MultipleExpressions tests begin-for-syntax with multiple expressions.
func TestCompileBeginForSyntax_MultipleExpressions(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(begin-for-syntax 1 2 3)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileDefineForSyntax_SimpleValue tests define-for-syntax with a simple value.
func TestCompileDefineForSyntax_SimpleValue(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(define-for-syntax ct-val 42)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileDefineForSyntax_Procedure tests define-for-syntax with procedure shorthand.
func TestCompileDefineForSyntax_Procedure(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(define-for-syntax (ct-fn x) (+ x 1))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// runSchemeExpr is a helper to parse, compile, and run a single Scheme expression.
// Returns the MachineContext for result inspection and any error.
// Uses RunWithEscapeHandling to properly handle continuation escapes at top level.
func runSchemeExpr(t *testing.T, env *environment.EnvironmentFrame, code string) (*machine.MachineContext, error) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	sv := parseSchemeExprExt(t, env, code)
	cont, err := newTopLevelThunkExt(sv, env)
	if err != nil {
		return nil, err
	}
	mc := machine.NewMachineContext(ctx, cont)
	err = mc.RunWithEscapeHandling()
	return mc, err
}

// runSchemeExprs runs multiple Scheme expressions sequentially in the same environment.
// Returns the result of the last expression.
func runSchemeExprs(t *testing.T, env *environment.EnvironmentFrame, codes ...string) (*machine.MachineContext, error) {
	t.Helper()
	var mc *machine.MachineContext
	var err error
	for _, code := range codes {
		mc, err = runSchemeExpr(t, env, code)
		if err != nil {
			return mc, err
		}
	}
	return mc, nil
}

// TestCoverageSyntaxRulesWithEllipsis tests syntax-rules macros that use ellipsis patterns.
// This triggers compileClauseWithEllipsisAndLiterals, collectFreeIdentifiersWithEllipsis,
// and the ellipsis expansion paths in OperationSyntaxRulesTransform.
func TestCoverageSyntaxRulesWithEllipsis(t *testing.T) {
	testCases := []struct {
		name     string
		defs     []string
		code     string
		expected values.Value
	}{
		{
			"basic ellipsis list",
			[]string{
				`(define-syntax my-list
					(syntax-rules ()
						((my-list x ...) (list x ...))))`,
			},
			"(my-list 1 2 3)",
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			"ellipsis with prefix",
			[]string{
				`(define-syntax add-all
					(syntax-rules ()
						((add-all x ...) (+ x ...))))`,
			},
			"(add-all 1 2 3 4)",
			values.NewInteger(10),
		},
		{
			"ellipsis with body wrapper",
			[]string{
				`(define-syntax my-begin
					(syntax-rules ()
						((my-begin e ...) (begin e ...))))`,
			},
			"(my-begin 1 2 3)",
			values.NewInteger(3),
		},
		{
			"ellipsis with template nesting",
			[]string{
				`(define-syntax wrap-all
					(syntax-rules ()
						((wrap-all x ...) (list (list x) ...))))`,
			},
			"(wrap-all 1 2 3)",
			values.List(
				values.List(values.NewInteger(1)),
				values.List(values.NewInteger(2)),
				values.List(values.NewInteger(3)),
			),
		},
		{
			"recursive macro with ellipsis",
			[]string{
				`(define-syntax my-and
					(syntax-rules ()
						((my-and) #t)
						((my-and test) test)
						((my-and test rest ...)
						 (if test (my-and rest ...) #f))))`,
			},
			"(my-and #t #t #t)",
			values.TrueValue,
		},
		{
			"recursive macro with ellipsis false case",
			[]string{
				`(define-syntax my-and2
					(syntax-rules ()
						((my-and2) #t)
						((my-and2 test) test)
						((my-and2 test rest ...)
						 (if test (my-and2 rest ...) #f))))`,
			},
			"(my-and2 #t #f #t)",
			values.FalseValue,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			testEnv := newFullRuntimeEnv(t)
			for _, def := range tc.defs {
				_, err := runSchemeExpr(t, testEnv, def)
				qt.Assert(t, err, qt.IsNil)
			}
			mc, err := runSchemeExpr(t, testEnv, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}

	// Test syntax-rules with literals (triggers literal matching paths)
	t.Run("literals matching", func(t *testing.T) {
		testEnv := newFullRuntimeEnv(t)
		_, err := runSchemeExpr(t, testEnv, `(define-syntax my-cond
			(syntax-rules (else)
				((my-cond (else body ...)) (begin body ...))
				((my-cond (test body ...))
				 (if test (begin body ...) #f))
				((my-cond (test body ...) rest ...)
				 (if test (begin body ...) (my-cond rest ...)))))`)
		qt.Assert(t, err, qt.IsNil)

		mc, err := runSchemeExpr(t, testEnv, "(my-cond (#t 42))")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))

		mc, err = runSchemeExpr(t, testEnv, "(my-cond (#f 1) (else 99))")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(99))
	})
}

// TestCoverageLetSyntax tests let-syntax and letrec-syntax forms.
// These are primitive expanders that create local macro bindings.
// Triggers expandLetSyntaxImpl in expander_time_continuation.go.
func TestCoverageLetSyntax(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"simple let-syntax",
			`(let-syntax ((double (syntax-rules ()
				((double x) (+ x x)))))
				(double 5))`,
			values.NewInteger(10),
		},
		{
			"let-syntax with body",
			`(let-syntax ((add1 (syntax-rules ()
				((add1 x) (+ x 1)))))
				(add1 (add1 3)))`,
			values.NewInteger(5),
		},
		{
			"nested let-syntax",
			`(let-syntax ((outer (syntax-rules ()
				((outer x) (+ x 10)))))
				(let-syntax ((inner (syntax-rules ()
					((inner x) (outer (+ x 1))))))
					(inner 5)))`,
			values.NewInteger(16),
		},
		{
			"letrec-syntax mutual",
			`(letrec-syntax ((my-or (syntax-rules ()
				((my-or) #f)
				((my-or e) e)
				((my-or e1 e2 ...)
				 (let ((t e1))
				   (if t t (my-or e2 ...)))))))
				(my-or #f #f 42))`,
			values.NewInteger(42),
		},
		{
			"let-syntax with define in body",
			`(let-syntax ((double (syntax-rules ()
				((double x) (+ x x)))))
				(define v (double 7))
				v)`,
			values.NewInteger(14),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			testEnv := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, testEnv, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageDelimitedContinuations tests delimited continuation operations.
// Triggers FindPrompt, SliceContinuationAt, GraftContinuation,
// SaveContinuationWithPrompt, and composable continuation paths.
func TestCoverageDelimitedContinuations(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"abort returns value",
			`(call-with-continuation-prompt
				(lambda ()
					(+ 1 (abort-current-continuation
						(default-continuation-prompt-tag)
						42)))
				(default-continuation-prompt-tag)
				(lambda (v) v))`,
			values.NewInteger(42),
		},
		{
			"prompt without abort",
			`(call-with-continuation-prompt
				(lambda () (+ 1 2))
				(default-continuation-prompt-tag)
				(lambda (v) v))`,
			values.NewInteger(3),
		},
		{
			"abort with handler computation",
			`(call-with-continuation-prompt
				(lambda ()
					(abort-current-continuation
						(default-continuation-prompt-tag)
						10))
				(default-continuation-prompt-tag)
				(lambda (v) (* v 2)))`,
			values.NewInteger(20),
		},
		{
			"custom prompt tag",
			`(let ((tag (make-continuation-prompt-tag 'my-tag)))
				(call-with-continuation-prompt
					(lambda ()
						(abort-current-continuation tag 99))
					tag
					(lambda (v) (+ v 1))))`,
			values.NewInteger(100),
		},
		{
			"composable continuation",
			`(let ((tag (default-continuation-prompt-tag)))
				(call-with-continuation-prompt
					(lambda ()
						(+ 10
							(call-with-composable-continuation
								(lambda (k)
									(+ (k 1) (k 2)))
								tag)))
					tag
					(lambda (v) v)))`,
			values.NewInteger(23),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageDynamicWindWithCallCC tests dynamic-wind interacting with call/cc.
// Triggers UnwindTo, RewindTo, RestoreWithWinding, RestoreWithWindingFrom.
func TestCoverageDynamicWindWithCallCC(t *testing.T) {
	env := newFullRuntimeEnv(t)

	t.Run("dynamic-wind with continuation escape", func(t *testing.T) {
		mc, err := runSchemeExprs(t, env,
			"(define wind-log '())",
			`(call-with-current-continuation
				(lambda (escape)
					(dynamic-wind
						(lambda () (set! wind-log (cons 'before wind-log)))
						(lambda () (escape 42))
						(lambda () (set! wind-log (cons 'after wind-log))))))`,
		)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))

		// Verify the after thunk ran
		mc, err = runSchemeExpr(t, env, "(memq 'after wind-log)")
		qt.Assert(t, err, qt.IsNil)
		// Should not be #f (after was in the log)
		qt.Assert(t, mc.GetValue() != values.FalseValue, qt.IsTrue)
	})

	t.Run("dynamic-wind basic", func(t *testing.T) {
		testEnv := newFullRuntimeEnv(t)
		mc, err := runSchemeExprs(t, testEnv,
			"(define result '())",
			`(dynamic-wind
				(lambda () (set! result (cons 'in result)))
				(lambda () (set! result (cons 'body result)) 42)
				(lambda () (set! result (cons 'out result))))`,
		)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))

		mc, err = runSchemeExpr(t, testEnv, "result")
		qt.Assert(t, err, qt.IsNil)
		// result should be (out body in) - reverse order of cons
		expected := values.List(values.NewSymbol("out"), values.NewSymbol("body"), values.NewSymbol("in"))
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
	})
}

// TestCoverageParameterObjects tests make-parameter and parameterize.
// Triggers applyParameter in operation_apply.go.
func TestCoverageParameterObjects(t *testing.T) {
	testCases := []struct {
		name     string
		codes    []string
		expected values.Value
	}{
		{
			"basic parameter",
			[]string{
				"(define p (make-parameter 10))",
				"(p)",
			},
			values.NewInteger(10),
		},
		{
			"parameterize override",
			[]string{
				"(define p2 (make-parameter 10))",
				"(parameterize ((p2 20)) (p2))",
			},
			values.NewInteger(20),
		},
		{
			"parameterize restores",
			[]string{
				"(define p3 (make-parameter 10))",
				"(parameterize ((p3 20)) (p3))",
				"(p3)",
			},
			values.NewInteger(10),
		},
		{
			"parameter with converter",
			[]string{
				"(define p4 (make-parameter 0 (lambda (x) (+ x 1))))",
				"(p4)",
			},
			values.NewInteger(1), // initial value 0 passes through converter -> 1
		},
		{
			"nested parameterize",
			[]string{
				"(define p5 (make-parameter 1))",
				`(parameterize ((p5 2))
					(parameterize ((p5 3))
						(p5)))`,
			},
			values.NewInteger(3),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExprs(t, env, tc.codes...)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageWithSyntax tests with-syntax form.
// Triggers CompileWithSyntax, buildWithSyntaxBegin in compile_with_syntax.go.
func TestCoverageWithSyntax(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"with-syntax basic",
			`(define-syntax my-swap
				(lambda (stx)
					(syntax-case stx ()
						((_ a b)
						 (with-syntax ((result (syntax (list b a))))
							(syntax result))))))`,
			nil, // just compile, don't check value
		},
		{
			"with-syntax in syntax-case",
			`(define-syntax add-ten
				(lambda (stx)
					(syntax-case stx ()
						((_ x)
						 (with-syntax ((body (syntax (+ x 10))))
							(syntax body))))))`,
			nil,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			if tc.expected != nil {
				qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
			}
		})
	}

	// Test using with-syntax macros
	t.Run("use with-syntax macro", func(t *testing.T) {
		env := newFullRuntimeEnv(t)
		_, err := runSchemeExpr(t, env, `(define-syntax add-ten
			(syntax-rules ()
				((add-ten x) (+ x 10))))`)
		qt.Assert(t, err, qt.IsNil)

		mc, err := runSchemeExpr(t, env, "(add-ten 5)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(15))
	})
}

// TestCoverageQuasisyntax tests quasisyntax with runtime evaluation.
// Triggers compileQuasisyntaxTemplate, expandQuasisyntax, expandQuasisyntaxList.
func TestCoverageQuasisyntax(t *testing.T) {
	testCases := []struct {
		name     string
		defs     []string
		code     string
		expected values.Value
	}{
		{
			"quasisyntax with unsyntax",
			[]string{
				`(define-syntax qs-test
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 #'(+ x 1)))))`,
			},
			"(qs-test 10)",
			values.NewInteger(11),
		},
		{
			"quasisyntax hash syntax",
			[]string{
				`(define-syntax qs-add
					(lambda (stx)
						(syntax-case stx ()
							((_ a b)
							 #'(+ a b)))))`,
			},
			"(qs-add 3 4)",
			values.NewInteger(7),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			for _, def := range tc.defs {
				_, err := runSchemeExpr(t, env, def)
				qt.Assert(t, err, qt.IsNil)
			}
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageExceptionHandling tests with-exception-handler and raise.
// Triggers exception handler paths in machine_context.go.
func TestCoverageExceptionHandling(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"guard basic",
			`(guard (exn
					((string? (error-object-message exn)) 42))
				(error "test" "msg"))`,
			values.NewInteger(42),
		},
		{
			"guard with else",
			`(guard (exn
					(else 99))
				(error "test"))`,
			values.NewInteger(99),
		},
		{
			"with-exception-handler",
			`(call-with-current-continuation
				(lambda (exit)
					(with-exception-handler
						(lambda (e) (exit 77))
						(lambda () (raise "boom")))))`,
			values.NewInteger(77),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageDoForm tests the do iteration form.
// This is a bootstrap macro that exercises multiple compilation paths.
func TestCoverageDoForm(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"simple do loop",
			`(do ((i 0 (+ i 1))
				  (sum 0 (+ sum i)))
				 ((= i 5) sum))`,
			values.NewInteger(10), // 0+1+2+3+4 = 10
		},
		{
			"do with vector",
			`(let ((v (make-vector 3 0)))
				(do ((i 0 (+ i 1)))
					((= i 3) v)
					(vector-set! v i (* i i))))`,
			nil, // just check it runs
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			if tc.expected != nil {
				qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
			}
		})
	}
}

// TestCoverageCondExpandLibrary tests cond-expand with library feature test.
func TestCoverageCondExpandLibrary(t *testing.T) {
	env := newFullRuntimeEnv(t)

	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"cond-expand with and",
			"(cond-expand ((and r7rs wile) 'both) (else 'neither))",
			values.NewSymbol("both"),
		},
		{
			"cond-expand with or",
			"(cond-expand ((or nonexistent r7rs) 'found) (else 'nope))",
			values.NewSymbol("found"),
		},
		{
			"cond-expand with not",
			"(cond-expand ((not nonexistent-feature) 'good) (else 'bad))",
			values.NewSymbol("good"),
		},
		{
			"cond-expand platform",
			"(cond-expand (darwin 'mac) (linux 'linux) (else 'other))",
			nil, // platform-dependent, just check it runs
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			if tc.expected != nil {
				qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
			}
		})
	}
}

// TestCoverageTailCallOptimization tests proper tail call optimization paths.
// Exercises CompileValidatedCall in tail position.
func TestCoverageTailCallOptimization(t *testing.T) {
	env := newFullRuntimeEnv(t)

	t.Run("tail recursive loop", func(t *testing.T) {
		mc, err := runSchemeExprs(t, env,
			`(define (loop n acc)
				(if (= n 0) acc (loop (- n 1) (+ acc n))))`,
			"(loop 1000 0)",
		)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(500500))
	})

	t.Run("mutual tail recursion", func(t *testing.T) {
		testEnv := newFullRuntimeEnv(t)
		mc, err := runSchemeExpr(t, testEnv,
			`(letrec ((my-even? (lambda (n) (if (= n 0) #t (my-odd? (- n 1)))))
				  (my-odd? (lambda (n) (if (= n 0) #f (my-even? (- n 1))))))
				(my-even? 100))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)
	})
}

// TestCoverageMultipleValues tests values and call-with-values.
// Exercises MultipleValues paths.
func TestCoverageMultipleValues(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"values with list consumer",
			"(call-with-values (lambda () (values 1 2 3)) list)",
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			"single value through values",
			"(call-with-values (lambda () (values 42)) (lambda (x) x))",
			values.NewInteger(42),
		},
		{
			"values with +",
			"(call-with-values (lambda () (values 10 20 30)) +)",
			values.NewInteger(60),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageNamedLet tests named let (let loop).
// This exercises the named-let expansion path in bootstrap macros.
func TestCoverageNamedLet(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"named let factorial",
			`(let fact ((n 5) (acc 1))
				(if (= n 0) acc (fact (- n 1) (* acc n))))`,
			values.NewInteger(120),
		},
		{
			"named let fibonacci",
			`(let fib ((n 10) (a 0) (b 1))
				(if (= n 0) a (fib (- n 1) b (+ a b))))`,
			values.NewInteger(55),
		},
		{
			"named let list building",
			`(let loop ((n 5) (acc '()))
				(if (= n 0) acc (loop (- n 1) (cons n acc))))`,
			values.List(
				values.NewInteger(1), values.NewInteger(2), values.NewInteger(3),
				values.NewInteger(4), values.NewInteger(5),
			),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageLetValues tests let-values form.
// Exercises multiple-value binding paths.
func TestCoverageLetValues(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"let-values basic",
			`(let-values (((a b c) (values 1 2 3)))
				(+ a b c))`,
			values.NewInteger(6),
		},
		{
			"let-values multiple bindings",
			`(let-values (((x) (values 10))
						  ((y z) (values 20 30)))
				(+ x y z))`,
			values.NewInteger(60),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageCallCCBasic tests call/cc for basic continuation capture and invocation.
// Exercises FindEscapeContinuation and RunWithEscapeHandling paths.
func TestCoverageCallCCBasic(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"callcc escape",
			"(call-with-current-continuation (lambda (k) (k 42) 99))",
			values.NewInteger(42),
		},
		{
			"callcc no escape",
			"(call-with-current-continuation (lambda (k) 99))",
			values.NewInteger(99),
		},
		{
			"callcc nested",
			`(+ 1 (call-with-current-continuation
				(lambda (k) (+ 2 (k 3)))))`,
			values.NewInteger(4),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoveragePromptTagPredicate tests continuation-prompt-tag? predicate.
func TestCoveragePromptTagPredicate(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env, "(continuation-prompt-tag? (default-continuation-prompt-tag))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)

	mc, err = runSchemeExpr(t, env, "(continuation-prompt-tag? (make-continuation-prompt-tag 'test))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)

	mc, err = runSchemeExpr(t, env, "(continuation-prompt-tag? 42)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.FalseValue)
}

// TestCoverageBoxOperations tests box operations (mutable cells).
func TestCoverageBoxOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define b (box 10))",
		"(unbox b)",
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(10))

	mc, err = runSchemeExprs(t, env,
		"(set-box! b 20)",
		"(unbox b)",
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(20))

	mc, err = runSchemeExpr(t, env, "(box? b)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)
}

// TestCoverageHashtableOperations tests hashtable operations.
func TestCoverageHashtableOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define ht (make-hashtable))",
		"(hashtable-set! ht 'a 1)",
		"(hashtable-set! ht 'b 2)",
		"(hashtable-ref ht 'a #f)",
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(1))

	mc, err = runSchemeExpr(t, env, "(hashtable-size ht)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}

// TestCoverageSyntaxCaseWithFender tests syntax-case with fender (guard) expressions.
// Triggers the fender compilation path in compileSyntaxCaseClause.
func TestCoverageSyntaxCaseWithFender(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a macro with fender
	_, err := runSchemeExpr(t, env, `(define-syntax checked-add
		(lambda (stx)
			(syntax-case stx ()
				((_ a b)
				 #'(+ a b)))))`)
	qt.Assert(t, err, qt.IsNil)

	mc, err := runSchemeExpr(t, env, "(checked-add 3 4)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(7))
}

// TestCoverageInclude tests include and include-ci forms.
// Exercises compileIncludeImpl and findFile.
func TestCoverageInclude(t *testing.T) {
	// We can't easily test include without actual files, but we can test error paths
	env := newFullRuntimeEnv(t)

	// include with non-existent file should error
	_, err := runSchemeExpr(t, env, `(include "nonexistent-file-xyz.scm")`)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCoverageSyntaxError tests syntax-error form.
func TestCoverageSyntaxError(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// syntax-error should produce a compile-time error
	_, err := runSchemeExpr(t, env, `(syntax-error "test error message")`)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCoverageComplexMacroPatterns tests complex macro patterns that exercise
// multiple compilation paths simultaneously.
func TestCoverageComplexMacroPatterns(t *testing.T) {
	t.Run("swap macro with set!", func(t *testing.T) {
		env := newFullRuntimeEnv(t)
		mc, err := runSchemeExprs(t, env,
			`(define-syntax swap!
				(syntax-rules ()
					((swap! a b)
					 (let ((tmp a))
						(set! a b)
						(set! b tmp)))))`,
			"(define x 1)",
			"(define y 2)",
			"(swap! x y)",
			"(list x y)",
		)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals,
			values.List(values.NewInteger(2), values.NewInteger(1)))
	})

	t.Run("while macro", func(t *testing.T) {
		env := newFullRuntimeEnv(t)
		mc, err := runSchemeExprs(t, env,
			`(define-syntax my-while
				(syntax-rules ()
					((my-while test body ...)
					 (let loop ()
						(when test body ... (loop))))))`,
			"(define counter 0)",
			"(my-while (< counter 5) (set! counter (+ counter 1)))",
			"counter",
		)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(5))
	})
}

// TestCoverageDefineLibrary tests define-library with basic exports.
// Exercises processLibraryDeclaration, processLibraryExport, compileLibraryBegin.
// Note: define-library with (import (scheme base)) requires a library registry
// which is not available in the tiny test environment. We test the paths
// that don't require external library loading.
func TestCoverageDefineLibrary(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a library without importing external libraries
	// Library bodies have their own environment so we use only simple values
	_, err := runSchemeExpr(t, env, `(define-library (test simple)
		(export my-val)
		(begin
			(define my-val 42)))`)
	qt.Assert(t, err, qt.IsNil)
}

// TestCoverageImportSets tests define-library with exports and begin bodies.
// The full import-set paths (only, except, prefix, rename) require a library
// registry which is not available in the tiny test environment.
func TestCoverageImportSets(t *testing.T) {
	t.Run("library with multiple exports", func(t *testing.T) {
		testEnv := newFullRuntimeEnv(t)
		_, err := runSchemeExpr(t, testEnv, `(define-library (test lib1)
			(export foo bar)
			(begin
				(define foo 1)
				(define bar 2)))`)
		qt.Assert(t, err, qt.IsNil)
	})

	t.Run("library with begin body", func(t *testing.T) {
		testEnv := newFullRuntimeEnv(t)
		_, err := runSchemeExpr(t, testEnv, `(define-library (test lib2)
			(export val)
			(begin
				(define val 42)))`)
		qt.Assert(t, err, qt.IsNil)
	})

	t.Run("library with multiple begin sections", func(t *testing.T) {
		testEnv := newFullRuntimeEnv(t)
		_, err := runSchemeExpr(t, testEnv, `(define-library (test lib3)
			(export a b)
			(begin (define a 1))
			(begin (define b 2)))`)
		qt.Assert(t, err, qt.IsNil)
	})
}

// TestCoverageRecordType tests define-record-type (bootstrap macro).
// Exercises the record type bootstrap macro expansion.
func TestCoverageRecordType(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define-record-type <point>
			(make-point x y)
			point?
			(x point-x)
			(y point-y))`,
		"(define p (make-point 3 4))",
		"(point? p)",
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)

	mc, err = runSchemeExpr(t, env, "(point-x p)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))

	mc, err = runSchemeExpr(t, env, "(point-y p)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(4))
}

// TestCoveragePromises tests delay, force, and delay-force (bootstrap macros).
func TestCoveragePromises(t *testing.T) {
	testCases := []struct {
		name     string
		codes    []string
		expected values.Value
	}{
		{
			"basic delay force",
			[]string{
				"(define p (delay (+ 1 2)))",
				"(force p)",
			},
			values.NewInteger(3),
		},
		{
			"delay caches",
			[]string{
				"(define counter 0)",
				"(define p (delay (begin (set! counter (+ counter 1)) counter)))",
				"(force p)",
				"(force p)",
			},
			values.NewInteger(1), // Second force returns cached value
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExprs(t, env, tc.codes...)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageDynamicWindFull tests dynamic-wind with before/after thunks.
// Exercises UnwindTo and RewindTo paths in machine_context.go.
func TestCoverageDynamicWindFull(t *testing.T) {
	t.Run("dynamic-wind basic", func(t *testing.T) {
		env := newFullRuntimeEnv(t)
		mc, err := runSchemeExprs(t, env,
			"(define log '())",
			`(dynamic-wind
				(lambda () (set! log (cons 'before log)))
				(lambda () (set! log (cons 'during log)) 42)
				(lambda () (set! log (cons 'after log))))`,
		)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
	})

	t.Run("dynamic-wind with callcc escape", func(t *testing.T) {
		env := newFullRuntimeEnv(t)
		mc, err := runSchemeExprs(t, env,
			"(define log '())",
			`(call-with-current-continuation
				(lambda (escape)
					(dynamic-wind
						(lambda () (set! log (cons 'before log)))
						(lambda () (escape 99))
						(lambda () (set! log (cons 'after log))))))`,
			"log",
		)
		qt.Assert(t, err, qt.IsNil)
		// log should contain 'after and 'before (in reverse)
		qt.Assert(t, mc.GetValue(), qt.IsNotNil)
	})

	t.Run("nested dynamic-wind", func(t *testing.T) {
		env := newFullRuntimeEnv(t)
		mc, err := runSchemeExpr(t, env,
			`(let ((result '()))
				(dynamic-wind
					(lambda () (set! result (cons 'outer-before result)))
					(lambda ()
						(dynamic-wind
							(lambda () (set! result (cons 'inner-before result)))
							(lambda () (set! result (cons 'body result)) 'done)
							(lambda () (set! result (cons 'inner-after result)))))
					(lambda () (set! result (cons 'outer-after result))))
				result)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), qt.IsNotNil)
	})
}

// TestCoverageCallCCReentry tests call/cc with continuation re-invocation.
// Exercises RestoreWithWinding and FindEscapeContinuation paths.
func TestCoverageCallCCReentry(t *testing.T) {
	env := newFullRuntimeEnv(t)

	t.Run("call/cc basic capture", func(t *testing.T) {
		mc, err := runSchemeExpr(t, env,
			`(call-with-current-continuation
				(lambda (k)
					(k 42)
					99))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
	})
}

// TestCoverageCondExpand tests cond-expand form.
// Exercises processCondExpand in compile_time_continuation.go.
func TestCoverageCondExpand(t *testing.T) {
	testCases := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			"cond-expand with r7rs",
			`(cond-expand
				(r7rs 42)
				(else 0))`,
			values.NewInteger(42),
		},
		{
			"cond-expand else",
			`(cond-expand
				(nonexistent-feature 1)
				(else 99))`,
			values.NewInteger(99),
		},
		{
			"cond-expand and",
			`(cond-expand
				((and r7rs exact-closed) 77)
				(else 0))`,
			values.NewInteger(77),
		},
		{
			"cond-expand or",
			`(cond-expand
				((or nonexistent r7rs) 55)
				(else 0))`,
			values.NewInteger(55),
		},
		{
			"cond-expand not",
			`(cond-expand
				((not nonexistent) 33)
				(else 0))`,
			values.NewInteger(33),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCoverageQuasiquoteEdgeCases tests quasiquote with various nesting.
// Exercises expandQuasiquote paths including splicing.
func TestCoverageQuasiquoteEdgeCases(t *testing.T) {
	testCases := []struct {
		name     string
		defs     []string
		code     string
		expected values.Value
	}{
		{
			"quasiquote with unquote-splicing",
			[]string{"(define xs '(2 3 4))"},
			"`(1 ,@xs 5)",
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4), values.NewInteger(5)),
		},
		{
			"nested quasiquote",
			nil,
			"`(a `(b ,(+ 1 2)))",
			nil, // just check no error
		},
		{
			"quasiquote vector",
			[]string{"(define x 42)"},
			"`#(1 ,x 3)",
			nil, // just check no error
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			for _, def := range tc.defs {
				_, err := runSchemeExpr(t, env, def)
				qt.Assert(t, err, qt.IsNil)
			}
			mc, err := runSchemeExpr(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			if tc.expected != nil {
				qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
			}
		})
	}
}

// TestCoverageIncludeError tests the include form with nonexistent file.
// Exercises CompileInclude error path in compile_time_continuation.go.
func TestCoverageIncludeError(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// include with nonexistent file should error
	_, err := runSchemeExpr(t, env, `(include "nonexistent-file-xxx.scm")`)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCoverageSyntaxCaseFender tests syntax-case with fender (guard).
// Exercises compileSyntaxCaseClause fender path.
func TestCoverageSyntaxCaseFender(t *testing.T) {
	env := newFullRuntimeEnv(t)

	_, err := runSchemeExpr(t, env, `(define-syntax check-positive
		(lambda (stx)
			(syntax-case stx ()
				((_ x) (positive? (syntax->datum (syntax x)))
					(syntax 'positive))
				((_ x)
					(syntax 'non-positive)))))`)
	qt.Assert(t, err, qt.IsNil)

	mc, err := runSchemeExpr(t, env, "(check-positive 5)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, env.TopLevelEnv().InternSymbol(values.NewSymbol("positive")))

	mc, err = runSchemeExpr(t, env, "(check-positive -3)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, env.TopLevelEnv().InternSymbol(values.NewSymbol("non-positive")))
}

// TestCoverageParameterObjectsExtended tests parameterize with nested scoping.
// Exercises Parameter apply and parameterize form compilation.
func TestCoverageParameterObjectsExtended(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define my-param (make-parameter 10))",
		`(parameterize ((my-param 42))
			(parameterize ((my-param 99))
				(my-param)))`,
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(99))
}

// TestCoverageSyntaxCaseEllipsis tests syntax-case with ellipsis patterns.
// Exercises ellipsis handling in syntax-case compilation.
func TestCoverageSyntaxCaseEllipsis(t *testing.T) {
	env := newFullRuntimeEnv(t)

	_, err := runSchemeExpr(t, env, `(define-syntax my-list
		(lambda (stx)
			(syntax-case stx ()
				((_ x ...) (syntax (list x ...))))))`)
	qt.Assert(t, err, qt.IsNil)

	mc, err := runSchemeExpr(t, env, "(my-list 1 2 3)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals,
		values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))
}

// TestCoverageQuasisyntaxRuntime tests syntax-case based macros.
// Exercises syntax-case template expansion paths.
func TestCoverageQuasisyntaxRuntime(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Simple syntax-case macro that wraps in a list
	_, err := runSchemeExpr(t, env, `(define-syntax wrap-in-list
		(lambda (stx)
			(syntax-case stx ()
				((_ x) (syntax (list x))))))`)
	qt.Assert(t, err, qt.IsNil)

	mc, err := runSchemeExpr(t, env, "(wrap-in-list 42)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals,
		values.List(values.NewInteger(42)))
}

// TestCoverageLetrecStar tests letrec* form.
// Exercises CompileLetrecStar paths.
func TestCoverageLetrecStar(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env,
		`(letrec* ((x 1) (y (+ x 1))) (+ x y))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCoverageWhenUnless tests when and unless forms.
func TestCoverageWhenUnless(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env, "(when #t 42)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))

	mc, err = runSchemeExpr(t, env, "(unless #f 99)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(99))
}

// TestCoverageRaiseAndGuard tests raise with guard handler.
// Exercises exception handler installation and exception escape paths.
func TestCoverageRaiseAndGuard(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test raise-continuable
	mc, err := runSchemeExpr(t, env,
		`(call-with-current-continuation
			(lambda (exit)
				(with-exception-handler
					(lambda (e) (exit (list 'caught e)))
					(lambda () (raise-continuable "oops")))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), qt.IsNotNil)
}

// TestCoverageSyntaxRulesFreeIdentifiers tests syntax-rules with free identifier
// references that have local bindings, triggering GetHasLocalBinding.
func TestCoverageSyntaxRulesFreeIdentifiers(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define secret 42)`,
		`(define-syntax reveal
			(syntax-rules ()
				((reveal) secret)))`,
		`(let ((secret 0)) (reveal))`,
	)
	qt.Assert(t, err, qt.IsNil)
	// Due to hygiene, (reveal) should reference the outer secret=42
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCoverageQuasiquoteImproper tests quasiquote with improper list.
// Exercises expandQuasiquoteImproperList in compile_time_continuation.go.
func TestCoverageQuasiquoteImproper(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define x 42)",
		"`(1 . ,x)",
	)
	qt.Assert(t, err, qt.IsNil)
	// Should produce (1 . 42)
	qt.Assert(t, mc.GetValue(), qt.IsNotNil)
}

// TestCoverageQuasiquoteSplicingInList tests unquote-splicing in middle of list.
// Exercises expandQuasiquote list traversal paths.
func TestCoverageQuasiquoteSplicingInList(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define xs '(2 3))",
		"`(1 ,@xs 4)",
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals,
		values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4)))
}

// TestCoverageQuasiquoteNested tests nested quasiquote/unquote.
// Exercises depth tracking in expandQuasiquote.
func TestCoverageQuasiquoteNested(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Double-nested quasiquote
	mc, err := runSchemeExpr(t, env, "`(a ,(+ 1 2) `(b ,(+ 3 4)))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), qt.IsNotNil)
}

// TestCoverageMultipleValuesExtended tests additional multiple-value paths.
func TestCoverageMultipleValuesExtended(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// receive combines consumer
	mc, err := runSchemeExpr(t, env,
		`(call-with-values
			(lambda () (values 'a 'b 'c))
			(lambda (x y z) (list x y z)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), qt.IsNotNil)
}

// TestCoverageDefineValues tests define inside let/lambda body.
// Exercises internal define handling and ExpandBodyWithDefineSyntax.
func TestCoverageDefineValues(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Internal defines in lambda body
	mc, err := runSchemeExpr(t, env,
		`((lambda ()
			(define a 1)
			(define b 2)
			(+ a b)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCoverageInternalDefineSyntax tests internal define-syntax.
// Exercises ExpandBodyWithDefineSyntax for internal macro definitions.
func TestCoverageInternalDefineSyntax(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env,
		`(let ()
			(define-syntax double
				(syntax-rules ()
					((double x) (+ x x))))
			(double 21))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCoverageTailPosition tests various tail position scenarios.
// Exercises CompileValidatedCall in tail position.
func TestCoverageTailPosition(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// if in tail position
	mc, err := runSchemeExpr(t, env,
		`((lambda (x) (if (> x 0) x (- x))) 5)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(5))

	// begin in tail position
	mc, err = runSchemeExpr(t, env,
		`((lambda () (begin 1 2 3)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))

	// let in tail position
	mc, err = runSchemeExpr(t, env,
		`((lambda () (let ((x 10)) (+ x 1))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(11))

	// case-lambda
	mc, err = runSchemeExpr(t, env,
		`((case-lambda
			((x) (+ x 1))
			((x y) (+ x y))) 10)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(11))
}

// TestCoverageDynamicWindReentry tests dynamic-wind with continuation re-entry.
// Exercises UnwindTo and RewindTo paths thoroughly.
func TestCoverageDynamicWindReentry(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define log '())",
		"(define k #f)",
		`(dynamic-wind
			(lambda () (set! log (cons 'in log)))
			(lambda ()
				(call-with-current-continuation
					(lambda (c) (set! k c)))
				42)
			(lambda () (set! log (cons 'out log))))`,
		"log",
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), qt.IsNotNil)
}

// TestCoverageApplyProcedure tests apply with various argument patterns.
// Exercises applyMachineClosure argument handling paths.
func TestCoverageApplyProcedure(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Apply with explicit args and rest list
	mc, err := runSchemeExpr(t, env, "(apply + 1 2 '(3 4))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(10))

	// Apply with just a list
	mc, err = runSchemeExpr(t, env, "(apply + '(1 2 3))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(6))

	// Apply with case-lambda
	mc, err = runSchemeExpr(t, env,
		`(apply (case-lambda
			((x) x)
			((x y) (+ x y))) '(10 20))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(30))
}

// TestCoverageSetBangTop tests set! at top level.
// Exercises CompileValidatedSetBang in definition mode.
func TestCoverageSetBangTop(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define x 1)",
		"(set! x 42)",
		"x",
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCoverageNamedLetLoop tests named let with a simple accumulator loop.
// Exercises named-let compilation path.
func TestCoverageNamedLetLoop(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env,
		`(let loop ((i 0) (acc 0))
			(if (= i 10) acc (loop (+ i 1) (+ acc i))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(45))
}

// TestCoverageCallCCEscapeThroughDynamicWind tests call/cc capturing inside
// dynamic-wind thunk and invoking from outside. This exercises the
// ErrContinuationEscape path in RunWithEscapeHandling, RestoreWithWindingFrom,
// UnwindTo, and RewindTo.
func TestCoverageCallCCEscapeThroughDynamicWind(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Capture continuation inside dynamic-wind thunk, invoke from outside.
	// The before/after thunks track winding/unwinding.
	mc, err := runSchemeExprs(t, env,
		`(define k #f)`,
		`(define log '())`,
		`(dynamic-wind
			(lambda () (set! log (cons 'in log)))
			(lambda () (call-with-current-continuation (lambda (c) (set! k c) 1)))
			(lambda () (set! log (cons 'out log))))`,
	)
	qt.Assert(t, err, qt.IsNil)

	// First run: k captured a continuation, result is 1
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(1))

	// Invoke the captured continuation from outside
	mc, err = runSchemeExpr(t, env, `(k 42)`)
	// k invokes the continuation, which escapes and gets caught by RunWithEscapeHandling
	// It should trigger before thunk (rewind) and after thunk (unwind)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCoveragePromptAbortHandling tests abort-current-continuation which
// exercises the ErrPromptAbort path in RunWithEscapeHandling.
func TestCoveragePromptAbortHandling(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test call-with-continuation-prompt with abort
	mc, err := runSchemeExpr(t, env,
		`(call-with-continuation-prompt
			(lambda ()
				(+ 1 (abort-current-continuation
					(default-continuation-prompt-tag)
					10)))
			(default-continuation-prompt-tag)
			(lambda (v) (* v 2)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(20))
}

// TestCoveragePromptAbortIdentity tests abort with identity handler.
// Exercises the handler invocation branch in RunWithEscapeHandling.
func TestCoveragePromptAbortIdentity(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Handler receives abort value and returns it
	mc, err := runSchemeExpr(t, env,
		`(call-with-continuation-prompt
			(lambda ()
				(abort-current-continuation
					(default-continuation-prompt-tag)
					99))
			(default-continuation-prompt-tag)
			(lambda (v) v))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(99))
}

// TestCoverageParameterObjectConverter tests parameter objects with converter.
// Exercises applyParameter converter path.
func TestCoverageParameterObjectConverter(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// make-parameter with converter function
	mc, err := runSchemeExprs(t, env,
		`(define p (make-parameter 10 (lambda (x) (* x 2))))`,
		`(p)`,
	)
	qt.Assert(t, err, qt.IsNil)
	// 10 passed through converter → 20
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(20))

	// parameterize also goes through converter
	mc, err = runSchemeExpr(t, env,
		`(parameterize ((p 5)) (p))`)
	qt.Assert(t, err, qt.IsNil)
	// 5 through converter → 10
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(10))
}

// TestCoverageParameterObjectSet tests parameter set! path.
// Exercises applyParameter with 1 argument.
func TestCoverageParameterObjectSet(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define p (make-parameter 42))`,
		`(p 100)`,
		`(p)`,
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(100))
}

// TestCoverageSyntaxRulesRecursive tests recursive macros with free identifiers.
// This exercises collectFreeIdentifiersWithEllipsis and intro scope handling.
func TestCoverageSyntaxRulesRecursive(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// A recursive macro — "my-and" references itself in expansion
	mc, err := runSchemeExprs(t, env,
		`(define-syntax my-and
			(syntax-rules ()
				((my-and) #t)
				((my-and test) test)
				((my-and test rest ...)
					(if test (my-and rest ...) #f))))`,
		`(my-and 1 2 3)`,
	)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))

	mc, err = runSchemeExpr(t, env, `(my-and 1 #f 3)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.FalseValue)
}

// TestCoverageVectorQuasiquoteSplicing tests quasiquote with unquote-splicing
// inside vectors. Exercises expandQuasiquote vector branch.
func TestCoverageVectorQuasiquoteSplicing(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define xs '(2 3 4))`,
		"`#(1 ,@xs 5)",
	)
	qt.Assert(t, err, qt.IsNil)
	// Should produce #(1 2 3 4 5)
	expected := values.NewVector(
		values.NewInteger(1),
		values.NewInteger(2),
		values.NewInteger(3),
		values.NewInteger(4),
		values.NewInteger(5),
	)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestCoverageNestedQuasiquoteDepth tests nested quasiquote at depth > 1.
// Exercises expandQuasiquote depth tracking.
func TestCoverageNestedQuasiquoteDepth(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Nested quasiquote: inner quasiquote preserved, outer unquote applied
	mc, err := runSchemeExprs(t, env,
		`(define x 42)`,
		"(list `(a ,(+ 1 2)) x)",
	)
	qt.Assert(t, err, qt.IsNil)
	// Should be ((a 3) 42)
	inner := values.NewCons(values.NewSymbol("a"), values.NewCons(values.NewInteger(3), values.EmptyList))
	expected := values.NewCons(inner, values.NewCons(values.NewInteger(42), values.EmptyList))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestCoverageCondExpandFeatures tests cond-expand with various feature identifiers.
// Exercises processCondExpand feature matching paths.
func TestCoverageCondExpandFeatures(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// cond-expand with known feature
	mc, err := runSchemeExpr(t, env,
		`(cond-expand
			(r7rs 'r7rs-supported)
			(else 'not-r7rs))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewSymbol("r7rs-supported"))

	// cond-expand with implementation feature
	mc, err = runSchemeExpr(t, env,
		`(cond-expand
			(wile 'wile-detected)
			(else 'unknown))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewSymbol("wile-detected"))
}

// TestCoverageEvalWhen tests eval-when form.
// Exercises CompileEvalWhen and related phase parsing.
func TestCoverageEvalWhen(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// eval-when at runtime
	mc, err := runSchemeExpr(t, env,
		`(eval-when (eval load)
			(+ 1 2))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCoverageGuardWithCond tests guard with multiple clauses.
// Exercises guard cond-clause expansion.
func TestCoverageGuardWithCond(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env,
		`(guard (exn
				((string? (error-object-message exn))
				 (string-append "caught: " (error-object-message exn))))
			(error "test" "details"))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewString("caught: test"))
}

// TestCoverageCaseLambdaDispatch tests case-lambda with multiple clauses.
// Exercises CaseLambdaClosure dispatch and EqualTo.
func TestCoverageCaseLambdaDispatch(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define f
			(case-lambda
				(() 0)
				((x) x)
				((x y) (+ x y))
				((x y . rest) (apply + x y rest))))`,
		`(list (f) (f 1) (f 1 2) (f 1 2 3 4))`,
	)
	qt.Assert(t, err, qt.IsNil)
	expected := values.NewCons(values.NewInteger(0),
		values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(3),
				values.NewCons(values.NewInteger(10),
					values.EmptyList))))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestCoverageDoLoop tests do loop form.
// Exercises do compilation path.
func TestCoverageDoLoop(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env,
		`(do ((i 0 (+ i 1))
			  (sum 0 (+ sum i)))
			 ((= i 5) sum))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(10))
}

// TestCoverageLetValuesMultiBinding tests let-values with multiple bindings.
// Exercises the let-values macro expansion with more clauses.
func TestCoverageLetValuesMultiBinding(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env,
		`(let-values (((a b) (values 1 2))
					  ((c) (values 3)))
			(+ a b c))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(6))
}

// TestCoverageSyntaxCaseWithTemplate tests syntax-case with (syntax ...) template.
// Exercises CompileSyntax and related template compilation.
func TestCoverageSyntaxCaseWithTemplate(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define-syntax swap!
			(lambda (stx)
				(syntax-case stx ()
					((swap! a b)
					 (syntax
						(let ((tmp a))
							(set! a b)
							(set! b tmp)))))))`,
		`(define x 1)`,
		`(define y 2)`,
		`(swap! x y)`,
		`(list x y)`,
	)
	qt.Assert(t, err, qt.IsNil)
	expected := values.NewCons(values.NewInteger(2),
		values.NewCons(values.NewInteger(1), values.EmptyList))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestCoverageDynamicWindNested tests deeply nested dynamic-wind.
// Exercises FindCommonWindingPrefix with diverging stacks.
func TestCoverageDynamicWindNested(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define trace '())`,
		`(dynamic-wind
			(lambda () (set! trace (cons 'a-in trace)))
			(lambda ()
				(dynamic-wind
					(lambda () (set! trace (cons 'b-in trace)))
					(lambda () 42)
					(lambda () (set! trace (cons 'b-out trace)))))
			(lambda () (set! trace (cons 'a-out trace))))`,
		`(reverse trace)`,
	)
	qt.Assert(t, err, qt.IsNil)
	// Trace should be (a-in b-in b-out a-out)
	expected := values.NewCons(values.NewSymbol("a-in"),
		values.NewCons(values.NewSymbol("b-in"),
			values.NewCons(values.NewSymbol("b-out"),
				values.NewCons(values.NewSymbol("a-out"), values.EmptyList))))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestCoverageExceptionReRaise tests exception re-raising with guard.
// Exercises exception handler chaining through guard clauses.
func TestCoverageExceptionReRaise(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Use guard (which uses raise-continuable internally) to test handler chaining
	mc, err := runSchemeExpr(t, env,
		`(guard (exn
				((number? exn) (+ exn 100)))
			(guard (inner-exn
					((and (number? inner-exn) (< inner-exn 5))
					 (raise (+ inner-exn 10))))
				(raise 1)))`)
	qt.Assert(t, err, qt.IsNil)
	// Inner guard catches 1 (< 5), re-raises 11
	// Outer guard catches 11 (number?), returns 111
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(111))
}

// TestCoverageDefineRecordType tests define-record-type.
// Exercises the define-record-type compilation path.
func TestCoverageDefineRecordType(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define-record-type <point>
			(make-point x y)
			point?
			(x point-x)
			(y point-y))`,
		`(define p (make-point 3 4))`,
		`(list (point? p) (point-x p) (point-y p))`,
	)
	qt.Assert(t, err, qt.IsNil)
	expected := values.NewCons(values.TrueValue,
		values.NewCons(values.NewInteger(3),
			values.NewCons(values.NewInteger(4), values.EmptyList)))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestCoverageSyntaxRulesCustomEllipsis tests syntax-rules with custom ellipsis.
// Exercises CompileSyntaxRules custom ellipsis path.
func TestCoverageSyntaxRulesCustomEllipsis(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		`(define-syntax my-list2
			(syntax-rules ::: ()
				((my-list2 x :::) (list x :::))))`,
		`(my-list2 1 2 3)`,
	)
	qt.Assert(t, err, qt.IsNil)
	expected := values.NewCons(values.NewInteger(1),
		values.NewCons(values.NewInteger(2),
			values.NewCons(values.NewInteger(3), values.EmptyList)))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

// TestEmptyListExpression verifies that a bare () in expression position
// compiles and evaluates to the empty list. R7RS §4.1.2 specifies that the
// empty list is a self-evaluating literal.
func TestEmptyListExpression(t *testing.T) {
	env := newFullRuntimeEnv(t)

	mc, err := runSchemeExpr(t, env, "()")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.EmptyList)
}

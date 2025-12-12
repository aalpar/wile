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
// Uses package machine_test to avoid import cycle with wile/runtime.

package machine_test

import (
	"bufio"
	"context"
	"strings"
	"testing"

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/runtime"
	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

// parseSchemeExprExt parses a Scheme expression
func parseSchemeExprExt(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, reader)
	sv, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)
	return sv
}

// newFullRuntimeEnv creates a full runtime environment with all primitives
func newFullRuntimeEnv(t *testing.T) *environment.EnvironmentFrame {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
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
	ccnt := machine.NewCompileTimeCallContext(false, true, env)
	if err := ctc.CompileExpression(ccnt, expanded); err != nil {
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call with 2 args
	sv = parseSchemeExprExt(t, env, "(cl 10 20)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(30))

	// Call with 3 args
	sv = parseSchemeExprExt(t, env, "(cl 1 2 3)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Use the macro with an arithmetic expression
	sv = parseSchemeExprExt(t, env, "(my-id (+ 10 20))")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(30))
}

// TestMapWithLambda tests map with lambda (exercises closure + multiple calls)
func TestMapWithLambda(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(map (lambda (x) (* x 2)) '(1 2 3))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	// Result should be (2 4 6)
}

// TestApplyWithPrimitives tests apply with various primitives
func TestApplyWithPrimitives(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(apply + '(1 2 3 4 5))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
			mc := machine.NewMachineContext(cont)
			err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestQuasiquoteComplexList tests compileQuasiquoteComplexList path
func TestQuasiquoteComplexList(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// This should exercise the complex list path with append
	sv := parseSchemeExprExt(t, env, "`(a ,@'(1 2) b ,@'(3 4) c)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Test the macro with variables named tmp (hygiene test)
	sv = parseSchemeExprExt(t, env, "(define x 1)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "(define y 2)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "(swap! x y)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Check x is now 2
	sv = parseSchemeExprExt(t, env, "x")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call with 5
	sv = parseSchemeExprExt(t, env, "(fact 5)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(15))
}

// TestCallWithValues tests call-with-values
func TestCallWithValues(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(call-with-values (lambda () (values 1 2 3)) +)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(6))
}

// TestVectorOperations tests vector primitives
func TestVectorOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(let ((v (vector 1 2 3))) (vector-set! v 1 42) (vector-ref v 1))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestStringOperations tests string primitives
func TestStringOperations(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, `(string-append "hello" " " "world")`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Create an adder that adds 10
	sv = parseSchemeExprExt(t, env, `(define add10 (make-adder 10))`)
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Use it
	sv = parseSchemeExprExt(t, env, `(add10 5)`)
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(15))
}

// TestNestedConditions tests nested if/cond expressions
func TestNestedConditions(t *testing.T) {
	env := newFullRuntimeEnv(t)

	sv := parseSchemeExprExt(t, env, "(if (> 5 3) (if (< 2 4) 'yes 'no) 'other)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
			mc := machine.NewMachineContext(cont)
			err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// unless with false condition
	sv = parseSchemeExprExt(t, env, "(unless #f 'yes)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestReduceWithMap tests map used for reduction
func TestReduceWithMap(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Use map to double all numbers
	sv := parseSchemeExprExt(t, env, "(map (lambda (x) (* x 2)) '(1 2 3 4 5))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call it with various args
	sv = parseSchemeExprExt(t, env, "(sum-all 1 2 3 4 5)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
			mc := machine.NewMachineContext(cont)
			err = mc.Run(context.Background())
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
			mc := machine.NewMachineContext(cont)
			err = mc.Run(context.Background())
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
			mc := machine.NewMachineContext(cont)
			err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)

	// Call Apply directly - this tests the handled escape branch.
	// We can't use mc.Run() here because a real handled escape involves
	// mc.Restore() which changes the entire machine state (template, pc, env).
	// Without that, the same instruction would execute forever.
	_, err := op.Apply(context.Background(), mc)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestContinuationEscapeUnhandled tests OperationForeignFunctionCall with unhandled continuation escape
func TestContinuationEscapeUnhandled(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Create a ForeignFunction that returns an unhandled continuation escape
	fn := func(ctx context.Context, mc *machine.MachineContext) error {
		escape := &machine.ErrContinuationEscape{
			Handled: false,
		}
		return escape
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	op := machine.NewOperationForeignFunctionCall(fn)
	tpl.AppendOperations(op)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(cont)
	err := mc.Run(context.Background())
	// Should return the error since it's unhandled
	qt.Assert(t, err, qt.IsNotNil)
	_, ok := err.(*machine.ErrContinuationEscape)
	qt.Assert(t, ok, qt.IsTrue)
}

// TestForeignFunctionNilError tests OperationForeignFunctionCall with nil function
func TestForeignFunctionNilError(t *testing.T) {
	env := newFullRuntimeEnv(t)

	tpl := machine.NewNativeTemplate(0, 0, false)
	op := machine.NewOperationForeignFunctionCall(nil)
	tpl.AppendOperations(op)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(cont)
	err := mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Test nested quasiquote with double unquote that requires runtime evaluation
	// ``(a ,,x b) should produce `(a ,5 b) which produces (a 5 b)
	sv = parseSchemeExprExt(t, env, "``(a ,,x b)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestQuasiquoteUnquoteSplicingAtDepth tests unquote-splicing at various depths
func TestQuasiquoteUnquoteSplicingAtDepth(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Test unquote-splicing at depth > 1
	sv := parseSchemeExprExt(t, env, "``(a ,@'(1 2 3) b)")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
			mc := machine.NewMachineContext(cont)
			err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExprExt(t, env, "global-var")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(100))

	// Test local symbol resolution
	sv = parseSchemeExprExt(t, env, "(let ((local-var 50)) local-var)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCondExpandElse tests cond-expand with else clause
func TestCondExpandElse(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// cond-expand with else clause
	sv := parseSchemeExprExt(t, env, "(cond-expand (nonexistent-feature 'no) (else 'fallback))")
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
	mc := machine.NewMachineContext(cont)
	err = mc.Run(context.Background())
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
			mc := machine.NewMachineContext(cont)
			err = mc.Run(context.Background())
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), qt.Equals, values.TrueValue,
				qt.Commentf("expected #t for: %s", tc.code))
		})
	}
}

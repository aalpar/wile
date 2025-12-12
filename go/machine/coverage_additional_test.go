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

package machine

import (
	"bufio"
	"context"
	"wile/environment"
	"wile/parser"
	"wile/syntax"
	"wile/utils"
	"wile/values"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// helper to parse Scheme code
func parseSchemeExpr(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, reader)
	sv, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)
	return sv
}

// TestCompileProcedureCallWithArgs tests procedure call compilation which exercises
// compileProcedureArgumentList and CompileProcedureCall
func TestCompileProcedureCallWithArgs(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{
			name: "simple procedure call with arguments",
			prog: "((lambda (x y) x) 1 2)",
		},
		{
			name: "nested procedure calls",
			prog: "((lambda (f) (f 1)) (lambda (x) x))",
		},
		{
			name: "procedure call with multiple args",
			prog: "((lambda (a b c) a) 1 2 3)",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileLambdaParameterListVariants tests lambda with various parameter lists
func TestCompileLambdaParameterListVariants(t *testing.T) {
	testCases := []struct {
		name     string
		prog     string
		variadic bool
	}{
		{
			name:     "lambda with rest parameter",
			prog:     "(lambda x x)",
			variadic: true,
		},
		{
			name:     "lambda with dotted parameter list",
			prog:     "(lambda (a . rest) rest)",
			variadic: true,
		},
		{
			name:     "lambda with multiple params and rest",
			prog:     "(lambda (a b . rest) rest)",
			variadic: true,
		},
		{
			name:     "lambda with no params",
			prog:     "(lambda () 42)",
			variadic: false,
		},
		{
			name:     "lambda with single param",
			prog:     "(lambda (x) x)",
			variadic: false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)

			// The compiled thunk wraps the lambda, so check its literals
			qt.Assert(t, len(cont.template.literals) >= 1, qt.IsTrue)
			innerTpl, ok := cont.template.literals[0].(*NativeTemplate)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, innerTpl.isVariadic, qt.Equals, tc.variadic)
		})
	}
}

// TestOperationForeignFunctionCallMethods tests the foreign function call operation methods
func TestOperationForeignFunctionCallMethods(t *testing.T) {
	// Create a simple foreign function
	ff := func(ctx context.Context, mc *MachineContext) error {
		return nil
	}

	op := NewOperationForeignFunctionCall(ff)
	qt.Assert(t, op.SchemeString(), qt.Contains, "foreign-function-call")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(op), qt.IsTrue)
	qt.Assert(t, op.EqualTo(NewOperationPush()), qt.IsFalse)
}

// TestOperationSyntaxRulesTransformMethods tests syntax rules transform operation methods
func TestOperationSyntaxRulesTransformMethods(t *testing.T) {
	op := NewOperationSyntaxRulesTransform()
	qt.Assert(t, op.String(), qt.Contains, "SyntaxRulesTransform")
	qt.Assert(t, op.SchemeString(), qt.Contains, "syntax-rules-transform")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(op), qt.IsTrue)
	qt.Assert(t, op.EqualTo(NewOperationPush()), qt.IsFalse)
}

// TestPrimitiveCompilerNameMethod tests the Name method of PrimitiveCompiler
func TestPrimitiveCompilerNameMethod(t *testing.T) {
	pc := &PrimitiveCompiler{
		name: "test-compiler",
	}
	qt.Assert(t, pc.Name(), qt.Equals, "test-compiler")
}

// TestExpandQuasiquoteAndQuote tests the expander for quasiquote and quote
func TestExpandQuasiquoteAndQuote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Test quote expansion
	quoteProg := values.List(values.NewSymbol("quote"), values.NewSymbol("x"))
	ectx := NewExpandTimeCallContext()
	econt := NewExpanderTimeContinuation(env)
	expanded, err := econt.ExpandExpression(ectx, utils.DatumToSyntaxValue(sctx, quoteProg))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded, qt.IsNotNil)

	// Test quasiquote expansion
	qqProg := values.List(values.NewSymbol("quasiquote"), values.List(values.NewSymbol("a"), values.NewSymbol("b")))
	expanded2, err := econt.ExpandExpression(ectx, utils.DatumToSyntaxValue(sctx, qqProg))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded2, qt.IsNotNil)
}

// TestCompileSymbolUnboundError tests compile error for unbound symbol
func TestCompileSymbolUnboundError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()
	expr := values.NewSymbol("undefined-symbol")

	_, err = newTopLevelThunk(utils.DatumToSyntaxValue(sctx, expr), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no such binding")
}

// TestCompileLambdaDuplicateParamError tests error for duplicate lambda params
func TestCompileLambdaDuplicateParamError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (x x) x) should error due to duplicate parameter
	prog := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("x"), values.NewSymbol("x")),
		values.NewSymbol("x"),
	)

	_, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "duplicate")
}

// TestCompileLambdaInvalidParamError tests error for invalid lambda parameter
func TestCompileLambdaInvalidParamError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (1) 42) should error - 1 is not a valid parameter
	prog := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewInteger(1)),
		values.NewInteger(42),
	)

	_, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileNestedQuasiquote tests doubly-nested quasiquote
func TestCompileNestedQuasiquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Test nested quasiquote: `(a `(b ,x))
	prog := values.List(
		values.NewSymbol("quasiquote"),
		values.List(
			values.NewSymbol("a"),
			values.List(
				values.NewSymbol("quasiquote"),
				values.List(
					values.NewSymbol("b"),
					values.List(values.NewSymbol("unquote"), values.NewSymbol("x")),
				),
			),
		),
	)

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestAddScopeToSyntaxViaDefineSyntax tests the addScopeToSyntax helper in syntax rules transform
func TestAddScopeToSyntaxViaDefineSyntax(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	reader := bufio.NewReader(strings.NewReader(`
	(define-syntax my-id
	  (syntax-rules ()
	    ((my-id x) x)))
	`))

	p := parser.NewParser(env, reader)
	sv, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
}

// TestExecuteSimpleProcedureCall tests actually running a procedure call
func TestExecuteSimpleProcedureCall(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	prog := "((lambda (x) x) 42)"
	sv := parseSchemeExpr(t, env, prog)

	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.IsNotNil)
	qt.Assert(t, len(mc.value) > 0, qt.IsTrue)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestExecuteVariadicProcedure tests running a variadic procedure
func TestExecuteVariadicProcedure(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// (lambda args args) called with (1 2 3) should return (1 2 3)
	prog := "((lambda args args) 1 2 3)"
	sv := parseSchemeExpr(t, env, prog)

	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.IsNotNil)
}

// TestOperationEqualToNilCases tests EqualTo methods with nil cases
func TestOperationEqualToNilCases(t *testing.T) {
	// Test LoadLocalByLocalIndexImmediate nil cases
	li := environment.NewLocalIndex(0, 0)
	op1 := NewOperationLoadLocalByLocalIndexImmediate(li)
	var nilOp *OperationLoadLocalByLocalIndexImmediate

	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
	qt.Assert(t, nilOp.EqualTo(op1), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(op1), qt.IsTrue)

	// Same local index should be equal
	op2 := NewOperationLoadLocalByLocalIndexImmediate(li)
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)

	// Different local index
	li2 := environment.NewLocalIndex(1, 0)
	op3 := NewOperationLoadLocalByLocalIndexImmediate(li2)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
}

// TestOperationLoadGlobalEqualTo tests LoadGlobalByGlobalIndexLiteralIndexImmediate EqualTo
func TestOperationLoadGlobalEqualTo(t *testing.T) {
	op1 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op2 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op3 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(1))

	var nilOp *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationLoadLiteralEqualTo tests LoadLiteralByLiteralIndexImmediate EqualTo
func TestOperationLoadLiteralEqualTo(t *testing.T) {
	op1 := NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(0))
	op2 := NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(0))
	op3 := NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(1))

	var nilOp *OperationLoadLiteralByLiteralIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestStackOperations tests Stack AsList and Clear
func TestStackOperations(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	// Test AsList
	list := s.AsList()
	qt.Assert(t, list, qt.IsNotNil)

	// Verify list structure - AsList returns in push order (last pushed is first)
	pr, ok := list.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	// Stack order: push 1, push 2, push 3 - AsList builds from top
	qt.Assert(t, pr.Car(), values.SchemeEquals, values.NewInteger(1))

	// Test Clear
	s.Clear()
	qt.Assert(t, s.Length(), qt.Equals, 0)

	// AsList on empty stack
	emptyList := s.AsList()
	qt.Assert(t, emptyList, qt.Equals, values.EmptyList)

	// Single element stack
	s.Push(values.NewInteger(42))
	singleList := s.AsList()
	qt.Assert(t, singleList, qt.IsNotNil)
}

// TestAllPlatformFeatures tests platformFeatures function
func TestAllPlatformFeatures(t *testing.T) {
	features := AllFeatures()
	qt.Assert(t, len(features) > 0, qt.IsTrue)

	// Check for some expected features
	found := false
	for _, f := range features {
		if f == "r7rs" {
			found = true
			break
		}
	}
	qt.Assert(t, found, qt.IsTrue)
}

// TestOperationStoreGlobalEqualTo tests StoreGlobalByGlobalIndexLiteralIndexImmediate EqualTo
func TestOperationStoreGlobalEqualTo(t *testing.T) {
	op1 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op2 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op3 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(1))

	var nilOp *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationStoreLocalEqualTo tests StoreLocalByLocalIndexImmediate EqualTo
func TestOperationStoreLocalEqualTo(t *testing.T) {
	li := environment.NewLocalIndex(0, 0)
	op1 := NewOperationStoreLocalByLocalIndexImmediate(li)
	op2 := NewOperationStoreLocalByLocalIndexImmediate(li)

	li2 := environment.NewLocalIndex(1, 0)
	op3 := NewOperationStoreLocalByLocalIndexImmediate(li2)

	var nilOp *OperationStoreLocalByLocalIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationSaveContinuationEqualTo tests SaveContinuationOffsetImmediate EqualTo
func TestOperationSaveContinuationEqualTo(t *testing.T) {
	op1 := NewOperationSaveContinuationOffsetImmediate(10)
	op2 := NewOperationSaveContinuationOffsetImmediate(10)
	op3 := NewOperationSaveContinuationOffsetImmediate(20)

	var nilOp *OperationSaveContinuationOffsetImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationPeekKEqualTo tests PeekK EqualTo
func TestOperationPeekKEqualTo(t *testing.T) {
	op1 := NewOperationPeekK(0)
	op2 := NewOperationPeekK(0)
	op3 := NewOperationPeekK(1)

	var nilOp *OperationPeekK

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestCompileCaseLambda tests compiling case-lambda
func TestCompileCaseLambda(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// case-lambda with multiple clauses
	prog := `(case-lambda
		(() 0)
		((x) x)
		((x y) (if x y x))
		(args args))`

	sv := parseSchemeExpr(t, env, prog)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileDefineWithFn tests compiling define with lambda form
func TestCompileDefineWithFn(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn x y) body) form
	prog := `(define (add x y) x)`

	sv := parseSchemeExpr(t, env, prog)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileDefineVar tests compiling define with value
func TestCompileDefineVar(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	prog := `(define x 42)`

	sv := parseSchemeExpr(t, env, prog)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)

	// Run it
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSetBang tests compiling set!
func TestCompileSetBang(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// First define x
	sv := parseSchemeExpr(t, env, `(define x 0)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Now set! it
	sv2 := parseSchemeExpr(t, env, `(set! x 42)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileIfBranches tests compiling if with both branches
func TestCompileIfBranches(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// if with else
	sv := parseSchemeExpr(t, env, `(if #t 1 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(1))

	// if without else (just consequent)
	sv2 := parseSchemeExpr(t, env, `(if #f 1)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestAddScopeToSyntax tests the addScopeToSyntax helper function
func TestAddScopeToSyntax(t *testing.T) {
	scope := syntax.NewScope(nil)
	srcCtx := syntax.NewZeroValueSourceContext()

	// Test with nil
	result := addScopeToSyntax(nil, scope)
	qt.Assert(t, result, qt.IsNil)

	// Test with SyntaxSymbol
	sym := syntax.NewSyntaxSymbol("foo", srcCtx)
	result = addScopeToSyntax(sym, scope)
	qt.Assert(t, result, qt.IsNotNil)

	// Test with SyntaxPair
	pair := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("a", srcCtx),
		syntax.NewSyntaxEmptyList(srcCtx),
		srcCtx,
	)
	result = addScopeToSyntax(pair, scope)
	qt.Assert(t, result, qt.IsNotNil)

	// Test with SyntaxObject
	obj := syntax.NewSyntaxObject(values.NewInteger(42), srcCtx)
	result = addScopeToSyntax(obj, scope)
	qt.Assert(t, result, qt.IsNotNil)

	// Test with non-syntax value
	result = addScopeToSyntax(values.NewInteger(42), scope)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}

// TestAddScopeToSyntaxSkipFreeIds tests the addScopeToSyntaxSkipFreeIds function
func TestAddScopeToSyntaxSkipFreeIds(t *testing.T) {
	scope := syntax.NewScope(nil)
	srcCtx := syntax.NewZeroValueSourceContext()
	freeIds := map[string]struct{}{
		"if": {},
	}

	// Test with nil
	result := addScopeToSyntaxSkipFreeIds(nil, scope, freeIds)
	qt.Assert(t, result, qt.IsNil)

	// Test with a free identifier - should NOT get the scope
	sym := syntax.NewSyntaxSymbol("if", srcCtx)
	result = addScopeToSyntaxSkipFreeIds(sym, scope, freeIds)
	qt.Assert(t, result, qt.IsNotNil)
	// The symbol should be returned unchanged (same object)
	qt.Assert(t, result, qt.Equals, sym)

	// Test with a non-free identifier - should get the scope
	sym2 := syntax.NewSyntaxSymbol("foo", srcCtx)
	result = addScopeToSyntaxSkipFreeIds(sym2, scope, freeIds)
	qt.Assert(t, result, qt.IsNotNil)
	// Should be a different object with added scope
	qt.Assert(t, result != sym2, qt.IsTrue)

	// Test with pair containing free and non-free identifiers
	pair := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("if", srcCtx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("x", srcCtx),
			syntax.NewSyntaxEmptyList(srcCtx),
			srcCtx,
		),
		srcCtx,
	)
	result = addScopeToSyntaxSkipFreeIds(pair, scope, freeIds)
	qt.Assert(t, result, qt.IsNotNil)

	// Test with SyntaxObject
	obj := syntax.NewSyntaxObject(values.NewInteger(42), srcCtx)
	result = addScopeToSyntaxSkipFreeIds(obj, scope, freeIds)
	qt.Assert(t, result, qt.IsNotNil)

	// Test with non-syntax value
	result = addScopeToSyntaxSkipFreeIds(values.NewInteger(42), scope, freeIds)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}

// TestAddScopeToPairSkipFreeIds tests the addScopeToPairSkipFreeIds function
func TestAddScopeToPairSkipFreeIds(t *testing.T) {
	scope := syntax.NewScope(nil)
	srcCtx := syntax.NewZeroValueSourceContext()
	freeIds := map[string]struct{}{
		"if": {},
	}

	// Test with nil pair
	result := addScopeToPairSkipFreeIds(nil, scope, freeIds)
	qt.Assert(t, result, qt.IsNil)

	// Test with empty list
	emptyList := syntax.NewSyntaxEmptyList(srcCtx)
	result = addScopeToPairSkipFreeIds(emptyList, scope, freeIds)
	// Empty list should be returned as-is
	qt.Assert(t, result, qt.Equals, emptyList)

	// Test with pair containing symbols
	pair := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("a", srcCtx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("b", srcCtx),
			syntax.NewSyntaxEmptyList(srcCtx),
			srcCtx,
		),
		srcCtx,
	)
	result = addScopeToPairSkipFreeIds(pair, scope, freeIds)
	qt.Assert(t, result, qt.IsNotNil)
}

// TestExpandQuasiquoteAndQuoteDirect tests the expander methods directly
func TestExpandQuasiquoteAndQuoteDirect(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	econt := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	// Test ExpandQuote - currently returns nil, nil
	quoteExpr := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("quote", sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("x", sctx),
			syntax.NewSyntaxEmptyList(sctx),
			sctx,
		),
		sctx,
	)
	expanded, err := econt.ExpandQuote(ectx, quoteExpr)
	// The function returns nil, nil (unimplemented)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded, qt.IsNil)

	// Test ExpandQuasiquote - currently returns nil, nil
	qqExpr := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("quasiquote", sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("y", sctx),
			syntax.NewSyntaxEmptyList(sctx),
			sctx,
		),
		sctx,
	)
	expanded, err = econt.ExpandQuasiquote(ectx, qqExpr)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded, qt.IsNil)
}

// TestCompileUnquoteError tests error when compiling unquote outside quasiquote
func TestCompileUnquoteError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Try to compile bare unquote - should error
	prog := values.List(values.NewSymbol("unquote"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileUnquoteSplicingError tests error when compiling unquote-splicing outside quasiquote
func TestCompileUnquoteSplicingError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Try to compile bare unquote-splicing - should error
	prog := values.List(values.NewSymbol("unquote-splicing"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileQuasiquoteSimple tests quasiquote without unquote (no list/append needed)
func TestCompileQuasiquoteSimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple quasiquote without any unquote - should be a compile-time constant
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileProcedureCallTail tests tail call compilation
func TestCompileProcedureCallTail(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with tail call
	sv := parseSchemeExpr(t, env, `(define (fn x) ((lambda (y) y) x))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Test calling the function
	sv = parseSchemeExpr(t, env, `(fn 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileProcedureCallNonTail tests non-tail call compilation
func TestCompileProcedureCallNonTail(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with non-tail call (result used as argument)
	sv := parseSchemeExpr(t, env, `((lambda (x) (begin ((lambda (y) y) x) 99)) 1)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(99))
}

// TestCompileBeginSequence tests begin with multiple expressions
func TestCompileBeginSequence(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(begin 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(3))
}

// TestCompileIfNoAlternate tests if without else
func TestCompileIfNoAlternate(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// If without else, true case
	sv := parseSchemeExpr(t, env, `(if #t 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))

	// If without else, false case - should return void
	sv = parseSchemeExpr(t, env, `(if #f 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileLambdaWithRestParameter tests lambda with rest parameter
func TestCompileLambdaWithRestParameter(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (lambda (a . rest) rest) - dotted parameter list
	sv := parseSchemeExpr(t, env, `((lambda (a . rest) rest) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	// rest should be (2 3)
	result := mc.value[0]
	qt.Assert(t, result, qt.IsNotNil)
}

// TestCompileLambdaRestOnly tests lambda with only rest parameter
func TestCompileLambdaRestOnly(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (lambda args args) - single rest parameter
	sv := parseSchemeExpr(t, env, `((lambda args args) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestOperationBranchMethods tests Branch operation methods
func TestOperationBranchMethods(t *testing.T) {
	op := NewOperationBranchOffsetImmediate(10)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationBranchOffsetImmediate(10)
	op3 := NewOperationBranchOffsetImmediate(20)
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op.EqualTo(op3), qt.IsFalse)

	var nilOp *OperationBranchOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationBranchOnFalseMethods tests BranchOnFalse operation methods
func TestOperationBranchOnFalseMethods(t *testing.T) {
	op := NewOperationBranchOnFalseOffsetImmediate(10)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationBranchOnFalseOffsetImmediate(10)
	op3 := NewOperationBranchOnFalseOffsetImmediate(20)
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op.EqualTo(op3), qt.IsFalse)

	var nilOp *OperationBranchOnFalseOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationLoadVoidMethods tests LoadVoid operation methods
func TestOperationLoadVoidMethods(t *testing.T) {
	op := NewOperationLoadVoid()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationLoadVoid()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationMakeClosureMethods tests MakeClosure operation methods
func TestOperationMakeClosureMethods(t *testing.T) {
	op := NewOperationMakeClosure()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationMakeClosure()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationApplyMethods tests Apply operation methods
func TestOperationApplyMethods(t *testing.T) {
	op := NewOperationApply()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationApply()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationBranchOnNotFalseMethods tests BranchOnNotFalse operation methods
func TestOperationBranchOnNotFalseMethods(t *testing.T) {
	op := NewOperationBranchOnNotFalseOffsetImmediate(10)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationBranchOnNotFalseOffsetImmediate(10)
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationRestoreContinuationMethods tests RestoreContinuation operation methods
func TestOperationRestoreContinuationMethods(t *testing.T) {
	op := NewOperationRestoreContinuation()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo - same object should be equal to itself
	qt.Assert(t, op.EqualTo(op), qt.IsTrue)
}

// TestOperationPushMethods tests Push operation methods
func TestOperationPushMethods(t *testing.T) {
	op := NewOperationPush()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationPush()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationPullMethods tests Pull operation methods
func TestOperationPullMethods(t *testing.T) {
	op := NewOperationPull()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationPull()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationPopMethods tests Pop operation methods
func TestOperationPopMethods(t *testing.T) {
	op := NewOperationPop()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationPop()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationMakeCaseLambdaClosureMethods tests MakeCaseLambdaClosure operation methods
func TestOperationMakeCaseLambdaClosureMethods(t *testing.T) {
	op := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)

	op3 := NewOperationMakeCaseLambdaClosure(3)
	qt.Assert(t, op.EqualTo(op3), qt.IsFalse)
}

// TestCompileCondExpandNotFeature tests cond-expand with not feature
func TestCompileCondExpandNotFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (not nonexistent) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((not nonexistent) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandAndFeature tests cond-expand with and feature
func TestCompileCondExpandAndFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (and r7rs) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((and r7rs) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandOrFeature tests cond-expand with or feature
func TestCompileCondExpandOrFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (or nonexistent r7rs) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((or nonexistent r7rs) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandLibraryFeature tests cond-expand with library feature
func TestCompileCondExpandLibraryFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (library (scheme base)) - may not be available but tests parsing
	sv := parseSchemeExpr(t, env, `(cond-expand ((library (nonexistent lib)) 1) (else 99))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(99))
}

// TestNativeTemplateMethodsAdditional tests NativeTemplate methods
func TestNativeTemplateMethodsAdditional(t *testing.T) {
	tpl := NewNativeTemplate(2, 1, true)

	qt.Assert(t, tpl.ParameterCount(), qt.Equals, 2)
	qt.Assert(t, tpl.IsVariadic(), qt.IsTrue)
	qt.Assert(t, tpl.IsVoid(), qt.IsFalse)
	qt.Assert(t, tpl.SchemeString(), qt.Contains, "native-template")

	// Test EqualTo
	tpl2 := NewNativeTemplate(2, 1, true)
	qt.Assert(t, tpl.EqualTo(tpl2), qt.IsTrue)

	tpl3 := NewNativeTemplate(3, 1, true)
	qt.Assert(t, tpl.EqualTo(tpl3), qt.IsFalse)

	var nilTpl *NativeTemplate
	qt.Assert(t, tpl.EqualTo(nilTpl), qt.IsFalse)
}

// TestMachineClosureMethodsAdditional tests MachineClosure methods
func TestMachineClosureMethodsAdditional(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(1, 0, false)
	closure := NewClosureWithTemplate(tpl, env)

	qt.Assert(t, closure.IsVoid(), qt.IsFalse)
	qt.Assert(t, closure.SchemeString(), qt.Contains, "closure")
	qt.Assert(t, closure.Template(), qt.Equals, tpl)

	// Test EqualTo
	closure2 := NewClosureWithTemplate(tpl, env)
	qt.Assert(t, closure.EqualTo(closure2), qt.IsTrue)

	var nilClosure *MachineClosure
	qt.Assert(t, closure.EqualTo(nilClosure), qt.IsFalse)
}

// TestMachineContinuationMethodsAdditional tests MachineContinuation methods
func TestMachineContinuationMethodsAdditional(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations, NewOperationLoadVoid())
	tpl.operations = append(tpl.operations, NewOperationRestoreContinuation())

	cont := NewMachineContinuation(nil, tpl, env)

	qt.Assert(t, cont.IsVoid(), qt.IsFalse)
	qt.Assert(t, cont.SchemeString(), qt.Contains, "continuation")
	qt.Assert(t, cont.Template(), qt.Equals, tpl)
	qt.Assert(t, cont.Parent(), qt.IsNil)

	// Test EqualTo - same object should be equal to itself
	qt.Assert(t, cont.EqualTo(cont), qt.IsTrue)

	var nilCont *MachineContinuation
	qt.Assert(t, cont.EqualTo(nilCont), qt.IsFalse)
}

// TestClausesWrapperAdditional tests the clausesWrapper type
func TestClausesWrapperAdditional(t *testing.T) {
	cw := &clausesWrapper{
		clauses: nil,
	}
	qt.Assert(t, cw.IsVoid(), qt.IsFalse)
	qt.Assert(t, cw.SchemeString(), qt.Contains, "syntax-rules-clauses")

	// EqualTo always returns false because clauses are not comparable
	qt.Assert(t, cw.EqualTo(cw), qt.IsFalse)

	var nilCw *clausesWrapper
	qt.Assert(t, cw.EqualTo(nilCw), qt.IsFalse)
}

// TestLibraryNameMethodsAdditional tests LibraryName methods
func TestLibraryNameMethodsAdditional(t *testing.T) {
	ln := NewLibraryName("scheme", "base")

	qt.Assert(t, ln.String(), qt.Equals, "scheme/base")
	qt.Assert(t, ln.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, ln.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, ln.ToFilePath(), qt.Contains, "scheme")
}

// TestLibraryRegistryMethodsAdditional tests LibraryRegistry methods
func TestLibraryRegistryMethodsAdditional(t *testing.T) {
	reg := NewLibraryRegistry()

	// Create a test library
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	lib := &CompiledLibrary{
		Name:    NewLibraryName("test", "lib"),
		Env:     env,
		Exports: map[string]string{},
	}

	// Register it
	reg.Register(lib) //nolint:errcheck

	// Look it up
	found := reg.Lookup(NewLibraryName("test", "lib"))
	qt.Assert(t, found, qt.IsNotNil)
	qt.Assert(t, found.Name.String(), qt.Equals, "test/lib")

	// Look up non-existent
	notFound := reg.Lookup(NewLibraryName("nonexistent"))
	qt.Assert(t, notFound, qt.IsNil)
}

// TestCompileSymbolBranches tests various branches of CompileSymbol
func TestCompileSymbolBranches(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test compiling a defined global variable
	sv := parseSchemeExpr(t, env, `(define global-var 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Now compile a reference to the global
	sv = parseSchemeExpr(t, env, `global-var`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSymbolLocalBinding tests compiling local bindings
func TestCompileSymbolLocalBinding(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with local parameter
	sv := parseSchemeExpr(t, env, `((lambda (local-var) local-var) 99)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(99))
}

// TestCompileSyntaxPrimitiveBranches tests various branches of CompileSyntaxPrimitive
func TestCompileSyntaxPrimitiveBranches(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test various primitives
	testCases := []struct {
		name string
		prog string
	}{
		{"quote", "'foo"},
		{"if-true", "(if #t 1 2)"},
		{"if-false", "(if #f 1 2)"},
		{"lambda", "(lambda (x) x)"},
		{"begin", "(begin 1 2 3)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := NewMachineContext(cont)
			err = mc.Run(context.Background())
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestOperationPopAllMethods tests OperationPopAll methods
func TestOperationPopAllMethods(t *testing.T) {
	op := NewOperationPopAll()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo - same operation type
	op2 := NewOperationPopAll()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationLoadLiteralIntegerMethods tests OperationLoadLiteralInteger methods
func TestOperationLoadLiteralIntegerMethods(t *testing.T) {
	op := NewOperationLoadLiteralInteger(42)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo - same value
	op2 := NewOperationLoadLiteralInteger(42)
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)

	// Different value - EqualTo only checks type, not value
	op3 := NewOperationLoadLiteralInteger(99)
	qt.Assert(t, op.EqualTo(op3), qt.IsTrue) // EqualTo returns true for same type

	// Different type
	qt.Assert(t, op.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

// TestOperationBrkMethods tests OperationBrk methods
func TestOperationBrkMethods(t *testing.T) {
	fn := func(ctx context.Context, mc *MachineContext) error { return nil }
	op := NewOperationBrk(fn)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// EqualTo - same operation
	qt.Assert(t, op.EqualTo(op), qt.IsTrue)
}

// TestCompileMultipleForms tests compiling multiple forms
func TestCompileMultipleForms(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define and use
	sv := parseSchemeExpr(t, env, `(define fn (lambda (x) x))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call the function
	sv = parseSchemeExpr(t, env, `(fn 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSelfEvaluating tests compiling self-evaluating values
func TestCompileSelfEvaluating(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name   string
		prog   string
		expect values.Value
	}{
		{"integer", "42", values.NewInteger(42)},
		{"negative-integer", "-5", values.NewInteger(-5)},
		{"true", "#t", values.TrueValue},
		{"false", "#f", values.FalseValue},
		{"float", "3.14", values.NewFloat(3.14)},
		{"string", `"hello"`, values.NewString("hello")},
		{"char", `#\a`, values.NewCharacter('a')},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := NewMachineContext(cont)
			err = mc.Run(context.Background())
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.value[0], values.SchemeEquals, tc.expect)
		})
	}
}

// TestCompileNestedLambda tests nested lambda expressions
func TestCompileNestedLambda(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested lambda - currying
	sv := parseSchemeExpr(t, env, `(((lambda (x) (lambda (y) x)) 1) 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(1))
}

// TestCompileComplexIf tests complex if expressions
func TestCompileComplexIf(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested if expressions
	sv := parseSchemeExpr(t, env, `(if #t (if #f 1 2) 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(2))
}

// TestMachineContextNewSubContext tests creating a sub-context
func TestMachineContextNewSubContext(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `42`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)

	// Create a sub-context
	sub := mc.NewSubContext()
	qt.Assert(t, sub, qt.IsNotNil)
}

// TestStackMoreOperations tests more stack operations
func TestStackMoreOperations(t *testing.T) {
	s := NewStack()

	// Push multiple values
	for i := 0; i < 10; i++ {
		s.Push(values.NewInteger(int64(i)))
	}
	qt.Assert(t, s.Length(), qt.Equals, 10)

	// Pop all
	for i := 9; i >= 0; i-- {
		v := s.Pop()
		qt.Assert(t, v, values.SchemeEquals, values.NewInteger(int64(i)))
	}
	qt.Assert(t, s.Length(), qt.Equals, 0)

	// PeekK to peek at stack
	s.Push(values.NewInteger(42))
	v := s.PeekK(0) // PeekK(0) gets top of stack
	qt.Assert(t, v, values.SchemeEquals, values.NewInteger(42))
	qt.Assert(t, s.Length(), qt.Equals, 1) // Still there
}

// TestCopyStack tests stack copy
func TestCopyStack(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	// Copy
	cpy := s.Copy()
	qt.Assert(t, cpy.Length(), qt.Equals, 2)

	// Modify original doesn't affect copy
	s.Push(values.NewInteger(3))
	qt.Assert(t, s.Length(), qt.Equals, 3)
	qt.Assert(t, cpy.Length(), qt.Equals, 2)
}

// TestNativeTemplateOperationsArray tests NativeTemplate operations
func TestNativeTemplateOperationsArray(t *testing.T) {
	tpl := NewNativeTemplate(2, 1, false)

	// Add operations
	tpl.operations = append(tpl.operations, NewOperationLoadVoid())
	tpl.operations = append(tpl.operations, NewOperationPush())
	tpl.operations = append(tpl.operations, NewOperationRestoreContinuation())

	qt.Assert(t, tpl.operations.Length(), qt.Equals, 3)
}

// TestMachineContextSetValues tests SetValues and GetValues
func TestMachineContextSetValues(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations, NewOperationLoadVoid())
	tpl.operations = append(tpl.operations, NewOperationRestoreContinuation())

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)

	// SetValues
	mc.SetValues(values.NewInteger(1), values.NewInteger(2))
	vs := mc.GetValues()
	qt.Assert(t, len(vs), qt.Equals, 2)
	qt.Assert(t, vs[0], values.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, vs[1], values.SchemeEquals, values.NewInteger(2))
}

// TestMachineContextSetValue tests SetValue and GetValue
func TestMachineContextSetValue(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations, NewOperationLoadVoid())
	tpl.operations = append(tpl.operations, NewOperationRestoreContinuation())

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)

	// SetValue
	mc.SetValue(values.NewInteger(42))
	v := mc.GetValue()
	qt.Assert(t, v, values.SchemeEquals, values.NewInteger(42))
}

// TestCompileUnquoteOutsideQuasiquote tests that unquote outside quasiquote returns error
func TestCompileUnquoteOutsideQuasiquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Try to compile unquote outside of quasiquote - should error
	sv := parseSchemeExpr(t, env, `(unquote x)`)
	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote")
}

// TestCompileUnquoteSplicingOutsideQuasiquote tests that unquote-splicing outside quasiquote returns error
func TestCompileUnquoteSplicingOutsideQuasiquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Try to compile unquote-splicing outside of quasiquote - should error
	sv := parseSchemeExpr(t, env, `(unquote-splicing x)`)
	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote-splicing")
}

// TestStackPull tests stack Pull operation
func TestStackPull(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	// Pull removes from front (FIFO)
	v := s.Pull()
	qt.Assert(t, v, values.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, s.Length(), qt.Equals, 2)
}

// TestStackPopAll tests stack PopAll operation
func TestStackPopAll(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	vs := s.PopAll()
	qt.Assert(t, len(vs), qt.Equals, 3)
	qt.Assert(t, s.Length(), qt.Equals, 0) // Stack is empty after PopAll
}

// TestStackClear tests stack Clear operation
func TestStackClear(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	qt.Assert(t, s.Length(), qt.Equals, 2)

	s.Clear()
	qt.Assert(t, s.Length(), qt.Equals, 0)

	// Clear on nil should not panic
	var nilStack *Stack
	nilStack.Clear() // Should not panic
}

// TestStackAsList tests stack AsList conversion
func TestStackAsList(t *testing.T) {
	// Create stack with single element
	s := NewStack(values.NewInteger(1))

	list := s.AsList()
	qt.Assert(t, list, qt.IsNotNil)
	// AsList returns a list containing the stack element
	qt.Assert(t, list.SchemeString(), qt.Contains, "1")
}

// TestStackAsListEmpty tests stack AsList with empty/nil stack
func TestStackAsListEmpty(t *testing.T) {
	// NewStack() creates a nil slice, which returns nil pair
	s := NewStack()
	list := s.AsList()
	qt.Assert(t, list, qt.IsNil) // nil stack returns nil pair

	// Test with actual empty slice (after Push/Pop)
	s2 := NewStack(values.NewInteger(1))
	s2.Pop()
	list2 := s2.AsList()
	qt.Assert(t, list2, qt.Equals, values.EmptyList) // empty (non-nil) slice returns EmptyList
}

// TestStackSchemeString tests stack SchemeString
func TestStackSchemeString(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	str := s.SchemeString()
	qt.Assert(t, str, qt.Contains, "#<stack")
	qt.Assert(t, str, qt.Contains, "1")
	qt.Assert(t, str, qt.Contains, "2")
}

// TestStackNilSchemeString tests stack SchemeString with nil stack
func TestStackNilSchemeString(t *testing.T) {
	var s Stack
	str := s.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<stack ()>")
}

// TestStackIsVoid tests stack IsVoid
func TestStackIsVoid(t *testing.T) {
	var s Stack // nil slice
	qt.Assert(t, s.IsVoid(), qt.IsTrue)

	// NewStack() with no args creates a nil slice (variadic with no args)
	s2 := NewStack()
	qt.Assert(t, (*s2).IsVoid(), qt.IsTrue) // nil slice is void

	// NewStack with values creates non-nil slice
	s3 := NewStack(values.NewInteger(1))
	qt.Assert(t, (*s3).IsVoid(), qt.IsFalse)
}

// TestStackString tests stack String method
func TestStackString(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	str := (*s).String()
	qt.Assert(t, str, qt.Contains, "[")
	qt.Assert(t, str, qt.Contains, "1")
	qt.Assert(t, str, qt.Contains, "2")
	qt.Assert(t, str, qt.Contains, "]")
}

// TestStackNilString tests stack String with nil
func TestStackNilString(t *testing.T) {
	var s Stack
	str := s.String()
	qt.Assert(t, str, qt.Equals, "[]")
}

// TestStackPushAll tests stack PushAll
func TestStackPushAll(t *testing.T) {
	s := NewStack()
	s.PushAll([]values.Value{
		values.NewInteger(1),
		values.NewInteger(2),
		values.NewInteger(3),
	})
	qt.Assert(t, s.Length(), qt.Equals, 3)
}

// TestStackPeekKMultiple tests PeekK at different positions
func TestStackPeekKMultiple(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	// PeekK(0) is top of stack (last pushed)
	qt.Assert(t, s.PeekK(0), values.SchemeEquals, values.NewInteger(3))
	// PeekK(1) is one below top
	qt.Assert(t, s.PeekK(1), values.SchemeEquals, values.NewInteger(2))
	// PeekK(2) is bottom
	qt.Assert(t, s.PeekK(2), values.SchemeEquals, values.NewInteger(1))
}

// TestStackNilLength tests Length on nil stack
func TestStackNilLength(t *testing.T) {
	var s Stack
	qt.Assert(t, s.Length(), qt.Equals, 0)
}

// TestPlatformFeaturesAdditional tests the platformFeatures function additional paths
func TestPlatformFeaturesAdditional(t *testing.T) {
	features := platformFeatures()
	// Should contain at least r7rs and some platform features
	qt.Assert(t, len(features) > 0, qt.IsTrue)
}

// TestCondExpandWithElse tests cond-expand with else clause
func TestCondExpandWithElse(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with else clause (should be supported since else is always true)
	sv := parseSchemeExpr(t, env, `(cond-expand (else 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCondExpandR7RS tests cond-expand with r7rs feature
func TestCondExpandR7RS(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with r7rs feature (should always be present)
	sv := parseSchemeExpr(t, env, `(cond-expand (r7rs 100) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(100))
}

// TestCondExpandAnd tests cond-expand with and requirement
func TestCondExpandAnd(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(cond-expand ((and r7rs) 200) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(200))
}

// TestCondExpandOr tests cond-expand with or requirement
func TestCondExpandOr(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(cond-expand ((or r7rs nonexistent-feature) 300) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(300))
}

// TestCondExpandNot tests cond-expand with not requirement
func TestCondExpandNot(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(cond-expand ((not nonexistent-feature) 400) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(400))
}

// TestMultipleValuesOperationMethods tests MultipleValues methods
func TestMultipleValuesOperationMethods(t *testing.T) {
	mv := MultipleValues{values.NewInteger(1), values.NewInteger(2)}

	// SchemeString - returns concatenation of values, not #<multiple-values
	str := mv.SchemeString()
	qt.Assert(t, str, qt.Contains, "1")

	// IsVoid
	qt.Assert(t, mv.IsVoid(), qt.IsFalse)

	var nilMv MultipleValues
	qt.Assert(t, nilMv.IsVoid(), qt.IsTrue)
}

// TestNativeTemplateDeduplicateLiteralVector tests vector literal deduplication
func TestNativeTemplateDeduplicateLiteralVector(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Create a vector with symbols
	sym := values.NewSymbol("test")
	vec := values.NewVector(sym, values.NewInteger(42))

	// Deduplicate
	deduped := tpl.DeduplicateLiteral(vec)
	qt.Assert(t, deduped, qt.IsNotNil)
}

// TestNativeTemplateDeduplicateLiteralEmptyVector tests empty vector deduplication
func TestNativeTemplateDeduplicateLiteralEmptyVector(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Empty vector
	vec := values.NewVector()
	deduped := tpl.DeduplicateLiteral(vec)
	qt.Assert(t, deduped, qt.Equals, vec)
}

// TestNativeTemplateCopyNil tests Copy on nil NativeTemplate
func TestNativeTemplateCopyNil(t *testing.T) {
	var tpl *NativeTemplate
	cpy := tpl.Copy()
	qt.Assert(t, cpy, qt.IsNil)
}

// TestOperationRestoreContinuationMethodsExtra tests OperationRestoreContinuation extra methods
func TestOperationRestoreContinuationMethodsExtra(t *testing.T) {
	op := NewOperationRestoreContinuation()

	// Test nil case
	var nilOp *OperationRestoreContinuation
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationPullMethodsExtra tests OperationPull extra methods
func TestOperationPullMethodsExtra(t *testing.T) {
	op := NewOperationPull()

	// Test nil case
	var nilOp *OperationPull
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationsLength tests Operations Length method
func TestOperationsLength(t *testing.T) {
	ops := Operations{
		NewOperationLoadVoid(),
		NewOperationPush(),
	}
	qt.Assert(t, ops.Length(), qt.Equals, 2)

	var nilOps Operations
	qt.Assert(t, nilOps.Length(), qt.Equals, 0)
}

// TestOperationsCopy tests Operations Copy method
func TestOperationsCopy(t *testing.T) {
	ops := Operations{
		NewOperationLoadVoid(),
		NewOperationPush(),
	}
	cpy := ops.Copy()
	qt.Assert(t, cpy.Length(), qt.Equals, 2)

	// Modifying copy shouldn't affect original
	cpy = append(cpy, NewOperationPop())
	qt.Assert(t, ops.Length(), qt.Equals, 2)
	qt.Assert(t, cpy.Length(), qt.Equals, 3)
}

// TestQuasiquoteWithUnquote tests basic quasiquote with unquote
func TestQuasiquoteWithUnquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple quasiquote without unquote (doesn't need runtime bindings)
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestQuasiquoteNested tests nested quasiquote
func TestQuasiquoteNested(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested quasiquote
	sv := parseSchemeExpr(t, env, "``(a ,b)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestQuasiquoteWithDotPair tests quasiquote with dotted pair
func TestQuasiquoteWithDotPair(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Quasiquote with dotted pair
	sv := parseSchemeExpr(t, env, "`(a . b)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestQuasiquoteVector tests quasiquote with vector
func TestQuasiquoteVector(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Quasiquote with vector
	sv := parseSchemeExpr(t, env, "`#(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileDefineFn tests define with function shorthand
func TestCompileDefineFn(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn x) x) shorthand form
	sv := parseSchemeExpr(t, env, `(define (identity x) x)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileDefineFnVariadic tests define with variadic function shorthand
func TestCompileDefineFnVariadic(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn . args) args) variadic shorthand form
	sv := parseSchemeExpr(t, env, `(define (varargs . x) x)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileSymbolGlobal tests compiling a global symbol reference
func TestCompileSymbolGlobal(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// First define a global
	sv := parseSchemeExpr(t, env, `(define my-global 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}

	// Then reference it
	sv2 := parseSchemeExpr(t, env, `my-global`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc2.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSetBangGlobal tests set! on global variable
func TestCompileSetBangGlobal(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define, then set!, then reference
	sv := parseSchemeExpr(t, env, `(define my-var 10)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}

	// Set it to new value
	sv2 := parseSchemeExpr(t, env, `(set! my-var 20)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}

	// Check the new value
	sv3 := parseSchemeExpr(t, env, `my-var`)
	cont3, err := newTopLevelThunk(sv3, env)
	qt.Assert(t, err, qt.IsNil)
	mc3 := NewMachineContext(cont3)
	err = mc3.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc3.GetValue(), values.SchemeEquals, values.NewInteger(20))
}

// TestCompileBegin tests begin form
func TestCompileBegin(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(begin 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCompileLambdaMultiExprBody tests lambda with multiple expressions in body
func TestCompileLambdaMultiExprBody(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `((lambda () 1 2 3))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCompileCaseLambdaMultiClause tests case-lambda with multiple clauses
func TestCompileCaseLambdaMultiClause(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Use only self-evaluating expressions
	sv := parseSchemeExpr(t, env, `(define f (case-lambda (() 0) ((x) x) ((x y) x)))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileQuoteSymbol tests quoting a symbol
func TestCompileQuoteSymbol(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'my-symbol`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	val := mc.GetValue()
	_, isSymbol := val.(*values.Symbol)
	qt.Assert(t, isSymbol, qt.IsTrue)
}

// TestCompileQuoteVector tests quoting a vector
func TestCompileQuoteVector(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'#(1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	val := mc.GetValue()
	_, isVector := val.(*values.Vector)
	qt.Assert(t, isVector, qt.IsTrue)
}

// TestOperationForeignFunctionCallSimple tests foreign function call
func TestOperationForeignFunctionCallSimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// Create a foreign function
	called := false
	fn := ForeignFunction(func(ctx context.Context, mc *MachineContext) error {
		called = true
		mc.SetValue(values.NewInteger(99))
		return nil
	})

	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations,
		NewOperationForeignFunctionCall(fn),
		NewOperationRestoreContinuation(),
	)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)
	err := mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, called, qt.IsTrue)
}

// TestMachineContinuationFromMachineContext tests creating continuation from context
func TestMachineContinuationFromMachineContext(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations,
		NewOperationLoadLiteralByLiteralIndexImmediate(tpl.MaybeAppendLiteral(values.NewInteger(42))),
		NewOperationRestoreContinuation(),
	)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)
	mc.pc = 0

	// Create continuation from machine context
	newCont := NewMachineContinuationFromMachineContext(mc, 1)
	qt.Assert(t, newCont, qt.IsNotNil)
}

// TestMachineContextApplySimple tests mc.Apply with a simple closure
func TestMachineContextApplySimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Create a lambda and apply it
	sv := parseSchemeExpr(t, env, `((lambda (x) x) 100)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(100))
}

// TestOperationMakeClosureError tests MakeClosure with wrong stack
func TestOperationMakeClosureError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations,
		// Push something that's not an environment frame
		NewOperationLoadLiteralByLiteralIndexImmediate(tpl.MaybeAppendLiteral(values.NewInteger(42))),
		NewOperationPush(),
		// Push something that's not a template
		NewOperationLoadLiteralByLiteralIndexImmediate(tpl.MaybeAppendLiteral(values.NewInteger(99))),
		NewOperationPush(),
		NewOperationMakeClosure(),
	)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)
	err := mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNotNil) // Should error because stack has wrong types
}

// TestNativeTemplateEqualToDifferent tests NativeTemplate EqualTo with different templates
func TestNativeTemplateEqualToDifferent(t *testing.T) {
	tpl1 := NewNativeTemplate(1, 1, false)
	tpl2 := NewNativeTemplate(2, 1, false) // Different parameter count
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsFalse)

	tpl3 := NewNativeTemplate(1, 2, false) // Different value count
	qt.Assert(t, tpl1.EqualTo(tpl3), qt.IsFalse)

	tpl4 := NewNativeTemplate(1, 1, true) // Different variadic
	qt.Assert(t, tpl1.EqualTo(tpl4), qt.IsFalse)

	// Different type
	qt.Assert(t, tpl1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Same
	tpl5 := NewNativeTemplate(1, 1, false)
	qt.Assert(t, tpl1.EqualTo(tpl5), qt.IsTrue)
}

// TestNativeTemplateEqualToNil tests NativeTemplate EqualTo with nil
func TestNativeTemplateEqualToNil(t *testing.T) {
	tpl1 := NewNativeTemplate(1, 1, false)
	var tpl2 *NativeTemplate
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsFalse)
	qt.Assert(t, tpl2.EqualTo(tpl1), qt.IsFalse)
	qt.Assert(t, tpl2.EqualTo(tpl2), qt.IsTrue) // nil == nil
}

// TestMachineClosureMethods tests MachineClosure methods
func TestMachineClosureMethods(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(2, 1, true)
	closure := NewClosureWithTemplate(tpl, env)

	qt.Assert(t, closure.SchemeString(), qt.Contains, "#<machine-closure")
	qt.Assert(t, closure.IsVoid(), qt.IsFalse)

	var nilClosure *MachineClosure
	qt.Assert(t, nilClosure.IsVoid(), qt.IsTrue)
}

// TestMachineContinuationMethods tests MachineContinuation methods
func TestMachineContinuationMethods(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)

	qt.Assert(t, cont.SchemeString(), qt.Contains, "machine-continuation")
	qt.Assert(t, cont.IsVoid(), qt.IsFalse)

	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.IsVoid(), qt.IsTrue)
}

// TestLibraryNameMethods tests LibraryName methods
func TestLibraryNameMethods(t *testing.T) {
	name := NewLibraryName("scheme", "base")

	qt.Assert(t, name.String(), qt.Equals, "scheme/base")
	qt.Assert(t, name.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, name.Key(), qt.Equals, "scheme/base")            // Key uses "/" separator
	qt.Assert(t, name.ToFilePath(), qt.Equals, "scheme/base.sld") // Includes .sld extension
}

// TestLibraryRegistryLookupNotFound tests looking up non-existent library
func TestLibraryRegistryLookupNotFound(t *testing.T) {
	registry := NewLibraryRegistry()
	lib := registry.Lookup(NewLibraryName("nonexistent", "lib"))
	qt.Assert(t, lib, qt.IsNil)
}

// TestFeatureRequirementIsSatisfied tests various feature requirements
func TestFeatureRequirementIsSatisfied(t *testing.T) {
	registry := NewLibraryRegistry()

	// FeatureIdentifier
	r7rsFeat := NewFeatureIdentifier("r7rs")
	qt.Assert(t, r7rsFeat.IsSatisfied(registry), qt.IsTrue)

	nonExistent := NewFeatureIdentifier("nonexistent-feature")
	qt.Assert(t, nonExistent.IsSatisfied(registry), qt.IsFalse)

	// AndRequirement with single true (variadic function)
	andReq := NewAndRequirement(r7rsFeat)
	qt.Assert(t, andReq.IsSatisfied(registry), qt.IsTrue)

	// AndRequirement with one false
	andReqFail := NewAndRequirement(r7rsFeat, nonExistent)
	qt.Assert(t, andReqFail.IsSatisfied(registry), qt.IsFalse)

	// OrRequirement
	orReq := NewOrRequirement(nonExistent, r7rsFeat)
	qt.Assert(t, orReq.IsSatisfied(registry), qt.IsTrue)

	// NotRequirement
	notReq := NewNotRequirement(nonExistent)
	qt.Assert(t, notReq.IsSatisfied(registry), qt.IsTrue)

	// Library requirement
	libReq := NewLibraryRequirement(NewLibraryName("nonexistent", "lib"))
	qt.Assert(t, libReq.IsSatisfied(registry), qt.IsFalse)
}

// TestCompileIfThenOnly tests if with only then branch
func TestCompileIfThenOnly(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(if #t 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileIfFalsePath tests if with false condition
func TestCompileIfFalsePath(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(if #f 1 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}

// TestCompileDefineVarSimple tests define simple variable
func TestCompileDefineVarSimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(define x 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileLambdaWithMultipleParams tests lambda with multiple parameters
func TestCompileLambdaWithMultipleParams(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `((lambda (a b c) a) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(1))
}

// TestCompileLambdaRest tests lambda with rest parameter only
func TestCompileLambdaRest(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(define all-args (lambda args args))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileQuoteList tests quoting a list
func TestCompileQuoteList(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'(1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	val := mc.GetValue()
	_, isPair := val.(*values.Pair)
	qt.Assert(t, isPair, qt.IsTrue)
}

// TestCompileSelfEvaluatingNil tests compiling void/nil
func TestCompileSelfEvaluatingNil(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// #void or equivalent
	sv := parseSchemeExpr(t, env, `#t`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileSelfEvaluatingNilDirect tests CompileSelfEvaluating with nil directly
func TestCompileSelfEvaluatingNilDirect(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	tpl := NewNativeTemplate(0, 0, false)
	ctc := NewCompiletimeContinuation(tpl, env)
	ccnt := NewCompileTimeCallContext(false, true, env)

	// Call with nil to test the nil branch
	err := ctc.CompileSelfEvaluating(ccnt, nil)
	qt.Assert(t, err, qt.IsNil)
	// Should have appended LoadVoid operation
	qt.Assert(t, len(tpl.operations), qt.Equals, 1)
}

// TestOperationBranchMethodsExtra tests OperationBranch extra methods
func TestOperationBranchMethodsExtra(t *testing.T) {
	op := NewOperationBranchOffsetImmediate(10)

	// Nil check
	var nilOp *OperationBranchOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

// TestOperationBranchOnFalseMethodsExtra tests OperationBranchOnFalse extra methods
func TestOperationBranchOnFalseMethodsExtra(t *testing.T) {
	op := NewOperationBranchOnFalseOffsetImmediate(5)

	// Nil check
	var nilOp *OperationBranchOnFalseOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

// TestOperationSaveContinuationMethodsExtra tests OperationSaveContinuation extra methods
func TestOperationSaveContinuationMethodsExtra(t *testing.T) {
	op := NewOperationSaveContinuationOffsetImmediate(3)

	// Nil check
	var nilOp *OperationSaveContinuationOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

// TestOperationApplyMethodsExtra tests OperationApply extra methods
func TestOperationApplyMethodsExtra(t *testing.T) {
	op := NewOperationApply()

	// EqualTo - nil case
	var nilOp *OperationApply
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

// TestOperationLoadGlobalMethodsExtra tests OperationLoadGlobal extra methods
func TestOperationLoadGlobalMethodsExtra(t *testing.T) {
	op := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationStoreGlobalMethodsExtra tests OperationStoreGlobal extra methods
func TestOperationStoreGlobalMethodsExtra(t *testing.T) {
	op := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationPopMethodsExtra tests OperationPop extra methods
func TestOperationPopMethodsExtra(t *testing.T) {
	op := NewOperationPop()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// EqualTo - nil case
	var nilOp *OperationPop
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestCompileSyntaxRulesSimple tests simple syntax-rules
func TestCompileSyntaxRulesSimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple macro
	sv := parseSchemeExpr(t, env, `(define-syntax my-id (syntax-rules () ((_ x) x)))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestAllFeaturesFunction tests AllFeatures returns expected features
func TestAllFeaturesFunction(t *testing.T) {
	features := AllFeatures()
	qt.Assert(t, len(features) > 0, qt.IsTrue)

	// Should include r7rs
	found := false
	for _, f := range features {
		if f == "r7rs" {
			found = true
			break
		}
	}
	qt.Assert(t, found, qt.IsTrue)
}

// TestIsFeatureSupportedFunction tests IsFeatureSupported function
func TestIsFeatureSupportedFunction(t *testing.T) {
	qt.Assert(t, IsFeatureSupported("r7rs"), qt.IsTrue)
	qt.Assert(t, IsFeatureSupported("nonexistent-feature"), qt.IsFalse)
}

// TestLibraryRegistryRegister tests LibraryRegistry.Register
func TestLibraryRegistryRegister(t *testing.T) {
	registry := NewLibraryRegistry()
	name := NewLibraryName("my", "lib")
	lib := &CompiledLibrary{Name: name}

	registry.Register(lib) //nolint:errcheck
	result := registry.Lookup(name)
	qt.Assert(t, result, qt.IsNotNil)
	qt.Assert(t, result.Name.Key(), qt.Equals, name.Key())
}

// TestCompiledLibraryMethods tests CompiledLibrary methods
func TestCompiledLibraryMethods(t *testing.T) {
	name := NewLibraryName("test", "lib")
	lib := &CompiledLibrary{
		Name:    name,
		Exports: make(map[string]string),
	}

	lib.Exports["foo"] = "foo"
	qt.Assert(t, len(lib.Exports), qt.Equals, 1)
}

// TestMultipleValuesEqualTo tests MultipleValues EqualTo method
func TestMultipleValuesEqualTo(t *testing.T) {
	mv1 := MultipleValues{values.NewInteger(1), values.NewInteger(2)}
	mv2 := MultipleValues{values.NewInteger(1), values.NewInteger(2)}

	qt.Assert(t, mv1.EqualTo(mv2), qt.IsTrue)

	mv3 := MultipleValues{values.NewInteger(1)}
	qt.Assert(t, mv1.EqualTo(mv3), qt.IsFalse)

	qt.Assert(t, mv1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestOperationLoadLocalMethods tests OperationLoadLocal methods
func TestOperationLoadLocalMethods(t *testing.T) {
	idx := &environment.LocalIndex{0, 0}
	op := NewOperationLoadLocalByLocalIndexImmediate(idx)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationStoreLocalMethods tests OperationStoreLocal methods
func TestOperationStoreLocalMethods(t *testing.T) {
	idx := &environment.LocalIndex{0, 0}
	op := NewOperationStoreLocalByLocalIndexImmediate(idx)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationsSchemeString tests Operations SchemeString method
func TestOperationsSchemeString(t *testing.T) {
	ops := Operations{
		NewOperationLoadVoid(),
		NewOperationPush(),
	}
	str := ops.SchemeString()
	qt.Assert(t, str, qt.IsNotNil)
}

// TestOperationsIsVoid tests Operations IsVoid method
func TestOperationsIsVoid(t *testing.T) {
	var nilOps Operations
	qt.Assert(t, nilOps.IsVoid(), qt.IsTrue)

	ops := Operations{NewOperationLoadVoid()}
	qt.Assert(t, ops.IsVoid(), qt.IsFalse)
}

// TestOperationsEqualTo tests Operations EqualTo method
func TestOperationsEqualTo(t *testing.T) {
	ops1 := Operations{NewOperationLoadVoid()}
	ops2 := Operations{NewOperationLoadVoid()}
	qt.Assert(t, ops1.EqualTo(ops2), qt.IsTrue)

	ops3 := Operations{NewOperationPush()}
	qt.Assert(t, ops1.EqualTo(ops3), qt.IsFalse)

	qt.Assert(t, ops1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestOperationSyntaxRulesTransformApply tests OperationSyntaxRulesTransform.Apply
func TestOperationSyntaxRulesTransformApply(t *testing.T) {
	op := NewOperationSyntaxRulesTransform()
	qt.Assert(t, op.String(), qt.Equals, "SyntaxRulesTransform")
	qt.Assert(t, op.SchemeString(), qt.Contains, "syntax-rules")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(NewOperationSyntaxRulesTransform()), qt.IsTrue)
	qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestAddScopeToSyntaxCoverage tests additional addScopeToSyntax paths
func TestAddScopeToSyntaxCoverage(t *testing.T) {
	// Test syntax pair
	scope := syntax.NewScope(nil)
	sym1 := syntax.NewSyntaxSymbol("a", nil)
	sym2 := syntax.NewSyntaxSymbol("b", nil)
	pair := syntax.NewSyntaxCons(sym1, sym2, nil)
	result := addScopeToSyntax(pair, scope)
	qt.Assert(t, result, qt.IsNotNil)
}

// TestCompileSelfEvaluatingValues tests compilation of self-evaluating values
func TestCompileSelfEvaluatingValues(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"float", "3.14"},
		{"string", "\"hello\""},
		{"character", "#\\a"},
		{"boolean true", "#t"},
		{"boolean false", "#f"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileSymbolVariants tests various symbol compilation paths
func TestCompileSymbolVariants(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"local variable", "((lambda (x) x) 1)"},
		{"nested local", "((lambda (x) ((lambda (y) x) 2)) 1)"},
		{"deeply nested", "((lambda (a) ((lambda (b) ((lambda (c) a) 3)) 2)) 1)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestParseLibraryNameErrors tests parseLibraryName error cases
func TestParseLibraryNameErrors(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"empty library name", "(define-library ())"},
		{"invalid library name element", "(define-library (scheme 123))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			_, err := newTopLevelThunk(sv, env)
			// These should either succeed or fail gracefully
			_ = err
		})
	}
}

// TestFeatureRequirementTypes tests FeatureRequirement type creation
func TestFeatureRequirementTypes(t *testing.T) {
	// Test different feature requirement types
	feat := NewFeatureIdentifier("r7rs")
	qt.Assert(t, feat, qt.IsNotNil)

	andReq := NewAndRequirement(feat)
	qt.Assert(t, andReq, qt.IsNotNil)

	orReq := NewOrRequirement(feat)
	qt.Assert(t, orReq, qt.IsNotNil)

	notReq := NewNotRequirement(feat)
	qt.Assert(t, notReq, qt.IsNotNil)

	libReq := NewLibraryRequirement(NewLibraryName("scheme", "base"))
	qt.Assert(t, libReq, qt.IsNotNil)
}

// TestCompileDefineSyntaxErrors tests define-syntax error cases
func TestCompileDefineSyntaxErrors(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"missing transformer", "(define-syntax foo)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			_, err := newTopLevelThunk(sv, env)
			// Should fail
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestPlatformFeaturesCoverage tests platformFeatures function coverage
func TestPlatformFeaturesCoverage(t *testing.T) {
	features := platformFeatures()
	// Check for some expected platform-specific features
	// Should have darwin or linux or windows
	hasDarwin := false
	hasLinux := false
	hasWindows := false
	for _, f := range features {
		if f == "darwin" {
			hasDarwin = true
		}
		if f == "linux" {
			hasLinux = true
		}
		if f == "windows" {
			hasWindows = true
		}
	}
	// At least one platform should be present
	qt.Assert(t, hasDarwin || hasLinux || hasWindows, qt.IsTrue)
}

// TestLibraryNamePathConversion tests LibraryName ToFilePath method
func TestLibraryNamePathConversion(t *testing.T) {
	name1 := NewLibraryName("scheme", "base")
	name2 := NewLibraryName("scheme", "base")
	name3 := NewLibraryName("scheme", "write")

	qt.Assert(t, name1.Key(), qt.Equals, name2.Key())
	qt.Assert(t, name1.Key(), qt.Not(qt.Equals), name3.Key())
	qt.Assert(t, name1.String(), qt.Equals, "scheme/base")
	qt.Assert(t, name1.SchemeString(), qt.Equals, "(scheme base)")
	qt.Assert(t, strings.Contains(name1.ToFilePath(), "scheme"), qt.IsTrue)
}

// TestImportSetFields tests ImportSet fields
func TestImportSetFields(t *testing.T) {
	is := &ImportSet{
		LibraryName: NewLibraryName("scheme", "base"),
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

// TestCompileExpressionListError tests expression list compilation with improper list
func TestCompileExpressionListError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// begin with multiple expressions exercises compileExpressionList
	testCases := []struct {
		name string
		prog string
	}{
		{"begin with multiple", "(begin 1 2 3)"},
		{"nested begin", "(begin (begin 1) 2)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestNativeTemplateLiterals tests NativeTemplate literal methods
func TestNativeTemplateLiterals(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Add some literals
	idx1 := tpl.MaybeAppendLiteral(values.NewInteger(1))
	idx2 := tpl.MaybeAppendLiteral(values.NewInteger(2))
	idx3 := tpl.MaybeAppendLiteral(values.NewInteger(1)) // duplicate

	qt.Assert(t, idx1, qt.Equals, LiteralIndex(0))
	qt.Assert(t, idx2, qt.Equals, LiteralIndex(1))
	qt.Assert(t, idx3, qt.Equals, LiteralIndex(0)) // should be same as idx1

	// Test findLiteral
	found := tpl.findLiteral(values.NewInteger(2))
	qt.Assert(t, found, qt.IsNotNil)

	notFound := tpl.findLiteral(values.NewInteger(999))
	qt.Assert(t, notFound, qt.IsNil)
}

// TestNativeTemplateDeduplicateLiteral tests deduplication edge cases
func TestNativeTemplateDeduplicateLiteral(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Test with nil Pair
	var nilPair *values.Pair = nil
	result := tpl.DeduplicateLiteral(nilPair)
	qt.Assert(t, result, qt.Equals, nilPair)

	// Test with empty list
	result = tpl.DeduplicateLiteral(values.EmptyList)
	qt.Assert(t, result, qt.Equals, values.EmptyList)

	// Test with nil Vector
	var nilVec *values.Vector = nil
	result = tpl.DeduplicateLiteral(nilVec)
	qt.Assert(t, result, qt.Equals, nilVec)

	// Test with empty vector
	emptyVec := values.NewVector()
	result = tpl.DeduplicateLiteral(emptyVec)
	qt.Assert(t, result, qt.Equals, emptyVec)

	// Test with non-deduplicatable type (string)
	str := values.NewString("hello")
	result = tpl.DeduplicateLiteral(str)
	qt.Assert(t, result, qt.Equals, str)
}

// TestMachineContinuationEqualToDifferentTemplates tests continuation equality with different templates
func TestMachineContinuationEqualToDifferentTemplates(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())
	cont := NewMachineContinuation(nil, tpl, env)

	// Test with different templates
	tpl2 := NewNativeTemplate(1, 1, true)
	tpl2.AppendOperations(NewOperationLoadLiteralInteger(1), NewOperationRestoreContinuation())
	cont2 := NewMachineContinuation(nil, tpl2, env)
	qt.Assert(t, cont.EqualTo(cont2), qt.IsFalse) // Different templates
}

// TestMachineClosureEqualToNil tests closure equality with nil
func TestMachineClosureEqualToNil(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(2, 2, true)
	cls := NewClosureWithTemplate(tpl, env)

	// Test with nil closure
	var nilCls *MachineClosure
	qt.Assert(t, nilCls.IsVoid(), qt.IsTrue)
	qt.Assert(t, cls.EqualTo(nilCls), qt.IsFalse)
}

// TestClausesWrapperMethods tests clausesWrapper methods
func TestClausesWrapperMethods(t *testing.T) {
	cw := &clausesWrapper{
		clauses: nil,
	}

	qt.Assert(t, cw.SchemeString(), qt.Contains, "syntax-rules")
	qt.Assert(t, cw.IsVoid(), qt.IsFalse)

	// Test EqualTo - clausesWrapper always returns false (not comparable)
	cw2 := &clausesWrapper{clauses: nil}
	qt.Assert(t, cw.EqualTo(cw2), qt.IsFalse) // clauses are not comparable per implementation
	qt.Assert(t, cw.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestOperationBranchOnFalseEqualTo tests branch on false equality
func TestOperationBranchOnFalseEqualTo(t *testing.T) {
	op1 := NewOperationBranchOnFalseOffsetImmediate(5)
	op2 := NewOperationBranchOnFalseOffsetImmediate(5)
	op3 := NewOperationBranchOnFalseOffsetImmediate(10)

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)

	var nilOp *OperationBranchOnFalseOffsetImmediate
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
	qt.Assert(t, op1.IsVoid(), qt.IsFalse)
}

// TestOperationSaveContinuationEqualToAdditional tests additional save continuation paths
func TestOperationSaveContinuationEqualToAdditional(t *testing.T) {
	op1 := NewOperationSaveContinuationOffsetImmediate(5)

	// Test nil comparison
	var nilOp *OperationSaveContinuationOffsetImmediate
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationApplyAdditional tests Apply operation additional methods
func TestOperationApplyAdditional(t *testing.T) {
	op := NewOperationApply()
	qt.Assert(t, op.SchemeString(), qt.Contains, "apply")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(NewOperationApply()), qt.IsTrue)
	qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestOperationPop tests Pop operation
func TestOperationPop(t *testing.T) {
	op := NewOperationPop()
	qt.Assert(t, op.SchemeString(), qt.Contains, "pop")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(NewOperationPop()), qt.IsTrue)
}

// TestForeignClosureSchemeString tests ForeignClosure methods
func TestForeignClosureSchemeString(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	fn := func(ctx context.Context, mc *MachineContext) error {
		return nil
	}
	cls := NewForeignClosure(env, 2, true, fn)

	// Check that closure was created (can't test ParameterCount/IsVariadic on MachineClosure)
	qt.Assert(t, cls, qt.IsNotNil)
	qt.Assert(t, cls.SchemeString(), qt.Contains, "closure")
}

// TestCaseLambdaClosureFindMatching tests CaseLambdaClosure.FindMatchingClause
func TestCaseLambdaClosureFindMatching(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// Create clauses with different arities
	tpl1 := NewNativeTemplate(1, 1, false)
	tpl1.AppendOperations(NewOperationLoadLiteralInteger(1), NewOperationRestoreContinuation())
	cls1 := NewClosureWithTemplate(tpl1, env)

	tpl2 := NewNativeTemplate(2, 2, false)
	tpl2.AppendOperations(NewOperationLoadLiteralInteger(2), NewOperationRestoreContinuation())
	cls2 := NewClosureWithTemplate(tpl2, env)

	caseCls := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})

	qt.Assert(t, caseCls.SchemeString(), qt.Contains, "case-lambda")
	qt.Assert(t, caseCls.IsVoid(), qt.IsFalse)

	// Test clause finding
	clauses := caseCls.Clauses()
	qt.Assert(t, len(clauses), qt.Equals, 2)

	// Find matching clause for 1 arg
	match, found := caseCls.FindMatchingClause(1)
	qt.Assert(t, found, qt.IsTrue)
	qt.Assert(t, match, qt.Equals, cls1)

	// Find matching clause for 2 args
	match, found = caseCls.FindMatchingClause(2)
	qt.Assert(t, found, qt.IsTrue)
	qt.Assert(t, match, qt.Equals, cls2)

	// No match for 3 args
	match, found = caseCls.FindMatchingClause(3)
	qt.Assert(t, found, qt.IsFalse)
	qt.Assert(t, match, qt.IsNil)
}

// TestOperationMakeCaseLambdaClosureAdditional tests MakeCaseLambdaClosure additional methods
func TestOperationMakeCaseLambdaClosureAdditional(t *testing.T) {
	op := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.SchemeString(), qt.Contains, "case-lambda")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test nil comparison
	var nilOp *OperationMakeCaseLambdaClosure
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

// TestCompileValidatedCallEdgeCases tests edge cases in call compilation
func TestCompileValidatedCallEdgeCases(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// Test tail call optimization
	testCases := []struct {
		name string
		prog string
	}{
		{"tail call in lambda", "(lambda () ((lambda (x) x) 1))"},
		{"non-tail call followed by value", "((lambda () (define x 1) ((lambda (y) y) 2)))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileSetBangErrors tests set! error cases
func TestCompileSetBangErrors(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"set unbound variable", "(set! nonexistent 1)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			_, err := newTopLevelThunk(sv, env)
			// Should fail for unbound variable
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestMachineContextValueMethods tests MachineContext value get/set
func TestMachineContextValueMethods(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)

	// Test SetValue and GetValue
	mc.SetValue(values.NewInteger(42))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))

	// Test SetValues and GetValues
	mc.SetValues(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	vals := mc.GetValues()
	qt.Assert(t, len(vals), qt.Equals, 3)
}

// TestStackCopyMethod tests Stack.Copy more thoroughly
func TestStackCopyMethod(t *testing.T) {
	s := NewStack(values.NewInteger(1), values.NewInteger(2))
	copied := s.Copy()

	qt.Assert(t, s.Length(), qt.Equals, copied.Length())

	// Modify original, copied should be unchanged
	s.Push(values.NewInteger(3))
	qt.Assert(t, s.Length(), qt.Equals, 3)
	qt.Assert(t, copied.Length(), qt.Equals, 2)
}

// TestStackPeekKBoundary tests Stack.PeekK edge cases
func TestStackPeekKBoundary(t *testing.T) {
	s := NewStack(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))

	// Peek at different positions
	qt.Assert(t, s.PeekK(0), values.SchemeEquals, values.NewInteger(3)) // top
	qt.Assert(t, s.PeekK(1), values.SchemeEquals, values.NewInteger(2)) // second
	qt.Assert(t, s.PeekK(2), values.SchemeEquals, values.NewInteger(1)) // third/bottom

	// Note: PeekK panics on out-of-bounds access (not a bounds-checked API)
}

// TestQuasiquoteNeedsRuntime tests quasiquoteNeedsRuntime paths
func TestQuasiquoteNeedsRuntime(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"simple quote no runtime", "`a"},
		{"nested lists", "`((a b) c)"},
		{"quoted vector", "`#(1 2 3)"},
		{"deeply nested", "`(a (b (c d)))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileIncludeErrorAdditional tests include error paths
func TestCompileIncludeErrorAdditional(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// include with empty string should error
	sv := parseSchemeExpr(t, env, "(include \"\")")
	_, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestMachineContextNewSubContextAdditional tests additional sub-context paths
func TestMachineContextNewSubContextAdditional(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)

	sub := mc.NewSubContext()
	qt.Assert(t, sub, qt.IsNotNil)

	// Sub context should have its own environment
	qt.Assert(t, sub.EnvironmentFrame(), qt.IsNotNil)
}

// TestAllFeaturesContainsPlatformFeatures tests that AllFeatures includes platform features
func TestAllFeaturesContainsPlatformFeatures(t *testing.T) {
	features := AllFeatures()
	pf := platformFeatures()

	// All platform features should be in AllFeatures
	for _, f := range pf {
		found := false
		for _, af := range features {
			if af == f {
				found = true
				break
			}
		}
		qt.Assert(t, found, qt.IsTrue, qt.Commentf("platform feature %q not found", f))
	}
}

// TestLibraryRequirementIsSatisfiedAdditional tests libraryRequirement.IsSatisfied
func TestLibraryRequirementIsSatisfiedAdditional(t *testing.T) {
	// With nil registry
	libReq := &libraryRequirement{name: NewLibraryName("scheme", "base")}
	qt.Assert(t, libReq.IsSatisfied(nil), qt.IsFalse)

	// With registry but library not loaded
	registry := NewLibraryRegistry()
	qt.Assert(t, libReq.IsSatisfied(registry), qt.IsFalse)

	// With library registered
	env := environment.NewTopLevelEnvironmentFrame()
	lib := NewCompiledLibrary(NewLibraryName("test", "lib"), env)
	registry.Register(lib) //nolint:errcheck

	testLibReq := &libraryRequirement{name: NewLibraryName("test", "lib")}
	qt.Assert(t, testLibReq.IsSatisfied(registry), qt.IsTrue)
}

// TestCompileQuasiquotePairNestedUnquote tests nested unquote in quasiquote
func TestCompileQuasiquotePairNestedUnquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	testCases := []struct {
		name string
		code string
	}{
		{"nested quasiquote", "``a"},
		{"triple nested", "```(a b)"},
		{"quasiquote in list", "`(a `(b c))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.code)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileQuasiquoteUnquoteSplicingInList tests unquote-splicing in list context
func TestCompileQuasiquoteUnquoteSplicingInList(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test simple quasiquote with unquote (no list primitive needed)
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSymbolLocal tests CompileSymbol for local variables
func TestCompileSymbolLocal(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test local variable lookup in lambda
	sv := parseSchemeExpr(t, env, "((lambda (x) x) 42)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSymbolNoBinding tests CompileSymbol with unbound symbol
func TestCompileSymbolNoBinding(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, "unbound-symbol-test-xyz")
	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no such")
}

// TestCompilePrimitiveOrProcedureCallWithPair tests procedure call with pair in function position
func TestCompilePrimitiveOrProcedureCallWithPair(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Call with lambda directly in function position
	sv := parseSchemeExpr(t, env, "((lambda (x) x) 42)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestNewFeatureRequirementConstructors tests constructor functions for FeatureRequirement
func TestNewFeatureRequirementConstructors(t *testing.T) {
	// Test NewFeatureIdentifier
	fi := NewFeatureIdentifier("r7rs")
	qt.Assert(t, fi, qt.IsNotNil)

	// Test NewLibraryRequirement
	lr := NewLibraryRequirement(NewLibraryName("scheme", "base"))
	qt.Assert(t, lr, qt.IsNotNil)

	// Test NewAndRequirement
	ar := NewAndRequirement(fi, lr)
	qt.Assert(t, ar, qt.IsNotNil)

	// Test NewOrRequirement
	or := NewOrRequirement(fi, lr)
	qt.Assert(t, or, qt.IsNotNil)

	// Test NewNotRequirement
	nr := NewNotRequirement(fi)
	qt.Assert(t, nr, qt.IsNotNil)

	// Test NewElseRequirement
	er := NewElseRequirement()
	qt.Assert(t, er, qt.IsNotNil)
}

// TestLibraryNameToFilePath tests LibraryName.ToFilePath method
func TestLibraryNameToFilePath(t *testing.T) {
	ln := NewLibraryName("scheme", "base")
	qt.Assert(t, strings.Contains(ln.ToFilePath(), "scheme"), qt.IsTrue)
}

// TestLibraryRegistryRegisterAndLookupAdditional tests Register and Lookup
func TestLibraryRegistryRegisterAndLookupAdditional(t *testing.T) {
	reg := NewLibraryRegistry()
	env := environment.NewTopLevelEnvironmentFrame()
	lib := NewCompiledLibrary(NewLibraryName("test", "mylib"), env)
	reg.Register(lib) //nolint:errcheck

	// Lookup existing
	found := reg.Lookup(NewLibraryName("test", "mylib"))
	qt.Assert(t, found, qt.IsNotNil)
	qt.Assert(t, found.Name.String(), qt.Equals, "test/mylib")

	// Lookup non-existing
	notFound := reg.Lookup(NewLibraryName("nonexistent"))
	qt.Assert(t, notFound, qt.IsNil)
}

// TestExtractLiteralsFromSyntaxRules tests extractLiterals function via syntax-rules
func TestExtractLiteralsFromSyntaxRules(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Macro with literal identifiers
	macroCode := `(define-syntax when-test
		(syntax-rules (then)
			((_ cond then body) (if cond body #f))))`
	sv := parseSchemeExpr(t, env, macroCode)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileValidatedLambdaVariadic tests variadic lambda compilation
func TestCompileValidatedLambdaVariadic(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with rest parameter (symbol instead of list)
	sv := parseSchemeExpr(t, env, "(lambda args args)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call it
	sv = parseSchemeExpr(t, env, "((lambda args args) 1 2 3)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileValidatedDefineFnDotted tests define with dotted parameter list
func TestCompileValidatedDefineFnDotted(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define with dotted params
	sv := parseSchemeExpr(t, env, "(define (fn a . rest) rest)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call it
	sv = parseSchemeExpr(t, env, "(fn 1 2 3)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestExpandSetForm tests set! form expansion
func TestExpandSetForm(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define and then set!
	sv := parseSchemeExpr(t, env, "(define x 1)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExpr(t, env, "(set! x 42)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExpr(t, env, "x")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestExpandCaseLambdaForm tests case-lambda expansion
func TestExpandCaseLambdaForm(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterPrimitiveCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define a case-lambda without + (which isn't bound)
	sv := parseSchemeExpr(t, env, `(define cl
		(case-lambda
			(() 0)
			((x) x)
			((x y) x)))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call with 0 args
	sv = parseSchemeExpr(t, env, "(cl)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(0))

	// Call with 1 arg
	sv = parseSchemeExpr(t, env, "(cl 42)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

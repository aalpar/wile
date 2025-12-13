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
	"strings"
	"testing"

	"wile/environment"
	"wile/parser"
	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

type dummyExpandTimeCallContext struct{}

func TestExpandSymbol_ReturnsSymbol(t *testing.T) {
	env := environment.NewEnvironmentFrame(nil, nil)
	cont := NewExpanderTimeContinuation(env)
	sym := syntax.NewSyntaxSymbol("foo", nil)
	result, err := cont.ExpandSymbol(NewExpandTimeCallContext(), sym)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != sym {
		t.Errorf("expected symbol to be returned unchanged")
	}
}

func TestExpandSelfEvaluating_ReturnsExpr(t *testing.T) {
	env := environment.NewEnvironmentFrame(nil, nil)
	cont := NewExpanderTimeContinuation(env)
	num := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	result, err := cont.ExpandSelfEvaluating(NewExpandTimeCallContext(), num)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != num {
		t.Errorf("expected self-evaluating expression to be returned unchanged")
	}
}

func TestExpandExpression_Symbol(t *testing.T) {
	env := environment.NewEnvironmentFrame(nil, nil)
	cont := NewExpanderTimeContinuation(env)
	sym := syntax.NewSyntaxSymbol("bar", nil)
	cctx := NewExpandTimeCallContext()
	result, err := cont.ExpandExpression(cctx, sym)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != sym {
		t.Errorf("expected symbol to be returned unchanged")
	}
}

func TestExpandExpression_List(t *testing.T) {
	// Test that macro expansion works with a dummy transformer.
	// The expander pushes the full form (sym . args) onto the eval stack,
	// so the transformer receives the complete macro invocation.
	env := environment.NewEnvironmentFrame(nil,
		environment.NewGlobalEnvironmentFrame(nil, nil))
	gi, ok := env.CreateGlobalBinding(values.NewSymbol("bar"), environment.BindingTypeSyntax)
	qt.Assert(t, ok, qt.Equals, true)
	// Dummy transformer that reverses the arguments: (bar 10 20) -> (bar 20 10)
	mcls := NewForeignClosure(env, 1, false, func(_ context.Context, mc *MachineContext) error {
		// The full form is pushed as a single item onto the eval stack
		form, ok := mc.env.GetLocalBindingByIndex(0).Value().(syntax.SyntaxValue)
		if !ok {
			return values.ErrNotASyntaxValue
		}
		// Extract car (symbol) and cdr (arguments)
		pair, ok := form.(*syntax.SyntaxPair)
		if !ok {
			return values.ErrNotASyntaxList
		}
		_ = pair.Car() // macro name 'bar', not used in result
		args := pair.Cdr().(*syntax.SyntaxPair)
		// Collect arguments and reverse them
		var argList []syntax.SyntaxValue
		syntax.SyntaxForEach(context.Background(), args, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error { //nolint:errcheck
			argList = append(argList, v)
			return nil
		})
		// Reverse the arguments
		for i, j := 0, len(argList)-1; i < j; i, j = i+1, j-1 {
			argList[i], argList[j] = argList[j], argList[i]
		}
		// Build result: (list reversed-args...) - use 'list' not 'bar' to avoid infinite recursion
		// Since expansion now recursively expands results, returning (bar ...) would loop forever
		listSym := syntax.NewSyntaxSymbol("list", nil)
		result := syntax.SyntaxList(nil, append([]syntax.SyntaxValue{listSym}, argList...)...)
		mc.value = NewMultipleValues(result)
		return nil
	})
	err := env.SetOwnGlobalValue(gi, mcls)
	qt.Assert(t, err, qt.IsNil)
	cont := NewExpanderTimeContinuation(env)
	lst0 := syntax.SyntaxList(nil,
		syntax.NewSyntaxSymbol("bar", nil),
		syntax.NewSyntaxObject(values.NewInteger(10), nil),
		syntax.NewSyntaxObject(values.NewInteger(20), nil))
	// Expected: (list 20 10) - arguments reversed, but using 'list' instead of 'bar'
	lst1 := syntax.SyntaxList(nil,
		syntax.NewSyntaxSymbol("list", nil),
		syntax.NewSyntaxObject(values.NewInteger(20), nil),
		syntax.NewSyntaxObject(values.NewInteger(10), nil))
	cctx := NewExpandTimeCallContext()
	result, err := cont.ExpandExpression(cctx, lst0)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.UnwrapAll(), values.SchemeEquals, lst1.UnwrapAll())
}

func TestExpandCaseLambdaForm_Basic(t *testing.T) {
	env := environment.NewEnvironmentFrame(nil, environment.NewGlobalEnvironmentFrame(nil, nil))
	cont := NewExpanderTimeContinuation(env)
	cctx := NewExpandTimeCallContext()

	// (case-lambda ((x) x) ((x y) (+ x y)))
	sym := syntax.NewSyntaxSymbol("case-lambda", nil)
	clause1 := syntax.SyntaxList(nil,
		syntax.SyntaxList(nil, syntax.NewSyntaxSymbol("x", nil)),
		syntax.NewSyntaxSymbol("x", nil))
	clause2 := syntax.SyntaxList(nil,
		syntax.SyntaxList(nil, syntax.NewSyntaxSymbol("x", nil), syntax.NewSyntaxSymbol("y", nil)),
		syntax.SyntaxList(nil,
			syntax.NewSyntaxSymbol("+", nil),
			syntax.NewSyntaxSymbol("x", nil),
			syntax.NewSyntaxSymbol("y", nil)))

	clauses := syntax.SyntaxList(nil, clause1, clause2)

	result, err := cont.expandCaseLambdaForm(cctx, sym, clauses)
	qt.Assert(t, err, qt.IsNil)

	// Result should be (case-lambda expanded-clauses...)
	resultPair, ok := result.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	resultSym, ok := resultPair.Car().(*syntax.SyntaxSymbol)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, resultSym.Unwrap(), values.SchemeEquals, values.NewSymbol("case-lambda"))

	// Count clauses
	clauseCount := 0
	clausePair, ok := resultPair.Cdr().(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	syntax.SyntaxForEach(context.Background(), clausePair, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error { //nolint:errcheck
		clauseCount++
		return nil
	})
	qt.Assert(t, clauseCount, qt.Equals, 2)
}

func TestExpandCaseLambdaForm_Empty(t *testing.T) {
	env := environment.NewEnvironmentFrame(nil, environment.NewGlobalEnvironmentFrame(nil, nil))
	cont := NewExpanderTimeContinuation(env)
	cctx := NewExpandTimeCallContext()

	sym := syntax.NewSyntaxSymbol("case-lambda", nil)
	emptyClauses := syntax.SyntaxList(nil)

	result, err := cont.expandCaseLambdaForm(cctx, sym, emptyClauses)
	qt.Assert(t, err, qt.IsNil)

	// Should return (case-lambda)
	resultPair, ok := result.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	resultSym, ok := resultPair.Car().(*syntax.SyntaxSymbol)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, resultSym.Unwrap(), values.SchemeEquals, values.NewSymbol("case-lambda"))
}

// Tests moved from coverage_additional_test.go
// TestAddScopeToSyntaxViaDefineSyntax tests the addScopeToSyntax helper in syntax rules transform
func TestAddScopeToSyntaxViaDefineSyntax(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
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

// TestExpandSetForm tests set! form expansion
func TestExpandSetForm(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
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
	err := RegisterSyntaxCompilers(env)
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

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

package compilation

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

type dummyExpandTimeCallContext struct{} //nolint:unused

func TestExpandSymbol_ReturnsSymbol(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	cont := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	sym := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	result, err := cont.ExpandSymbol(sym)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != sym {
		t.Errorf("expected symbol to be returned unchanged")
	}
}

func TestExpandSelfEvaluating_ReturnsExpr(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	cont := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	num := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	result, err := cont.ExpandSelfEvaluating(num)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != num {
		t.Errorf("expected self-evaluating expression to be returned unchanged")
	}
}

func TestExpandExpression_Symbol(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	cont := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	sym := syntax.NewSyntaxSymbol("bar", nil)
	result, err := cont.ExpandExpression(sym)
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
	env := environment.NewNamespace().Runtime()
	gi, ok := env.MaybeCreateOwnGlobalBinding(values.NewSymbol("bar"), environment.BindingTypeSyntax)
	qt.Assert(t, ok, qt.Equals, true)
	// Dummy transformer that reverses the arguments: (bar 10 20) -> (bar 20 10)
	mcls := machine.NewForeignClosure(env, 1, false, func(mc machine.CallContext) error {
		// The full form is pushed as a single item onto the eval stack
		form, ok := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(syntax.SyntaxValue)
		if !ok {
			return werr.ErrNotASyntaxValue
		}
		// Extract car (symbol) and cdr (arguments)
		pair, ok := form.(*syntax.SyntaxPair)
		if !ok {
			return werr.ErrNotASyntaxList
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
		mc.SetValue(result)
		return nil
	})
	err := env.SetOwnGlobalValue(gi, mcls)
	qt.Assert(t, err, qt.IsNil)
	cont := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	lst0 := syntax.SyntaxList(nil,
		syntax.NewSyntaxSymbol("bar", nil),
		syntax.NewSyntaxObject(values.NewInteger(10), nil),
		syntax.NewSyntaxObject(values.NewInteger(20), nil))
	// Expected: (list 20 10) - arguments reversed, but using 'list' instead of 'bar'
	lst1 := syntax.SyntaxList(nil,
		syntax.NewSyntaxSymbol("list", nil),
		syntax.NewSyntaxObject(values.NewInteger(20), nil),
		syntax.NewSyntaxObject(values.NewInteger(10), nil))
	result, err := cont.ExpandExpression(lst0)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.UnwrapAll(), valuestest.SchemeEquals, lst1.UnwrapAll())
}

func TestExpandCaseLambdaForm_Basic(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	cont := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())

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

	result, err := cont.expandCaseLambdaForm(sym, clauses)
	qt.Assert(t, err, qt.IsNil)

	// Result should be (case-lambda expanded-clauses...)
	resultPair, ok := result.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	resultSym, ok := resultPair.Car().(*syntax.SyntaxSymbol)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, resultSym.Unwrap(), valuestest.SchemeEquals, values.NewSymbol("case-lambda"))

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
	env := environment.NewNamespace().Runtime()
	cont := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())

	sym := syntax.NewSyntaxSymbol("case-lambda", nil)
	emptyClauses := syntax.SyntaxList(nil)

	result, err := cont.expandCaseLambdaForm(sym, emptyClauses)
	qt.Assert(t, err, qt.IsNil)

	// Should return (case-lambda)
	resultPair, ok := result.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	resultSym, ok := resultPair.Car().(*syntax.SyntaxSymbol)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, resultSym.Unwrap(), valuestest.SchemeEquals, values.NewSymbol("case-lambda"))
}

// TestExpandSetForm tests set! form expansion
func TestExpandSetForm(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define and then set!
	sv := parseSchemeExpr(t, env, "(define x 1)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExpr(t, env, "(set! x 42)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	sv = parseSchemeExpr(t, env, "x")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestExpandCaseLambdaForm tests case-lambda expansion
func TestExpandCaseLambdaForm(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Call with 0 args
	sv = parseSchemeExpr(t, env, "(cl)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(0))

	// Call with 1 arg
	sv = parseSchemeExpr(t, env, "(cl 42)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

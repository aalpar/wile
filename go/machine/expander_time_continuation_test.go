package machine

import (
	"context"
	"skeme/environment"
	"skeme/syntax"
	"skeme/values"
	"testing"

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
		environment.NewGlobalEnvironment(nil, nil))
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
		syntax.SyntaxForEach(args, func(i int, hasNext bool, v syntax.SyntaxValue) error {
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
	err := env.SetGlobalValue(gi, mcls)
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

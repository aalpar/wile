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
	"context"
	"errors"

	"wile/environment"
	"wile/syntax"
	"wile/values"
)

// CompileDefineForSyntax handles (define-for-syntax name expr) or
// (define-for-syntax (name args...) body...).
//
// This form defines a binding in the expand phase environment that is
// available during macro expansion. The expression is compiled and
// evaluated at compile time, and the result is stored in env.Expand().
//
// Unlike define-syntax (which stores macro transformers), define-for-syntax
// stores regular values with BindingTypeVariable.
func (p *CompileTimeContinuation) CompileDefineForSyntax(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if p.env == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-for-syntax: nil environment")
	}
	if p.template == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-for-syntax: nil template")
	}

	// expr is (name expr) or ((name args...) body...) - the args after 'define-for-syntax'
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(argsPair) {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "define-for-syntax: expected name and expression")
	}

	// Get the first element - either a symbol (simple define) or a pair (function define)
	first := argsPair.Car()
	if first == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-for-syntax: missing name")
	}

	// Get the rest (value expression or body)
	restVal := argsPair.Cdr()
	restPair, ok := restVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(restPair) {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "define-for-syntax: missing expression")
	}

	var nameSym *values.Symbol
	var valueExpr syntax.SyntaxValue

	// Check if it's a function definition: (define-for-syntax (name args...) body...)
	if firstPair, ok := first.(*syntax.SyntaxPair); ok {
		// Function shorthand - extract name and build lambda
		nameStx := firstPair.Car()
		nameSyntaxSym, ok := nameStx.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "define-for-syntax: function name must be a symbol")
		}
		nameSym = nameSyntaxSym.Unwrap().(*values.Symbol)

		// Build (lambda (args...) body...)
		params, ok := firstPair.Cdr().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "define-for-syntax: invalid parameter list")
		}
		lambdaSym := syntax.NewSyntaxSymbol("lambda", nameSyntaxSym.SourceContext())
		lambdaArgs := syntax.NewSyntaxCons(params, restPair, nameSyntaxSym.SourceContext())
		valueExpr = syntax.NewSyntaxCons(lambdaSym, lambdaArgs, nameSyntaxSym.SourceContext())
	} else {
		// Simple definition: (define-for-syntax name expr)
		nameSyntaxSym, ok := first.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "define-for-syntax: name must be a symbol")
		}
		nameSym = nameSyntaxSym.Unwrap().(*values.Symbol)

		// Get the value expression
		valueExpr, ok = restPair.Car().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "define-for-syntax: invalid expression")
		}
	}

	// Intern the symbol
	nameSym = p.env.InternSymbol(nameSym)

	// Compile the expression to a temporary template
	// Use the expand-phase environment so the expression can access other
	// define-for-syntax bindings and runtime primitives
	expandEnv := p.env.Expand()
	tmpTpl := NewNativeTemplate(0, 0, false)
	tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)

	// Expand the expression first (it may contain macros)
	ectx := NewExpandTimeCallContext()
	expander := NewExpanderTimeContinuation(p.env)
	expandedExpr, err := expander.ExpandExpression(ectx, valueExpr)
	if err != nil {
		return values.WrapForeignErrorf(err, "define-for-syntax: expansion failed")
	}

	// Compile the expanded expression
	if err := tmpCcnt.CompileExpression(ccnt, expandedExpr); err != nil {
		return values.WrapForeignErrorf(err, "define-for-syntax: compilation failed")
	}

	// Execute the compiled code at compile time
	cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
	mc := NewMachineContext(cont)
	if err := mc.Run(context.Background()); err != nil {
		if !errors.Is(err, ErrMachineHalt) {
			return values.WrapForeignErrorf(err, "define-for-syntax: evaluation failed")
		}
	}

	// Get the result
	result := mc.GetValue()

	// Store the result in the expand phase environment with BindingTypeVariable
	globalIndex, _ := expandEnv.MaybeCreateGlobalBinding(nameSym, environment.BindingTypeVariable)
	if globalIndex != nil {
		if err := expandEnv.SetGlobalValue(globalIndex, result); err != nil {
			return values.WrapForeignErrorf(err, "define-for-syntax: failed to store value")
		}
	}

	// define-for-syntax has no runtime effect - don't emit any operations
	// The caller expects us to emit something, so emit void
	return nil
}

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

// expander_let_syntax.go implements let-syntax and letrec-syntax expansion.
//
// Both forms are fully resolved during the expansion phase: macro bindings
// are compiled, the body is expanded in a child environment, and the
// let-syntax/letrec-syntax wrapper disappears from the output.
//
// Extracted from expander_time_continuation.go.

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// expandLetSyntax fully expands let-syntax during the expansion phase.
// Creates local macro bindings, expands the body, and returns the expanded result.
// The let-syntax wrapper disappears - only the expanded body remains.
//
// R7RS §4.3.1: let-syntax establishes local macro definitions visible only in the body.
func (p *ExpanderTimeContinuation) expandLetSyntax(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetSyntaxImpl(sym, expr, false)
}

// expandLetrecSyntax fully expands letrec-syntax during the expansion phase.
// Like let-syntax but transformers can reference each other (mutual recursion).
//
// R7RS §4.3.1: letrec-syntax is like let-syntax but with mutual visibility.
func (p *ExpanderTimeContinuation) expandLetrecSyntax(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetSyntaxImpl(sym, expr, true)
}

// expandLetSyntaxImpl implements both let-syntax and letrec-syntax expansion.
// The recursive parameter controls whether bindings can see each other.
//
// This function:
// 1. Creates a child expand environment with local macro bindings
// 2. Compiles each syntax-rules transformer
// 3. Expands body expressions with the child environment
// 4. Wraps in lambda if body contains defines (for scope isolation)
// 5. Returns the expanded body - the let-syntax wrapper disappears
func (p *ExpanderTimeContinuation) expandLetSyntaxImpl(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, recursive bool) (syntax.SyntaxValue, error) {
	formName := "let-syntax"
	if recursive {
		formName = "letrec-syntax"
	}
	sc := sym.SourceContext()

	// expr is (<bindings> <body>) - args after keyword
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(argsPair) {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected bindings and body", formName))
	}

	// Get the bindings list
	bindingsStx := argsPair.SyntaxCar()
	bindingsEmpty := syntax.IsSyntaxEmptyList(bindingsStx)
	var bindingsPair *syntax.SyntaxPair
	if !bindingsEmpty {
		var pairOk bool
		bindingsPair, pairOk = bindingsStx.(*syntax.SyntaxPair)
		if !pairOk {
			return nil, wrapSourcedError(bindingsStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected bindings list", formName))
		}
	}

	// Get the body
	bodyStx := argsPair.SyntaxCdr()
	bodyPair, ok := bodyStx.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected body expressions", formName))
	}

	// Count bindings for local environment allocation
	numBindings := 0
	var current *syntax.SyntaxPair
	if !bindingsEmpty {
		current = bindingsPair
		for !syntax.IsSyntaxEmptyList(current) {
			numBindings++
			cdr := current.SyntaxCdr()
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				break
			}
			current = nextPair
		}
	}

	// Create child expand environment for macro bindings.
	// Use p.env directly as the parent (not p.env.Expand()) to preserve
	// the environment chain for nested let-syntax. When we have:
	//   (let-syntax ((outer ...))
	//     (let-syntax ((inner ...)) ...))
	// The inner let-syntax's environment must have outer's environment
	// in its parent chain, not the global expand environment.
	localExpandEnv := environment.NewLocalEnvironment(numBindings)
	childExpandEnv := environment.NewEnvironmentFrameWithParent(localExpandEnv, p.env)

	// Create a rebinding scope for the let-syntax body.
	// Rebinding scopes indicate that auxiliary syntax could be shadowed.
	// This is used in literalScopesMatch to correctly reject shadowed literals.
	letScope := syntax.NewRebindingScopeWithLabel("let-syntax")

	// For letrec-syntax, pre-register all keywords so transformers can see each other
	if recursive && !bindingsEmpty {
		current = bindingsPair
		for !syntax.IsSyntaxEmptyList(current) {
			bindingStx := current.SyntaxCar()
			bindingPair, ok := bindingStx.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(bindingPair) {
				return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: invalid binding", formName))
			}
			keywordStx := bindingPair.SyntaxCar()
			keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
			if !ok {
				return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "%s: keyword must be a symbol", formName))
			}
			keyword := keywordSym.Unwrap().(*values.Symbol)
			// Create binding with letScope so free identifier resolution works
			_, _ = childExpandEnv.MaybeCreateLocalBinding(keyword, environment.BindingTypeSyntax, []*syntax.Scope{letScope}, keywordSym.SourceContext())

			cdr := current.SyntaxCdr()
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				break
			}
			current = nextPair
		}
	}

	// Compile each transformer and store in child expand environment
	if !bindingsEmpty {
		current = bindingsPair
	}
	for !bindingsEmpty && !syntax.IsSyntaxEmptyList(current) {
		bindingStx := current.SyntaxCar()
		bindingPair, ok := bindingStx.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(bindingPair) {
			return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: invalid binding", formName))
		}

		// Get keyword
		keywordStx := bindingPair.SyntaxCar()
		keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "%s: keyword must be a symbol", formName))
		}
		keyword := keywordSym.Unwrap().(*values.Symbol)

		// Get transformer expression
		transformerCdr := bindingPair.SyntaxCdr()
		transformerPair, ok := transformerCdr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(transformerPair) {
			return nil, wrapSourcedError(bindingPair.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: missing transformer expression", formName))
		}
		transformerExpr := transformerPair.SyntaxCar()

		// Compile the transformer (supports syntax-rules, lambda, er-macro-transformer)
		closure, err := compileTransformerToMachineClosure(p.ctx, p.env, transformerExpr, p.libraryScope, p.evaluator)
		if err != nil {
			return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(err, "%s: could not compile transformer for %s", formName, keyword.Key))
		}

		// Store in child expand environment with letScope for free identifier resolution
		localIndex, created := childExpandEnv.MaybeCreateLocalBinding(keyword, environment.BindingTypeSyntax, []*syntax.Scope{letScope}, keywordSym.SourceContext())
		if !created {
			localIndex = childExpandEnv.GetLocalIndex(keyword, nil)
		}
		if localIndex == nil {
			return nil, wrapSourcedError(keywordSym.SourceContext(), werr.WrapForeignErrorf(
				werr.ErrInvalidSyntax,
				"%s: failed to create binding for %s", formName, keyword.Key,
			))
		}
		err = childExpandEnv.SetLocalValue(localIndex, closure)
		if err != nil {
			return nil, wrapSourcedError(keywordSym.SourceContext(), werr.WrapForeignErrorf(err, "%s: failed to store transformer for %s", formName, keyword.Key))
		}

		cdr := current.SyntaxCdr()
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			break
		}
		current = nextPair
	}

	// Add the let-syntax scope to the body
	scopedBody := bodyPair.AddScope(letScope)
	scopedBodyPair, ok := scopedBody.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(bodyPair.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAList, "%s: body must be a list", formName))
	}

	// Create expander with child expand environment for body expansion
	childExpander := NewExpanderTimeContinuation(p.ctx, childExpandEnv, p.evaluator)

	// Expand all body expressions and check for defines
	var expandedExprs []syntax.SyntaxValue
	hasDefine := false
	current = scopedBodyPair
	for !syntax.IsSyntaxEmptyList(current) {
		expr := current.SyntaxCar()
		expandedExpr, err := childExpander.ExpandExpression(expr)
		if err != nil {
			return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "%s: failed to expand body expression", formName))
		}
		expandedExprs = append(expandedExprs, expandedExpr)
		if isSyntaxFormWithKeyword(expandedExpr, "define") {
			hasDefine = true
		}
		cdr := current.SyntaxCdr()
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok { //nolint:gocritic // ifElseChain: type assertion + value check, not a switch candidate
			current = nextPair
		} else if !syntax.IsSyntaxEmptyList(cdr) {
			return nil, wrapSourcedError(current.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAList, "%s: body must be a proper list", formName))
		} else {
			break
		}
	}

	// Build result: (begin body...) or ((lambda () (begin body...))) if has defines
	beginSym := syntax.NewSyntaxSymbol("begin", sc)
	beginBody := syntax.SyntaxList(sc, expandedExprs...)
	beginExpr := syntax.NewSyntaxCons(beginSym, beginBody, sc)

	if hasDefine {
		// Wrap in lambda to create new runtime scope for defines
		lambdaSym := syntax.NewSyntaxSymbol("lambda", sc)
		emptyArgs := syntax.SyntaxEmptyList
		lambdaExpr := syntax.SyntaxList(sc, lambdaSym, emptyArgs, beginExpr)
		return syntax.SyntaxList(sc, lambdaExpr), nil
	}

	return beginExpr, nil
}

// isSyntaxFormWithKeyword checks if expr is a syntax pair whose car is
// a syntax symbol with the given keyword.
func isSyntaxFormWithKeyword(expr syntax.SyntaxValue, keyword string) bool {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return false
	}
	sym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return false
	}
	return sym.Key() == keyword
}

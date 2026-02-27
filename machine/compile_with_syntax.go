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

package machine

import (
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// CompileWithSyntax compiles the (with-syntax ((pattern expr) ...) body ...) form.
//
// with-syntax is a convenience form for binding pattern variables from expressions.
// It's equivalent to:
//
//	(syntax-case (list expr ...) ()
//	  ((pattern ...) (let () body ...)))
//
// For now, this implements a simple transformation approach.
func (p *CompileTimeContinuation) CompileWithSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (already has keyword stripped).
	// So expr = (((pattern expr) ...) body ...)
	argsPair, err := formArgs(expr, "with-syntax", "bindings and body")
	if err != nil {
		return err
	}

	// Get the bindings list (CAR of args)
	bindingsList := argsPair.SyntaxCar()
	bindingsEmpty := syntax.IsSyntaxEmptyList(bindingsList)
	var bindingsPair *syntax.SyntaxPair
	if !bindingsEmpty {
		var ok bool
		bindingsPair, ok = bindingsList.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-syntax: bindings must be a list")
		}
	}

	// Get the body (CDR of args)
	bodyCdr := argsPair.SyntaxCdr()
	if values.IsEmptyList(bodyCdr) {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-syntax: expected body expressions")
	}
	bodyList, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-syntax: expected body expressions")
	}

	// Transform to syntax-case form
	srcCtx := argsPair.SourceContext()

	// If no bindings, just compile the body as a begin
	if bindingsEmpty {
		return p.compileWithSyntaxBody(ctctx, bodyList)
	}

	// Build: (syntax-case (list expr ...) () ((pattern ...) (begin body ...)))
	// First, collect patterns and expressions
	var patterns []syntax.SyntaxValue
	var exprs []syntax.SyntaxValue

	current := bindingsPair
	for !syntax.IsSyntaxEmptyList(current) {
		binding := current.SyntaxCar()
		bindingPair, ok := binding.(*syntax.SyntaxPair)
		if !ok || bindingPair.IsEmptyList() {
			return values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-syntax: each binding must be (pattern expr)")
		}

		// Get pattern (first element)
		pattern := bindingPair.SyntaxCar()
		patterns = append(patterns, pattern)

		// Get expr (second element)
		rest, ok := bindingPair.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok || rest.IsEmptyList() {
			return values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-syntax: each binding must be (pattern expr)")
		}
		expr = rest.SyntaxCar()
		exprs = append(exprs, expr)

		// Move to next binding
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		current, _ = cdr.(*syntax.SyntaxPair)
		if current == nil {
			break
		}
	}

	// Build (list expr ...)
	listElems := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("list", srcCtx)}
	listElems = append(listElems, exprs...)
	listExpr := p.buildQuasiquoteSyntaxList(srcCtx, listElems...)

	// Build (pattern ...)
	patternList := p.buildQuasiquoteSyntaxList(srcCtx, patterns...)

	// Build (begin body ...)
	bodyBegin := p.buildWithSyntaxBegin(srcCtx, bodyList)

	// Build the clause: ((pattern ...) (begin body ...))
	clause := p.buildQuasiquoteSyntaxList(srcCtx, patternList, bodyBegin)

	// Build the full syntax-case form:
	// (syntax-case (list expr ...) () ((pattern ...) (begin body ...)))
	syntaxCaseForm := p.buildQuasiquoteSyntaxList(srcCtx,
		syntax.NewSyntaxSymbol("syntax-case", srcCtx),
		listExpr,
		syntax.SyntaxEmptyList, // empty literals list
		clause,
	)

	// Compile the transformed form
	return p.CompileExpression(ctctx, syntaxCaseForm)
}

// compileWithSyntaxBody compiles the body of with-syntax when there are no bindings.
func (p *CompileTimeContinuation) compileWithSyntaxBody(ctctx CompileTimeCallContext, bodyList *syntax.SyntaxPair) error {
	if bodyList.IsEmptyList() {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-syntax: expected body expressions")
	}

	// Compile each body expression, the last one in tail position
	current := bodyList
	for !syntax.IsSyntaxEmptyList(current) {
		body := current.SyntaxCar()
		cdr := current.SyntaxCdr()
		isLast := syntax.IsSyntaxEmptyList(cdr)

		var exprCtx CompileTimeCallContext
		if isLast {
			exprCtx = ctctx
		} else {
			exprCtx = ctctx.NotInTail()
		}

		err := p.CompileExpression(exprCtx, body)
		if err != nil {
			return err
		}

		if !isLast {
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if ok {
				current = nextPair
			} else {
				break
			}
		} else {
			break
		}
	}

	return nil
}

// buildWithSyntaxBegin builds a (begin body ...) form from a list of expressions.
func (p *CompileTimeContinuation) buildWithSyntaxBegin(srcCtx *syntax.SourceContext, bodyList *syntax.SyntaxPair) syntax.SyntaxValue {
	elems := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("begin", srcCtx)}

	current := bodyList
	for !syntax.IsSyntaxEmptyList(current) {
		body := current.SyntaxCar()
		elems = append(elems, body)
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
		} else {
			break
		}
	}

	return p.buildQuasiquoteSyntaxList(srcCtx, elems...)
}

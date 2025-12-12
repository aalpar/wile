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
	"wile/syntax"
	"wile/values"
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
func (p *CompileTimeContinuation) CompileWithSyntax(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (already has keyword stripped).
	// So expr = (((pattern expr) ...) body ...)
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || argsPair.IsEmptyList() {
		return values.NewForeignError("with-syntax: expected bindings and body")
	}

	// Get the bindings list (CAR of args)
	bindingsList, ok := argsPair.Car().(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("with-syntax: expected bindings list")
	}

	bindingsPair, ok := bindingsList.(*syntax.SyntaxPair)
	if !ok {
		return values.NewForeignError("with-syntax: bindings must be a list")
	}

	// Get the body (CDR of args)
	bodyList, ok := argsPair.Cdr().(*syntax.SyntaxPair)
	if !ok || bodyList.IsEmptyList() {
		return values.NewForeignError("with-syntax: expected body expressions")
	}

	// Transform to syntax-case form
	srcCtx := argsPair.SourceContext()

	// If no bindings, just compile the body as a begin
	if bindingsPair.IsEmptyList() {
		return p.compileWithSyntaxBody(ccnt, bodyList)
	}

	// Build: (syntax-case (list expr ...) () ((pattern ...) (begin body ...)))
	// First, collect patterns and expressions
	var patterns []syntax.SyntaxValue
	var exprs []syntax.SyntaxValue

	current := bindingsPair
	for !values.IsEmptyList(current) {
		binding := current.Car()
		bindingPair, ok := binding.(*syntax.SyntaxPair)
		if !ok || bindingPair.IsEmptyList() {
			return values.NewForeignError("with-syntax: each binding must be (pattern expr)")
		}

		// Get pattern (first element)
		pattern, ok := bindingPair.Car().(syntax.SyntaxValue)
		if !ok {
			return values.NewForeignError("with-syntax: expected pattern")
		}
		patterns = append(patterns, pattern)

		// Get expr (second element)
		rest, ok := bindingPair.Cdr().(*syntax.SyntaxPair)
		if !ok || rest.IsEmptyList() {
			return values.NewForeignError("with-syntax: each binding must be (pattern expr)")
		}
		expr, ok := rest.Car().(syntax.SyntaxValue)
		if !ok {
			return values.NewForeignError("with-syntax: expected expression")
		}
		exprs = append(exprs, expr)

		// Move to next binding
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
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
		syntax.NewSyntaxEmptyList(srcCtx), // empty literals list
		clause,
	)

	// Compile the transformed form
	return p.CompileExpression(ccnt, syntaxCaseForm)
}

// compileWithSyntaxBody compiles the body of with-syntax when there are no bindings.
func (p *CompileTimeContinuation) compileWithSyntaxBody(ccnt CompileTimeCallContext, bodyList *syntax.SyntaxPair) error {
	if bodyList.IsEmptyList() {
		return values.NewForeignError("with-syntax: expected body expressions")
	}

	// Compile each body expression, the last one in tail position
	current := bodyList
	for !values.IsEmptyList(current) {
		body, ok := current.Car().(syntax.SyntaxValue)
		if !ok {
			return values.NewForeignError("with-syntax: expected expression in body")
		}

		cdr := current.Cdr()
		isLast := values.IsEmptyList(cdr)

		var ctx CompileTimeCallContext
		if isLast {
			ctx = ccnt
		} else {
			ctx = ccnt.NotInTail()
		}

		err := p.CompileExpression(ctx, body)
		if err != nil {
			return err
		}

		if !isLast {
			if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
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
	for !values.IsEmptyList(current) {
		if body, ok := current.Car().(syntax.SyntaxValue); ok {
			elems = append(elems, body)
		}

		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	return p.buildQuasiquoteSyntaxList(srcCtx, elems...)
}

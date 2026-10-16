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
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileWithSyntax compiles the (with-syntax ((pattern expr) ...) body ...) form.
//
// with-syntax is a convenience form for binding pattern variables from expressions.
// It's equivalent to:
//
//	(syntax-case (list expr ...) ()
//	  ((pattern ...) (begin body ...)))
//
// For now, this implements a simple transformation approach.
func (p *CompileTimeContinuation) CompileWithSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	goSyntaxFormCompiles.Add(1)
	// expr is the CDR of the form (keyword stripped by syntaxCompiler in register.go).
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
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-syntax: bindings must be a list"))
		}
	}

	// Get the body (CDR of args)
	bodyCdr := argsPair.SyntaxCdr()
	if values.IsEmptyList(bodyCdr) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-syntax: expected body expressions"))
	}
	bodyList, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-syntax: expected body expressions"))
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
		// Exactly (pattern expr): an extra element or an improper tail is an
		// error, not ignored, since the expander pass left this form unexpanded.
		bindingPair, ok := binding.(*syntax.SyntaxPair)
		if !ok || !hasSyntaxArity(bindingPair, 2) {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-syntax: each binding must be (pattern expr)"))
		}
		patterns = append(patterns, bindingPair.SyntaxCar())
		exprs = append(exprs, bindingPair.SyntaxCdr().(*syntax.SyntaxPair).SyntaxCar())

		// Move to next binding
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		current, ok = cdr.(*syntax.SyntaxPair)
		if !ok {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-syntax: bindings must be a proper list"))
		}
	}

	// Build (list expr ...)
	listElems := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("list", srcCtx)}
	listElems = append(listElems, exprs...)
	listExpr := buildQuasiSyntaxList(srcCtx, listElems...)

	// Build (pattern ...)
	patternList := buildQuasiSyntaxList(srcCtx, patterns...)

	// Build (begin body ...)
	bodyBegin := p.buildWithSyntaxBegin(srcCtx, bodyList)

	// Build the clause: ((pattern ...) (begin body ...))
	clause := buildQuasiSyntaxList(srcCtx, patternList, bodyBegin)

	// The operands of (syntax-case (list expr ...) () ((pattern ...) (begin body ...))),
	// compiled directly rather than as a synthesized syntax-case form: the expander
	// pass has already expanded each expr (expandWithSyntaxForm), and re-expanding
	// the input would resolve the scopeless list head at the use site, where a user
	// (define-syntax list …) captures it. That protects against a macro only: the
	// head is still unpinned, so a user variable named list captures it at compile
	// time (TODO.md).
	syntaxCaseArgs := buildQuasiSyntaxList(srcCtx,
		listExpr,
		syntax.SyntaxEmptyList, // empty literals list
		clause,
	)
	return p.CompileSyntaxCase(ctctx, syntaxCaseArgs)
}

// compileWithSyntaxBody compiles the body of with-syntax when there are no bindings.
func (p *CompileTimeContinuation) compileWithSyntaxBody(ctctx CompileTimeCallContext, bodyList *syntax.SyntaxPair) error {
	if bodyList.IsEmptyList() {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-syntax: expected body expressions"))
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

		if isLast {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			break
		}
		current = nextPair
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

	return buildQuasiSyntaxList(srcCtx, elems...)
}

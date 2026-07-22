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

// expander_lambda.go implements expand-time handling of lambda and
// case-lambda forms, plus shared helpers for formal parameter extraction
// and body expression collection.
//
// Extracted from expander_time_continuation.go.

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// expandLambdaForm expands (lambda (args...) body...)
//
// R7RS §4.2.2: Lambda parameters shadow outer bindings including macros and
// primitive forms. This function creates a child environment with the formals
// as local variable bindings before expanding the body, ensuring that references
// to parameter names (like `if`, `let`) don't get treated as special forms.
//
// R7RS §4.3.2: Auxiliary syntax hygiene. Lambda adds a scope to both formals
// and body BEFORE expanding inner macros. This ensures that identifiers in the
// body (like `=>` in a `cond`) carry the lambda's scope, enabling correct
// `free-identifier=?` comparisons during macro pattern matching.
//
// R7RS §5.3: Internal define-syntax forms are processed before expanding the
// rest of the body, so locally-defined macros are visible to subsequent forms.
func (p *ExpanderTimeContinuation) expandLambdaForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	formals := pair.SyntaxCar()
	cdrVal := pair.SyntaxCdr()
	cdrPair, ok := cdrVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	formalsStx, expandedBody, err := p.expandProcedureBody("lambda", formals, cdrPair, sym.SourceContext())
	if err != nil {
		return nil, wrapSourcedError(expr.SourceContext(), err)
	}

	// Build (lambda formals expanded-body...)
	args := syntax.NewSyntaxCons(formalsStx, expandedBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandProcedureBody performs the shared expand-time handling for a procedure
// body — the body of a lambda, and of the function-shorthand define
// (define (name . formals) body...), which is sugar for
// (define name (lambda formals body...)). It mints a fresh binding scope, adds
// it to the formals and the body BEFORE inner expansion, binds the formals in a
// child expand environment, and expands the body with internal define-syntax
// processing (R7RS §5.3 letrec* semantics).
//
// Both callers must run this identically: the scope is what upholds
// CompileSymbol's fast-path invariant (a symbol inside a local binding context
// always carries a scope — see compile_time_continuation.go), and the
// ExpandBodyWithDefineSyntax pass is what makes internal macros visible to
// later body forms. The shorthand define previously skipped both (it expanded
// its body as a flat argument list), so an internal define-syntax was invisible
// and raised "no such local or global binding" when used.
//
// formName ("lambda"/"define") only tunes the error prefix so each caller keeps
// its own diagnostics. It returns the scoped formals and the rebuilt body; the
// caller adds the surrounding source context.
func (p *ExpanderTimeContinuation) expandProcedureBody(
	formName string,
	formals syntax.SyntaxValue,
	body *syntax.SyntaxPair,
	src *syntax.SourceContext,
) (formalsStx syntax.SyntaxValue, expandedBody syntax.SyntaxValue, err error) {
	// This scope is added to both formals and body BEFORE any inner expansion,
	// ensuring that pattern matching in inner macros (like cond) can correctly
	// detect when identifiers (like =>) have been bound by this procedure.
	scope := syntax.NewScopeWithLabel("lambda")

	formalsStx = syntax.AddScopeToSyntax(formals, scope)
	bodyWithScope := body.AddScope(scope).(*syntax.SyntaxPair)

	// Extract formal parameter symbols (now with the body scope included) and
	// bind them in a child expand environment, so lookups during body expansion
	// find them.
	formalSyms := extractFormalSymbols(formalsStx)
	childEnv := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0),
		p.env,
	)
	for _, fs := range formalSyms {
		childEnv.MaybeCreateLocalBinding(fs.sym, environment.BindingTypeVariable, fs.scopes, fs.source)
	}

	bodyExprs, err := collectBodyExpressions(bodyWithScope)
	if err != nil {
		return nil, nil, werr.WrapForeignErrorf(err, "%s: invalid body expression", formName)
	}

	// Handle the case where body is wrapped in (begin ...) - common from let macro
	unwrappedExprs, wasBeginWrapped, err := unwrapBeginBodyWithFlag(bodyExprs)
	if err != nil {
		return nil, nil, werr.WrapForeignErrorf(err, "%s: invalid body", formName)
	}

	// Expand body in the child environment, compiling define-syntax as encountered
	childExpander := p.newChildExpander(childEnv)
	expandedExprs, err := childExpander.ExpandBodyWithDefineSyntax(unwrappedExprs)
	if err != nil {
		return nil, nil, werr.WrapForeignErrorf(err, "%s: failed to expand body", formName)
	}

	// Rebuild the body as a syntax list, re-wrapping in begin when it arrived so.
	if wasBeginWrapped {
		beginSym := syntax.NewSyntaxSymbol("begin", src)
		innerList := syntax.SyntaxList(src, expandedExprs...)
		beginForm := syntax.NewSyntaxCons(beginSym, innerList, src)
		expandedBody = syntax.SyntaxList(src, beginForm)
	} else {
		expandedBody = syntax.SyntaxList(src, expandedExprs...)
	}

	return formalsStx, expandedBody, nil
}

// collectBodyExpressions collects all expressions from a body syntax pair into a slice.
// Used by expandLambdaForm (this file) and expandBeginForm (expander_primitive_forms.go).
func collectBodyExpressions(body *syntax.SyntaxPair) ([]syntax.SyntaxValue, error) {
	var exprs []syntax.SyntaxValue
	current := body
	for !syntax.IsSyntaxEmptyList(current) {
		exprs = append(exprs, current.SyntaxCar())
		cdr := current.SyntaxCdr()
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok { //nolint:gocritic // ifElseChain: type assertion + value check, not a switch candidate
			current = nextPair
		} else if syntax.IsSyntaxEmptyList(cdr) {
			break
		} else {
			return nil, wrapSourcedError(body.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAList, "body must be a proper list"))
		}
	}
	return exprs, nil
}

// unwrapBeginBodyWithFlag handles the case where a lambda body is a single (begin ...) form.
// This is common from let macro expansion: (let () body...) -> ((lambda () (begin body...)))
// Returns the contents of the begin, a flag indicating if unwrapping occurred, and any error.
func unwrapBeginBodyWithFlag(exprs []syntax.SyntaxValue) ([]syntax.SyntaxValue, bool, error) {
	if len(exprs) != 1 {
		return exprs, false, nil
	}
	pair, ok := asSyntaxFormWithKeyword(exprs[0], "begin")
	if !ok {
		return exprs, false, nil
	}
	cdrPair, ok := pair.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return exprs, false, nil
	}
	innerExprs, err := collectBodyExpressions(cdrPair)
	if err != nil {
		return nil, false, wrapSourcedError(cdrPair.SourceContext(), werr.WrapForeignErrorf(err, "begin: malformed body"))
	}
	return innerExprs, true, nil
}

// formalSymbol pairs a symbol with its scopes for formal parameter tracking.
type formalSymbol struct {
	sym    *values.Symbol
	scopes []*syntax.Scope
	source *syntax.SourceContext
}

// extractFormalSymbols extracts symbols from a lambda formals expression.
// Handles proper lists (x y z), improper lists (x y . rest), and single symbols (args).
func extractFormalSymbols(formals syntax.SyntaxValue) []formalSymbol {
	var result []formalSymbol

	switch f := formals.(type) {
	case *syntax.SyntaxSymbol:
		// Rest argument: (lambda args body...)
		result = append(result, formalSymbol{f.Sym, f.Scopes(), f.SourceContext()})
	case *syntax.SyntaxPair:
		// List of arguments: (lambda (x y z) body...) or (lambda (x y . rest) body...)
		current := f
		for !syntax.IsSyntaxEmptyList(current) {
			car := current.SyntaxCar()
			sym, ok := car.(*syntax.SyntaxSymbol)
			if ok {
				result = append(result, formalSymbol{sym.Sym, sym.Scopes(), sym.SourceContext()})
			}
			cdr := current.SyntaxCdr()
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if ok {
				current = nextPair
			} else {
				sym, ok := cdr.(*syntax.SyntaxSymbol)
				if ok {
					// Improper list: (x y . rest)
					result = append(result, formalSymbol{sym.Sym, sym.Scopes(), sym.SourceContext()})
				}
				break
			}
		}
	}

	return result
}

// extractIdentifierList extracts SyntaxSymbols from an identifier list.
// This is used by with-binding-scope to get the bound identifiers.
// Returns the symbols with their scopes preserved.
func extractIdentifierList(idList syntax.SyntaxValue) []*syntax.SyntaxSymbol {
	var result []*syntax.SyntaxSymbol

	pair, ok := idList.(*syntax.SyntaxPair)
	if !ok {
		// Single identifier or empty
		sym, ok := idList.(*syntax.SyntaxSymbol)
		if ok {
			return []*syntax.SyntaxSymbol{sym}
		}
		return nil
	}

	// List of identifiers
	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		sym, ok := car.(*syntax.SyntaxSymbol)
		if ok {
			result = append(result, sym)
		}
		cdr := current.SyntaxCdr()
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
		} else {
			break
		}
	}

	return result
}

// expandCaseLambdaForm expands (case-lambda (formals body...) ...)
func (p *ExpanderTimeContinuation) expandCaseLambdaForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Expand each clause
	var expandedClauses []syntax.SyntaxValue
	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		clauseVal := current.SyntaxCar()
		clauseStx := clauseVal
		// Each clause is (formals body...)
		clausePair, ok := clauseStx.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(clausePair) {
			// Keep malformed clauses as-is, let validator report errors
			expandedClauses = append(expandedClauses, clauseStx)
		} else {
			// Keep formals unchanged
			formals := clausePair.SyntaxCar()
			formalsStx := formals
			// Expand body
			cdrVal := clausePair.SyntaxCdr()
			cdrPair, ok := cdrVal.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
				// No body - keep clause as-is
				expandedClauses = append(expandedClauses, clauseStx)
			} else {
				expandedBody, err := p.ExpandSyntaxArgumentList(cdrPair)
				if err != nil {
					return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "case-lambda: failed to expand clause body"))
				}

				// Build (formals expanded-body...)
				expandedClause := syntax.NewSyntaxCons(formalsStx, expandedBody, clausePair.SourceContext())
				expandedClauses = append(expandedClauses, expandedClause)
			}
		}

		cdr := current.SyntaxCdr()
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
		} else {
			break
		}
	}

	// Build (case-lambda expanded-clauses...)
	clauseList := syntax.SyntaxList(sym.SourceContext(), expandedClauses...)
	return syntax.NewSyntaxCons(sym, clauseList, sym.SourceContext()), nil
}

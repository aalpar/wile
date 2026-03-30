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

// expander_lambda.go implements expand-time handling of lambda and
// case-lambda forms, plus shared helpers for formal parameter extraction
// and body expression collection.
//
// Extracted from expander_time_continuation.go.

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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

	// Create a scope for this lambda's bindings.
	// This scope is added to both formals and body BEFORE any inner expansion,
	// ensuring that pattern matching in inner macros (like cond) can correctly
	// detect when identifiers (like =>) have been bound by this lambda.
	//
	// This also maintains the compiler's fast-path invariant: every symbol inside
	// a local binding context has at least one scope (lambdaScope), so symbols
	// with empty scopes can safely skip scope-aware resolution. See CompileSymbol.
	lambdaScope := syntax.NewScopeWithLabel("lambda")

	// Add lambda scope to formals and body
	formalsStx := syntax.AddScopeToSyntax(formals, lambdaScope)
	bodyWithScope := cdrPair.AddScope(lambdaScope).(*syntax.SyntaxPair)

	// Extract formal parameter symbols (now with lambda scope included)
	formalSyms := extractFormalSymbols(formalsStx)

	// Create a child environment with the formals as local variable bindings.
	// The bindings include the lambda scope, so lookups will find them.
	childEnv := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0),
		p.env,
	)
	for _, fs := range formalSyms {
		childEnv.MaybeCreateLocalBindingWithScopes(fs.sym, environment.BindingTypeVariable, fs.scopes, fs.source)
	}

	// R7RS §5.3: Process define-syntax forms before expanding subsequent expressions
	// This makes locally-defined macros visible to later body expressions
	bodyExprs, err := collectBodyExpressions(bodyWithScope)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "lambda: invalid body expression")
	}

	// Handle the case where body is wrapped in (begin ...) - common from let macro
	unwrappedExprs, wasBeginWrapped := unwrapBeginBodyWithFlag(bodyExprs)

	// Expand body in the child environment, compiling define-syntax as encountered
	childExpander := NewExpanderTimeContinuation(p.ctx, childEnv, p.evaluator)
	expandedExprs, err := childExpander.ExpandBodyWithDefineSyntax(unwrappedExprs)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "lambda: failed to expand body")
	}

	// Rebuild the body as a syntax list
	var expandedBody syntax.SyntaxValue
	if wasBeginWrapped {
		// Re-wrap in begin
		beginSym := syntax.NewSyntaxSymbol("begin", sym.SourceContext())
		innerList := syntax.SyntaxList(sym.SourceContext(), expandedExprs...)
		beginForm := syntax.NewSyntaxCons(beginSym, innerList, sym.SourceContext())
		expandedBody = syntax.SyntaxList(sym.SourceContext(), beginForm)
	} else {
		expandedBody = syntax.SyntaxList(sym.SourceContext(), expandedExprs...)
	}

	// Build (lambda formals expanded-body...)
	args := syntax.NewSyntaxCons(formalsStx, expandedBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
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
			return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "body must be a proper list")
		}
	}
	return exprs, nil
}

// unwrapBeginBodyWithFlag handles the case where a lambda body is a single (begin ...) form.
// This is common from let macro expansion: (let () body...) -> ((lambda () (begin body...)))
// Returns the contents of the begin and a flag indicating if unwrapping occurred.
func unwrapBeginBodyWithFlag(exprs []syntax.SyntaxValue) ([]syntax.SyntaxValue, bool) {
	if len(exprs) != 1 {
		return exprs, false
	}
	pair, ok := exprs[0].(*syntax.SyntaxPair)
	if !ok {
		return exprs, false
	}
	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return exprs, false
	}
	sym, ok := carSym.Unwrap().(*values.Symbol)
	if !ok || sym.Key != "begin" {
		return exprs, false
	}
	// It's (begin ...), extract the contents
	cdr := pair.Cdr()
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok {
		return exprs, false
	}
	innerExprs, err := collectBodyExpressions(cdrPair)
	if err != nil {
		return exprs, false
	}
	return innerExprs, true
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
					return nil, werr.WrapForeignErrorf(err, "case-lambda: failed to expand clause body")
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

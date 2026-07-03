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

// expander_primitive_forms.go implements expand-time handlers for primitive
// special forms: quote-family (via expandUnchanged), if, begin, set!, define,
// import, with-binding-scope, syntax-error, dynamic-wind, and
// with-continuation-mark.
//
// Each handler is registered in primitive_expanders_registry.go and invoked
// by ExpandSyntaxExpression when the expander encounters a primitive keyword.
//
// Extracted from expander_time_continuation.go.

import (
	"context"
	"slices"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// formUnchanged rebuilds the primitive form (sym . expr) without expanding its
// body. It serves two callers: the quote-family passthrough expanders, and the
// arity guards that hand a malformed form to the validator for error reporting
// rather than raising during expansion.
func formUnchanged(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// asNonEmptyPair narrows v to a non-empty *SyntaxPair. It reports ok=false for
// both a non-pair and the empty list — the two conditions every arity guard in
// this file tests together before bailing.
func asNonEmptyPair(v syntax.SyntaxValue) (*syntax.SyntaxPair, bool) {
	pair, ok := v.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return nil, false
	}
	return pair, true
}

// expandSub expands a single sub-expression, wrapping any failure with the
// form's source context and the caller-supplied "<form>: failed to expand
// <part>" message so the error chain names both the location and the part.
func (p *ExpanderTimeContinuation) expandSub(expr, sub syntax.SyntaxValue, what string) (syntax.SyntaxValue, error) {
	expanded, err := p.ExpandExpression(sub)
	if err != nil {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "%s", what))
	}
	return expanded, nil
}

// expandUnchanged returns the form unchanged. Used for forms whose subexpressions
// are processed at a later stage (compile time) or should not be expanded (quote).
//
// Forms using this expander:
//   - quote: Content is literal data, not code
//   - define-syntax: Transformer is compiled, not expanded
//   - quasiquote: Has special expansion rules handled at compile time
//   - unquote, unquote-splicing: Only valid inside quasiquote
//   - include, include-ci: Files are read at compile time
//   - cond-expand: Feature expressions use special syntax, not macros
//   - syntax, syntax-case, quasisyntax, unsyntax, unsyntax-splicing, with-syntax:
//     Compile-time forms handled during compilation
func (*ExpanderTimeContinuation) expandUnchanged(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return formUnchanged(sym, expr)
}

// expandWithBindingScope implements the (with-binding-scope (id ...) body) form.
//
// This primitive expander creates a fresh "binding scope" and adds it to the entire
// body. This is essential for hygienic macro expansion of binding forms like `let`.
//
// When a binding form (let, letrec, etc.) expands, it wraps its output in
// with-binding-scope. The expander then:
//  1. Creates a fresh scope S
//  2. Adds S to the entire body (binding sites AND references)
//  3. Returns the scoped body for further expansion
//
// This ensures that:
//   - Each let form creates a unique scope
//   - Binding identifiers and their references share that scope
//   - Nested lets have different scopes, enabling hygiene
//
// Example:
//
//	(let ((x 1)) (+ x 1))
//	→ macro expands to: (with-binding-scope (x) ((lambda (x) (+ x 1)) 1))
//	→ expander adds scope S to body: ((lambda (x+S) (+ x+S 1)) 1)
//	→ returns: ((lambda (x+S) (+ x+S 1)) 1)
//
// The identifier list (x) is currently unused but reserved for future use
// (e.g., selective scope application or debugging).
func (p *ExpanderTimeContinuation) expandWithBindingScope(_ *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// expr is the cdr of (with-binding-scope (id ...) body)
	// which is ((id ...) body)
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-binding-scope: expected (with-binding-scope (id ...) body)"))
	}

	// Get the identifier list - these are the identifiers being bound
	idListStx := pair.SyntaxCar()

	// Get the body
	bodyPair, ok := asNonEmptyPair(pair.SyntaxCdr())
	if !ok {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-binding-scope: missing body"))
	}
	body := bodyPair.SyntaxCar()

	// Create a fresh binding scope
	bindingScope := syntax.NewScopeWithLabel("binding")

	// Add the scope to the entire body
	// This adds the scope to ALL identifiers in the body, including:
	// - Lambda parameters (binding sites)
	// - References to those parameters in the lambda body
	// - Any other identifiers
	scopedBody := syntax.AddScopeToSyntax(body, bindingScope)

	// Extract bound identifiers and create placeholder bindings for them.
	// This is critical for R7RS §4.3.2 auxiliary syntax hygiene: when a macro
	// like cond checks if => has been bound, it needs to find these placeholder
	// bindings in the expand-time environment.
	boundIds := extractIdentifierList(idListStx)
	if len(boundIds) > 0 {
		// Create a child expand environment with placeholder bindings
		localExpandEnv := environment.NewLocalEnvironment(len(boundIds))
		childExpandEnv := environment.NewEnvironmentFrameWithParent(localExpandEnv, p.env)

		// Add placeholder bindings for each bound identifier.
		// The scopes include the binding scope we just created.
		for _, id := range boundIds {
			// Get the identifier's current scopes and add the binding scope
			idScopes := id.Scopes()
			newScopes := make([]*syntax.Scope, len(idScopes)+1)
			copy(newScopes, idScopes)
			newScopes[len(idScopes)] = bindingScope

			sym := id.Sym
			childExpandEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, newScopes, id.SourceContext())
		}

		// Continue expansion with the child environment
		childExpander := p.newChildExpander(childExpandEnv)
		return childExpander.ExpandExpression(scopedBody)
	}

	// No bound identifiers - just continue with current environment
	return p.ExpandExpression(scopedBody)
}

// expandSyntaxError handles the (syntax-error message arg ...) form.
// R7RS §4.3.1: syntax-error signals a compile-time error during macro expansion.
// When encountered, it raises a compilation error with the given message and
// arguments formatted as irritants.
//
// This allows macro authors to provide meaningful error messages for invalid uses:
//
//	(define-syntax must-be-pair
//	  (syntax-rules ()
//	    ((must-be-pair (a . b)) 'ok)
//	    ((must-be-pair x) (syntax-error "expected a pair" x))))
func (p *ExpanderTimeContinuation) expandSyntaxError(_ *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// Extract message (required first argument)
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-error: missing message argument"))
	}

	// Get the message
	msgVal := pair.SyntaxCar()
	var message string
	switch m := msgVal.(type) {
	case *syntax.SyntaxObject:
		str, ok := m.Unwrap().(*values.String)
		if ok {
			message = str.Value
		} else {
			message = m.Unwrap().SchemeString()
		}
	case *syntax.SyntaxSymbol:
		message = m.Key()
	default:
		message = msgVal.SchemeString()
	}

	// Collect irritants (remaining arguments)
	var irritants []string
	rest := pair.SyntaxCdr()
	for {
		restPair, ok := rest.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(restPair) {
			break
		}
		irritant := restPair.SyntaxCar()
		irritants = append(irritants, irritant.SchemeString())
		rest = restPair.SyntaxCdr()
	}

	// Format the error message
	if len(irritants) > 0 {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-error: %s: %s", message, formatIrritants(irritants)))
	}
	return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-error: %s", message))
}

// formatIrritants joins irritants with commas for error display.
func formatIrritants(irritants []string) string {
	return strings.Join(irritants, ", ")
}

// expandBeginForm expands (begin expr ...) by expanding all subexpressions.
// Uses ExpandBodyWithDefineSyntax to compile define-syntax forms immediately,
// ensuring macros defined in begin are available to subsequent forms.
func (p *ExpanderTimeContinuation) expandBeginForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	exprPair, ok := asNonEmptyPair(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}

	// Collect forms from the begin body
	forms, err := collectBodyExpressions(exprPair)
	if err != nil {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "failed to collect begin body"))
	}

	// Use ExpandBodyWithDefineSyntax to compile define-syntax forms immediately
	// This ensures macros defined in begin are available to subsequent forms
	expandedForms, err := p.ExpandBodyWithDefineSyntax(forms)
	if err != nil {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "failed to expand begin body"))
	}

	// Rebuild the begin form with expanded contents
	expandedArgs := syntax.SyntaxEmptyList
	for i := range slices.Backward(expandedForms) {
		expandedArgs = syntax.NewSyntaxCons(expandedForms[i], expandedArgs, sym.SourceContext())
	}
	return syntax.NewSyntaxCons(sym, expandedArgs, sym.SourceContext()), nil
}

// expandIfForm expands (if test consequent [alternative])
func (p *ExpanderTimeContinuation) expandIfForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}

	expandedTest, err := p.expandSub(expr, pair.SyntaxCar(), "if: failed to expand test")
	if err != nil {
		return nil, err
	}

	// Get consequent. Bail (return the form unchanged) when it is missing so the
	// validator reports the arity error — consistent with set!/define/begin/lambda.
	cdrPair, ok := asNonEmptyPair(pair.SyntaxCdr())
	if !ok {
		return formUnchanged(sym, expr)
	}

	expandedConseq, err := p.expandSub(expr, cdrPair.SyntaxCar(), "if: failed to expand consequent")
	if err != nil {
		return nil, err
	}

	// Check for alternative
	altPair, ok := asNonEmptyPair(cdrPair.SyntaxCdr())
	if !ok {
		// No alternative - (if test conseq)
		args := syntax.SyntaxList(sym.SourceContext(), expandedTest, expandedConseq)
		return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
	}

	expandedAlt, err := p.expandSub(expr, altPair.SyntaxCar(), "if: failed to expand alternative")
	if err != nil {
		return nil, err
	}

	// Bail (return the form unchanged) on extra trailing args so the validator
	// reports the over-arity error — symmetric with the under-arity bail above.
	_, hasExtra := asNonEmptyPair(altPair.SyntaxCdr())
	if hasExtra {
		return formUnchanged(sym, expr)
	}

	// Build (if test conseq alt)
	args := syntax.SyntaxList(sym.SourceContext(), expandedTest, expandedConseq, expandedAlt)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandSetForm expands (set! var value)
func (p *ExpanderTimeContinuation) expandSetForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}

	// Keep variable unchanged
	varExpr := pair.SyntaxCar()

	// Expand value
	cdrPair, ok := asNonEmptyPair(pair.SyntaxCdr())
	if !ok {
		return formUnchanged(sym, expr)
	}

	expandedValue, err := p.expandSub(expr, cdrPair.SyntaxCar(), "set!: failed to expand value")
	if err != nil {
		return nil, err
	}

	// Bail (return the form unchanged) on extra trailing args so the validator
	// reports the over-arity error — symmetric with the under-arity bail above.
	_, hasExtra := asNonEmptyPair(cdrPair.SyntaxCdr())
	if hasExtra {
		return formUnchanged(sym, expr)
	}

	// Build (set! var expanded-value)
	args := syntax.SyntaxList(sym.SourceContext(), varExpr, expandedValue)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandDefineForm expands (define var value) or (define (name . args) body ...)
func (p *ExpanderTimeContinuation) expandDefineForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}

	first := pair.SyntaxCar()
	cdrPair, ok := asNonEmptyPair(pair.SyntaxCdr())
	if !ok {
		return formUnchanged(sym, expr)
	}

	// Check if it's a function definition (define (name args...) body...)
	_, isSymbol := first.(*syntax.SyntaxSymbol)
	if !isSymbol {
		// Function definition - first is (name args...)
		// Expand the body expressions
		expandedBody, err := p.ExpandSyntaxArgumentList(cdrPair)
		if err != nil {
			return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "define: failed to expand body"))
		}
		args := syntax.NewSyntaxCons(first, expandedBody, sym.SourceContext())
		return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
	}

	// Simple definition (define var value)
	expandedValue, err := p.expandSub(expr, cdrPair.SyntaxCar(), "define: failed to expand value")
	if err != nil {
		return nil, err
	}

	// Build (define var expanded-value)
	args := syntax.SyntaxList(sym.SourceContext(), first, expandedValue)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandImportForm processes (import <import-set> ...) during expansion.
// This loads libraries and makes their bindings (including macros) available
// for subsequent forms. Without this, imported macros wouldn't be recognized
// during expansion since imports were only processed during compilation.
//
// R7RS §5.2: Import declarations must be processed before expressions that
// use the imported bindings. For macros, this means processing during expansion.
func (p *ExpanderTimeContinuation) expandImportForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// expr is the arguments after 'import': (<import-set> ...)
	if syntax.IsSyntaxEmptyList(expr) {
		// Empty import is valid, return unchanged
		return formUnchanged(sym, expr)
	}

	importSets, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "import: expected list of import sets"))
	}

	// Process each import set to load libraries and copy bindings
	_, err := syntax.SyntaxForEach(p.ctx, importSets, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		return ResolveAndInstallImportSet(ctx, importSetExpr.UnwrapAll(), p.env, environment.PhaseExpand, p.evaluator)
	})

	if err != nil {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "import: failed to resolve import set"))
	}

	// Return the import form unchanged for later compilation
	return formUnchanged(sym, expr)
}

// expandWithContinuationMarkForm expands all three sub-expressions of
// (with-continuation-mark key val body).
func (p *ExpanderTimeContinuation) expandWithContinuationMarkForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}

	expandedKey, err := p.expandSub(expr, pair.SyntaxCar(), "with-continuation-mark: failed to expand key")
	if err != nil {
		return nil, err
	}

	// Get val pair. Bail when missing so the validator reports the arity error.
	valPair, ok := asNonEmptyPair(pair.SyntaxCdr())
	if !ok {
		return formUnchanged(sym, expr)
	}
	expandedVal, err := p.expandSub(expr, valPair.SyntaxCar(), "with-continuation-mark: failed to expand value")
	if err != nil {
		return nil, err
	}

	// Get body pair. Bail when missing so the validator reports the arity error.
	bodyPair, ok := asNonEmptyPair(valPair.SyntaxCdr())
	if !ok {
		return formUnchanged(sym, expr)
	}
	expandedBody, err := p.expandSub(expr, bodyPair.SyntaxCar(), "with-continuation-mark: failed to expand body")
	if err != nil {
		return nil, err
	}

	// Bail (return the form unchanged) on extra trailing args so the validator
	// reports the over-arity error — symmetric with the under-arity bails above.
	_, hasExtra := asNonEmptyPair(bodyPair.SyntaxCdr())
	if hasExtra {
		return formUnchanged(sym, expr)
	}

	args := syntax.SyntaxList(sym.SourceContext(), expandedKey, expandedVal, expandedBody)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

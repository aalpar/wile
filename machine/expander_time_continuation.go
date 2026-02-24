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

// expander_time_continuation.go implements macro expansion for syntax-rules.
//
// The expander runs after parsing and before compilation. It walks the syntax
// tree looking for macro invocations and expands them to their definitions.
//
// Expansion Process:
//   1. For each expression, check if it's a macro invocation
//   2. If yes, invoke the transformer closure (OperationSyntaxRulesTransform)
//   3. The transformer returns the expanded syntax
//   4. Recursively expand the result (macros can expand to other macro calls)
//   5. Return the fully expanded syntax tree to the compiler
//
// Macro Detection:
//   When ExpandSyntaxExpression sees a symbol, it checks the environment
//   for a binding with BindingTypeSyntax. If found, the binding's value
//   is a MachineClosure (the compiled transformer), which is invoked.
//
// The expander is separate from the compiler because:
//   - Macros must be expanded before compiling (they change the syntax)
//   - Expansion may need to run compiled code (the transformer)
//   - Hygiene scopes are added during expansion, not compilation
//
// Reference: R7RS Section 4.3 (Macros)

import (
	"context"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// ExpanderTimeContinuation is a continuation used during the expansion phase.
//
// It walks the syntax tree, detecting and expanding macro invocations.
// The env field provides access to macro definitions (BindingTypeSyntax bindings).
type ExpanderTimeContinuation struct {
	ctx context.Context
	env *environment.EnvironmentFrame
}

// NewExpanderTimeContinuation creates a new ExpanderTimeContinuation.
func NewExpanderTimeContinuation(ctx context.Context, env *environment.EnvironmentFrame) *ExpanderTimeContinuation {
	q := &ExpanderTimeContinuation{
		ctx: ctx,
		env: env,
	}
	return q
}

// Context returns the context associated with this expander continuation.
func (p *ExpanderTimeContinuation) Context() context.Context {
	return p.ctx
}

// hasLocalVariableBinding checks if the symbol has a local variable binding
// in the runtime environment that would shadow any macro definition.
// R7RS §4.2.2: let bindings shadow outer bindings including macros.
//
// Both this path and the compiler's CompileSymbolReference (compile_time_continuation.go)
// check bindingScopes ⊆ useScopes via syntax.ScopesMatch. This path checks a single
// binding for shadowing; the compiler uses the environment's maximality algorithm
// (GetLocalIndexWithScopes) to select the most specific binding for codegen dispatch.
func (p *ExpanderTimeContinuation) hasLocalVariableBinding(sym *values.Symbol, scopes []*syntax.Scope) bool {
	// Only check local bindings - global variables don't shadow macros
	li := p.env.GetLocalIndex(sym)
	if li == nil {
		return false
	}

	// Get the actual binding to check its type and scopes
	binding := p.env.GetLocalBinding(li)
	if binding == nil {
		return false
	}

	// Only variable bindings shadow macros
	if binding.BindingType() != environment.BindingTypeVariable {
		return false
	}

	// Check scope compatibility for hygiene
	bindingScopes := binding.Scopes()
	if len(bindingScopes) == 0 {
		// Binding has no scopes (user code) - matches any use
		return true
	}

	return syntax.ScopesMatch(scopes, bindingScopes)
}

// ExpandExpression expands a syntax expression.
func (p *ExpanderTimeContinuation) ExpandExpression(expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	select {
	case <-p.ctx.Done():
		return nil, p.ctx.Err()
	default:
	}
	var result syntax.SyntaxValue
	var err error
	if syntax.IsSyntaxEmptyList(expr) {
		return expr, nil
	}
	switch stx := expr.(type) {
	case *syntax.SyntaxPair:
		car := stx.SyntaxCar()
		cdr := stx.SyntaxCdr()
		result, err = p.ExpandSyntaxOrProcedureCall(car, cdr)
		if err != nil {
			return nil, err
		}
		return result, nil
	case *syntax.SyntaxSymbol:
		return p.ExpandSymbol(stx)
	case *syntax.SyntaxObject:
		// Self-evaluating value (integer, boolean, string, etc.)
		return stx, nil
	}
	return p.ExpandSelfEvaluating(expr)
}

// ExpandSymbol handles a symbol expression.
func (p *ExpanderTimeContinuation) ExpandSymbol(expr *syntax.SyntaxSymbol) (syntax.SyntaxValue, error) {
	return expr, nil
}

// ExpandSyntaxOrProcedureCall handles a list expression. The car may be a
// symbol (possibly a macro), a nested pair (computed procedure), or a
// self-evaluating value (like in quoted data or malformed expressions).
func (p *ExpanderTimeContinuation) ExpandSyntaxOrProcedureCall(car syntax.SyntaxValue, cdr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	switch v := car.(type) {
	case *syntax.SyntaxPair:
		// Car is a pair - expand it (computed procedure), then expand arguments
		newCar, err := p.ExpandExpression(v)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "failed to expand car expression")
		}
		rest1, err := p.ExpandSyntaxArgumentList(cdr)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "failed to expand argument list")
		}
		return syntax.NewSyntaxCons(newCar, rest1, newCar.SourceContext()), nil
	case *syntax.SyntaxSymbol:
		// Car is a symbol - check if it's a macro, expand arguments either way
		return p.ExpandSyntaxExpression(v, cdr)
	case *syntax.SyntaxObject:
		// Car is a self-evaluating value - just expand arguments
		rest1, err := p.ExpandSyntaxArgumentList(cdr)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "failed to expand argument list")
		}
		return syntax.NewSyntaxCons(car, rest1, car.SourceContext()), nil
	default:
		// Unknown car type - return expression unchanged
		return syntax.NewSyntaxCons(car, cdr, car.SourceContext()), nil
	}
}

// ExpandSelfEvaluating handles self-evaluating expressions.
func (p *ExpanderTimeContinuation) ExpandSelfEvaluating(expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return expr, nil
}

// ExpandPrimitiveForm handles expansion within primitive forms like if, begin,
// lambda, define, etc. Some primitives need their subexpressions expanded
// (like if, begin) while others should be left unchanged (like quote, define-syntax).
//
// This function looks up the primitive expander in the expand environment registry.
// If found, it invokes the expander; otherwise returns the form unchanged.
func (p *ExpanderTimeContinuation) ExpandPrimitiveForm(primName string, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// Look up the primitive expander in the registry
	symVal := p.env.InternSymbol(values.NewSymbol(primName))
	scopes := sym.Scopes()

	pe := LookupPrimitiveExpander(p.env, symVal, scopes)
	if pe != nil {
		return pe.Expand(p, sym, expr)
	}
	// Unknown primitive - return unchanged (safe default)
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
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
func (p *ExpanderTimeContinuation) expandUnchanged(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

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
	if !ok || argsPair.IsEmptyList() {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: expected bindings and body", formName)
	}

	// Get the bindings list
	bindingsStx := argsPair.SyntaxCar()
	bindingsEmpty := syntax.IsSyntaxEmptyList(bindingsStx)
	var bindingsPair *syntax.SyntaxPair
	if !bindingsEmpty {
		var pairOk bool
		bindingsPair, pairOk = bindingsStx.(*syntax.SyntaxPair)
		if !pairOk {
			return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: expected bindings list", formName)
		}
	}

	// Get the body
	bodyStx := argsPair.SyntaxCdr()
	bodyPair, ok := bodyStx.(*syntax.SyntaxPair)
	if !ok || bodyPair.IsEmptyList() {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: expected body expressions", formName)
	}

	// Count bindings for local environment allocation
	numBindings := 0
	var current *syntax.SyntaxPair
	if !bindingsEmpty {
		current = bindingsPair
		for !syntax.IsSyntaxEmptyList(current) {
			numBindings++
			cdr := current.SyntaxCdr()
			if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
				current = nextPair
			} else {
				break
			}
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
	letScope := syntax.NewRebindingScope()

	// For letrec-syntax, pre-register all keywords so transformers can see each other
	if recursive && !bindingsEmpty {
		current = bindingsPair
		for !syntax.IsSyntaxEmptyList(current) {
			bindingStx := current.SyntaxCar()
			bindingPair, ok := bindingStx.(*syntax.SyntaxPair)
			if !ok || bindingPair.IsEmptyList() {
				return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: invalid binding", formName)
			}
			keywordStx := bindingPair.SyntaxCar()
			keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
			if !ok {
				return nil, values.WrapForeignErrorf(values.ErrNotASymbol, "%s: keyword must be a symbol", formName)
			}
			keyword := keywordSym.Unwrap().(*values.Symbol)
			// Create binding with letScope so free identifier resolution works
			_, _ = childExpandEnv.MaybeCreateLocalBindingWithScopes(keyword, environment.BindingTypeSyntax, []*syntax.Scope{letScope}, keywordSym.SourceContext())

			cdr := current.SyntaxCdr()
			if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
				current = nextPair
			} else {
				break
			}
		}
	}

	// Compile each transformer and store in child expand environment
	if !bindingsEmpty {
		current = bindingsPair
	}
	for !bindingsEmpty && !syntax.IsSyntaxEmptyList(current) {
		bindingStx := current.SyntaxCar()
		bindingPair, ok := bindingStx.(*syntax.SyntaxPair)
		if !ok || bindingPair.IsEmptyList() {
			return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: invalid binding", formName)
		}

		// Get keyword
		keywordStx := bindingPair.SyntaxCar()
		keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASymbol, "%s: keyword must be a symbol", formName)
		}
		keyword := keywordSym.Unwrap().(*values.Symbol)

		// Get transformer expression
		transformerCdr := bindingPair.SyntaxCdr()
		transformerPair, ok := transformerCdr.(*syntax.SyntaxPair)
		if !ok || transformerPair.IsEmptyList() {
			return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: missing transformer expression", formName)
		}
		transformerExpr := transformerPair.SyntaxCar()

		// Check if transformer is a syntax-rules form
		transformerPairExpr, ok := transformerExpr.(*syntax.SyntaxPair)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrUnsupportedTransformer, "%s: only syntax-rules transformers are currently supported", formName)
		}
		car := transformerPairExpr.SyntaxCar()
		if car == nil {
			return nil, values.WrapForeignErrorf(values.ErrUnsupportedTransformer, "%s: invalid transformer", formName)
		}
		srSym, ok := car.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrUnsupportedTransformer, "%s: only syntax-rules transformers are currently supported", formName)
		}
		srSymVal := srSym.Unwrap()
		srSymbol, ok := srSymVal.(*values.Symbol)
		if !ok || srSymbol.Key != "syntax-rules" {
			return nil, values.WrapForeignErrorf(values.ErrUnsupportedTransformer, "%s: only syntax-rules transformers are currently supported", formName)
		}

		// Compile the syntax-rules transformer
		closure, err := CompileSyntaxRules(p.ctx, p.env, transformerPairExpr)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "%s: could not compile transformer for %s", formName, keyword.Key)
		}

		// Store in child expand environment with letScope for free identifier resolution
		localIndex, created := childExpandEnv.MaybeCreateLocalBindingWithScopes(keyword, environment.BindingTypeSyntax, []*syntax.Scope{letScope}, keywordSym.SourceContext())
		if !created {
			localIndex = childExpandEnv.GetLocalIndex(keyword)
		}
		if localIndex != nil {
			err := childExpandEnv.SetLocalValue(localIndex, closure)
			if err != nil {
				return nil, err
			}
		}

		cdr := current.SyntaxCdr()
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	// Add the let-syntax scope to the body
	scopedBody := bodyPair.AddScope(letScope)
	scopedBodyPair, ok := scopedBody.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "%s: body must be a list", formName)
	}

	// Create expander with child expand environment for body expansion
	childExpander := NewExpanderTimeContinuation(p.ctx, childExpandEnv)

	// Expand all body expressions and check for defines
	var expandedExprs []syntax.SyntaxValue
	hasDefine := false
	current = scopedBodyPair
	for !syntax.IsSyntaxEmptyList(current) {
		expr := current.SyntaxCar()
		expandedExpr, err := childExpander.ExpandExpression(expr)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "%s: failed to expand body expression", formName)
		}
		expandedExprs = append(expandedExprs, expandedExpr)
		if isSyntaxFormWithKeyword(expandedExpr, "define") {
			hasDefine = true
		}
		cdr := current.SyntaxCdr()
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else if !syntax.IsSyntaxEmptyList(cdr) {
			return nil, values.WrapForeignErrorf(values.ErrNotAList, "%s: body must be a proper list", formName)
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
	return sym.Sym.Key == keyword
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
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-binding-scope: expected (with-binding-scope (id ...) body)")
	}

	// Get the identifier list - these are the identifiers being bound
	idListStx := pair.SyntaxCar()

	// Get the body
	cdr := pair.SyntaxCdr()
	bodyPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "with-binding-scope: missing body")
	}
	body := bodyPair.SyntaxCar()

	// Create a fresh binding scope
	bindingScope := syntax.NewScope()

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

			sym := p.env.InternSymbol(id.Sym)
			childExpandEnv.MaybeCreateLocalBindingWithScopes(sym, environment.BindingTypeVariable, newScopes, id.SourceContext())
		}

		// Continue expansion with the child environment
		childExpander := NewExpanderTimeContinuation(p.ctx, childExpandEnv)
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
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "syntax-error: missing message argument")
	}

	// Get the message
	msgVal := pair.SyntaxCar()
	var message string
	switch m := msgVal.(type) {
	case *syntax.SyntaxObject:
		if str, ok := m.Unwrap().(*values.String); ok {
			message = str.Value
		} else {
			message = m.Unwrap().SchemeString()
		}
	case *syntax.SyntaxSymbol:
		message = m.Sym.Key
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
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "syntax-error: %s: %s", message, formatIrritants(irritants))
	}
	return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "syntax-error: %s", message)
}

// formatIrritants joins irritants with commas for error display.
func formatIrritants(irritants []string) string {
	return strings.Join(irritants, ", ")
}

// expandBeginForm expands (begin expr ...) by expanding all subexpressions.
// Uses ExpandBodyWithDefineSyntax to compile define-syntax forms immediately,
// ensuring macros defined in begin are available to subsequent forms.
func (p *ExpanderTimeContinuation) expandBeginForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	exprPair, ok := expr.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(exprPair) {
		// Collect forms from the begin body
		forms, err := collectBodyExpressions(exprPair)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "failed to collect begin body")
		}

		// Use ExpandBodyWithDefineSyntax to compile define-syntax forms immediately
		// This ensures macros defined in begin are available to subsequent forms
		expandedForms, err := p.ExpandBodyWithDefineSyntax(forms)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "failed to expand begin body")
		}

		// Rebuild the begin form with expanded contents
		expandedArgs := syntax.SyntaxEmptyList
		for i := len(expandedForms) - 1; i >= 0; i-- {
			expandedArgs = syntax.NewSyntaxCons(expandedForms[i], expandedArgs, sym.SourceContext())
		}
		return syntax.NewSyntaxCons(sym, expandedArgs, sym.SourceContext()), nil
	}
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandIfForm expands (if test consequent [alternative])
func (p *ExpanderTimeContinuation) expandIfForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Expand test
	expandedTest, err := p.ExpandExpression(pair.SyntaxCar())
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "if: failed to expand test")
	}

	// Get consequent
	cdrPair, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "if: missing consequent")
	}

	expandedConseq, err := p.ExpandExpression(cdrPair.SyntaxCar())
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "if: failed to expand consequent")
	}

	// Check for alternative
	altPair, ok := cdrPair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(altPair) {
		// No alternative - (if test conseq)
		args := syntax.SyntaxList(sym.SourceContext(), expandedTest, expandedConseq)
		return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
	}

	// Expand alternative
	expandedAlt, err := p.ExpandExpression(altPair.SyntaxCar())
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "if: failed to expand alternative")
	}

	// Build (if test conseq alt)
	args := syntax.SyntaxList(sym.SourceContext(), expandedTest, expandedConseq, expandedAlt)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandSetForm expands (set! var value)
func (p *ExpanderTimeContinuation) expandSetForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Keep variable unchanged
	varExpr := pair.SyntaxCar()

	// Expand value
	cdrPair, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	expandedValue, err := p.ExpandExpression(cdrPair.SyntaxCar())
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "set!: failed to expand value")
	}

	// Build (set! var expanded-value)
	args := syntax.SyntaxList(sym.SourceContext(), varExpr, expandedValue)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandDefineForm expands (define var value) or (define (name . args) body ...)
func (p *ExpanderTimeContinuation) expandDefineForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	first := pair.SyntaxCar()
	cdrPair, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Check if it's a function definition (define (name args...) body...)
	_, isSymbol := first.(*syntax.SyntaxSymbol)
	if !isSymbol {
		// Function definition - first is (name args...)
		// Expand the body expressions
		expandedBody, err := p.ExpandSyntaxArgumentList(cdrPair)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "define: failed to expand body")
		}
		args := syntax.NewSyntaxCons(first, expandedBody, sym.SourceContext())
		return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
	}

	// Simple definition (define var value)
	expandedValue, err := p.ExpandExpression(cdrPair.SyntaxCar())
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "define: failed to expand value")
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
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	importSets, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "import: expected list of import sets")
	}

	// Process each import set to load libraries and copy bindings
	_, err := syntax.SyntaxForEach(p.ctx, importSets, func(_ context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		importSet, parseErr := parseImportSet(p.ctx, importSetExpr)
		if parseErr != nil {
			return parseErr
		}

		// Load the library
		lib, loadErr := LoadLibrary(p.ctx, importSet.LibraryName, p.env)
		if loadErr != nil {
			return values.WrapForeignErrorf(loadErr, "import: failed to load library %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply import modifiers (only, except, prefix, rename) to get final bindings
		bindings, applyErr := importSet.ApplyToExports(lib)
		if applyErr != nil {
			return values.WrapForeignErrorf(applyErr, "import: error applying modifiers for %s",
				importSet.LibraryName.SchemeString())
		}

		// Copy bindings to the target phase - this makes macros available
		copyErr := CopyLibraryBindingsToEnvAtPhase(lib, bindings, p.env, importSet.PhaseShift)
		if copyErr != nil {
			return values.WrapForeignErrorf(copyErr, "import: error copying bindings from %s",
				importSet.LibraryName.SchemeString())
		}

		return nil
	})

	if err != nil {
		return nil, err
	}

	// Return the import form unchanged for later compilation
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

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
	lambdaScope := syntax.NewScope()

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
		return nil, values.WrapForeignErrorf(err, "lambda: invalid body expression")
	}

	// Handle the case where body is wrapped in (begin ...) - common from let macro
	unwrappedExprs, wasBeginWrapped := unwrapBeginBodyWithFlag(bodyExprs)

	// Expand body in the child environment, compiling define-syntax as encountered
	childExpander := NewExpanderTimeContinuation(p.ctx, childEnv)
	expandedExprs, err := childExpander.ExpandBodyWithDefineSyntax(unwrappedExprs)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "lambda: failed to expand body")
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
func collectBodyExpressions(body *syntax.SyntaxPair) ([]syntax.SyntaxValue, error) {
	var exprs []syntax.SyntaxValue
	current := body
	for !syntax.IsSyntaxEmptyList(current) {
		exprs = append(exprs, current.SyntaxCar())
		cdr := current.SyntaxCdr()
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else if syntax.IsSyntaxEmptyList(cdr) {
			break
		} else {
			return nil, values.WrapForeignErrorf(values.ErrNotAList, "body must be a proper list")
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

// extractDefineName extracts the name being defined from a define form.
// Returns nil if the form is not a define or is malformed.
//
// Note: This intentionally excludes define-syntax forms. Macro bindings are
// handled separately by compileDefineSyntaxFromSyntax which stores them in the
// expand environment. We only pre-register define bindings so that macros can
// reference forward-declared variable definitions.
//
// Handles:
//   - (define name value)
//   - (define (name args...) body...)
func extractDefineName(form syntax.SyntaxValue) *syntax.SyntaxSymbol {
	pair, ok := form.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return nil
	}

	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return nil
	}

	sym := carSym.Unwrap().(*values.Symbol)
	// Only handle define, not define-syntax (macros are handled separately)
	if sym.Key != "define" {
		return nil
	}

	cdr := pair.SyntaxCdr()
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return nil
	}

	second := cdrPair.SyntaxCar()
	switch s := second.(type) {
	case *syntax.SyntaxSymbol:
		// (define name ...)
		return s
	case *syntax.SyntaxPair:
		// (define (name args...) body...) - extract name from the pair
		if !syntax.IsSyntaxEmptyList(s) {
			nameExpr, ok := s.SyntaxCar().(*syntax.SyntaxSymbol)
			if ok {
				return nameExpr
			}
		}
	}
	return nil
}

// ExpandBodyWithDefineSyntax expands a sequence of body forms, compiling
// define-syntax forms as encountered so subsequent forms can use the macros.
//
// This unifies the expansion pattern used by:
// - Lambda bodies (internal define-syntax)
// - Library bodies (top-level define-syntax)
// - Include files (top-level define-syntax)
//
// R7RS §5.3: Internal define-syntax forms must be processed before expanding
// subsequent body expressions so that locally-defined macros are visible.
//
// R7RS §5.3.2: Bodies use letrec* semantics where all defined names are visible
// to all initializers. This enables forward references within macros - a macro
// can reference a definition that appears later in the same body.
func (p *ExpanderTimeContinuation) ExpandBodyWithDefineSyntax(
	forms []syntax.SyntaxValue,
) ([]syntax.SyntaxValue, error) {
	// Pre-scan: Register placeholder bindings for all define/define-syntax forms
	// This enables forward hygienic references within the body (R7RS letrec* semantics)
	for _, form := range forms {
		nameSym := extractDefineName(form)
		if nameSym != nil {
			name := p.env.InternSymbol(nameSym.Unwrap().(*values.Symbol))
			scopes := nameSym.Scopes()
			source := nameSym.SourceContext()
			// Create placeholder binding in current environment (not expand phase)
			if p.env.LocalEnvironment() != nil {
				p.env.MaybeCreateLocalBindingWithScopes(name, environment.BindingTypeVariable, scopes, source)
			} else {
				gi, _ := p.env.MaybeCreateOwnGlobalBinding(name, environment.BindingTypeVariable)
				if source != nil {
					binding := p.env.GetGlobalBinding(gi)
					if binding != nil {
						binding.SetSource(source)
					}
				}
			}
		}
	}

	// Now expand sequentially with all bindings visible
	var result []syntax.SyntaxValue
	for _, form := range forms {
		expanded, err := p.ExpandExpression(form)
		if err != nil {
			return nil, err
		}

		// If define-syntax, compile it now for subsequent forms
		if isSyntaxFormWithKeyword(expanded, "define-syntax") {
			pair := expanded.(*syntax.SyntaxPair)
			err = compileDefineSyntaxFromSyntax(p.ctx, p.env, pair)
			if err != nil {
				return nil, err
			}
		}

		result = append(result, expanded)
	}
	return result, nil
}

// compileDefineSyntaxFromSyntax compiles a define-syntax form and stores the transformer
// in the expand environment.
//
// The env parameter is used for free identifier resolution during compilation (so macros
// can see local bindings like lambda parameters), while the actual macro binding is stored
// in env.Expand() for lookup during expansion.
func compileDefineSyntaxFromSyntax(ctx context.Context, env *environment.EnvironmentFrame, dsPair *syntax.SyntaxPair) error {
	expandEnv := env.Expand()

	// Extract: (define-syntax keyword transformer)
	cdr, ok := dsPair.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "define-syntax: malformed")
	}
	keywordSym, ok := cdr.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASymbol, "define-syntax: keyword must be a symbol")
	}
	keyword := expandEnv.InternSymbol(keywordSym.Unwrap().(*values.Symbol))
	symbolScopes := keywordSym.Scopes()

	transformerCdr, ok := cdr.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "define-syntax: missing transformer")
	}
	transformer := transformerCdr.SyntaxCar()

	// Compile the transformer using the full environment for free identifier resolution
	// This allows macros to see local bindings (e.g., lambda parameters, forward references)
	// Supports both syntax-rules and lambda (procedural) transformers
	closure, err := compileTransformerToMachineClosure(ctx, env, transformer)
	if err != nil {
		return err
	}

	// Store in the expand environment (for macro lookup during expansion)
	globalIndex, _ := expandEnv.MaybeCreateOwnGlobalBinding(keyword, environment.BindingTypeSyntax)
	binding := expandEnv.GetGlobalBinding(globalIndex)
	if binding != nil && symbolScopes != nil {
		binding.SetScopes(symbolScopes)
	}
	return expandEnv.SetOwnGlobalValue(globalIndex, closure)
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
			if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
				current = nextPair
			} else if sym, ok := cdr.(*syntax.SyntaxSymbol); ok {
				// Improper list: (x y . rest)
				result = append(result, formalSymbol{sym.Sym, sym.Scopes(), sym.SourceContext()})
				break
			} else {
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
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
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
					return nil, values.WrapForeignErrorf(err, "case-lambda: failed to expand clause body")
				}

				// Build (formals expanded-body...)
				expandedClause := syntax.NewSyntaxCons(formalsStx, expandedBody, clausePair.SourceContext())
				expandedClauses = append(expandedClauses, expandedClause)
			}
		}

		cdr := current.SyntaxCdr()
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	// Build (case-lambda expanded-clauses...)
	clauseList := syntax.SyntaxList(sym.SourceContext(), expandedClauses...)
	return syntax.NewSyntaxCons(sym, clauseList, sym.SourceContext()), nil
}

// ExpandSyntaxExpression checks if sym is a macro and expands it, or returns
// the expression as a procedure call if not.
//
// This is where macro invocation happens:
//  1. Look up the symbol in the expand environment
//  2. If bound with BindingTypeSyntax, it's a macro - invoke the transformer
//  3. If it's a primitive (like quote, if, define-syntax), don't expand args
//  4. Otherwise, treat as procedure call and expand arguments
//
// The transformer closure (MachineClosure from CompileSyntaxRules) is invoked
// by creating a MachineContext and running it. The transformer:
//   - Receives the full macro invocation form on the eval stack
//   - Pattern matches against its clauses (OperationSyntaxRulesTransform)
//   - Expands the matching template with captured bindings
//   - Adds an intro scope to the expansion for hygiene
//   - Returns the expanded syntax in the value register
//
// The expanded result may itself contain macro invocations, so the caller
// should recursively expand it.
func (p *ExpanderTimeContinuation) ExpandSyntaxExpression(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	sym0, ok := sym.Unwrap().(*values.Symbol) // Ensure sym is a symbol
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASymbol, "expected a symbol for syntax, got %T", sym.Unwrap())
	}

	// R7RS §4.2.2: Local variable bindings shadow macros AND primitive forms
	// Check if there's a local variable binding before checking for macros or primitives
	hasLocalBinding := p.hasLocalVariableBinding(sym0, sym.Scopes())

	if !hasLocalBinding {
		// No local variable shadowing - check for macros
		// First check local bindings in p.env (supports let-syntax local macros)
		// Then fall back to the global expand environment
		var bnd *environment.Binding

		// Check local bindings first (for let-syntax/letrec-syntax)
		bnd = p.env.GetBinding(sym0)

		// If not found locally, check the global expand environment
		if bnd == nil || bnd.BindingType() != environment.BindingTypeSyntax {
			expandEnv := p.env.Expand()
			bnd = expandEnv.GetBinding(sym0)
		}

		// Check if it's a macro binding
		if bnd != nil && !values.IsVoid(bnd) && bnd.BindingType() == environment.BindingTypeSyntax {
			// This is a macro - invoke the transformer
			return p.expandMacroInvocation(sym, expr, bnd)
		}

		// Not a macro - check if it's a primitive (quote, if, define-syntax, etc.)
		symVal := p.env.InternSymbol(sym0)
		pe := LookupPrimitiveExpander(p.env, symVal, sym.Scopes())
		if pe != nil {
			return pe.Expand(p, sym, expr)
		}
	}

	// Regular procedure call - expand arguments (they might contain macro calls)
	exprPair, ok := expr.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(exprPair) {
		expandedArgs, err := p.ExpandSyntaxArgumentList(exprPair)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "failed to expand arguments")
		}
		return syntax.NewSyntaxCons(sym, expandedArgs, sym.SourceContext()), nil
	}
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandMacroInvocation invokes a macro transformer and returns the expanded result.
// This is called when ExpandSyntaxExpression determines that a symbol is bound to a macro.
func (p *ExpanderTimeContinuation) expandMacroInvocation(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, bnd *environment.Binding) (syntax.SyntaxValue, error) {
	mcls, ok := bnd.Value().(*MachineClosure)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAClosure, "not a machine closure: %T", bnd.Value())
	}
	// Acquire a pooled machine context for the macro transformer.
	mc := acquireMacroContext(p.ctx, mcls)
	defer ReleaseSubContext(mc)

	// Set the expander context so the transformer can access the use-site environment.
	// This is critical for R7RS §4.3.2 auxiliary syntax hygiene: the pattern matcher
	// needs to check if input identifiers have lexical bindings at the use site.
	// For example, in (let ((=> #f)) (cond (#t => 'ok))), the pattern matcher needs
	// to see that => is bound by the lambda (from let expansion) to correctly
	// determine that it shouldn't match the literal => in cond's pattern.
	expanderCtx := NewExpanderContext(p.env, p)
	mc.SetExpanderContext(expanderCtx)

	// For syntax-rules transformers, we pass the entire input form as an argument.
	// The transformer expects the full form including the macro name.
	inputForm := syntax.NewSyntaxCons(sym, expr, sym.SourceContext())

	// Apply the transformer with the input form as the argument
	// This sets up the local environment binding for parameter 0
	_, err := mc.Apply(mcls, inputForm)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "failed to apply transformer")
	}

	err = mc.Run()
	if err != nil {
		return nil, err
	}
	// Check if the transformer produced a result
	result := mc.GetValue()
	if values.IsVoid(result) {
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedNil, "syntax transformer produced no result")
	}

	// For syntax-rules transformers, the result should be the expanded form.
	// The expanded result may itself contain macro invocations (especially for
	// recursive macros like `and`, `or`, `let*`, etc.), so we must recursively
	// expand it.
	stx, ok := result.(syntax.SyntaxValue)
	if ok {
		// Recursively expand the result to handle nested macro calls
		return p.ExpandExpression(stx)
	}
	return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "syntax transformer returned non-syntax value: %T", result)
}

// ExpandOnce performs a single step of macro expansion.
// Returns (expanded-syntax, did-expand, error).
// If the input is a macro call, it expands it once and returns (result, true, nil).
// If the input is not a macro call, it returns (input, false, nil).
// Unlike ExpandExpression, this does NOT recursively expand the result.
func (p *ExpanderTimeContinuation) ExpandOnce(expr syntax.SyntaxValue) (syntax.SyntaxValue, bool, error) {
	// Only pairs can be macro calls
	stxPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return expr, false, nil
	}

	// Handle empty list
	if syntax.IsSyntaxEmptyList(stxPair) {
		return expr, false, nil
	}

	// Check if the car is a symbol
	car := stxPair.SyntaxCar()
	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		return expr, false, nil
	}

	sym0, ok := sym.Unwrap().(*values.Symbol)
	if !ok {
		return expr, false, nil
	}

	// R7RS §4.2.2: Local variable bindings shadow macros
	// Check if there's a local variable binding before checking for macros
	if p.hasLocalVariableBinding(sym0, sym.Scopes()) {
		// Local variable shadows macro - no expansion
		return expr, false, nil
	}

	// Look up syntax bindings in the expand phase environment
	expandEnv := p.env.Expand()
	bnd := expandEnv.GetBinding(sym0)

	// Check if it's a macro binding
	if values.IsVoid(bnd) || bnd.BindingType() != environment.BindingTypeSyntax {
		// Not a macro - no expansion
		return expr, false, nil
	}

	// Get the transformer closure
	mcls, ok := bnd.Value().(*MachineClosure)
	if !ok {
		return nil, false, values.WrapForeignErrorf(values.ErrNotAClosure, "not a machine closure: %T", bnd.Value())
	}

	// Acquire a pooled machine context for the macro transformer.
	mc := acquireMacroContext(p.ctx, mcls)
	defer ReleaseSubContext(mc)

	// Build the input form
	var cdr syntax.SyntaxValue
	cdrPair, ok := stxPair.SyntaxCdr().(*syntax.SyntaxPair)
	if ok {
		cdr = cdrPair
	} else {
		cdr = syntax.SyntaxEmptyList
	}
	inputForm := syntax.NewSyntaxCons(sym, cdr, sym.SourceContext())

	// Apply the transformer
	_, err := mc.Apply(mcls, inputForm)
	if err != nil {
		return nil, false, values.WrapForeignErrorf(err, "failed to apply transformer")
	}

	err = mc.Run()
	if err != nil {
		return nil, false, err
	}

	// Check if the transformer produced a result
	result := mc.GetValue()
	if values.IsVoid(result) {
		return nil, false, values.WrapForeignErrorf(values.ErrUnexpectedNil, "syntax transformer produced no result")
	}

	// Return the result WITHOUT recursive expansion
	stx, ok := result.(syntax.SyntaxValue)
	if ok {
		return stx, true, nil
	}

	return nil, false, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "syntax transformer returned non-syntax value: %T", result)
}

// ExpandSyntaxArgumentList expands each argument in the argument list.
// It returns a new syntax list with the expanded arguments.
func (p *ExpanderTimeContinuation) ExpandSyntaxArgumentList(args syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// instantiate result list
	q := syntax.SyntaxEmptyList
	// go through each argument and expand it
	// and append to result list
	// if any error, return error
	// if not a proper list, return error
	// finally return the new list
	tail, err := syntax.SyntaxForEach(p.ctx, args, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		v0, err := p.ExpandExpression(v)
		if err != nil {
			return values.WrapForeignErrorf(err, "failed to expand argument list")
		}
		// append to result list
		cdr := syntax.SyntaxList(v0.SourceContext(), v0)
		q = q.SyntaxAppend(cdr).(syntax.SyntaxTuple)
		return nil
	})
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "failed to expand argument list")
	}
	// tail contains the last element of the list, which should be an empty list. anything else is an error.
	if !syntax.IsSyntaxEmptyList(tail) {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxList, "expected a list of arguments, got %T", tail)
	}
	return q, nil
}

// ExpandQuasiquote handles the expansion of quasiquoted expressions.
func (p *ExpanderTimeContinuation) ExpandQuasiquote(_ syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return nil, nil
}

// ExpandQuote handles the expansion of quoted expressions.
func (p *ExpanderTimeContinuation) ExpandQuote(_ syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return nil, nil
}

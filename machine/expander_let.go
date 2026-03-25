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

// expander_let.go implements expand-time handling of let, let*, letrec,
// and letrec* binding forms as core forms (replacing macro expansion).
//
// Hygiene — scoping precision (improvement over macro):
//   - let:          scope on names + body, NOT on init exprs (R7RS §4.2.2)
//   - let*:         scope on names + body, each init sees only preceding bindings
//   - letrec/letrec*: scope on names + body + ALL init exprs

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
)

// expandLetForm expands (let ((name val) ...) body ...) and
// (let tag ((name val) ...) body ...) for named let.
//
// R7RS §4.2.2: let bindings are not visible to init expressions.
// Named let is rewritten to (letrec ((tag (lambda (names...) body...))) (tag vals...)).
func (p *ExpanderTimeContinuation) expandLetForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Detect named let: second element is a symbol
	firstArg := pair.SyntaxCar()
	_, isSym := firstArg.(*syntax.SyntaxSymbol)
	if isSym {
		return p.expandNamedLet(sym, pair)
	}

	// Plain let: ((name val) ...) body ...
	bindingsStx := pair.SyntaxCar()
	bodyCdr := pair.SyntaxCdr()
	bodyPair, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Create a scope for this let's bindings
	letScope := syntax.NewScopeWithLabel("let")

	// For let: scope on binding NAMES + body, NOT on init expressions
	// Expand init expressions in current env (they don't see let bindings)
	expandedBindings, bindingSyms, err := p.expandLetBindings(bindingsStx, letScope, false)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "let: failed to expand bindings")
	}

	// Add scope to body
	bodyWithScope := syntax.AddScopeToSyntax(bodyPair, letScope)

	// Create child env with binding names
	childEnv := p.createBindingEnv(bindingSyms)

	// Expand body in child env
	expandedBody, err := p.expandBindingBody(childEnv, bodyWithScope)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "let: failed to expand body")
	}

	// Reconstruct (let ((name expanded-val) ...) expanded-body...)
	args := syntax.NewSyntaxCons(expandedBindings, expandedBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandNamedLet handles (let tag ((name val) ...) body ...).
// Rewrites to (letrec ((tag (lambda (names...) body...))) (tag vals...))
// which the letrec expander then handles.
func (p *ExpanderTimeContinuation) expandNamedLet(sym *syntax.SyntaxSymbol, argsPair *syntax.SyntaxPair) (syntax.SyntaxValue, error) {
	tagSym := argsPair.SyntaxCar().(*syntax.SyntaxSymbol)

	cdr := argsPair.SyntaxCdr()
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return syntax.NewSyntaxCons(sym, argsPair, sym.SourceContext()), nil
	}

	bindingsStx := cdrPair.SyntaxCar()
	bodyCdr := cdrPair.SyntaxCdr()
	bodyPair, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return syntax.NewSyntaxCons(sym, argsPair, sym.SourceContext()), nil
	}

	// Create scope for the letrec that wraps the named let
	letrecScope := syntax.NewScopeWithLabel("letrec")

	// Add scope to tag
	tagWithScope := syntax.AddScopeToSyntax(tagSym, letrecScope).(*syntax.SyntaxSymbol)

	// Expand bindings: init exprs don't see the tag
	expandedBindings, bindingSyms, err := p.expandLetBindings(bindingsStx, letrecScope, false)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "named let: failed to expand bindings")
	}

	// Add scope to body
	bodyWithScope := syntax.AddScopeToSyntax(bodyPair, letrecScope)

	// Create child env with tag + binding names
	childEnv := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0),
		p.env,
	)
	childEnv.MaybeCreateLocalBindingWithScopes(
		tagWithScope.Sym, environment.BindingTypeVariable,
		tagWithScope.Scopes(), tagWithScope.SourceContext(),
	)
	for _, bs := range bindingSyms {
		childEnv.MaybeCreateLocalBindingWithScopes(
			bs.Sym, environment.BindingTypeVariable,
			bs.Scopes(), bs.SourceContext(),
		)
	}

	// Expand body in child env
	expandedBody, err := p.expandBindingBody(childEnv, bodyWithScope)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "named let: failed to expand body")
	}

	// Reconstruct (let tag ((name val) ...) expanded-body...)
	bindsAndBody := syntax.NewSyntaxCons(expandedBindings, expandedBody, sym.SourceContext())
	tagAndRest := syntax.NewSyntaxCons(tagWithScope, bindsAndBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, tagAndRest, sym.SourceContext()), nil
}

// expandLetStarForm expands (let* ((name val) ...) body ...).
//
// R7RS §4.2.2: Each binding's init sees all preceding bindings.
func (p *ExpanderTimeContinuation) expandLetStarForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	bindingsStx := pair.SyntaxCar()
	bodyCdr := pair.SyntaxCdr()
	bodyPair, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Create scope for let* bindings
	letScope := syntax.NewScopeWithLabel("let*")

	// For let*: expand each init sequentially, adding each binding to env
	// before expanding the next init
	expandedBindings, bindingSyms, err := p.expandLetStarBindings(bindingsStx, letScope)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "let*: failed to expand bindings")
	}

	// Add scope to body
	bodyWithScope := syntax.AddScopeToSyntax(bodyPair, letScope)

	// Create child env with all binding names
	childEnv := p.createBindingEnv(bindingSyms)

	// Expand body
	expandedBody, err := p.expandBindingBody(childEnv, bodyWithScope)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "let*: failed to expand body")
	}

	args := syntax.NewSyntaxCons(expandedBindings, expandedBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandLetrecForm expands (letrec ((name val) ...) body ...).
//
// R7RS §4.2.2: All bindings are in scope for all inits and body.
func (p *ExpanderTimeContinuation) expandLetrecForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetrecCommon(sym, expr, "letrec")
}

// expandLetrecStarForm expands (letrec* ((name val) ...) body ...).
//
// R7RS §4.2.2: Same scoping as letrec; left-to-right evaluation order is
// a runtime semantic, not a scoping difference.
func (p *ExpanderTimeContinuation) expandLetrecStarForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetrecCommon(sym, expr, "letrec*")
}

// expandLetrecCommon is shared logic for letrec and letrec*.
func (p *ExpanderTimeContinuation) expandLetrecCommon(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, label string) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	bindingsStx := pair.SyntaxCar()
	bodyCdr := pair.SyntaxCdr()
	bodyPair, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Create scope for letrec bindings
	letrecScope := syntax.NewScopeWithLabel(label)

	// For letrec: scope on names + init exprs + body (all bindings visible everywhere)
	expandedBindings, bindingSyms, err := p.expandLetBindings(bindingsStx, letrecScope, true)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "%s: failed to expand bindings", label)
	}

	// Add scope to body
	bodyWithScope := syntax.AddScopeToSyntax(bodyPair, letrecScope)

	// Create child env with all bindings
	childEnv := p.createBindingEnv(bindingSyms)

	// Expand body
	expandedBody, err := p.expandBindingBody(childEnv, bodyWithScope)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "%s: failed to expand body", label)
	}

	args := syntax.NewSyntaxCons(expandedBindings, expandedBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// --- Shared helpers ---

// expandLetBindings expands init expressions in a binding list.
// For let: scopeInits=false (inits don't see bindings).
// For letrec/letrec*: scopeInits=true (inits see all bindings).
//
// The scope is added to binding names regardless. Init expressions get
// the scope added only if scopeInits is true, then all inits are expanded
// in the appropriate environment (current env for let, child env for letrec).
func (p *ExpanderTimeContinuation) expandLetBindings(
	bindingsStx syntax.SyntaxValue,
	scope *syntax.Scope,
	scopeInits bool,
) (syntax.SyntaxValue, []*syntax.SyntaxSymbol, error) {
	if syntax.IsSyntaxEmptyList(bindingsStx) {
		return bindingsStx, nil, nil
	}

	bindingsPair, ok := bindingsStx.(*syntax.SyntaxPair)
	if !ok {
		return bindingsStx, nil, nil
	}

	// First pass: collect binding names with scope, add scope to inits if needed
	var scopedNames []*syntax.SyntaxSymbol
	var bindingPairs []*syntax.SyntaxPair
	current := bindingsPair
	for !syntax.IsSyntaxEmptyList(current) {
		bindingForm := current.SyntaxCar()
		bPair, ok := bindingForm.(*syntax.SyntaxPair)
		if !ok {
			// Malformed — let validator will catch it
			return bindingsStx, nil, nil
		}

		// Extract name and add scope
		nameStx := bPair.SyntaxCar()
		nameSym, ok := nameStx.(*syntax.SyntaxSymbol)
		if !ok {
			return bindingsStx, nil, nil
		}
		scopedName := syntax.AddScopeToSyntax(nameSym, scope).(*syntax.SyntaxSymbol)
		scopedNames = append(scopedNames, scopedName)
		bindingPairs = append(bindingPairs, bPair)

		cdr := current.SyntaxCdr()
		next, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = next
		} else {
			break
		}
	}

	// For letrec: create child env before expanding inits
	var initExpander *ExpanderTimeContinuation
	if scopeInits {
		childEnv := p.createBindingEnv(scopedNames)
		initExpander = NewExpanderTimeContinuation(p.ctx, childEnv)
	} else {
		initExpander = p
	}

	// Second pass: expand each init and rebuild binding list
	var expandedBindingsList []syntax.SyntaxValue
	for i, bPair := range bindingPairs {
		// Get init expression (second element of binding pair)
		initCdr := bPair.SyntaxCdr()
		initPair, ok := initCdr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(initPair) {
			return bindingsStx, nil, nil
		}
		initExpr := initPair.SyntaxCar()

		// Add scope to init if letrec
		if scopeInits {
			initExpr = syntax.AddScopeToSyntax(initExpr, scope)
		}

		// Expand the init expression
		expandedInit, err := initExpander.ExpandExpression(initExpr)
		if err != nil {
			return nil, nil, err
		}

		// Rebuild (name expanded-init)
		sc := bPair.SourceContext()
		initList := syntax.SyntaxList(sc, expandedInit)
		rebuilt := syntax.NewSyntaxCons(scopedNames[i], initList, sc)
		expandedBindingsList = append(expandedBindingsList, rebuilt)
	}

	return syntax.SyntaxList(bindingsPair.SourceContext(), expandedBindingsList...), scopedNames, nil
}

// expandLetStarBindings expands init expressions sequentially for let*,
// creating the child environment incrementally so each init sees preceding bindings.
func (p *ExpanderTimeContinuation) expandLetStarBindings(
	bindingsStx syntax.SyntaxValue,
	scope *syntax.Scope,
) (syntax.SyntaxValue, []*syntax.SyntaxSymbol, error) {
	if syntax.IsSyntaxEmptyList(bindingsStx) {
		return bindingsStx, nil, nil
	}

	bindingsPair, ok := bindingsStx.(*syntax.SyntaxPair)
	if !ok {
		return bindingsStx, nil, nil
	}

	// Build child env incrementally
	childEnv := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0),
		p.env,
	)

	var scopedNames []*syntax.SyntaxSymbol
	var expandedBindingsList []syntax.SyntaxValue

	current := bindingsPair
	for !syntax.IsSyntaxEmptyList(current) {
		bindingForm := current.SyntaxCar()
		bPair, ok := bindingForm.(*syntax.SyntaxPair)
		if !ok {
			return bindingsStx, nil, nil
		}

		nameStx := bPair.SyntaxCar()
		nameSym, ok := nameStx.(*syntax.SyntaxSymbol)
		if !ok {
			return bindingsStx, nil, nil
		}

		// Get init expression
		initCdr := bPair.SyntaxCdr()
		initPair, ok := initCdr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(initPair) {
			return bindingsStx, nil, nil
		}
		initExpr := initPair.SyntaxCar()

		// Add scope to init so references can resolve to preceding bindings.
		// The scope enables matching; the env controls visibility (preceding
		// bindings are in env, current binding is not yet).
		scopedInit := syntax.AddScopeToSyntax(initExpr, scope)

		// Expand init in current child env (sees preceding bindings)
		currentExpander := NewExpanderTimeContinuation(p.ctx, childEnv)
		expandedInit, err := currentExpander.ExpandExpression(scopedInit)
		if err != nil {
			return nil, nil, err
		}

		// Add scope to name AFTER expanding init (name not visible to own init)
		scopedName := syntax.AddScopeToSyntax(nameSym, scope).(*syntax.SyntaxSymbol)
		scopedNames = append(scopedNames, scopedName)

		// Add binding to child env for subsequent inits
		childEnv.MaybeCreateLocalBindingWithScopes(
			scopedName.Sym, environment.BindingTypeVariable,
			scopedName.Scopes(), scopedName.SourceContext(),
		)

		// Rebuild (name expanded-init)
		sc := bPair.SourceContext()
		initList := syntax.SyntaxList(sc, expandedInit)
		rebuilt := syntax.NewSyntaxCons(scopedName, initList, sc)
		expandedBindingsList = append(expandedBindingsList, rebuilt)

		cdr := current.SyntaxCdr()
		next, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = next
		} else {
			break
		}
	}

	return syntax.SyntaxList(bindingsPair.SourceContext(), expandedBindingsList...), scopedNames, nil
}

// createBindingEnv creates a child environment with the given symbols
// as local variable bindings.
func (p *ExpanderTimeContinuation) createBindingEnv(syms []*syntax.SyntaxSymbol) *environment.EnvironmentFrame {
	childEnv := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0),
		p.env,
	)
	for _, s := range syms {
		childEnv.MaybeCreateLocalBindingWithScopes(
			s.Sym, environment.BindingTypeVariable,
			s.Scopes(), s.SourceContext(),
		)
	}
	return childEnv
}

// expandBindingBody expands a body syntax value in the given environment,
// processing define-syntax forms before expanding subsequent expressions.
func (p *ExpanderTimeContinuation) expandBindingBody(
	env *environment.EnvironmentFrame,
	bodyStx syntax.SyntaxValue,
) (syntax.SyntaxValue, error) {
	bodyPair, ok := bodyStx.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return bodyStx, nil
	}

	bodyExprs, err := collectBodyExpressions(bodyPair)
	if err != nil {
		return nil, err
	}

	childExpander := NewExpanderTimeContinuation(p.ctx, env)
	expandedExprs, err := childExpander.ExpandBodyWithDefineSyntax(bodyExprs)
	if err != nil {
		return nil, err
	}

	return syntax.SyntaxList(bodyPair.SourceContext(), expandedExprs...), nil
}

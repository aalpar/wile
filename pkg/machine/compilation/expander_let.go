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

// expander_let.go implements expand-time handling of let, let*, letrec,
// and letrec* binding forms as core forms (replacing macro expansion).
//
// All four forms share a single entry point parameterized by scopeInits
// (whether to add scope to init expressions) and sequential (whether to
// expand inits with incremental env visibility). Named let is handled
// as a special case of plain let.

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// expandLetForm expands (let ...) including named let.
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

	return p.expandLetCommon(sym, expr, "let", false, false)
}

// expandLetStarForm expands (let* ...).
func (p *ExpanderTimeContinuation) expandLetStarForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetCommon(sym, expr, "let*", false, true)
}

// expandLetrecForm expands (letrec ...).
func (p *ExpanderTimeContinuation) expandLetrecForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetCommon(sym, expr, "letrec", true, false)
}

// expandLetrecStarForm expands (letrec* ...).
// sequential=false because all inits see all bindings (same as letrec at
// expansion time). The sequential evaluation order is a compiler concern,
// not an expander concern — handled by LetKindLetrecStar in CompileValidatedLet.
func (p *ExpanderTimeContinuation) expandLetrecStarForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetCommon(sym, expr, "letrec*", true, false)
}

// expandLetCommon is the shared expansion logic for all four binding forms.
//
// Parameters:
//   - scopeInits: add binding scope to init expressions (letrec, letrec*)
//   - sequential: expand inits with incremental env visibility (let*)
//
// Hygiene — scoping precision:
//   - let:          scope on names + body, NOT on init exprs (R7RS §4.2.2)
//   - let*:         scope on names + body, each init sees preceding bindings
//   - letrec/letrec*: scope on names + body + ALL init exprs
func (p *ExpanderTimeContinuation) expandLetCommon(
	sym *syntax.SyntaxSymbol,
	expr syntax.SyntaxValue,
	label string,
	scopeInits bool,
	sequential bool,
) (syntax.SyntaxValue, error) {
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

	scope := p.newBinderScope(label)

	// Expand bindings based on form semantics
	var expandedBindings syntax.SyntaxValue
	var bindingSyms []*syntax.SyntaxSymbol
	var err error

	if sequential {
		expandedBindings, bindingSyms, err = p.expandLetStarBindings(bindingsStx, scope)
	} else {
		expandedBindings, bindingSyms, err = p.expandLetBindings(bindingsStx, scope, scopeInits)
	}
	if err != nil {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "%s: failed to expand bindings", label))
	}

	// Add scope to body
	bodyWithScope := syntax.AddScopeToSyntax(bodyPair, scope)

	// Create child env with all binding names
	childEnv := p.createBindingEnv(bindingSyms)

	// Expand body
	expandedBody, err := p.expandBindingBody(childEnv, bodyWithScope)
	if err != nil {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "%s: failed to expand body", label))
	}

	args := syntax.NewSyntaxCons(expandedBindings, expandedBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandNamedLet handles (let tag ((name val) ...) body ...).
func (p *ExpanderTimeContinuation) expandNamedLet(sym *syntax.SyntaxSymbol, argsPair *syntax.SyntaxPair) (syntax.SyntaxValue, error) {
	tagSym, ok := argsPair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return syntax.NewSyntaxCons(sym, argsPair, sym.SourceContext()), nil
	}

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

	letrecScope := p.newBinderScope("letrec")

	tagWithScope := syntax.AddScopeToSyntax(tagSym, letrecScope).(*syntax.SyntaxSymbol)

	// Expand bindings: init exprs don't see the tag
	expandedBindings, bindingSyms, err := p.expandLetBindings(bindingsStx, letrecScope, false)
	if err != nil {
		return nil, wrapSourcedError(argsPair.SourceContext(), werr.WrapForeignErrorf(err, "named let: failed to expand bindings"))
	}

	bodyWithScope := syntax.AddScopeToSyntax(bodyPair, letrecScope)

	// Create child env with tag + binding names. The tag is bound first (slot 0)
	// so it is visible to the body as the recursive procedure name.
	childEnv := p.createBindingEnv(append([]*syntax.SyntaxSymbol{tagWithScope}, bindingSyms...))

	expandedBody, err := p.expandBindingBody(childEnv, bodyWithScope)
	if err != nil {
		return nil, wrapSourcedError(argsPair.SourceContext(), werr.WrapForeignErrorf(err, "named let: failed to expand body"))
	}

	bindsAndBody := syntax.NewSyntaxCons(expandedBindings, expandedBody, sym.SourceContext())
	tagAndRest := syntax.NewSyntaxCons(tagWithScope, bindsAndBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, tagAndRest, sym.SourceContext()), nil
}

// --- Shared helpers ---

// expandLetBindings expands init expressions in a binding list.
// scopeInits controls whether the binding scope is added to init expressions.
//
// Structural errors (non-pair bindings, non-symbol names, wrong arity) return
// the original syntax unchanged with nil bindingSyms. The validator reports
// precise structural errors downstream.
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

	// First pass: collect names with scope
	var scopedNames []*syntax.SyntaxSymbol
	var bindingPairs []*syntax.SyntaxPair
	rest := syntax.SyntaxValue(bindingsPair)
	for !syntax.IsSyntaxEmptyList(rest) {
		current, ok := rest.(*syntax.SyntaxPair)
		if !ok {
			// Improper bindings list — pass through for validator.
			return bindingsStx, nil, nil
		}

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
		scopedName := syntax.AddScopeToSyntax(nameSym, scope).(*syntax.SyntaxSymbol)
		scopedNames = append(scopedNames, scopedName)
		bindingPairs = append(bindingPairs, bPair)

		rest = current.SyntaxCdr()
	}

	// For letrec: create child env before expanding inits
	var initExpander *ExpanderTimeContinuation
	if scopeInits {
		childEnv := p.createBindingEnv(scopedNames)
		initExpander = p.newChildExpander(childEnv)
	} else {
		initExpander = p
	}

	// Second pass: expand each init and rebuild binding list
	var expandedBindingsList []syntax.SyntaxValue
	for i, bPair := range bindingPairs {
		initCdr := bPair.SyntaxCdr()
		initPair, ok := initCdr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(initPair) {
			return bindingsStx, nil, nil
		}
		// Binding must be exactly (name init) — reject extra elements
		// so the validator sees the original malformed form.
		if !syntax.IsSyntaxEmptyList(initPair.SyntaxCdr()) {
			return bindingsStx, nil, nil
		}
		initExpr := initPair.SyntaxCar()

		if scopeInits {
			initExpr = syntax.AddScopeToSyntax(initExpr, scope)
		}

		expandedInit, err := initExpander.ExpandExpression(initExpr)
		if err != nil {
			return nil, nil, wrapSourcedError(bPair.SourceContext(), werr.WrapForeignErrorf(err, "let: failed to expand init expression"))
		}

		sc := bPair.SourceContext()
		initList := syntax.SyntaxList(sc, expandedInit)
		rebuilt := syntax.NewSyntaxCons(scopedNames[i], initList, sc)
		expandedBindingsList = append(expandedBindingsList, rebuilt)
	}

	return syntax.SyntaxList(bindingsPair.SourceContext(), expandedBindingsList...), scopedNames, nil
}

// expandLetStarBindings expands init expressions sequentially for let*,
// creating the child environment incrementally so each init sees preceding bindings.
//
// Structural errors return the original syntax unchanged with nil bindingSyms.
// The validator reports precise structural errors downstream.
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

	childEnv := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0),
		p.env,
	)

	var scopedNames []*syntax.SyntaxSymbol
	var expandedBindingsList []syntax.SyntaxValue

	rest := syntax.SyntaxValue(bindingsPair)
	for !syntax.IsSyntaxEmptyList(rest) {
		current, ok := rest.(*syntax.SyntaxPair)
		if !ok {
			// Improper bindings list — pass through for validator.
			return bindingsStx, nil, nil
		}

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

		initCdr := bPair.SyntaxCdr()
		initPair, ok := initCdr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(initPair) {
			return bindingsStx, nil, nil
		}
		// Binding must be exactly (name init) — reject extra elements.
		if !syntax.IsSyntaxEmptyList(initPair.SyntaxCdr()) {
			return bindingsStx, nil, nil
		}
		initExpr := initPair.SyntaxCar()

		// Add scope to init so references can resolve to preceding bindings.
		scopedInit := syntax.AddScopeToSyntax(initExpr, scope)

		currentExpander := p.newChildExpander(childEnv)
		expandedInit, err := currentExpander.ExpandExpression(scopedInit)
		if err != nil {
			return nil, nil, wrapSourcedError(bPair.SourceContext(), werr.WrapForeignErrorf(err, "let*: failed to expand init expression"))
		}

		// Add scope to name AFTER expanding init
		scopedName := syntax.AddScopeToSyntax(nameSym, scope).(*syntax.SyntaxSymbol)
		scopedNames = append(scopedNames, scopedName)

		childEnv.MaybeCreateLocalBinding(
			scopedName.Sym, environment.BindingTypeVariable,
			scopedName.Scopes(), scopedName.SourceContext(),
		)

		sc := bPair.SourceContext()
		initList := syntax.SyntaxList(sc, expandedInit)
		rebuilt := syntax.NewSyntaxCons(scopedName, initList, sc)
		expandedBindingsList = append(expandedBindingsList, rebuilt)

		rest = current.SyntaxCdr()
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
		childEnv.MaybeCreateLocalBinding(
			s.Sym, environment.BindingTypeVariable,
			s.Scopes(), s.SourceContext(),
		)
	}
	return childEnv
}

// expandBindingBody expands a body syntax value in the given environment.
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
		return nil, wrapSourcedError(bodyStx.SourceContext(), werr.WrapForeignErrorf(err, "binding body: failed to collect expressions"))
	}

	childExpander := p.newChildExpander(env)
	expandedExprs, err := childExpander.ExpandBodyWithDefineSyntax(bodyExprs)
	if err != nil {
		return nil, wrapSourcedError(bodyStx.SourceContext(), werr.WrapForeignErrorf(err, "binding body: failed to expand"))
	}

	return syntax.SyntaxList(bodyPair.SourceContext(), expandedExprs...), nil
}

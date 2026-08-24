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

// expander_use_site.go holds the use-site scope registry and the pruning pass
// that strips registered use-site scopes from binder positions whose binding is
// visible outside the macro use that created it.
//
// The scope is minted per macro invocation by expandMacroInvocation (and by
// ExpandOnce, its hand-copy) and never flipped back off, which is what makes
// plain subset resolution refuse the nested-macro capture in
// match.TemplateDenotesPatternVariable. See
// memory/2026-08-20-use-site-scopes-impl.local.md.

import (
	"slices"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// useSiteScopeLog records the use-site scopes one expansion run minted. Shared
// by pointer with child expanders (newChildExpander), like depthGuard.
//
// A SET rather than a slice, because the only question asked of it is "did I
// mint this particular scope" — a membership test run once per scope on a
// binder identifier, never an enumeration.
//
// **Membership, never a tag.** The registry IS the mechanism, and a kind field
// on Scope would not be. Racket keeps a `kind` on its scopes, mints use-site
// scopes with kind 'use-site, and prunes by object identity against a box on
// the expand context — so a scope from make-syntax-introducer carries kind
// 'use-site and is never pruned, same tag and opposite behaviour. Wile has its
// own evidence that tags rot: one introduction scope is minted with a bare
// syntax.NewScope() and carries no label at all, so any Label == "intro" test is
// already wrong today. Do not add a kind and branch on it.
type useSiteScopeLog struct {
	scopes values.MapSet[*syntax.Scope]
}

// newUseSiteScope mints the scope one macro invocation stamps on its input, and
// registers it so pruning can recognise it later.
//
// Unlike the introduction scope, it is NOT flipped on the way out. That
// asymmetry is the whole mechanism: use-site syntax keeps the scope and
// macro-introduced syntax never acquires it, so an inner macro's pattern
// variable stops being a subset of an outer macro's introduced template
// identifier and plain subset resolution refuses the capture.
//
// The "use-site" label is diagnostic only, exactly like the "intro" and "lambda"
// labels beside it. The registry, not the label, is what pruning reads — see
// useSiteScopeLog.
func (p *ExpanderTimeContinuation) newUseSiteScope() *syntax.Scope {
	scope := syntax.NewScopeWithLabel("use-site")
	if p.useSiteScopes != nil {
		p.useSiteScopes.scopes.Set(scope)
	}
	return scope
}

// withUseSiteScope stamps a fresh registered use-site scope on a macro's input
// form.
func (p *ExpanderTimeContinuation) withUseSiteScope(inputForm syntax.SyntaxValue) syntax.SyntaxValue {
	return syntax.AddScopeToSyntax(inputForm, p.newUseSiteScope())
}

// isUseSiteScope reports whether this run minted scope as a use-site scope.
func (p *ExpanderTimeContinuation) isUseSiteScope(scope *syntax.Scope) bool {
	if p.useSiteScopes == nil {
		return false
	}
	return p.useSiteScopes.scopes.ContainsOne(scope)
}

// pruneUseSiteScopes strips this run's use-site scopes from the binder position
// of every definition in a macro's output, recursing through begin.
//
// **Why definitions and nothing else.** A use-site scope must be stripped
// exactly where a binding created inside a macro use is visible OUTSIDE it. A
// `let` or `lambda` binder needs no pruning: its binder and its references both
// arrive from the use site and both wear the scope, so they agree, which is why
//
//	(define-syntax bind-it (syntax-rules () ((_ v body) (let ((v 7)) body))))
//	(bind-it q (* q 3))
//
// is unaffected. A definition is the case that escapes:
//
//	(define-syntax def-five (syntax-rules () ((_ name) (define name 5))))
//	(def-five foo)
//	foo
//
// binds `foo` at {us} while the later top-level `foo` carries {}, and without
// this pass the reference cannot reach the binding.
//
// **Why here.** expandMacroInvocation ends with one recursion into
// ExpandExpression, so every macro output passes a single point before being
// re-expanded, and a definition that only surfaces after several expansions is
// pruned at its own level. A body pre-scan would see only the forms written in
// the body.
//
// Returns the input unchanged when nothing is stripped, which is every call
// while the registry is empty, so the pass allocates nothing until minting
// begins.
func (p *ExpanderTimeContinuation) pruneUseSiteScopes(stx syntax.SyntaxValue) syntax.SyntaxValue {
	if p.useSiteScopes == nil || len(p.useSiteScopes.scopes) == 0 {
		return stx
	}
	pair, ok := stx.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return stx
	}
	if isSyntaxFormWithKeyword(pair, "begin") {
		return p.pruneUseSiteScopesInBegin(pair)
	}
	return p.pruneDefinitionBinder(pair)
}

// pruneUseSiteScopesInBegin maps the pruner over a begin's forms, rebuilding the
// spine only when a form actually changed.
func (p *ExpanderTimeContinuation) pruneUseSiteScopesInBegin(pair *syntax.SyntaxPair) syntax.SyntaxValue {
	var forms []syntax.SyntaxValue
	changed := false
	cur := pair.SyntaxCdr()
	for {
		curPair, ok := cur.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(curPair) {
			break
		}
		form := curPair.SyntaxCar()
		pruned := p.pruneUseSiteScopes(form)
		if pruned != form {
			changed = true
		}
		forms = append(forms, pruned)
		cur = curPair.SyntaxCdr()
	}
	if !changed {
		return pair
	}
	// cur is the terminating cdr, so an improper tail survives rather than being
	// silently dropped.
	q := cur
	for _, form := range slices.Backward(forms) {
		q = syntax.NewSyntaxCons(form, q, form.SourceContext())
	}
	return syntax.NewSyntaxCons(pair.SyntaxCar(), q, pair.SourceContext())
}

// pruneDefinitionBinder strips use-site scopes from a definition form's binder.
//
// Handles the two `define` shapes and `define-syntax`. NOT handled, and named
// here rather than left to be discovered: define-values, define-record-type, and
// a library's export list, each of which also creates an externally visible
// binding. A missed form fails LOUDLY — the reference outside the expansion
// reports no such binding with compatible scopes — so this is a bug list rather
// than a hygiene hole, but it is a list.
//
// define-syntax is handled despite extractDefineName deliberately excluding it
// (macro bindings live in the expand environment, not the pre-declared runtime
// one). It has to be: a macro-generating macro is the shape this whole arc is
// about, and an unpruned `inner` in
//
//	(define-syntax outer (syntax-rules () ((_ name pat) (define-syntax name ...))))
//	(outer inner (_ x))
//
// would leave the generated macro's keyword carrying a scope no later reference
// to `inner` has, making the macro invisible at its own use sites.
func (p *ExpanderTimeContinuation) pruneDefinitionBinder(pair *syntax.SyntaxPair) syntax.SyntaxValue {
	keyword, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return pair
	}
	if keyword.Key() != "define" && keyword.Key() != "define-syntax" {
		return pair
	}
	cdrPair, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return pair
	}

	// (define name value) / (define-syntax name transformer)
	target := cdrPair.SyntaxCar()
	nameSym, ok := target.(*syntax.SyntaxSymbol)
	if ok {
		pruned := p.pruneBinderSymbol(nameSym)
		if pruned == nameSym {
			return pair
		}
		return syntax.NewSyntaxCons(
			keyword,
			syntax.NewSyntaxCons(pruned, cdrPair.SyntaxCdr(), cdrPair.SourceContext()),
			pair.SourceContext(),
		)
	}

	// (define (name args...) body...) — the binder is the head of the head.
	headPair, ok := target.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(headPair) {
		return pair
	}
	nameSym, ok = headPair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return pair
	}
	pruned := p.pruneBinderSymbol(nameSym)
	if pruned == nameSym {
		return pair
	}
	return syntax.NewSyntaxCons(
		keyword,
		syntax.NewSyntaxCons(
			syntax.NewSyntaxCons(pruned, headPair.SyntaxCdr(), headPair.SourceContext()),
			cdrPair.SyntaxCdr(),
			cdrPair.SourceContext(),
		),
		pair.SourceContext(),
	)
}

// pruneBinderSymbol removes every scope this run minted as a use-site scope from
// sym, returning sym itself when it wears none of them.
//
// Iterates the SYMBOL's scopes and asks the registry about each, rather than
// iterating the registry: a registry accumulates one entry per macro use over a
// whole expansion run, while an identifier carries a handful of scopes.
func (p *ExpanderTimeContinuation) pruneBinderSymbol(sym *syntax.SyntaxSymbol) *syntax.SyntaxSymbol {
	q := sym
	for _, s := range sym.Scopes() {
		if p.isUseSiteScope(s) {
			q = q.RemoveScope(s)
		}
	}
	return q
}

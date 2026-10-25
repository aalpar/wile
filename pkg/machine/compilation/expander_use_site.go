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
// memory/2026-08-20-use-site-scopes-impl.md.

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
// own evidence that tags rot: one of its three introduction-scope mints went
// unlabelled until a lint rule (noBareScopeMint) made the label total, and a
// Label == "intro" test written before then would have missed it silently. A
// label is diagnostics. Do not add a kind and branch on it.
type useSiteScopeLog struct {
	scopes values.MapSet[*syntax.Scope]

	// includes holds the include and include-ci heads pruneUseSiteScopes met at a
	// splice position of a transformer's output. expandIncludedFile prunes a
	// file's forms only for these; see markSplicedInclude.
	includes values.MapSet[*syntax.SyntaxSymbol]
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
// of every definition in a macro's output, recursing through begin,
// begin-for-syntax and cond-expand, and marks an include there for
// expandIncludedFile to prune (markSplicedInclude).
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
// **Why here.** Each transformer boundary (expandMacroInvocation,
// invokeERTransformer, ExpandOnce) calls this on its output before it is
// re-expanded, so a definition that only surfaces after several expansions is
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
	head, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return stx
	}
	switch headFormName(p.env, head) {
	case "begin", "begin-for-syntax":
		// Both splicing heads recurse into their forms. begin-for-syntax is here
		// and not in pruneDefinitionBinder's list because it binds nothing itself:
		// it is a route to definitions one phase up, and those definitions are
		// what need their binders pruned.
		return p.pruneUseSiteScopesInBegin(pair)
	case "cond-expand":
		return p.pruneUseSiteScopesInCondExpand(pair)
	case "include", "include-ci":
		return p.markSplicedInclude(pair, head)
	}
	return p.pruneDefinitionBinder(pair)
}

// markSplicedInclude records an include met at a splice position, so the file's
// forms are pruned as this boundary would have pruned them had they been
// written in place. The forms are not visible here: the file is read only when
// the include expands.
//
// **Splice position only.** An include a macro merely receives as an argument
// and places inside its own body, (let () f a) for f = (include "u.scm"), is in
// a definition context the macro created. Its forms keep the use-site scope, as
// a cond-expand or begin in that position does, so a define in the file cannot
// capture the macro's binder a. Pruning every included form, as expandIncludedFile
// did before, stripped the scope there too. This holds for an include the macro
// receives directly: under (m (pass (include ...))) pass's boundary marks it, and
// the prune strips every use-site scope the run minted, m's included, as it does
// for a plain (m (pass (define ...))). That is the pruner's rule, not the mark's.
//
// **The mark is the head's identity, not a scope.** The two fail in opposite
// directions. A lost mark leaves the forms unpruned, so a definition the include
// should expose is unbound: loud, and what TestIncludePassedThroughAMacro checks.
// A scope travels with the syntax wherever a later transformer moves it, so a
// mark that reached a non-splice position would prune there: the silent capture
// this exists to prevent. A scope would also be stamped onto every included form
// with the head's other scopes. The window the identity must survive is short:
// a marked include is the next form expanded, before any transformer sees it.
//
// The head is recorded as it stands, not cloned. A transformer can return one
// identifier in two places (an ER macro hands back the spine's identifiers
// unchanged), but a copy in a position that can hold a definition is inside a
// binding form, whose scope is added to its body before the include is reached,
// and adding a scope clones the head. Measured: an ER macro splicing one include
// at top level and again inside (let ((a 'macro)) (let () e a)) answers macro
// with a clone and without one.
func (p *ExpanderTimeContinuation) markSplicedInclude(pair *syntax.SyntaxPair, head *syntax.SyntaxSymbol) syntax.SyntaxValue {
	if p.useSiteScopes.includes == nil {
		p.useSiteScopes.includes = values.NewMapSet[*syntax.SyntaxSymbol](1)
	}
	p.useSiteScopes.includes.Set(head)
	return pair
}

// isSplicedInclude reports whether head is an include markSplicedInclude
// recorded.
func (p *ExpanderTimeContinuation) isSplicedInclude(head *syntax.SyntaxSymbol) bool {
	if p.useSiteScopes == nil {
		return false
	}
	return p.useSiteScopes.includes.ContainsOne(head)
}

// pruneUseSiteScopesInCondExpand prunes every clause body as a begin body.
func (p *ExpanderTimeContinuation) pruneUseSiteScopesInCondExpand(pair *syntax.SyntaxPair) syntax.SyntaxValue {
	var clauses []syntax.SyntaxValue
	cur := pair.SyntaxCdr()
	for {
		curPair, ok := cur.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(curPair) {
			break
		}
		clause := curPair.SyntaxCar()
		clausePair, ok := clause.(*syntax.SyntaxPair)
		if ok && !syntax.IsSyntaxEmptyList(clausePair) {
			clause = p.pruneUseSiteScopesInBegin(clausePair)
		}
		clauses = append(clauses, clause)
		cur = curPair.SyntaxCdr()
	}
	q := cur
	for _, c := range slices.Backward(clauses) {
		q = syntax.NewSyntaxCons(c, q, c.SourceContext())
	}
	return syntax.NewSyntaxCons(pair.SyntaxCar(), q, pair.SourceContext())
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
// Handles the two `define` shapes, `define-syntax` and `define-for-syntax`. NOT
// handled, and named here rather than left to be discovered: define-values,
// define-record-type, and a library's export list, each of which also creates an
// externally visible binding. A missed form fails LOUDLY — the reference outside
// the expansion reports no such binding with compatible scopes — so this is a bug
// list rather than a hygiene hole, but it is a list.
//
// define-values and define-record-type are now reachable by a THIRD route, and
// that is a widening of the bug list rather than a new defect: a macro expanding
// to (begin-for-syntax (define-values (a b) ...)) hits exactly the gap above, and
// it did so before this form was added here too. Measured on both sides of the
// change — refused either way, where Racket answers — so the residual is filed
// as its own ledger row and is not what this list's third entry bought.
//
// define-for-syntax is handled for the reason the whole pair exists. Its binder
// used to be created under the EMPTY scope set (runDefineForSyntax), which both
// leaked a macro-private name to later transformers and, by accident, kept a
// macro-SUPPLIED name reachable. Giving the binder its symbol's scopes closes the
// leak and breaks the supplied case; pruning it here restores the supplied case
// alone. Neither half is correct without the other: see the two gate rows in
// TestForSyntaxBinderScopes, whose r1 and r2 move in opposite directions.
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
	form := headFormName(p.env, keyword)
	if form != "define" && form != "define-syntax" && form != "define-for-syntax" {
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
	for s := range sym.Scopes().All() {
		if p.isUseSiteScope(s) {
			q = q.RemoveScope(s)
		}
	}
	return q
}

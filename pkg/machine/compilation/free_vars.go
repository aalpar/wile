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

// free_vars.go is Pass 1 of flat-closure conversion: the ordered free-variable
// set of a lambda body, decided by RESOLUTION rather than by a closure-boundary
// count.
//
// The boundary count was tried and is wrong in both directions.
// validate.WalkBindingRefs reports a depth that counts ESCAPING closure
// boundaries, and deliberately does not increment for an immediately-applied
// lambda or for a named-let loop lambda — yet both still emit a real
// OpMakeClosure. A depth > 0 predicate therefore omits exactly the variables
// those two shapes capture.

import (
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// freeVarKey identifies a free variable by WHERE IT RESOLVES in the enclosing
// compile-time frame, not by how it is spelled.
//
// Two same-spelled binders from different macro expansions resolve to different
// (slot, depth) pairs and therefore get different closure slots. Keying on
// Sym.Key would merge them, which is the binding-identity defect
// CLAUDE.local.md names — and it is invisible to every value assertion, because
// merging two variables that happen to hold the same value still evaluates
// correctly.
//
// The pair is taken against ONE frame (the lambda's enclosing env) in ONE pass,
// so it is stable: a *environment.Binding pointer would not be, because
// EnsureLocalBinding appends and can reallocate the frame's []Binding.
type freeVarKey struct {
	slot  int
	depth int
}

// freeVar is one slot of a lambda's flat-closure free vector.
//
// sym is retained for diagnostics and for the reflection names phase 7 reports;
// key is the identity. boxed is filled in by Pass 2 (boxing.go) and is always
// false as collectFreeVars leaves it.
type freeVar struct {
	sym *syntax.SyntaxSymbol
	key freeVarKey
	// abs is key resolved to the frame that OWNS the slot. key is relative to
	// the enclosing frame and so means something different to every reader; abs
	// is what a reference arbitrarily deep inside the body can be matched
	// against without re-basing.
	abs   boxedSlotKey
	boxed bool
}

// collectFreeVars returns the ordered free-variable set of a lambda body.
//
// enclosing is the compile-time frame the lambda is compiled UNDER —
// CompileTimeContinuation.env at compileClosureBody's entry — not the lambda's
// own childEnv. A reference is free iff:
//
//	(1) no binder introduced inside the lambda covers it, and
//	(2) it resolves to a LOCAL in enclosing.
//
// (2) is what excludes globals: a global reference compiles to a cached-binding
// or global load and never travels through the closure.
//
// Neither test consults a closure-boundary count, which is the whole point: an
// immediately-applied lambda and a named-let loop lambda are simply nested
// lambdas here and need no exemption.
//
// The set is closed transitively — nested lambda bodies are walked with their
// own binders pushed — so a variable free in a nested lambda and not bound by
// this one appears here too. The creation protocol depends on that closure:
// when the nested closure is built, its value must be pushed from THIS lambda's
// free vector, and there would be no slot to push from otherwise.
//
// Order is first-reference-in-source order. It fixes the free vector's layout,
// so it must be deterministic — do not reach for a map to build q.
func collectFreeVars(v validate.ValidatedBodyAndParams, enclosing *environment.EnvironmentFrame) []freeVar {
	if enclosing == nil {
		return nil
	}
	c := &freeVarCollector{
		enclosing: enclosing,
		seen:      make(map[freeVarKey]bool),
	}
	c.walkProc(v)
	return c.q
}

// freeVarCollector carries the binder stack and the accumulated layout across
// the walk.
type freeVarCollector struct {
	enclosing *environment.EnvironmentFrame
	binders   [][]*syntax.SyntaxSymbol
	seen      map[freeVarKey]bool
	q         []freeVar
}

func (p *freeVarCollector) pushBinders(bs []*syntax.SyntaxSymbol) {
	p.binders = append(p.binders, bs)
}

func (p *freeVarCollector) popBinders() {
	p.binders = p.binders[:len(p.binders)-1]
}

// procBinders returns the binders a lambda, case-lambda clause, or function-form
// define introduces: the required parameters and the rest parameter.
//
// The nil guard is NOT the zero-arg case: validateParams returns a non-nil empty
// *ValidatedParams for `()`, so `(case-lambda (() e))` has Params() != nil.
// (compile_closure.go's Phase 1 comment claims otherwise and is stale.) Params()
// is nil for a ValidatedDefine's VALUE form, whose validatedProcBase is left
// zero-valued, and for a form whose parameter list failed validation — both
// validateLambda and validateCaseLambdaClause store the nil unconditionally,
// alongside a non-empty result.Errors. Neither reaches walkProc on a clean
// compile; the guard keeps the collector total, so a walk over a
// partially-validated tree returns a wrong-but-safe free set instead of
// panicking.
func procBinders(v validate.ValidatedBodyAndParams) []*syntax.SyntaxSymbol {
	ps := v.Params()
	if ps == nil {
		return nil
	}
	q := make([]*syntax.SyntaxSymbol, 0, len(ps.Required)+1)
	q = append(q, ps.Required...)
	if ps.Rest != nil {
		q = append(q, ps.Rest)
	}
	return q
}

// visit records sym as a free variable if it is neither bound inside the lambda
// nor resolved to a global.
func (p *freeVarCollector) visit(sym *syntax.SyntaxSymbol) {
	if sym == nil {
		return
	}
	if p.boundInside(sym) {
		return
	}
	li := p.enclosing.GetLocalIndex(sym.Sym, syntax.ScopesOf(sym.Scopes()))
	if li == nil {
		return
	}
	k := freeVarKey{slot: li.Over(), depth: li.Up()}
	if p.seen[k] {
		return
	}
	abs, ok := absoluteSlot(p.enclosing, li)
	if !ok {
		return
	}
	p.seen[k] = true
	p.q = append(p.q, freeVar{sym: sym, key: k, abs: abs})
}

// boundInside reports whether some binder introduced within the lambda covers
// sym, searching innermost-out so shadowing resolves naturally.
//
// The Key comparison NARROWS the candidate set — the binder stack is a name
// table, exactly as an environment frame is. ScopesCompatible DECIDES, asking
// bindingScopes ⊆ useScopes, the same subset test GetLocalIndex applies.
// Dropping the scope test would make this a spelling comparison and merge a
// macro-introduced binder with a user identifier of the same name.
func (p *freeVarCollector) boundInside(sym *syntax.SyntaxSymbol) bool {
	use := sym.Scopes()
	for _, frame := range slices.Backward(p.binders) {
		for _, b := range frame {
			if b == nil || b.Sym.Key != sym.Sym.Key {
				continue
			}
			if syntax.ScopesCompatible(b.Scopes(), use) {
				return true
			}
		}
	}
	return false
}

// walk visits every symbol reference in expr, maintaining the binder stack.
//
// FOUR forms bind, and they are the whole list: ValidatedLambda,
// ValidatedCaseLambda, ValidatedLet (all LetKinds, plus the named-let Tag) and
// ValidatedDefine (an internal define's name, and its parameters in the function
// form). Everything else delegates to WalkSubExprs. If a fifth binding form is
// added to pkg/internal/validate it must be added here; nothing enforces that
// automatically, which is what TestCollectFreeVarsCoversEveryBinder is for.
func (p *freeVarCollector) walk(expr validate.ValidatedExpr) {
	if expr == nil {
		return
	}

	sym, ok := expr.(*validate.ValidatedSymbol)
	if ok {
		p.visit(sym.Symbol)
		return
	}

	setBang, ok := expr.(*validate.ValidatedSetBang)
	if ok {
		// The target is a reference to the variable, same as any read: a set!
		// through a closure must reach the same slot the reads do.
		p.visit(setBang.Name)
		p.walk(setBang.SubExp())
		return
	}

	lam, ok := expr.(*validate.ValidatedLambda)
	if ok {
		p.walkProc(lam)
		return
	}

	cl, ok := expr.(*validate.ValidatedCaseLambda)
	if ok {
		for _, clause := range cl.Clauses() {
			p.walkProc(clause)
		}
		return
	}

	let, ok := expr.(*validate.ValidatedLet)
	if ok {
		p.walkLet(let)
		return
	}

	def, ok := expr.(*validate.ValidatedDefine)
	if ok {
		p.walkDefine(def)
		return
	}

	validate.WalkSubExprs(expr, func(child validate.ValidatedExpr, _ validate.ChildRole) {
		p.walk(child)
	})
}

// walkProc walks a lambda, case-lambda clause, or function-form define body with
// its own parameters pushed.
func (p *freeVarCollector) walkProc(v validate.ValidatedBodyAndParams) {
	p.pushBinders(procBinders(v))
	for _, e := range v.Body() {
		p.walk(e)
	}
	p.popBinders()
}

// walkLet walks inits with the let's OWN binders NOT pushed, and the body with
// them pushed.
//
// That is exact for let and let*, and deliberately conservative for letrec and
// letrec*: a letrec init's reference to a sibling binding is not masked here, so
// it is resolved against the enclosing frame instead. It resolves to nothing
// (the sibling is a let-local, not a member of enclosing) and is dropped, or —
// if an outer local happens to share the spelling AND the scopes — it adds one
// spurious free slot. A spurious slot is dead weight: the compiled body loads
// that variable as a local, never through the free vector. The opposite error is
// not survivable: masking `(let ((x x)) …)`'s init would DROP a genuinely free x.
// Over-approximate here, never under.
//
// The named-let Tag is pushed for the inits too, because it is the loop
// procedure's own binder and the init IS the loop lambda.
func (p *freeVarCollector) walkLet(v *validate.ValidatedLet) {
	var tag []*syntax.SyntaxSymbol
	if v.Tag != nil {
		tag = []*syntax.SyntaxSymbol{v.Tag}
	}
	p.pushBinders(tag)
	for i := range v.Bindings {
		p.walk(v.Bindings[i].Init)
	}
	names := make([]*syntax.SyntaxSymbol, 0, len(v.Bindings))
	for i := range v.Bindings {
		names = append(names, v.Bindings[i].Name)
	}
	p.pushBinders(names)
	for _, e := range v.Body() {
		p.walk(e)
	}
	p.popBinders()
	p.popBinders()
}

// walkDefine handles an internal define. The name binds in the ENCLOSING body
// (letrec* semantics, R7RS §5.3.2), so it is pushed onto the frame already on
// top of the stack rather than onto a new one.
func (p *freeVarCollector) walkDefine(v *validate.ValidatedDefine) {
	top := len(p.binders) - 1
	if top >= 0 {
		p.binders[top] = append(p.binders[top], v.Name())
	}
	if v.IsFunction {
		p.walkProc(v)
		return
	}
	p.walk(v.SubExp())
}

// freeVarNames projects a free-variable layout down to the symbols reflection
// reports, preserving slot order.
func freeVarNames(fvs []freeVar) []*values.Symbol {
	if len(fvs) == 0 {
		return nil
	}
	q := make([]*values.Symbol, len(fvs))
	for i, fv := range fvs {
		q[i] = fv.sym.Sym
	}
	return q
}

// bodyReadsThroughFrameChain reports whether body contains an OPAQUE SUBTREE:
// a quasiquote template, or a passthrough form parked in a ValidatedLiteral
// (cond-expand, include, let-syntax, with-syntax, a `syntax` template …).
//
// WHY THIS GATES THE STATIC LINK. Pass 1 decides free-variable membership over
// the VALIDATED tree, and an opaque subtree is raw syntax this package never
// looks inside — pkg/internal/validate's own opaque_subtree.go documents the
// same blindness and takes the same stance, that an un-analysed subtree counts
// as unsafe. A `syntax` template's pattern-variable references are exactly such
// a reference: they resolve at run time through a frame BindPatternVars pushed,
// and they appear in no free layout because nothing walked them. Narrowing such
// a closure's link to the lexical root makes that frame unreachable, which is
// how "no syntax-case pattern-variable frame in scope" and "no such local
// binding 1:1" showed up the moment the link moved.
//
// The answer is transitive for free: WalkSubExprs descends into nested lambda
// bodies, so a template whose INNER lambda hides an opaque subtree is flagged
// too — which it must be, or the chain the inner closure needs is already
// severed above it.
//
// Refusing to narrow costs the space win on these bodies and nothing else. The
// opposite error is a wrong answer, and a loud one: the read faults rather than
// returning the wrong value.
func bodyReadsThroughFrameChain(body []validate.ValidatedExpr) bool {
	found := false
	var walk func(e validate.ValidatedExpr)
	walk = func(e validate.ValidatedExpr) {
		if found || e == nil {
			return
		}
		if isOpaqueSubtree(e) {
			found = true
			return
		}
		validate.WalkSubExprs(e, func(child validate.ValidatedExpr, _ validate.ChildRole) {
			walk(child)
		})
	}
	for _, e := range body {
		walk(e)
	}
	return found
}

// isOpaqueSubtree mirrors validate's own classification of the two shapes that
// reach compilation as un-analysed code.
//
// A *ValidatedLiteral is overloaded: genuine self-evaluating data (numbers,
// strings, booleans, the empty list) AND passthrough forms. Only the latter
// conceal code, and a form is a non-empty syntax pair — self-evaluating data
// never is.
func isOpaqueSubtree(expr validate.ValidatedExpr) bool {
	_, isQuasi := expr.(*validate.ValidatedQuasiquote)
	if isQuasi {
		return true
	}
	lit, ok := expr.(*validate.ValidatedLiteral)
	if !ok {
		return false
	}
	pair, ok := lit.Value.(*syntax.SyntaxPair)
	return ok && !pair.IsEmptyList()
}

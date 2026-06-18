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

package validate

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

// captureOperatorNames are the core primitives that capture or abort a
// continuation. Detection is gated on resolved binding identity (see
// makeIsCaptureOp), not the name alone, so a locally-shadowed or user-redefined
// symbol of the same name is not mistaken for the primitive.
var captureOperatorNames = map[string]struct{}{
	"call-with-current-continuation":    {},
	"call/cc":                           {},
	"call-with-composable-continuation": {},
	"abort-current-continuation":        {},
}

// captureSafePrimitiveNames is the sound-by-default whitelist (plan Q-1): core
// primitives that cannot invoke a Scheme procedure and therefore cannot
// transitively capture. An UNLISTED primitive is treated as unsafe (unresolved
// edge ⇒ heap). Membership is confirmed against an imported binding before it
// is trusted, so a same-unit redefinition (caught earlier as a top-level node)
// or a non-imported shadow does not slip through. Grow this set deliberately;
// the principled long-term replacement is a PrimitiveSpec capability field
// (logged as a follow-up).
var captureSafePrimitiveNames = map[string]struct{}{
	"+": {}, "-": {}, "*": {}, "/": {},
	"=": {}, "<": {}, ">": {}, "<=": {}, ">=": {},
	"zero?": {}, "null?": {}, "pair?": {}, "not": {},
	"cons": {}, "car": {}, "cdr": {},
	// extend as measured need arises
}

// IsCaptureSafePrimitiveName reports whether name is a core primitive the
// frame-reclaim classifier trusts as capture-safe — it cannot invoke a Scheme
// procedure or capture a continuation. This is the authoritative set for two
// callers: the classifier's own trust gate (classifyCallee) and the registry's
// WithStableBasePrimitives freeze. Only these primitives are ever trusted, so
// only these need stamping Stable under the optimization flag — stamping any
// other primitive would freeze it for no classifier benefit.
func IsCaptureSafePrimitiveName(name string) bool {
	_, ok := captureSafePrimitiveNames[name]
	return ok
}

// makeIsCaptureOp returns a fail-safe capture-operator identity test bound to
// env. A symbol is a capture operator iff its name is a capture-operator name
// AND it resolves to an imported binding. Hygiene is delegated to
// env.GetBinding (scope-aware, two-phase): a locally-shadowed call/cc resolves
// to a non-imported local and returns false. A name the test cannot confirm
// returns false — soundness does not rest on this predicate, because the
// edge-resolution path treats any non-whitelisted operator as unsafe anyway;
// this check is the explicit, defense-in-depth marker for a body that captures.
func makeIsCaptureOp(env *environment.EnvironmentFrame) func(*syntax.SyntaxSymbol) bool {
	return func(sym *syntax.SyntaxSymbol) bool {
		if env == nil {
			return false
		}
		_, named := captureOperatorNames[sym.Sym.Key]
		if !named {
			return false
		}
		b := env.GetBinding(sym.Sym, sym.Scopes())
		return b != nil && b.IsImported()
	}
}

// ClassifyFrameReclaim returns, for every top-level function define in unit,
// whether its frame is reclaimable at its tail calls. It is the exported entry
// point over the Layer-B/C internals: build the call graph, run the
// greatest-fixpoint mayCapture, and project to a name→verdict map. The verdict
// is conservative (any uncertainty ⇒ not reclaimable).
//
// Same-unit-define immutability is read from the producer's Stable bit via
// env.GetBinding(...).IsStable() — the single source of truth, backed by
// Option-B enforcement (the set!-gate + the cross-unit redefine guard). The
// tier-(a) vs tier-(b) distinction is therefore a property of WHICH env is
// passed, not a classifier argument: an env whose same-unit defines are NOT
// stamped Stable (WithImmutableTopLevel off, or a non-compiled env) yields
// mutable same-unit edges (tier-(a) behavior); an env whose defines ARE Stable
// (WithImmutableTopLevel on, compiled) yields immutable edges (tier-(b)).
//
// PRECONDITION: env must be the post-compile env for unit (bindings stamped). An
// un-stamped (validate-only) env reports IsStable()==false uniformly, yielding
// the conservative tier-(a) verdict with no diagnostic — sound for the optimizer
// but a silent artifact for a measurement (guard with a positive control).
func ClassifyFrameReclaim(
	unit []ValidatedExpr,
	env *environment.EnvironmentFrame,
) map[string]bool {
	nodes, byName := buildReclaimGraph(unit, env)
	verdict := mayCapture(nodes)
	out := make(map[string]bool, len(byName))
	for name, n := range byName {
		out[name] = frameReclaimable(n, verdict)
	}
	return out
}

// buildReclaimGraph constructs the reclaim call graph for a validated unit. Each
// top-level function define becomes a reclaimNode; its structural capture facts
// come from the Layer A predicates and each constraining call site is resolved
// to an edge.
//
// A call to another top-level define yields an edge whose immutability is the
// callee binding's IsStable() in env. Imported capture-safe primitives
// contribute no edge.
func buildReclaimGraph(
	unit []ValidatedExpr,
	env *environment.EnvironmentFrame,
) ([]*reclaimNode, map[string]*reclaimNode) {
	isCaptureOp := makeIsCaptureOp(env)

	byName := make(map[string]*reclaimNode)
	var defs []*ValidatedDefine
	var defNodes []*reclaimNode

	// Pass 1: a node per top-level function define, with its structural facts.
	collectTopLevelDefines(unit, func(d *ValidatedDefine) {
		if !d.IsFunction {
			return
		}
		name := d.Name().Sym.Key
		n := &reclaimNode{
			label:             name,
			referencesCapture: bodyReferencesCaptureOperator(d.Body(), isCaptureOp),
			createsEscaping:   bodyCreatesEscapingClosure(d.Body()),
		}
		byName[name] = n
		defs = append(defs, d)
		defNodes = append(defNodes, n)
	})

	// Pass 2: resolve each define's constraining call sites to edges (needs the
	// full node set so same-unit callees resolve).
	for i, d := range defs {
		defNodes[i].callees = resolveCallEdges(d, env, byName)
	}

	nodes := make([]*reclaimNode, 0, len(byName))
	for _, n := range byName {
		nodes = append(nodes, n)
	}
	return nodes, byName
}

// collectTopLevelDefines invokes fn for each *ValidatedDefine directly in the
// unit. A top-level (begin ...) is flattened one level (R7RS top-level body
// semantics).
func collectTopLevelDefines(unit []ValidatedExpr, fn func(*ValidatedDefine)) {
	for _, e := range unit {
		switch v := e.(type) {
		case *ValidatedDefine:
			fn(v)
		case *ValidatedBegin:
			collectTopLevelDefines(v.Body(), fn)
		}
	}
}

// resolveCallEdges walks d's body and emits one reclaimEdge per call site that
// imposes a capture constraint. A call to a capture-safe primitive imposes no
// constraint and contributes no edge, so a function calling only such
// primitives has zero edges and is trivially safe.
//
// The walk threads a nameSet of locally-bound (shadowing) names so an operator
// shadowed by an enclosing lambda/let/internal-define is not mistaken for the
// same-Key top-level define or primitive (OQ-1). The seed is d's OWN parameters;
// d's own name is deliberately NOT seeded so a self-recursive call resolves to
// this top-level node.
func resolveCallEdges(
	d *ValidatedDefine,
	env *environment.EnvironmentFrame,
	byName map[string]*reclaimNode,
) []reclaimEdge {
	var edges []reclaimEdge
	collect := func(proc ValidatedExpr, bound nameSet) {
		edge, constrains := classifyCallee(proc, env, byName, bound)
		if constrains {
			edges = append(edges, edge)
		}
	}
	walkBodySeq(d.Body(), nameSet(nil).withParams(d.Params()), collect)
	return edges
}

// classifyCallee maps a call operator to (edge, constrains) under the set of
// names lexically shadowed at the call site. constrains==false means the call
// imposes no capture constraint (a confirmed capture-safe primitive) and
// contributes no edge.
func classifyCallee(
	proc ValidatedExpr,
	env *environment.EnvironmentFrame,
	byName map[string]*reclaimNode,
	bound nameSet,
) (reclaimEdge, bool) {
	sym, ok := proc.(*ValidatedSymbol)
	if !ok {
		return reclaimEdge{target: nil}, true // computed operator ⇒ unresolved ⇒ unsafe
	}
	name := sym.Symbol.Sym.Key

	// Lexical shadow guard (OQ-1): an operator bound by an enclosing local scope
	// resolves to that local, NOT the same-Key top-level define or imported
	// primitive. The classifier resolves against a flat env with no local frames,
	// so without this guard env.GetBinding(name) would return the global (Stable)
	// binding and mark the edge immutable — a false positive at the wrong callee
	// (e.g. (define (use h) (let ((sq h)) (sq 3))) with a Stable top-level sq).
	// A shadowed operator is therefore unresolved ⇒ unsafe.
	if bound.has(name) {
		return reclaimEdge{target: nil}, true
	}

	// Same-unit top-level define: resolvable. Its edge is immutable iff the
	// callee binding IsStable() — the producer's Stable bit, the single source
	// of truth (StableInUnit = defined-once ∧ never-set!, made sound by Option-B
	// enforcement). A non-stamped binding (flag off, or non-compiled env) ⇒
	// mutable edge ⇒ unsafe, the sound tier-(a) default. Mirror the primitive
	// lookup below: assignment then test, not a compound if.
	target, ok := byName[name]
	if ok {
		b := env.GetBinding(sym.Symbol.Sym, sym.Symbol.Scopes())
		immutable := b != nil && b.IsStable()
		return reclaimEdge{target: target, immutable: immutable}, true
	}

	// Capture-safe core primitive, confirmed non-rebindable: no constraint, no
	// edge. The gate is IsStable() (Imported ∨ Stable): an imported primitive is
	// immutable by R7RS, and under WithStableBasePrimitives an ambient base
	// primitive is stamped Stable (Phase 2.6) — both are non-rebindable, so a
	// call to either cannot be hijacked into a capturing procedure. A
	// non-stable shadow of the same name (set!-able, or no opt-in) is rejected
	// and falls through to the unsafe default below.
	_, safe := captureSafePrimitiveNames[name]
	if safe {
		b := env.GetBinding(sym.Symbol.Sym, sym.Symbol.Scopes())
		if b != nil && b.IsStable() {
			return reclaimEdge{}, false
		}
	}

	return reclaimEdge{target: nil}, true // unknown ⇒ unresolved ⇒ unsafe
}

// walkCallSites invokes fn with the operator of every ValidatedCall and
// ValidatedApply reachable from expr, paired with the nameSet of names lexically
// shadowed (locally bound) at that site. Binding forms (lambda, case-lambda,
// let, internal define) extend the shadow set for their sub-scopes; every other
// form descends with the inherited set.
func walkCallSites(expr ValidatedExpr, bound nameSet, fn func(proc ValidatedExpr, bound nameSet)) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *ValidatedCall:
		fn(e.Proc(), bound)
		for _, arg := range e.Body() {
			walkCallSites(arg, bound, fn)
		}

	case *ValidatedApply:
		fn(e.Proc, bound)
		for _, arg := range e.PrefixArgs {
			walkCallSites(arg, bound, fn)
		}
		walkCallSites(e.FinalList, bound, fn)

	case *ValidatedLambda:
		walkBodySeq(e.Body(), bound.withParams(e.Params()), fn)

	case *ValidatedCaseLambda:
		for _, clause := range e.Clauses() {
			walkBodySeq(clause.Body(), bound.withParams(clause.Params()), fn)
		}

	case *ValidatedLet:
		// Conservative: the let's bound names shadow in the body AND (over-
		// approximating plain-let scoping) in the inits. Over-approximation only
		// drops a same-name-init edge to unsafe (a leak, sound), never the reverse.
		inner := bound.withLetBindings(e.Bindings)
		for _, b := range e.Bindings {
			walkCallSites(b.Init, inner, fn)
		}
		walkBodySeq(e.Body(), inner, fn)

	case *ValidatedBegin:
		// A begin in a body context can splice internal defines into the sequence.
		walkBodySeq(e.Body(), bound, fn)

	case *ValidatedDefine:
		if e.IsFunction {
			walkBodySeq(e.Body(), bound.withParams(e.Params()), fn)
		} else {
			walkCallSites(e.SubExp(), bound, fn)
		}

	default:
		WalkSubExprs(expr, func(child ValidatedExpr, _ ChildRole) {
			walkCallSites(child, bound, fn)
		})
	}
}

// walkBodySeq walks a body sequence, first hoisting every internal define's name
// into scope (letrec* — internal defines are mutually visible across the whole
// body, including their own bodies and earlier siblings), then descending each
// expression with that augmented shadow set.
func walkBodySeq(body []ValidatedExpr, bound nameSet, fn func(proc ValidatedExpr, bound nameSet)) {
	var defined []string
	for _, e := range body {
		d, ok := e.(*ValidatedDefine)
		if ok {
			defined = append(defined, d.Name().Sym.Key)
		}
	}
	inner := bound
	if len(defined) > 0 {
		inner = bound.with(defined...)
	}
	for _, e := range body {
		walkCallSites(e, inner, fn)
	}
}

// nameSet is a set of identifier Keys lexically bound in an enclosing local
// scope. A call operator whose name is in the set is shadowed by a local binding
// and therefore is NOT the same-Key top-level define or imported primitive.
type nameSet map[string]struct{}

// with returns a copy of s extended with names. Copy-on-extend keeps sibling
// scopes independent — a name bound in one let does not leak to its siblings.
func (s nameSet) with(names ...string) nameSet {
	out := make(nameSet, len(s)+len(names))
	for k := range s {
		out[k] = struct{}{}
	}
	for _, n := range names {
		out[n] = struct{}{}
	}
	return out
}

// withParams returns a copy of s extended with a parameter list's required and
// rest names. A nil params list returns s unchanged.
func (s nameSet) withParams(p *ValidatedParams) nameSet {
	if p == nil {
		return s
	}
	names := make([]string, 0, len(p.Required)+1)
	for _, req := range p.Required {
		names = append(names, req.Sym.Key)
	}
	if p.Rest != nil {
		names = append(names, p.Rest.Sym.Key)
	}
	return s.with(names...)
}

// withLetBindings returns a copy of s extended with a let form's bound names.
func (s nameSet) withLetBindings(bindings []ValidatedLetBinding) nameSet {
	if len(bindings) == 0 {
		return s
	}
	names := make([]string, 0, len(bindings))
	for _, b := range bindings {
		names = append(names, b.Name.Sym.Key)
	}
	return s.with(names...)
}

// has reports whether name is lexically shadowed in this scope.
func (s nameSet) has(name string) bool {
	_, ok := s[name]
	return ok
}

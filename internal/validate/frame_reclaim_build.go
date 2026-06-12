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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
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

// buildReclaimGraph constructs the reclaim call graph for a validated unit.
// Each top-level function define becomes a reclaimNode; its structural capture
// facts come from the Layer A predicates and each constraining call site is
// resolved to an edge.
//
// Tier-(a) immutability ONLY (sound today): a callee is immutable iff it is an
// imported binding (a capture-safe primitive — no edge) or a local binding
// (not modelled in this pass). A call to another top-level define is treated as
// NOT immutable (cross-unit redefinition / cross-thread set! — the L3 hazard);
// it yields a mutable edge ⇒ unsafe. This is the expected "Low on Gabriel"
// tier-(a) result. The optimistic top-level tier and the unit-closure proof are
// out of scope (sibling plan Phases 2/7).
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
		defNodes[i].callees = resolveCallEdges(d.Body(), env, byName)
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

// resolveCallEdges walks body and emits one reclaimEdge per call site that
// imposes a capture constraint. A call to a capture-safe primitive imposes no
// constraint and contributes no edge, so a function calling only such
// primitives has zero edges and is trivially safe.
func resolveCallEdges(
	body []ValidatedExpr,
	env *environment.EnvironmentFrame,
	byName map[string]*reclaimNode,
) []reclaimEdge {
	var edges []reclaimEdge
	for _, e := range body {
		walkCallSites(e, func(proc ValidatedExpr) {
			edge, constrains := classifyCallee(proc, env, byName)
			if constrains {
				edges = append(edges, edge)
			}
		})
	}
	return edges
}

// classifyCallee maps a call operator to (edge, constrains) under tier-(a)
// rules. constrains==false means the call imposes no capture constraint (a
// confirmed capture-safe primitive) and contributes no edge.
func classifyCallee(
	proc ValidatedExpr,
	env *environment.EnvironmentFrame,
	byName map[string]*reclaimNode,
) (reclaimEdge, bool) {
	sym, ok := proc.(*ValidatedSymbol)
	if !ok {
		return reclaimEdge{target: nil}, true // computed operator ⇒ unresolved ⇒ unsafe
	}
	name := sym.Symbol.Sym.Key

	// Same-unit top-level define: resolvable, but tier-(a) treats it as NOT
	// immutable (a global that could be rebound). Mutable edge ⇒ unsafe.
	target, ok := byName[name]
	if ok {
		return reclaimEdge{target: target, immutable: false}, true
	}

	// Capture-safe core primitive, confirmed imported: no constraint, no edge.
	// The IsImported gate rejects a non-imported shadow of the same name.
	_, safe := captureSafePrimitiveNames[name]
	if safe {
		b := env.GetBinding(sym.Symbol.Sym, sym.Symbol.Scopes())
		if b != nil && b.IsImported() {
			return reclaimEdge{}, false
		}
	}

	return reclaimEdge{target: nil}, true // unknown ⇒ unresolved ⇒ unsafe
}

// walkCallSites invokes fn with the operator of every ValidatedCall and
// ValidatedApply reachable from expr (including nested and closure-body calls).
func walkCallSites(expr ValidatedExpr, fn func(proc ValidatedExpr)) {
	if expr == nil {
		return
	}

	call, ok := expr.(*ValidatedCall)
	if ok {
		fn(call.Proc())
	}
	app, ok := expr.(*ValidatedApply)
	if ok {
		fn(app.Proc)
	}

	WalkSubExprs(expr, func(child ValidatedExpr, _ ChildRole) {
		walkCallSites(child, fn)
	})
}

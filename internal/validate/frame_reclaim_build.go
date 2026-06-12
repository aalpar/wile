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

// ReclaimTier selects how a same-unit top-level define is treated for
// immutability — the one axis the classifier's precision turns on. It exists so
// the Phase-2 measurement harness can price the hard top-level-immutability
// analysis (sibling plan Phase 7) before it is built, by classifying the same
// units under both tiers and comparing recovered allocation volume.
type ReclaimTier int

const (
	// TierLocal (tier a) is sound TODAY with no new analysis: a call to another
	// top-level define is treated as NOT immutable (it can be redefined / set!
	// cross-unit / cross-thread — the L3 hazard), yielding a mutable edge ⇒
	// unsafe. Only imported primitives and local-by-construction bindings are
	// immutable. This is the "Low on Gabriel" baseline.
	TierLocal ReclaimTier = iota

	// TierTopLevel (tier b) is the OPTIMISTIC, not-yet-proven tier the harness
	// measures: a same-unit top-level define is treated as immutable UNLESS it is
	// set! somewhere in the unit. This models the payoff of a future defined-once
	// ∧ never-set! ∧ unit-closed proof (Phase 7) without building it. It is NOT
	// sound for codegen as-is (separate compilation / eval / load can still
	// rebind a global); it exists only to quantify the gap.
	TierTopLevel
)

// ClassifyFrameReclaim returns, for every top-level function define in unit,
// whether its frame is reclaimable at its tail calls under the given tier. It is
// the exported entry point over the Layer-B/C internals: build the call graph
// under tier, run the greatest-fixpoint mayCapture, and project to a name→verdict
// map. The verdict is conservative (any uncertainty ⇒ not reclaimable) under
// both tiers; the tiers differ only in whether same-unit top-level-define edges
// are immutable.
func ClassifyFrameReclaim(
	unit []ValidatedExpr,
	env *environment.EnvironmentFrame,
	tier ReclaimTier,
) map[string]bool {
	nodes, byName := buildReclaimGraphTier(unit, env, tier)
	verdict := mayCapture(nodes)
	out := make(map[string]bool, len(byName))
	for name, n := range byName {
		out[name] = frameReclaimable(n, verdict)
	}
	return out
}

// buildReclaimGraph constructs the tier-(a) reclaim call graph for a validated
// unit. It is the sound-today entry point used by the Layer-C tests; see
// buildReclaimGraphTier for the tier-parameterized form.
func buildReclaimGraph(
	unit []ValidatedExpr,
	env *environment.EnvironmentFrame,
) ([]*reclaimNode, map[string]*reclaimNode) {
	return buildReclaimGraphTier(unit, env, TierLocal)
}

// buildReclaimGraphTier constructs the reclaim call graph for a validated unit
// under the given immutability tier. Each top-level function define becomes a
// reclaimNode; its structural capture facts come from the Layer A predicates and
// each constraining call site is resolved to an edge.
//
// Under TierLocal a call to another top-level define yields a mutable edge ⇒
// unsafe. Under TierTopLevel the same edge is immutable unless the callee is
// set! anywhere in the unit (collected up front). Imported capture-safe
// primitives contribute no edge under either tier.
func buildReclaimGraphTier(
	unit []ValidatedExpr,
	env *environment.EnvironmentFrame,
	tier ReclaimTier,
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

	// A top-level define that is set! anywhere in the unit is not immutable even
	// under TierTopLevel. Empty under TierLocal (every top-level edge is mutable
	// there regardless).
	mutated := map[string]bool{}
	if tier == TierTopLevel {
		mutated = collectMutatedTopLevelNames(unit, byName)
	}

	// Pass 2: resolve each define's constraining call sites to edges (needs the
	// full node set so same-unit callees resolve).
	for i, d := range defs {
		defNodes[i].callees = resolveCallEdges(d.Body(), env, byName, tier, mutated)
	}

	nodes := make([]*reclaimNode, 0, len(byName))
	for _, n := range byName {
		nodes = append(nodes, n)
	}
	return nodes, byName
}

// collectMutatedTopLevelNames returns the set of top-level define names that are
// the target of a set! anywhere in the unit (including inside other function
// bodies). These names are not immutable even under TierTopLevel.
func collectMutatedTopLevelNames(
	unit []ValidatedExpr,
	byName map[string]*reclaimNode,
) map[string]bool {
	mutated := make(map[string]bool)
	var walk func(e ValidatedExpr)
	walk = func(e ValidatedExpr) {
		if e == nil {
			return
		}
		sb, ok := e.(*ValidatedSetBang)
		if ok && sb.Name != nil {
			name := sb.Name.Sym.Key
			_, isTop := byName[name]
			if isTop {
				mutated[name] = true
			}
		}
		WalkSubExprs(e, func(child ValidatedExpr, _ ChildRole) {
			walk(child)
		})
	}
	for _, e := range unit {
		walk(e)
	}
	return mutated
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
	tier ReclaimTier,
	mutated map[string]bool,
) []reclaimEdge {
	var edges []reclaimEdge
	for _, e := range body {
		walkCallSites(e, func(proc ValidatedExpr) {
			edge, constrains := classifyCallee(proc, env, byName, tier, mutated)
			if constrains {
				edges = append(edges, edge)
			}
		})
	}
	return edges
}

// classifyCallee maps a call operator to (edge, constrains) under the given
// tier. constrains==false means the call imposes no capture constraint (a
// confirmed capture-safe primitive) and contributes no edge.
func classifyCallee(
	proc ValidatedExpr,
	env *environment.EnvironmentFrame,
	byName map[string]*reclaimNode,
	tier ReclaimTier,
	mutated map[string]bool,
) (reclaimEdge, bool) {
	sym, ok := proc.(*ValidatedSymbol)
	if !ok {
		return reclaimEdge{target: nil}, true // computed operator ⇒ unresolved ⇒ unsafe
	}
	name := sym.Symbol.Sym.Key

	// Same-unit top-level define: resolvable. TierLocal treats it as NOT
	// immutable (a global that could be rebound) ⇒ mutable edge ⇒ unsafe.
	// TierTopLevel treats it as immutable UNLESS it is set! in-unit, modelling
	// the optimistic defined-once proof the harness prices.
	target, ok := byName[name]
	if ok {
		immutable := tier == TierTopLevel && !mutated[name]
		return reclaimEdge{target: target, immutable: immutable}, true
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

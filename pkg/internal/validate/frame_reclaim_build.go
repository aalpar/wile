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
	"maps"
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// captureOperatorNames are the core primitives that capture or abort a
// continuation. Detection is gated on resolved binding identity (see
// makeIsCaptureOp), not the name alone, so a locally-shadowed or user-redefined
// symbol of the same name is not mistaken for the primitive.
var captureOperatorNames = values.StringSet{
	"call-with-current-continuation":    {},
	"call/cc":                           {},
	"call-with-composable-continuation": {},
	"abort-current-continuation":        {},
}

// The capture-safe primitive set is no longer a hand-maintained name whitelist.
// A primitive's capture-safety is now a registration-time capability —
// !PrimitiveSpec.InvokesProcedure — stamped onto its binding as
// environment.BindingMeta.CaptureSafe (registry/apply.go) and read by classifyCallee
// via Binding.IsCaptureSafe(). This package cannot import pkg/registry, so the flag
// flows binding-side exactly as Stable does. The flipped default (most primitives
// are capture-safe; the procedure-invoking minority is annotated InvokesProcedure:
// true — deliberately not counted here, the count has drifted four times) is
// guarded by three tests in pkg/wile: TestInvokesProcedureStaticGuard finds the
// requirement in the Impl's call graph, TestProcedureInvokersMatchesInvokesProcedure
// derives the name list from the annotations, and TestInvokesProcedureCompleteness
// pins that list to IsCaptureSafe()==false on the live binding. A capture-safe
// Scheme procedure (stdlib zero?/not, or a user helper) is additionally trusted by
// compile-time proof — see ProcedureBodyIsCaptureSafe (self_tail.go).

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
		b := env.GetBinding(sym.Sym, syntax.ScopesOf(sym.Scopes()))
		return b != nil && b.IsImported()
	}
}

// ClassifyFrameReclaim returns, for every top-level function define in unit,
// whether its frame is reclaimable at its tail calls. It is the exported entry
// point over the Layer-B/C internals: build the call graph, run the
// greatest-fixpoint mayCapture, and project to a per-identity verdict map. The
// verdict is conservative (any uncertainty ⇒ not reclaimable).
//
// The map is keyed by ScopedBindingKey — the define name's (Sym.Key, scope
// fingerprint) — NOT by Sym.Key alone, and its value is the reclaimable bool. Two
// hygiene-distinct top-level binders of one name (a macro-introduced define and a
// user define) carry different scope sets, so each gets its own verdict instead of
// sharing one. The read side (compilation.frameReuseForDefine) looks up the identity
// of the very define it is compiling, an exact match against the key built from that
// same define here, so reference→verdict resolution cannot diverge. The identity is
// value-stable — it survives env.Copy and cross-library sharing — unlike the *Binding
// pointer a scope-keyed global slot happens to mint. A name-oriented consumer reads
// the define name back off the key (id.Key).
//
// Same-unit-define immutability is the callee node's rebindStable
// (d.StableInUnit ∧ the namespace's ImmutableTopLevel), computed thread-locally
// from the ValidatedDefine and an engine-construction-time flag — never read from
// the callee's *Binding (the T1.5 decoupling). The tier-(a) vs tier-(b) distinction
// is therefore a property of WHAT is passed: defines whose StableInUnit is unset, or
// a namespace without immutable top-level, yield mutable same-unit edges (tier-(a));
// StableInUnit defines under immutable top-level yield immutable edges (tier-(b)).
//
// env is still consulted, but only for binding-side FACTS about non-same-unit
// callees: capture-operator identity (makeIsCaptureOp) and the capture-safe-primitive
// gate (Binding.IsCaptureSafe ∧ IsStable). Those reads are fail-safe on a
// nil/unstamped binding (⇒ unsafe), so an un-stamped env costs only primitive-callee
// precision, never soundness — though a measurement still wants a positive control.
func ClassifyFrameReclaim(
	unit []ValidatedExpr,
	env *environment.EnvironmentFrame,
) map[ScopedBindingKey]bool {
	nodes, byIdent := buildReclaimGraph(unit, env)
	verdict := mayCapture(nodes)
	out := make(map[ScopedBindingKey]bool, len(byIdent))
	for id, n := range byIdent {
		out[id] = frameReclaimable(n, verdict)
	}
	return out
}

// buildReclaimGraph constructs the reclaim call graph for a validated unit. Each
// top-level function define becomes a reclaimNode; its structural capture facts
// come from the Layer A predicates and each constraining call site is resolved
// to an edge.
//
// A call to another top-level define yields an edge whose immutability is the
// callee node's rebindStable (StableInUnit ∧ immutable-top). Imported capture-safe
// primitives contribute no edge.
func buildReclaimGraph(
	unit []ValidatedExpr,
	env *environment.EnvironmentFrame,
) ([]*reclaimNode, map[ScopedBindingKey]*reclaimNode) {
	isCaptureOp := makeIsCaptureOp(env)

	// A same-unit define is provably non-rebindable only when the namespace
	// enforces immutable top-level; without it a later unit may redefine/set! the
	// name, so StableInUnit (in-unit evidence) is not a sound basis for an immutable
	// edge. Read once from engine-construction-time state (never mutated after
	// construction), the sound conjunct the pre-fix binding Stable bit carried.
	immTop := false
	ns := env.Namespace()
	if ns != nil {
		immTop = ns.ImmutableTopLevel()
	}

	byIdent := make(map[ScopedBindingKey]*reclaimNode)
	var defs []*ValidatedDefine
	var defNodes []*reclaimNode

	// Pass 1: a node per top-level function define, keyed by its ScopedBindingKey,
	// with its structural facts. The key comes from the define name's own
	// (Sym.Key, scopes) — NOT from env.GetBinding — so node creation does not depend
	// on the binding being predeclared or stamped in env (the T1.5 decoupling): a
	// define always gets a node, and the same-unit graph is resolved structurally
	// below (resolveNodeByScopes), never through the binding lifecycle.
	collectTopLevelDefines(unit, func(d *ValidatedDefine) {
		if !d.IsFunction {
			return
		}
		name := d.Name()
		id := ScopedBindingKeyOf(name)
		_, dup := byIdent[id]
		n := &reclaimNode{
			label:             name.Sym.Key,
			scopes:            name.Scopes(),
			referencesCapture: bodyReferencesCaptureOperator(d.Body(), isCaptureOp),
			createsEscaping:   bodyCreatesEscapingClosure(d.Body()),
			rebindStable:      d.StableInUnit && immTop,
			// dup is a same-identity redefinition (same name AND scopes): byIdent is
			// last-wins and the fixpoint's node set comes from its values, so the
			// earlier node's facts never reach the fixpoint. Hygiene-distinct binders
			// carry different scopes, so they get distinct identities and no longer
			// collide — the recovery this keying buys. Forcing the survivor unsafe is
			// conservative — see reclaimNode.collided.
			collided: dup,
		}
		byIdent[id] = n
		defs = append(defs, d)
		defNodes = append(defNodes, n)
	})

	// Pass 2: resolve each define's constraining call sites to edges (needs the
	// full node set so same-unit callees resolve).
	for i, d := range defs {
		defNodes[i].callees = resolveCallEdges(d, env, byIdent)
	}

	nodes := make([]*reclaimNode, 0, len(byIdent))
	for _, n := range byIdent {
		nodes = append(nodes, n)
	}
	return nodes, byIdent
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
	byIdent map[ScopedBindingKey]*reclaimNode,
) []reclaimEdge {
	var edges []reclaimEdge
	collect := func(proc ValidatedExpr, bound nameSet) {
		edge, constrains := classifyCallee(proc, env, byIdent, bound)
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
	byIdent map[ScopedBindingKey]*reclaimNode,
	bound nameSet,
) (reclaimEdge, bool) {
	sym, ok := proc.(*ValidatedSymbol)
	if !ok {
		return reclaimEdge{target: nil}, true // computed operator ⇒ unresolved ⇒ unsafe
	}
	name := sym.Symbol.Sym.Key

	// Lexical shadow guard (OQ-1): an operator bound by an enclosing local scope
	// resolves to that local, NOT the same-name top-level define or imported
	// primitive. The classifier resolves against a flat env with no local frames,
	// so falling through would resolve the callee to the global (Stable) binding
	// and mark the edge immutable — a false positive at the wrong callee (e.g.
	// (define (use h) (let ((sq h)) (sq 3))) with a Stable top-level sq).
	//
	// A VISIBLE, UNMUTATED LOCAL CONSTRAINS NOTHING — the A-local rule, applied at
	// the classifier's own callee site rather than only at the per-body predicate's
	// (localCaptureSafe, self_tail.go). The premise is identical and so is the
	// walk: resolveCallEdges descends into that initializer, so every call the
	// local's body makes has already contributed its own edge to this same
	// accumulator, and the constraint survives transitively. The `sq`-bound-to-a-
	// parameter hazard above is exactly what localCaptureSafe's init != nil clause
	// refuses — a parameter records no initializer.
	//
	// Widening only the escape fact was not enough to move any verdict: a shadowed
	// operator resolved to a nil target here and failed nodeSafe's edge test
	// regardless of createsEscaping, so both refusals had to lift together.
	// The lookup is scope-precise, so a binder this operator does not resolve to —
	// a macro-introduced one of the same spelling, say — does not divert it from the
	// same-unit define below. shadowUnknown (an ambiguous tie) is treated exactly
	// like an opaque local: unsafe.
	local, verdict := bound.shadowLookup(name, sym.Symbol.Scopes())
	if verdict == shadowYes {
		if localCaptureSafe(local) {
			return reclaimEdge{}, false
		}
		return reclaimEdge{target: nil}, true
	}
	if verdict == shadowUnknown {
		return reclaimEdge{target: nil}, true
	}

	// Same-unit top-level define: resolved structurally over the graph's nodes
	// (resolveNodeByScopes), replicating env.GetBinding's maximal-subset resolution
	// without consulting the binding — so the edge does not depend on the callee
	// being predeclared/stamped in env (the T1.5 decoupling). Its edge is immutable
	// iff the producer is provably non-rebindable (rebindStable = StableInUnit ∧
	// immutable-top), read off the callee NODE: the node is thread-local to this
	// compile, so a concurrent compile owning the same name cannot perturb this read.
	// A rebindable producer ⇒ mutable edge ⇒ unsafe, the sound tier-(a) default.
	target := resolveNodeByScopes(byIdent, name, sym.Symbol.Scopes())
	if target != nil {
		return reclaimEdge{target: target, immutable: target.rebindStable}, true
	}

	// Not a same-unit define: consult the binding for the capture-safe-primitive
	// fact. Two binding facts must both hold. CaptureSafe is the static "cannot
	// invoke a Scheme procedure" capability, stamped at registration from
	// !PrimitiveSpec.InvokesProcedure (registry/apply.go) — validate cannot import
	// registry, so it reads the binding flag. IsStable() (Imported ∨ Stable) confirms
	// the binding cannot be rebound to a capturing procedure: an imported primitive
	// is immutable by R7RS, and under WithStableBasePrimitives an ambient capture-safe
	// primitive is stamped Stable. A reference whose hygiene matches no binding
	// resolves to nil, and a user shadow (BindingTypeVariable) or a set!-able
	// primitive carries neither flag — all fall through to the unsafe default below.
	b := env.GetBinding(sym.Symbol.Sym, syntax.ScopesOf(sym.Symbol.Scopes()))
	if b != nil && b.IsCaptureSafe() && b.IsStable() {
		return reclaimEdge{}, false
	}

	return reclaimEdge{target: nil}, true // unknown/unsafe ⇒ unresolved ⇒ unsafe
}

// resolveNodeByScopes finds the same-unit define node a reference resolves to,
// replicating env.GetBinding's maximal-subset resolution (Flatt's argmax) over the
// graph's nodes instead of the environment's frame slots: among nodes whose name
// matches and whose define-site scopes are a subset of the reference's scopes, it
// returns the one with the LARGEST scope set. Subset — not fingerprint equality —
// is required because a reference nested in a let/lambda body carries that form's
// scope, a strict superset of the top-level define name's scopes; e.g. a mutual
// cross-call nested in a let, (define (ff n) (let (...) (gg m))), resolves to gg's
// node only by subset. (A self-call in that position happens to be recoverable via
// the capture-safe fallback too, so mutual recursion is the case that actually needs
// this — see TestFrameReclaimSeam_LetNestedMutualCallResolves.) Returns nil for "no
// same-unit define", the caller's cue to try the capture-safe-primitive path.
//
// A well-formed program yields a unique maximum. An AMBIGUOUS maximum — two
// same-name nodes with equal-cardinality but mutually-incomparable scope sets, both
// subset-matching the reference — is refused (returns nil ⇒ unresolved ⇒ unsafe),
// NOT resolved by map-iteration order. env.GetBinding answers the same tie by
// panicking with a wrapped werr.ErrAmbiguousBinding (global_environment_frame.go
// resolveRankedLocked); this analysis declines instead, because it is an
// optimization gate — raising here would turn a forgone reclamation into a compile
// error, whereas refusing costs only the optimization and keeps the sound direction
// (frame_reclaim.go: a false positive would corrupt). It is the same mapping the
// caller gives shadowUnknown. The refusal costs reclamation only on a genuinely
// ambiguous binder, which a sound analysis would not have reclaimed anyway.
func resolveNodeByScopes(byIdent map[ScopedBindingKey]*reclaimNode, name string, refScopes []*syntax.Scope) *reclaimNode {
	var best *reclaimNode
	bestLen := -1
	tie := false
	for _, n := range byIdent {
		if n.label != name {
			continue
		}
		// ScopesMatch(use, binding) ⟺ binding ⊆ use, so this tests n.scopes ⊆ refScopes.
		if !syntax.ScopesMatch(refScopes, n.scopes) {
			continue
		}
		// Equal-cardinality matches are necessarily distinct scope sets (equal sets
		// share a ScopedBindingKey, hence one node), so incomparable ⇒ ambiguous.
		switch {
		case len(n.scopes) > bestLen:
			bestLen = len(n.scopes)
			best = n
			tie = false
		case len(n.scopes) == bestLen:
			tie = true
		}
	}
	if tie {
		return nil
	}
	return best
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
		// The two scopes carry the same membership and differ only in the evidence
		// recorded — see withLet.
		initScope, bodyScope := bound.withLet(e)
		for _, b := range e.Bindings {
			walkCallSites(b.Init, initScope, fn)
		}
		walkBodySeq(e.Body(), bodyScope, fn)

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
	inner := bound.withInternalDefines(body)
	for _, e := range body {
		walkCallSites(e, inner, fn)
	}
}

// localBinding is what the shadow walk knows about one lexically bound name.
// The zero value — no initializer, not mutated — means "bound, but opaque", and
// is what every name in the set carries as far as the current consumers are
// concerned: they query membership only (has) and every hit refuses.
type localBinding struct {
	// init is the binder's initializer when it is a procedure the walk can see
	// through: a lambda init, or a function-form internal define (whose own
	// params and body ARE the procedure). Everything else records nil — a
	// parameter, a computed or non-procedure init, and any position where the
	// name does not yet denote this initializer (a plain let's own inits).
	//
	// Recording it is a PROMISE that walkCallSites descends into that
	// initializer, which is what lets localCaptureSafe accept the local without
	// re-proving it. Both recording sites below sit next to the descent that
	// keeps the promise; a new binder form must add both or neither.
	init ValidatedBodyAndParams
	// mutated records a set! of this name anywhere within its own binding form.
	// A mutated name's init no longer describes what an operator of that name
	// resolves to at the call, so the initializer is not evidence about it.
	mutated bool
	// scopes is the BINDER identifier's own scope set, which is what makes this
	// entry answerable to a reference rather than to a spelling. These predicates
	// run POST-expansion, where a macro-introduced binder and a user binder of the
	// same name differ in nothing else.
	scopes []*syntax.Scope
}

// nameSet maps an identifier Key to EVERY enclosing local binder of that name,
// outermost first. A reference resolves against them by scope set, not by
// membership.
//
// WHY A SLICE. The obvious map[string]localBinding overwrote an outer binder with
// an inner one, which is correct only when the inner one is the answer. It is not:
// a macro-introduced binder carries the intro scope, so a use-site reference does
// NOT resolve to it, and the answer is an OUTER same-named binder (or none). That
// is repro (b) — a template's own (let ((loop 0)) …) around a set! of a
// use-site-supplied name made the real loop look shadowed, hiding the set! and
// arming self-tail on a mutable self.
//
// NOT keyed by a composite (name, scopes) key: a fingerprint answers equality, and
// resolution needs a SUBSET query, so a composite key would force a whole-map scan
// per call operator. Name-keyed with a per-entry scope set keeps the common case
// one map probe over a one- or two-element slice.
//
// Entries are appended outermost-first, so shadowLookup scans in reverse to give
// the innermost binder precedence among equally-scoped matches.
type nameSet map[string][]localBinding

// shadowVerdict is shadowLookup's tri-state answer. shadowUnknown exists because
// the two-valued form cannot be conservative for every consumer at once: the
// callee/tail walks read "shadowed" as a REFUSAL and exprMutatesName reads it as
// a SUPPRESSION of the set! report. Collapsing an ambiguous lookup to either
// boolean is unsound for one of them, so each consumer maps shadowUnknown to its
// OWN refusing answer:
//
//   - calleeCaptureSafe: false (do not clear the callee).
//   - classifyCallee: reclaimEdge{target: nil}, unsafe (same as an opaque local).
//   - tailExprHasSelfCall: false (not a rewritable self call).
//   - exprMutatesName: treat as NOT shadowed, i.e. REPORT the mutation. Opposite
//     polarity, and the reason the tri-state is not optional.
type shadowVerdict int

const (
	// shadowNo — no enclosing binder of that name resolves for this reference.
	shadowNo shadowVerdict = iota
	// shadowYes — exactly one maximal binder resolves; the localBinding is it.
	shadowYes
	// shadowUnknown — two incomparable binders tie for maximal, so no one binder
	// is THE resolution (the Flatt ambiguity, per environment.scopedBestOf).
	shadowUnknown
)

// shadowLookup answers "which enclosing binder does a reference carrying refScopes
// resolve to?" by the rule environment.scopedBestOf implements over frame slots: a
// binder matches when binderScopes ⊆ refScopes, and among matches the largest
// scope set wins. Equal-cardinality matches with different sets are mutually
// incomparable, so neither is the maximum — shadowUnknown.
//
// Reverse iteration is load-bearing: entries are appended outermost-first, and for
// the ordinary nested-let case every binder is ∅-scoped, so all of them match with
// equal cardinality. Scanning innermost-first with a strict improvement test makes
// the innermost one win, which is what lexical scoping means.
func (s nameSet) shadowLookup(name string, refScopes []*syntax.Scope) (localBinding, shadowVerdict) {
	cands := s[name]
	best := -1
	ambiguous := false
	for i := range slices.Backward(cands) {
		if !syntax.ScopesCompatible(cands[i].scopes, refScopes) {
			continue
		}
		if best < 0 || len(cands[i].scopes) > len(cands[best].scopes) {
			best = i
			ambiguous = false
			continue
		}
		if len(cands[i].scopes) == len(cands[best].scopes) &&
			!syntax.ScopesMatch(cands[i].scopes, cands[best].scopes) {
			// Equal cardinality and not the same set ⇒ incomparable.
			ambiguous = true
		}
	}
	if best < 0 {
		return localBinding{}, shadowNo
	}
	if ambiguous {
		return localBinding{}, shadowUnknown
	}
	return cands[best], shadowYes
}

// with returns a copy of s extended with binders recorded opaquely, carrying each
// binder symbol's own scope set.
func (s nameSet) with(syms ...*syntax.SyntaxSymbol) nameSet {
	out := s.clone(len(syms))
	for _, sym := range syms {
		out.add(sym, localBinding{})
	}
	return out
}

// add appends one binder for sym's name, retaining any outer binders already
// recorded, and stamps the entry with sym's own scope set — the single place scopes
// are populated, so a new binder form cannot record an unscoped entry.
//
// slices.Clip forces the next append to copy rather than extend a shared backing
// array, which is the same independence clone gives the map: two sibling scopes
// must not see each other's binders.
func (s nameSet) add(sym *syntax.SyntaxSymbol, bnd localBinding) {
	bnd.scopes = sym.Scopes()
	key := sym.Sym.Key
	s[key] = append(slices.Clip(s[key]), bnd)
}

// clone copies s with room for extra further entries. Copy-on-extend keeps
// sibling scopes independent — a name bound in one let does not leak to its
// siblings.
func (s nameSet) clone(extra int) nameSet {
	out := make(nameSet, len(s)+extra)
	maps.Copy(out, s)
	return out
}

// withParams returns a copy of s extended with a parameter list's required and
// rest names. A nil params list returns s unchanged.
func (s nameSet) withParams(p *ValidatedParams) nameSet {
	if p == nil {
		return s
	}
	syms := make([]*syntax.SyntaxSymbol, 0, len(p.Required)+1)
	syms = append(syms, p.Required...)
	if p.Rest != nil {
		syms = append(syms, p.Rest)
	}
	return s.with(syms...)
}

// withLetBindings returns a copy of s extended with a let form's bound names,
// all opaque. This is the membership-only extender, for callers that ask nothing
// but "is this name shadowed" — notably exprMutatesName, which cannot use withLet
// because recording evidence there runs letMutatesName, whose own scan re-enters
// exprMutatesName once per enclosing let.
func (s nameSet) withLetBindings(bindings []ValidatedLetBinding) nameSet {
	if len(bindings) == 0 {
		return s
	}
	syms := make([]*syntax.SyntaxSymbol, 0, len(bindings))
	for _, b := range bindings {
		syms = append(syms, b.Name)
	}
	return s.with(syms...)
}

// withLet returns the two shadow sets a let form's sub-scopes run under: inits
// and body. They differ because a name denotes its own initializer only in the
// letrec family (LetKind.InitsInScope) — in a plain let or let*, a reference to
// the name inside its own init is the OUTER binding, so recording the init there
// would be evidence about the wrong procedure. The init scope binds every name
// opaquely in that case, preserving the walk's existing over-approximation
// (shadowed ⇒ refuse) rather than narrowing it.
func (s nameSet) withLet(v *ValidatedLet) (nameSet, nameSet) {
	if len(v.Bindings) == 0 {
		return s, s
	}
	body := s.clone(len(v.Bindings))
	for _, b := range v.Bindings {
		body.add(b.Name, letBindingLocal(v, b))
	}
	if v.Kind.InitsInScope() {
		return body, body
	}
	inits := s.clone(len(v.Bindings))
	for _, b := range v.Bindings {
		inits.add(b.Name, localBinding{})
	}
	return inits, body
}

// letBindingLocal records what one let binding is known to bind. Only a lambda
// init is evidence. letMutatesName covers the two set! paths a lexical binding
// has — a sibling init and the let body — and nothing outside the let can reach
// the name, which is the same argument LetBindingSelfTailReusable relies on.
func letBindingLocal(v *ValidatedLet, b ValidatedLetBinding) localBinding {
	lam, ok := b.Init.(*ValidatedLambda)
	if !ok {
		return localBinding{}
	}
	return localBinding{init: lam, mutated: letMutatesName(v, b.Name.Sym.Key)}
}

// withInternalDefines returns a copy of s extended with a body sequence's
// internal define names (letrec* — mutually visible across the whole body,
// including their own bodies and earlier siblings). A function-form define
// records itself as its own init: its params and body ARE the procedure.
func (s nameSet) withInternalDefines(body []ValidatedExpr) nameSet {
	var defines []*ValidatedDefine
	for _, e := range body {
		d, ok := e.(*ValidatedDefine)
		if ok {
			defines = append(defines, d)
		}
	}
	if len(defines) == 0 {
		return s
	}
	out := s.clone(len(defines))
	for _, d := range defines {
		out.add(d.Name(), internalDefineLocal(d, body))
	}
	return out
}

// internalDefineLocal records what one internal define is known to bind.
func internalDefineLocal(d *ValidatedDefine, body []ValidatedExpr) localBinding {
	if !d.IsFunction {
		return localBinding{}
	}
	return localBinding{init: d, mutated: bodyMutatesOwnDefine(body, d.Name().Sym.Key)}
}

// bodyMutatesOwnDefine reports whether a body sequence contains a set! of name,
// where name is bound BY this sequence's own internal defines. It cannot be
// seqMutatesName: that one hoists every internal define name into the shadow
// set, which would mask exactly the set! this asks about.
func bodyMutatesOwnDefine(body []ValidatedExpr, name string) bool {
	for _, e := range body {
		if exprMutatesName(e, name, nameSet(nil)) {
			return true
		}
	}
	return false
}

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

// The capture-safe primitive set is no longer a hand-maintained name whitelist.
// A primitive's capture-safety is now a registration-time capability —
// !PrimitiveSpec.InvokesProcedure — stamped onto its binding as
// environment.BindingMeta.CaptureSafe (registry/apply.go) and read by classifyCallee
// via Binding.IsCaptureSafe(). This package cannot import pkg/registry, so the flag
// flows binding-side exactly as Stable does. The flipped default (most primitives
// are capture-safe; the ~24 procedure-invoking ones are annotated InvokesProcedure:
// true) is guarded by TestInvokesProcedureCompleteness (pkg/wile). A capture-safe
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
	// so without this guard the callee would resolve to the global (Stable) binding
	// and mark the edge immutable — a false positive at the wrong callee (e.g.
	// (define (use h) (let ((sq h)) (sq 3))) with a Stable top-level sq). A shadowed
	// operator is therefore unresolved ⇒ unsafe.
	if bound.has(name) {
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
// NOT resolved by map-iteration order. GetBinding breaks that tie deterministically
// by binding creation order (global_environment_frame.go bestSlotLocked over a
// creation-ordered []int); this map cannot cheaply replicate that order, so it
// declines to guess rather than grant a reclaim verdict on a coin-flip — the sound
// direction (frame_reclaim.go: a false positive would corrupt). The refusal costs
// reclamation only on a genuinely ambiguous binder, which a sound analysis would not
// have reclaimed anyway.
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
}

// nameSet maps an identifier Key lexically bound in an enclosing local scope to
// what the walk knows about that binding. A call operator whose name is in the
// set is shadowed by a local binding and therefore is NOT the same-Key top-level
// define or imported primitive.
//
// The recorded localBinding is the evidence a proof of a locally-bound operator
// would need (the A-local lever): it is populated here and deliberately not yet
// read, so the type change and the semantic change stay separable.
type nameSet map[string]localBinding

// with returns a copy of s extended with names bound opaquely.
func (s nameSet) with(names ...string) nameSet {
	out := s.clone(len(names))
	for _, n := range names {
		out[n] = localBinding{}
	}
	return out
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
	names := make([]string, 0, len(p.Required)+1)
	for _, req := range p.Required {
		names = append(names, req.Sym.Key)
	}
	if p.Rest != nil {
		names = append(names, p.Rest.Sym.Key)
	}
	return s.with(names...)
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
	names := make([]string, 0, len(bindings))
	for _, b := range bindings {
		names = append(names, b.Name.Sym.Key)
	}
	return s.with(names...)
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
		body[b.Name.Sym.Key] = letBindingLocal(v, b)
	}
	if v.Kind.InitsInScope() {
		return body, body
	}
	inits := s.clone(len(v.Bindings))
	for _, b := range v.Bindings {
		inits[b.Name.Sym.Key] = localBinding{}
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
		out[d.Name().Sym.Key] = internalDefineLocal(d, body)
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

// has reports whether name is lexically shadowed in this scope.
func (s nameSet) has(name string) bool {
	_, ok := s[name]
	return ok
}

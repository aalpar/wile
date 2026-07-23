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
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// envWithImported builds a runtime env whose global frame holds an imported
// binding for each given name. internal/validate sits below the registry, so
// the real continuation/arithmetic primitives are not available here — these
// stand-in imported globals exercise the builder's resolution + IsImported
// gate exactly as the real ones would.
func envWithImported(t *testing.T, names ...string) *environment.EnvironmentFrame {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	// The classifier's same-unit edge is immutable only under immutable top-level
	// (rebindStable = StableInUnit ∧ this flag). These tests model that regime; the
	// discriminator between reclaimable and not is markStableInUnit, not the flag.
	env.Namespace().SetImmutableTopLevel(true)
	for _, name := range names {
		sym := syntax.NewSyntaxSymbol(name, nil).Sym
		env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)
		b := env.GetBinding(sym, values.AllScopes())
		if b == nil {
			t.Fatalf("failed to create global binding %q", name)
		}
		b.UpdateMeta(func(m *environment.BindingMeta) bool {
			m.Imported = true
			return true
		})
		// Model the registry's CaptureSafe stamp (Lever E): the post-whitelist
		// classifier trusts a primitive callee on IsCaptureSafe()&&IsStable(). These
		// unit tests lack the real registry, so a capture-safe primitive stand-in
		// must carry the flag by hand, mirroring how markBindingImported propagates
		// it on a real import. Procedure-invokers (map), capture operators (call/cc),
		// and the non-primitive callee stand-ins (proc, fns, pong, k) used in the
		// negative tests are deliberately absent — they must NOT be trusted.
		if captureSafeTestPrims[name] {
			b.UpdateMeta(func(m *environment.BindingMeta) bool {
				m.CaptureSafe = true
				return true
			})
		}
	}
	return env
}

// captureSafeTestPrims is the set of primitive stand-in names these tests import
// that invoke no Scheme procedure (arithmetic, comparison, pair accessors), i.e.
// the names the real registry would stamp !InvokesProcedure ⇒ CaptureSafe. It is
// test fixture data modelling that capability, not a production whitelist (Lever E
// retired the production captureSafePrimitiveNames map).
var captureSafeTestPrims = map[string]bool{
	"+": true, "-": true, "*": true, "/": true,
	"=": true, "<": true, ">": true, "<=": true, ">=": true,
	"car": true, "cdr": true, "cons": true,
}

// markStableInUnit sets StableInUnit on the named top-level defines in unit,
// modelling the validator's finalizeStability verdict (defined-once ∧ never-set!)
// that a same-unit define is rebind-stable. The frame-reclaim classifier reads
// this thread-local bit off the ValidatedDefine — NOT the shared *Binding — so a
// same-unit callee edge is immutable iff its producer carries it. Replaces the old
// stampStable, which wrote the callee binding's Stable field; the T1.5 follow-on
// removed the pre-stamp that surfaced StableInUnit through the binding, so the
// classifier no longer reads the binding for same-unit edges. A define left
// unmarked models the tier-(a) case (defined-twice, set!, or non-compiled): its
// edge stays mutable. Flattens one level of begin, matching collectTopLevelDefines.
func markStableInUnit(unit []ValidatedExpr, names ...string) {
	want := make(map[string]bool, len(names))
	for _, name := range names {
		want[name] = true
	}
	collectTopLevelDefines(unit, func(d *ValidatedDefine) {
		if want[d.name.Key()] {
			d.StableInUnit = true
		}
	})
}

// nodeByLabel returns the graph node whose define name is label. The unit tests
// build synthetic symbols with nil scopes and unique names, so a label lookup is
// unambiguous here; production keys the graph on the full ScopedBindingKey.
func nodeByLabel(m map[ScopedBindingKey]*reclaimNode, label string) *reclaimNode {
	for _, n := range m {
		if n.label == label {
			return n
		}
	}
	return nil
}

// reclaimNames classifies unit and projects the verdict to name -> reclaimable.
// Each test define has a unique name, so the name projection is lossless here
// (production keys on the full ScopedBindingKey).
func reclaimNames(unit []ValidatedExpr, env *environment.EnvironmentFrame) map[string]bool {
	out := make(map[string]bool)
	for id, reclaimable := range ClassifyFrameReclaim(unit, env) {
		out[id.Key] = reclaimable
	}
	return out
}

// TestResolveNodeByScopes_AmbiguousMaxRefusesToGuess pins the soundness fix for the
// argmax tie. Two same-name nodes with equal-cardinality, mutually-incomparable scope
// sets, both subset-matching the reference, form an ambiguous maximum. GetBinding
// breaks such a tie deterministically by binding creation order; this map cannot, so
// resolveNodeByScopes refuses (returns nil ⇒ unresolved ⇒ unsafe) rather than pick by
// map-iteration order — the sound direction (a false positive would corrupt). The
// control (a strictly larger, unique maximum) still resolves.
func TestResolveNodeByScopes_AmbiguousMaxRefusesToGuess(t *testing.T) {
	sa := syntax.NewScope()
	sb := syntax.NewScope()

	// {sa} and {sb} are both cardinality 1 and incomparable; refScopes ⊇ both.
	nodeA := &reclaimNode{label: "f", scopes: []*syntax.Scope{sa}}
	nodeB := &reclaimNode{label: "f", scopes: []*syntax.Scope{sb}}
	byIdent := map[ScopedBindingKey]*reclaimNode{
		{Key: "f", ScopeKey: syntax.ScopeFingerprint(nodeA.scopes)}: nodeA,
		{Key: "f", ScopeKey: syntax.ScopeFingerprint(nodeB.scopes)}: nodeB,
	}
	refScopes := []*syntax.Scope{sa, sb}
	got := resolveNodeByScopes(byIdent, "f", refScopes)
	if got != nil {
		t.Fatalf("ambiguous maximum must resolve to nil (unsafe), got node with scopes %v", got.scopes)
	}

	// Control: nodeB now carries {sa,sb}, a strict superset of nodeA's {sa} ⇒ the
	// maximum is unique and resolution succeeds.
	nodeB.scopes = []*syntax.Scope{sa, sb}
	byIdent2 := map[ScopedBindingKey]*reclaimNode{
		{Key: "f", ScopeKey: syntax.ScopeFingerprint(nodeA.scopes)}: nodeA,
		{Key: "f", ScopeKey: syntax.ScopeFingerprint(nodeB.scopes)}: nodeB,
	}
	got = resolveNodeByScopes(byIdent2, "f", refScopes)
	if got != nodeB {
		t.Fatalf("unique maximum {sa,sb} must resolve to nodeB, got %v", got)
	}
}

func TestBuildReclaimGraph_DetectsCallCC(t *testing.T) {
	env := envWithImported(t, "call/cc")
	// (define (bad k) (call/cc k)) — references the capture primitive, no
	// escaping lambda, so this isolates capture-operator detection.
	bad := defineFn("bad", call(symRef("call/cc"), symRef("k")))
	nodes, byIdent := buildReclaimGraph([]ValidatedExpr{bad}, env)
	v := mayCapture(nodes)
	if frameReclaimable(nodeByLabel(byIdent, "bad"), v) {
		t.Fatalf("a define invoking call/cc must not be reclaimable")
	}
}

func TestBuildReclaimGraph_LeafFnReclaimable(t *testing.T) {
	env := envWithImported(t, "*")
	// (define (sq x) (* x x))   — only the capture-safe primitive * ⇒ reclaimable.
	// (define (use) (sq 3))     — calls a top-level define; sq is NOT Stable in
	//                             this env ⇒ mutable edge ⇒ NOT reclaimable.
	sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	use := defineFn("use", call(symRef("sq"), lit()))
	nodes, byIdent := buildReclaimGraph([]ValidatedExpr{sq, use}, env)
	v := mayCapture(nodes)
	if !frameReclaimable(nodeByLabel(byIdent, "sq"), v) {
		t.Fatalf("sq over only the capture-safe primitive * must be reclaimable")
	}
	if frameReclaimable(nodeByLabel(byIdent, "use"), v) {
		t.Fatalf("use calls a non-Stable top-level define ⇒ NOT reclaimable")
	}
}

func TestBuildReclaimGraph_EscapingClosureNotReclaimable(t *testing.T) {
	env := envWithImported(t)
	// (define (esc) (lambda () 1)) — returns a closure that escapes.
	esc := defineFn("esc", lam(lit()))
	nodes, byIdent := buildReclaimGraph([]ValidatedExpr{esc}, env)
	v := mayCapture(nodes)
	if frameReclaimable(nodeByLabel(byIdent, "esc"), v) {
		t.Fatalf("a define returning an escaping closure must not be reclaimable")
	}
}

func TestBuildReclaimGraph_UnlistedPrimitiveNotReclaimable(t *testing.T) {
	env := envWithImported(t, "map")
	// (define (cm) (map f xs)) — map is imported but NOT on the capture-safe
	// whitelist (it invokes a procedure that could capture) ⇒ unresolved edge
	// ⇒ NOT reclaimable. This is the sound-by-default guarantee.
	cm := defineFn("cm", call(symRef("map"), symRef("f"), symRef("xs")))
	nodes, byIdent := buildReclaimGraph([]ValidatedExpr{cm}, env)
	v := mayCapture(nodes)
	if frameReclaimable(nodeByLabel(byIdent, "cm"), v) {
		t.Fatalf("a define calling an unlisted (non-whitelisted) primitive must not be reclaimable")
	}
}

// TestClassifyFrameReclaim_StableDefineUnlocksCaller is the Phase-2 measurement
// core, re-expressed over the Stable bit: stamping a same-unit top-level define
// Stable flips its in-edges from mutable to immutable, so a function whose only
// constraint is a call to another (safe) top-level define becomes reclaimable.
// The flag-off vs flag-on gap the harness measures is now a property of WHICH
// env is passed — Finding 3.
func TestClassifyFrameReclaim_StableDefineUnlocksCaller(t *testing.T) {
	// (define (sq x) (* x x)) (define (use) (sq 3))
	mkUnit := func() []ValidatedExpr {
		sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
		use := defineFn("use", call(symRef("sq"), lit()))
		return []ValidatedExpr{sq, use}
	}

	// Env without a Stable sq (tier-(a) equivalent): use's edge to sq is mutable.
	envA := envWithImported(t, "*")
	a := reclaimNames(mkUnit(), envA)
	if !a["sq"] || a["use"] {
		t.Fatalf("no-Stable env: want sq reclaimable, use not — got sq=%v use=%v", a["sq"], a["use"])
	}

	// Env with sq stable-in-unit (tier-(b) equivalent): use→sq immutable, sq safe.
	envB := envWithImported(t, "*")
	unitB := mkUnit()
	markStableInUnit(unitB, "sq")
	b := reclaimNames(unitB, envB)
	if !b["sq"] || !b["use"] {
		t.Fatalf("Stable-sq env: want both reclaimable (use→sq immutable, sq safe) — got sq=%v use=%v", b["sq"], b["use"])
	}
}

// TestClassifyFrameReclaim_SelfRecursiveTopLevel covers the Gabriel-shaped case:
// a self-recursive top-level define (fib/tak) is non-reclaimable when its
// binding is not Stable (mutable self-edge), but reclaimable once the producer
// stamps it Stable.
func TestClassifyFrameReclaim_SelfRecursiveTopLevel(t *testing.T) {
	// (define (fib n) (+ (fib (- n 1)) (fib (- n 2))))
	mkUnit := func() []ValidatedExpr {
		fib := defineFn("fib",
			call(symRef("+"),
				call(symRef("fib"), call(symRef("-"), symRef("n"), lit())),
				call(symRef("fib"), call(symRef("-"), symRef("n"), lit()))))
		return []ValidatedExpr{fib}
	}

	envA := envWithImported(t, "+", "-")
	if reclaimNames(mkUnit(), envA)["fib"] {
		t.Fatalf("no-Stable env: a self-recursive top-level define must NOT be reclaimable (mutable self-edge)")
	}

	envB := envWithImported(t, "+", "-")
	unitB := mkUnit()
	markStableInUnit(unitB, "fib")
	if !reclaimNames(unitB, envB)["fib"] {
		t.Fatalf("Stable env: a self-recursive top-level define over safe primitives MUST be reclaimable")
	}
}

// TestClassifyFrameReclaim_TwiceDefinedNotReclaimable pins the defined-once
// soundness gap (plan Finding 2 / Task 1). A name defined twice in a unit is
// genuinely rebindable: the producer's StableInUnit = (definedKeyCount==1 ∧
// ¬mutated) drops it ⇒ no Stable binding ⇒ a later set!/redefine is permitted.
// The OLD tier-(b) predicate (`!mutated[name]`) omitted the defined-once
// conjunct and wrongly treated such a name immutable — a false positive that, in
// codegen (Phase 4), reclaims a frame for a binding that can still be rebound:
// exactly the continuation-corruption class the design exists to avoid.
//
// Modelled discriminatingly: the same self-recursive f, classified against a
// Stable-stamped control env (reclaimable) and a non-stamped twice-defined env
// (NOT reclaimable). The discriminating power requires one binding stamped and
// the other not — a non-compiled env alone would make everything non-reclaimable
// and pass vacuously (Task 1 CROSSCHECK).
func TestClassifyFrameReclaim_TwiceDefinedNotReclaimable(t *testing.T) {
	mkUnit := func() []ValidatedExpr {
		f := defineFn("f", call(symRef("f"), call(symRef("-"), symRef("n"), lit())))
		return []ValidatedExpr{f}
	}

	// Control: defined-once, never-set! ⇒ producer is StableInUnit ⇒ reclaimable.
	control := envWithImported(t, "-")
	controlUnit := mkUnit()
	markStableInUnit(controlUnit, "f")
	if !reclaimNames(controlUnit, control)["f"] {
		t.Fatalf("control (Stable f): self-recursive define over safe primitives MUST be reclaimable")
	}

	// Bug case: twice-defined ⇒ producer leaves f non-Stable ⇒ NOT reclaimable.
	// The old tier-(b) `!mutated` code returned true here — the soundness gap.
	twiceDefined := envWithImported(t, "-")
	if reclaimNames(mkUnit(), twiceDefined)["f"] {
		t.Fatalf("twice-defined (non-Stable f): a rebindable name must NOT be reclaimable")
	}
}

// TestClassifyFrameReclaim_MutualRecursionPair hardens the which-env claim
// (Finding 3) on a mutually-recursive pair f→g, g→f: non-reclaimable when
// neither is Stable, reclaimable when both are. The greatest-fixpoint converges
// because each node's cross-edge points at a still-Safe node.
func TestClassifyFrameReclaim_MutualRecursionPair(t *testing.T) {
	// (define (f n) (g (- n 1))) (define (g n) (f (- n 1)))
	mkUnit := func() []ValidatedExpr {
		f := defineFn("f", call(symRef("g"), call(symRef("-"), symRef("n"), lit())))
		g := defineFn("g", call(symRef("f"), call(symRef("-"), symRef("n"), lit())))
		return []ValidatedExpr{f, g}
	}

	envA := envWithImported(t, "-")
	a := reclaimNames(mkUnit(), envA)
	if a["f"] || a["g"] {
		t.Fatalf("no-Stable env: mutual recursion over mutable edges must NOT be reclaimable — got f=%v g=%v", a["f"], a["g"])
	}

	envB := envWithImported(t, "-")
	unitB := mkUnit()
	markStableInUnit(unitB, "f", "g")
	b := reclaimNames(unitB, envB)
	if !b["f"] || !b["g"] {
		t.Fatalf("both-Stable env: mutual recursion over immutable edges MUST be reclaimable — got f=%v g=%v", b["f"], b["g"])
	}
}

// TestClassifyFrameReclaim_SetBangCalleeNotStable is the soundness anchor for a
// callee mutated by set!: the producer's StableInUnit drops a set!-target, so
// its binding is not Stable and its callers stay non-reclaimable. set! on a
// callee's binding does not, however, make the callee's OWN frame escapable —
// the verdict is over the body, not the binding.
func TestClassifyFrameReclaim_SetBangCalleeNotStable(t *testing.T) {
	// (define (sq x) (* x x)) (define (use) (sq 3)) — sq set! in-unit, so the
	// producer leaves sq non-Stable (modelled by not stamping it).
	sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	use := defineFn("use", call(symRef("sq"), lit()))
	unit := []ValidatedExpr{sq, use}

	env := envWithImported(t, "*") // sq deliberately NOT stamped Stable
	b := reclaimNames(unit, env)
	if b["use"] {
		t.Fatalf("use must NOT be reclaimable when its callee sq is not Stable (set! in-unit)")
	}
	if !b["sq"] {
		t.Fatalf("sq's own frame stays reclaimable — set! mutates the binding, not the frame")
	}
}

// TestClassifyFrameReclaim_BeginWrappedUnit pins the production input shape: the
// measurement harness wraps each program as a single top-level (begin define...)
// before classifying, relying on collectTopLevelDefines to flatten one level. A
// flat slice (every other test) does not exercise that branch.
func TestClassifyFrameReclaim_BeginWrappedUnit(t *testing.T) {
	mkUnit := func() []ValidatedExpr {
		sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
		use := defineFn("use", call(symRef("sq"), lit()))
		begin := &ValidatedBegin{
			validatedBase: validatedBase{formName: "begin"},
			body:          []ValidatedExpr{sq, use},
		}
		return []ValidatedExpr{begin}
	}

	envA := envWithImported(t, "*")
	a := reclaimNames(mkUnit(), envA)
	if !a["sq"] || a["use"] {
		t.Fatalf("begin-wrapped no-Stable: want sq reclaimable, use not — got sq=%v use=%v", a["sq"], a["use"])
	}

	envB := envWithImported(t, "*")
	unitB := mkUnit()
	markStableInUnit(unitB, "sq")
	b := reclaimNames(unitB, envB)
	if !b["sq"] || !b["use"] {
		t.Fatalf("begin-wrapped Stable-sq: want both reclaimable — got sq=%v use=%v", b["sq"], b["use"])
	}
}

// TestBuildReclaimGraph_QuasiquoteNotReclaimable is the soundness guard for the
// quasiquote blind spot: a quasiquote template is raw unvalidated syntax that the
// sub-expression walk does not descend into, so an unquoted (call/cc …) inside it
// is invisible. The classifier conservatively treats any quasiquote in a body as
// a capture risk, so a define containing one must NOT be reclaimable — regardless
// of whether its own binding is Stable.
func TestBuildReclaimGraph_QuasiquoteNotReclaimable(t *testing.T) {
	// (define (g) `(,(...)))  — modelled as a body containing a quasiquote whose
	// template may capture/escape/set! at runtime, unseen by the walk.
	mkUnit := func() []ValidatedExpr {
		g := defineFn("g", &ValidatedQuasiquote{validatedBase: validatedBase{formName: "quasiquote"}})
		return []ValidatedExpr{g}
	}

	env := envWithImported(t)
	nodes, byIdent := buildReclaimGraph(mkUnit(), env)
	vA := mayCapture(nodes)
	if frameReclaimable(nodeByLabel(byIdent, "g"), vA) {
		t.Fatalf("a define whose body contains a quasiquote must not be reclaimable (no-Stable env)")
	}

	stableEnv := envWithImported(t)
	stableUnit := mkUnit()
	markStableInUnit(stableUnit, "g")
	if reclaimNames(stableUnit, stableEnv)["g"] {
		t.Fatalf("a define whose body contains a quasiquote must not be reclaimable (Stable env)")
	}
}

// TestBuildReclaimGraph_ImmutableEdgeImpliesStable is the kill-criterion guard
// (plan Risks): the soundness invariant lives on the edge, not the node verdict.
// Every reclaimEdge marked immutable MUST point at a callee node that is
// StableInUnit — because `immutable` is assigned from exactly that field (the
// T1.5 follow-on moved the read from the shared *Binding to the thread-local
// node). This introspects the built graph and fails if any immutable edge resolves
// to a non-stable (rebindable) callee, catching a future regression that decouples
// the flag from the read.
func TestBuildReclaimGraph_ImmutableEdgeImpliesStable(t *testing.T) {
	// (define (sq x) (* x x)) (define (use) (sq 3)) with sq StableInUnit: use→sq is
	// the one immutable same-unit edge; sq→* contributes no edge (capture-safe).
	sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	use := defineFn("use", call(symRef("sq"), lit()))
	unit := []ValidatedExpr{sq, use}
	markStableInUnit(unit, "sq")
	env := envWithImported(t, "*")

	nodes, _ := buildReclaimGraph(unit, env)
	immutableEdges := 0
	for _, n := range nodes {
		for _, e := range n.callees {
			if !e.immutable {
				continue
			}
			immutableEdges++
			if e.target == nil || !e.target.rebindStable {
				t.Fatalf("immutable edge %s→%s resolves to a rebindable callee node — soundness invariant violated",
					n.label, e.target.label)
			}
		}
	}
	if immutableEdges == 0 {
		t.Fatalf("expected at least one immutable edge (use→sq) to exercise the invariant")
	}
}

// TestClassifyFrameReclaim_LocalShadowNotReclaimable pins the OQ-1 fix: a local
// binding (parameter or let) that shadows a Stable top-level define name in
// operator position must NOT yield a reclaimable verdict — the real callee is
// the local (arbitrary, possibly continuation-capturing), not the Stable
// top-level define. The classifier resolves against a flat env that cannot see
// the local frame, so env.GetBinding would otherwise return the global Stable
// binding and mark the edge immutable (a false positive at the wrong callee).
// The call-site walk tracks shadowing names to reject these.
func TestClassifyFrameReclaim_LocalShadowNotReclaimable(t *testing.T) {
	// Top-level (define (sq x) (* x x)), always stamped Stable below.
	mkSq := func() *ValidatedDefine {
		return defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	}

	// (a) PARAMETER shadow: (define (use sq) (sq 3)) — sq is use's parameter.
	useParam := &ValidatedDefine{
		validatedBase: validatedBase{formName: "define"},
		validatedProcBase: validatedProcBase{
			params: &ValidatedParams{Required: []*syntax.SyntaxSymbol{syntax.NewSyntaxSymbol("sq", nil)}},
			body:   []ValidatedExpr{call(symRef("sq"), lit())},
		},
		name:       syntax.NewSyntaxSymbol("use", nil),
		IsFunction: true,
	}
	envA := envWithImported(t, "*")
	unitA := []ValidatedExpr{mkSq(), useParam}
	markStableInUnit(unitA, "sq")
	if reclaimNames(unitA, envA)["use"] {
		t.Fatalf("parameter shadow: use must NOT be reclaimable — (sq 3) calls the param, not the Stable top-level sq")
	}

	// (b) LET shadow: (define (use) (let ((sq #f)) (sq))) — sq is let-bound.
	useLet := defineFn("use",
		nestedLet([]ValidatedLetBinding{{Name: syntax.NewSyntaxSymbol("sq", nil), Init: lit()}},
			call(symRef("sq"))))
	envB := envWithImported(t, "*")
	unitLet := []ValidatedExpr{mkSq(), useLet}
	markStableInUnit(unitLet, "sq")
	if reclaimNames(unitLet, envB)["use"] {
		t.Fatalf("let shadow: use must NOT be reclaimable — (sq) calls the let binding, not the Stable top-level sq")
	}

	// (c) CONTROL: genuine same-unit call, no shadow — (define (use) (sq 3)) is
	// reclaimable (sq Stable ⇒ immutable edge to a safe node). Proves the shadow
	// guard does not over-reject legitimate same-unit calls.
	useReal := defineFn("use", call(symRef("sq"), lit()))
	envC := envWithImported(t, "*")
	unitReal := []ValidatedExpr{mkSq(), useReal}
	markStableInUnit(unitReal, "sq")
	if !reclaimNames(unitReal, envC)["use"] {
		t.Fatalf("control: a genuine same-unit call to Stable sq MUST be reclaimable")
	}
}

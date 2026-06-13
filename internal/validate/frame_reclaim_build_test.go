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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// envWithImported builds a runtime env whose global frame holds an imported
// binding for each given name. internal/validate sits below the registry, so
// the real continuation/arithmetic primitives are not available here — these
// stand-in imported globals exercise the builder's resolution + IsImported
// gate exactly as the real ones would.
func envWithImported(t *testing.T, names ...string) *environment.EnvironmentFrame {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	for _, name := range names {
		sym := syntax.NewSyntaxSymbol(name, nil).Sym
		env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
		b := env.GetBinding(sym, nil)
		if b == nil {
			t.Fatalf("failed to create global binding %q", name)
		}
		b.EnsureMeta().Imported = true
	}
	return env
}

// stampStable creates (or reuses) a global binding for each name and stamps it
// Stable, so IsStable() returns true — modelling a same-unit top-level define
// the producer has proven rebind-stable (StableInUnit = defined-once ∧
// never-set!, made sound by Option-B enforcement). internal/validate cannot run
// a full compile to populate the bit naturally, so the classifier's Stable read
// is exercised by stamping the bit by hand. An env where a define is NOT stamped
// models the tier-(a) case (flag off, or non-compiled env): the same-unit edge
// stays mutable.
func stampStable(t *testing.T, env *environment.EnvironmentFrame, names ...string) {
	t.Helper()
	for _, name := range names {
		sym := syntax.NewSyntaxSymbol(name, nil).Sym
		env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
		b := env.GetBinding(sym, nil)
		if b == nil {
			t.Fatalf("failed to create global binding %q", name)
		}
		b.EnsureMeta().Stable = true
	}
}

func TestBuildReclaimGraph_DetectsCallCC(t *testing.T) {
	env := envWithImported(t, "call/cc")
	// (define (bad k) (call/cc k)) — references the capture primitive, no
	// escaping lambda, so this isolates capture-operator detection.
	bad := defineFn("bad", call(symRef("call/cc"), symRef("k")))
	nodes, byName := buildReclaimGraph([]ValidatedExpr{bad}, env)
	v := mayCapture(nodes)
	if frameReclaimable(byName["bad"], v) {
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
	nodes, byName := buildReclaimGraph([]ValidatedExpr{sq, use}, env)
	v := mayCapture(nodes)
	if !frameReclaimable(byName["sq"], v) {
		t.Fatalf("sq over only the capture-safe primitive * must be reclaimable")
	}
	if frameReclaimable(byName["use"], v) {
		t.Fatalf("use calls a non-Stable top-level define ⇒ NOT reclaimable")
	}
}

func TestBuildReclaimGraph_EscapingClosureNotReclaimable(t *testing.T) {
	env := envWithImported(t)
	// (define (esc) (lambda () 1)) — returns a closure that escapes.
	esc := defineFn("esc", lam(lit()))
	nodes, byName := buildReclaimGraph([]ValidatedExpr{esc}, env)
	v := mayCapture(nodes)
	if frameReclaimable(byName["esc"], v) {
		t.Fatalf("a define returning an escaping closure must not be reclaimable")
	}
}

func TestBuildReclaimGraph_UnlistedPrimitiveNotReclaimable(t *testing.T) {
	env := envWithImported(t, "map")
	// (define (cm) (map f xs)) — map is imported but NOT on the capture-safe
	// whitelist (it invokes a procedure that could capture) ⇒ unresolved edge
	// ⇒ NOT reclaimable. This is the sound-by-default guarantee.
	cm := defineFn("cm", call(symRef("map"), symRef("f"), symRef("xs")))
	nodes, byName := buildReclaimGraph([]ValidatedExpr{cm}, env)
	v := mayCapture(nodes)
	if frameReclaimable(byName["cm"], v) {
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
	a := ClassifyFrameReclaim(mkUnit(), envA)
	if !a["sq"] || a["use"] {
		t.Fatalf("no-Stable env: want sq reclaimable, use not — got sq=%v use=%v", a["sq"], a["use"])
	}

	// Env with sq stamped Stable (tier-(b) equivalent): use→sq immutable, sq safe.
	envB := envWithImported(t, "*")
	stampStable(t, envB, "sq")
	b := ClassifyFrameReclaim(mkUnit(), envB)
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
	if ClassifyFrameReclaim(mkUnit(), envA)["fib"] {
		t.Fatalf("no-Stable env: a self-recursive top-level define must NOT be reclaimable (mutable self-edge)")
	}

	envB := envWithImported(t, "+", "-")
	stampStable(t, envB, "fib")
	if !ClassifyFrameReclaim(mkUnit(), envB)["fib"] {
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

	// Control: defined-once, never-set! ⇒ producer stamps Stable ⇒ reclaimable.
	control := envWithImported(t, "-")
	stampStable(t, control, "f")
	if !ClassifyFrameReclaim(mkUnit(), control)["f"] {
		t.Fatalf("control (Stable f): self-recursive define over safe primitives MUST be reclaimable")
	}

	// Bug case: twice-defined ⇒ producer leaves f non-Stable ⇒ NOT reclaimable.
	// The old tier-(b) `!mutated` code returned true here — the soundness gap.
	twiceDefined := envWithImported(t, "-")
	if ClassifyFrameReclaim(mkUnit(), twiceDefined)["f"] {
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
	a := ClassifyFrameReclaim(mkUnit(), envA)
	if a["f"] || a["g"] {
		t.Fatalf("no-Stable env: mutual recursion over mutable edges must NOT be reclaimable — got f=%v g=%v", a["f"], a["g"])
	}

	envB := envWithImported(t, "-")
	stampStable(t, envB, "f", "g")
	b := ClassifyFrameReclaim(mkUnit(), envB)
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
	b := ClassifyFrameReclaim(unit, env)
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
	a := ClassifyFrameReclaim(mkUnit(), envA)
	if !a["sq"] || a["use"] {
		t.Fatalf("begin-wrapped no-Stable: want sq reclaimable, use not — got sq=%v use=%v", a["sq"], a["use"])
	}

	envB := envWithImported(t, "*")
	stampStable(t, envB, "sq")
	b := ClassifyFrameReclaim(mkUnit(), envB)
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
	nodes, byName := buildReclaimGraph(mkUnit(), env)
	vA := mayCapture(nodes)
	if frameReclaimable(byName["g"], vA) {
		t.Fatalf("a define whose body contains a quasiquote must not be reclaimable (no-Stable env)")
	}

	stableEnv := envWithImported(t)
	stampStable(t, stableEnv, "g")
	if ClassifyFrameReclaim(mkUnit(), stableEnv)["g"] {
		t.Fatalf("a define whose body contains a quasiquote must not be reclaimable (Stable env)")
	}
}

// TestBuildReclaimGraph_ImmutableEdgeImpliesStable is the kill-criterion guard
// (plan Risks): the soundness invariant lives on the edge, not the node verdict.
// Every reclaimEdge marked immutable MUST point at a callee binding that
// IsStable() in env — because `immutable` is assigned from exactly that read.
// This introspects the built graph and fails if any immutable edge resolves to a
// non-Stable (rebindable) binding, catching a future regression that decouples
// the flag from the read.
func TestBuildReclaimGraph_ImmutableEdgeImpliesStable(t *testing.T) {
	// (define (sq x) (* x x)) (define (use) (sq 3)) with sq Stable: use→sq is the
	// one immutable same-unit edge; sq→* contributes no edge (capture-safe).
	sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	use := defineFn("use", call(symRef("sq"), lit()))
	env := envWithImported(t, "*")
	stampStable(t, env, "sq")

	nodes, _ := buildReclaimGraph([]ValidatedExpr{sq, use}, env)
	immutableEdges := 0
	for _, n := range nodes {
		for _, e := range n.callees {
			if !e.immutable {
				continue
			}
			immutableEdges++
			sym := syntax.NewSyntaxSymbol(e.target.label, nil).Sym
			b := env.GetBinding(sym, nil)
			if b == nil || !b.IsStable() {
				t.Fatalf("immutable edge %s→%s resolves to a non-Stable binding — soundness invariant violated",
					n.label, e.target.label)
			}
		}
	}
	if immutableEdges == 0 {
		t.Fatalf("expected at least one immutable edge (use→sq) to exercise the invariant")
	}
}

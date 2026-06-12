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
	// (define (use) (sq 3))     — calls a top-level define; tier-(a) treats it as
	//                             NOT immutable ⇒ NOT reclaimable.
	sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	use := defineFn("use", call(symRef("sq"), lit()))
	nodes, byName := buildReclaimGraph([]ValidatedExpr{sq, use}, env)
	v := mayCapture(nodes)
	if !frameReclaimable(byName["sq"], v) {
		t.Fatalf("sq over only the capture-safe primitive * must be reclaimable")
	}
	if frameReclaimable(byName["use"], v) {
		t.Fatalf("use calls a top-level define (tier-a non-immutable) ⇒ NOT reclaimable")
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

// TestClassifyFrameReclaim_TierBUnlocksTopLevel is the Phase-2 measurement core:
// the optimistic top-level tier (b) flips a same-unit top-level-define edge from
// mutable to immutable, so a function whose only constraint is a call to another
// (safe) top-level define becomes reclaimable. This is the difference Phase 2
// measures as recovered_toplevel − recovered_local.
func TestClassifyFrameReclaim_TierBUnlocksTopLevel(t *testing.T) {
	env := envWithImported(t, "*")
	// (define (sq x) (* x x)) (define (use) (sq 3))
	sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	use := defineFn("use", call(symRef("sq"), lit()))
	unit := []ValidatedExpr{sq, use}

	a := ClassifyFrameReclaim(unit, env, TierLocal)
	if !a["sq"] || a["use"] {
		t.Fatalf("tier (a): want sq reclaimable, use not — got sq=%v use=%v", a["sq"], a["use"])
	}
	b := ClassifyFrameReclaim(unit, env, TierTopLevel)
	if !b["sq"] || !b["use"] {
		t.Fatalf("tier (b): want both reclaimable (use→sq immutable, sq safe) — got sq=%v use=%v", b["sq"], b["use"])
	}
}

// TestClassifyFrameReclaim_SelfRecursiveTopLevel covers the Gabriel-shaped case:
// a self-recursive top-level define (fib/tak) is non-reclaimable under tier (a)
// because its self-edge is mutable, but reclaimable under tier (b).
func TestClassifyFrameReclaim_SelfRecursiveTopLevel(t *testing.T) {
	env := envWithImported(t, "+", "-")
	// (define (fib n) (+ (fib (- n 1)) (fib (- n 2))))
	fib := defineFn("fib",
		call(symRef("+"),
			call(symRef("fib"), call(symRef("-"), symRef("n"), lit())),
			call(symRef("fib"), call(symRef("-"), symRef("n"), lit()))))
	unit := []ValidatedExpr{fib}
	if ClassifyFrameReclaim(unit, env, TierLocal)["fib"] {
		t.Fatalf("tier (a): a self-recursive top-level define must NOT be reclaimable (mutable self-edge)")
	}
	if !ClassifyFrameReclaim(unit, env, TierTopLevel)["fib"] {
		t.Fatalf("tier (b): a self-recursive top-level define over safe primitives MUST be reclaimable")
	}
}

// TestClassifyFrameReclaim_TierBRespectsSetBang is the soundness guard on tier
// (b): a top-level define that is set! anywhere in the unit is NOT immutable, so
// its callers stay non-reclaimable even under tier (b). set! on a callee's
// binding does not, however, make the callee's OWN frame escapable.
func TestClassifyFrameReclaim_TierBRespectsSetBang(t *testing.T) {
	env := envWithImported(t, "*")
	// (define (sq x) (* x x)) (define (use) (sq 3)) (set! sq #f)
	sq := defineFn("sq", call(symRef("*"), symRef("x"), symRef("x")))
	use := defineFn("use", call(symRef("sq"), lit()))
	mut := setBang("sq", lit())
	unit := []ValidatedExpr{sq, use, mut}

	b := ClassifyFrameReclaim(unit, env, TierTopLevel)
	if b["use"] {
		t.Fatalf("tier (b): use must NOT be reclaimable when its callee sq is set! in-unit")
	}
	if !b["sq"] {
		t.Fatalf("tier (b): sq's own frame stays reclaimable — set! mutates the binding, not the frame")
	}
}

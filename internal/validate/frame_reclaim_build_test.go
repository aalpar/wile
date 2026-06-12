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

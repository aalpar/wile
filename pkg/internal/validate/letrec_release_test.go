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
)

// mutualPair builds a two-binding letrec group of the given kind whose members
// are the supplied lambdas, bound to "ev" and "od".
func mutualPair(kind LetKind, ev, od *ValidatedLambda) *ValidatedLet {
	return letBinds(kind, []string{"ev", "od"},
		[]ValidatedExpr{ev, od},
		call(symRef("ev"), symRef("n")))
}

// TestLetBindingFrameReleasable covers Phase D's predicate directly, because the
// allocation-slope probes in pkg/wile cannot discriminate here: when the group
// co-induction is broken, the UNSAFE member keeps allocating, so the measured
// slope stays above any sane floor while the safe member is released anyway.
// Three mutants survived that suite and are killed here.
func TestLetBindingFrameReleasable(t *testing.T) {
	t.Run("mutual recursion over capture-safe primitives", func(t *testing.T) {
		env := envWithImported(t, "=", "-")
		v := mutualPair(LetKindLetrec,
			lam(call(symRef("od"), call(symRef("-"), symRef("i")))),
			lam(call(symRef("ev"), call(symRef("-"), symRef("i")))))
		if !LetBindingFrameReleasable(v, 0, env) {
			t.Error("mutual local recursion must be releasable — neither member has a " +
				"depth-0 self call, so this is the release path or nothing")
		}
	})

	// THE MUTANT THIS KILLS: verifying only the binding asked about instead of
	// every member of the group. `ev` alone looks clean — its only callee is `od`,
	// which the group seed clears. That clearance is an ASSUMPTION about `od`, and
	// `od` captures, so it must not be granted.
	t.Run("a sibling captures the continuation", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "call/cc")
		v := mutualPair(LetKindLetrec,
			lam(call(symRef("od"), call(symRef("-"), symRef("i")))),
			lam(call(symRef("call/cc"), symRef("k"))))
		if LetBindingFrameReleasable(v, 0, env) {
			t.Error("a capturing sibling must refuse the WHOLE group: clearing the call to " +
				"od assumes od does not capture, and verifying only ev leaves that " +
				"assumption standing on itself")
		}
	})

	// THE MUTANT THIS KILLS: dropping the group-wide capture-operator scan. Same
	// shape as above but with the capture reached through a nested form, so a
	// mutant that only inspects the direct callee list still sees a clean `ev`.
	t.Run("a sibling captures below the top of its body", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "call/cc")
		v := mutualPair(LetKindLetrec,
			lam(call(symRef("od"), call(symRef("-"), symRef("i")))),
			lam(call(symRef("-"), call(symRef("call/cc"), symRef("k")))))
		if LetBindingFrameReleasable(v, 0, env) {
			t.Error("the capture scan is whole-tree and group-wide; a capture nested inside " +
				"a sibling's body pins the frame exactly as a top-level one does")
		}
	})

	// THE MUTANT THIS KILLS: dropping the InitsInScope precondition. In a plain
	// let the bindings are NOT in scope in each other's inits, so `od` inside ev's
	// body denotes an OUTER binding the group seed knows nothing about — seeding
	// it from these bindings describes the wrong procedure entirely.
	t.Run("plain let is not a recursive binding group", func(t *testing.T) {
		env := envWithImported(t, "=", "-")
		v := mutualPair(LetKindLet,
			lam(call(symRef("od"), call(symRef("-"), symRef("i")))),
			lam(call(symRef("ev"), call(symRef("-"), symRef("i")))))
		if LetBindingFrameReleasable(v, 0, env) {
			t.Error("a plain let's inits do not see the bindings, so the group seed would " +
				"describe outer bindings — InitsInScope is the guard")
		}
	})

	t.Run("a sibling calls a procedure-invoking callee", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "map", "proc", "xs")
		v := mutualPair(LetKindLetrec,
			lam(call(symRef("od"), call(symRef("-"), symRef("i")))),
			lam(call(symRef("map"), symRef("proc"), symRef("xs"))))
		if LetBindingFrameReleasable(v, 0, env) {
			t.Error("map invokes an unknown callback, which could capture the continuation " +
				"that pins the frame")
		}
	})

	t.Run("the binding creates an escaping closure", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "cons")
		v := mutualPair(LetKindLetrec,
			lam(call(symRef("cons"), lam(symRef("i"))), call(symRef("od"), symRef("i"))),
			lam(call(symRef("ev"), call(symRef("-"), symRef("i")))))
		if LetBindingFrameReleasable(v, 0, env) {
			t.Error("a closure created in this binding's own body parents the very frame " +
				"being released — the one escape check that is per-binding, not group-wide")
		}
	})

	// The escape check is per-binding on purpose: a sibling's escaping closure
	// parents the SIBLING's frame, not this one, and the sibling is refused when
	// it is itself compiled. Over-applying it group-wide would be sound but would
	// forgo the optimization for no reason, so the asymmetry is pinned.
	t.Run("a sibling's escaping closure does not refuse this binding", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "cons")
		v := mutualPair(LetKindLetrec,
			lam(call(symRef("od"), call(symRef("-"), symRef("i")))),
			lam(call(symRef("cons"), lam(symRef("i"))), call(symRef("ev"), symRef("i"))))
		if !LetBindingFrameReleasable(v, 0, env) {
			t.Error("ev's frame is not parented by a closure od creates; the escape clause " +
				"is per-binding and must not spread across the group")
		}
		if LetBindingFrameReleasable(v, 1, env) {
			t.Error("od itself creates the escaping closure and must be refused")
		}
	})

	t.Run("a non-lambda member is not assumed safe", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "h")
		v := letBinds(LetKindLetrec, []string{"ev", "od"},
			[]ValidatedExpr{
				lam(call(symRef("od"), call(symRef("-"), symRef("i")))),
				symRef("h"),
			},
			call(symRef("ev"), symRef("n")))
		if LetBindingFrameReleasable(v, 0, env) {
			t.Error("od is bound to whatever h denotes — possibly a capturing procedure — so " +
				"the seed records no evidence for it and the call to it must refuse")
		}
	})
}

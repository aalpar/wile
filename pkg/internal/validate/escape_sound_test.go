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

	"github.com/aalpar/wile/pkg/syntax"
)

// scopedSym builds a reference carrying the given scopes, for the cases that
// turn on hygiene rather than on the spelling of a name.
func scopedSym(name string, scopes ...*syntax.Scope) *ValidatedSymbol {
	sym := syntax.NewSyntaxSymbol(name, nil)
	for _, s := range scopes {
		sym = sym.AddScope(s).(*syntax.SyntaxSymbol)
	}
	return &ValidatedSymbol{
		formName: "@symbol",
		Symbol:   sym,
	}
}

// scopedBinder is scopedSym's binder-side twin: a let binding whose NAME carries
// scopes, so a reference must satisfy binderScopes ⊆ refScopes to count.
func scopedBinder(name string, init ValidatedExpr, scopes ...*syntax.Scope) ValidatedLetBinding {
	sym := syntax.NewSyntaxSymbol(name, nil)
	for _, s := range scopes {
		sym = sym.AddScope(s).(*syntax.SyntaxSymbol)
	}
	return ValidatedLetBinding{Name: sym, Init: init}
}

// TestLetBoundClosureEscapes covers the Lever B predicate: a let-bound lambda
// escapes unless every reference to it is a call.
//
// The negatives are the load-bearing half. This predicate gates frame reuse, so
// a false "does not escape" is a use-after-release — the frame is recycled while
// a closure that parents it is still reachable. Each negative below names the
// mutant it kills; a relaxation that survives all of them is the one to worry
// about, because the three historical reverts in this area all passed a green
// suite before failing under continuation re-entry.
func TestLetBoundClosureEscapes(t *testing.T) {
	tests := []struct {
		name string
		let  *ValidatedLet
		want bool
		why  string
	}{
		{
			name: "called only",
			let: letBinds(LetKindLet, []string{"step"},
				[]ValidatedExpr{lam(call(symRef("+"), symRef("x")))},
				call(symRef("step"), symRef("n"))),
			want: false,
			why:  "the whole point of the lever: a helper that is only invoked cannot outlive the call",
		},
		{
			name: "named let loop",
			let: namedLet("loop", &ValidatedParams{},
				[]ValidatedExpr{call(symRef("loop"))}),
			want: false,
			why: "a named let is a letrec whose single binding is the loop lambda; the " +
				"self-reference inside its own body is a call, admitted co-inductively",
		},
		{
			name: "returned from the body",
			let: letBinds(LetKindLet, []string{"step"},
				[]ValidatedExpr{lam(symRef("x"))},
				symRef("step")),
			want: true,
			why:  "kills a mutant that only inspects call operators and never sees a bare reference",
		},
		{
			name: "passed as an argument",
			let: letBinds(LetKindLet, []string{"step"},
				[]ValidatedExpr{lam(symRef("x"))},
				call(symRef("cons"), symRef("step"), lit())),
			want: true,
			why: "cons is capture-safe yet RETAINS: capture-safety is about invocation, " +
				"not retention, so it must not be read as a licence to escape",
		},
		{
			name: "set! of the binding",
			let: letBinds(LetKindLet, []string{"step"},
				[]ValidatedExpr{lam(symRef("x"))},
				setBang("step", lam(symRef("y"))),
				call(symRef("step"), symRef("n"))),
			want: true,
			why:  "kills a mutant that treats RefSetBangTarget as harmless: the init stops describing the name",
		},
		{
			name: "escapes through a sibling init",
			let: letBinds(LetKindLetrec, []string{"step", "keep"},
				[]ValidatedExpr{
					lam(symRef("x")),
					call(symRef("cons"), symRef("step"), lit()),
				},
				call(symRef("step"), symRef("n"))),
			want: true,
			why:  "kills a mutant that walks only the let body and not the binding inits",
		},
		{
			name: "non-lambda init is never clearable",
			let: letBinds(LetKindLet, []string{"step"},
				[]ValidatedExpr{symRef("h")},
				call(symRef("step"), symRef("n"))),
			want: true,
			why: "kills a mutant that clears any called binding: (let ((sq h)) (sq 3)) binds " +
				"sq to whatever the caller passed, which may capture",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := letBoundClosureEscapes(tt.let, 0)
			if got != tt.want {
				t.Errorf("letBoundClosureEscapes = %v, want %v — %s", got, tt.want, tt.why)
			}
		})
	}
}

// TestLetBoundClosureEscapes_ScopesDiscriminate pins the hygiene half. Two
// bindings spell the same name in disjoint scopes: one is only called, the other
// is consed. Matching on the name alone would let the second one's escape
// contaminate the first, and the lever would refuse every loop whose name
// collides with an unrelated binding elsewhere in the same form.
//
// The soundness direction is the reason this is a subset test and not equality:
// a genuine reference always carries the binder's scopes plus whatever the
// intervening forms added, so requiring binderScopes ⊆ refScopes can never drop
// a real reference — only unrelated same-name ones.
func TestLetBoundClosureEscapes_ScopesDiscriminate(t *testing.T) {
	inner := syntax.NewScopeWithLabel("inner")
	outer := syntax.NewScopeWithLabel("outer")

	// binding 0: `step` at scope {outer}, referenced only as a call at {outer}.
	// The consing reference spells `step` too, but at {inner} — a different
	// binding, invisible to this one.
	v := &ValidatedLet{
		formName: "let",
		Kind:     LetKindLet,
		Bindings: []ValidatedLetBinding{
			scopedBinder("step", lam(symRef("x")), outer),
		},
		body: []ValidatedExpr{
			call(scopedSym("step", outer), symRef("n")),
			call(symRef("cons"), scopedSym("step", inner), lit()),
		},
	}
	got := letBoundClosureEscapes(v, 0)
	if got {
		t.Errorf("letBoundClosureEscapes = true, want false — the consing reference is a " +
			"DIFFERENT binding at scope {inner}; binderScopes {outer} is not a subset of " +
			"it, so it must not be attributed to this binding")
	}

	// Control: the same reference at a scope that DOES include the binder's must
	// count, or the subset test has been inverted into unsoundness.
	v.body[1] = call(symRef("cons"), scopedSym("step", outer, inner), lit())
	got = letBoundClosureEscapes(v, 0)
	if !got {
		t.Errorf("letBoundClosureEscapes = false, want true — a reference at {outer,inner} " +
			"DOES resolve to a binder at {outer} (subset), so consing it is a real escape")
	}
}

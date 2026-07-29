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

// orShaped builds the (let ((t E)) (if t t B)) form `or` expands to, with the
// binder and both `if` head positions spelled at the same scopes.
func orShaped(name string, init ValidatedExpr, alt ValidatedExpr, scopes ...*syntax.Scope) *ValidatedLet {
	v := &ValidatedLet{
		validatedBase: validatedBase{formName: "let"},
		Kind:          LetKindLet,
		Bindings:      []ValidatedLetBinding{scopedBinder(name, init, scopes...)},
		body: []ValidatedExpr{
			ifx(scopedSym(name, scopes...), scopedSym(name, scopes...), alt),
		},
	}
	return v
}

// TestLetIsOrShaped covers the positive recognition. The frame this identifies
// is unobservable: the bound name is the test and the consequent of one `if` and
// occurs nowhere else, so the value the test leaves in the value register IS the
// consequent's value and the binding needs no slot.
func TestLetIsOrShaped(t *testing.T) {
	init := call(symRef("f"), symRef("n"))
	alt := call(symRef("g"), symRef("n"))
	v := orShaped("t", init, alt)

	gotInit, gotAlt, ok := LetIsOrShaped(v)
	if !ok {
		t.Fatal("LetIsOrShaped = false, want true — this is exactly or's expansion")
	}
	if gotInit != ValidatedExpr(init) {
		t.Errorf("init = %#v, want the binding's own init", gotInit)
	}
	if gotAlt != ValidatedExpr(alt) {
		t.Errorf("alt = %#v, want the if's alternative", gotAlt)
	}
}

// TestLetIsOrShapedRefusals is the load-bearing half. Lowering drops the frame,
// so every case here would change the meaning of the program if admitted.
func TestLetIsOrShapedRefusals(t *testing.T) {
	// THE CASE THAT MAKES THE PREDICATE MORE THAN A SHAPE MATCH. `or`'s temp is
	// hygienic and cannot appear in the alternative, but the same shape written by
	// hand can. After lowering the alternative is evaluated in the OUTER frame, so
	// a reference there would resolve to a different binding or to nothing.
	t.Run("binder referenced in the alternative", func(t *testing.T) {
		v := orShaped("t", lit(), call(symRef("g"), symRef("t")))
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: `t` in the alternative loses its binding when the frame goes")
		}
	})

	t.Run("binder set! in the alternative", func(t *testing.T) {
		v := orShaped("t", lit(), &ValidatedSetBang{
			validatedBase: validatedBase{formName: "set!"},
			Name:          syntax.NewSyntaxSymbol("t", nil),
			subExp:        lit(),
		})
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: a set! target is a reference to the binding")
		}
	})

	// (cond (test => f)) expands to (let ((t test)) (if t (f t) ...)) — the same
	// let, but the consequent CONSUMES the bound value rather than returning it,
	// so the register passthrough does not apply.
	t.Run("consequent is not the binder", func(t *testing.T) {
		v := &ValidatedLet{
			validatedBase: validatedBase{formName: "let"},
			Kind:          LetKindLet,
			Bindings:      []ValidatedLetBinding{scopedBinder("t", lit())},
			body: []ValidatedExpr{
				ifx(symRef("t"), call(symRef("f"), symRef("t")), lit()),
			},
		}
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: the cond-=> shape consumes the bound value")
		}
	})

	t.Run("test is not the binder", func(t *testing.T) {
		v := &ValidatedLet{
			validatedBase: validatedBase{formName: "let"},
			Kind:          LetKindLet,
			Bindings:      []ValidatedLetBinding{scopedBinder("t", lit())},
			body:          []ValidatedExpr{ifx(symRef("x"), symRef("t"), lit())},
		}
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: the value register holds x, not t")
		}
	})

	t.Run("no alternative", func(t *testing.T) {
		v := &ValidatedLet{
			validatedBase: validatedBase{formName: "let"},
			Kind:          LetKindLet,
			Bindings:      []ValidatedLetBinding{scopedBinder("t", lit())},
			body:          []ValidatedExpr{ifx(symRef("t"), symRef("t"), nil)},
		}
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: a one-armed if yields void on the false branch, " +
				"which the lowering has no code path for")
		}
	})

	t.Run("letrec kind", func(t *testing.T) {
		v := orShaped("t", lit(), lit())
		v.Kind = LetKindLetrec
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: a letrec init is evaluated with the binding in " +
				"scope, so it cannot be hoisted out of the frame")
		}
	})

	t.Run("two bindings", func(t *testing.T) {
		v := letBinds(LetKindLet, []string{"t", "u"},
			[]ValidatedExpr{lit(), lit()},
			ifx(symRef("t"), symRef("t"), lit()))
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: `u` still needs a slot")
		}
	})

	t.Run("two body expressions", func(t *testing.T) {
		v := orShaped("t", lit(), lit())
		v.body = append(v.body, lit())
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: the if is no longer the whole body")
		}
	})

	t.Run("named let", func(t *testing.T) {
		v := orShaped("t", lit(), lit())
		v.Tag = syntax.NewSyntaxSymbol("loop", nil)
		_, _, ok := LetIsOrShaped(v)
		if ok {
			t.Error("must refuse: a tag binds a loop procedure over this frame")
		}
	})
}

// TestLetIsOrShapedScopesDiscriminate pins the hygiene half in both directions.
// The alternative scan must attribute a same-name occurrence to this binding
// whenever binderScopes ⊆ refScopes — the subset test cannot drop a genuine
// reference — while an occurrence at a disjoint scope is a different binding and
// must not cost the lowering.
func TestLetIsOrShapedScopesDiscriminate(t *testing.T) {
	outer := syntax.NewScopeWithLabel("outer")
	inner := syntax.NewScopeWithLabel("inner")

	// The alternative mentions `t`, but at {inner} — an unrelated binding.
	v := orShaped("t", lit(), call(symRef("g"), scopedSym("t", inner)), outer)
	_, _, ok := LetIsOrShaped(v)
	if !ok {
		t.Error("LetIsOrShaped = false, want true — `t` at {inner} is a DIFFERENT " +
			"binding; binderScopes {outer} is not a subset of it")
	}

	// Control: at a scope that DOES include the binder's, it is a real reference.
	v = orShaped("t", lit(), call(symRef("g"), scopedSym("t", outer, inner)), outer)
	_, _, ok = LetIsOrShaped(v)
	if ok {
		t.Error("LetIsOrShaped = true, want false — a reference at {outer,inner} " +
			"DOES resolve to a binder at {outer}, so the frame is observable")
	}
}

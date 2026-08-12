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

package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// forwardRefProgram references g from f's body before g is defined. As one
// (begin ...) compilation unit the reference resolves; compiled per-form it does
// not.
const forwardRefProgram = `(define (f) (g))
(define (g) 42)
(f)`

// TestEngine_EvalProgram_ForwardReference verifies EvalProgram compiles the whole
// input as a single (begin ...) unit, so a define may forward-reference a later
// define — file/program semantics (R22).
func TestEngine_EvalProgram_ForwardReference(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	result, err := eng.EvalProgram(ctx, forwardRefProgram, "<test>")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "42")
}

// TestEngine_EvalMultiple_ForwardReferenceFails documents the contrast EvalProgram
// encapsulates: EvalMultiple compiles each top-level form independently, so the
// forward reference fails.
func TestEngine_EvalMultiple_ForwardReferenceFails(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, forwardRefProgram)
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("EvalMultiple should fail the forward reference that EvalProgram allows"))
}

// TestEngine_EvalProgram_StructuralWrap covers the cases where building the
// (begin ...) wrapper structurally (parse-then-wrap) is more robust than the old
// string surgery ("(begin " + code + "\n)").
func TestEngine_EvalProgram_StructuralWrap(t *testing.T) {
	ctx := context.Background()

	t.Run("trailing line comment without newline", func(t *testing.T) {
		// A naive string wrap "(begin " + code + ")" would let the trailing comment
		// swallow the synthetic close paren; the structural wrapper has no such paren.
		eng, err := wile.NewEngine(ctx)
		qt.Assert(t, err, qt.IsNil)
		result, err := eng.EvalProgram(ctx,
			"(define x 41)\n(+ x 1) ; eof comment, no newline", "<test>")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result.SchemeString(), qt.Equals, "42")
	})

	t.Run("empty and comment-only programs are void", func(t *testing.T) {
		eng, err := wile.NewEngine(ctx)
		qt.Assert(t, err, qt.IsNil)
		for _, src := range []string{"", "   \n\t ", "; just a comment", "#| block |#"} {
			result, evalErr := eng.EvalProgram(ctx, src, "<test>")
			qt.Assert(t, evalErr, qt.IsNil, qt.Commentf("src=%q", src))
			qt.Assert(t, result.IsVoid(), qt.IsTrue, qt.Commentf("src=%q", src))
		}
	})
}

// TestEngine_EvalProgram_WrapperHeadIsNotShadowable pins the scope on
// wrapInBegin's synthetic head.
//
// A special form is now shadowable by a top-level define (R7RS §4.3, decided by
// binding identity in validate.headDenotesSpecialForm rather than by the head's
// spelling). That made the program wrapper itself capturable: with (define begin
// 9) anywhere in the file, the (begin form ...) EvalProgram builds compiled to
// (9 form ...) — measured, it ran the whole program and then died with
// "application: expected a procedure, got #<void>". The user's binder carries no
// scopes, so a scoped head is out of its reach.
//
// `let` is the second row because it is the head every `or`, `and` and `cond`
// expansion goes through, so a capture there is the widest blast radius a single
// define can have.
func TestEngine_EvalProgram_WrapperHeadIsNotShadowable(t *testing.T) {
	ctx := context.Background()
	cases := []struct {
		name string
		src  string
		want string
	}{
		{name: "define begin", src: "(define begin 9)\n(let () 1 2 3)", want: "3"},
		{name: "define let", src: "(define let 3)\n(or #f 5)", want: "5"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			result, err := eng.EvalProgram(ctx, tc.src, "<test>")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestEngine_EvalProgram_DefinitionHeadIsNotSelfShadowed pins that a define
// whose name is a syntactic keyword is still a define. R7RS §5.3.1 explicitly
// contemplates it, Chez answers 3 for (define define 3) then define, and so did
// master.
//
// It regressed the moment special-form heads started being decided by binding
// identity: the expander's letrec* pre-scan predeclares a top-level define's
// ∅-scoped binder BEFORE validation resolves the head, so the head of the very
// form that CREATES the binding found it and demoted itself. The whole program
// then compiled to (#<void> define 3) and died at RUNTIME with "application:
// expected a procedure, got #<void>" — no compile diagnostic, exit 1 only after
// the earlier forms had already run.
//
// EvalProgram, not EvalMultiple, is the reproducing path and the only one that
// matters: the pre-scan runs over a (begin …) body, which is what EvalProgram
// (and the CLI's -e, which joins its expressions into one program) builds.
//
// The last row is why the question is asked as an identity compare rather than
// "this form is a define": once `define` IS a variable, a LATER define is an
// application of it. Measured on Petite Chez 10 — "(define define 3) (define foo
// 5)" reports "variable foo is not bound", i.e. Chez also read the second form as
// a call. That holds for a later define of the SAME name too, which is what the
// own-head exemption used to suppress and why deleting it cost nothing here: the
// rows below are green with and without it. The redefinition case is ratcheted
// in binding_model_matrix_test.go, not here.
func TestEngine_EvalProgram_DefinitionHeadIsNotSelfShadowed(t *testing.T) {
	ctx := context.Background()
	cases := []struct {
		name    string
		src     string
		want    string
		wantErr bool
	}{
		{name: "define define", src: "(define define 3)\ndefine", want: "3"},
		{name: "define define alone", src: "(define define 3)", want: "#<void>"},
		// The later-form shape: the failure was reported at <eval>:2:1, so the
		// demotion is not an artifact of the define being first in the program.
		{name: "define define in a later form",
			src: "(+ 1 1)\n(define define 3)\ndefine", want: "3"},
		// define-syntax is a keyword the pre-scan does NOT predeclare (it binds in
		// the expand environment), so this row is a ratchet: green before and
		// after, pinning that the exemption did not have to reach it.
		{name: "define define-syntax", src: "(define define-syntax 3)\ndefine-syntax", want: "3"},
		{name: "define-syntax define-syntax",
			src:  "(define-syntax define-syntax (syntax-rules () ((_ a b) 7)))\n(define-syntax p q)",
			want: "7"},
		{name: "a later define of another name is a call",
			src: "(define define 3)\n(define foo 5)", wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			result, err := eng.EvalProgram(ctx, tc.src, "<test>")
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil,
					qt.Commentf("a define form headed by a variable must be a call"))
				return
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

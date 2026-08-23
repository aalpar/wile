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

// TestQuasisyntaxDottedTail is the quasisyntax twin of
// TestQuasiquoteDottedTailDepth, and it exists because "quasisyntax does not
// implement the dotted form" turned out to describe two different things.
//
// Declining #`(a . #,x) is a design choice — no obligation source covers
// quasisyntax, so R7RS §7.1.4 cannot be cited against it. But the same omission
// also made #`(a . #`(b #,x)) read its tail ELEMENT-WISE, which put #,x at
// depth 1 and fired it, where every implementation of the notation keeps it
// literal at depth 2. That is a wrong datum, not a declined feature, and the two
// are inseparable: one spine test decides both (compilation.dottedTailCell).
// The flag that held them apart was deleted on 2026-08-23; that test now takes
// no dialect parameter at all, so the two dialects cannot drift again.
//
// Every `want` below is Chez Scheme's answer. Racket agrees on all of them but
// the two marked as its rejects, which it refuses to compile rather than
// rendering differently — so on this corpus there is no shape where the two
// oracles disagree with each other, only shapes where one declines to answer.
//
// The mirror matters more than any single row: run the same template through
// quasiquote and the answers correspond exactly, keyword for keyword. The tail
// reading is now ONE rule wearing two spellings, which is the property the
// shape-level ratchet (TestQuasiExpandShape's dotted rows) pins structurally and
// this one pins by value.
func TestQuasisyntaxDottedTail(t *testing.T) {
	const preamble = "(define x 5) (define y 7) (define xs '(1 2)) "

	cases := []struct {
		name string
		code string
		want string
	}{
		{
			// The declined form itself. Wile used to render (a unsyntax x).
			name: "dotted unsyntax evaluates",
			code: "#`(a . #,x)",
			want: "(a . 5)",
		},
		{
			// The wrong datum. The tail is ⟨quasisyntax 2⟩, so #,x stays
			// literal; Wile used to answer (a quasisyntax (b 5)).
			name: "nested quasisyntax as the tail",
			code: "#`(a . #`(b #,x))",
			want: "(a quasisyntax (b (unsyntax x)))",
		},
		{
			name: "nested quasisyntax as the tail, bare unsyntax inside",
			code: "#`(a . #`#,x)",
			want: "(a quasisyntax (unsyntax x))",
		},
		{
			// The outer unsyntax decrements and stays literal, the inner one
			// reaches depth 0 and fires.
			name: "dotted unsyntax at depth two",
			code: "#`#`(a . #,#,x)",
			want: "(quasisyntax (a unsyntax 5))",
		},
		{
			name: "dotted unsyntax at depth two, reached through a nesting form",
			code: "#`(a #`(b . #,#,x))",
			want: "(a (quasisyntax (b unsyntax 5)))",
		},
		{
			// Both rules at once, on the same cell.
			name: "dotted unsyntax at depth two under a dotted nesting form",
			code: "#`(a . #`(b . #,#,x))",
			want: "(a quasisyntax (b unsyntax 5))",
		},
		{
			name: "nested quasisyntax as the tail of a sublist",
			code: "#`((a . #`(b #,x)))",
			want: "((a quasisyntax (b (unsyntax x))))",
		},
		{
			// A vector is not a spine, but a list inside one is — the seam
			// quasiSpine holds, now exercised on both dialects.
			name: "nested quasisyntax as the tail of a list under a vector",
			code: "#`#((a . #`(b #,x)))",
			want: "#((a quasisyntax (b (unsyntax x))))",
		},

		// The five shapes that must NOT move.
		{
			// ⟨splicing unquotation D⟩ derives only from the ELEMENT production,
			// never the tail, so this stays three ordinary elements. Racket
			// rejects it outright ("quasisyntax: misuse within quasisyntax");
			// Chez renders it as Wile does.
			name: "splicing is not a tail",
			code: "#`(a . #,@xs)",
			want: "(a unsyntax-splicing xs)",
		},
		{
			// (syntax T) keeps the SAME depth, so the element-wise walk already
			// renders it correctly and quoting needs no spine case.
			name: "syntax as the tail keeps the depth",
			code: "#`(a . #'(b #,x))",
			want: "(a syntax (b 5))",
		},
		{
			name: "a genuine improper tail is untouched",
			code: "#`(a . b)",
			want: "(a . b)",
		},
		{
			name: "a vector improper tail is untouched",
			code: "#`(a . #(b))",
			want: "(a . #(b))",
		},
		{
			// A bare `unsyntax` on a VECTOR's own element sequence. The vector
			// admits no dotted tail however it was written, so the symbol stays
			// an ordinary element. Racket rejects this one too; Chez agrees with
			// Wile.
			name: "a vector spine has no tail",
			code: "#`#((#,x) a unsyntax x)",
			want: "#((5) a unsyntax x)",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()

			engine, err := wile.NewEngine(ctx)
			c.Assert(err, qt.IsNil)
			got, err := engine.EvalMultiple(ctx, preamble+"(syntax->datum "+tc.code+")")
			c.Assert(err, qt.IsNil, qt.Commentf("code: %s", tc.code))
			c.Assert(got.SchemeString(), qt.Equals, tc.want, qt.Commentf("code: %s", tc.code))
		})
	}
}

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

package compilation

import (
	"context"
	"fmt"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// quasiShapeCase is one template rendered against both dialects at both depths.
//
// The source carries placeholders rather than literal keywords so a single row
// states one structural question and the table answers it four ways. SPLICE,
// UNQ and NEST are replaced with the dialect's own spellings, which is the only
// way to ask "does quasisyntax see a splice here?" — the quasisyntax walk
// matches `unsyntax-splicing`, so a row spelled `unquote-splicing` silently
// tests the no-splice path instead.
type quasiShapeCase struct {
	name string
	src  string
	qq1  string // quasiquote, depth 1
	qq2  string // quasiquote, depth 2
	qs1  string // quasisyntax, depth 1
	qs2  string // quasisyntax, depth 2
}

// TestQuasiExpandShape pins the EXPANSION each template compiles to, not the
// value that expansion evaluates to. Several of the rules this cluster
// implements are invisible to a value assertion, and one of them is the reason
// this file exists:
//
// A malformed splice closes the preceding literal run. `flushNormal()` runs
// before the arity test, so `(a b (unquote-splicing) c ,@x d) expands to
// (append (list 'a 'b) (list '(unquote-splicing) 'c) x (list 'd)) — four
// arguments where one run would have given three. `append` of two lists equals
// the one list they concatenate to, so every value probe in the tree agrees
// with both shapes. The whole suite is blind to it.
//
// The other rows are the same kind of pin at lower stakes: which head gets
// synthesized (list / cons / append / list->vector), where the boundary between
// runs falls, and which arm a malformed form lands in. Together they are the
// ratchet for any change to the walk itself, since a walk that produces the
// right values by a different route has changed something the next change will
// depend on.
//
// Read against UnwrapAll: the expansion is syntax all the way down, and the
// `#'` prefixes on every node triple the width of each row without
// distinguishing any two of them.
func TestQuasiExpandShape(t *testing.T) {
	cases := []quasiShapeCase{
		{
			name: "plain",
			src:  "(a b c)",
			qq1:  "(list (quote a) (quote b) (quote c))",
			qq2:  "(list (quote a) (quote b) (quote c))",
			qs1:  "(list (syntax a) (syntax b) (syntax c))",
			qs2:  "(list (syntax a) (syntax b) (syntax c))",
		},
		{
			name: "empty",
			src:  "()",
			qq1:  "(quote ())",
			qq2:  "(quote ())",
			qs1:  "(syntax ())",
			qs2:  "(syntax ())",
		},
		{
			name: "unquote",
			src:  "(a (UNQ x) b)",
			qq1:  "(list (quote a) x (quote b))",
			qq2:  "(list (quote a) (list (quote unquote) (quote x)) (quote b))",
			qs1:  "(list (syntax a) x (syntax b))",
			qs2:  "(list (syntax a) (list (syntax unsyntax) (syntax x)) (syntax b))",
		},
		{
			name: "deep",
			src:  "(a (b (UNQ x)) c)",
			qq1:  "(list (quote a) (list (quote b) x) (quote c))",
			qq2:  "(list (quote a) (list (quote b) (list (quote unquote) (quote x))) (quote c))",
			qs1:  "(list (syntax a) (list (syntax b) x) (syntax c))",
			qs2:  "(list (syntax a) (list (syntax b) (list (syntax unsyntax) (syntax x))) (syntax c))",
		},
		{
			name: "splice",
			src:  "(a (SPLICE x) b)",
			qq1:  "(append (list (quote a)) x (list (quote b)))",
			qq2:  "(list (quote a) (list (quote unquote-splicing) (quote x)) (quote b))",
			qs1:  "(append (list (syntax a)) x (list (syntax b)))",
			qs2:  "(list (syntax a) (list (syntax unsyntax-splicing) (syntax x)) (syntax b))",
		},
		{
			name: "splice-first",
			src:  "((SPLICE x) b)",
			qq1:  "(append x (list (quote b)))",
			qq2:  "(list (list (quote unquote-splicing) (quote x)) (quote b))",
			qs1:  "(append x (list (syntax b)))",
			qs2:  "(list (list (syntax unsyntax-splicing) (syntax x)) (syntax b))",
		},
		{
			name: "splice-last",
			src:  "(a (SPLICE x))",
			qq1:  "(append (list (quote a)) x)",
			qq2:  "(list (quote a) (list (quote unquote-splicing) (quote x)))",
			qs1:  "(append (list (syntax a)) x)",
			qs2:  "(list (syntax a) (list (syntax unsyntax-splicing) (syntax x)))",
		},
		{
			// The row this file was written for. A malformed splice takes the
			// "treat as normal" arm, but only AFTER flushNormal has closed the
			// run — so `a b` and `c` land in two separate (list ...) arguments.
			name: "malformed-splice-run-split",
			src:  "(a b (SPLICE) c (SPLICE x) d)",
			qq1: "(append (list (quote a) (quote b))" +
				" (list (quote (unquote-splicing)) (quote c)) x (list (quote d)))",
			qq2: "(list (quote a) (quote b) (quote (unquote-splicing)) (quote c)" +
				" (list (quote unquote-splicing) (quote x)) (quote d))",
			qs1: "(append (list (syntax a) (syntax b))" +
				" (list (syntax (unsyntax-splicing)) (syntax c)) x (list (syntax d)))",
			qs2: "(list (syntax a) (syntax b) (syntax (unsyntax-splicing)) (syntax c)" +
				" (list (syntax unsyntax-splicing) (syntax x)) (syntax d))",
		},
		{
			// Same split with no well-formed splice anywhere: the prescan sees
			// the splicing symbol, so this still takes the append path even
			// though nothing is ever spliced.
			name: "malformed-splice-only",
			src:  "(a b (SPLICE) c)",
			qq1:  "(append (list (quote a) (quote b)) (list (quote (unquote-splicing)) (quote c)))",
			qq2:  "(list (quote a) (quote b) (quote (unquote-splicing)) (quote c))",
			qs1:  "(append (list (syntax a) (syntax b)) (list (syntax (unsyntax-splicing)) (syntax c)))",
			qs2:  "(list (syntax a) (syntax b) (syntax (unsyntax-splicing)) (syntax c))",
		},
		{
			// A malformed form is quoted verbatim rather than raising: this is
			// why the arity tests here return a bool and not an error.
			name: "bare-unquote-arity0",
			src:  "(UNQ)",
			qq1:  "(quote (unquote))",
			qq2:  "(quote (unquote))",
			qs1:  "(syntax (unsyntax))",
			qs2:  "(syntax (unsyntax))",
		},
		{
			name: "bare-splicing-arity0",
			src:  "(SPLICE)",
			qq1:  "(quote (unquote-splicing))",
			qq2:  "(quote (unquote-splicing))",
			qs1:  "(syntax (unsyntax-splicing))",
			qs2:  "(syntax (unsyntax-splicing))",
		},
		{
			// R7RS §4.2.8. `(a . ,x) READS as (a unquote x), so the dotted tail
			// arrives as a bare symbol on the spine. quasisyntax does not
			// implement the form (handleDottedUnquote is false), and the qs
			// columns are what that costs: three ordinary elements.
			name: "dotted-unquote",
			src:  "(a UNQ x)",
			qq1:  "(cons (quote a) x)",
			qq2:  "(list (quote a) (quote unquote) (quote x))",
			qs1:  "(list (syntax a) (syntax unsyntax) (syntax x))",
			qs2:  "(list (syntax a) (syntax unsyntax) (syntax x))",
		},
		{
			// The dotted tail reached through the splice walk rather than the
			// no-splice walk. Both arms carry the test; only this row separates
			// them.
			name: "dotted-unquote-with-splice",
			src:  "((SPLICE x) UNQ y)",
			qq1:  "(append x y)",
			qq2:  "(list (list (quote unquote-splicing) (quote x)) (quote unquote) (quote y))",
			qs1:  "(append x (list (syntax unsyntax) (syntax y)))",
			qs2:  "(list (list (syntax unsyntax-splicing) (syntax x)) (syntax unsyntax) (syntax y))",
		},
		{
			name: "dotted-in-sublist",
			src:  "((a . (UNQ x)))",
			qq1:  "(list (cons (quote a) x))",
			qq2:  "(list (list (quote a) (quote unquote) (quote x)))",
			qs1:  "(list (list (syntax a) (syntax unsyntax) (syntax x)))",
			qs2:  "(list (list (syntax a) (syntax unsyntax) (syntax x)))",
		},
		{
			name: "improper",
			src:  "(a . b)",
			qq1:  "(cons (quote a) (quote b))",
			qq2:  "(cons (quote a) (quote b))",
			qs1:  "(cons (syntax a) (syntax b))",
			qs2:  "(cons (syntax a) (syntax b))",
		},
		{
			// An improper tail on the splice path becomes append's final
			// argument, which R7RS lets be a non-list.
			name: "improper-with-splice",
			src:  "((SPLICE x) . b)",
			qq1:  "(append x (quote b))",
			qq2:  "(cons (list (quote unquote-splicing) (quote x)) (quote b))",
			qs1:  "(append x (syntax b))",
			qs2:  "(cons (list (syntax unsyntax-splicing) (syntax x)) (syntax b))",
		},
		{
			// The improper tail is itself a template and gets expanded, not
			// treated as inert.
			name: "improper-vector-tail",
			src:  "(a . #(b))",
			qq1:  "(cons (quote a) (list->vector (list (quote b))))",
			qq2:  "(cons (quote a) (list->vector (list (quote b))))",
			qs1:  "(cons (syntax a) (list->vector (list (syntax b))))",
			qs2:  "(cons (syntax a) (list->vector (list (syntax b))))",
		},
		{
			// Nesting shifts depth UP, so the inner unquote stays literal at
			// both entry depths.
			name: "nested-quasi",
			src:  "(a (NEST (b (UNQ x))) c)",
			qq1: "(list (quote a) (list (quote quasiquote)" +
				" (list (quote b) (list (quote unquote) (quote x)))) (quote c))",
			qq2: "(list (quote a) (list (quote quasiquote)" +
				" (list (quote b) (list (quote unquote) (quote x)))) (quote c))",
			qs1: "(list (syntax a) (list (syntax quasisyntax)" +
				" (list (syntax b) (list (syntax unsyntax) (syntax x)))) (syntax c))",
			qs2: "(list (syntax a) (list (syntax quasisyntax)" +
				" (list (syntax b) (list (syntax unsyntax) (syntax x)))) (syntax c))",
		},
		{
			name: "vector",
			src:  "#(1 (UNQ x) 3)",
			qq1:  "(list->vector (list (quote 1) x (quote 3)))",
			qq2:  "(list->vector (list (quote 1) (list (quote unquote) (quote x)) (quote 3)))",
			qs1:  "(list->vector (list (syntax 1) x (syntax 3)))",
			qs2:  "(list->vector (list (syntax 1) (list (syntax unsyntax) (syntax x)) (syntax 3)))",
		},
		{
			name: "vector-splice",
			src:  "#(1 (SPLICE x) 3)",
			qq1:  "(list->vector (append (list (quote 1)) x (list (quote 3))))",
			qq2:  "(list->vector (list (quote 1) (list (quote unquote-splicing) (quote x)) (quote 3)))",
			qs1:  "(list->vector (append (list (syntax 1)) x (list (syntax 3))))",
			qs2:  "(list->vector (list (syntax 1) (list (syntax unsyntax-splicing) (syntax x)) (syntax 3)))",
		},
		{
			name: "vector-malformed-splice",
			src:  "#(a b (SPLICE) c (SPLICE x) d)",
			qq1: "(list->vector (append (list (quote a) (quote b))" +
				" (list (quote (unquote-splicing)) (quote c)) x (list (quote d))))",
			qq2: "(list->vector (list (quote a) (quote b) (quote (unquote-splicing)) (quote c)" +
				" (list (quote unquote-splicing) (quote x)) (quote d)))",
			qs1: "(list->vector (append (list (syntax a) (syntax b))" +
				" (list (syntax (unsyntax-splicing)) (syntax c)) x (list (syntax d))))",
			qs2: "(list->vector (list (syntax a) (syntax b) (syntax (unsyntax-splicing)) (syntax c)" +
				" (list (syntax unsyntax-splicing) (syntax x)) (syntax d)))",
		},
		{
			name: "vector-empty",
			src:  "#()",
			qq1:  "(list->vector (list))",
			qq2:  "(list->vector (list))",
			qs1:  "(list->vector (list))",
			qs2:  "(list->vector (list))",
		},
		{
			// A dotted unquote inside a list nested under a vector. The vector
			// is not a spine, but the list inside it is, so the dotted arm must
			// still fire here.
			name: "nested-list-under-vector",
			src:  "#((a . (UNQ x)))",
			qq1:  "(list->vector (list (cons (quote a) x)))",
			qq2:  "(list->vector (list (list (quote a) (quote unquote) (quote x))))",
			qs1:  "(list->vector (list (list (syntax a) (syntax unsyntax) (syntax x))))",
			qs2:  "(list->vector (list (list (syntax a) (syntax unsyntax) (syntax x))))",
		},
		{
			// A bare `unquote` on the VECTOR's own element sequence. A vector
			// has no dotted tail, so the symbol stays an ordinary element and
			// must not reach the compiler as a live `unquote`.
			name: "vector-spine-bare-unquote",
			src:  "#((UNQ y) a UNQ x)",
			qq1:  "(list->vector (list y (quote a) (quote unquote) (quote x)))",
			qq2: "(list->vector (list (list (quote unquote) (quote y))" +
				" (quote a) (quote unquote) (quote x)))",
			qs1: "(list->vector (list y (syntax a) (syntax unsyntax) (syntax x)))",
			qs2: "(list->vector (list (list (syntax unsyntax) (syntax y))" +
				" (syntax a) (syntax unsyntax) (syntax x)))",
		},
	}

	dialects := []struct {
		name string
		kw   quasiKeywords
		want func(quasiShapeCase) (string, string)
	}{
		{
			name: "quasiquote",
			kw:   quasiquoteKW,
			want: func(tc quasiShapeCase) (string, string) {
				return tc.qq1, tc.qq2
			},
		},
		{
			name: "quasisyntax",
			kw:   quasisyntaxKW,
			want: func(tc quasiShapeCase) (string, string) {
				return tc.qs1, tc.qs2
			},
		},
	}

	for _, d := range dialects {
		rep := strings.NewReplacer("SPLICE", d.kw.splicing, "UNQ", d.kw.unquote, "NEST", d.kw.nesting)
		for _, tc := range cases {
			want1, want2 := d.want(tc)
			probes := []struct {
				depth int
				want  string
			}{
				{depth: 1, want: want1},
				{depth: 2, want: want2},
			}
			for _, probe := range probes {
				t.Run(fmt.Sprintf("%s/%s/depth%d", d.name, tc.name, probe.depth), func(t *testing.T) {
					c := qt.New(t)
					ccnt, env := newTestCompiler()
					stx := parseSchemeExpr(t, env, rep.Replace(tc.src))
					got, err := ccnt.expandQuasi(context.Background(), stx, probe.depth, d.kw, ccnt.newQuasiDepthGuard())
					c.Assert(err, qt.IsNil)
					c.Assert(got.UnwrapAll().SchemeString(), qt.Equals, probe.want)
				})
			}
		}
	}
}

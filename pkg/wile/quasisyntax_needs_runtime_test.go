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

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestQuasisyntaxNeedsRuntimeReachesVectorsAndTails pins the two places
// quasisyntax's literal-vs-runtime predicate has to look and used not to.
//
// compileQuasisyntaxTemplate emits the whole template as ONE literal when
// quasisyntaxNeedsRuntime says no unsyntax fires at this depth. The predicate
// therefore has to reach everywhere the expander does, and its answer is not
// an optimization: a false negative does not make the program slower, it makes
// the unsyntax stop happening and leaves the keyword in the output datum.
//
// Two shapes it missed. A vector had no case at all and fell to
// `default: return false`; the list walk `break`ed on a non-pair cdr, dropping
// the improper tail. expandQuasi handles both, so only the predicate was wrong,
// and the quasiquote twin already carries the tail fix — with a comment naming
// this bug — in what is now the shared quasiNeedsRuntimeList.
//
// The control column is the point: quasiquote answers correctly on the same
// shapes, so each row is a divergence between two dialects of one algorithm,
// not a question about what the right answer is.
func TestQuasisyntaxNeedsRuntimeReachesVectorsAndTails(t *testing.T) {
	tests := []struct {
		name string
		// qs and qq are the same template in both dialects; want is the datum
		// both must produce. Every row carries both — there is no shape left in
		// this file on which the dialects legitimately answer differently.
		qs   string
		qq   string
		want string
	}{
		{
			name: "unsyntax inside a vector",
			qs:   "#`#(1 #,x)",
			qq:   "`#(1 ,x)",
			want: "#(1 5)",
		},
		{
			name: "unsyntax-splicing inside a vector",
			qs:   "#`#(1 #,@xs)",
			qq:   "`#(1 ,@xs)",
			want: "#(1 1 2)",
		},
		{
			name: "vector as an improper tail",
			qs:   "#`(1 . #(#,x))",
			qq:   "`(1 . #(,x))",
			want: "(1 . #(5))",
		},
		{
			name: "vector as an improper tail under a quote",
			qs:   "#`(quote (1 . #(#,x)))",
			qq:   "`(quote (1 . #(,x)))",
			want: "(quote (1 . #(5)))",
		},
		{
			// The dotted form inside the vector's element. These three rows
			// carried no qq column while quasisyntax declined the dotted tail
			// (want was #((1 2 unsyntax x)), tracking quasisyntax alone). Both
			// dialects read the tail now, so the control column is available
			// again — and these are the rows where it earns the most, since the
			// element is a list but its container is not, which is the exact
			// seam quasiSpine holds.
			name: "splice and unsyntax in a dotted element under a vector",
			qs:   "#`#((#,@xs . #,x))",
			qq:   "`#((,@xs . ,x))",
			want: "#((1 2 . 5))",
		},
		{
			name: "two unsyntaxes in a dotted element under a vector",
			qs:   "#`#((#,x . #,y))",
			qq:   "`#((,x . ,y))",
			want: "#((5 . 7))",
		},
		{
			name: "nested vector inside a vector",
			qs:   "#`#(#,x #((n . #,y)) #,@xs)",
			qq:   "`#(,x #((n . ,y)) ,@xs)",
			want: "#(5 #((n . 7)) 1 2)",
		},
	}

	const preamble = "(define x 5) (define y 7) (define xs '(1 2)) "

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()

			engine, err := wile.NewEngine(ctx)
			c.Assert(err, qt.IsNil)
			got, err := engine.EvalMultiple(ctx, preamble+"(syntax->datum "+tc.qs+")")
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, tc.want)

			control, err := wile.NewEngine(ctx)
			c.Assert(err, qt.IsNil)
			gotQQ, err := control.EvalMultiple(ctx, preamble+tc.qq)
			c.Assert(err, qt.IsNil)
			c.Assert(gotQQ.SchemeString(), qt.Equals, tc.want)
		})
	}
}

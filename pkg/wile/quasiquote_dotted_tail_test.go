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

// Quasiquote dotted tails (R7RS §4.2.8), two confirmed codegen defects from
// reviews/2026-07-13.
//
// Both come from the same reader fact, which is worth stating once because it is
// what makes the bugs non-obvious: a dotted unquote is NOT an improper list. The
// reader turns `(a . ,y) into the PROPER four-element list (a unquote y). So any
// expander arm that expects a dotted tail to arrive as a non-pair cdr will never
// see it, and will instead walk the symbol `unquote` in as an ordinary element.
//
//   quasi_expand.go — the SPLICE path had no dotted-unquote arm (the non-splice
//     path did), so `(,@x . ,y) rendered as the list (1 2 unquote y).
//   compile_time_continuation_quasiquote.go — the needs-runtime fold BROKE on a
//     non-pair improper tail instead of asking whether it needs runtime, so the
//     unquote in `(1 . #(,x)) was never noticed and the form was emitted as a
//     literal: (1 . #((unquote x))).
//
// Both verified RED against the unfixed tree.

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

func TestQuasiquoteDottedTail(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			// The splice-path finding. Before the fix: (1 2 unquote y).
			name: "splice then dotted unquote",
			src:  "(let ((x '(1 2)) (y 3)) `(,@x . ,y))",
			want: "(1 2 . 3)",
		},
		{
			name: "literal, splice, then dotted unquote",
			src:  "(let ((x '(1 2)) (y 3)) `(a ,@x . ,y))",
			want: "(a 1 2 . 3)",
		},
		{
			// The needs-runtime fold finding. Before the fix: (1 . #((unquote x))).
			name: "unquote inside a vector in the improper tail",
			src:  "(let ((x 7)) `(1 . #(,x)))",
			want: "(1 . #(7))",
		},
		{
			// The non-splice dotted-unquote arm already worked. Kept as the control:
			// it is what the splice path was missing, so if this ever breaks, the two
			// arms have diverged again.
			name: "dotted unquote, no splice",
			src:  "(let ((y 3)) `(1 . ,y))",
			want: "(1 . 3)",
		},
		{
			name: "dotted unquote after several elements",
			src:  "(let ((y 3)) `(1 2 . ,y))",
			want: "(1 2 . 3)",
		},
		{
			// A dotted tail that is genuinely improper and NOT an unquote must still
			// come through untouched — the new arm must not capture it.
			name: "plain improper tail is untouched",
			src:  "`(1 . 2)",
			want: "(1 . 2)",
		},
		{
			// Splicing with a proper tail must be unaffected by the new arm.
			name: "splice with proper tail",
			src:  "(let ((x '(1 2))) `(0 ,@x 9))",
			want: "(0 1 2 9)",
		},
		{
			name: "unquote in a vector, proper list",
			src:  "(let ((x 7)) `#(,x))",
			want: "#(7)",
		},
		{
			// The depth guard on the new arm. Inside a NESTED quasiquote the dotted
			// unquote sits at depth 2, where R7RS §4.2.8 says it is data, not a tail
			// to evaluate — substitution happens only at the outermost level. The
			// arm's `depth == 1` condition is what keeps this literal; drop it and y
			// gets evaluated here too.
			name: "nested quasiquote keeps a depth-2 dotted unquote literal",
			src:  "(let ((y 3)) `(1 `(2 . ,y)))",
			want: "(1 (quasiquote (2 unquote y)))",
		},
		{
			// Same guard, one level down: the doubly-unquoted expression IS at the
			// outermost level and must be evaluated, while the singly-unquoted one is
			// not. This is R7RS §4.2.8's own worked example.
			name: "R7RS 4.2.8 canonical nesting example",
			src:  "(let ((c (lambda a a)) (d 1) (e 2)) `(a `(b ,(c ,(+ 1 2) d) e) f))",
			want: "(a (quasiquote (b (unquote (c 3 d)) e)) f)",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				_ = eng.Close()
			}()

			v, err := eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

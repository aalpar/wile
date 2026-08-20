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

package wile

import (
	"context"
	"strings"
	"testing"
)

// A merged `let`'s slots (merged_slots.go) live in the enclosing lambda's
// PARAMETER frame, which outlives the `let`. So any scheme that recycles a closed
// `let`'s slots — for a sibling `let`, or for anything else — has to answer what
// can still READ them afterwards.
//
// These are the two readers that can, and both were measured to break under
// sibling reuse before it was reverted:
//
//   - a closure stamped RetainsLexicalEnv (an opaque subtree in its body) does NOT
//     copy its free variables into a free vector; it reads the slot by depth when
//     it is CALLED, which can be long after the `let` closed. The first case gave
//     (2) instead of (1).
//   - a continuation captured inside the `let` and RE-ENTERED after the recycling
//     sibling ran resumes a body reading an overwritten slot. The second case gave
//     (1 2 2 2) instead of (1 2 1 2). This is schelog's backtracking shape.
//
// Kept after the revert because they are the only thing in the tree that sees it:
// the unsound version passed all of pkg/machine and pkg/wile. That is this arc's
// standing failure mode ("every change fails toward deopt, and a total deopt
// passes the entire value-assertion suite silently") in its dangerous direction.
// Detail and the measurement in
// plans/2026-08-19-let-slot-merging-impl.local.md §10.
var mergedLetSlotReuseCases = []struct {
	name string
	code string
	want string
}{
	{
		// A closure whose body holds an opaque subtree (here a quasiquote) is
		// stamped RetainsLexicalEnv: its free variables are NOT copied into a free
		// vector, so it reads `a` by depth through the frame chain when it is
		// CALLED — after the sibling `let` has run. Reusing the slot makes it
		// answer (2).
		name: "retaining closure outlives its let",
		code: `
(define (f)
  (let ((g #f))
    (let ((a 1))
      (set! g (lambda () ` + "`" + `(,a))))
    (let ((b 2))
      b)
    (g)))
(f)`,
		want: "(1)",
	},
	{
		// A continuation captured inside the first `let` and re-entered after the
		// sibling ran resumes a body that reads `a`. Reusing the slot makes the
		// second trip print 2 for it.
		name: "re-entered continuation reads its own let slot",
		code: `
(define (h)
  (let ((k #f) (n 0) (out '()))
    (let ((a 1))
      (call-with-current-continuation (lambda (c) (set! k c)))
      (set! out (cons a out)))
    (let ((b 2))
      (set! out (cons b out)))
    (set! n (+ n 1))
    (if (= n 1) (k 'again))
    (reverse out)))
(h)`,
		want: "(1 2 1 2)",
	},
}

func TestMergedLetSlotReuseIsNotObservable(t *testing.T) {
	ctx := context.Background()
	for _, tc := range mergedLetSlotReuseCases {
		t.Run(strings.ReplaceAll(tc.name, " ", "_"), func(t *testing.T) {
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatalf("new engine: %v", err)
			}
			got, err := engine.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			if got.SchemeString() != tc.want {
				t.Errorf("= %s, want %s — a sibling `let` reused a slot whose "+
					"reader was still live; see reserveAnonSlots",
					got.SchemeString(), tc.want)
			}
		})
	}
}

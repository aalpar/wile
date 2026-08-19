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
	"testing"

	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"

	qt "github.com/frankban/quicktest"
)

// boxedFreeVars returns the spellings of the boxed free-vector slots of the
// innermost lambda in code, in slot order.
func boxedFreeVars(t *testing.T, code string) []string {
	t.Helper()
	inner := deepestTemplate(t, compileToTemplate(t, code))
	boxed := inner.FreeBoxed()
	if boxed == nil {
		return nil
	}
	names := inner.FreeNames()
	var q []string
	for i, b := range boxed {
		if b {
			q = append(q, names[i].Key)
		}
	}
	return q
}

// TestBoxingSet pins Pass 2's predicate: a free variable is boxed iff it is
// CAPTURED (a member of some nested lambda's free set) AND ASSIGNED (a
// RefSetBangTarget anywhere in its scope, at ANY depth).
//
// Every case is stated from the innermost lambda's point of view, because that
// is where the layout being asserted lives.
func TestBoxingSet(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want []string
	}{
		{
			// The overwhelming majority. Captured, never assigned: copy by
			// value. Boxing this is the regression phase 2 exists to prevent.
			name: "captured but never assigned is NOT boxed",
			code: `(lambda (a) (lambda (b) (cons a b)))`,
			want: nil,
		},
		{
			// `a` is assigned, but the assignment and the reference are in the
			// SAME activation — nothing copies it, so there is nothing to share.
			// It is not even a free variable of the inner lambda.
			name: "assigned but never captured is NOT boxed",
			code: `(lambda (a) (set! a 1) (lambda (b) b))`,
			want: nil,
		},
		{
			name: "captured and assigned across the boundary is boxed",
			code: `(lambda (a) (lambda (b) (set! a b)))`,
			want: []string{"a"},
		},
		{
			// Reported at depth 0 by WalkBindingRefs, because an
			// immediately-applied lambda is not an ESCAPING closure — so a
			// depth-qualified predicate did not box it, and
			// (let ((a 0)) ((lambda (x) (set! a x)) 7) a) evaluated to 0.
			name: "set! through an immediately-applied lambda is boxed",
			code: `(lambda (a) ((lambda (x) (set! a x)) 7) a)`,
			want: []string{"a"},
		},
		{
			// The second, independent defect in the depth-qualified form: this
			// set! is at depth 0 for the plainer reason that it is inside no
			// lambda at all. The closure captured `a` by value, and a later
			// set! in the BINDER's own body is just as invisible to it.
			name: "set! in the binder's own body after a nested capture is boxed",
			code: `(lambda (a) (lambda (b) (cons a b)) (set! a 1))`,
			want: []string{"a"},
		},
		{
			// Only the assigned member of a multi-slot layout is boxed.
			name: "an unassigned sibling slot stays unboxed",
			code: `(lambda (a b) (lambda (c) (set! a c) (cons b c)))`,
			want: []string{"a"},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got := boxedFreeVars(t, tc.code)
			c.Assert(got, qt.DeepEquals, tc.want)
		})
	}
}

// validatedLetOf compiles code far enough to hand back the ValidatedLet it
// consists of, so the letrec tiers can be asserted on the real validated tree.
func validatedLetOf(t *testing.T, code string) *validate.ValidatedLet {
	t.Helper()
	env := newFreeVarEnv()
	prog := parseSchemeExpr(t, env, code)
	ctx := context.Background()
	expanded, err := NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator()).
		ExpandTopLevelExpression(prog)
	qt.Assert(t, err, qt.IsNil)
	res := validate.ValidateExpression(ctx, env, expanded)
	qt.Assert(t, res.Ok(), qt.IsTrue, qt.Commentf("%v", res.Errors))
	v, ok := res.Expr.(*validate.ValidatedLet)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("fixture is a %T, not a ValidatedLet", res.Expr))
	return v
}

// TestLetrecTiers pins the three-tier carve-out.
//
// THE T2 CASE MUST FAIL IF T2 IS BOXED. Over-boxing is CORRECT — it evaluates to
// the same values — so no value assertion catches a T2 that silently regressed
// to T1. This is the arc's fail-toward-conservative trap at binding granularity.
func TestLetrecTiers(t *testing.T) {
	tcs := []struct {
		name  string
		code  string
		index int
		want  letrecTier
	}{
		{
			// `b` is captured by sibling `a`'s init, and does not reciprocate.
			name:  "T1: captured by a sibling init",
			code:  `(letrec ((a (lambda () (b))) (b (lambda () 1))) a)`,
			index: 1,
			want:  tierBoxed,
		},
		{
			// `a` is read while `b`'s init is being EVALUATED, not from inside a
			// closure. Its value must exist by then, so the group needs a cell.
			name:  "T1: referenced during init evaluation, not only from a closure",
			code:  `(letrec ((a 1) (b (+ a 1))) b)`,
			index: 0,
			want:  tierBoxed,
		},
		{
			// unify1 in examples/logic/schelog/schelog.scm is exactly this shape,
			// and it is the inner loop of unification. Boxing it puts an unbox on
			// every recursive call, for a binding whose value exists the instant
			// the closure is constructed.
			name:  "T2: lambda references only its own binding",
			code:  `(letrec ((f (lambda (n) (if (= n 0) 1 (f (- n 1)))))) (f 5))`,
			index: 0,
			want:  tierSelfPatch,
		},
		{
			name: "T3: mutual recursion is boxed, for now",
			code: `(letrec ((ev (lambda (n) (if (= n 0) 1 (od (- n 1)))))
			               (od (lambda (n) (if (= n 0) 0 (ev (- n 1))))))
			         (ev 4))`,
			index: 0,
			want:  tierMutual,
		},
		{
			// A non-lambda init cannot be back-patched: the value is whatever the
			// init computes, and there is no closure to overwrite a slot in.
			name:  "a self-referencing non-lambda init is boxed",
			code:  `(letrec ((a (cons 1 a))) a)`,
			index: 0,
			want:  tierBoxed,
		},
		{
			// No forward reference at all. Still T1 by default: T2 is a positive
			// claim about a self-reference, not the absence of one.
			name:  "no self reference is boxed by default",
			code:  `(letrec ((a (lambda (n) n))) (a 1))`,
			index: 0,
			want:  tierBoxed,
		},
		{
			// The named let's loop lambda. Same shape as T2's letrec, reached
			// through the named-let desugaring, which is the shape that actually
			// occurs in the corpus.
			name:  "T2: a named let's loop lambda",
			code:  `(let loop ((i 0)) (if (= i 3) i (loop (+ i 1))))`,
			index: 0,
			want:  tierSelfPatch,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := validatedLetOf(t, tc.code)
			got := letrecBindingTier(v, tc.index)
			c.Assert(got, qt.Equals, tc.want,
				qt.Commentf("binding %q classified %v, want %v",
					v.Bindings[tc.index].Name.Sym.Key, got, tc.want))
		})
	}
}

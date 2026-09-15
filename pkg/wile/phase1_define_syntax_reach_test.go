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

// A define-syntax at phase > 0 must be reachable from its own rung.
//
// RED on master (measured 2026-09-13 on e907a44f by reverting the two one-line
// expander-root changes in place): every row raises "no such local or global
// binding \"m\" at phase N of this unit's macro tower". The variable analogue
// passed at every one of these coordinates, which is what made the gap
// keyword-specific rather than phase hermeticity doing its job.
//
// Root cause: executeFormsAtCompileTime and CompileDefineForSyntax compiled and
// evaluated the body against p.env.NextPhase() but rooted the EXPANDER at p.env.
// The body is phase-(N+1) code, so a define-syntax in it deposits at N+2
// (CompileDefineSyntax climbs from its own p.env, which is that N+1 frame),
// while the p.env-rooted expander's NextPhase arm read N+1 — one rung low.
// compileTransformerValue had it right all along: it roots the expander at the
// same frame it compiles against.
//
// Each row reveals a phase-1 value at phase 0 through an er-macro-transformer,
// the technique TestBindingModelMatrix uses, because a keyword cannot carry its
// expansion down a rung by itself.

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func TestPhase1DefineSyntaxIsReachableFromItsOwnRung(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want string
	}{
		{
			// The filed row: deposit and use in two SEPARATE phase-1 bodies.
			name: "syntax-rules keyword used in a later begin-for-syntax body",
			src: `(begin-for-syntax (define-syntax m (syntax-rules () ((_) 7))))
			      (begin-for-syntax (define probe (m)))
			      (define-syntax reveal (er-macro-transformer (lambda (form rename compare) probe)))
			      (reveal)`,
			want: "7",
		},
		{
			// Same rung, same body: rules out "a later body gets a fresh frame".
			name: "syntax-rules keyword used in the same begin-for-syntax body",
			src: `(begin-for-syntax (define-syntax m (syntax-rules () ((_) 7))) (define probe (m)))
			      (define-syntax reveal (er-macro-transformer (lambda (form rename compare) probe)))
			      (reveal)`,
			want: "7",
		},
		{
			// The transformer kind is not the discriminator; both were unreachable.
			name: "er-macro-transformer keyword at phase 1",
			src: `(begin-for-syntax (define-syntax m (er-macro-transformer (lambda (form rename compare) 7))))
			      (begin-for-syntax (define probe (m)))
			      (define-syntax reveal (er-macro-transformer (lambda (form rename compare) probe)))
			      (reveal)`,
			want: "7",
		},
		{
			// define-for-syntax is the second site with the same asymmetry: its
			// value expression is phase-1 code too, so it must see phase-1
			// keywords. This row was still RED after the begin-for-syntax half
			// alone, which is why both sites changed.
			name: "define-for-syntax value expression uses a phase-1 keyword",
			src: `(begin-for-syntax (define-syntax m (syntax-rules () ((_) 7))))
			      (define-for-syntax probe (m))
			      (define-syntax reveal (er-macro-transformer (lambda (form rename compare) probe)))
			      (reveal)`,
			want: "7",
		},
		{
			// The climb is relative, not a hard-wired rung: one level up, the
			// deposit lands at 3 and the use reads 3.
			//
			// This row cannot reveal through a phase-0 transformer — the value
			// lives at phase 2, and reading it from phase 1 is the hermeticity
			// this fix does NOT relax. So the phase-2 code checks itself and
			// raises on a wrong answer; the discriminator is that it compiles at
			// all, which on master it does not.
			name: "nested begin-for-syntax reaches a phase-2 keyword",
			src: `(begin-for-syntax (begin-for-syntax (define-syntax m (syntax-rules () ((_) 7)))))
			      (begin-for-syntax (begin-for-syntax (if (= (m) 7) 'ok (error "phase-2 expansion produced the wrong value"))))
			      99`,
			want: "99",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := phaseDistinctnessEngine(t)
			v, err := eng.EvalMultiple(context.Background(), tt.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tt.want)
		})
	}
}

// A phase-1 keyword stays INVISIBLE at phase 0. The fix lifts the deposit to the
// rung its own code reads, it does not flatten the tower: (m) at phase 0 must
// still be unbound, or begin-for-syntax would have stopped separating phases.
// GUARD — passes on both sides of the fix.
func TestPhase1DefineSyntaxIsNotVisibleAtPhaseZero(t *testing.T) {
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(wile.StdLibFS),
		wile.WithLibraryPaths("lib"),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(context.Background(),
		`(begin-for-syntax (define-syntax m (syntax-rules () ((_) 7))))
		 (m)`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, `no such local or global binding "m"`)
}

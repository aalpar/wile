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
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// A pattern literal that names a USER MACRO matches it. I169.
//
// A define-syntax binds at NextPhase(), so the identifier is not at the use
// site's own phase at all — it is one above — and the use-side resolution found
// nothing. R7RS §4.3.2's "both occurrences refer to the same binding" arm is
// satisfied, so the clause must match; instead it silently lost to the next
// one. Silent wrong clause selection, hence a conformance fix rather than a
// diagnostic one.
//
// ORACLES, measured rather than inherited: in R6RS form petite 10.4.1 answers
// lit and plain `#lang racket/base` answers lit, where Wile answered other.
// Both oracles also answer other for the let-bound-shadow control below, so
// they agree on the control as well as on the defect.
func patternLiteralMacroEngine(t *testing.T, libs map[string]string) *wile.Engine {
	t.Helper()
	files := fstest.MapFS{}
	for name, body := range libs {
		files[name] = &fstest.MapFile{Data: []byte(body)}
	}
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(files),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func TestPatternLiteralNamingAUserMacroMatches(t *testing.T) {
	const libMacro = `(define-library (i169lib)
  (export topelse)
  (import (scheme base))
  (begin
    (define-syntax topelse (syntax-rules () ((_) 'x)))))
`

	for _, row := range []struct {
		name string
		libs map[string]string
		src  string
		want string
	}{
		{
			name: "a locally defined macro named as a pattern literal matches",
			src: `(import (scheme base))
(define-syntax topelse (syntax-rules () ((_) 'x)))
(define-syntax m2 (syntax-rules (topelse) ((_ topelse) 'lit) ((_ y) 'other)))
(m2 topelse)`,
			want: "lit",
		},
		{
			name: "an IMPORTED macro named as a pattern literal matches",
			libs: map[string]string{"i169lib.scm": libMacro},
			src: `(import (scheme base) (i169lib))
(define-syntax m2 (syntax-rules (topelse) ((_ topelse) 'lit) ((_ y) 'other)))
(m2 topelse)`,
			want: "lit",
		},

		// CONTROLS. The first is the one that matters: a use-site shadow must
		// still beat the macro, or the climb has started outranking the lexical
		// chain. petite answers other here too.
		{
			name: "a let-bound shadow still beats the macro",
			src: `(import (scheme base))
(define-syntax topelse (syntax-rules () ((_) 'x)))
(define-syntax m4 (syntax-rules (topelse) ((_ topelse) 'lit) ((_ y) 'other)))
(let ((topelse 4)) (m4 topelse))`,
			want: "other",
		},
		{
			// `latemac`, which the plan offers as a control and which CANNOT
			// discriminate this fix: instrumented, it takes the UNPINNED arm, so
			// it never reaches the climb and answers lit under any version of
			// the change. Kept as a plain regression row, not as evidence of
			// narrowness.
			name: "a macro defined after the syntax-rules that lists it",
			src: `(import (scheme base))
(define-syntax m3 (syntax-rules (mac) ((_ mac) 'lit) ((_ y) 'other)))
(define-syntax mac (syntax-rules () ((_) 'x)))
(m3 mac)`,
			want: "lit",
		},
		{
			// A user (define-syntax else ...) shadowing the dialect's auxiliary
			// keyword is the row that forced the climb to run AFTER the
			// language's bulk rows rather than as an entry in the descent's
			// fallbacks loop. Folding it in changes this answer.
			name: "a user define-syntax over an auxiliary keyword",
			src: `(import (scheme base))
(define-syntax else (syntax-rules () ((_) 'user-else)))
(define-syntax m5 (syntax-rules (else) ((_ else) 'lit) ((_ y) 'other)))
(m5 else)`,
			want: "lit",
		},
	} {
		t.Run(row.name, func(t *testing.T) {
			eng := patternLiteralMacroEngine(t, row.libs)
			v, err := eng.EvalMultiple(context.Background(), row.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, row.want)
		})
	}
}

// The RESIDUAL this task narrows rather than closes, measured in BOTH shapes
// because only one of them is the residual and the difference is not obvious.
//
// TODO.md's row is the nested let-syntax over `else` SPECIFICALLY, and `else` is
// the point: the dialect supplies auxiliary syntax at every macro phase through
// its bulk rows, and that read answers before the let-syntax-bound `else` is
// reached. So the inner macro's literal pin misses the enclosing binding and the
// clause loses — `other`, where petite answers `lit`. The climb does not help,
// because the binding it needs is lexical, not one phase up.
//
// The same nesting over a PLAIN name already answered `lit` on master, so it is
// not the residual and must not be cited as one: a measurement taken on that
// shape reports the row closed when it is not.
func TestNestedLetSyntaxPatternLiteralIsStillRefused(t *testing.T) {
	for _, row := range []struct {
		name string
		src  string
		want string
	}{
		{
			name: "RESIDUAL: nested let-syntax over the auxiliary keyword else",
			src: `(import (scheme base))
(let-syntax ((else (syntax-rules () ((_) 'x))))
  (let-syntax ((m (syntax-rules (else) ((_ else) 'lit) ((_ y) 'other))))
    (m else)))`,
			want: "other",
		},
		{
			name: "the same nesting over a plain name was never broken",
			src: `(import (scheme base))
(let-syntax ((inner (syntax-rules () ((_) 'x))))
  (let-syntax ((m (syntax-rules (inner) ((_ inner) 'lit) ((_ y) 'other))))
    (m inner)))`,
			want: "lit",
		},
	} {
		t.Run(row.name, func(t *testing.T) {
			eng := patternLiteralMacroEngine(t, nil)
			v, err := eng.EvalMultiple(context.Background(), row.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, row.want,
				qt.Commentf("petite answers lit for both; only the first is the open residual"))
		})
	}
}

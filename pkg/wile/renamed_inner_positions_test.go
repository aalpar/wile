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

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// A renamed identifier keeps its meaning in INNER positions too: as a marker
// inside a quasiquote template, and as a macro's pattern literal. R7RS §4.3.2
// states the literal rule in terms of bindings, not spellings. Measured
// 2026-09-15: Racket 9.2 answers 2 and (1 2) for the two shapes below; Petite
// Chez 10.4.1 answers 2 for the literal shape.

type innerPositionRow struct {
	name string
	src  string
	want string
}

func runInnerPositionRows(t *testing.T, rows []innerPositionRow) {
	t.Helper()
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			eng, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
			)
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			v, err := eng.EvalMultiple(context.Background(), row.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, row.want)
		})
	}
}

// Task 2: quasiquote markers.
func TestRenamedQuasiquoteMarkers(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		{"renamed unquote",
			`(import (scheme base) (rename (scheme base) (unquote uq))) (quasiquote (1 (uq (+ 1 1))))`, "(1 2)"},
		{"renamed unquote-splicing",
			`(import (scheme base) (rename (scheme base) (unquote-splicing uqs))) (quasiquote (1 (uqs (list 2 3)) 4))`, "(1 2 3 4)"},
		{"prefixed unquote",
			`(import (prefix (scheme base) b:)) (b:quasiquote (1 (b:unquote (b:+ 1 1))))`, "(1 2)"},
		// The nested marker is DATA, and it renders with the spelling the user
		// wrote: dispatch reads the denotation, rewrapQuasiForm rebuilds with the
		// original symbol. This is the design's choice, not an oracle claim; the
		// control row below shows the same shape with canonical names (measured:
		// `(1 `(2 ,(+ 1 1))) renders as (1 (quasiquote (2 (unquote (+ 1 1)))))).
		{"renamed nested quasiquote keeps the user's spelling as data",
			`(import (scheme base) (rename (scheme base) (quasiquote qq) (unquote uq))) (qq (1 (qq (2 (uq (+ 1 1))))))`,
			"(1 (qq (2 (uq (+ 1 1)))))"},
		{"control: nested markers unrenamed",
			`(import (scheme base)) (quasiquote (1 (quasiquote (2 (unquote (+ 1 1))))))`,
			"(1 (quasiquote (2 (unquote (+ 1 1)))))"},
		{"control: unrenamed markers",
			`(import (scheme base)) (quasiquote (1 (unquote (+ 1 1))))`, "(1 2)"},
		{"control: a local binding of the marker name is not a marker",
			`(import (scheme base)) (let ((uq list)) (quasiquote (1 (uq 2))))`, "(1 (uq 2))"},
	})
}

// Task 3: pattern literals.
func TestRenamedAuxiliaryKeywords(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		{"renamed else in cond",
			`(import (scheme base) (rename (scheme base) (else otherwise))) (cond (#f 1) (otherwise 2))`, "2"},
		{"renamed else in case",
			`(import (scheme base) (rename (scheme base) (else otherwise))) (case 9 ((1) 'a) (otherwise 'b))`, "b"},
		{"renamed => in cond",
			`(import (scheme base) (rename (scheme base) (=> arrow))) (cond (5 arrow (lambda (x) (+ x 1))))`, "6"},
		{"prefixed else",
			`(import (prefix (scheme base) b:)) (b:cond (#f 1) (b:else 2))`, "2"},
		{"control: unrenamed else",
			`(import (scheme base)) (cond (#f 1) (else 2))`, "2"},
		{"control: a local binding of else is not the literal",
			`(import (scheme base)) (let ((else #t)) (cond (else 3) (#t 4)))`, "3"},
		// (cond (test result)) and (cond (else result...)) both evaluate
		// `result` unconditionally when the test is truthy, so a plain
		// `(car 5)` clause answers 5 whichever reading is taken — it cannot
		// tell a correct refusal from a silent regression. `(test => proc)`
		// discriminates: the non-literal reading applies proc to the test
		// (5), but the else-literal reading puts `=>` in expression/variable
		// position — (begin => (lambda (x) 5)) — and errors. Measured
		// directly: (cond (#f 1) (else => (lambda (x) 5))) (forcing the
		// literal-else reading with the real `else`) raises `syntactic
		// keyword "=>" used as a variable`; reverting literalNotShadowed's
		// IsImported rider to unconditional accept (so `car` wrongly matches
		// `else` too) makes THIS row raise the identical error instead of
		// answering 5 — confirmed in a disposable worktree, not asserted.
		{"control: an unrelated keyword does not match the literal",
			`(import (scheme base) (rename (scheme base) (else otherwise))) (cond (#f 1) (car => (lambda (x) 5)))`, "5"},
	})
}

// Fix round 2, C1: a renamed marker must be visible to validate's opaque-subtree
// walk, not just to the compiler's.
//
// A quasiquote template is raw syntax the validator never looks inside, so
// pkg/internal/validate/opaque_subtree.go walks it separately and records every
// symbol in an EVALUATED position as a possible set! target and a possible
// capture. That walk recognized the markers by SPELLING while this branch moved
// the compiler's walk to the denotation, so an unquote reached through a rename
// or a prefix put its argument back at template depth for the walk and at depth
// 0 for the compiler: the set! inside it went unmarked, and the inliner and the
// boxing pass then miscompiled a program the compiler had already agreed to
// evaluate. quasiHeadDepth's own doc names this disagreement as the failure the
// file exists to prevent.
//
// Every row states the canonical-spelling answer as its want; that is the
// control, and each was measured on 079c62ab as (1 7) / (0 1) / a raised
// "cannot mutate immutable top-level binding" / (0).
func TestRenamedQuasiquoteMarkerIsVisibleToTheOpaqueScan(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		// The inliner: f is let-bound to a lambda and set! to another one from
		// inside the template. Unmarked, f is still Stable, so the call is
		// inlined with the stale body and (f) answers 7 after the set! ran.
		{"renamed unquote hides a set! of a let-bound lambda",
			`(import (scheme base) (rename (scheme base) (unquote uq)))
			 (let ((f (lambda () 7)) (n 0))
			   (quasiquote ((uq (begin (set! n 1) (set! f (lambda () 99))))))
			   (list n (f)))`, "(1 99)"},
		{"prefixed unquote hides a set! of a let-bound lambda",
			`(import (scheme base) (prefix (scheme base) b:))
			 (let ((f (lambda () 7)) (n 0))
			   (quasiquote ((b:unquote (begin (set! n 1) (set! f (lambda () 99))))))
			   (list n (f)))`, "(1 99)"},
		// The boxing pass: n is captured by f and set! from inside the template.
		// Unmarked, n is never boxed, so the closure keeps a copy of 0. The
		// vector-set! is the witness that the template's expression really ran.
		{"renamed unquote hides a set! of a captured binder",
			`(import (scheme base) (rename (scheme base) (unquote uq)))
			 (define v (vector 0))
			 (define g
			   (let ((n 0))
			     (let ((f (lambda () n)))
			       (quasiquote ((uq (begin (vector-set! v 0 1) (set! n 99)))))
			       f)))
			 (list (g) (vector-ref v 0))`, "(99 1)"},
		// Top-level immutability keys on the same Stable stamp, so the unmarked
		// direction is not only a wrong value: it REFUSES a legal program.
		//
		// (begin …)-wrapped because the stamp is per VALIDATION UNIT and
		// EvalMultiple's unit is one top-level expression: left unwrapped, the
		// define is stamped Stable before the let is even parsed and the set! is
		// refused whatever the marking says — the canonical spelling is refused
		// too, measured. The wrap is what the file loader does for the same
		// reason (EvalMultipleWithSource), and it is what the CLI run of this
		// program used.
		{"renamed unquote hides a set! of a top-level binding",
			`(import (scheme base) (rename (scheme base) (unquote uq)))
			 (begin
			   (define g 0)
			   (let ((f (lambda () 7)))
			     (quasiquote ((uq (begin (set! g 1) (set! f (lambda () 99))))))
			     (list g (f))))`, "(1 99)"},
		// The same disagreement reaches the or-shaped let lowering, which drops
		// the frame when the binder occurs nowhere in the alternative. The
		// occurrence scan is forEachRawSymbol's, so a renamed unquote hid the
		// only reference to the inner t and the alternative was compiled in the
		// enclosing frame, where t is the top-level 0.
		{"renamed unquote hides a reference in an or-shaped let's alternative",
			`(import (scheme base) (rename (scheme base) (unquote uq)))
			 (define t 0)
			 (let ((t #f)) (if t t (quasiquote ((uq t)))))`, "(#f)"},
	})
}

// Fix round 1, C1: literalNotShadowed's IsImported rider, when the
// definition-site literal denotes no form (an ordinary variable or user
// macro — every literal that is not an auxiliary keyword like else/=>), used
// to accept ANY imported binding regardless of spelling. Before match.go's
// literal arm could reach a differently-spelled use-site identifier, spelling
// was already guaranteed equal by the caller, so this was unreachable with a
// mismatch. Once it wasn't, an unrelated imported identifier (here: `car`,
// unrelated to the macro's own `state`/`on` literals) could match a literal
// purely by being imported, stealing the wrong clause silently.
//
// This is the shape of examples/macros/state-machine.scm:25 — a
// syntax-rules macro whose literals are ordinary-looking names (state, on)
// that a real program's `(scheme base)` imports (car, cdr, ...) can collide
// with. Measured on 80aeca39 (before this fix round): (go) answered
// ((STATE 1) (ON 2) (STATE 3)) — the third clause wrongly took the `state`
// arm because car's imported binding satisfied literalNotShadowed's
// denoted=="" fallback unconditionally.
func TestRenamedLiteralDoesNotOverAcceptUnrelatedImports(t *testing.T) {
	const lib = `(define-library (sm)
  (export go)
  (import (scheme base))
  (begin
    (define state 0)
    (define on 0)
    (define-syntax machine
      (syntax-rules (state on)
        ((_ state n) (list 'STATE n))
        ((_ on n)    (list 'ON n))
        ((_ other n) (list 'OTHER n))))
    (define (go) (list (machine state 1) (machine on 2) (machine car 3)))))
`
	eng := w16LibEngine(t, map[string]string{"sm.scm": lib})
	got := evalString(t, eng, "(import (sm))\n(go)")
	want := "((STATE 1) (ON 2) (OTHER 3))"
	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}

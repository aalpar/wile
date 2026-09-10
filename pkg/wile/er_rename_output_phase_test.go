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
	"testing"

	qt "github.com/frankban/quicktest"
)

// erRenamePhase1Prologue binds `car` under a second spelling at phase 1 ONLY.
// Nothing binds p1-only-car at phase 0, and a plain phase-0 reference to it is
// a compile error — the first row below pins that, so the third row's error is
// read as the phase wall holding rather than as a typo.
const erRenamePhase1Prologue = "(import (for-syntax (rename (scheme base) (car p1-only-car))))\n"

// TestERRenameDenotesTheOutputPhase pins what (rename 'x) denotes inside an ER
// macro whose OUTPUT is phase-0 code: x's binding at the phase the output lands
// in, which is the phase of the `define-syntax`, NOT the phase the transformer
// BODY runs at.
//
// The contract is Clinger 1991's — rename "maps an identifier into the macro's
// definition environment" (BIBLIOGRAPHY.md) — read against a phase tower Clinger
// did not have, and design §3.5 fixes the reading: rename is
// `(lambda (sym) (datum->syntax #'k sym))`, where `k` is the
// `er-macro-transformer` keyword AS WRITTEN at the user's `define-syntax`. That
// identifier sits in phase-0 program text, so the renamed identifier is a
// phase-0 identifier. Measured in Racket, which has the same explicit phasing:
// a template identifier resolves at phase 0 while the transformer body's
// reference to the same name resolves at phase 1 (one name, two modules, two
// answers), and a for-syntax-only import leaves a template identifier UNBOUND.
// Chez answers the other way and is not the oracle here: it implements R6RS
// implicit phasing and collapses the tower — measured, `(for (a) expand)` alone
// satisfies a phase-0 template reference in petite.
//
// Wile resolved the rename in the frame the TRANSFORMER compiles in, one phase
// above the definition site. The LAST TWO rows are RED with that reading; the
// first is a control, green on both sides, and is here so the second row's error
// reads as the phase wall holding rather than as a misspelt name.
//
// The defect is not import-specific, and the import is only the cheapest trigger:
// a bare `(define list 5)` plus an ER macro renaming `list` splits the same way,
// with no import anywhere. The import rows are kept because they are what was
// filed and what the integration fixture hits.
func TestERRenameDenotesTheOutputPhase(t *testing.T) {
	// A phase-1-only name is not reachable from phase 0. The control: without
	// it the next row proves nothing.
	t.Run("a phase-1-only import is unbound at phase 0", func(t *testing.T) {
		c := qt.New(t)
		err := evalSyntaxFormsErr(t, erRenamePhase1Prologue+`(p1-only-car (list 1 2))`,
			withStdlibResolver()...)
		c.Assert(err, qt.ErrorMatches, `(?s).*no such .*binding "p1-only-car".*`)
	})

	// The leak, and the sharper half of the defect: rename is not a hole in the
	// phase wall. An ER macro cannot smuggle a name it can only see at phase 1
	// into the phase-0 code it emits — Racket answers "unbound identifier" for
	// the analogous template.
	t.Run("rename does not smuggle a phase-1 binding into phase-0 output", func(t *testing.T) {
		c := qt.New(t)
		err := evalSyntaxFormsErr(t, erRenamePhase1Prologue+`(define-syntax pick
  (er-macro-transformer
    (lambda (form rename compare)
      (list (rename 'p1-only-car) (list (rename 'quote) (list 1 2))))))
(pick)`, withStdlibResolver()...)
		// Matched on the binding message, not merely on the name: a bare
		// `.*p1-only-car.*` would also be satisfied by an arity or reader failure.
		c.Assert(err, qt.ErrorMatches, `(?s).*no such .*binding "p1-only-car".*`)
	})

	// The reported symptom. `else` is an auxiliary keyword the phase-1 import
	// re-slots at (ExactPhase(1), mutable); the use site's `else` is the phase-0
	// keyword. Resolving the rename at the OUTPUT phase makes both sides read
	// the same slot, so the import stops being observable here.
	//
	// Asserts equality against the un-imported arm, not a constant, so it cannot
	// be satisfied by making both arms equally wrong.
	t.Run("compare against a rename is unmoved by a phase-1 import", func(t *testing.T) {
		c := qt.New(t)
		body := `(define-syntax probe
  (er-macro-transformer
    (lambda (form rename compare)
      (list (rename 'quote)
            (if (compare (car (cdr form)) (rename 'else)) 'same 'diff)))))
(probe else)`
		bare := evalSyntaxForms(t, body, withStdlibResolver()...)
		c.Assert(bare, qt.Equals, "same")
		imported := evalSyntaxForms(t, "(import (for-syntax (scheme base)))\n"+body,
			withStdlibResolver()...)
		c.Assert(imported, qt.Equals, bare)
	})
}

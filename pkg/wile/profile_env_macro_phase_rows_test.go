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

// What each environment constructor sees above phase 0.
//
// A (wile <profile>) environment is built by pkg/internal/bootstrap, not by the
// engine, and the language rows (the base at phase 0, the macro vocabulary at
// every macro phase) were installed only on the engine path. Phase 1 still
// worked there, because the registry writes its expand-phase primitives as
// exact phase-1 slots, so the gap showed only at phase 2: a transformer that
// defines a local macro, whose own body calls car, raised
//
//	no such binding "list" with compatible scopes at phase 2 of this unit's macro tower
//
// under (environment '(wile small)) while the interaction environment returned 5.
//
// An (environment <import-set> ...) stays empty above phase 0, as R7RS 6.12 and
// Racket's namespaces have it: a transformer reaches car there only through
// (for-syntax ...).

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// phaseTwoTransformerProgram evaluates, under envExpr, a transformer whose body
// uses a let-syntax macro, so that inner transformer's body (car, cdr, list)
// runs at phase 2. All three names are in the macro vocabulary.
//
// Two evals rather than one begin: a define-syntax used in the same eval'd begin
// is not visible under an import-set environment, a separate open defect.
func phaseTwoTransformerProgram(envExpr string) string {
	return `(let ((env ` + envExpr + `))
  (eval '(define-syntax second-of
           (er-macro-transformer
             (lambda (form rename compare)
               (let-syntax ((second
                              (er-macro-transformer
                                (lambda (f r c) (list 'car (list 'cdr (car (cdr f))))))))
                 (second form)))))
        env)
  (eval '(second-of 5 6) env))`
}

func newPhaseRowsEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(wile.StdLibFS),
		wile.WithLibraryPaths("lib"),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func TestProfileEnvironmentHasTheMacroVocabularyAtEveryPhase(t *testing.T) {
	rows := []struct {
		name    string
		envExpr string
	}{
		// The control: the engine path installs the rows.
		{name: "interaction environment", envExpr: "(interaction-environment)"},
		{name: "small profile", envExpr: "(environment '(wile small))"},
		{name: "kitchen-sink profile", envExpr: "(environment '(wile kitchen-sink))"},
		// Narrowing withholds bindings from the visible top level, not the
		// vocabulary rows; the engine installs them under WithoutAmbientBindings too.
		{name: "small profile, core", envExpr: "(environment '(wile small core))"},
	}
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			eng := newPhaseRowsEngine(t)
			v, err := eng.EvalMultiple(context.Background(), phaseTwoTransformerProgram(row.envExpr))
			qt.Assert(t, err, qt.IsNil,
				qt.Commentf("a phase-2 transformer body must reach the macro vocabulary under %s", row.envExpr))
			qt.Assert(t, v.SchemeString(), qt.Equals, "5")
		})
	}
}

func TestImportSetEnvironmentIsEmptyAboveRuntime(t *testing.T) {
	eng := newPhaseRowsEngine(t)
	_, err := eng.EvalMultiple(context.Background(), phaseTwoTransformerProgram("(environment '(scheme base))"))
	qt.Assert(t, err, qt.ErrorMatches, `(?s).*no such .*binding "list".*`)

	v, err := eng.EvalMultiple(context.Background(),
		phaseTwoTransformerProgram("(environment '(scheme base) '(for-syntax (scheme base)))"))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "5")
}

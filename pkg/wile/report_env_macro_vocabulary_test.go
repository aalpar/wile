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

// The user-visible face of GlobalEnvironmentFrame.Copy dropping what it claims
// to carry: a report environment loses the phase-1 macro vocabulary entirely.
//
// Plan: plans/2026-09-10-flatt-binding-model-b-impl.local.md, Task 2.
//
// RED on master, measured: the report-env row fails with
//
//	no such binding "not" with compatible scopes at phase 1 of this unit's macro tower
//
// while the interaction-environment control returns 1. The two differ by the
// environment argument alone, which is what makes the failure attributable to
// the copy rather than to er-macro-transformer, to eval, or to the vocabulary
// row itself.
//
// Three defects in Copy converge here and any one of them is sufficient:
// the vocabulary row is wrapped in a filteredBulkSource, which Copy's
// *storeBulkSource type assertion skipped, so the row stayed pointed at the
// PARENT store and materializeBulkLocked (which requires store == p) made it
// INERT; the sealed source's minTier was reconstructed rather than carried; and
// the macro-phase templates were dropped, so a phase the report env reaches for
// the first time gets no row at all. The store-level
// pins for each live in pkg/environment/bulk_row_copy_test.go.

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// reportEnvTransformerProgram is a procedural transformer whose body calls a
// MACRO-VOCABULARY name, evaluated under the given environment expression.
//
// not, not cadr. The vocabulary is a strict subset of the base (dialect.go's
// defaultMacroVocabulary) and cadr is deliberately outside it — a transformer
// body reaches cadr only through (import (for-syntax (scheme base))), which is
// D4's break and not what this pins. not is inside it, so it must resolve at
// phase 1 with no import in EVERY environment the vocabulary row reaches.
func reportEnvTransformerProgram(envExpr string) string {
	return `(eval '(begin
  (define-syntax pick
    (er-macro-transformer
      (lambda (form rename compare)
        (if (not #f) 1 2))))
  (pick)) ` + envExpr + `)`
}

func TestReportEnvironmentKeepsTheMacroVocabulary(t *testing.T) {
	rows := []struct {
		name    string
		envExpr string
	}{
		// The control. Green on both sides: it is what says the program itself
		// compiles and that (not #f) is reachable at phase 1 to begin with.
		{name: "interaction environment", envExpr: "(interaction-environment)"},
		// The pin. Its store is GlobalEnvironmentFrame.Copy's output.
		{name: "report environment", envExpr: "(scheme-report-environment 7)"},
	}
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(wile.StdLibFS),
				wile.WithLibraryPaths("lib"),
			)
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})

			v, err := eng.EvalMultiple(ctx, reportEnvTransformerProgram(row.envExpr))
			qt.Assert(t, err, qt.IsNil,
				qt.Commentf("a transformer body must reach the macro vocabulary under %s", row.envExpr))
			qt.Assert(t, v.SchemeString(), qt.Equals, "1")
		})
	}
}

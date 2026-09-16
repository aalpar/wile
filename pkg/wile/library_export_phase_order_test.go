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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// findLibraryBinding (machine/compilation/library_bindings.go) answers a plain
// export from phase 0, or from phase 1 for a keyword, since Wile stores a keyword
// one phase above the code that uses it. PHASE 0 WINS for any name bound at both.
//
// For a keyword bound only at phase 1 that is harmless and the export works. For
// a name bound at both — syntax-rules holds a phase-0 SyntaxCompiler AND a phase-1
// PrimitiveExpander — the export takes the phase-0 binding, which is not the one a
// transformer right-hand side needs, and the imported name is unusable there.
// Validation does not catch it: the phase-0 binding is a legitimate plain export.
//
// These are ANSWER pins of current behavior, recorded because the failure is
// silent. A plain export binds at phase 0 only, as Racket's does; the phase-1 row
// is exported by (for-syntax ...), and (scheme base) does not declare one. The
// let-syntax row must keep passing either way.

func exportProbeEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fstest.MapFS{
			// Each library exports one expand-phase name under a fresh spelling, so
			// the use site cannot accidentally resolve the original.
			"lib-letsyntax.scm": &fstest.MapFile{Data: []byte(
				"(define-library (lib-letsyntax)\n" +
					"  (export (rename let-syntax my-let-syntax))\n" +
					"  (begin (define anchor 1)))\n")},
			"lib-syntaxrules.scm": &fstest.MapFile{Data: []byte(
				"(define-library (lib-syntaxrules)\n" +
					"  (export (rename syntax-rules my-sr))\n" +
					"  (begin (define anchor 1)))\n")},
		}),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestPresentPhasesIsAscendingFromRuntime pins the ordering the export lookup
// inherits: findLibraryBinding binary-searches PresentPhases to avoid creating a
// phase frame, and the export diagnostic reports the lowest phase a name is bound
// at. If this ever reverses, the search misses phases that are present.
func TestPresentPhasesIsAscendingFromRuntime(t *testing.T) {
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	phases := eng.Environment().PresentPhases()
	qt.Assert(t, len(phases) >= 2, qt.IsTrue,
		qt.Commentf("expected at least runtime and expand, got %v", phases))
	qt.Assert(t, phases[0], qt.Equals, environment.PhaseRuntime)
	qt.Assert(t, phases[1], qt.Equals, environment.PhaseExpand)
	for i := 1; i < len(phases); i++ {
		qt.Assert(t, phases[i] > phases[i-1], qt.IsTrue,
			qt.Commentf("PresentPhases must be strictly ascending, got %v", phases))
	}
}

// TestLibraryExportOfExpandPhaseNameValidates pins that export VALIDATION is not
// evidence the export is usable: both names pass validateLibraryExports, and only
// one of them survives the round trip below.
func TestLibraryExportOfExpandPhaseNameValidates(t *testing.T) {
	eng := exportProbeEngine(t)
	for _, lib := range []string{"(lib-letsyntax)", "(lib-syntaxrules)"} {
		t.Run(lib, func(t *testing.T) {
			_, err := eng.EvalMultiple(context.Background(), "(import "+lib+")")
			qt.Assert(t, err, qt.IsNil,
				qt.Commentf("%s must load and pass export validation", lib))
		})
	}
}

// TestLibraryExportTakesFirstPresentPhase is the round trip. let-syntax has no
// phase-0 binding, so the walk falls through to its phase-1 PrimitiveExpander and
// the renamed name expands. syntax-rules has one, so the walk stops at phase 0 and
// the renamed name is not a primitive expander at all — LookupPrimitiveExpander
// reads env.Expand() and finds nothing.
func TestLibraryExportTakesFirstPresentPhase(t *testing.T) {
	t.Run("let-syntax (phase-1 only) round-trips", func(t *testing.T) {
		eng := exportProbeEngine(t)
		// (car '()) raises, so anything but 42 fails loudly rather than rendering.
		_, err := eng.EvalMultiple(context.Background(), `(import (lib-letsyntax))
(if (= 42 (my-let-syntax ((m (syntax-rules () ((_) 42)))) (m))) 'ok (car '()))`)
		qt.Assert(t, err, qt.IsNil)
	})

	t.Run("syntax-rules (two phases) does not", func(t *testing.T) {
		eng := exportProbeEngine(t)
		_, err := eng.EvalMultiple(context.Background(), `(import (lib-syntaxrules))
(define-syntax two (my-sr () ((_) 2)))`)
		// Current behavior. When findLibraryBinding learns to prefer the phase the
		// importer needs, this becomes qt.IsNil and the assertion below goes away.
		qt.Assert(t, err, qt.IsNotNil,
			qt.Commentf("expected the phase-0 export to be unusable as a transformer"))
		qt.Assert(t, err.Error(), qt.Contains, "my-sr")
	})
}

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
	"errors"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// reexportHOFLibFS defines libraries over srfi/1 exercising the re-export gate:
//   - (myfold)  imports (srfi 1) and re-exports fold UNCHANGED — the real srfi-1
//     fold reaches an importer through one re-export hop;
//   - (foldish) imports (srfi 1) and re-exports its filter RENAMED as fold — a
//     DIFFERENT srfi-1 procedure wearing fold's surface name;
//   - (frr)     imports (srfi 1) and re-exports fold-right UNCHANGED — used to
//     rename a re-exported curated HOF onto ANOTHER curated HOF's name;
//   - (h1)/(h2) a two-hop re-export chain of fold: (h2) re-exports (h1)'s fold,
//     which re-exports (srfi 1)'s fold.
func reexportHOFLibFS() fstest.MapFS {
	return fstest.MapFS{
		"myfold.scm": &fstest.MapFile{Data: []byte(
			"(define-library (myfold) (export fold) (import (srfi 1)))\n")},
		"foldish.scm": &fstest.MapFile{Data: []byte(
			"(define-library (foldish) (export (rename filter fold)) (import (srfi 1)))\n")},
		"frr.scm": &fstest.MapFile{Data: []byte(
			"(define-library (frr) (export fold-right) (import (srfi 1)))\n")},
		"h1.scm": &fstest.MapFile{Data: []byte(
			"(define-library (h1) (export fold) (import (srfi 1)))\n")},
		"h2.scm": &fstest.MapFile{Data: []byte(
			"(define-library (h2) (export fold) (import (h1)))\n")},
		"customfold.scm": &fstest.MapFile{Data: []byte(
			"(define-library (customfold)\n" +
				"  (export fold)\n" +
				"  (import (scheme base))\n" +
				"  (begin (define (fold a b c) (list 'CUSTOM-FOLD))))\n")},
	}
}

func reexportHOFEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(reexportHOFLibFS()),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
		wile.WithImmutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestInlineHOFStamp_FoldViaReexport pins that the import-gated inline-HOF stamp
// follows the provenance ROOT, not the immediate import edge: the real srfi-1
// fold keeps its InlineHOF capability even when imported through a re-exporting
// library. The stamp gate keys on the binding's origin root ((srfi/1, fold)),
// which survives the re-export hop, rather than the re-exporting library's key.
func TestInlineHOFStamp_FoldViaReexport(t *testing.T) {
	eng := reexportHOFEngine(t)
	ctx := context.Background()

	_, err := eng.EvalMultiple(ctx, "(import (myfold))")
	qt.Assert(t, err, qt.IsNil)

	env := eng.Environment()
	b := env.GetBinding(values.NewSymbol("fold"), values.AllScopes())
	qt.Assert(t, b, qt.IsNotNil, qt.Commentf("fold must be bound after (import (myfold))"))
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, 0,
		qt.Commentf("fold re-exported from srfi/1 is still the curated HOF; its root (srfi/1, fold) "+
			"must carry the InlineHOF capability through the re-export hop"))

	// Value smoke: confirm the stamped binding really IS srfi-1 fold (a left fold),
	// so the stamp is on the right procedure, not merely present.
	got, err := eng.EvalMultiple(ctx, "(fold cons '() '(1 2 3))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.Internal().SchemeString(), qt.Equals, "(3 2 1)")
}

// TestInlineHOFStamp_TwoHopReexportChain pins that the stamp survives more than
// one re-export hop: (h2) re-exports (h1)'s fold, which re-exports srfi-1's fold.
// The root (srfi/1, fold) propagates verbatim at each hop.
func TestInlineHOFStamp_TwoHopReexportChain(t *testing.T) {
	eng := reexportHOFEngine(t)
	ctx := context.Background()

	_, err := eng.EvalMultiple(ctx, "(import (h2))")
	qt.Assert(t, err, qt.IsNil)

	env := eng.Environment()
	b := env.GetBinding(values.NewSymbol("fold"), values.AllScopes())
	qt.Assert(t, b, qt.IsNotNil, qt.Commentf("fold must be bound after (import (h2))"))
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, 0,
		qt.Commentf("fold's root (srfi/1, fold) survives two re-export hops and must stay stamped"))

	// Value smoke: confirm the twice-hopped binding is really srfi-1 LEFT fold.
	got, err := eng.EvalMultiple(ctx, "(fold cons '() '(1 2 3))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.Internal().SchemeString(), qt.Equals, "(3 2 1)")
}

// TestInlineHOFStamp_ReexportedFilterAsFoldNotStamped is the soundness guard for
// the root gate: keying on the root LIBRARY alone would mis-stamp any srfi-1
// procedure re-exported under the name fold. (foldish) re-exports srfi-1's filter
// as fold — same root library (srfi/1) but a different root NAME (filter) — so it
// must NOT be stamped, or (fold ...) would inline fold's template for filter.
func TestInlineHOFStamp_ReexportedFilterAsFoldNotStamped(t *testing.T) {
	eng := reexportHOFEngine(t)
	ctx := context.Background()

	_, err := eng.EvalMultiple(ctx, "(import (foldish))")
	qt.Assert(t, err, qt.IsNil)

	env := eng.Environment()
	b := env.GetBinding(values.NewSymbol("fold"), values.AllScopes())
	qt.Assert(t, b, qt.IsNotNil, qt.Commentf("fold is bound (foldish re-exports filter as fold)"))
	// Pin the precondition this test's discriminating power rests on: the binding's
	// root is (srfi/1, filter). A root-LIBRARY-only gate would wrongly stamp it
	// (RootLib == srfi/1); the RootName == filter is what refuses it. If srfi/1 were
	// refactored so filter's root diverged, this assertion fails loudly instead of
	// the test silently losing its teeth.
	qt.Assert(t, b.Origin(), qt.IsNotNil)
	qt.Assert(t, *b.Origin(), qt.Equals,
		environment.OriginRef{RootLib: "srfi/1", RootName: "filter"})
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, -1,
		qt.Commentf("this fold is really srfi-1 filter (root name filter, not fold); it must NOT "+
			"be stamped an inline HOF even though its root library is srfi/1"))
}

// TestInlineHOFRenamedOntoCuratedNameDispatchesCorrectly is the regression guard
// for the consumer fix: a curated HOF renamed onto ANOTHER curated HOF's surface
// name must run its OWN semantics, not the template the call-site name names. The
// inline dispatch keys on the binding's stamped identity, not sym.Sym.Key. Covers
// both the direct-import form (pre-existing bug) and the re-export form (widened
// by the origin gate). A wrong template shows as a wrong result.
func TestInlineHOFRenamedOntoCuratedNameDispatchesCorrectly(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "direct: fold-right imported as fold folds RIGHT",
			code: `(import (rename (only (srfi 1) fold-right) (fold-right fold)))
			       (fold cons '() '(1 2 3))`,
			want: "(1 2 3)",
		},
		{
			name: "direct: fold imported as fold-right folds LEFT",
			code: `(import (rename (only (srfi 1) fold) (fold fold-right)))
			       (fold-right cons '() '(1 2 3))`,
			want: "(3 2 1)",
		},
		{
			name: "reexport: fold-right re-exported then renamed to fold folds RIGHT",
			code: `(import (rename (frr) (fold-right fold)))
			       (fold cons '() '(1 2 3))`,
			want: "(1 2 3)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := reexportHOFEngine(t)
			got, err := eng.EvalMultiple(context.Background(), tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.Internal().SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestInlineHOFConflationReimportClearsStaleStamp guards a soundness edge the
// origin-following dispatch would otherwise widen: two libraries both export a
// procedure whose closure is Go-named "fold" — srfi-1's real fold (via (myfold))
// and a DIFFERENT custom fold (via (customfold)). sameImportedBinding conflates
// them by name (R7RS §5.6 last-import-wins, the documented irreducible gap), so
// the second import overwrites the value. The inline-HOF stamp from the first
// import must be CLEARED on the overwrite — otherwise (z ...) would inline srfi-1
// fold's template onto the custom fold value (a silent wrong result). z resolves
// to the custom fold, so it must return (CUSTOM-FOLD), not srfi-1 fold's 6.
func TestInlineHOFConflationReimportClearsStaleStamp(t *testing.T) {
	eng := reexportHOFEngine(t)
	got, err := eng.EvalMultiple(context.Background(),
		`(import (rename (myfold) (fold z)))
		 (import (rename (customfold) (fold z)))
		 (z + 0 '(1 2 3))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.Internal().SchemeString(), qt.Equals, "(CUSTOM-FOLD)",
		qt.Commentf("z resolves to the custom fold (last import wins); the stale srfi-1 fold stamp "+
			"must not inline its template onto the replaced value"))
}

// TestInlineHOFRenamedOntoIncompatibleArityErrors pins that renaming fold (3 args)
// onto map's name does NOT let a 2-argument call inline map's template: the call
// must reach the real fold and raise an arity error, not fabricate a value.
func TestInlineHOFRenamedOntoIncompatibleArityErrors(t *testing.T) {
	eng := reexportHOFEngine(t)
	_, err := eng.EvalMultiple(context.Background(),
		`(import (rename (only (srfi 1) fold) (fold map)))
		 (map (lambda (x) (* x x)) '(1 2 3))`)
	qt.Assert(t, errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue,
		qt.Commentf("a 2-arg call to fold-renamed-map must raise an arity error (not inline map's "+
			"template); got %v", err))
}

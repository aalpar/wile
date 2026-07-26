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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// An imported docstring must track the value it describes. Two libraries
// exporting the same name conflate under sameImportedBinding's by-name diamond
// (R7RS §5.6 last-import-wins), so the second import replaces the first's value
// in the shared slot. markBindingImported used to copy the source docstring only
// when non-empty, which cannot clear a stale one: importing a DOCUMENTED macro
// and then an UNDOCUMENTED macro of the same name left ,doc reporting the
// displaced macro's documentation for the macro that actually expands.
//
// Procedures never had this problem — a MachineClosure carries its docstring on
// its template, so it travels with the value — which is why the defect was
// confined to the macro path, where BindingMeta.Doc is the only carrier.
//
// This is the same staleness class as the inline-HOF stamp reset four lines
// above the copy site (fixed in 9b2afa8c): metadata outliving its value.
func TestImportedMacroDocTracksTheWinningValue(t *testing.T) {
	ctx := context.Background()
	fs := fstest.MapFS{
		// documented
		"maca.scm": &fstest.MapFile{Data: []byte(`(define-library (maca)
  (export mac)
  (begin
    (define-syntax mac
      "DOC-FROM-MACA: expands to the symbol maca."
      (syntax-rules () ((_) 'maca)))))
`)},
		// same export name, NO docstring
		"macb.scm": &fstest.MapFile{Data: []byte(`(define-library (macb)
  (export mac)
  (begin
    (define-syntax mac
      (syntax-rules () ((_) 'macb)))))
`)},
	}
	newEng := func(t *testing.T) *wile.Engine {
		t.Helper()
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(fs),
			wile.WithSourceFS(stdlib.FS),
			wile.WithLibraryPaths("."))
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		return eng
	}
	// macroDoc reads the docstring ,doc reports for a macro: the expand-phase
	// binding's meta, which is the macro path's only doc carrier.
	macroDoc := func(t *testing.T, eng *wile.Engine, name string) string {
		t.Helper()
		bnd := eng.Namespace().Expand().GetBinding(values.NewSymbol(name), values.AllScopes())
		qt.Assert(t, bnd, qt.IsNotNil,
			qt.Commentf("%q must be bound in the expand phase for the doc assertion to mean anything", name))
		return bnd.Doc()
	}

	// The regression. macb's undocumented macro wins the slot, so the docstring
	// must go with maca's displaced value — not linger describing it.
	t.Run("documented then undocumented: doc is cleared with the displaced value", func(t *testing.T) {
		eng := newEng(t)
		v, err := eng.EvalMultiple(ctx, `(import (maca) (macb)) (mac)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "macb",
			qt.Commentf("last import must win the value, or this test is not exercising the conflation"))
		qt.Assert(t, macroDoc(t, eng, "mac"), qt.Equals, "",
			qt.Commentf("macb's macro has no docstring; maca's must not survive to describe it"))
	})

	// The mirror. The documented macro wins, so its doc must be installed even
	// though the slot already held an (empty) doc from the earlier import.
	t.Run("undocumented then documented: doc follows the winning value", func(t *testing.T) {
		eng := newEng(t)
		v, err := eng.EvalMultiple(ctx, `(import (macb) (maca)) (mac)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "maca")
		qt.Assert(t, macroDoc(t, eng, "mac"), qt.Equals, "DOC-FROM-MACA: expands to the symbol maca.")
	})

	// Guard against over-correcting: dropping the non-empty condition must not
	// cost a plain import its docstring (the behavior 028c37e0 added the copy for).
	t.Run("single import of a documented macro keeps its docstring", func(t *testing.T) {
		eng := newEng(t)
		v, err := eng.EvalMultiple(ctx, `(import (maca)) (mac)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "maca")
		qt.Assert(t, macroDoc(t, eng, "mac"), qt.Equals, "DOC-FROM-MACA: expands to the symbol maca.")
	})
}

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

// An imported macro keeps its docstring: a macro has no template to carry one, so
// markBindingImported copies BindingMeta.Doc across the import, and ,doc reads it
// from the expand-phase binding.
//
// This test used to pin the displacement case as well: two libraries each
// defining a macro mac, imported one after the other, read as one binding under
// the old by-name comparison, so the second import replaced the first's value and
// the copied docstring had to be replaced with it. Those two macros have
// different provenance roots, so that import is now a conflict
// (TestImportConflictDetectionByOrigin) and no value is displaced.
func TestImportedMacroKeepsItsDocstring(t *testing.T) {
	ctx := context.Background()
	fs := fstest.MapFS{
		"maca.scm": &fstest.MapFile{Data: []byte(`(define-library (maca)
  (export mac)
  (begin
    (define-syntax mac
      "DOC-FROM-MACA: expands to the symbol maca."
      (syntax-rules () ((_) 'maca)))))
`)},
	}
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fs),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	v, err := eng.EvalMultiple(ctx, `(import (maca)) (mac)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "maca")
	bnd := eng.Namespace().Expand().GetBinding(values.NewSymbol("mac"), values.AllScopes())
	qt.Assert(t, bnd, qt.IsNotNil,
		qt.Commentf("mac must be bound in the expand phase for the doc assertion to mean anything"))
	qt.Assert(t, bnd.Doc(), qt.Equals, "DOC-FROM-MACA: expands to the symbol maca.")
}

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
	"github.com/aalpar/wile/pkg/wile"
)

// A library that imports a macro and then defines its own macro of the same
// name must use — and export — its OWN definition, not the imported one. The
// local definition shadows the import (the variable analogue already behaves
// this way). Previously the imported macro was mirrored into the library's
// runtime frame, and both findLibraryBinding (export) and body macro resolution
// found that mirror first, so the imported macro won.
func TestLibraryLocalMacroShadowsImportedMacro(t *testing.T) {
	ctx := context.Background()
	fs := fstest.MapFS{
		"mb.scm": &fstest.MapFile{Data: []byte(`(define-library (mb)
  (export twice)
  (begin
    (define-syntax twice (syntax-rules () ((_ x) (list 'mb-doubles (* 2 x)))))))
`)},
		// mc imports mb (which exports macro twice), then defines and exports its
		// own twice. Its own use-twice must resolve to mc's macro.
		"mc.scm": &fstest.MapFile{Data: []byte(`(define-library (mc)
  (import (mb))
  (export twice use-twice)
  (begin
    (define-syntax twice (syntax-rules () ((_ x) (list 'mc-triples (+ x x x)))))
    (define (use-twice n) (twice n))))
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

	t.Run("exported macro is the library's own", func(t *testing.T) {
		eng := newEng(t)
		v, err := eng.EvalMultiple(ctx, `(import (mc)) (twice 5)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "(mc-triples 15)")
	})

	t.Run("library body resolves its own macro", func(t *testing.T) {
		eng := newEng(t)
		v, err := eng.EvalMultiple(ctx, `(import (mc)) (use-twice 5)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "(mc-triples 15)")
	})

	// Regression: importing a macro without redefining it must still work — the
	// imported macro is usable in the importing library's body.
	t.Run("plain imported macro still usable", func(t *testing.T) {
		fs2 := fstest.MapFS{
			"mb.scm": fs["mb.scm"],
			"md.scm": &fstest.MapFile{Data: []byte(`(define-library (md)
  (import (mb))
  (export apply-twice)
  (begin
    (define (apply-twice n) (twice n))))
`)},
		}
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(fs2),
			wile.WithSourceFS(stdlib.FS),
			wile.WithLibraryPaths("."))
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		v, err := eng.EvalMultiple(ctx, `(import (md)) (apply-twice 5)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "(mb-doubles 10)")
	})
}

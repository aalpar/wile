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

var libDVFS = fstest.MapFS{
	"dvlib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (dvlib)
  (import (scheme base))
  (export a b c d)
  (begin
    (define-values (a b) (values 11 22))
    (define-values (c d) (values 33 44))))`),
	},
}

// TestLibraryBodyDefineValues guards that define-values works inside a library
// body, including two in one body. Library bodies compile in a flat, mutable
// child-runtime frame (one expansion per form), so the macro-introduced temp
// does not hit the immutable-top-level / cross-unit collision that the top-level
// binder-hygiene pass exists to solve — this pins that they keep working.
func TestLibraryBodyDefineValues(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(libDVFS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	q, evalErr := eng.EvalMultiple(ctx, `(import (dvlib)) (list a b c d)`)
	qt.Assert(t, evalErr, qt.IsNil, qt.Commentf("library body define-values"))
	qt.Assert(t, q.SchemeString(), qt.Equals, "(11 22 33 44)")
}

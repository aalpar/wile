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
	"fmt"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestMaxExpandDepthPopulatedOnNamespace verifies the engine forwards its
// configured (or defaulted) expansion-depth bound onto the shared EngineServices
// so NewExpanderTimeContinuation can read it. An explicit WithMaxExpandDepth(0)
// (unlimited) must be retained and distinguished from "unset" via the set flag.
// Sibling of TestInlineThresholdPopulatedOnNamespace.
func TestMaxExpandDepthPopulatedOnNamespace(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name string
		opts []wile.EngineOption
		want int
	}{
		{name: "explicit non-default", opts: []wile.EngineOption{wile.WithMaxExpandDepth(1234)}, want: 1234},
		{name: "explicit zero (unlimited) is preserved", opts: []wile.EngineOption{wile.WithMaxExpandDepth(0)}, want: 0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx, tc.opts...)
			qt.Assert(t, err, qt.IsNil)

			got, ok := eng.Namespace().MaxExpandDepth()
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}

	// A default engine (no WithMaxExpandDepth) still populates the namespace with
	// a positive, set value — the crux is that the expander sites never see an
	// unset (false) flag for an engine-built namespace.
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	got, ok := eng.Namespace().MaxExpandDepth()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got > 0, qt.IsTrue)
}

// TestMaxExpandDepthHonoredForImportedLibrary is the regression guard for the
// fix: macro expansion INSIDE an imported library must obey the engine's
// WithMaxExpandDepth. Library body macros are re-expanded during compilation
// through per-site expanders (not the top-level pass), so the bound only reaches
// them because NewExpanderTimeContinuation reads it from the env's namespace.
//
// deep is a recursive syntax-rules macro: each use re-invokes deep on the list
// tail, so a 30-element argument expands ~90 levels deep. A default engine
// imports it fine; a tight WithMaxExpandDepth(20) makes the import fail with a
// catchable ErrExpandDepthExceeded instead of a fatal Go stack overflow.
// Pre-fix the library path ignored the bound entirely, so even depth 20 imported
// successfully — this test failed.
func TestMaxExpandDepthHonoredForImportedLibrary(t *testing.T) {
	ctx := context.Background()

	items := strings.TrimSpace(strings.Repeat("a ", 30))
	src := fmt.Sprintf(`(define-library (probe deep)
  (import (scheme base))
  (export d)
  (begin
    (define-syntax deep
      (syntax-rules ()
        ((_ () acc) acc)
        ((_ (x . rest) acc) (deep rest (+ 1 acc)))))
    (define d (deep (%s) 0))))`, items)
	fsys := fstest.MapFS{
		"probe/deep.sld": &fstest.MapFile{Data: []byte(src)},
	}

	importDeep := func(t *testing.T, opts ...wile.EngineOption) error {
		base := []wile.EngineOption{
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
		}
		eng, err := wile.NewEngine(ctx, append(base, opts...)...)
		qt.Assert(t, err, qt.IsNil)

		_, err = eng.EvalMultiple(ctx, `(import (probe deep))`)
		return err
	}

	// Default bound (50000): the ~90-level expansion is well within it.
	qt.Assert(t, importDeep(t), qt.IsNil)

	// Tight bound: the library's own macro re-expansion now trips the guard.
	err := importDeep(t, wile.WithMaxExpandDepth(20))
	qt.Assert(t, errors.Is(err, werr.ErrExpandDepthExceeded), qt.IsTrue)
}

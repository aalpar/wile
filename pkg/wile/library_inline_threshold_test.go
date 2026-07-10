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
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// TestInlineThresholdPopulatedOnNamespace verifies the engine forwards its
// configured (or defaulted) inline threshold onto the shared EngineServices so
// the library-load path can read it. An explicit WithInlineThreshold(0) must be
// retained and distinguished from "unset" via the set flag.
func TestInlineThresholdPopulatedOnNamespace(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name string
		opts []wile.EngineOption
		want int
	}{
		{name: "explicit non-default", opts: []wile.EngineOption{wile.WithInlineThreshold(9)}, want: 9},
		{name: "explicit zero (inlining disabled) is preserved", opts: []wile.EngineOption{wile.WithInlineThreshold(0)}, want: 0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx, tc.opts...)
			qt.Assert(t, err, qt.IsNil)

			got, ok := eng.Namespace().InlineThreshold()
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}

	// A default engine (no WithInlineThreshold) still populates the namespace
	// with a positive, set value — the crux is that the library path never sees
	// an unset (false) flag for an engine-built namespace.
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	got, ok := eng.Namespace().InlineThreshold()
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got > 0, qt.IsTrue)
}

// TestInlineThresholdHonoredForImportedLibrary is the regression guard for the
// fix: procedure inlining inside an imported library must obey the engine's
// WithInlineThreshold, not the compiler default. use-helper calls a let-bound
// helper f; when inlining is enabled the call is folded to the inlined body (no
// CallLocal in the proc's bytecode), and when disabled it stays a real local
// call (CallLocal present). Pre-fix the library path always compiled at the
// default threshold, so the disabled case would fail (call still inlined).
func TestInlineThresholdHonoredForImportedLibrary(t *testing.T) {
	ctx := context.Background()

	// use-helper's body has exactly one local call: (f n). So "CallLocal" appears
	// in its disassembly iff that call is NOT inlined. (The let-binding's dead
	// MakeClosure is emitted either way, so it is not a usable discriminator.)
	fsys := fstest.MapFS{
		"probe/helper.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe helper)
  (import (scheme base))
  (export use-helper)
  (begin
    (define (use-helper n)
      (let ((f (lambda (x) (+ x 1))))
        (f n)))))`),
		},
	}

	helperCalledLocally := func(t *testing.T, opts ...wile.EngineOption) bool {
		base := []wile.EngineOption{
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
		}
		eng, err := wile.NewEngine(ctx, append(base, opts...)...)
		qt.Assert(t, err, qt.IsNil)

		_, err = eng.EvalMultiple(ctx, `(import (probe helper))`)
		qt.Assert(t, err, qt.IsNil)

		v, ok := eng.Get("use-helper")
		qt.Assert(t, ok, qt.IsTrue)
		closure, ok := v.Internal().(*machine.MachineClosure)
		qt.Assert(t, ok, qt.IsTrue)

		disasm := machine.DisassembleString(closure.Template())
		return strings.Contains(disasm, "CallLocal")
	}

	// Default engine: inlining on -> the call is folded in, no CallLocal.
	qt.Assert(t, helperCalledLocally(t), qt.IsFalse)

	// WithInlineThreshold(0): inlining off -> the call survives as CallLocal.
	// This is the case the fix repairs; it failed before the library path read
	// the engine threshold.
	qt.Assert(t, helperCalledLocally(t, wile.WithInlineThreshold(0)), qt.IsTrue)
}

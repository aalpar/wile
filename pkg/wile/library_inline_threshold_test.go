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
// helper f; when inlining is enabled f's BODY appears in use-helper's own
// bytecode, and when disabled only a call to f's slot does. Pre-fix the library
// path always compiled at the default threshold, so the disabled case would
// fail (body still inlined).
func TestInlineThresholdHonoredForImportedLibrary(t *testing.T) {
	ctx := context.Background()

	// The discriminator is f's body operator, `+`, appearing in use-helper's own
	// disassembly — it is there iff the body was folded in.
	//
	// It replaces an earlier "CallLocal is present iff NOT inlined" test, which
	// stopped discriminating once frame reclaim was armed inside a merged `let`
	// body: the un-inlined call now compiles to
	// `PushLocal f; PushLocal n; ReleaseEnvFrame; PullApply`, and the peephole
	// correctly refuses to fuse a PushLocal callee across the release (a
	// CallLocal there would resolve f out of the frame just handed to the pool —
	// peephole.go, releaseSafeCallee). So BOTH branches lost their CallLocal and
	// the proxy read "inlined" for everything.
	//
	// Asserting on the inlined BODY rather than on the shape of the call it
	// replaces is what makes this robust: the body's presence is the property
	// under test, where CallLocal was only a side effect of how the surviving
	// call happened to fuse. It is still a disassembly string, so a change in how
	// `+` renders here would break it — but loudly, and in both arms at once,
	// where the old proxy degraded to a constant.
	// (The let-binding's dead MakeClosure is emitted either way, so it is not a
	// usable discriminator.)
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

	helperBodyInlined := func(t *testing.T, opts ...wile.EngineOption) bool {
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
		return strings.Contains(disasm, `<global-index "+">`)
	}

	// Default engine: inlining on -> f's body is folded into use-helper.
	qt.Assert(t, helperBodyInlined(t), qt.IsTrue)

	// WithInlineThreshold(0): inlining off -> f stays a real call, so its body
	// is absent from use-helper. This is the case the fix repairs; it failed
	// before the library path read the engine threshold.
	qt.Assert(t, helperBodyInlined(t, wile.WithInlineThreshold(0)), qt.IsFalse)
}

// TestInlineThresholdBoundaryForImportedLibrary proves a NON-DEFAULT POSITIVE
// threshold gates library inlining at the exact body-length boundary — not just
// the on/off extremes the test above covers. Inlining keeps a let-bound lambda
// iff len(body) <= threshold (compile_let.go: len(body) > threshold skips it).
// use-helper binds f with a 3-expression body, so threshold 2 leaves f a real
// call and threshold 3 folds its body in. A regression that honored only
// 0-vs-nonzero — mapping every positive value to the default — would pass the
// extremes test yet fail here.
func TestInlineThresholdBoundaryForImportedLibrary(t *testing.T) {
	ctx := context.Background()

	// f's body is (x x (+ x 1)) -> length 3. The two leading bare-x expressions
	// are inert; they exist only to set the body length. The discriminator is f's
	// body operator appearing in use-helper's own bytecode — see the sibling test
	// above for why "CallLocal" no longer serves.
	fsys := fstest.MapFS{
		"probe/helper.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe helper)
  (import (scheme base))
  (export use-helper)
  (begin
    (define (use-helper n)
      (let ((f (lambda (x) x x (+ x 1))))
        (f n)))))`),
		},
	}

	bodyInlined := func(t *testing.T, threshold int) bool {
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
			wile.WithInlineThreshold(threshold),
		)
		qt.Assert(t, err, qt.IsNil)

		_, err = eng.EvalMultiple(ctx, `(import (probe helper))`)
		qt.Assert(t, err, qt.IsNil)

		v, ok := eng.Get("use-helper")
		qt.Assert(t, ok, qt.IsTrue)
		closure, ok := v.Internal().(*machine.MachineClosure)
		qt.Assert(t, ok, qt.IsTrue)

		return strings.Contains(machine.DisassembleString(closure.Template()), `<global-index "+">`)
	}

	// threshold 2 < body length 3 -> not inlined -> f's body absent.
	qt.Assert(t, bodyInlined(t, 2), qt.IsFalse)
	// threshold 3 == body length 3 -> inlined -> f's body present.
	qt.Assert(t, bodyInlined(t, 3), qt.IsTrue)
}

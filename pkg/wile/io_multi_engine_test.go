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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// TestTwoEngines_CurrentOutputPortIsolated is the staff-sweep #8 regression
// oracle: two Engines in one process must not share the current-output-port
// base value. Engine A redirects its base output port to a string port p and
// writes "a" to it. Engine B then writes "b" to *its* current-output-port (its
// own default, not p). With the former package-global port parameters, B's
// current-output-port was the same cell A set to p, so "b" landed in p and
// (get-output-string p) read "ab". With per-engine io.State, B's default is its
// own port, so p reads "a".
func TestTwoEngines_CurrentOutputPortIsolated(t *testing.T) {
	ctx := context.Background()
	ea, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer ea.Close()
	eb, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eb.Close()

	// Engine A redirects its current-output-port to a string port and writes.
	_, err = ea.EvalMultiple(ctx, `
		(define p (open-output-string))
		(current-output-port p)
		(display "a" (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)

	// Engine B writes "b" to its own current-output-port. Under isolation this
	// is B's own default port, leaving A's string port untouched.
	_, err = eb.EvalMultiple(ctx, `(display "b" (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)

	got, err := ea.EvalMultiple(ctx, `(get-output-string p)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, `"a"`)
}

// TestTwoEngines_CurrentInputPortIsolated is the input-side companion: engine A
// redirects its base input port to a string port and reads one char; engine B's
// current-input-port must be its own default, not A's string port. Reading a
// char in A must not advance or share B's read position.
func TestTwoEngines_CurrentInputPortIsolated(t *testing.T) {
	ctx := context.Background()
	ea, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer ea.Close()
	eb, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eb.Close()

	// Engine A redirects its current-input-port to "xy" and reads one char ('x').
	got, err := ea.EvalMultiple(ctx, `
		(current-input-port (open-input-string "xy"))
		(read-char (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, `#\x`)

	// Engine B redirects ITS current-input-port to a different string and reads.
	// This is the discriminating step: under the former shared-global param, B's
	// redirect would overwrite the one shared cell, so A's next read would come
	// from B's "zw" port ('w') instead of continuing A's own "xy" port.
	_, err = eb.EvalMultiple(ctx, `
		(current-input-port (open-input-string "zw"))
		(read-char (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)

	// A continues its OWN port at 'y' — proving B's redirect did not touch A's.
	got, err = ea.EvalMultiple(ctx, `(read-char (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, `#\y`)
}

// TestImportDoesNotResetCurrentPort guards the idempotent io namespace-init
// hook. Importing a library re-runs applyBaseEnvironment (hence the hook) against
// a library environment that SHARES the engine's Namespace (NewChildRuntime). The
// hook must REUSE the engine's existing io.State, not mint a fresh one — otherwise
// the import silently resets current-output-port mid-program, discarding a prior
// redirect. Regression for a defect found verifying staff-sweep #8: a fresh
// per-Apply State sent output to stdout the moment any library was imported.
func TestImportDoesNotResetCurrentPort(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(wile.StdLibFS),
		wile.WithLibraryPaths("lib"))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	// Redirect the DEFAULT output port to a string port, THEN import a library,
	// THEN write with no explicit port arg. The default-port write must resolve
	// through the engine's (unchanged) State and land in the redirected port, not
	// stdout. The no-arg display is essential: it exercises current-output-port
	// resolution, which the reset-on-import bug corrupted; an explicit
	// (current-output-port) arg would read the still-bound original param and mask
	// the defect.
	got, err := eng.EvalMultiple(ctx, `
		(define p (open-output-string))
		(current-output-port p)
		(import (scheme write))
		(display "x")
		(get-output-string p)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, `"x"`)
}

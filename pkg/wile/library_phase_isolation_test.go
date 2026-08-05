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

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// A define-library body must separate phases exactly as the top level does.
// Each direction is asserted TWICE — once at the top level, once in a library —
// and the point of the pair is that the two results agree. Asserting only the
// library half would pass against an implementation that had merely become
// uniformly wrong.
//
// See plans/2026-08-04-library-phase-isolation-{design,impl}.local.md.
func phaseIsolationEngine(t *testing.T, files fstest.MapFS) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(files),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// Downward: phase-0 code must not resolve a phase-1 define. The library form of
// this produced a wrong VALUE, not a wrong void — (go) returned 99.
func TestLibraryPhaseIsolationDownward(t *testing.T) {
	ctx := context.Background()

	t.Run("top level refuses", func(t *testing.T) {
		eng := phaseIsolationEngine(t, fstest.MapFS{})
		_, err := eng.EvalMultiple(ctx, `(begin-for-syntax (define ctonly 99))
			(define (go) ctonly)
			(go)`)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
			qt.Commentf("want ErrNoSuchBinding, got: %v", err))
	})

	t.Run("library refuses too", func(t *testing.T) {
		eng := phaseIsolationEngine(t, fstest.MapFS{
			"down.scm": &fstest.MapFile{Data: []byte(`(define-library (down)
  (export go)
  (import (scheme base))
  (begin
    (begin-for-syntax (define ctonly 99))
    (define (go) ctonly)))
`)},
		})
		_, err := eng.EvalMultiple(ctx, `(import (down)) (go)`)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
			qt.Commentf("want ErrNoSuchBinding, got: %v", err))
	})
}

// Upward: phase-1 code must not resolve a phase-0 define. The library form of
// this was silent — the phase-1 body read the predeclared, not-yet-written
// phase-0 slot as #!void and computed with it.
func TestLibraryPhaseIsolationUpward(t *testing.T) {
	ctx := context.Background()

	t.Run("top level refuses", func(t *testing.T) {
		eng := phaseIsolationEngine(t, fstest.MapFS{})
		_, err := eng.EvalMultiple(ctx, `(define counter 41)
			(begin-for-syntax (define seen counter))`)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
			qt.Commentf("want ErrNoSuchBinding, got: %v", err))
	})

	t.Run("library refuses too", func(t *testing.T) {
		eng := phaseIsolationEngine(t, fstest.MapFS{
			"up.scm": &fstest.MapFile{Data: []byte(`(define-library (up)
  (export dummy)
  (import (scheme base))
  (begin
    (define counter 41)
    (begin-for-syntax (define seen counter))
    (define dummy 0)))
`)},
		})
		_, err := eng.EvalMultiple(ctx, `(import (up)) dummy`)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
			qt.Commentf("want ErrNoSuchBinding, got: %v", err))
	})
}

// A library begin-for-syntax body must still reach the library's PRIMITIVES.
// This is design E4's supply line, and it is the reason the phase-relative arm
// cannot ship before the library sealed base: without the base, phase-1 library
// code loses car/list along with the phase-0 defines it is supposed to lose.
func TestLibraryPhaseOneReachesPrimitives(t *testing.T) {
	ctx := context.Background()
	eng := phaseIsolationEngine(t, fstest.MapFS{
		"prims.scm": &fstest.MapFile{Data: []byte(`(define-library (prims)
  (export ok)
  (import (scheme base))
  (begin
    (begin-for-syntax (define ct (car (list 1 2 3))))
    (define ok 'yes)))
`)},
	})
	v, err := eng.EvalMultiple(ctx, `(import (prims)) ok`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "yes")
}

// The arm is phase-RELATIVE, not phase-0-pinned: a library's phase-1 code must
// still resolve the library's own phase-1 defines. Without this the fix would
// have traded the upward leak for a phase-1 blackout.
func TestLibraryPhaseOneSeesItsOwnPhaseOneDefines(t *testing.T) {
	ctx := context.Background()
	eng := phaseIsolationEngine(t, fstest.MapFS{
		"ct.scm": &fstest.MapFile{Data: []byte(`(define-library (ct)
  (export ok)
  (import (scheme base))
  (begin
    (begin-for-syntax (define ctbase 7))
    (begin-for-syntax (define ctderived (+ ctbase 1)))
    (define ok 'yes)))
`)},
	})
	v, err := eng.EvalMultiple(ctx, `(import (ct)) ok`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "yes")
}

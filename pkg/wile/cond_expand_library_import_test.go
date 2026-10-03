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
	"bytes"
	"context"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	ioext "github.com/aalpar/wile/pkg/extensions/io"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// TestCondExpandLibraryImportability verifies that cond-expand's (library X)
// requirement is satisfied by real importability, not mere file presence
// (plan 5F/P6). A .sld that resolves on disk but fails to import (here, because
// it imports a non-existent library) must NOT satisfy (library X) — the else
// clause is selected instead.
func TestCondExpandLibraryImportability(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		// Imports cleanly.
		"probe/good.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe good)
  (import (scheme base))
  (export good-marker)
  (begin (define good-marker 1)))`),
		},
		// Resolves on disk but fails to import: depends on a library that
		// does not exist, so LoadLibrary errors.
		"probe/broken.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe broken)
  (import (no such library))
  (export x)
  (begin (define x 1)))`),
		},
	}

	newEngine := func() *wile.Engine {
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
		)
		c.Assert(err, qt.IsNil)
		return eng
	}

	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "importable library satisfies requirement",
			code: `(cond-expand ((library (probe good)) 'have-good) (else 'no-good))`,
			want: "have-good",
		},
		{
			name: "resolvable-but-unimportable library does not satisfy",
			code: `(cond-expand ((library (probe broken)) 'have-broken) (else 'fallback))`,
			want: "fallback",
		},
		{
			name: "absent library does not satisfy",
			code: `(cond-expand ((library (probe absent)) 'have-absent) (else 'fallback))`,
			want: "fallback",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newEngine()
			result, err := eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestCondExpandLibraryProbeDoesNotRunTheBody is P3.14's gate (I141). A
// (library X) requirement asks whether X is AVAILABLE (R7RS §4.2.1); deciding it
// by importing X answers a different question and charges the program X's
// effects for asking.
//
// Two rows are RED on master: the leaf purity row, where the probe printed SFX,
// and the criterion row, where a library whose BODY fails at runtime answered
// not-available because the old probe executed it. The third row is the residual
// this change does NOT close, pinned so it is recorded rather than discovered.
func TestCondExpandLibraryProbeDoesNotRunTheBody(t *testing.T) {
	ctx := context.Background()

	fsys := fstest.MapFS{
		// A body with an observable effect.
		"probe/sfx.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe sfx)
  (import (scheme base) (scheme write))
  (export sfx-marker)
  (begin (display "SFX") (define sfx-marker 1)))`),
		},
		// Imports (probe sfx), so probing THIS library instantiates that one.
		"probe/depper.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe depper)
  (import (scheme base) (probe sfx))
  (export dep)
  (begin (define dep sfx-marker)))`),
		},
		// Compiles; raises when run.
		"probe/bust.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe bust)
  (import (scheme base))
  (export bz)
  (begin (define bz (car (list)))))`),
		},
	}

	// newEngine returns an engine whose current-output-port is buf, so a library
	// body's effect is observable from Go. The port is installed before anything
	// is evaluated, which matters: an import runs at COMPILE time of the form
	// that contains it, outside any dynamic extent a parameterize could cover.
	newEngine := func(t *testing.T) (*wile.Engine, *bytes.Buffer) {
		t.Helper()
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
		)
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		st, ok := eng.Namespace().IOState().(*ioext.State)
		qt.Assert(t, ok, qt.IsTrue)
		buf := &bytes.Buffer{}
		st.SetOutputPort(values.NewCharacterOutputPortFromWriter(buf))
		return eng, buf
	}

	t.Run("a leaf probe runs no body", func(t *testing.T) {
		eng, buf := newEngine(t)
		v, err := eng.EvalMultiple(ctx,
			`(cond-expand ((library (probe sfx)) 'available) (else 'not-available))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "available")
		qt.Assert(t, buf.String(), qt.Equals, "",
			qt.Commentf("RED on master, which printed SFX while deciding the clause"))
	})

	t.Run("the import that follows a probe runs the body exactly once", func(t *testing.T) {
		eng, buf := newEngine(t)
		_, err := eng.EvalMultiple(ctx,
			`(cond-expand ((library (probe sfx)) 'available) (else 'not-available))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, buf.String(), qt.Equals, "")

		v, err := eng.EvalMultiple(ctx, `(import (probe sfx)) sfx-marker`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "1")
		// Once, not twice and not never: the probe left a compiled library whose
		// instantiation was still pending, and the import is what flushes it.
		qt.Assert(t, strings.Count(buf.String(), "SFX"), qt.Equals, 1)
	})

	t.Run("an absent library is still unavailable", func(t *testing.T) {
		eng, buf := newEngine(t)
		v, err := eng.EvalMultiple(ctx,
			`(cond-expand ((library (probe absent)) 'available) (else 'not-available))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "not-available")
		qt.Assert(t, buf.String(), qt.Equals, "")
	})

	t.Run("a library whose body raises is available", func(t *testing.T) {
		eng, _ := newEngine(t)
		v, err := eng.EvalMultiple(ctx,
			`(cond-expand ((library (probe bust)) 'available) (else 'not-available))`)
		qt.Assert(t, err, qt.IsNil)
		// RED on master, which answered not-available because the probe ran the
		// body and took the raise as evidence of unavailability. The criterion
		// change is deliberate and documented in
		// docs/reference/r7rs-differences.md: §4.2.1 asks whether the library is
		// available, not for its state.
		qt.Assert(t, v.SchemeString(), qt.Equals, "available",
			qt.Commentf("RED on master, which answered not-available"))
	})

	t.Run("RESIDUAL: probing a library instantiates the libraries IT imports", func(t *testing.T) {
		eng, buf := newEngine(t)
		v, err := eng.EvalMultiple(ctx,
			`(cond-expand ((library (probe depper)) 'available) (else 'not-available))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "available")
		// NOT a target of this change, and pinned so the gap is recorded rather
		// than rediscovered. The deferral is one level deep by construction: a
		// library's own import clauses go through LoadLibrary with instantiate
		// true, because resolveImportSets is the shared parse-load-apply prefix
		// for the top-level and library-internal paths alike, and only the
		// probe's own call is parameterised. Closing this means threading the
		// flag through that prefix.
		qt.Assert(t, strings.Count(buf.String(), "SFX"), qt.Equals, 1,
			qt.Commentf("the probe is pure at its own level only"))
	})
}

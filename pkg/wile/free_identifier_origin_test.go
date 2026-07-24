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

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// originLibFS defines three libraries used to exercise free-identifier=? across
// the import-provenance graph (plan
// 2026-07-24-free-identifier-origin-provenance-design):
//
//   - (aa)    a root library with a single (define foo ...);
//   - (reexp) imports foo from (aa) and re-exports it unchanged (a re-export hop);
//   - (cc)    imports foo from (aa) and re-exports it RENAMED as bar (a re-export
//     hop that also changes the surface name);
//   - (dd)    defines x and exports it under TWO renamed names (foo, bar) — the
//     SYNTHESIZE branch of the origin fold, where the internal defining name (x)
//     differs from both external names;
//   - (bb)    an unrelated library with its OWN (define foo ...).
//
// Rename-imports of (aa)'s foo — taken directly, through (reexp), or through
// (cc) under the surface name bar — all denote the SAME binding; (bb)'s foo is a
// DIFFERENT binding that merely shares the name.
func originLibFS() fstest.MapFS {
	return fstest.MapFS{
		"aa.scm": &fstest.MapFile{Data: []byte(`(define-library (aa)
  (export foo)
  (import (scheme base))
  (begin (define (foo x) (car x))))
`)},
		"reexp.scm": &fstest.MapFile{Data: []byte(`(define-library (reexp)
  (export foo)
  (import (aa)))
`)},
		"cc.scm": &fstest.MapFile{Data: []byte(`(define-library (cc)
  (export (rename foo bar))
  (import (aa)))
`)},
		"dd.scm": &fstest.MapFile{Data: []byte(`(define-library (dd)
  (export (rename x foo) (rename x bar))
  (import (scheme base))
  (begin (define (x p) (car p))))
`)},
		"bb.scm": &fstest.MapFile{Data: []byte(`(define-library (bb)
  (export foo)
  (import (scheme base))
  (begin (define (foo x) (cdr x))))
`)},
	}
}

func newOriginEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(originLibFS()),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestFreeIdentifierOriginProvenance pins R7RS-conformant free-identifier=?
// behavior (verified against Racket + Chez) on identifiers whose bindings share,
// or do not share, an import-provenance root. Two rename-imports of ONE (define)
// — imported directly or through a re-exporting library, renamed or not — are
// the same binding; two DIFFERENT (define)s under one name are different
// bindings.
//
// Guards the origin-provenance comparator (plan §4/§5): environment.SameBinding
// keys on the import root, replacing the former pointer-equality at
// prim_syntax.go, which mints a distinct *Binding per import and so answered #f
// on every cross-import pair.
func TestFreeIdentifierOriginProvenance(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "reexport chain: foo via (aa) and via (reexp) are same",
			code: `(import (rename (aa) (foo foo1))
			                (rename (reexp) (foo foo2)))
			       (free-identifier=? #'foo1 #'foo2)`,
			want: "#t",
		},
		{
			name: "direct double-rename: foo twice from (aa) are same",
			code: `(import (rename (aa) (foo a1))
			                (rename (aa) (foo a2)))
			       (free-identifier=? #'a1 #'a2)`,
			want: "#t",
		},
		{
			// Root keys on the DEFINING name (internal foo), not the surface
			// name: (cc) re-exports foo as bar, so x (surface foo) and y
			// (surface bar) differ by name yet share root (aa, foo).
			name: "renamed re-export keys on defining name, not surface name",
			code: `(import (rename (aa) (foo x))
			                (rename (cc) (bar y)))
			       (free-identifier=? #'x #'y)`,
			want: "#t",
		},
		{
			// Synthesize branch: (dd) defines ONE binding x, exports it under
			// two renamed external names. Both f and b key on the DEFINING name
			// x, so they share root (dd, x) — pins that the fold uses
			// internalName, not exportName (a swap to exportName makes this #f).
			name: "export-renamed to two names keys on internal define name",
			code: `(import (rename (dd) (foo f))
			                (rename (dd) (bar b)))
			       (free-identifier=? #'f #'b)`,
			want: "#t",
		},
		{
			name: "different libraries, same name are different",
			code: `(import (rename (aa) (foo a1))
			                (rename (bb) (foo b1)))
			       (free-identifier=? #'a1 #'b1)`,
			want: "#f",
		},
		{
			// Guard for the removed same-value fallback direction: an
			// unimported sealed-base primitive (nil origin) compared to itself
			// resolves to the identical binding object, so SameBinding's a==b
			// short-circuit keeps this #t.
			name: "sealed-base primitive against itself is same",
			code: `(free-identifier=? #'car #'car)`,
			want: "#t",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newOriginEngine(t)
			got := evalString(t, eng, tc.code)
			if got != tc.want {
				t.Errorf("free-identifier=?: got %s, want %s", got, tc.want)
			}
		})
	}
}

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

// erSelfOriginLibFS defines two libraries whose foo holds the SAME value (car):
//   - (erlib)  exports foo and an ER macro is-foo? that compares its argument
//     against (rename 'foo) — a rename resolved at erlib's DEFINITION site, so
//     it denotes erlib's internal foo binding;
//   - (bblib)  exports its OWN foo (also car, a distinct binding).
//
// Same value, different defining library: the value-fallback ER-compare matched
// them (over-match); origin-keyed SameBinding must not.
func erSelfOriginLibFS() fstest.MapFS {
	return fstest.MapFS{
		"erlib.scm": &fstest.MapFile{Data: []byte(`(define-library (erlib)
  (export foo baz is-foo?)
  (import (scheme base))
  (begin
    (define foo car)
    (define baz car)
    (define-syntax is-foo?
      (er-macro-transformer
        (lambda (form rename compare)
          (if (compare (cadr form) (rename 'foo))
              (list (rename 'quote) 'yes)
              (list (rename 'quote) 'no)))))))
`)},
		"bblib.scm": &fstest.MapFile{Data: []byte(`(define-library (bblib)
  (export foo)
  (import (scheme base))
  (begin (define foo car)))
`)},
	}
}

func newErSelfOriginEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(erSelfOriginLibFS()),
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

// TestErCompareInternalVsImport pins option B: ER-compare, which resolves a
// rename at its DEFINITION site, correctly matches a library-internal binding
// against an import of ITSELF (the "did the caller hand me my own exported foo?"
// pattern), while refusing a same-named, same-valued binding from a DIFFERENT
// library. The internal binding gets its self-root on import (markBindingImported),
// so both sides carry origin (erlib, foo); the other library's foo has a distinct
// root and is refused even though its value is identical (car).
func TestErCompareInternalVsImport(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "imported foo matches (rename 'foo) at definition site",
			code: `(import (erlib))
			       (is-foo? foo)`,
			want: "yes",
		},
		{
			name: "a different identifier is not foo",
			code: `(import (erlib))
			       (define bar 5)
			       (is-foo? bar)`,
			want: "no",
		},
		{
			name: "same-named same-valued foo from another library is not foo",
			code: `(import (erlib) (rename (bblib) (foo bfoo)))
			       (is-foo? bfoo)`,
			want: "no",
		},
		{
			// Same library, DIFFERENT export (baz, also car): roots (erlib, foo)
			// vs (erlib, baz) differ on RootName, so baz is not foo — even though
			// its value is identical. Guards against keying the root on library
			// alone (RootName dropped).
			name: "different export of the same library is not foo",
			code: `(import (erlib))
			       (is-foo? baz)`,
			want: "no",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newErSelfOriginEngine(t)
			got := evalString(t, eng, tc.code)
			if got != tc.want {
				t.Errorf("is-foo?: got %s, want %s", got, tc.want)
			}
		})
	}
}

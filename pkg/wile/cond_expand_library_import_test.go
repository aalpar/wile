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

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

// multiShiftLibs holds a library whose DECLARATION imports two libraries through
// one phase shift, so the declaration install path is covered alongside the
// top-level and (environment ...) paths.
var multiShiftLibs = fstest.MapFS{
	"decl-multi.scm": &fstest.MapFile{Data: []byte(`(define-library (decl-multi)
  (export probe)
  (import (scheme base))
  (import (for-syntax (scheme base) (scheme cxr)))
  (begin
    (define-syntax m
      (er-macro-transformer (lambda (f r c) (cadddr f))))
    (define (probe) (m 1 2 3 4))))
`)},
}

// TestPhaseShiftImportTakesEveryImportSet pins that a phase-shift import set
// with more than one operand imports every operand, as Racket's
// (for-syntax require-spec ...) does. The parser used to read the first operand
// and drop the rest silently, so (for-syntax (scheme base) (scheme cxr)) left
// cadddr unbound at phase 1 while (for-syntax (scheme base)) (for-syntax
// (scheme cxr)) bound it.
func TestPhaseShiftImportTakesEveryImportSet(t *testing.T) {
	tests := []struct {
		name    string
		src     string
		want    string
		wantErr error
	}{
		{
			name: "top-level for-syntax imports its second operand",
			src: `(import (for-syntax (scheme base) (scheme cxr)))
(define-syntax m (er-macro-transformer (lambda (f r c) (cadddr f))))
(m 1 2 3 4)`,
			want: "3",
		},
		{
			name: "top-level for-meta imports its second operand",
			src: `(import (for-meta 1 (scheme base) (scheme cxr)))
(define-syntax m (er-macro-transformer (lambda (f r c) (cadddr f))))
(m 1 2 3 4)`,
			want: "3",
		},
		{
			name: "a library declaration's for-syntax imports its second operand",
			src:  `(import (decl-multi)) (probe)`,
			want: "3",
		},
		{
			name: "an (environment ...) spec's for-syntax imports its second operand",
			src: `(import (scheme base) (scheme eval))
(eval '(let-syntax ((m (er-macro-transformer (lambda (f r c) (cadddr f))))) (m 1 2 3 4))
      (environment '(scheme base) '(for-syntax (scheme base) (scheme cxr))))`,
			want: "3",
		},
		{
			// Racket checks a filter over a multi-operand shift against the union
			// of the operands; a per-library check would reject what Racket
			// accepts, so the form is refused rather than half-supported.
			name:    "a multi-operand shift under only is refused, not filtered per library",
			src:     `(import (only (for-syntax (scheme base) (scheme cxr)) cadddr))`,
			wantErr: werr.ErrInvalidSyntax,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(multiShiftLibs),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
			)
			c.Assert(err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			v, err := eng.EvalMultiple(ctx, tc.src)
			if tc.wantErr != nil {
				c.Assert(errors.Is(err, tc.wantErr), qt.IsTrue, qt.Commentf("got %v", err))
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

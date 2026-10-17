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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestMaxExpandDepthBoundsTransformerCompilation pins that WithMaxExpandDepth
// reaches the compile of a transformer right-hand side, not only top-level text.
// The same 5-deep let nest is refused at depth 3 in all three positions; before
// the fix the two transformer positions compiled it under DefaultMaxExpandDepth.
func TestMaxExpandDepthBoundsTransformerCompilation(t *testing.T) {
	const deep = `(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) e)))))`

	tcs := []struct {
		name string
		src  string
	}{
		{name: "top-level", src: deep},
		{name: "define-syntax-rhs", src: `(define-syntax m (er-macro-transformer (lambda (f r c) ` + deep + `)))`},
		{name: "eval-primitive", src: `(eval '` + deep + `)`},
		{name: "compile-primitive", src: `((compile '` + deep + `))`},
		{name: "let-syntax-rhs", src: `(let-syntax ((m (er-macro-transformer (lambda (f r c) ` + deep + `)))) 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMaxExpandDepth(3))
			qt.Assert(t, err, qt.IsNil)

			_, err = eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, errors.Is(err, werr.ErrExpandDepthExceeded), qt.IsTrue, qt.Commentf("err = %v", err))
		})
	}
}

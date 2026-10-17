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
	"os"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestMaxExpandDepthBoundsTransformerCompilation pins that WithMaxExpandDepth
// reaches the compile of an ER-transformer lambda, the eval and compile
// primitives, and top-level text. The same 5-deep let nest is refused at depth 3
// in every position; on master all but top-level compiled it under
// DefaultMaxExpandDepth. Procedural right-hand sides and load are covered by
// TestMaxExpandDepthBoundsProceduralTransformerAndLoad.
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

// deepLet nests five lets; at WithMaxExpandDepth(3) it must be refused.
const deepLet = `(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) e)))))`
const deepLam = `(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) (lambda (x) x))))))`

// inlinableLam inlines its let-bound lambda call (visible as AddTail) unless
// WithInlineThreshold(0).
const inlinableLam = `(lambda (n) (let ((f (lambda (x) (+ x 1)))) (f n)))`

func TestMaxExpandDepthBoundsProceduralTransformerAndLoad(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, "deep.scm")
	qt.Assert(t, os.WriteFile(p, []byte(deepLet), 0o600), qt.IsNil)
	tcs := []struct{ name, src string }{
		{"define-syntax-procedural-rhs", `(define-syntax m ` + deepLam + `)`},
		{"let-syntax-procedural-rhs", `(let-syntax ((m ` + deepLam + `)) 1)`},
		{"load-primitive", `(load "` + p + `")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMaxExpandDepth(3))
			qt.Assert(t, err, qt.IsNil)
			_, err = eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, errors.Is(err, werr.ErrExpandDepthExceeded), qt.IsTrue, qt.Commentf("err = %v", err))
			// control: default engine accepts it
			eng2, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			_, err = eng2.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

func TestInlineThresholdReachesEvalCompileLoad(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, "lam.scm")
	qt.Assert(t, os.WriteFile(p, []byte(`(define g `+inlinableLam+`)`), 0o600), qt.IsNil)
	tcs := []struct{ name, src string }{
		{"eval", `(define g (eval '` + inlinableLam + `))`},
		{"compile", `(define g ((compile '` + inlinableLam + `)))`},
		{"load", `(load "` + p + `")`},
	}
	inlined := func(t *testing.T, src string, opts ...wile.EngineOption) bool {
		ctx := context.Background()
		eng, err := wile.NewEngine(ctx, append([]wile.EngineOption{wile.WithProfile(wile.KitchenSink)}, opts...)...)
		qt.Assert(t, err, qt.IsNil)
		_, err = eng.EvalMultiple(ctx, src)
		qt.Assert(t, err, qt.IsNil)
		v, ok := eng.Get("g")
		qt.Assert(t, ok, qt.IsTrue)
		c, ok := v.Internal().(*machine.MachineClosure)
		qt.Assert(t, ok, qt.IsTrue)
		d := machine.DisassembleString(c.Template())
		return strings.Contains(d, "AddTail")
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, inlined(t, tc.src), qt.IsTrue)
			qt.Assert(t, inlined(t, tc.src, wile.WithInlineThreshold(0)), qt.IsFalse)
		})
	}
}

func TestInlineThresholdReachesTransformerCompilation(t *testing.T) {
	tcs := []struct{ name, src string }{
		{"er", `(define-syntax m (er-macro-transformer (lambda (f r c) (list (r 'quote) ` + inlinableLam + `)))) (define d (m))`},
		{"procedural", `(define-syntax m (let ((h ` + inlinableLam + `)) (lambda (stx) (datum->syntax stx (list 'quote h))))) (define d (m))`},
	}
	run := func(t *testing.T, src string, opts ...wile.EngineOption) string {
		ctx := context.Background()
		eng, err := wile.NewEngine(ctx, append([]wile.EngineOption{wile.WithProfile(wile.KitchenSink)}, opts...)...)
		qt.Assert(t, err, qt.IsNil)
		_, err = eng.EvalMultiple(ctx, src)
		qt.Assert(t, err, qt.IsNil)
		v, ok := eng.Get("d")
		qt.Assert(t, ok, qt.IsTrue)
		c, ok := v.Internal().(*machine.MachineClosure)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("%T", v.Internal()))
		return machine.DisassembleString(c.Template())
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			on := run(t, tc.src)
			off := run(t, tc.src, wile.WithInlineThreshold(0))
			qt.Assert(t, strings.Contains(on, "AddTail"), qt.IsTrue)
			qt.Assert(t, strings.Contains(off, "AddTail"), qt.IsFalse)
		})
	}
}

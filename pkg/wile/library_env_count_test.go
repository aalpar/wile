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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// newLibraryCountEngine builds an engine whose (scheme base) resolves from the
// embedded stdlib and whose own libraries resolve from fsys (nil for none).
func newLibraryCountEngine(ctx context.Context, fsys fstest.MapFS) (*wile.Engine, error) {
	opts := []wile.EngineOption{
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(wile.StdLibFS),
	}
	if fsys != nil {
		opts = append(opts, wile.WithSourceFS(fsys))
	}
	opts = append(opts, wile.WithLibraryPaths("lib"))
	return wile.NewEngine(ctx, opts...)
}

// oneDefineLibrary is a file library with one define, importable as (mylib).
var oneDefineLibrary = fstest.MapFS{
	"lib/mylib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (mylib)
  (import (scheme base))
  (export answer)
  (begin (define answer 42)))`),
	},
}

// countingFactory wraps the engine's installed LibraryEnvFactory and counts
// calls. Installed after a warm-up import, so the count covers only the library
// under test and not the stdlib libraries it pulls in.
func countingFactory(ns *environment.Namespace) *int {
	n := 0
	inner := ns.LibraryEnvFactory()
	ns.SetLibraryEnvFactory(func(ctx context.Context, callerEnv *environment.EnvironmentFrame, name []string) (*environment.EnvironmentFrame, error) {
		n++
		return inner(ctx, callerEnv, name)
	})
	return &n
}

// TestOneLibraryEnvPerFileLibraryLoad pins I216: a file library takes ONE
// library environment. The loader minted one to read the file and
// CompileDefineLibrary minted a second to keep, so the first was discarded.
//
// The warm-up import is load-bearing: without it the counter also sees the
// (scheme base) library the first import pulls in, and the assertion is off by
// that constant on both sides of the fix.
func TestOneLibraryEnvPerFileLibraryLoad(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	ctx := context.Background()

	engine, err := newLibraryCountEngine(ctx, oneDefineLibrary)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(err, qt.IsNil)

	n := countingFactory(engine.Namespace())

	result, err := engine.EvalMultiple(ctx, `(import (mylib)) answer`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")
	c.Assert(*n, qt.Equals, 1)
}

// TestInlineDefineLibraryStillMintsItsOwnEnv guards the consume side of the
// hint: a (define-library ...) written in a program is not a file load, so it
// has no pre-made environment and must still take one from the factory. It
// catches a hint installed on the top-level compiler, which would hand an inline
// library the loader's environment instead of a fresh one.
func TestInlineDefineLibraryStillMintsItsOwnEnv(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	ctx := context.Background()

	engine, err := newLibraryCountEngine(ctx, oneDefineLibrary)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(err, qt.IsNil)

	n := countingFactory(engine.Namespace())

	_, err = engine.EvalMultiple(ctx, `(import (mylib))`)
	c.Assert(err, qt.IsNil)
	c.Assert(*n, qt.Equals, 1)

	_, err = engine.EvalMultiple(ctx, `
(define-library (inl)
  (import (scheme base))
  (export v)
  (begin (define v 7)))
(define-library (inl2)
  (import (scheme base))
  (export w)
  (begin (define w 8)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(*n, qt.Equals, 3)
}

// TestDefineLibraryInAProgramProcedureBodyIsRefused: a define-library inside a
// lambda body is not a top-level form of a program either; it compiled, minted
// an environment and registered nothing. An inline top-level one stays legal.
func TestDefineLibraryInAProgramProcedureBodyIsRefused(t *testing.T) {
	t.Parallel()
	c := qt.New(t)
	ctx := context.Background()
	engine, err := newLibraryCountEngine(ctx, fstest.MapFS{})
	c.Assert(err, qt.IsNil)
	for _, src := range []string{
		`(define (g) (define-library (q) (import (scheme base)) (export)) 1)`,
		`(let () (define-library (q) (import (scheme base)) (export)) 1)`,
		`(letrec ((x 1)) (define-library (q) (import (scheme base)) (export)) x)`,
		`(define-syntax m (lambda (stx) (syntax-case stx () (_ #'1)))) (define (h x) (syntax-case x () (_ (define-library (q) (import (scheme base)) (export)))))`,
	} {
		_, err = engine.EvalMultiple(ctx, src)
		c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue, qt.Commentf("%s\nerr: %v", src, err))
		c.Assert(err.Error(), qt.Contains, "define-library: not permitted inside a procedure body", qt.Commentf("%s", src))
	}
	_, err = engine.EvalMultiple(ctx, `(define-library (top) (import (scheme base)) (export z) (begin (define z 5)))`)
	c.Assert(err, qt.IsNil)
}

// TestNestedDefineLibraryIsRefused pins the refusal of a (define-library ...)
// inside a file library, in a body and in declaration position. R7RS 5.6.1
// lists the declarations, and define-library is not one, nor an expression or
// definition. Compiled anyway (the former behaviour this test pinned only
// half of), the nested library got its own environment and was never
// registered, so an export of its own name read as "no binding" and nothing
// could import it. The refusal names the nested form's location; the outer
// library's import of (dep) must not have leaked either way.
func TestNestedDefineLibraryIsRefused(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name, file, src, at string
	}{
		{"in a begin body", "lib/leak.sld", `(define-library (leak)
  (import (scheme base))
  (export answer)
  (begin
    (define-library (leak-inner)
      (import (dep))
      (export))
    (define answer depval)))`, "leak.sld:5:5"},
		{"in declaration position", "lib/leak.sld", `(define-library (leak)
  (import (scheme base))
  (export answer)
  (define-library (leak-inner) (import (dep)) (export))
  (begin (define answer 1)))`, "leak.sld:4:2"},
		{"in a procedure body inside the library", "lib/leak.sld", `(define-library (leak)
  (import (scheme base))
  (export answer)
  (begin
    (define (answer) (define-library (leak-inner) (import (dep)) (export)) 1)))`, "leak.sld:5:22"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			c := qt.New(t)
			ctx := context.Background()
			fsys := fstest.MapFS{
				"lib/dep.sld": &fstest.MapFile{
					Data: []byte(`(define-library (dep)
  (import (scheme base))
  (export depval)
  (begin (define depval 99)))`),
				},
				tc.file: &fstest.MapFile{Data: []byte(tc.src)},
			}
			engine, err := newLibraryCountEngine(ctx, fsys)
			c.Assert(err, qt.IsNil)
			_, err = engine.EvalMultiple(ctx, `(import (leak))`)
			c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue, qt.Commentf("err: %v", err))
			c.Assert(err.Error(), qt.Contains, "define-library: not permitted inside a")
			c.Assert(err.Error(), qt.Contains, tc.at)
		})
	}
}

// TestIncludeLibraryDeclarationsResolvesBesideTheLibrary is a green pin for the
// declaration-position include (untested before): a relative
// include-library-declarations resolves beside the .sld, so lib/sub/decls.scm
// is found for lib/sub/inner.sld. It held on the old findFile path too (the
// load stack carries the library's directory); it now goes through
// readIncludedForms, the reader include uses.
func TestIncludeLibraryDeclarationsResolvesBesideTheLibrary(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	ctx := context.Background()
	fsys := fstest.MapFS{
		"lib/sub/inner.sld": &fstest.MapFile{
			Data: []byte(`(define-library (sub inner)
  (import (scheme base))
  (include-library-declarations "decls.scm"))`),
		},
		"lib/sub/decls.scm": &fstest.MapFile{
			Data: []byte(`(export answer)
(begin (define answer 42))`),
		},
	}
	engine, err := newLibraryCountEngine(ctx, fsys)
	c.Assert(err, qt.IsNil)
	result, err := engine.EvalMultiple(ctx, `(import (sub inner)) answer`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

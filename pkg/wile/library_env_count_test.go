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

// TestNestedDefineLibraryDoesNotShareTheOuterEnv pins that a (define-library
// ...) nested in a file library's body gets its own environment. The nested
// form compiles on the library compiler, which must not carry the outer hint;
// if it did, the nested library's (import (dep)) would land in the outer
// library's environment and make `depval` visible to the outer body.
func TestNestedDefineLibraryDoesNotShareTheOuterEnv(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"lib/dep.sld": &fstest.MapFile{
			Data: []byte(`(define-library (dep)
  (import (scheme base))
  (export depval)
  (begin (define depval 99)))`),
		},
		"lib/leak.sld": &fstest.MapFile{
			Data: []byte(`(define-library (leak)
  (import (scheme base))
  (export answer)
  (begin
    (define-library (leak-inner)
      (import (dep))
      (export))
    (define answer depval)))`),
		},
	}

	engine, err := newLibraryCountEngine(ctx, fsys)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (leak)) answer`)
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue, qt.Commentf("err: %v", err))
}

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

	"github.com/aalpar/wile"
)

// TestWithSourceFS_Include verifies that (include ...) loads a file from
// a virtual filesystem provided via WithSourceFS.
//
// Note: We use (include ...) rather than (load ...) because load is a
// runtime primitive in the eval extension (internal package, not
// importable from an external test). include exercises the same
// FSFileResolver.ResolveAndOpen path at compile time.
func TestWithSourceFS_Include(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"helper.scm": &fstest.MapFile{
			Data: []byte(`(define helper-val 42)`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(include "helper.scm") helper-val`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

// TestWithSourceFS_NestedInclude verifies that nested includes resolve
// relative paths correctly within the virtual filesystem. main.scm
// includes sub/helper.scm, and definitions from the nested file are
// visible at the top level.
func TestWithSourceFS_NestedInclude(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"main.scm": &fstest.MapFile{
			Data: []byte(`(include "sub/helper.scm")`),
		},
		"sub/helper.scm": &fstest.MapFile{
			Data: []byte(`(define nested-val 99)`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(include "main.scm") nested-val`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "99")
}

// TestWithSourceFS_LibraryImport verifies that the R7RS library system
// can find and import .sld library files from a virtual filesystem.
func TestWithSourceFS_LibraryImport(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"lib/mylib.sld": &fstest.MapFile{
			Data: []byte(`(define-library (mylib)
  (export greet)
  (begin
    (define greet "hello from fs")))`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(import (mylib)) greet`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `"hello from fs"`)
}

// TestWithSourceFS_IncludeInLibrary verifies that (include ...) inside a
// define-library form resolves files relative to the library's directory
// within the virtual filesystem.
func TestWithSourceFS_IncludeInLibrary(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"lib/mylib.sld": &fstest.MapFile{
			Data: []byte(`(define-library (mylib)
  (export compute)
  (include "impl.scm"))`),
		},
		"lib/impl.scm": &fstest.MapFile{
			Data: []byte(`(begin (define compute 777))`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(import (mylib)) compute`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "777")
}

// TestWithSourceFS_IncludeRejectsAbsolutePath verifies that the
// FSFileResolver rejects absolute paths. Absolute paths have no meaning
// in a virtual filesystem.
func TestWithSourceFS_IncludeRejectsAbsolutePath(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"dummy.scm": &fstest.MapFile{
			Data: []byte(`(define x 1)`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(include "/absolute/path.scm")`)
	c.Assert(err, qt.IsNotNil)
}

// TestWithSourceFS_NotSet_UsesOSFilesystem verifies that without
// WithSourceFS, the engine falls back to the OS filesystem. Including a
// nonexistent file should produce an error (proving the OS resolver is
// used, not a nil/missing resolver).
func TestWithSourceFS_NotSet_UsesOSFilesystem(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx, wile.WithSafeExtensions())
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(include "definitely-nonexistent-file.scm")`)
	c.Assert(err, qt.IsNotNil)
}

// TestWithSourceFS_LibraryScmFallback verifies that the library loader
// falls back to .scm when no .sld file exists. This exercises the
// .sld-then-.scm resolution path in LoadLibrary.
func TestWithSourceFS_LibraryScmFallback(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"lib/mylib.scm": &fstest.MapFile{
			Data: []byte(`(define-library (mylib)
  (export val)
  (begin (define val 123)))`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(import (mylib)) val`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "123")
}

// TestWithSourceFS_TransitiveLibraryImport verifies that a library can
// import another library and re-export values. Library (derived) imports
// (base) and defines derived-val in terms of base-val.
func TestWithSourceFS_TransitiveLibraryImport(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"lib/base.sld": &fstest.MapFile{
			Data: []byte(`(define-library (base)
  (export base-val)
  (begin (define base-val 10)))`),
		},
		"lib/derived.sld": &fstest.MapFile{
			Data: []byte(`(define-library (derived)
  (import (base))
  (export derived-val)
  (begin (define derived-val (+ base-val 5))))`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(import (derived)) derived-val`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "15")
}

// TestWithSourceFS_DeeplyNestedInclude verifies that three levels of
// nested includes resolve correctly: a.scm -> sub/b.scm -> sub/deep/c.scm.
// Each include is relative to the including file's directory, not the
// original evaluation root.
func TestWithSourceFS_DeeplyNestedInclude(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"a.scm": &fstest.MapFile{
			Data: []byte(`(include "sub/b.scm")`),
		},
		"sub/b.scm": &fstest.MapFile{
			Data: []byte(`(include "deep/c.scm")`),
		},
		"sub/deep/c.scm": &fstest.MapFile{
			Data: []byte(`(define deep-val 333)`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(include "a.scm") deep-val`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "333")
}

// TestWithSourceFS_IncludeDotDotTraversal verifies that a library's
// (include ...) directive can use ".." to reach files outside its own
// directory. The library at lib/mylib.sld includes ../shared/common.scm,
// which resolves to shared/common.scm in the virtual filesystem.
func TestWithSourceFS_IncludeDotDotTraversal(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"shared/common.scm": &fstest.MapFile{
			Data: []byte(`(define common-val 55)`),
		},
		"lib/mylib.sld": &fstest.MapFile{
			Data: []byte(`(define-library (mylib)
  (export common-val)
  (include "../shared/common.scm"))`),
		},
	}

	engine, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `(import (mylib)) common-val`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "55")
}

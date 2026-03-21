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

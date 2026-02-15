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
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile"

	qt "github.com/frankban/quicktest"
)

func TestWithLibraryPaths(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Create temp dir with a .sld library file
	tmpDir := t.TempDir()
	libDir := filepath.Join(tmpDir, "mylib")
	err := os.MkdirAll(libDir, 0o755)
	c.Assert(err, qt.IsNil)

	sld := `(define-library (mylib greet)
  (export greeting)
  (begin
    (define greeting "hello from library")))`

	err = os.WriteFile(filepath.Join(libDir, "greet.sld"), []byte(sld), 0o644)
	c.Assert(err, qt.IsNil)

	engine, err := wile.NewEngine(ctx, wile.WithLibraryPaths(tmpDir))
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `
		(import (mylib greet))
		greeting
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `"hello from library"`)
}

func TestWithLibraryPaths_DefaultsOnly(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// WithLibraryPaths() with no args enables library support with defaults.
	// We can't easily test that defaults work without placing files in "." or "./lib",
	// so we verify that the engine starts without error and the library system is active
	// (importing a nonexistent library gives a "could not find" error, not a config error).
	engine, err := wile.NewEngine(ctx, wile.WithLibraryPaths())
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (nonexistent lib))`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Not(qt.Contains), "no library registry configured")
}

func TestWithLibraryPaths_NotEnabled(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Without WithLibraryPaths, (import ...) fails with a configuration error.
	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "no library registry configured")
}

func TestWithLibraryPaths_GoFuncsAvailableInLibrary(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Create a library that exports a pure Scheme function
	tmpDir := t.TempDir()

	sld := `(define-library (math-utils)
  (export double)
  (begin
    (define (double x) (* x 2))))`

	err := os.WriteFile(filepath.Join(tmpDir, "math-utils.sld"), []byte(sld), 0o644)
	c.Assert(err, qt.IsNil)

	engine, err := wile.NewEngine(ctx, wile.WithLibraryPaths(tmpDir))
	c.Assert(err, qt.IsNil)

	// Register a Go function on the engine
	err = engine.RegisterFunc("go-add", func(a, b int64) int64 {
		return a + b
	})
	c.Assert(err, qt.IsNil)

	// Compose: import library function + call Go function in engine-level code
	result, err := engine.EvalMultiple(ctx, `
		(import (math-utils))
		(double (go-add 3 7))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "20")
}

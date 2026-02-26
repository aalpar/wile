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

package wile

import (
	"context"
	"errors"
	"os"
	"testing"

	"github.com/aalpar/wile/extensions/math"

	qt "github.com/frankban/quicktest"
)

// TestSafeEngine_RejectsPrivileged verifies that an engine with only safe
// extensions rejects privileged primitives at compile time.
func TestSafeEngine_RejectsPrivileged(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx, WithSafeExtensions())
	c.Assert(err, qt.IsNil)

	// These primitives come from privileged extensions and should not exist.
	privileged := []struct {
		name string
		code string
	}{
		{"open-input-file (files)", `(open-input-file "x")`},
		{"eval (eval)", `(eval '(+ 1 2))`},
		{"exit (system)", `(exit 0)`},
		{"make-channel (gointerop)", `(make-channel 1)`},
		{"load (eval)", `(load "x")`},
		{"delete-file (files)", `(delete-file "x")`},
	}
	for _, tc := range privileged {
		t.Run(tc.name, func(t *testing.T) {
			_, err := engine.Eval(ctx, tc.code)
			var compErr *CompilationError
			c.Assert(errors.As(err, &compErr), qt.IsTrue,
				qt.Commentf("expected CompilationError for %s, got %T: %v", tc.name, err, err))
		})
	}
}

// TestSafeEngine_AllowsSafe verifies that safe primitives work in a
// sandboxed engine.
func TestSafeEngine_AllowsSafe(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx, WithSafeExtensions())
	c.Assert(err, qt.IsNil)

	safe := []struct {
		name string
		code string
		want string
	}{
		// core
		{"+ (core)", "(+ 1 2 3)", "6"},
		{"car (core)", "(car '(1 2 3))", "1"},
		{"vector-ref (core)", "(vector-ref #(10 20 30) 1)", "20"},
		// io
		{"display (io)", `(let ((p (open-output-string))) (display 42 p) (get-output-string p))`, `"42"`},
		// math
		{"sqrt (math)", "(sqrt 4)", "2.0"},
		// exceptions — raise + guard
		{"guard (exceptions)", "(guard (e (#t e)) (raise 42))", "42"},
		// all-safe: records
		{"make-record-type (all-safe)", `(record-type? (make-record-type 'point '(x y)))`, "#t"},
		// all-safe: promises
		{"force (all-safe)", "(force (make-promise 42))", "42"},
	}
	for _, tc := range safe {
		t.Run(tc.name, func(t *testing.T) {
			result, err := engine.Eval(ctx, tc.code)
			c.Assert(err, qt.IsNil, qt.Commentf("code: %s", tc.code))
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestWithoutCore_BareEngine verifies that WithoutCore produces an engine
// where core primitives are absent.
func TestWithoutCore_BareEngine(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx, WithoutCore())
	c.Assert(err, qt.IsNil)

	// Even basic primitives should be unbound.
	bare := []string{
		"(+ 1 2)",
		"(car '(1 2))",
	}
	for _, code := range bare {
		t.Run(code, func(t *testing.T) {
			_, err := engine.Eval(ctx, code)
			var compErr *CompilationError
			c.Assert(errors.As(err, &compErr), qt.IsTrue,
				qt.Commentf("expected CompilationError for %s, got %T: %v", code, err, err))
		})
	}
}

// TestWithoutCore_PlusExtension verifies that WithoutCore + a specific
// extension gives only that extension's primitives.
func TestWithoutCore_PlusExtension(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx, WithoutCore(), WithExtension(math.Extension))
	c.Assert(err, qt.IsNil)

	// math extension primitives should work
	result, err := engine.Eval(ctx, "(sqrt 9)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3.0")

	// core primitives should be absent
	_, err = engine.Eval(ctx, "(+ 1 2)")
	var compErr *CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue)
}

// TestSafeEngine_LibraryPropagation verifies that library environments
// created by a safe engine also lack privileged primitives.
func TestSafeEngine_LibraryPropagation(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Create a temp directory with a library that tries to use open-input-file
	libDir := t.TempDir()
	libFile := libDir + "/bad.sld"
	err := writeTestFile(libFile, `(define-library (bad)
  (export try-open)
  (begin
    (define (try-open) (open-input-file "x"))))`)
	c.Assert(err, qt.IsNil)

	engine, err := NewEngine(ctx,
		WithSafeExtensions(),
		WithLibraryPaths(libDir),
	)
	c.Assert(err, qt.IsNil)

	// Importing and calling the library should fail because open-input-file
	// is not in the restricted engine's registry.
	_, err = engine.EvalMultiple(ctx, `(import (bad)) (try-open)`)
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("expected error from library using privileged primitive"))
}

// writeTestFile is a helper that writes content to a file.
func writeTestFile(path, content string) error {
	return os.WriteFile(path, []byte(content), 0o644)
}

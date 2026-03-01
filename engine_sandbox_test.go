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
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/system"
	eval "github.com/aalpar/wile/internal/extensions/eval"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"

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
		{"sqrt (math)", "(sqrt 4)", "2"},
		// exceptions — raise + guard
		{"guard (exceptions)", "(guard (e (#t e)) (raise 42))", "42"},
		// all-safe: records
		{"make-record-type (all-safe)", `(record-type? (make-record-type 'point '(x y)))`, "#t"},
		// all-safe: promises
		{"force (all-safe)", "(force (make-promise 42))", "42"},
		// introspection
		{"environment? (introspection)", "(environment? 42)", "#f"},
		{"environment-bound? (introspection)", "(environment-bound? (interaction-environment) '+)", "#t"},
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
	c.Assert(result.SchemeString(), qt.Equals, "3")

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

// TestWithout_ImmutableSandbox verifies that Registry.Without can remove
// mutation primitives from a full engine, producing compile-time errors
// for set-car! while leaving car working.
func TestWithout_ImmutableSandbox(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Build a default engine to get its fully populated registry.
	full, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	restricted := full.Registry().Without("set-car!", "set-cdr!")
	engine, err := NewEngine(ctx, WithRegistry(restricted))
	c.Assert(err, qt.IsNil)

	// car still works.
	result, err := engine.Eval(ctx, "(car '(1 2 3))")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "1")

	// set-car! produces a compile error.
	_, err = engine.Eval(ctx, "(set-car! (cons 1 2) 3)")
	var compErr *CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue,
		qt.Commentf("expected CompilationError for set-car!, got %T: %v", err, err))
}

// TestWithoutCategory_RemoveHashtables verifies that WithoutCategory
// removes all primitives in a category.
func TestWithoutCategory_RemoveHashtables(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	full, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	restricted := full.Registry().WithoutCategory("hashtables")
	engine, err := NewEngine(ctx, WithRegistry(restricted))
	c.Assert(err, qt.IsNil)

	// Core primitives still work.
	result, err := engine.Eval(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")

	// Hashtable primitives should be gone.
	_, err = engine.Eval(ctx, "(make-hashtable)")
	var compErr *CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue,
		qt.Commentf("expected CompilationError for make-hashtable, got %T: %v", err, err))
}

// TestImportObserver verifies that the import observer is called with
// correct event data when a library is imported.
func TestImportObserver(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Create a library file
	libDir := t.TempDir()
	err := writeTestFile(libDir+"/mylib.sld", `(define-library (mylib)
  (export greet)
  (begin
    (define (greet) "hello")))`)
	c.Assert(err, qt.IsNil)

	var events []LibraryImportEvent
	engine, err := NewEngine(ctx,
		WithLibraryPaths(libDir),
		WithImportObserver(func(evt LibraryImportEvent) {
			events = append(events, evt)
		}),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (mylib)) (greet)`)
	c.Assert(err, qt.IsNil)

	// Filter to only the mylib event (bootstrap may trigger others)
	var myEvents []LibraryImportEvent
	for _, evt := range events {
		if len(evt.Library) == 1 && evt.Library[0] == "mylib" {
			myEvents = append(myEvents, evt)
		}
	}
	c.Assert(len(myEvents), qt.Equals, 1)

	evt := myEvents[0]
	c.Assert(evt.Library, qt.DeepEquals, []string{"mylib"})
	c.Assert(evt.Exports, qt.DeepEquals, []string{"greet"})
	c.Assert(evt.Imported, qt.DeepEquals, []string{"greet"})
	c.Assert(evt.Importer, qt.IsNil) // top-level import
}

// TestImportObserver_OnlyModifier verifies that the Imported field reflects
// import set modifiers like (only ...).
func TestImportObserver_OnlyModifier(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	libDir := t.TempDir()
	err := writeTestFile(libDir+"/twoexports.sld", `(define-library (twoexports)
  (export alpha beta)
  (begin
    (define (alpha) 1)
    (define (beta) 2)))`)
	c.Assert(err, qt.IsNil)

	var events []LibraryImportEvent
	engine, err := NewEngine(ctx,
		WithLibraryPaths(libDir),
		WithImportObserver(func(evt LibraryImportEvent) {
			events = append(events, evt)
		}),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (only (twoexports) alpha)) (alpha)`)
	c.Assert(err, qt.IsNil)

	var myEvents []LibraryImportEvent
	for _, evt := range events {
		if len(evt.Library) == 1 && evt.Library[0] == "twoexports" {
			myEvents = append(myEvents, evt)
		}
	}
	c.Assert(len(myEvents), qt.Equals, 1)

	evt := myEvents[0]
	c.Assert(evt.Exports, qt.DeepEquals, []string{"alpha", "beta"})
	c.Assert(evt.Imported, qt.DeepEquals, []string{"alpha"}) // only alpha was imported
}

// TestWithAuthorizer_FlowsToContext verifies that WithAuthorizer injects
// the authorizer into the context so security.Check sees it.
func TestWithAuthorizer_FlowsToContext(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	var captured []security.AccessRequest
	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		captured = append(captured, req)
		return nil
	})

	engine, err := NewEngine(ctx,
		WithAuthorizer(auth),
		WithExtension(files.Extension),
		WithExtension(system.Extension),
	)
	c.Assert(err, qt.IsNil)

	// file-exists? should trigger a file/stat check once the primitives
	// are gated (Phase 4). For now, verify the authorizer is reachable
	// from the context by checking it via security.FromContext inside
	// a Go primitive.
	engine.RegisterPrimitive(PrimitiveSpec{
		Name:       "test-auth-check",
		ParamCount: 0,
		Impl: func(mc *MachineContext) error {
			err := security.Check(mc.Context(), security.AccessRequest{
				Resource: security.ResourceFile,
				Action:   security.ActionRead,
				Target:   "/test/path",
			})
			if err != nil {
				return err
			}
			mc.SetValue(values.TrueValue)
			return nil
		},
	})

	result, err := engine.Eval(ctx, "(test-auth-check)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
	c.Assert(len(captured), qt.Equals, 1)
	c.Assert(captured[0].Target, qt.Equals, "/test/path")
}

// TestWithAuthorizer_DenyBlocksEval verifies that a denying authorizer
// causes security.Check to return ErrAccessDenied.
func TestWithAuthorizer_DenyBlocksEval(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithAuthorizer(security.DenyAll()),
	)
	c.Assert(err, qt.IsNil)

	engine.RegisterPrimitive(PrimitiveSpec{
		Name:       "test-auth-deny",
		ParamCount: 0,
		Impl: func(mc *MachineContext) error {
			return security.Check(mc.Context(), security.AccessRequest{
				Resource: security.ResourceFile,
				Action:   security.ActionWrite,
				Target:   "/secret",
			})
		},
	})

	_, err = engine.Eval(ctx, "(test-auth-deny)")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

// TestNoAuthorizer_AllowsByDefault verifies that without WithAuthorizer,
// security.Check allows everything.
func TestNoAuthorizer_AllowsByDefault(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	engine.RegisterPrimitive(PrimitiveSpec{
		Name:       "test-auth-open",
		ParamCount: 0,
		Impl: func(mc *MachineContext) error {
			err := security.Check(mc.Context(), security.AccessRequest{
				Resource: security.ResourceFile,
				Action:   security.ActionWrite,
				Target:   "/anything",
			})
			if err != nil {
				return err
			}
			mc.SetValue(values.TrueValue)
			return nil
		},
	})

	result, err := engine.Eval(ctx, "(test-auth-open)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

// ---------------------------------------------------------------------------
// Phase 4-6 integration tests: security.Check gates on real primitives
// ---------------------------------------------------------------------------

func TestAuthorizer_DenyBlocksFileRead(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithAuthorizer(security.DenyAll()),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(file-exists? "/tmp/x")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_DenyBlocksFileWrite(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithAuthorizer(security.DenyAll()),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(open-output-file "/tmp/x")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_DenyBlocksDelete(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithAuthorizer(security.DenyAll()),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(delete-file "/tmp/x")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_ReadOnlyAllowsStat(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	dir := t.TempDir()

	engine, err := NewEngine(ctx,
		WithAuthorizer(security.ReadOnly()),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(ctx, fmt.Sprintf(`(file-exists? %q)`, dir))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

func TestAuthorizer_ReadOnlyDeniesWrite(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithAuthorizer(security.ReadOnly()),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(open-output-file "/tmp/x")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_DenyBlocksEnvVar(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceEnv {
			return security.ErrAccessDenied
		}
		return nil
	})
	engine, err := NewEngine(ctx,
		WithAuthorizer(auth),
		WithExtension(system.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(get-environment-variable "PATH")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_DenyBlocksExit(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceProcess {
			return security.ErrAccessDenied
		}
		return nil
	})
	engine, err := NewEngine(ctx,
		WithAuthorizer(auth),
		WithExtension(system.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(exit)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_DenyBlocksLoad(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceCode {
			return security.ErrAccessDenied
		}
		return nil
	})

	dir := t.TempDir()
	scmFile := filepath.Join(dir, "file.scm")
	err := writeTestFile(scmFile, `(define x 42)`)
	c.Assert(err, qt.IsNil)

	engine, err := NewEngine(ctx,
		WithAuthorizer(auth),
		WithExtension(eval.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, fmt.Sprintf(`(load %q)`, scmFile))
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_DenyBlocksImport(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	dir := t.TempDir()
	err := writeTestFile(filepath.Join(dir, "testlib.sld"), `(define-library (testlib)
  (export val)
  (begin (define val 99)))`)
	c.Assert(err, qt.IsNil)

	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceCode {
			return security.ErrAccessDenied
		}
		return nil
	})
	engine, err := NewEngine(ctx,
		WithAuthorizer(auth),
		WithLibraryPaths(dir),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (testlib))`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_FilesystemRootAllowsInside(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	dir := t.TempDir()
	txtFile := filepath.Join(dir, "hello.txt")
	err := writeTestFile(txtFile, "hello")
	c.Assert(err, qt.IsNil)

	engine, err := NewEngine(ctx,
		WithAuthorizer(security.FilesystemRoot(dir)),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(ctx, fmt.Sprintf(`(file-exists? %q)`, txtFile))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

func TestAuthorizer_FilesystemRootDeniesOutside(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	dir := t.TempDir()
	engine, err := NewEngine(ctx,
		WithAuthorizer(security.FilesystemRoot(dir)),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, `(file-exists? "/etc/passwd")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestAuthorizer_SelectivePolicy(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	dir := t.TempDir()
	dataFile := filepath.Join(dir, "data.txt")
	err := writeTestFile(dataFile, "data")
	c.Assert(err, qt.IsNil)

	// Allow reads, deny writes
	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		switch req.Action {
		case security.ActionRead, security.ActionStat, security.ActionLoad:
			return nil
		default:
			return security.ErrAccessDenied
		}
	})
	engine, err := NewEngine(ctx,
		WithAuthorizer(auth),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	// Read should succeed
	result, err := engine.Eval(ctx, fmt.Sprintf(`(file-exists? %q)`, dataFile))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")

	// Write should fail
	_, err = engine.Eval(ctx, fmt.Sprintf(`(open-output-file %q)`, filepath.Join(dir, "out.txt")))
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

// writeTestFile is a helper that writes content to a file.
func writeTestFile(path, content string) error {
	return os.WriteFile(path, []byte(content), 0o644)
}

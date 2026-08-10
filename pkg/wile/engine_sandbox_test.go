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
	"slices"
	"testing"

	eval "github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/pkg/internal/extensions/envvars"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestConsole_RejectsPrivileged verifies that the Console profile rejects
// privileged primitives. Extensions not included in Console (eval, system,
// gointerop) produce compile-time errors for their primitives. Files extension
// IS included under Console but the ConsoleAuthorizer restricts file
// operations to /tmp, so reads/writes outside /tmp fail at runtime with
// ErrAccessDenied.
func TestConsole_RejectsPrivileged(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx, WithProfile(Console))
	c.Assert(err, qt.IsNil)

	// Compile-time rejection: primitives from extensions absent in Console.
	compileTimeRejected := []struct {
		name string
		code string
	}{
		{"eval (eval)", `(eval '(+ 1 2))`},
		{"exit (system)", `(exit 0)`},
		{"make-atomic (gointerop)", `(make-atomic 0)`},
		{"load (eval)", `(load "x")`},
	}
	for _, tc := range compileTimeRejected {
		t.Run(tc.name, func(t *testing.T) {
			_, err := engine.Eval(ctx, engine.MustParse(ctx, tc.code))
			var compErr *CompilationError
			c.Assert(errors.As(err, &compErr), qt.IsTrue,
				qt.Commentf("expected CompilationError for %s, got %T: %v", tc.name, err, err))
		})
	}

	// Runtime rejection: files extension IS registered, but operations
	// outside /tmp are denied by ConsoleAuthorizer.
	runtimeDenied := []struct {
		name string
		code string
	}{
		{"open-input-file outside /tmp", `(open-input-file "/etc/passwd")`},
		{"delete-file outside /tmp", `(delete-file "/etc/passwd")`},
	}
	for _, tc := range runtimeDenied {
		t.Run(tc.name, func(t *testing.T) {
			_, err := engine.Eval(ctx, engine.MustParse(ctx, tc.code))
			c.Assert(err, qt.IsNotNil, qt.Commentf("expected error for %s", tc.name))
			c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
				qt.Commentf("expected ErrAccessDenied for %s, got: %v", tc.name, err))
		})
	}
}

// TestConsole_AllowsSafe verifies that safe primitives work in the
// Console profile.
func TestConsole_AllowsSafe(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx, WithProfile(Console))
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
	}
	for _, tc := range safe {
		t.Run(tc.name, func(t *testing.T) {
			result, err := engine.Eval(ctx, engine.MustParse(ctx, tc.code))
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
			_, err := engine.Eval(ctx, engine.MustParse(ctx, code))
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
	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(sqrt 9)"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")

	// core primitives should be absent
	_, err = engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2)"))
	var compErr *CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue)
}

// TestConsole_LibraryPropagation verifies that library environments
// created by a Console-profile engine inherit the authorizer — so libraries
// that attempt privileged operations are still blocked at runtime.
func TestConsole_LibraryPropagation(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Create a temp directory with a library that tries to use eval,
	// which is absent from the Console profile entirely.
	libDir := t.TempDir()
	libFile := libDir + "/bad.sld"
	err := writeTestFile(libFile, `(define-library (bad)
  (export try-eval)
  (begin
    (define (try-eval) (eval '(+ 1 2)))))`)
	c.Assert(err, qt.IsNil)

	engine, err := NewEngine(ctx,
		WithProfile(Console),
		WithLibraryPaths(libDir),
	)
	c.Assert(err, qt.IsNil)

	// Importing and calling the library should fail because eval is not
	// in the Console-profile registry.
	_, err = engine.EvalMultiple(ctx, `(import (bad)) (try-eval)`)
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("expected error from library using privileged primitive"))
}

// TestWithout_RemovesMutationPrimitives verifies that Registry.Without can
// remove mutation primitives from a full engine, producing compile-time errors
// for set-car! while leaving car working.
//
// Despite this file's name, removal is a statement about the language surface,
// not an enforcement boundary: it narrows what the engine can name, and nothing
// stops a Go embedder from handing the same capability back. Sandbox
// enforcement is the authorizer's job (security.Check gate sites).
func TestWithout_RemovesMutationPrimitives(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Build a default engine to get its fully populated registry.
	full, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	restricted := full.Registry().Without("set-car!", "set-cdr!")
	engine, err := NewEngine(ctx, WithRegistry(restricted))
	c.Assert(err, qt.IsNil)

	// car still works.
	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(car '(1 2 3))"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "1")

	// set-car! produces a compile error.
	_, err = engine.Eval(ctx, engine.MustParse(ctx, "(set-car! (cons 1 2) 3)"))
	var compErr *CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue,
		qt.Commentf("expected CompilationError for set-car!, got %T: %v", err, err))
}

// TestWithoutCategory_RemoveHashtables verifies that WithoutCategory
// removes all primitives in a category.
//
// The category has a bootstrap dependent — hashtable-update!, whose body is
// (hashtable-set! ...) — and WithoutCategory drops it automatically, because the
// source is registered with AddProcedureSource(src, "hashtables"). An earlier
// cut of this work left the caller to do that themselves and this test performed
// the workaround inline; that was the test accommodating a regression, since
// removing the category worked on master and had stopped working here.
//
// Sources that do NOT declare a dependency are unaffected, which is why
// WithoutCategory still fails for "vectors", "strings", "pairs" and "lists":
// their dependents are inside the main bootstrap source and are not separable at
// all. Declaring a dependency is what makes a category removable.
func TestWithoutCategory_RemoveHashtables(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	full, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	before := len(full.Registry().ProcedureSources())
	restricted := full.Registry().WithoutCategory("hashtables")
	c.Assert(len(restricted.ProcedureSources()), qt.Equals, before-1,
		qt.Commentf("WithoutCategory must drop HashtableUpdateSource with the category"))
	c.Assert(slices.Contains(restricted.ProcedureSources(), core.HashtableUpdateSource), qt.IsFalse)

	engine, err := NewEngine(ctx, WithRegistry(restricted))
	c.Assert(err, qt.IsNil)

	// Core primitives still work.
	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2)"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")

	// Hashtable primitives should be gone.
	_, err = engine.Eval(ctx, engine.MustParse(ctx, "(make-equal-hashtable)"))
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
		if evt.Library.Key() == "mylib" {
			myEvents = append(myEvents, evt)
		}
	}
	// The observer fires twice per top-level import: once during expansion
	// (to make macros available) and once during compilation. Both events
	// carry identical data because LoadLibrary caches the library.
	c.Assert(len(myEvents), qt.Equals, 2)

	for _, evt := range myEvents {
		c.Assert(evt.Library.Parts, qt.DeepEquals, []string{"mylib"})
		c.Assert(evt.Exports, qt.DeepEquals, []string{"greet"})
		c.Assert(evt.Imported, qt.DeepEquals, []string{"greet"})
		c.Assert(evt.Importer.Parts, qt.IsNil) // top-level import
	}
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
		if evt.Library.Key() == "twoexports" {
			myEvents = append(myEvents, evt)
		}
	}
	// Observer fires at both expand and compile phases for top-level imports
	c.Assert(len(myEvents), qt.Equals, 2)
	c.Assert(myEvents[0].Phase, qt.Equals, PhaseExpand)
	c.Assert(myEvents[1].Phase, qt.Equals, PhaseCompile)

	for _, evt := range myEvents {
		c.Assert(evt.Exports, qt.DeepEquals, []string{"alpha", "beta"})
		c.Assert(evt.Imported, qt.DeepEquals, []string{"alpha"}) // only alpha was imported
	}
}

// TestImportRegistersDocstrings verifies that importing a library with
// structured docstrings makes those procedures visible to the documentation
// system (,doc, ,topic, ,apropos).
func TestImportRegistersDocstrings(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	libDir := t.TempDir()
	err := writeTestFile(libDir+"/doclib.sld", `(define-library (doclib)
  (export find-thing)
  (begin
    (define (find-thing pred ls)
      "Return the first element of LS satisfying PRED.

Parameters:
  pred : procedure
  ls : list
Returns: any
Category: search"
      (cond ((null? ls) #f)
            ((pred (car ls)) (car ls))
            (else (find-thing pred (cdr ls)))))))`)
	c.Assert(err, qt.IsNil)

	engine, err := NewEngine(ctx, WithLibraryPaths(libDir))
	c.Assert(err, qt.IsNil)

	// Before import: find-thing should not be in the registry
	regBefore := engine.Registry()
	_, found := regBefore.FindPrimitive("find-thing", 0)
	c.Assert(found, qt.IsFalse, qt.Commentf("find-thing should not be visible before import"))

	// Import the library
	_, err = engine.EvalMultiple(ctx, `(import (doclib))`)
	c.Assert(err, qt.IsNil)

	// After import: find-thing should be in the registry with correct metadata
	reg := engine.Registry()
	pr, found := reg.FindPrimitive("find-thing", 0)
	c.Assert(found, qt.IsTrue, qt.Commentf("find-thing should be visible after import"))
	c.Assert(pr.Spec.Category, qt.Equals, "search")
	c.Assert(pr.Spec.ParamNames, qt.DeepEquals, []string{"pred", "ls"})

	// The category should appear in PrimitivesByCategory
	byCategory := reg.PrimitivesByCategory()
	searchPrims := byCategory["search"]
	c.Assert(len(searchPrims) > 0, qt.IsTrue, qt.Commentf("search category should exist"))

	var names []string
	for _, p := range searchPrims {
		names = append(names, p.Spec.Name)
	}
	c.Assert(slices.Contains(names, "find-thing"), qt.IsTrue)
}

// TestImportRegistersDocstrings_UnexportedNotVisible verifies that
// unexported bindings from imported libraries do NOT appear in docs.
func TestImportRegistersDocstrings_UnexportedNotVisible(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	libDir := t.TempDir()
	err := writeTestFile(libDir+"/partial.sld", `(define-library (partial)
  (export public-fn)
  (begin
    (define (helper x)
      "Internal helper.

Parameters:
  x : any
Category: internal"
      x)
    (define (public-fn x)
      "Public function.

Parameters:
  x : any
Category: mylib"
      (helper x))))`)
	c.Assert(err, qt.IsNil)

	engine, err := NewEngine(ctx, WithLibraryPaths(libDir))
	c.Assert(err, qt.IsNil)

	// Before import: neither should be visible
	regBefore := engine.Registry()
	_, found := regBefore.FindPrimitive("public-fn", 0)
	c.Assert(found, qt.IsFalse)

	_, err = engine.EvalMultiple(ctx, `(import (partial))`)
	c.Assert(err, qt.IsNil)

	reg := engine.Registry()

	// Exported: visible
	_, found = reg.FindPrimitive("public-fn", 0)
	c.Assert(found, qt.IsTrue)

	// Unexported: NOT visible
	_, found = reg.FindPrimitive("helper", 0)
	c.Assert(found, qt.IsFalse, qt.Commentf("unexported helper should not be in docs"))
}

// TestImportRegistersDocstrings_ChainsUserObserver verifies that the
// doc registration observer chains with a user-provided import observer.
func TestImportRegistersDocstrings_ChainsUserObserver(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	libDir := t.TempDir()
	err := writeTestFile(libDir+"/chainlib.sld", `(define-library (chainlib)
  (export thing)
  (begin
    (define (thing)
      "A thing.

Category: widgets"
      42)))`)
	c.Assert(err, qt.IsNil)

	var observerCalled bool
	engine, err := NewEngine(ctx,
		WithLibraryPaths(libDir),
		WithImportObserver(func(evt LibraryImportEvent) {
			if evt.Library.Key() == "chainlib" {
				observerCalled = true
			}
		}),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (chainlib))`)
	c.Assert(err, qt.IsNil)

	// User observer was called
	c.Assert(observerCalled, qt.IsTrue, qt.Commentf("user import observer should still fire"))

	// Doc registration also happened
	reg := engine.Registry()
	pr, found := reg.FindPrimitive("thing", 0)
	c.Assert(found, qt.IsTrue)
	c.Assert(pr.Spec.Category, qt.Equals, "widgets")
}

// TestWithAuthorizer_FlowsToNamespace verifies that WithAuthorizer stores
// the authorizer on the namespace so mc.Authorizer() sees it.
func TestWithAuthorizer_FlowsToNamespace(t *testing.T) {
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

	engine.RegisterPrimitive(PrimitiveSpec{
		Name:       "test-auth-check",
		ParamCount: 0,
		Impl: func(mc CallContext) error {
			err := security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
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

	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(test-auth-check)"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
	c.Assert(len(captured), qt.Equals, 1)
	c.Assert(captured[0].Target, qt.Equals, "/test/path")
}

// TestWithAuthorizer_DenyBlocksEval verifies that a denying authorizer
// causes security.CheckWithAuthorizer to return ErrAccessDenied.
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
		Impl: func(mc CallContext) error {
			return security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
				Resource: security.ResourceFile,
				Action:   security.ActionWrite,
				Target:   "/secret",
			})
		},
	})

	_, err = engine.Eval(ctx, engine.MustParse(ctx, "(test-auth-deny)"))
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
		Impl: func(mc CallContext) error {
			err := security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
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

	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(test-auth-open)"))
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

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(file-exists? "/tmp/x")`))
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

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(open-output-file "/tmp/x")`))
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

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(delete-file "/tmp/x")`))
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

	result, err := engine.Eval(ctx, engine.MustParse(ctx, fmt.Sprintf(`(file-exists? %q)`, dir)))
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

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(open-output-file "/tmp/x")`))
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
		WithExtension(envvars.Extension),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(get-environment-variable "PATH")`))
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

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(exit)`))
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

	_, err = engine.Eval(ctx, engine.MustParse(ctx, fmt.Sprintf(`(load %q)`, scmFile)))
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

	result, err := engine.Eval(ctx, engine.MustParse(ctx, fmt.Sprintf(`(file-exists? %q)`, txtFile)))
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

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(file-exists? "/etc/passwd")`))
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
	result, err := engine.Eval(ctx, engine.MustParse(ctx, fmt.Sprintf(`(file-exists? %q)`, dataFile)))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")

	// Write should fail
	_, err = engine.Eval(ctx, engine.MustParse(ctx, fmt.Sprintf(`(open-output-file %q)`, filepath.Join(dir, "out.txt"))))
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

// writeTestFile is a helper that writes content to a file.
func writeTestFile(path, content string) error {
	return os.WriteFile(path, []byte(content), 0o644)
}

// TestAuthorizer_DenyBlocksCommandLine verifies that (command-line) is gated
// symmetrically with the other system primitives: under an authorizer that
// denies ResourceProcess, reading the host argv must raise ErrAccessDenied
// rather than leaking os.Args.
func TestAuthorizer_DenyBlocksCommandLine(t *testing.T) {
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
	defer engine.Close()

	_, err = engine.Eval(ctx, engine.MustParse(ctx, `(command-line)`))
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

// TestAuthorizer_DenyAllSweep runs a representative privileged primitive for each
// authorizer gate under DenyAll and asserts each raises ErrAccessDenied. This is
// the regression net for gate coverage: a primitive that reads a host resource
// without an authorizer check (as (command-line) once did) shows up here as a nil
// error. One KitchenSink engine registers every extension; WithAuthorizer(DenyAll())
// overrides the profile's (nil) authorizer. Each expression is shaped so the
// authorizer gate is the FIRST failure (valid arg count and types) — a missing gate
// surfaces as a nil error, not an unrelated arg/type error. exit's gate fires before
// os.Exit, so invoking it in-process is safe.
//
// The sweep is representative, not exhaustive: it covers one primitive per gate
// action (process:read/exit/exec/exec-shell, env:read, file:stat/write/delete,
// code:eval), PLUS one per separately-reachable gate SITE, not every gated
// primitive — e.g. open-input-file shares file:read/stat with the file-exists?
// row, and current-directory is uncovered. A new gate action should add a row
// here, and so should a new gate site on an existing action.
//
// The second clause is load-bearing and was added when expand/expand-once were
// gated: they share code:eval with the eval row, so under the action-only
// charter this sweep would never have grown a row for them, and a regression
// that dropped their gate would have passed. They are two rows and not one used
// as a proxy for the other, because they reach their gate by different paths —
// expand is InvokesProcedure and dispatches through the recovered
// OperationForeignFunctionCall path, expand-once is a leaf call through
// callForeignCached.
//
// The sweep also does NOT cover stream:{read,write}. That gate runs once per
// engine, when io.NewState builds the port parameters, so a denial surfaces as a
// closed current-{input,output,error}-port rather than as an ErrAccessDenied
// from a primitive — the shape every row here asserts. TestHostStdioIsGated
// covers it in the shape it actually has.
//
// The sweep deliberately does NOT cover (environment '(wile kitchen-sink)).
// That constructor is now gated, but on containment rather than on a per-action
// denial, so it does not belong in an action sweep: the question it answers is
// "does this profile widen the engine's surface?", and the hazard it closes is
// an extension with no gate sites at all (threads, gointerop), which no row here
// could ever have caught. See TestProfileEnvironmentCannotWidenSandboxedEngine
// below, and checkProfileWidening in pkg/internal/bootstrap.
func TestAuthorizer_DenyAllSweep(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithAuthorizer(security.DenyAll()),
	)
	c.Assert(err, qt.IsNil)
	defer engine.Close()

	privileged := []struct {
		name string
		code string
	}{
		{"command-line (system, process:read)", `(command-line)`},
		{"exit (system, process:exit)", `(exit)`},
		{"get-environment-variable (envvars, env:read)", `(get-environment-variable "PATH")`},
		{"file-exists? (files, file:stat)", `(file-exists? "/tmp/x")`},
		{"open-output-file (files, file:write)", `(open-output-file "/tmp/x")`},
		{"delete-file (files, file:delete)", `(delete-file "/tmp/x")`},
		{"eval (eval, code:eval)", `(eval '(+ 1 2))`},
		{"expand (eval, code:eval — separate gate site)", `(expand (datum->syntax #f '(+ 1 2)))`},
		{"expand-once (eval, code:eval — separate gate site)", `(expand-once (datum->syntax #f '(+ 1 2)))`},
		{"system (process, process:exec-shell)", `(system "true")`},
		{"process-spawn (process, process:exec)", `(process-spawn "true")`},
	}
	for _, tc := range privileged {
		t.Run(tc.name, func(t *testing.T) {
			_, err := engine.Eval(ctx, engine.MustParse(ctx, tc.code))
			c.Assert(err, qt.IsNotNil, qt.Commentf("expected denial for %s", tc.code))
			c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
				qt.Commentf("expected ErrAccessDenied for %s, got: %v", tc.code, err))
		})
	}
}

// TestProfileEnvironmentCannotWidenSandboxedEngine is the end-to-end form of the
// containment rule: through a real Engine, from Scheme, a sandboxed engine
// cannot acquire an ungated extension by naming a richer profile.
//
// The hazard is specific and is why an authorizer alone was not enough.
// (environment '(wile kitchen-sink)) builds the child via NewChildNamespace,
// which copies the authorizer, so every GATED resource stays under the engine's
// policy. But threads, gointerop and namespace declare no security.Check sites,
// so their primitives were reachable from an engine that never registered them,
// and no policy was ever consulted — an authorizer cannot refuse what it is not
// asked about.
//
// Each arm states which half of the policy it pins, so a future edit that
// collapses them is visibly changing the contract rather than tidying tests.
func TestProfileEnvironmentCannotWidenSandboxedEngine(t *testing.T) {
	ctx := context.Background()

	t.Run("sandboxed engine is refused a widening profile", func(t *testing.T) {
		c := qt.New(t)
		engine, err := NewEngine(ctx, WithProfile(ConsoleWithLoad))
		c.Assert(err, qt.IsNil)
		defer engine.Close()

		_, err = engine.Eval(ctx, engine.MustParse(ctx, `(environment '(wile kitchen-sink))`))
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
			qt.Commentf("expected the widening to be refused, got: %v", err))
	})

	t.Run("sandboxed engine may still construct its own profile", func(t *testing.T) {
		c := qt.New(t)
		engine, err := NewEngine(ctx, WithProfile(ConsoleWithLoad))
		c.Assert(err, qt.IsNil)
		defer engine.Close()

		_, err = engine.Eval(ctx, engine.MustParse(ctx, `(environment '(wile console-with-load))`))
		c.Assert(err, qt.IsNil, qt.Commentf("a contained profile must not be newly denied"))
	})

	t.Run("engine without an authorizer keeps the documented widening path", func(t *testing.T) {
		c := qt.New(t)
		engine, err := NewEngine(ctx, WithProfile(KitchenSink))
		c.Assert(err, qt.IsNil)
		defer engine.Close()

		_, err = engine.Eval(ctx, engine.MustParse(ctx, `(environment '(wile kitchen-sink))`))
		c.Assert(err, qt.IsNil)
	})
}

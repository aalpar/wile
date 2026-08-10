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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

func newEngineWithStdlib(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	// These tests cover R7RS-permissive top-level mutation (define-supersede-import,
	// top-level set!) which is the opt-out behavior under the immutable default. Imported
	// bindings stay Stable via the import mechanism regardless of this flag.
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
		wile.WithMutableTopLevel(),
	)
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestSetBangOnImportedBindingRejected verifies that set! on an imported
// binding produces a compilation error wrapping ErrImmutableBinding.
func TestSetBangOnImportedBindingRejected(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngineWithStdlib(t)

	_, err := eng.EvalMultiple(ctx, `(import (scheme base)) (set! cons 42)`)
	c.Assert(err, qt.IsNotNil)

	// The error chain is: CompilationError → SourcedError → ForeignError → ErrImmutableBinding.
	// errors.Is traverses the full Unwrap chain.
	c.Assert(errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("expected ErrImmutableBinding in chain, got: %v", err))

	// Also verify it is a CompilationError (compile-time rejection, not runtime).
	var compErr *wile.CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue,
		qt.Commentf("expected CompilationError, got %T: %v", err, err))
}

// TestSetBangOnLocalDefineAllowed verifies that set! on a user-defined
// binding works normally.
func TestSetBangOnLocalDefineAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngineWithStdlib(t)

	result, err := eng.EvalMultiple(ctx, `(import (scheme base)) (define x 1) (set! x 2) x`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "2")
}

// TestDefineThenSetBangOnImportAllowed verifies that top-level define
// supersedes an imported binding, clearing the Imported flag so that
// a subsequent set! on the same binding succeeds (R7RS §5.3.1).
func TestDefineThenSetBangOnImportAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngineWithStdlib(t)

	result, err := eng.EvalMultiple(ctx,
		`(import (scheme base)) (define cons 99) (set! cons 42) cons`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

// TestSetBangOnShadowedImportAllowed verifies that a lexical binding
// shadows the imported binding, and set! on the shadow succeeds.
func TestSetBangOnShadowedImportAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngineWithStdlib(t)

	result, err := eng.EvalMultiple(ctx,
		`(import (scheme base)) (let ((cons 42)) (set! cons 99) cons)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "99")
}

// TestDefineSyntaxSupersedesImportClearsImported pins the syntax half of the
// R7RS §5.3.1 supersede rule. A top-level define-syntax over an imported macro
// reuses the import's (1, mutable) slot and writes the user's transformer, so
// the import *provenance* has to go with it — exactly as the variable path
// already does in compile_define.go.
//
// Before the fix the syntax path touched only m.Doc, so imported stayed true.
// IsStable() is `m.Imported || m.Stable`, which meant validate.classifyCallee
// and the frame-reclaim classifier were told "cannot be rebound" about a
// binding the user had just rebound.
//
// The pointer equality is the non-vacuity guard: it distinguishes supersede
// in place from a second slot that merely outranks the first, which would make
// the flag assertions pass for the wrong reason.
func TestDefineSyntaxSupersedesImportClearsImported(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngineWithStdlib(t)

	_, err := eng.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(err, qt.IsNil)

	sym := values.NewSymbol("when")
	before := eng.Namespace().Expand().GetBinding(sym, values.AllScopes())
	c.Assert(before, qt.IsNotNil, qt.Commentf("when should be bound at phase 1 after import"))
	c.Assert(before.IsImported(), qt.IsTrue,
		qt.Commentf("premise: the import outranks bootstrap's sealed when"))

	_, err = eng.EvalMultiple(ctx,
		`(define-syntax when (syntax-rules () ((_ x) (quote user-when))))`)
	c.Assert(err, qt.IsNil)

	after := eng.Namespace().Expand().GetBinding(sym, values.AllScopes())
	c.Assert(after, qt.IsNotNil)
	c.Assert(after, qt.Equals, before,
		qt.Commentf("define-syntax must supersede the import in place, not shadow it"))
	c.Assert(after.IsImported(), qt.IsFalse,
		qt.Commentf("define-syntax supersedes an import, so the import provenance is dropped (R7RS §5.3.1)"))
	c.Assert(after.IsStable(), qt.IsFalse,
		qt.Commentf("a binding the user just rebound is not stable"))

	result, err := eng.EvalMultiple(ctx, `(when 1)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "user-when")
}

// TestImportedBindingStableFlag verifies that after importing (scheme base),
// the binding for "cons" has both IsImported and IsStable flags set.
func TestImportedBindingStableFlag(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngineWithStdlib(t)

	_, err := eng.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(err, qt.IsNil)

	env := eng.Environment()
	gi := environment.NewGlobalIndex(values.NewSymbol("cons"))
	binding := env.GetGlobalBinding(gi)
	c.Assert(binding, qt.IsNotNil, qt.Commentf("cons should be bound after import"))
	c.Assert(binding.IsImported(), qt.IsTrue, qt.Commentf("cons should be marked imported"))
	c.Assert(binding.IsStable(), qt.IsTrue, qt.Commentf("cons should be marked stable"))
}

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

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/stdlib"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func newEngineWithStdlib(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
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

// TestSetBangOnShadowedImportAllowed verifies that a lexical binding
// shadows the imported binding, and set! on the shadow succeeds.
// Top-level (define cons ...) reuses the existing imported binding, so
// true shadowing requires a new lexical scope via let.
func TestSetBangOnShadowedImportAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngineWithStdlib(t)

	result, err := eng.EvalMultiple(ctx,
		`(import (scheme base)) (let ((cons 42)) (set! cons 99) cons)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "99")
}

// TestImportedBindingConstantFlag verifies that after importing (scheme base),
// the binding for "cons" has both IsImported and IsConstant flags set.
func TestImportedBindingConstantFlag(t *testing.T) {
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
	c.Assert(binding.IsConstant(), qt.IsTrue, qt.Commentf("cons should be marked constant"))
}

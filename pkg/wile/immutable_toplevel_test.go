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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func newImmutableTopLevelEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(), wile.WithImmutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// --- Flag OFF (default): strict R7RS, status quo preserved ---

func TestImmutableTopLevel_OffByDefault_CrossFormSetBangAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	// Two separate top-level forms (two compilation units). With the option off,
	// f never becomes Stable, so the later set! is permitted.
	result, err := eng.EvalMultiple(ctx, `(define f 5) (set! f 6) f`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "6")
}

func TestImmutableTopLevel_OffByDefault_RedefineAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx, `(define f 5) (define f 6) f`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "6")
}

// --- Flag ON: Option B enforcement ---

func TestImmutableTopLevel_On_CrossFormSetBangRejected(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	// Unit 1 marks f Stable (defined-once, never set! in its unit); unit 2's
	// set! is then rejected at compile time.
	_, err := eng.EvalMultiple(ctx, `(define f 5) (set! f 6)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("expected ErrImmutableBinding, got: %v", err))

	var compErr *wile.CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue,
		qt.Commentf("expected compile-time rejection, got %T: %v", err, err))
}

func TestImmutableTopLevel_On_RedefineRejected(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	_, err := eng.EvalMultiple(ctx, `(define f 5) (define f 6)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("expected ErrImmutableBinding on redefine, got: %v", err))
}

func TestImmutableTopLevel_On_InUnitSetBangAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	// One compilation unit: f is set! within the unit, so the validator marks it
	// non-stable and the set! is permitted. "Mutable when set! in its own unit."
	result, err := eng.EvalMultiple(ctx, `(begin (define f 5) (set! f 6) f)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "6")
}

func TestImmutableTopLevel_On_StableFlagReflectsInUnitMutation(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	// A clean define is Stable; a define set! in its own unit is not.
	_, err := eng.EvalMultiple(ctx, `(define clean 5) (begin (define dirty 5) (set! dirty 6))`)
	c.Assert(err, qt.IsNil)

	env := eng.Environment()
	clean := env.GetGlobalBinding(environment.NewGlobalIndex(values.NewSymbol("clean")))
	c.Assert(clean, qt.IsNotNil)
	c.Assert(clean.IsStable(), qt.IsTrue, qt.Commentf("never-set! define must be Stable"))

	dirty := env.GetGlobalBinding(environment.NewGlobalIndex(values.NewSymbol("dirty")))
	c.Assert(dirty, qt.IsNotNil)
	c.Assert(dirty.IsStable(), qt.IsFalse, qt.Commentf("set!-in-unit define must not be Stable"))
}

func TestImmutableTopLevel_On_LocalDefineUnaffected(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	// An internal define + set! of a local binding is unaffected by top-level
	// immutability (only top-level/global defines are stamped Stable).
	result, err := eng.EvalMultiple(ctx,
		`(define (proc) (define h 1) (set! h 2) h) (proc)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "2")
}

// TestImmutableTopLevel_On_DefineValuesCovered confirms that define-values —
// which desugars to (begin (define ...) ...) — routes through the same guarded
// define path, so the binding it creates becomes Stable and a later set! is
// rejected. (define-values is a core bootstrap macro; no import needed.)
func TestImmutableTopLevel_On_DefineValuesCovered(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	// Single-value define-values expands to a clean (define x ...) → x is Stable;
	// a separate set! is then rejected.
	_, err := eng.EvalMultiple(ctx, `(define-values (x) 5) (set! x 6)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("set! on a define-values binding must be rejected, got: %v", err))
}

// TestImmutableTopLevel_On_DefineValuesMultiStillWorks confirms the flag does
// NOT break define-values' own internal set!. The multi-var expansion does
// (define var0 ...) ... (set! var0 ...) within one unit, so var0 is set! in its
// own unit → non-stable → the internal set! is permitted.
func TestImmutableTopLevel_On_DefineValuesMultiStillWorks(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	result, err := eng.EvalMultiple(ctx, `(define-values (p q) (values 1 2)) (+ p q)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

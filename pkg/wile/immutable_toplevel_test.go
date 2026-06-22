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

func newImmutableTopLevelEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(), wile.WithImmutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// newImmutableTopLevelLibraryEngine builds an immutable-top-level engine that can
// resolve (import ...) against the embedded stdlib, so library-load paths can be
// exercised under immutability.
func newImmutableTopLevelLibraryEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
		wile.WithImmutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// --- Flag OFF via WithMutableTopLevel() opt-out: strict R7RS, status quo preserved ---
// Immutability is now the default; these pin the opt-out path that restores mutable
// top-level set!/redefine.

func TestImmutableTopLevel_OptOut_CrossFormSetBangAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithMutableTopLevel())
	c.Assert(err, qt.IsNil)

	// Two separate top-level forms (two compilation units). With the opt-out,
	// f never becomes Stable, so the later set! is permitted.
	result, err := eng.EvalMultiple(ctx, `(define f 5) (set! f 6) f`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "6")
}

func TestImmutableTopLevel_OptOut_RedefineAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithMutableTopLevel())
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

// TestImmutableTopLevel_On_PrestampedUnitRedefineRejected pins the Lever A
// pre-stamp/revert round trip against the redefine guard. Unit 1 is a begin-wrapped
// unit, which fires the frame-reclaim pre-stamp pass (CompileValidatedBegin
// transiently stamps the defines Stable to classify, then reverts SYNCHRONOUSLY
// before Pass 2). The three assertions pin distinct properties:
//   - Unit 1 compiling cleanly is the revert's load-bearing pin: a botched revert
//     that left the bindings Stable into Pass 2 would make Pass 2's own redefine
//     guard reject this very unit (the collision the revert exists to prevent).
//   - a.IsStable() afterward confirms a's FINAL Stable bit survives the round trip.
//     That bit is re-applied by Pass 2's per-define stamp (not the reverted
//     pre-stamp), so this is the property the no-pre-stamp RedefineRejected test
//     cannot exercise — it pins that the revert did not permanently clear it.
//   - The cross-unit redefine then confirms that surviving Stable bit is enforced
//     at compile time.
//
// Unlike TestImmutableTopLevel_On_RedefineRejected (two non-begin units, pre-stamp
// never fires), this is the only test exercising the guard ON the pre-stamped path.
func TestImmutableTopLevel_On_PrestampedUnitRedefineRejected(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	// Begin unit fires the pre-stamp/revert pass; a clean compile is the direct
	// witness that the revert ran before Pass 2's redefine guard.
	_, err := eng.EvalMultiple(ctx, `(begin (define a 1) (define b 2))`)
	c.Assert(err, qt.IsNil)

	// a's final Stable bit survives the pre-stamp+revert round trip (re-applied by
	// Pass 2) — directly pinned, mirroring TestImmutableTopLevel_On_StableFlagReflectsInUnitMutation.
	env := eng.Environment()
	a := env.GetGlobalBinding(environment.NewGlobalIndex(values.NewSymbol("a")))
	c.Assert(a, qt.IsNotNil)
	c.Assert(a.IsStable(), qt.IsTrue,
		qt.Commentf("a must remain Stable after the pre-stamp+revert round trip"))

	// And that surviving Stable bit is enforced: a cross-unit redefine is rejected
	// at compile time (sentinel + CompilationError, like CrossFormSetBangRejected).
	_, err = eng.EvalMultiple(ctx, `(define a 3)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("expected ErrImmutableBinding after pre-stamped begin unit, got: %v", err))
	var compErr *wile.CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue,
		qt.Commentf("expected compile-time rejection, got %T: %v", err, err))
}

// TestImmutableTopLevel_On_InUnitRedefineAllowed pins the boundary the pre-stamp
// must not move: a name defined twice WITHIN one begin unit has StableInUnit=false
// (the validator sees two defines), so the pre-stamp skips it and the in-unit
// redefine is allowed — unlike the cross-unit case above, which is rejected. Guards
// the pre-stamp's StableInUnit gate / "already Stable, leave it" branch.
func TestImmutableTopLevel_On_InUnitRedefineAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := newImmutableTopLevelEngine(t)

	result, err := eng.EvalMultiple(ctx, `(begin (define f 5) (define f 6) f)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "6")
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

// TestImmutableTopLevel_On_CxrReExportImport is the regression guard for commit
// d8911c15. The cxr accessors (caar..cddddr) are defined ambiently in
// registry/core/bootstrap_procedures.scm and stamped Stable under immutability.
// (scheme cxr) was previously re-defining them, so (import (scheme cxr)) collided
// with the already-Stable ambient binding -> ErrImmutableBinding. After making
// cxr.sld a pure re-export manifest, the import resolves the single ambient
// binding instead. (scheme r5rs) reaches the same accessors via its transitive
// (scheme cxr) import; importing (scheme base) AND (scheme cxr) together must not
// collide either, since both now export the same binding rather than two copies.
func TestImmutableTopLevel_On_CxrReExportImport(t *testing.T) {
	cases := []struct {
		name     string
		src      string
		expected string
	}{
		{"cxr-2level", `(import (scheme cxr)) (caar '((1 2) 3))`, "1"},
		{"cxr-4level", `(import (scheme cxr)) (cddddr '(1 2 3 4 5 6))`, "(5 6)"},
		{"r5rs-transitive", `(import (scheme r5rs)) (caddr '(1 2 3 4))`, "3"},
		{"base-and-cxr-together", `(import (scheme base) (scheme cxr)) (caar '((9)))`, "9"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			eng := newImmutableTopLevelLibraryEngine(t)

			result, err := eng.EvalMultiple(ctx, tc.src)
			c.Assert(err, qt.IsNil,
				qt.Commentf("cxr re-export import must not collide with the Stable ambient binding"))
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

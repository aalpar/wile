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

// TestDefineSyntaxShadowsImportedMacro pins the syntax half of R7RS §5.3.1,
// which says a definition of a name bound to a syntactic keyword binds it to a
// NEW LOCATION. So the user's define-syntax gets its own slot and OUTRANKS the
// import; it does not reuse the import's slot and write through it.
//
// RENAMED AND RE-SPECIFIED 2026-10-02 (P3.11). This read
// TestDefineSyntaxSupersedesImportClearsImported and asserted
// `after == before`, pointer-identical, calling that "supersede in place". That
// was the behaviour, and it was order-dependent: the same pair written in the
// other order shadowed, so the two orders disagreed, and phase 0 already
// shadowed while phase 1 superseded — two phases disagreeing about one
// relation. The import install now takes the shadowable tier at every phase.
//
// THE ASSERTION IS NOW A RANKING ONE, not an identity one, and that is the
// whole substance of the change. What must hold is that a read resolves to the
// user's binding; which slot it lives in is the implementation of that, and
// pinning the slot pinned the wrong thing. `after != before` is kept as the
// non-vacuity guard, inverted: it distinguishes a real second slot from a
// write-through that would make the resolution assertion pass for the wrong
// reason.
//
// The provenance clear is still asserted, and is now about the NEW slot: a
// binding the user just created is not imported and not stable. IsStable() is
// `m.Imported || m.Stable`, so a stale true there tells validate.classifyCallee
// and the frame-reclaim classifier "cannot be rebound" about a binding the user
// had just rebound.
func TestDefineSyntaxShadowsImportedMacro(t *testing.T) {
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
	c.Assert(after, qt.Not(qt.Equals), before,
		qt.Commentf("define-syntax binds a NEW location (R7RS §5.3.1), so it must not be the "+
			"import's own binding written through"))
	c.Assert(after.IsImported(), qt.IsFalse,
		qt.Commentf("the user's own slot is not an import"))
	c.Assert(after.IsStable(), qt.IsFalse,
		qt.Commentf("a binding the user just created is not stable"))
	c.Assert(before.IsImported(), qt.IsTrue,
		qt.Commentf("and the import's binding is UNTOUCHED: shadowing does not reach into it. "+
			"This is the assertion that fails if the shadow ever regresses to a write-through"))

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

// TestSupersedeClearsImportedOnlyAtAMutableCoordinate is the missing half of the
// two supersede tests above: WHERE the cleared binding sits, not just that it
// was cleared.
//
// Imported is not only provenance — it is a RANKING INPUT. tierOf reads it to
// separate tierExactImported from tierExactSealed at one and the same (phase,
// sealed) coordinate, so clearing it on a SEALED slot would silently promote
// that slot from the import tier into the startup set's. The R7RS §5.3.1
// supersede rule clears it at exactly two sites (compile_define.go,
// compile_define_syntax.go), and both are safe only because they write through
// MUTABLE coordinates, where the tier is tierExactMutable regardless of the
// flag. Nothing said so.
//
// The two existing tests each pin half and neither pins this:
// TestDefineSyntaxSupersedesImportClearsImported asserts pointer identity and
// the flag; TestImportedBindingTakesTheSealedPhaseZeroTier asserts the import's
// sealed slot survives a define. Neither asserts that the slot the WRITE landed
// on is mutable, which is the property that makes the clear tier-neutral.
//
// GUARD, not a pin: the property holds on master, and there is no build on which
// it does not — a test that could go red here would be one where the supersede
// had already moved to a sealed coordinate. It is here so that move fails loudly
// instead of re-tiering a slot in silence. It carries no claim that the clear
// sites were ever wrong.
//
// Both top-level modes, because the two clear sites run under both and the
// immutable default routes the variable path through an extra guard
// (declareDefineBinding's immTop branch) that the mutable one skips.
func TestSupersedeClearsImportedOnlyAtAMutableCoordinate(t *testing.T) {
	modes := []struct {
		name string
		opt  wile.EngineOption
	}{
		{name: "immutable top level (default)", opt: wile.WithImmutableTopLevel()},
		{name: "mutable top level", opt: wile.WithMutableTopLevel()},
	}
	for _, mode := range modes {
		t.Run(mode.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
				mode.opt,
			)
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = eng.Close()
			}()
			store := eng.Environment().Namespace().Store()

			_, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
			c.Assert(err, qt.IsNil)

			// The VARIABLE path (compile_define.go). The define takes a slot of its
			// own in the mutable tier, so the clear runs on a binding that was never
			// imported; the import's sealed slot is left carrying its stamp.
			varSym := values.NewSymbol("list-copy")
			c.Assert(store.IsImportedBindingAt(varSym, values.EmptyScopes(), environment.PhaseRuntime),
				qt.IsTrue, qt.Commentf("premise: the import holds the phase-0 imported tier"))

			_, err = eng.EvalMultiple(ctx, `(define list-copy 7)`)
			c.Assert(err, qt.IsNil)
			c.Assert(store.IsSealedBindingAt(varSym, values.EmptyScopes(), environment.PhaseRuntime),
				qt.IsFalse,
				qt.Commentf("the define's slot is SEALED: clearing Imported on it moves it from "+
					"tierExactImported to tierExactSealed, above the import it was meant to shadow"))
			c.Assert(store.ImportedBindingAt(varSym, values.EmptyScopes(), environment.PhaseRuntime),
				qt.IsNotNil,
				qt.Commentf("the import's own slot lost its stamp, which is the same re-tiering seen from underneath"))

			// The SYNTAX path (compile_define_syntax.go). Since P3.11 this
			// SHADOWS rather than superseding in place, so the define gets its own
			// slot and the clear lands on THAT — the import's binding keeps its
			// stamp. The variable arm above is the one that still re-tiers a
			// shared slot, which is why the two arms assert different things.
			synSym := values.NewSymbol("when")
			before := eng.Namespace().Expand().GetBinding(synSym, values.AllScopes())
			c.Assert(before, qt.IsNotNil)
			c.Assert(before.IsImported(), qt.IsTrue,
				qt.Commentf("premise: the import outranks bootstrap's sealed when at phase 1"))

			_, err = eng.EvalMultiple(ctx, `(define-syntax when (syntax-rules () ((_ x) (quote user-when))))`)
			c.Assert(err, qt.IsNil)

			after := eng.Namespace().Expand().GetBinding(synSym, values.AllScopes())
			c.Assert(after, qt.Not(qt.Equals), before,
				qt.Commentf("premise: define-syntax SHADOWS, so this is the user's own slot"))
			c.Assert(after.IsImported(), qt.IsFalse)
			c.Assert(before.IsImported(), qt.IsTrue,
				qt.Commentf("the import's binding keeps its stamp: nothing was cleared in place"))
		})
	}
}

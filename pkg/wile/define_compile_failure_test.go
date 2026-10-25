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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestFailedUnitLeavesNoSlotBehind pins that a unit which fails to expand or
// compile leaves no top-level slot behind for a name it was defining. Before
// the fix the slot the unit minted (the expander's predeclaration, or the
// define's own declaration) stayed bound to #<void>, so a later (f 1) reported
// "expected a procedure, got #<void>" instead of naming f, and under the
// default immutable top level a corrected redefinition was refused as a
// rebind of a stable binding. Racket and Chez (petite) both report f unbound
// after a failed (define (f p) (if)), and both accept the redefinition.
//
// Each row runs its failing unit, then the probe as a separate unit.
func TestFailedUnitLeavesNoSlotBehind(t *testing.T) {
	t.Parallel()
	rows := []struct {
		name  string
		fail  string
		probe string
	}{
		{"body names an unbound variable", `(define (f p) zzz-unbound)`, `(f 1)`},
		{"body is a syntax error", `(define (f p) (if))`, `(f 1)`},
		{"variable form", `(define f zzz-unbound)`, `f`},
		{"lambda value", `(define f (lambda (p) (if)))`, `f`},
		{"macro-produced define, sibling fails", `(begin (define-values (bar) (values 1)) (if))`, `bar`},
		{"define compiled, sibling fails to expand", `(begin (define x 1) (if))`, `x`},
		{"define compiled, sibling fails to compile", `(begin (define x 1) zzz-unbound)`, `x`},
		// The eval-when runtime half expands with its own expander, which must
		// log into this unit.
		{"eval-when body define, sibling fails to compile", `(begin (eval-when (eval) (define evf 1)) zzz-unbound)`, `evf`},
		// The right-hand side commits a phase-1 unit while this one is still
		// expanding; a commit at another phase must not block the retraction.
		{"define-syntax in the failing unit", `(begin (define-syntax m (syntax-rules () ((_) 1))) (define (f p) zzz-unbound))`, `(f 1)`},
	}
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			t.Parallel()
			c := qt.New(t)
			eng := newEngine(t)
			ctx := context.Background()
			_, err := eng.EvalMultiple(ctx, row.fail)
			c.Assert(err, qt.IsNotNil, qt.Commentf("the unit must fail for the row to mean anything"))
			_, err = eng.EvalMultiple(ctx, row.probe)
			c.Assert(err, qt.ErrorIs, werr.ErrNoSuchBinding)
		})
	}
}

// TestFailedDefineThenRedefine pins the user-visible half: a typo in a define
// must not lock the name. Under the default immutable top level the failed
// define used to leave a Stable #<void> slot, and the corrected define was
// refused with "cannot redefine immutable top-level binding".
func TestFailedDefineThenRedefine(t *testing.T) {
	t.Parallel()
	for _, fail := range []string{`(define (f p) zzz-unbound)`, `(define f zzz-unbound)`, `(define (f p) (if))`} {
		t.Run(fail, func(t *testing.T) {
			t.Parallel()
			c := qt.New(t)
			eng := newEngine(t)
			ctx := context.Background()
			_, err := eng.EvalMultiple(ctx, fail)
			c.Assert(err, qt.IsNotNil)
			got, err := eng.EvalMultiple(ctx, `(define (f p) p) (f 1)`)
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, "1")
		})
	}
}

// TestFailedRedefineKeepsTheOldValue is the control: only a slot the failing
// unit CREATED is retracted. A redefinition of an existing binding reuses its
// slot, and a failed one must leave the old value standing. Under the default
// the redefine guard refuses before the value compiles, so the mutable top
// level is the configuration where the retraction is reachable at all.
func TestFailedRedefineKeepsTheOldValue(t *testing.T) {
	t.Parallel()
	for _, row := range []struct {
		name string
		opts []wile.EngineOption
	}{
		{"immutable top level", nil},
		{"mutable top level", []wile.EngineOption{wile.WithMutableTopLevel()}},
	} {
		t.Run(row.name, func(t *testing.T) {
			t.Parallel()
			c := qt.New(t)
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx, row.opts...)
			c.Assert(err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			_, err = eng.EvalMultiple(ctx, `(define (f) 1)`)
			c.Assert(err, qt.IsNil)
			_, err = eng.EvalMultiple(ctx, `(define (f) zzz-unbound)`)
			c.Assert(err, qt.IsNotNil)
			got, err := eng.EvalMultiple(ctx, `(f)`)
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, "1")
		})
	}
}

// TestFailedShadowOfAPrimitiveRestoresThePrimitive pins that a define of a
// base name lands on a slot of its own (the base sits at sealed coordinates),
// so a failed one retracts that slot and the base name means the primitive
// again rather than #<void>.
func TestFailedShadowOfAPrimitiveRestoresThePrimitive(t *testing.T) {
	t.Parallel()
	c := qt.New(t)
	eng := newEngine(t)
	ctx := context.Background()
	_, err := eng.EvalMultiple(ctx, `(define (car p) zzz-unbound)`)
	c.Assert(err, qt.IsNotNil)
	got, err := eng.EvalMultiple(ctx, `(car '(1 2))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "1")
}

// TestFailedUnitKeepsASlotCommittedCodeHolds pins the refusal half of the
// retraction. A transformer that evaluates into the interaction environment
// commits code while the outer unit is still expanding; here that code caches
// the outer unit's freshly minted f. Retracting f when the outer unit then
// fails would orphan the cached binding: k would read #<void> forever while a
// later (define (f) 2) landed on a new slot. A unit commit at the slot's phase
// during the failing unit therefore keeps every slot it minted, which is the
// behavior before retraction existed: f stays, the later define reuses it, and
// k sees it.
func TestFailedUnitKeepsASlotCommittedCodeHolds(t *testing.T) {
	t.Parallel()
	c := qt.New(t)
	ctx := context.Background()
	eng := probeEngine(t, `(define (k) f)`)
	_, err := eng.EvalMultiple(ctx, `(begin (define (f) 1) (probe) (if))`)
	c.Assert(err, qt.IsNotNil)
	got, err := eng.EvalMultiple(ctx, `(define (f) 2) ((k))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "2")
}

// TestSelfReferenceSurvivesUnitRollbackWiring is the success-path control for
// the declare-before-compile order that self-reference relies on.
func TestSelfReferenceSurvivesUnitRollbackWiring(t *testing.T) {
	t.Parallel()
	c := qt.New(t)
	eng := newEngine(t)
	ctx := context.Background()
	got, err := eng.EvalMultiple(ctx, `(define (f n) (if (= n 0) 'done (f (- n 1)))) (f 3)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "done")
}

// TestFailedUnitRetractsAnIncludedDefine pins that an included file's defines
// belong to the including unit: a sibling's failure retracts them too.
func TestFailedUnitRetractsAnIncludedDefine(t *testing.T) {
	t.Parallel()
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithSourceFS(fstest.MapFS{
		"a.scm": &fstest.MapFile{Data: []byte(`(define (incf p) p)`)},
	}))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	for _, fail := range []string{`(begin (include "a.scm") zzz-unbound)`, `(begin (include "a.scm") (if))`} {
		_, err = eng.EvalMultiple(ctx, fail)
		c.Assert(err, qt.IsNotNil)
		_, err = eng.EvalMultiple(ctx, `incf`)
		c.Assert(err, qt.ErrorIs, werr.ErrNoSuchBinding)
	}
	got, err := eng.EvalMultiple(ctx, `(begin (include "a.scm") (incf 1))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "1")
}

// probeEngine returns an engine defining a macro, probe, whose transformer
// evaluates form into the interaction environment: a unit that commits while
// the unit expanding (probe) is still in flight.
func probeEngine(t *testing.T, form string) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(ctx, `
(import (scheme base) (scheme eval) (scheme repl)
        (for-syntax (scheme base) (scheme eval) (scheme repl)))
(define-syntax probe
  (er-macro-transformer
    (lambda (x r c)
      (eval '`+form+` (interaction-environment))
      ''ok)))`)
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestKeptSlotDoesNotLockTheName pins the other half of a refused retraction.
// The commit in the window is unrelated to f, but the slot is kept anyway, and
// the failed define had stamped it Stable before its value compiled. Kept as
// is, the immutable top level would refuse every later define of f; the kept
// slot is reverted to a placeholder instead, so the later define binds it.
func TestKeptSlotDoesNotLockTheName(t *testing.T) {
	t.Parallel()
	c := qt.New(t)
	ctx := context.Background()
	eng := probeEngine(t, `(define zz-other 0)`)
	_, err := eng.EvalMultiple(ctx, `(begin (define (f) 1) (probe) zzz-unbound)`)
	c.Assert(err, qt.IsNotNil)
	got, err := eng.EvalMultiple(ctx, `(define (f) 2) (f)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "2")
}

// TestFailedUnitKeepsSlotsWhileAnotherUnitIsInFlight pins the concurrent case
// deterministically: a unit in flight at the same phase (here registered by
// hand, as another SRFI-18 thread's compile would be) may already have
// resolved and cached the failing unit's slot, so the slot is kept, as a
// placeholder a later define binds. Once that unit ends, a failure retracts.
func TestFailedUnitKeepsSlotsWhileAnotherUnitIsInFlight(t *testing.T) {
	t.Parallel()
	c := qt.New(t)
	ctx := context.Background()
	eng := newEngine(t)
	store := eng.Environment().Namespace().Store()

	store.BeginUnit(environment.PhaseRuntime)
	_, err := eng.EvalMultiple(ctx, `(define (f p) zzz-unbound)`)
	c.Assert(err, qt.IsNotNil)
	_, err = eng.EvalMultiple(ctx, `f`)
	c.Assert(err, qt.IsNil, qt.Commentf("the slot is kept while another unit is in flight"))
	got, err := eng.EvalMultiple(ctx, `(define (f p) p) (f 1)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "1")
	store.EndUnit(environment.PhaseRuntime, false)

	_, err = eng.EvalMultiple(ctx, `(define (g p) zzz-unbound)`)
	c.Assert(err, qt.IsNotNil)
	_, err = eng.EvalMultiple(ctx, `g`)
	c.Assert(err, qt.ErrorIs, werr.ErrNoSuchBinding)
}

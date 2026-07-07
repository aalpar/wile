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

// White-box (package wile) so the enumeration tests can name the internal
// mutationPrimitives()/bootstrapCoupledMutators() sources of truth and construct a
// test-local PrimitiveRemover, matching dialect_minimal_test.go.
package wile

import (
	"context"
	"errors"
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// removerDialect is a test-local Dialect + PrimitiveRemover that removes exactly
// the primitives it is given (and no forms). Used to probe which removals engine
// construction tolerates, independent of the shipped NoMutation policy.
type removerDialect struct {
	removed []string
}

func (removerDialect) Name() string {
	return "test-remover"
}

func (removerDialect) InstallForms(_ *forms.FormRegistry) error {
	return nil
}

func (d removerDialect) RemovedPrimitives() []string {
	return slices.Clone(d.removed)
}

// TestNoMutation_Name pins the dialect identity.
func TestNoMutation_Name(t *testing.T) {
	c := qt.New(t)
	c.Assert(NoMutation, qt.IsNotNil)
	c.Assert(NoMutation.Name(), qt.Equals, "no-mutation")
}

// TestNoMutation_ImplementsPrimitiveRemover proves NoMutation crosses the
// forms-only ceiling via the optional PrimitiveRemover capability, that its removal
// set is the canonical mutation set minus the bootstrap-coupled retentions, and that
// RemovedPrimitives hands back a defensive copy.
func TestNoMutation_ImplementsPrimitiveRemover(t *testing.T) {
	c := qt.New(t)
	remover, ok := NoMutation.(PrimitiveRemover)
	c.Assert(ok, qt.IsTrue, qt.Commentf("NoMutation must implement PrimitiveRemover"))

	// Removed set == full mutation set minus the bootstrap-coupled retentions.
	got := remover.RemovedPrimitives()
	coupled := bootstrapCoupledMutators()
	for _, name := range mutationPrimitives() {
		want := !slices.Contains(coupled, name)
		c.Assert(slices.Contains(got, name), qt.Equals, want,
			qt.Commentf("%s: removed?=%v (coupled=%v)", name, want, coupled))
	}
	// None of the retained (bootstrap-coupled) mutators appear in the removed set.
	for _, name := range coupled {
		c.Assert(slices.Contains(got, name), qt.IsFalse,
			qt.Commentf("%s is bootstrap-coupled and must be retained, not removed", name))
	}

	// Defensive copy: mutating the returned slice must not corrupt the canonical set.
	got[0] = "corrupted"
	c.Assert(slices.Contains(remover.RemovedPrimitives(), "corrupted"), qt.IsFalse)
}

// TestNoMutation_Engine_MutatorsGone_RestIntact is the end-to-end validation: an
// engine on no-mutation loses BOTH the set! form (like r7rs-minimal) AND the
// removable mutation primitives (the ceiling this dialect crosses), the rest of
// R7RS still works, and a default engine retains them — the difference is the dialect.
func TestNoMutation_Engine_MutatorsGone_RestIntact(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(NoMutation))
	c.Assert(err, qt.IsNil)

	// The set! FORM is gone (as in r7rs-minimal).
	_, err = eng.EvalMultiple(ctx, "(let ((x 1)) (set! x 2) x)")
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("set! form must be gone under no-mutation, got %v", err))

	// The set-car! PRIMITIVE is gone — this is what no-mutation adds beyond the
	// forms-only r7rs-minimal (which retains all mutation primitives).
	_, err = eng.EvalMultiple(ctx, "(set-car! (cons 1 2) 9)")
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("set-car! primitive must be gone under no-mutation, got %v", err))
	c.Assert(err.Error(), qt.Contains, "set-car!",
		qt.Commentf("the unbound identifier must be set-car! specifically"))

	// Non-mutating R7RS is intact.
	got, err := eng.EvalMultiple(ctx,
		"(let loop ((i 0) (acc 0)) (if (< i 5) (loop (+ i 1) (+ acc i)) acc))")
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "10")

	got, err = eng.EvalMultiple(ctx, "(car (cons 1 2))")
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "1")

	// COW isolation: a default engine still mutates — the removal is per-engine.
	base, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	got, err = base.EvalMultiple(ctx, "(let ((p (cons 1 2))) (set-car! p 9) (car p))")
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "9")
}

// TestNoMutation_RemovedPrimitivesUnbound pins that every primitive NoMutation
// removes is unbound at the top level of a no-mutation engine. Table-driven over the
// single source of truth so extending the removed set automatically extends the guard.
func TestNoMutation_RemovedPrimitivesUnbound(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithDialect(NoMutation))
	qt.Assert(t, err, qt.IsNil)

	remover := NoMutation.(PrimitiveRemover)
	for _, name := range remover.RemovedPrimitives() {
		t.Run(name, func(t *testing.T) {
			// The name in operator position resolves to an unbound global before
			// any arity check, so a bare application surfaces ErrNoSuchBinding.
			_, err := eng.EvalMultiple(ctx, "("+name+")")
			qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
				qt.Commentf("%s must be unbound under no-mutation, got %v", name, err))
		})
	}
}

// TestNoMutation_RetainedMutatorsRemainBound is the honesty pin: the two
// bootstrap-coupled mutators NoMutation cannot remove are still callable. This is
// the documented Phase-A boundary, analogous to r7rs-minimal keeping set-car!.
func TestNoMutation_RetainedMutatorsRemainBound(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithDialect(NoMutation))
	c.Assert(err, qt.IsNil)

	// vector-set! and string-set! remain because the stdlib's vector-map/string-map
	// are built on them (see bootstrapCoupledMutators).
	got, err := eng.EvalMultiple(ctx,
		"(let ((v (make-vector 2 0))) (vector-set! v 0 7) (vector-ref v 0))")
	c.Assert(err, qt.IsNil,
		qt.Commentf("vector-set! is bootstrap-coupled and retained under no-mutation"))
	c.Assert(got.SchemeString(), qt.Equals, "7")

	got, err = eng.EvalMultiple(ctx,
		`(let ((s (make-string 2 #\a))) (string-set! s 0 #\z) (string-ref s 0))`)
	c.Assert(err, qt.IsNil,
		qt.Commentf("string-set! is bootstrap-coupled and retained under no-mutation"))
	c.Assert(got.SchemeString(), qt.Equals, `#\z`)
}

// TestNoMutation_RetainedMutatorsAreBootstrapNecessary proves the retention is
// minimal and load-bearing, not arbitrary: an engine that removes NoMutation's set
// builds fine, but additionally removing either bootstrap-coupled mutator breaks
// NewEngine (the eager bootstrap's vector-map/string-map need them). If those stdlib
// builders are ever rewritten mutation-free, this test flips and the retention set
// can shrink.
func TestNoMutation_RetainedMutatorsAreBootstrapNecessary(t *testing.T) {
	ctx := context.Background()
	removed := NoMutation.(PrimitiveRemover).RemovedPrimitives()

	// Baseline: NoMutation's own removal set constructs successfully.
	_, err := NewEngine(ctx, WithDialect(removerDialect{removed: removed}))
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("removing NoMutation's set must not break construction"))

	// Adding each retained mutator to the removal set must break construction.
	for _, extra := range bootstrapCoupledMutators() {
		t.Run(extra, func(t *testing.T) {
			withExtra := append(slices.Clone(removed), extra)
			_, err := NewEngine(ctx, WithDialect(removerDialect{removed: withExtra}))
			qt.Assert(t, errors.Is(err, werr.ErrEngineInit), qt.IsTrue,
				qt.Commentf("removing bootstrap-coupled %s must break NewEngine, got %v", extra, err))
		})
	}
}

// TestNoMutation_ImportReexposes_DocumentsBoundary pins the Phase-A boundary:
// removal is at the visible top level only. (import (scheme base)) re-exposes a
// removed mutator, because the library/import surface is a separate reader the
// primitive dialect layer does not gate. Airtight enforcement across import is the
// expander-level track — this test is the target that phase will invert.
func TestNoMutation_ImportReexposes_DocumentsBoundary(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// WithLibraryPaths() (no args) enables the embedded library system so (import …)
	// resolves — the import surface whose gating this boundary is about.
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithDialect(NoMutation),
		WithSourceFS(stdlib.FS), WithLibraryPaths("."))
	c.Assert(err, qt.IsNil)

	got, err := eng.EvalMultiple(ctx,
		"(import (scheme base)) (let ((p (cons 1 2))) (set-car! p 9) (car p))")
	c.Assert(err, qt.IsNil,
		qt.Commentf("Phase A removes from the top level only; import re-exposes — Phase B closes this"))
	c.Assert(got.SchemeString(), qt.Equals, "9")
}

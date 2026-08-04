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

// White-box (package wile) so the tests can name the internal mutationPrimitives()
// source of truth and the core bootstrap-fragment sources, matching
// dialect_minimal_test.go.
package wile

import (
	"context"
	"errors"
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestNoMutation_Name pins the dialect identity.
func TestNoMutation_Name(t *testing.T) {
	c := qt.New(t)
	c.Assert(NoMutation, qt.IsNotNil)
	c.Assert(NoMutation.Name(), qt.Equals, "no-mutation")
}

// TestNoMutation_ImplementsPrimitiveRemover proves NoMutation crosses the forms-only
// ceiling via the optional PrimitiveRemover capability, that its removal set is the
// full canonical mutation set, and that RemovedPrimitives hands back a defensive copy.
func TestNoMutation_ImplementsPrimitiveRemover(t *testing.T) {
	c := qt.New(t)
	remover, ok := NoMutation.(PrimitiveRemover)
	c.Assert(ok, qt.IsTrue, qt.Commentf("NoMutation must implement PrimitiveRemover"))
	c.Assert(remover.RemovedPrimitives(), qt.DeepEquals, mutationPrimitives())

	// Defensive copy: mutating the returned slice must not corrupt the canonical set.
	got := remover.RemovedPrimitives()
	c.Assert(len(got) > 0, qt.IsTrue)
	got[0] = "corrupted"
	c.Assert(slices.Contains(remover.RemovedPrimitives(), "corrupted"), qt.IsFalse)
}

// TestNoMutation_ImplementsBootstrapProcedureRewriter proves NoMutation swaps the
// mutating vector-map/string-map bootstrap fragment for the mutation-free one, and
// leaves every other source untouched.
func TestNoMutation_ImplementsBootstrapProcedureRewriter(t *testing.T) {
	c := qt.New(t)
	rw, ok := NoMutation.(BootstrapProcedureRewriter)
	c.Assert(ok, qt.IsTrue, qt.Commentf("NoMutation must implement BootstrapProcedureRewriter"))

	in := []string{"core-procs", core.MutableVectorStringMapSource, "trailing"}
	out := rw.RewriteBootstrapProcedures(in)
	c.Assert(out, qt.DeepEquals,
		[]string{"core-procs", core.ImmutableVectorStringMapSource, "trailing"},
		qt.Commentf("only the mutating maps fragment is swapped; order and others preserved"))
}

// TestNoMutation_Engine_MutatorsGone_RestIntact is the end-to-end validation: an
// engine on no-mutation loses the set! form AND the mutation primitives, the rest of
// R7RS still works, and a default engine retains them — the difference is the dialect.
func TestNoMutation_Engine_MutatorsGone_RestIntact(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(NoMutation))
	c.Assert(err, qt.IsNil)

	// The set! FORM is gone.
	_, err = eng.EvalMultiple(ctx, "(let ((x 1)) (set! x 2) x)")
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("set! form must be gone under no-mutation, got %v", err))

	// The set-car! PRIMITIVE is gone.
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

	// COW isolation: a default engine still mutates — the removal is per-engine.
	base, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	got, err = base.EvalMultiple(ctx, "(let ((p (cons 1 2))) (set-car! p 9) (car p))")
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "9")
}

// TestNoMutation_AllMutationPrimitivesUnbound pins that EVERY primitive in the
// canonical mutation set is unbound at the top level of a no-mutation engine — the
// full set, including vector-set!/string-set!, which the bootstrap rewrite frees up.
func TestNoMutation_AllMutationPrimitivesUnbound(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithDialect(NoMutation))
	qt.Assert(t, err, qt.IsNil)

	for _, name := range mutationPrimitives() {
		t.Run(name, func(t *testing.T) {
			// The name in operator position resolves to an unbound global before any
			// arity check, so a bare application surfaces ErrNoSuchBinding.
			_, err := eng.EvalMultiple(ctx, "("+name+")")
			qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
				qt.Commentf("%s must be unbound under no-mutation, got %v", name, err))
		})
	}
}

// TestNoMutation_VectorStringMapWorkMutationFree is the payoff: under no-mutation
// vector-set!/string-set! are gone, yet vector-map and string-map still work — the
// dialect swapped in the mutation-free bootstrap fragment. Covers single- and
// multi-argument (shortest-length) forms for both.
func TestNoMutation_VectorStringMapWorkMutationFree(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithDialect(NoMutation))
	c.Assert(err, qt.IsNil)

	// The mutators the default bootstrap used are themselves gone.
	_, err = eng.EvalMultiple(ctx, "(vector-set! (make-vector 1 0) 0 1)")
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue)
	_, err = eng.EvalMultiple(ctx, `(string-set! (make-string 1 #\a) 0 #\b)`)
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"vector-map single", "(vector-map (lambda (x) (* x x)) (vector 1 2 3))", "#(1 4 9)"},
		{"vector-map multi (shortest)", "(vector-map + (vector 1 2 3) (vector 10 20))", "#(11 22)"},
		{"string-map single",
			`(string-map (lambda (c) (integer->char (+ 1 (char->integer c)))) "abc")`, `"bcd"`},
		{"string-map multi (shortest)",
			`(string-map (lambda (a b) (if (char<? a b) a b)) "abc" "ba")`, `"aa"`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("%s must still work mutation-free, got %v", tc.name, err))
			qt.Assert(t, got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestNoMutation_ImportReexposes_DocumentsBoundary pins the boundary: removal is at
// the visible top level only. (import (scheme base)) re-exposes a removed mutator,
// because the library/import surface is a separate reader the primitive dialect layer
// does not gate. A no-mutation dialect is a language-surface statement, not a sandbox.
func TestNoMutation_ImportReexposes_DocumentsBoundary(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// WithLibraryPaths() + the embedded stdlib FS enable (import …) resolution.
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithDialect(NoMutation),
		WithSourceFS(stdlib.FS), WithLibraryPaths("."))
	c.Assert(err, qt.IsNil)

	got, err := eng.EvalMultiple(ctx,
		"(import (scheme base)) (let ((p (cons 1 2))) (set-car! p 9) (car p))")
	c.Assert(err, qt.IsNil,
		qt.Commentf("removal is top-level only; import re-exposes — a dialect is not a sandbox"))
	c.Assert(got.SchemeString(), qt.Equals, "9")
}

// TestNoMutationHasNoHashtableUpdate pins the dialect placement of
// hashtable-update!.
//
// It IS a mutation — its body is (hashtable-set! ht key (proc ...)) — so a
// NoMutation engine must not have it. It is its OWN bootstrap procedure source
// (core.HashtableUpdateSource), and RewriteBootstrapProcedures DROPS that source
// rather than swapping it. The distinction matters: the vector-map/string-map
// fragment beside it is swapped, because those two have a mutation-free
// definition. hashtable-update! has none — the procedure IS a mutation — so
// there is nothing to swap it for.
//
// TestNoMutationRemovesEveryDestructivePrimitive structurally CANNOT catch this:
// that test iterates Registry().Primitives(), and hashtable-update! is a Scheme
// define, not a registered primitive. This test is the only guard.
func TestNoMutationHasNoHashtableUpdate(t *testing.T) {
	e, err := NewEngine(context.Background(),
		WithProfile(KitchenSink), WithDialect(NoMutation))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = e.Close()
	}()
	_, bound := e.Get("hashtable-update!")
	qt.Assert(t, bound, qt.IsFalse)
}

// TestDefaultDialectHasHashtableUpdate is the other half: the guard above must
// fail because NoMutation removed it, not because it was never defined.
func TestDefaultDialectHasHashtableUpdate(t *testing.T) {
	e, err := NewEngine(context.Background(), WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = e.Close()
	}()
	_, bound := e.Get("hashtable-update!")
	qt.Assert(t, bound, qt.IsTrue)
}

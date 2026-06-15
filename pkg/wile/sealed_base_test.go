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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	valuestest "github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"
)

// TestSealedBase_G2_StdlibProcedureDocs is the G2 regression. Post-carve, Scheme-defined
// bootstrap procedures (e.g. caar) live in the sealed base, not the mutable runtime
// child. Two indexing paths must read the sealed base:
//   - procedure-documentation reads the procedure value's own doc (the closure landed in
//     the sealed base WITH its docstring).
//   - registerSchemeDocstrings walks the sealed base to register doc-only entries for
//     ,apropos/,doc/SearchDoc; reading the empty mutable child would drop them all.
func TestSealedBase_G2_StdlibProcedureDocs(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		qt.Assert(t, eng.Close(), qt.IsNil)
	}()

	// procedure-documentation: the caar closure (in the sealed base) carries its doc.
	result, err := eng.EvalMultiple(ctx, `(procedure-documentation caar)`)
	qt.Assert(t, err, qt.IsNil)
	doc, ok := result.Internal().(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, strings.Contains(doc.String(), "Category: pairs"), qt.IsTrue)

	// registerSchemeDocstrings: the registry doc-only index includes caar (read from the
	// sealed base). If it read the mutable child's empty frame, caar would be absent.
	found := false
	for _, dp := range eng.Registry().DocPrimitives() {
		if dp.Name == "caar" {
			found = true
			qt.Assert(t, dp.Category, qt.Equals, "pairs")
			break
		}
	}
	qt.Assert(t, found, qt.IsTrue)
}

// TestSealedBase_CaptureSafeAnchorsAreStable pins the "sealed base holds the optimizer's
// Stable anchors" invariant under the immutable default. Capture-safe callees — BOTH Go
// primitives (car, +) AND Scheme bootstrap procedures (zero?, not) — must be Stable in the
// sealed base, or the frame-reclaim classifier stops trusting them and the GC payoff
// silently shrinks for loops using them as base-case predicates. (Regression guard for the
// Q3 enforcement scope: the sealed base must be enforced, only libraries stay mutable.)
func TestSealedBase_CaptureSafeAnchorsAreStable(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	defer func() { qt.Assert(t, eng.Close(), qt.IsNil) }()
	base := eng.Environment().Namespace().SealedBase()
	for _, name := range []string{"car", "+", "zero?", "not", "null?", "pair?"} {
		t.Run(name, func(t *testing.T) {
			gi := base.GlobalEnvironment().GetGlobalIndex(values.NewSymbol(name))
			qt.Assert(t, gi, qt.IsNotNil) // owned by the sealed base
			bnd := base.GetGlobalBinding(gi)
			qt.Assert(t, bnd, qt.IsNotNil)
			qt.Assert(t, bnd.IsStable(), qt.IsTrue) // trusted anchor for the reclaim classifier
		})
	}
}

// TestSealedBase_PrimitiveGlobalIndexPinsSealedBase verifies compiled references resolve
// through the frame-pinned GlobalIndex into the sealed base, for primitives, stdlib
// procedures, and user defines (in the mutable child reaching the sealed base).
func TestSealedBase_PrimitiveGlobalIndexPinsSealedBase(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{"primitive call resolves post-carve", `(+ 1 2 3)`, values.NewInteger(6)},
		{"stdlib proc resolves post-carve", `(caar '((1 2) 3))`, values.NewInteger(1)},
		{"user define then call", `(define (sq x) (* x x)) (sq 9)`, values.NewInteger(81)},
	}
	ctx := context.Background()
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()
			result, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.Internal(), valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestSealedBase_SetBangBehavior pins the set! enforcement states. The sealed base is
// per-namespace and OWNED (nothing shares it within an Engine), so a flag-off set! of a
// non-Stable sealed name mutates that owned binding in place — equivalent to today's
// per-namespace mutation, NOT corruption. A flag-on set! of a Stable sealed name (a
// capture-safe primitive) is rejected (compile_validated.go:574).
func TestSealedBase_SetBangBehavior(t *testing.T) {
	ctx := context.Background()

	t.Run("flag-off set! of non-Stable sealed proc mutates the owned base", func(t *testing.T) {
		// caar is a bootstrap procedure in the sealed base; it is NOT capture-safe, so it
		// is never stamped Stable. set! resolves it to the sealed binding and mutates in
		// place — harmless because the optimizer only ever trusts Stable bindings.
		eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithMutableTopLevel())
		qt.Assert(t, err, qt.IsNil)
		defer func() { qt.Assert(t, eng.Close(), qt.IsNil) }()
		result, err := eng.EvalMultiple(ctx, `(set! caar (lambda (x) 'mutated)) (caar '((1 2) 3))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewSymbol("mutated"))
	})

	t.Run("flag-on set! of a Stable sealed name is rejected", func(t *testing.T) {
		// car is capture-safe, so under WithImmutableTopLevel it is Stable; set! rejected.
		eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithImmutableTopLevel())
		qt.Assert(t, err, qt.IsNil)
		defer func() { qt.Assert(t, eng.Close(), qt.IsNil) }()
		_, err = eng.EvalMultiple(ctx, `(set! car (lambda (x) 'mutated))`)
		qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue)
	})
}

// TestSealedBase_SetBangPortParameterPermitted pins the port-parameter corner of the
// sealed-base carve under the immutable default. The three I/O port parameters
// (current-output-port etc.) are registered as global values into the sealed base
// (registry/apply.go:160-171) but are NOT capture-safe, so they are never stamped Stable.
// The set! gate rejects only Stable sealed bindings (compile_validated.go:583), so a user
// (set! current-output-port ...) stays PERMITTED even under the immutable default — and,
// because the binding is non-Stable, it is not compile-time-pinned, so the mutation is
// observable on a subsequent read of the same binding. This is the documented intent at
// registry/apply.go:165; the test locks it against regression.
func TestSealedBase_SetBangPortParameterPermitted(t *testing.T) {
	ctx := context.Background()
	// No WithMutableTopLevel: exercise the immutable DEFAULT. The point is that a
	// non-Stable sealed binding stays settable even with top-level immutability on.
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	defer func() { qt.Assert(t, eng.Close(), qt.IsNil) }()

	// set! resolves through the parent walk to the sealed-base binding, mutates it, and a
	// subsequent read of the same binding observes the new value (non-Stable => not pinned).
	result, err := eng.EvalMultiple(ctx, `(set! current-output-port 'replaced) current-output-port`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewSymbol("replaced"))
}

// TestSealedBase_ThreadSharesMutableGlobalSeesSealedBase verifies a top-level define in
// the parent is visible to a spawned SRFI-18 thread (shared mutable global), and that
// primitives (sealed base) resolve inside the thread. Run under -race to confirm the
// thread captures the one mutable runtime, not a per-thread copy or the sealed base.
func TestSealedBase_ThreadSharesMutableGlobalSeesSealedBase(t *testing.T) {
	ctx := context.Background()
	// Opt out of the immutable default: the thread mutates a top-level cell via set!.
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths(), WithMutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	defer func() { qt.Assert(t, eng.Close(), qt.IsNil) }()
	// KitchenSink binds the SRFI-18 thread primitives ambiently — no import needed.
	code := `
      (define shared 41)
      (define result #f)
      (define th (make-thread (lambda () (set! result (+ shared 1)))))
      (thread-start! th)
      (thread-join! th)
      result`
	result, err := eng.EvalMultiple(ctx, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewInteger(42))
}

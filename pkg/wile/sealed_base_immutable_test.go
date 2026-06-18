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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	valuestest "github.com/aalpar/wile/pkg/values/valuestest"
)

// newSealedImmutableEngine builds an immutable-top-level engine that can resolve
// (import ...) against the embedded stdlib, for exercising R1/R2 under the carve.
func newSealedImmutableEngine(t *testing.T) *Engine {
	t.Helper()
	eng, err := NewEngine(context.Background(),
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths("."),
		WithImmutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestSealedBase_R1_RedefineIsShadowUnderImmutability: under WithImmutableTopLevel, a
// (define caar ...) and (import (scheme cxr)) — the R1 failures — now succeed because
// the sealed base owns caar and the redefine/import lands as a child-frame shadow
// (created=true in the mutable runtime), never tripping the redefine guard.
func TestSealedBase_R1_RedefineIsShadowUnderImmutability(t *testing.T) {
	ctx := context.Background()
	tcs := []struct {
		name string
		code string
	}{
		{"user redefine of stdlib proc", `(define caar (lambda (x) 'shadowed)) (caar '((1) 2))`},
		{"import scheme cxr", `(import (scheme cxr)) (caar '((1 2) 3))`},
		{"import scheme r5rs (transitive cxr)", `(import (scheme r5rs)) (caar '((1 2) 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newSealedImmutableEngine(t)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()
			_, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil) // previously: ErrImmutableBinding
		})
	}
}

// TestSealedBase_R1a_ZeroRedefine: zero? is a capture-safe name AND a Scheme bootstrap
// procedure (so it lives in the sealed base). Under WithImmutableTopLevel a user redefine
// lands as a child-frame shadow (created=true) and succeeds — the carve obviates the
// scoping plan's "shrink the capture-safe set" workaround, which would have cost reclaim
// coverage. No production change needed if this passes.
func TestSealedBase_R1a_ZeroRedefine(t *testing.T) {
	ctx := context.Background()
	eng := newSealedImmutableEngine(t)
	defer func() {
		qt.Assert(t, eng.Close(), qt.IsNil)
	}()
	result, err := eng.EvalMultiple(ctx, `(define zero? (lambda (x) (eqv? x 'additive-id))) (zero? 'additive-id)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.TrueValue)
}

// TestSealedBase_R2_LibraryCrossFormSetBang: a real .scm library whose body (define
// *cell*) then (set! *cell* ...) across forms must work under immutability. This is the
// form-by-form library-body case (the actual R2), loaded from a file FS — not a top-level
// begin-wrap (which already worked pre-carve and proves nothing about R2). Q3 (do not
// enforce immutability in library/child envs — ns.Runtime() != the library frame) is what
// makes the cross-form set! succeed; the library define is never stamped Stable.
func TestSealedBase_R2_LibraryCrossFormSetBang(t *testing.T) {
	ctx := context.Background()
	libFS := fstest.MapFS{
		"test/r2.scm": &fstest.MapFile{Data: []byte(`
(define-library (test r2)
  (import (scheme base))
  (begin
    (define *cell* 0)
    (set! *cell* (+ *cell* 1))
    (define (get-cell) *cell*))
  (export get-cell))`)},
	}
	// Chain the test lib FS and the embedded stdlib (for (scheme base)); enforce immutability.
	eng, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithSourceFS(libFS),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths("."),
		WithImmutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		qt.Assert(t, eng.Close(), qt.IsNil)
	}()
	result, err := eng.EvalMultiple(ctx, `(import (test r2)) (get-cell)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewInteger(1))
}

// TestSealedBase_B3_RedefineInEvalEnvironmentPermitted is the B3 regression: under the
// immutable default, redefining a binding inside a first-class eval environment —
// (environment ...) or scheme-report-environment — is PERMITTED. Those are mutable
// interaction/eval scratch spaces (child namespaces), NOT the immutable engine root,
// per the "compilation units only" scope. Before the fix the child inherited
// immutability via root delegation, so the second eval'd define was rejected with
// ErrImmutableBinding.
func TestSealedBase_B3_RedefineInEvalEnvironmentPermitted(t *testing.T) {
	ctx := context.Background()
	tcs := []struct {
		name string
		code string
	}{
		{"environment scheme base", `(define e (environment '(scheme base))) (eval '(define zz 1) e) (eval '(define zz 2) e) (eval 'zz e)`},
		{"scheme-report-environment", `(define e (scheme-report-environment 7)) (eval '(define zz 1) e) (eval '(define zz 2) e) (eval 'zz e)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newSealedImmutableEngine(t)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()
			result, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil) // previously: ErrImmutableBinding on the second define
			qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewInteger(2))
		})
	}
}

// TestSealedBase_C1_ProfileChildSetBangConsistent is the C1 regression: in a profile
// child created by (environment '(wile <profile>)), set! of a base name behaves
// consistently. The child is a mutable eval scratch space, so neither a Go primitive
// (car) nor a Scheme bootstrap procedure (caar) is stamped Stable there, and set! on
// either is PERMITTED. Before "compilation units only" the child inherited immutability
// and stamped bootstrap procedures Stable, so (set! caar ...) was rejected while
// (set! car ...) was permitted — an origin-dependent divergence from the engine root.
func TestSealedBase_C1_ProfileChildSetBangConsistent(t *testing.T) {
	ctx := context.Background()
	for _, name := range []string{"car", "caar"} {
		t.Run(name, func(t *testing.T) {
			eng := newSealedImmutableEngine(t)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()
			code := `(eval '(set! ` + name + ` (lambda (x) 'replaced)) (environment '(wile kitchen-sink)))`
			_, err := eng.EvalMultiple(ctx, code)
			qt.Assert(t, err, qt.IsNil) // mutable profile child: set! permitted for car AND caar
		})
	}
}

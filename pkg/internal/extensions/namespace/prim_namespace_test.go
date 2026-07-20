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

package namespace_test

import (
	"context"
	"testing"

	wile "github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithLibraryPaths(),
	)
	qt.Assert(t, err, qt.IsNil)
	return eng
}

func schemeEval(t *testing.T, eng *wile.Engine, code string) wile.Value {
	t.Helper()
	ctx := context.Background()
	result, err := eng.EvalMultiple(ctx, code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

func schemeEvalExpectError(t *testing.T, eng *wile.Engine, code string) {
	t.Helper()
	ctx := context.Background()
	expr, err := eng.Parse(ctx, code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = eng.Eval(ctx, expr)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestNamespaceQ(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(namespace? (interaction-environment))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")

	result = schemeEval(t, eng, `(namespace? 42)`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

func TestNamespaceName(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(namespace-name (interaction-environment))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, `"interaction-environment"`)
}

func TestMakeNamespace(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(namespace? (make-namespace))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")

	// Empty namespace has no bindings
	result = schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-bound? ns '+))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

func TestNamespaceDefineAndRef(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'x 42)
		(namespace-ref ns 'x))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "42")
}

func TestNamespaceRefDefault(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-ref ns 'nonexistent 'default))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "default")
}

func TestNamespaceRefError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-ref ns 'nonexistent))`)
}

func TestNamespaceBound(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'x 1)
		(namespace-bound? ns 'x))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")

	result = schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-bound? ns 'nonexistent))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

func TestNamespaceUndefine(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'y 10)
		(namespace-undefine! ns 'y)
		(namespace-bound? ns 'y))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

// TestNamespaceUndefineSealedRejected pins A4: undefining a sealed-base binding (a
// primitive or bootstrap procedure) is rejected rather than silently succeeding.
// Post-carve those bindings live in the immutable, engine-shared sealed base, so
// DeleteBinding on the mutable runtime removes nothing — previously the primitive
// returned Void while the binding stayed bound (a silent no-op). caar is a bootstrap
// procedure owned by the sealed base of the interaction environment.
func TestNamespaceUndefineSealedRejected(t *testing.T) {
	eng := newEngine(t)
	schemeEvalExpectError(t, eng, `(namespace-undefine! (interaction-environment) 'caar)`)
}

func TestNamespaceBoundNames(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'a 1)
		(namespace-define! ns 'b 2)
		(let ((names (namespace-bound-names ns)))
			(and (memq 'a names) (memq 'b names) #t)))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
}

func TestNamespaceDerive(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace? (namespace-derive ns)))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
}

func TestNamespaceNameEmpty(t *testing.T) {
	eng := newEngine(t)

	// make-namespace creates a namespace with name "namespace"
	result := schemeEval(t, eng, `(namespace-name (make-namespace))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, `"namespace"`)
}

func TestMakeNamespaceErrors(t *testing.T) {
	eng := newEngine(t)

	// Invalid import spec
	schemeEvalExpectError(t, eng, `(make-namespace 42)`)
}

func TestNamespaceRequireErrors(t *testing.T) {
	eng := newEngine(t)

	// Not a namespace
	schemeEvalExpectError(t, eng, `(namespace-require 42 '(scheme base))`)

	// Invalid import spec
	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-require ns 42))`)
}

func TestNamespaceDefineSymbolError(t *testing.T) {
	eng := newEngine(t)

	// Not a symbol for second arg
	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 42 "value"))`)
}

func TestNamespaceBoundSymbolError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-bound? ns 42))`)
}

func TestNamespaceUndefineSymbolError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-undefine! ns 42))`)
}

func TestNamespaceRefSymbolError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-ref ns 42))`)
}

func TestNamespaceRequireLibraryNotFound(t *testing.T) {
	eng := newEngine(t)

	// Library not found
	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-require ns '(nonexistent library)))`)
}

func TestMakeNamespaceLibraryNotFound(t *testing.T) {
	eng := newEngine(t)

	// Library not found during make-namespace
	schemeEvalExpectError(t, eng, `(make-namespace '(nonexistent library))`)
}

func TestMakeNamespaceImportSetError(t *testing.T) {
	eng := newEngine(t)

	// An import-set modifier naming a binding the library does not export
	// exercises ImportSpecInto's ApplyToExports failure branch (shared with
	// the environment and namespace-require primitives).
	schemeEvalExpectError(t, eng, `(make-namespace '(only (scheme base) totally-not-a-real-binding))`)
}

func TestNamespaceTypeErrors(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(namespace-name 42)`)
	schemeEvalExpectError(t, eng, `(namespace-define! 42 'x 1)`)
	schemeEvalExpectError(t, eng, `(namespace-ref 42 'x)`)
	schemeEvalExpectError(t, eng, `(namespace-bound? 42 'x)`)
	schemeEvalExpectError(t, eng, `(namespace-undefine! 42 'x)`)
	schemeEvalExpectError(t, eng, `(namespace-bound-names 42)`)
	schemeEvalExpectError(t, eng, `(namespace-derive 42)`)
	schemeEvalExpectError(t, eng, `(namespace-require 42 '(scheme base))`)
}

// Hygiene of the namespace read primitives.
//
// namespace-define! creates under the ambient (empty) scope set, so the reads
// must resolve under that same set. A wildcard read returns the name's first
// live slot whatever its hygiene, which diverges as soon as a macro introduced
// the same name earlier in the frame. (interaction-environment) is the engine's
// own namespace, so macro-introduced top-level binders are reachable through it.

// A wildcard read would return the macro's variable rather than the user's.
func TestNamespaceRef_ResolvesAmbientNotMacroBinder(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_) (define x 0))))
		(m)
		(define x 1)
		(namespace-ref (interaction-environment) 'x)`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "1")
}

// namespace-define! must round-trip through namespace-ref even when a macro has
// already introduced a binder of the same name.
func TestNamespaceDefine_RoundTripsPastMacroBinder(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_) (define y 0))))
		(m)
		(namespace-define! (interaction-environment) 'y 42)
		(namespace-ref (interaction-environment) 'y)`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "42")
}

// A macro-introduced binder is unreachable from any source-written reference in
// the namespace, so namespace-bound? must not report it as bound.
func TestNamespaceBound_IgnoresMacroOnlyBinder(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_) (define z 0))))
		(m)
		(namespace-bound? (interaction-environment) 'z)`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

// The ambient read must still reach the sealed base through the parent chain:
// scope-keying the lookup must not narrow it to the namespace's own frame.
func TestNamespaceRef_StillReachesSealedBase(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(procedure? (namespace-ref (interaction-environment) 'car))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
}

// namespace-bound-names must list only what the other namespace primitives can
// reach. Listing a macro-introduced binder would break enumerate-then-dereference
// — the primary use of the listing — and would disclose a name no reference
// written in that namespace can resolve.
func TestNamespaceBoundNames_HidesMacroOnlyBinder(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_) (define w 0))))
		(m)
		(list (namespace-bound? (interaction-environment) 'w)
		      (if (memq 'w (namespace-bound-names (interaction-environment))) 'listed 'not-listed))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(#f not-listed)")
}

// The listing invariant, swept over the whole namespace rather than one planted
// name: every name namespace-bound-names reports must resolve. Returns the first
// offending name instead of #t so a failure names the leak. bound? and ref share
// their lookup line, so this covers (map (lambda (n) (namespace-ref ns n)) ...)
// without the raise on failure.
//
// The sweep carries its own non-emptiness floor and its own planted-name check.
// Without them the loop returns #t on an empty listing, so the single most likely
// regression of a filter — rejecting everything — would leave this test green.
// Relying on a sibling test for that is not enough: the floor has to live in the
// assertion it protects, or deleting the sibling silently disarms this one.
func TestNamespaceBoundNames_EveryListedNameResolves(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_) (define unreachable 0))))
		(m)
		(define reachable 1)
		(let ((ns (interaction-environment)))
		  (list (> (length (namespace-bound-names ns)) 10)
		        (if (memq 'reachable (namespace-bound-names ns)) 'planted 'missing)
		        (let loop ((names (namespace-bound-names ns)))
		          (cond ((null? names) #t)
		                ((namespace-bound? ns (car names)) (loop (cdr names)))
		                (else (car names))))))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(#t planted #t)")
}

// A macro-introduced shadow of a sealed-base name is the case whose resolution
// the ambient filter actually changed: the macro's slot no longer matches, so the
// parent walk falls through to the sealed base instead of stopping at the runtime
// frame. TestNamespaceRef_StillReachesSealedBase covers the unshadowed path and
// passes either way, so it cannot pin this.
func TestNamespaceRef_MacroShadowOfSealedBaseFallsThrough(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_) (define car 0))))
		(m)
		(procedure? (namespace-ref (interaction-environment) 'car))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
}

// Imported names are the third population the listing must keep, alongside
// sealed-base primitives and user defines. installImportedBinding creates them
// ambient, so the filter admits them — but that is the assumption AmbientScopes
// rests on, and nothing else measures it.
//
// Needs its own engine: the shared newEngine has no library paths, so a real
// (scheme base) import cannot resolve there.
func TestNamespaceBoundNames_KeepsImportedNames(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(wile.StdLibFS),
		wile.WithLibraryPaths("lib"),
	)
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	result := schemeEval(t, eng, `
		(let ((ns (namespace-derive (interaction-environment))))
		  (namespace-require ns '(scheme base))
		  (list (if (memq 'map (namespace-bound-names ns)) 'listed 'not-listed)
		        (namespace-bound? ns 'map)))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(listed #t)")
}

// Filtering to the ambient scope set must not cost the listing its real
// entries: sealed-base primitives and ordinary top-level defines.
func TestNamespaceBoundNames_KeepsPrimitivesAndUserDefines(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define user-defined 1)
		(let ((names (namespace-bound-names (interaction-environment))))
		  (list (if (memq 'car names) 'listed 'not-listed)
		        (if (memq 'user-defined names) 'listed 'not-listed)))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(listed listed)")
}

// The two read families must agree. They take the same *environment.Namespace
// and share one listing body, so any future move of one family off the ambient
// set re-opens the split this pins. Asserting agreement rather than a literal
// keeps the test honest if the shared answer ever changes.
func TestReadFamiliesAgree_NamespaceAndEnvironment(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_) (define q 'MACRO-VALUE))))
		(m)
		(define visible 'USER-VALUE)
		(let ((ns (interaction-environment)))
		  (list (eq? (namespace-bound? ns 'q)       (environment-bound? ns 'q))
		        (eq? (namespace-bound? ns 'visible) (environment-bound? ns 'visible))
		        (eq? (namespace-ref ns 'visible)    (environment-ref ns 'visible))))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(#t #t #t)")
}

// The discriminating case for issue #805: one name owning both an ambient
// binding and a macro-introduced one. Undefining removes exactly the ambient
// binding — the one namespace-ref would have returned — and leaves the macro's
// alone. While delete was name-level and the read surface scope-exact, this
// destroyed a binding the caller could not read.
//
// namespace-bound? is the load-bearing assertion. The (get-w) half is a weaker
// witness than it looks: a closure keeps reading its binding after a successful
// undefine (true on master for a plain ambient binding too), so it cannot by
// itself distinguish "spared" from "deleted". The frame-level no-op case is
// pinned directly by TestGlobalFrame_DeleteOfMacroOnlyNameUnderAmbientScopesIsNoOp.
func TestNamespaceUndefine_RemovesAmbientAndSparesMacroBinder(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `
		(define-syntax m (syntax-rules () ((_ v) (begin (define w v) (lambda () w)))))
		(define get-w (m 7))
		(define w 1)
		(namespace-undefine! (interaction-environment) 'w)
		(list (namespace-bound? (interaction-environment) 'w) (get-w))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(#f 7)")
}

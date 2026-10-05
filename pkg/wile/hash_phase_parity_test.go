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

// All four R6RS hash procedures register PhaseSetRuntime, so none is bound at
// phase 1 without (import (for-syntax (rnrs hashtables))), which exports all
// four. Two routes reach a hash at phase 1 without the import; the two boundary
// tests at the end of this file pin them.
//
// History: string-ci-hash was the filed row (I066, 2026-10-03), which no library
// exported until (rnrs hashtables) did. The other three were
// ambient at phase 1 (PhaseSetRuntime|PhaseSetExpand in
// pkg/registry/core/hashes.go) until 2026-10-04, when they moved to the same
// route.
//
// The route is pinned, not just the reachability: each row imports its hash BY
// NAME through `only`, which fails if (rnrs hashtables) stops exporting it. A
// plain (for-syntax (rnrs hashtables)) import stayed green with string-ci-hash's
// export removed and its registration widened.
//
// Each row reveals the phase-1 value at phase 0 through an er-macro-transformer,
// the technique TestPhase1DefineSyntaxIsReachableFromItsOwnRung uses: a
// begin-for-syntax body cannot hand its value down a rung by itself. It then
// compares that value against the SAME call made at phase 0. A shape check —
// "an exact non-negative integer came back" — was the first cut and does not
// discriminate: measured, revealing (string-hash "ABC") under the string-ci-hash
// row passes it, so a phase-1 name resolving to a DIFFERENT hash stayed green.
// That is the live hazard and not a hypothetical, because the quartet is split
// across two files precisely so string-ci-hash can carry a fold string-hash does
// not. All four hashes are deterministic, so the comparison is not flaky.

import (
	"context"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// r6rsHashCalls is the quartet, one deterministic call each.
var r6rsHashCalls = []struct {
	name string
	call string
}{
	{name: "equal-hash", call: `(equal-hash (list 1 2))`},
	// "ABC", not "abc": case folding leaves "abc" unchanged, so on "abc"
	// string-hash and string-ci-hash agree and an export of string-ci-hash
	// under the name string-hash passed every row.
	{name: "string-hash", call: `(string-hash "ABC")`},
	{name: "string-ci-hash", call: `(string-ci-hash "ABC")`},
	{name: "symbol-hash", call: `(symbol-hash 'foo)`},
}

func TestR6RSHashProceduresAreReachableAtPhaseOne(t *testing.T) {
	for _, tt := range r6rsHashCalls {
		t.Run(tt.name, func(t *testing.T) {
			src := `(import (for-syntax (only (rnrs hashtables) ` + tt.name + `)))
			        (begin-for-syntax (define zz ` + tt.call + `))
			        (define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
			        (and (exact-integer? (reveal)) (>= (reveal) 0) (= (reveal) ` + tt.call + `))`
			got, err := evalPhaseDistinctness(t, src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "#t")
		})
	}
}

// The case-insensitive pair agrees at phase 1 on the input the FULL fold exists
// for. This is the property the export buys and the reason the hash had to share
// string-ci=?'s fold rather than approximate it: under simple folding or
// strings.ToLower, "ß" and "SS" disagree, so the hash would contradict its own
// documented contract ("if (string-ci=? a b) then equal hashes") on exactly that
// input.
//
// Both halves need an import at phase 1 and they are DIFFERENT imports:
// string-ci=? comes from (scheme char), the hash from (rnrs hashtables). The
// import is the plain whole-library one the records document as the route; the
// reachability test above narrows it with `only` to pin the export itself.
func TestStringCiPairAgreesAtPhaseOne(t *testing.T) {
	got, err := evalPhaseDistinctness(t, `
		(import (for-syntax (scheme char)) (for-syntax (rnrs hashtables)))
		(begin-for-syntax
		  (define zz (and (string-ci=? "ß" "SS")
		                  (= (string-ci-hash "ß") (string-ci-hash "SS")))))
		(define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
		(reveal)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "#t")
}

// None of the four is bound at phase 1 without the import. This is the other
// half of the route the reachability test pins, and the
// per-name pin for "do not widen" (pkg/registry/core/hashes.go,
// pkg/internal/extensions/all/register.go): widening any one registration to
// PhaseSetRuntime|PhaseSetExpand flips its row here. For string-ci-hash this is
// the only pin; widening a core hash also flips its row of
// TestCorePrimitivePhaseCensus, and widening equal-hash flips
// TestPhaseOneHashRouteNeedsALoadableLibrary.
func TestR6RSHashesAreNotAmbientAtPhaseOne(t *testing.T) {
	for _, tt := range r6rsHashCalls {
		t.Run(tt.name, func(t *testing.T) {
			// Positive control, same configuration: the name exists at phase 0, so
			// the refusal below is about the phase and not a missing registration.
			got, err := evalPhaseDistinctness(t, `(exact-integer? `+tt.call+`)`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "#t")

			_, err = evalPhaseDistinctness(t, `(begin-for-syntax (define zz `+tt.call+`))`)
			qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSuchBinding)
			// By name as well: the sentinel alone is satisfied by any unbound
			// identifier.
			qt.Assert(t, err, qt.ErrorMatches, `(?s).*"`+tt.name+`" at phase 1.*`)
		})
	}
}

// make-hashtable accepts the imported equal-hash at phase 1. make-hashtable is
// itself both-phase; the pair it recognizes is decided by primitive identity, so
// the import's copy of the hash is the registered one.
func TestMakeHashtableAcceptsTheImportedHashAtPhaseOne(t *testing.T) {
	got, err := evalPhaseDistinctness(t, `
		(import (for-syntax (only (rnrs hashtables) equal-hash)))
		(begin-for-syntax (define zz (hashtable? (make-hashtable equal-hash equal?))))
		(define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
		(reveal)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "#t")
}

// With no import, phase 1 still reaches equal-hash's value through
// hashtable-hash-function on an equal table: the hashtable procedures stay
// both-phase, and the accessor finds the hash in the namespace's runtime
// (setRecognizedPrimitive, pkg/registry/core/prim_hashtables.go).
//
// This pins a BOUNDARY, not a feature, listed in the hash note of
// pkg/stdlib/lib/rnrs/hashtables.sld. Moving the hashtable procedures to
// PhaseSetRuntime flips it, on purpose, and that note changes with it.
func TestEqualHashReachesPhaseOneThroughTheHashtableAccessor(t *testing.T) {
	got, err := evalPhaseDistinctness(t, `
		(begin-for-syntax
		  (define zz ((hashtable-hash-function (make-equal-hashtable)) (list 1 2))))
		(define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
		(= (reveal) (equal-hash (list 1 2)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "#t")
}

// The second route to a hash at phase 1 without the import: a syntax-rules
// template that introduces the name inside begin-for-syntax resolves it, the
// template leak TODO.md files on its row about values being unbound at phase 1.
// A BOUNDARY like the accessor test above: closing that leak flips this, on
// purpose, and the .sld's note changes with it.
func TestTemplateIntroducedHashResolvesAtPhaseOne(t *testing.T) {
	got, err := evalPhaseDistinctness(t, `
		(define-syntax g (syntax-rules () ((_ v) (begin-for-syntax (define v (equal-hash (list 1 2)))))))
		(g zz)
		(define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
		(= (reveal) (equal-hash (list 1 2)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "#t")
}

// phaseOneHashLibFS holds three libraries whose macro bodies call equal-hash at
// phase 1: from a transformer body and from begin-for-syntax with the library's
// own for-syntax import, and from a transformer body without it.
func phaseOneHashLibFS() fstest.MapFS {
	return fstest.MapFS{
		"hashmac.sld": &fstest.MapFile{Data: []byte(`(define-library (hashmac)
  (export hash-of)
  (import (scheme base) (for-syntax (only (rnrs hashtables) equal-hash)))
  (begin
    (define-syntax hash-of
      (er-macro-transformer (lambda (form rename compare) (equal-hash (list 1 2)))))))
`)},
		"hashbfs.sld": &fstest.MapFile{Data: []byte(`(define-library (hashbfs)
  (export z)
  (import (scheme base) (for-syntax (only (rnrs hashtables) equal-hash)))
  (begin
    (begin-for-syntax (define zz (equal-hash (list 1 2))))
    (define-syntax reveal (er-macro-transformer (lambda (form rename compare) zz)))
    (define z (reveal))))
`)},
		"hashbare.sld": &fstest.MapFile{Data: []byte(`(define-library (hashbare)
  (export hash-of)
  (import (scheme base))
  (begin
    (define-syntax hash-of
      (er-macro-transformer (lambda (form rename compare) (equal-hash (list 1 2)))))))
`)},
	}
}

// A library's macro bodies bind the hashes through the library's OWN for-syntax
// import, as a program's do through its own: the route is per environment, so
// what the program imports does not reach into the library. Without that
// import the library fails to load, naming equal-hash at phase 1.
//
// Every row fails on master, where equal-hash was bound at phase 1 without an
// import and (rnrs hashtables) did not export it: the two importing libraries
// fail to load, and hashbare loads.
func TestLibraryMacroBodyBindsTheHashThroughItsOwnImport(t *testing.T) {
	ctx := context.Background()
	newEngine := func(t *testing.T) *wile.Engine {
		t.Helper()
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(phaseOneHashLibFS()),
			wile.WithSourceFS(stdlib.FS),
			wile.WithLibraryPaths("."))
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		return eng
	}

	bound := []struct {
		name string
		src  string
	}{
		{name: "transformer body", src: `(import (hashmac)) (= (hash-of) (equal-hash (list 1 2)))`},
		{name: "begin-for-syntax", src: `(import (hashbfs)) (= z (equal-hash (list 1 2)))`},
	}
	for _, tt := range bound {
		t.Run(tt.name, func(t *testing.T) {
			got, err := newEngine(t).EvalMultiple(ctx, tt.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.SchemeString(), qt.Equals, "#t")
		})
	}

	// The program's own for-syntax import comes first, so the failure is the
	// library's missing one.
	t.Run("no for-syntax import", func(t *testing.T) {
		eng := newEngine(t)
		_, err := eng.EvalMultiple(ctx, `(import (for-syntax (rnrs hashtables)))`)
		qt.Assert(t, err, qt.IsNil)
		_, err = eng.EvalMultiple(ctx, `(import (hashbare))`)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSuchBinding)
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*no such binding "equal-hash" with compatible scopes at phase 1.*`)
	})
}

// Under WithoutAmbientBindings no hash is bound at phase 0 either, and
// (rnrs hashtables) binds each one there by name. The library exported
// string-ci-hash alone on master, so the other three rows fail there.
func TestRnrsHashtablesBindsTheHashesWithoutAmbientBindings(t *testing.T) {
	for _, tt := range r6rsHashCalls {
		t.Run(tt.name, func(t *testing.T) {
			// Control: level 2 binds no hash without the import, so the import
			// below is what binds it.
			_, err := evalUnderProfile(t, wile.Small, `(import (scheme base)) `+tt.call,
				wile.WithoutAmbientBindings())
			qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSuchBinding)
			qt.Assert(t, err, qt.ErrorMatches, `(?s).*"`+tt.name+`".*`)

			got, err := evalUnderProfile(t, wile.Small,
				`(import (scheme base) (only (rnrs hashtables) `+tt.name+`)) (exact-integer? `+tt.call+`)`,
				wile.WithoutAmbientBindings())
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "#t")
		})
	}
}

// TestHashAccessorRefusalNamesPhaseZero pins hashtable-hash-function's refusal
// when no binding of equal-hash at phase 0 is the primitive, under
// WithoutAmbientBindings: once with equal-hash left out of the import, and once
// imported at phase 1 only, which the accessor's phase-0 probes do not reach.
// The message names the phase it looked at. Only the text is pinned; the
// sentinel is open in TODO.md.
//
// Fails on master, which said "equal-hash is not registered in this namespace".
func TestHashAccessorRefusalNamesPhaseZero(t *testing.T) {
	tests := []struct {
		name string
		src  string
	}{
		{name: "not imported", src: `
			(import (scheme base) (only (rnrs hashtables) make-equal-hashtable hashtable-hash-function))
			(hashtable-hash-function (make-equal-hashtable))`},
		{name: "imported at phase 1 only", src: `
			(import (scheme base) (for-syntax (scheme base) (rnrs hashtables)))
			(begin-for-syntax (define h (hashtable-hash-function (make-equal-hashtable))))`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := evalUnderProfile(t, wile.Small, tt.src, wile.WithoutAmbientBindings())
			qt.Assert(t, err, qt.ErrorMatches,
				`(?s).*hashtable-hash-function: no binding of equal-hash at phase 0 is the primitive.*`)
		})
	}
}

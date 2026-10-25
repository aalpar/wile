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

// An unbound-binding diagnostic must name the PHASE it failed at.
//
// Plan: memory/2026-09-08-flatt-binding-model-a-impl.md, Task 1;
// design §6.6. Stage A makes "bound at phase 0, referenced at phase 1" a
// ROUTINE failure for code that reads as correct — a procedural transformer
// body that calls cadr is the canonical case — so a message carrying only the
// name sends the reader hunting for a typo that is not there.
//
// Every program below already raised before Stage A — three at phase 1, one per
// raise site, plus a phase-2 case for the rendering — so the only thing Stage
// A's edit moved was the text. That was deliberate: a test whose RED state is
// "nothing raises" cannot be turned green by a wrap-message change. The case
// where a name stops resolving at all is
// TestPhase1_ProceduralTransformerUnboundWithoutImport (Task 2), which went green
// when Task 7 deleted the ambient tier.
//
// These were not written onto the census names (assoc, caar, vector-map,
// string-map, make-parameter) for that reason. Measured before Stage A, every
// one of them resolved inside a transformer body with err == nil:
// "phase-0-only" in the binding_tier_census pins meant exact@1 == false, not
// invisible at phase 1, because the ambient tier still covered them.

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// evalForPhaseDiagnostic runs src and returns the error text, failing the test
// if src compiles. It drives the Engine rather than a compiler harness: two of
// the three raise sites are reached only through ExpandTopLevelExpression.
func evalForPhaseDiagnostic(t *testing.T, src string) string {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()
	_, err = eng.EvalMultiple(ctx, src)
	qt.Assert(t, err, qt.IsNotNil)
	return err.Error()
}

// TestUnboundDiagnosticNamesThePhase pins that each of the three
// ErrNoSuchBinding raise sites renders the compile phase when it is not the
// unit's own runtime.
//
// The phase is relative to the ENCLOSING UNIT's macro tower, not the program's:
// EnvironmentFrame.PhaseLevel's own doc calls it "a base to compute from ... not
// a stage tag", and a transformer body inside a define-library reports that
// library's level. There is no owner-independent number to render instead — a
// library's runtime is phase 0 to a plain importer and phase 1 to a for-syntax
// one — so the rendering carries the qualification rather than the number
// carrying it alone.
func TestUnboundDiagnosticNamesThePhase(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name string
		src  string
		// site is the raise site the program reaches, for the reader.
		site string
		want string
	}{
		{
			name: "scoped reference in an er-macro-transformer body",
			src: `(define counter 41)
			      (define-syntax reveal (er-macro-transformer (lambda (f r c) counter)))
			      (reveal)`,
			site: "compile_time_continuation.go, CompileSymbol scoped arm",
			want: `no such binding "counter" with compatible scopes at phase 1 of this unit's macro tower`,
		},
		{
			name: "empty-scope reference in a begin-for-syntax body",
			src: `(define counter 41)
			      (begin-for-syntax (define seen counter))`,
			site: "compile_time_continuation.go, CompileSymbol empty-scope arm",
			want: `no such local or global binding "counter" at phase 1 of this unit's macro tower`,
		},
		{
			name: "set! of a lower-phase name from a transformer body",
			src: `(define counter 41)
			      (define-syntax reveal (er-macro-transformer (lambda (f r c) (set! counter 5))))
			      (reveal)`,
			site: "compile_validated.go, CompileValidatedSetBang",
			want: `no such binding "counter" with compatible scopes for set! at phase 1 of this unit's macro tower`,
		},
		{
			// The tower is unbounded — Phase's own doc says "there is no named
			// constant above 1 ... the macro tower climbs past 2 whenever a
			// transformer body defines a macro of its own" — so the rendering
			// must be the number, not a word per rung. This row is what stops a
			// later reader from replacing it with Phase.String(), which renders
			// "expand" at 1 and "phase(2)" at 2.
			name: "the number generalizes above phase 1",
			src: `(define counter 41)
			      (begin-for-syntax (begin-for-syntax (define seen counter)))`,
			site: "compile_time_continuation.go, CompileSymbol empty-scope arm",
			want: `no such local or global binding "counter" at phase 2 of this unit's macro tower`,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := evalForPhaseDiagnostic(t, tc.src)
			qt.Assert(t, strings.Contains(got, tc.want), qt.IsTrue,
				qt.Commentf("site %s\nwant substring: %s\ngot: %s", tc.site, tc.want, got))
		})
	}
}

// TestUnboundDiagnosticStaysSilentAtRuntimePhase is a GUARD: it passes on both
// sides of Stage A.
//
// Phase 0 renders no phase SENTENCE at all, following Racket, whose phase-0
// unbound message says nothing about phase and whose phase-1 one names it in
// words ("no #%app syntax transformer is bound in the transformer phase" —
// measured against /opt/local/bin/racket 9.2, 2026-09-09, on
// `#lang racket/base` + an unbound reference at each phase). What that buys is
// the direction asserted here: ABSENCE rules the phase sentence out. The
// message is no longer byte-identical at phase 0 (unboundSuffix appends one
// discriminating clause), but every existing assertion is a prefix or substring
// match, so none needed touching.
//
// Presence rules something in only through that clause: the phase sentence's
// gate is still the compile phase alone, so it also renders for a typo compiled
// at phase 1, while the clause says whether the name is bound anywhere.
func TestUnboundDiagnosticStaysSilentAtRuntimePhase(t *testing.T) {
	t.Parallel()

	got := evalForPhaseDiagnostic(t, `(totally-absent-name 1 2)`)
	qt.Assert(t, strings.Contains(got, `no such local or global binding "totally-absent-name"`), qt.IsTrue,
		qt.Commentf("got: %s", got))
	qt.Assert(t, strings.Contains(got, "macro tower"), qt.IsFalse,
		qt.Commentf("phase 0 must render no phase clause; got: %s", got))
}

// TestUnboundDiagnosticKeepsItsSentinel is a GUARD. The phase goes in the WRAP
// message, never in the sentinel and never in an errors.Is comparison: every
// errors.Is(err, werr.ErrNoSuchBinding) caller must keep matching, and
// TestNoAmbientBindingsFloor depends on it.
//
// Text only, deliberately. The TokenizerError precedent this could have
// followed — stamp position, rune and state as struct fields, exclude them from
// Is — applies to facts that must be recoverable programmatically. Nothing
// consumes the phase; it is provenance for a human reader. If a consumer ever
// appears, it needs a field, and this comment is the record that the choice was
// made rather than defaulted into.
func TestUnboundDiagnosticKeepsItsSentinel(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()
	_, err = eng.EvalMultiple(ctx, `(define counter 41)
	                                (begin-for-syntax (define seen counter))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("the sentinel must still match through the wrap; got: %s", err.Error()))
}

// TestUnboundDiagnosticDiscriminatesAbsenceFromScopeRefusal pins that the
// unbound diagnostic says which of three things happened, instead of asserting
// a scope refusal it never tested. "with compatible scopes" blamed scopes for a
// plain typo: a repeat lookup under syntax.AllScopes() misses too, so no scope
// set would have found the name. Such a name also loses the scope wording from
// the prefix, which reads as the empty-scope arm's ("no such local or global
// binding"), so the absence clause no longer contradicts the text it follows;
// a name with a same-named binding somewhere keeps "with compatible scopes".
//
// Every row asserts the EXACT tail of the message, not a substring: the clause
// text, its parentheses and its position before the sentinel are the contract,
// and a substring match survives each of those drifting.
//
// The rows holding a name that IS bound somewhere (a phase-0 local read from a
// transformer body aside, which is a local of an enclosing phase and so outside
// the global-table claim) are the ones that kill a false absence clause.
func TestUnboundDiagnosticDiscriminatesAbsenceFromScopeRefusal(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name string
		src  string
		tail string
	}{
		{
			name: "a name bound at no phase says so",
			src:  `(begin (define-syntax q (lambda (stx) (totally-absent-name-xyz stx))) (q))`,
			tail: `no such local or global binding "totally-absent-name-xyz" at phase 1 of this unit's macro tower (no global binding of that name at any phase): no such binding`,
		},
		{
			name: "a phase-0 typo names its absence and carries no phase sentence",
			src:  `(totally-absent-name 1 2)`,
			tail: `no such local or global binding "totally-absent-name" (no global binding of that name at any phase): no such binding`,
		},
		{
			name: "set! of a name bound nowhere says so",
			src:  `(set! totally-absent-name-xyz 5)`,
			tail: `no such local or global binding "totally-absent-name-xyz" for set! (no global binding of that name at any phase): no such binding`,
		},
		{
			// The scoped half of the same row: a transformer body's identifier
			// carries scopes, and still no scope set would find the name.
			name: "set! of a name bound nowhere from a transformer body says so",
			src:  `(begin (define-syntax q (lambda (stx) (set! totally-absent-q 1))) (q))`,
			tail: `no such local or global binding "totally-absent-q" for set! at phase 1 of this unit's macro tower (no global binding of that name at any phase): no such binding`,
		},
		{
			name: "a name bound at phase 0 and referenced at phase 1 says so",
			src:  `(begin (define-syntax q (lambda (stx) (cadr (list 1 2)))) (q))`,
			tail: `no such binding "cadr" with compatible scopes at phase 1 of this unit's macro tower (bound at phase 0): no such binding`,
		},
		{
			name: "a name bound only above the reference phase names that phase",
			src:  `(begin-for-syntax (define only-expand 1)) only-expand`,
			tail: `no such local or global binding "only-expand" (bound at phase 1): no such binding`,
		},
		{
			name: "every bound phase is listed, not the nearest",
			src: `(define both 1)
			      (begin-for-syntax (begin-for-syntax (define both 2)))
			      (begin-for-syntax both)`,
			tail: `no such local or global binding "both" at phase 1 of this unit's macro tower (bound at phases 0, 2): no such binding`,
		},
		{
			name: "the empty-scope arm names the phase it is bound at",
			src: `(define counter 41)
			      (begin-for-syntax (define seen counter))`,
			tail: `no such local or global binding "counter" at phase 1 of this unit's macro tower (bound at phase 0): no such binding`,
		},
		{
			name: "a macro-introduced binder the use site cannot see names hygiene",
			src: `(define-syntax bind-it (syntax-rules () ((_ x) (let ((secret 1)) x))))
			      (bind-it secret)`,
			tail: `no such binding "secret" with compatible scopes (a same-named binding here is hygienically distinct): no such binding`,
		},
		{
			name: "set! of a hygienically distinct binder names hygiene",
			src: `(define-syntax bind-it (syntax-rules () ((_ x) (let ((secret 1)) (set! x 2)))))
			      (bind-it secret)`,
			tail: `no such binding "secret" with compatible scopes for set! (a same-named binding here is hygienically distinct): no such binding`,
		},
		{
			// The empty-scope arm does not claim a scope refusal, so a name that
			// resolves only under the wildcard gets no hygiene clause. This is the
			// row that reaches the claimsScopeRefusal gate on that arm.
			name: "the empty-scope arm never names hygiene",
			src: `(define-syntax def-it2 (syntax-rules () ((_) (define secret 1))))
			      (def-it2)
			      secret`,
			tail: `no such local or global binding "secret": no such binding`,
		},
		{
			// A macro-introduced global bound at another phase under scopes the use
			// cannot satisfy: bound, so never "no global binding".
			name: "a macro-introduced phase-1 binder is not called absent",
			src: `(define-syntax def-it4 (syntax-rules () ((_ x) (begin (begin-for-syntax (define secret 1)) x))))
			      (def-it4 secret)`,
			tail: `no such binding "secret" with compatible scopes (a same-named binding at phase 1 is hygienically distinct): no such binding`,
		},
		{
			name: "a phase-1 binder introduced by a macro is not called absent from phase 0",
			src: `(define-syntax def-ct (syntax-rules () ((_ v) (begin-for-syntax (define hidden v)))))
			      (def-ct 5)
			      hidden`,
			tail: `no such local or global binding "hidden": no such binding`,
		},
		{
			name: "a phase-0 binder introduced by a macro is not called absent from phase 1",
			src: `(define-syntax def0 (syntax-rules () ((_) (define hid0 1))))
			      (def0)
			      (begin-for-syntax hid0)`,
			tail: `no such local or global binding "hid0" at phase 1 of this unit's macro tower: no such binding`,
		},
		{
			// The only same-named binding is at ANOTHER phase, so "here" would be
			// false: the clause names the phase instead.
			name: "a hygienically distinct binder at another phase names that phase",
			src: `(define-syntax d5 (syntax-rules () ((_ x) (begin (define sec5 1) (begin-for-syntax x)))))
			      (d5 sec5)`,
			tail: `no such binding "sec5" with compatible scopes at phase 1 of this unit's macro tower (a same-named binding at phase 0 is hygienically distinct): no such binding`,
		},
		{
			// A local of an enclosing LOWER phase is invisible to the global
			// table, so the clause must claim only the global table.
			name: "a phase-0 local read from a transformer body is a global absence only",
			src:  `(let ((loc 1)) (define-syntax q (lambda (stx) loc)) (q))`,
			tail: `no such local or global binding "loc" at phase 1 of this unit's macro tower (no global binding of that name at any phase): no such binding`,
		},
		{
			name: "the let-syntax form of the same local",
			src:  `(let ((yy 1)) (let-syntax ((m (lambda (stx) yy))) (m)))`,
			tail: `no such local or global binding "yy" at phase 1 of this unit's macro tower (no global binding of that name at any phase): no such binding`,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := evalForPhaseDiagnostic(t, tc.src)
			qt.Assert(t, strings.HasSuffix(got, tc.tail), qt.IsTrue,
				qt.Commentf("want tail: %s\ngot: %s", tc.tail, got))
		})
	}
}

// TestUnboundDiagnosticSeesForTemplateImports pins that the cross-phase probe
// reaches phase -1. PresentPhases drops negative phases on purpose (resolution
// ranks runtime-first), so a probe built on it alone called a name imported
// for-template "bound at no phase".
func TestUnboundDiagnosticSeesForTemplateImports(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	root := t.TempDir()
	err := os.MkdirAll(filepath.Join(root, "tpl"), 0o755)
	qt.Assert(t, err, qt.IsNil)
	err = os.WriteFile(filepath.Join(root, "tpl", "lib.sld"),
		[]byte(`(define-library (tpl lib) (export tplonly) (import) (begin (define tplonly 1)))`), 0o644)
	qt.Assert(t, err, qt.IsNil)

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink), wile.WithSourceOS(), wile.WithLibraryPaths(root))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	_, err = eng.EvalMultiple(ctx, `(import (for-template (tpl lib))) tplonly`)
	qt.Assert(t, err, qt.IsNotNil)
	want := `no such local or global binding "tplonly" (bound at phase -1): no such binding`
	qt.Assert(t, strings.HasSuffix(err.Error(), want), qt.IsTrue,
		qt.Commentf("want tail: %s\ngot: %v", want, err))

	_, err = eng.EvalMultiple(ctx, `(set! tplonly 2)`)
	qt.Assert(t, err, qt.IsNotNil)
	want = `no such binding "tplonly" with compatible scopes for set! (bound at phase -1): no such binding`
	qt.Assert(t, strings.HasSuffix(err.Error(), want), qt.IsTrue,
		qt.Commentf("want tail: %s\ngot: %v", want, err))
}

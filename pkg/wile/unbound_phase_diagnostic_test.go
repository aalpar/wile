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
// Plan: memory/2026-09-08-flatt-binding-model-a-impl.local.md, Task 1;
// design §6.6. Stage A makes "bound at phase 0, referenced at phase 1" a
// ROUTINE failure for code that reads as correct — a procedural transformer
// body that calls cadr is the canonical case — so a message carrying only the
// name sends the reader hunting for a typo that is not there.
//
// Every program below already raises on master — three at phase 1, one per
// raise site, plus a phase-2 case for the rendering — so the only thing Stage
// A's edit moves is the text. That is deliberate: a test whose RED state is
// "nothing raises" cannot be turned green by a wrap-message change. The case
// where a name stops resolving at all is
// TestPhase1_ProceduralTransformerUnboundWithoutImport (Task 2), green only
// after the ambient tier is deleted in Task 7.
//
// Do NOT rewrite these onto the census names (assoc, caar, vector-map,
// string-map, make-parameter). Measured on master, every one of them resolves
// inside a transformer body with err == nil: "phase-0-only" in the
// binding_tier_census pins means exact@1 == false, not invisible at phase 1,
// because the ambient tier still covers them.

import (
	"context"
	"errors"
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
// Phase 0 renders no phase clause at all, following Racket, whose phase-0
// unbound message says nothing about phase and whose phase-1 one names it in
// words ("no #%app syntax transformer is bound in the transformer phase" —
// measured against /opt/local/bin/racket 9.2, 2026-09-09, on
// `#lang racket/base` + an unbound reference at each phase). What that buys is
// the direction asserted here: ABSENCE rules phase out, and the far commoner
// phase-0 message stays byte-identical, which is why none of the 37 existing
// assertions on this text needed touching.
//
// Presence rules nothing in — see phaseSuffix's doc. The gate is the compile
// phase alone, so the clause also renders for a typo compiled at phase 1.
func TestUnboundDiagnosticStaysSilentAtRuntimePhase(t *testing.T) {
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

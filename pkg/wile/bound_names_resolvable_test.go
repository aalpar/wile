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
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// Engine.BoundNames is a tab-completion listing, so enumerate-then-dereference
// has to work: every name it offers must resolve. I095.
//
// A macro template's binder is a DIFFERENT variable that happens to share a
// name — it carries the intro scope, so no source-written reference reaches it —
// and the listing offered it anyway, because it ranged every live slot with no
// scope or phase test. The other Wile listing already agreed the name is
// unreachable, so the two disagreed with each other as well as with the
// dereference.
func boundNamesEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(context.Background(),
		`(import (scheme base) (scheme write) (scheme char))
(define-syntax defhidden (syntax-rules () ((_ v) (define hidden v))))
(defhidden 42)`)
	qt.Assert(t, err, qt.IsNil)
	return eng
}

func TestW2I095HiddenNotOffered(t *testing.T) {
	eng := boundNamesEngine(t)

	// The macro-introduced binder exists in the store and is unreachable. Both
	// halves are asserted, because "not offered" is only a defect in company
	// with "cannot be dereferenced".
	_, err := eng.EvalMultiple(context.Background(), "hidden")
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("precondition: the template-introduced binder must be unreachable"))

	names := eng.BoundNames()
	qt.Assert(t, slices.Contains(names, "hidden"), qt.IsFalse,
		qt.Commentf("BoundNames offered a name whose dereference raises"))

	// MEMBERSHIP, not cardinality — though the cardinality was MEASURED and is
	// exactly as filed: on this engine the unfiltered walk yields 587 distinct
	// names, the filtered one 586, and `hidden` is the sole drop. It is not
	// asserted because it would rot on any stdlib change and because no
	// Scheme-level accessor reaches this walk, so a failure here would be
	// unreproducible from Scheme. "Sole drop" also holds only by accident of the
	// current stdlib: any bootstrap or stdlib macro expanding to a top-level
	// define of a template-introduced name is already offered, already
	// unreachable, and would be dropped by this filter too.
	qt.Assert(t, len(names) > 100, qt.IsTrue,
		qt.Commentf("sanity: the listing is still a listing, got %d names", len(names)))

	// The non-vacuity guard, and it has to be the PHASE-1 names. Measured phase
	// by phase: let*, letrec and syntax-rules are PHASE-0 special forms and say
	// nothing about a per-slot-phase predicate, so they are vacuous here. These
	// live at phase 1 and are the only rows that exercise it.
	//
	// The guard BITES, verified by building the wrong fix: keying the predicate
	// on PhaseRuntime instead of the slot's own phase fails this loop at "when".
	// That is the one way this change could have been worse than the defect it
	// fixes, and it is the row that catches it.
	for _, kw := range []string{
		"when", "unless", "do", "case", "guard", "define-record-type", "parameterize",
		"and", "or", "cond", "delay",
	} {
		qt.Assert(t, slices.Contains(names, kw), qt.IsTrue,
			qt.Commentf("phase-1 keyword %q dropped out of the listing", kw))
	}

	// The phase-0 three the plan lists, kept as plain regression rows and
	// labelled so nobody reads them as evidence about the phase axis.
	for _, kw := range []string{"let*", "letrec", "syntax-rules", "lambda", "define", "if"} {
		qt.Assert(t, slices.Contains(names, kw), qt.IsTrue,
			qt.Commentf("phase-0 keyword %q dropped out of the listing", kw))
	}
}

// The two listings must agree. environment-bound-names already reports `hidden`
// absent, which is what made the disagreement visible in the first place.
func TestW2I095ListingsAgree(t *testing.T) {
	eng := boundNamesEngine(t)

	v, err := eng.EvalMultiple(context.Background(),
		"(memq 'hidden (environment-bound-names (interaction-environment)))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "#f",
		qt.Commentf("the phase-0 listing's answer, green before and after"))
}

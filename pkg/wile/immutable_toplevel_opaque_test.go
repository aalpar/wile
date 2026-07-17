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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// TestImmutableTopLevel_OpaqueSubtreeOverMark pins what an opaque subtree does to
// top-level immutability. validate.markOpaqueSubtree marks only the EVALUATED
// positions of a quasiquote as possible set! targets; a marked name never gets the
// Stable stamp, and the set! rejection keys on Stable. So a name that appears only
// as template DATA keeps its immutability, while a name a live unquote reaches
// loses it.
//
// The comparison must span two compilation units. An in-unit set! marks its own
// target, so a same-unit probe cannot isolate the template's contribution — which
// is why the review's `(begin `(x) (set! x 1))` repro showed nothing (it has no
// define to stamp either).
//
// The depth-aware walk (fix/opaque-subtree-quasi-depth) draws the line: `(x)
// mentions x as DATA no unquote reaches, so x KEEPS its stamp and a later set! is
// rejected; `(,(set! x 9)) puts x in an evaluated position, so the mark is earned.
// The table below pins both directions.
func TestImmutableTopLevel_OpaqueSubtreeOverMark(t *testing.T) {
	ctx := context.Background()
	tcs := []struct {
		name string
		// unit is compiled first, under the default immutable top level.
		unit string
		// setRejected is whether a LATER unit's (set! x 2) is refused, i.e.
		// whether x kept the Stable stamp.
		setRejected bool
		why         string
	}{
		{
			name:        "define alone is stable",
			unit:        `(define x 1)`,
			setRejected: true,
			why:         "defined once, never set!: the stamp lands and enforcement follows",
		},
		{
			name:        "quasiquote mentioning x leaves the stamp alone",
			unit:        "(begin (define x 1) `(x))",
			setRejected: true,
			why:         "x is template DATA no unquote reaches, so it is not a set! target",
		},
		{
			name:        "nested template keeps an inner unquote as data",
			unit:        "(begin (define x 1) `(a `(b ,(set! x 9))))",
			setRejected: true,
			why:         "one unquote under two quasiquotes lands at depth 1: data, not evaluated",
		},
		{
			name:        "quote inside a template is not a barrier",
			unit:        "(begin (define x 1) `(a '(b ,(set! x 9))))",
			setRejected: false,
			why:         "R7RS §4.2.6: unquotes under a quoted template stay live, so the set! is real",
		},
		{
			name:        "dotted unquote reaches the tail",
			unit:        "(begin (define x 1) `(a . ,(set! x 9)))",
			setRejected: false,
			why:         "`(a . ,e) parses as (a unquote e): a bare unquote in the SPINE, still live",
		},
		{
			name:        "quasiquote unquoting a set! of x",
			unit:        "(begin (define x 1) `(,(set! x 9)))",
			setRejected: false,
			why:         "a real set! hides here; the mark is earned, not imprecision",
		},
		{
			name:        "quote does not over-mark",
			unit:        `(begin (define x 1) '(x))`,
			setRejected: true,
			why:         "quote validates to *ValidatedQuote, which opaqueRawSyntax never matches",
		},
		{
			name:        "marking is per name, not per unit",
			unit:        "(begin (define x 1) `(y))",
			setRejected: true,
			why:         "only mentioned names are marked; an opaque subtree does not taint the unit",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(ctx, WithProfile(Small), WithImmutableTopLevel())
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()

			_, err = eng.EvalMultiple(ctx, tc.unit)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("unit itself must compile"))

			_, err = eng.EvalMultiple(ctx, `(set! x 2)`)
			if !tc.setRejected {
				qt.Assert(t, err, qt.IsNil, qt.Commentf("%s", tc.why))
				return
			}
			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("%s", tc.why))
			qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
				qt.Commentf("want the immutability sentinel, got %v", err))
		})
	}
}

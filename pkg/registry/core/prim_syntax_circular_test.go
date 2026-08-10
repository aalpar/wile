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

package core_test

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

// TestDatumToSyntaxRefusesCircularDatum covers every aggregate arm that
// recurses. Each of these used to kill the HOST PROCESS with `fatal error:
// stack overflow` — a runtime throw, not a panic, which no recover can
// intercept, so an embedder lost every engine in the process rather than one
// evaluation.
//
// The deep-proper-list half of the same defect is NOT gated here, and that is
// deliberate. The old converter recursed once per cdr, so list LENGTH became
// Go stack depth — DefaultMaxParseDepth counts nesting, not length, and
// bounded nothing. But the threshold is high: measured against the pre-fix
// binary, 1,000,000 elements still returned and 4,000,000 overflowed, while
// the fixed binary answers 4,000,000 in about three seconds. A gate at 4M
// costs seconds and hundreds of megabytes in every `make test`, and a gate at
// any affordable size is green in both directions — a test that cannot fail.
// The iterative spine ships on that measurement, recorded in the commit.
func TestDatumToSyntaxRefusesCircularDatum(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "cdr-cycle", Code: `(let ((x (list 1))) (set-cdr! x x) (datum->syntax #f x))`},
		{Name: "car-cycle", Code: `(let ((x (list 1))) (set-car! x x) (datum->syntax #f x))`},
		{Name: "vector-self-reference", Code: `(let ((v (vector 0))) (vector-set! v 0 v) (datum->syntax #f v))`},
		// The cycle closes on the THIRD cell, not the head: a visited set that
		// only remembered the entry point would walk straight past this.
		{Name: "cycle-on-inner-cell", Code: `(let ((x (list 1 2 3))) (set-cdr! (cddr x) (cdr x)) (datum->syntax #f x))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
			// WHICH error: any sentinel would satisfy IsNotNil, and the depth
			// bound below raises a different one on inputs that look similar.
			qt.Assert(t, errors.Is(err, werr.ErrCircularList), qt.IsTrue,
				qt.Commentf("want ErrCircularList, got %v", err))
		})
	}
}

// TestDatumToSyntaxCircularIsCatchable pins the classification. A circular
// argument is a domain error, so per CODING_STYLE.md's line it must be a
// catchable condition; the stack overflow it replaced was not catchable by
// anything, at any layer.
func TestDatumToSyntaxCircularIsCatchable(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t,
		`(guard (e (#t 'caught)) (let ((x (list 1))) (set-cdr! x x) (datum->syntax #f x)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("caught"))
}

// TestDatumToSyntaxSharedStructureStillConverts is the control that keeps the
// visited set PATH-scoped. A DAG is not a cycle: if the set were global rather
// than unwound on the way out, this would be refused too.
func TestDatumToSyntaxSharedStructureStillConverts(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((shared (list 1 2)))
		  (syntax->datum (datum->syntax #f (list shared shared))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals,
		values.List(values.List(values.NewInteger(1), values.NewInteger(2)),
			values.List(values.NewInteger(1), values.NewInteger(2))))
}

// TestDatumToSyntaxRefusesTooDeeplyNestedDatum is the third leg. The cycle
// refusal and the iterative spine handle circularity and LENGTH; NESTING is
// the one that stays recursive, and no reader bound reaches a datum built at
// runtime — which is exactly where these arrive from.
//
// Measured before the bound: 200,000 levels converted, 2,000,000 killed the
// host process with `fatal error: stack overflow`. It is now refused with a
// catchable error naming the limit, at any depth.
func TestDatumToSyntaxRefusesTooDeeplyNestedDatum(t *testing.T) {
	deep := `(let loop ((i 0) (acc '()))
	           (if (= i 20000) acc (loop (+ i 1) (list acc))))`

	_, err := testhelpers.RunSchemeCode(t, `(datum->syntax #f `+deep+`)`)
	qt.Assert(t, err, qt.IsNotNil)
	// The depth sentinel, NOT the cycle sentinel: this datum is acyclic, and
	// conflating the two would hide a real cycle-detector regression.
	qt.Assert(t, errors.Is(err, werr.ErrParseDepthExceeded), qt.IsTrue,
		qt.Commentf("want ErrParseDepthExceeded, got %v", err))
	qt.Assert(t, errors.Is(err, werr.ErrCircularList), qt.IsFalse)

	// A domain error, so guard must catch it — the stack overflow it replaced
	// was catchable by nothing, at any layer.
	caught, err := testhelpers.RunSchemeCode(t,
		`(guard (e (#t 'caught)) (datum->syntax #f `+deep+`))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, caught, valuestest.SchemeEquals, values.NewSymbol("caught"))

	// The control: well under the bound still converts, so the limit narrows
	// rather than forbids. LENGTH is deliberately unbounded — a long list is
	// not a deep one — and the shared-structure row above covers that the
	// bound is not accidentally counting spine cells.
	shallow, err := testhelpers.RunSchemeCode(t, `
		(let loop ((i 0) (acc '()))
		  (if (= i 5000)
		      (begin (datum->syntax #f acc) 'ok)
		      (loop (+ i 1) (list acc))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, shallow, valuestest.SchemeEquals, values.NewSymbol("ok"))
}

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

// A phase-1 definition must be visible to a define-syntax later in the SAME
// compilation unit.
//
// RED on fcf7b99c: every row but the guard raised `no such binding "helper"
// with compatible scopes at phase 1 of this unit's macro tower`. The same forms
// passed one unit at a time (EvalMultiple, the REPL), which is why the defect
// hid: a file is one (begin ...) unit, and so is a library body.
//
// Root cause: define-for-syntax, begin-for-syntax and eval-when were
// expandUnchanged, so their compile-time half ran only when the COMPILER reached
// them, while a define-syntax later in the unit compiles its transformer during
// the EXPANDER's body scan. Within one unit the transformer was therefore built
// before the phase-1 definition existed. The expander now runs that half when it
// expands the form, and the compiler no longer runs it a second time.
//
// (for-syntax (scheme base)) supplies cadr at phase 1, so the rows isolate the
// phase-1 definition rather than the startup set's phase-1 reach.

import (
	"context"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"
)

func TestPhase1DefinitionIsVisibleWithinItsUnit(t *testing.T) {
	const macro = `(define-syntax m (er-macro-transformer (lambda (f r c) (helper (cadr f)))))`
	tests := []struct {
		name string
		src  string
	}{
		{
			name: "define-for-syntax",
			src: `(import (for-syntax (scheme base)))
			      (define-for-syntax (helper x) (* x 2))
			      ` + macro + `
			      (m 21)`,
		},
		{
			name: "begin-for-syntax",
			src: `(import (for-syntax (scheme base)))
			      (begin-for-syntax (define (helper x) (* x 2)))
			      ` + macro + `
			      (m 21)`,
		},
		{
			// The third form sharing the compile-time executor.
			name: "eval-when expand",
			src: `(import (for-syntax (scheme base)))
			      (eval-when (expand) (define (helper x) (* x 2)))
			      ` + macro + `
			      (m 21)`,
		},
		{
			// A lambda body runs the same body scan as the top-level begin.
			name: "define-for-syntax in a let body",
			src: `(import (for-syntax (scheme base)))
			      (let ()
			        (define-for-syntax (helper x) (* x 2))
			        ` + macro + `
			        (m 21))`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := phaseDistinctnessEngine(t)
			v, err := eng.EvalProgram(context.Background(), tt.src, "")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, "42")
		})
	}
}

// A library body is one unit for the same reason a file is.
func TestPhase1DefinitionIsVisibleWithinALibraryBody(t *testing.T) {
	tests := []struct {
		name string
		def  string
	}{
		{name: "define-for-syntax", def: `(define-for-syntax (helper x) (* x 2))`},
		{name: "begin-for-syntax", def: `(begin-for-syntax (define (helper x) (* x 2)))`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := phaseIsolationEngine(t, fstest.MapFS{
				"within.scm": &fstest.MapFile{Data: []byte(`(define-library (within)
  (export go)
  (import (scheme base) (for-syntax (scheme base)))
  (begin
    ` + tt.def + `
    (define-syntax m (er-macro-transformer (lambda (f r c) (helper (cadr f)))))
    (define (go) (m 21))))
`)},
			})
			v, err := eng.EvalMultiple(context.Background(), `(import (within)) (go)`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, "42")
		})
	}
}

// A phase-1 form runs EXACTLY ONCE per unit. Moving its execution into the
// expander is only sound if the compiler stops executing it: a second run would
// repeat every begin-for-syntax and eval-when side effect, and re-bind a
// define-for-syntax to a freshly evaluated value after transformers had already
// used the first.
//
// A second run would happen at COMPILE time, after every expansion in its unit,
// so no transformer in that unit could see it. The counting unit is therefore
// read back from a LATER unit.
func TestPhase1FormRunsOncePerUnit(t *testing.T) {
	ctx := context.Background()
	eng := phaseDistinctnessEngine(t)
	_, err := eng.EvalMultiple(ctx, `(import (for-syntax (scheme base)))
		(begin-for-syntax (define count 0))`)
	qt.Assert(t, err, qt.IsNil)
	_, err = eng.EvalProgram(ctx,
		`(begin-for-syntax (set! count (+ count 1)))
		 (eval-when (expand) (set! count (+ count 1)))
		 (define-for-syntax snapshot (begin (set! count (+ count 1)) count))`, "")
	qt.Assert(t, err, qt.IsNil)
	v, err := eng.EvalMultiple(ctx,
		`(define-syntax reveal (er-macro-transformer (lambda (f r c) (list 'quote (list count snapshot)))))
		 (reveal)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "(3 3)")
}

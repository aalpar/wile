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
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func TestReadExpression(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name       string
		input      string
		wantErr    bool
		incomplete bool
		wantValue  string // SchemeString of eval result (only checked if !wantErr)
	}{
		{"simple atom", "42", false, false, "42"},
		{"list expression", "(+ 1 2)", false, false, "3"},
		{"string literal", `"hello"`, false, false, `"hello"`},
		{"incomplete paren", "(+ 1", true, true, ""},
		{"incomplete string", `"hello`, true, true, ""},
		{"empty input", "", true, true, ""},
		{"trailing input ignored", "(+ 1 2) (+ 3 4)", false, false, "3"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			r := strings.NewReader(tc.input)
			expr, parseErr := eng.ReadExpression(ctx, r)
			if tc.wantErr {
				qt.Assert(t, parseErr, qt.IsNotNil)
				qt.Assert(t, wile.IsIncompleteInput(parseErr), qt.Equals, tc.incomplete)
				return
			}
			qt.Assert(t, parseErr, qt.IsNil)
			cc, compileErr := eng.Compile(ctx, expr)
			qt.Assert(t, compileErr, qt.IsNil)
			val, runErr := eng.Run(ctx, cc)
			qt.Assert(t, runErr, qt.IsNil)
			qt.Assert(t, val.SchemeString(), qt.Equals, tc.wantValue)
		})
	}
}

// TestReadExpressions covers the multi-form read that backs the REPL drain:
// every complete form on a line is returned (not just the first), a trailing
// incomplete form is reported as IsIncompleteInput without dropping it, and a
// genuine syntax error is distinguished from end-of-input.
func TestReadExpressions(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name       string
		input      string
		wantErr    bool
		incomplete bool
		wantValues []string // SchemeString of each form's eval result (only if !wantErr)
	}{
		{"single form", "42", false, false, []string{"42"}},
		{"two value forms", "1 2", false, false, []string{"1", "2"}},
		{"three value forms", "1 2 3", false, false, []string{"1", "2", "3"}},
		{"adjacent forms no space", "(+ 1 2)(* 3 4)", false, false, []string{"3", "12"}},
		// Backward references work: the define is run before the use is
		// compiled, because the REPL drain compiles+runs each form in turn.
		{"define then use in one buffer", "(define x 5) (* x x)", false, false, []string{"", "25"}},
		{"complete forms then incomplete tail", "(+ 1 2) (+ 3", true, true, nil},
		{"unbalanced single form", "(+ 1", true, true, nil},
		{"hard syntax error", "(+ 1 2) )", true, false, nil},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			exprs, parseErr := eng.ReadExpressions(ctx, strings.NewReader(tc.input))
			if tc.wantErr {
				qt.Assert(t, parseErr, qt.IsNotNil)
				qt.Assert(t, wile.IsIncompleteInput(parseErr), qt.Equals, tc.incomplete)
				return
			}
			qt.Assert(t, parseErr, qt.IsNil)
			qt.Assert(t, len(exprs), qt.Equals, len(tc.wantValues))
			for i, expr := range exprs {
				cc, compileErr := eng.Compile(ctx, expr)
				qt.Assert(t, compileErr, qt.IsNil)
				val, runErr := eng.Run(ctx, cc)
				qt.Assert(t, runErr, qt.IsNil)
				want := tc.wantValues[i]
				if want == "" {
					qt.Assert(t, val.IsVoid(), qt.IsTrue,
						qt.Commentf("form %d expected void", i))
					continue
				}
				qt.Assert(t, val.SchemeString(), qt.Equals, want)
			}
		})
	}
}

// TestReadExpressions_ForwardReferenceBoundary pins the one semantic where the
// REPL's per-form drain differs from begin/file evaluation. ReadExpressions
// parses all forms fine, but the drain compiles+runs each independently, so a
// forward reference — a body naming a global defined by a *later* form — fails
// to compile, exactly as typing the forms on separate REPL lines does today.
// Wrapping the buffer in a single (begin ...) would instead resolve it (the
// whole body compiles with all defines in scope); we deliberately keep per-form
// evaluation so each form's value still echoes. This test is the reference
// point if that trade-off is ever revisited.
func TestReadExpressions_ForwardReferenceBoundary(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	exprs, parseErr := eng.ReadExpressions(ctx,
		strings.NewReader("(define (f) (g)) (define (g) 7) (f)"))
	qt.Assert(t, parseErr, qt.IsNil)
	qt.Assert(t, len(exprs), qt.Equals, 3)

	// First form parses, but compiling it references the not-yet-defined g.
	_, compileErr := eng.Compile(ctx, exprs[0])
	qt.Assert(t, compileErr, qt.IsNotNil)
}

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

package integration_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestDatumLabel_AssignmentDoesNotSurviveIntoTheValue pins that a #n= label
// assignment reads as the labeled datum itself. The label table is populated
// before the datum is returned, so the wrapper carried nothing the reader
// needed — but it reached the compiler as an opaque literal, which made
// #0=(+ 1 2) evaluate to the list (+ 1 2), #0=x evaluate to the symbol x, and
// (#0=+ 1 2) fail with "expected a procedure, got +".
func TestDatumLabel_AssignmentDoesNotSurviveIntoTheValue(t *testing.T) {
	tests := []struct {
		name string
		code string
		want string
	}{
		{
			name: "labeled combination evaluates",
			code: `#0=(+ 1 2)`,
			want: "3",
		},
		{
			name: "labeled variable reference resolves",
			code: `(define x 5) #0=x`,
			want: "5",
		},
		{
			name: "labeled operator position applies",
			code: `(#0=+ 1 2)`,
			want: "3",
		},
		{
			name: "labeled datum inside a quote keeps its shape",
			code: `'#0=(+ 1 2)`,
			want: "(+ 1 2)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestDatumLabel_SyntaxObjectRendersAsSyntax pins the escalation: SchemeString
// is reachable from Scheme, and the assignment wrapper rendered as its bare
// label number, so (write (syntax #0=(a b))) printed "0".
func TestDatumLabel_SyntaxObjectRendersAsSyntax(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(let ((out (open-output-string)))
		  (write (syntax #0=(a b)) out)
		  (get-output-string out))
	`))
	c.Assert(err, qt.IsNil)
	got := result.SchemeString()
	c.Assert(got, qt.Not(qt.Equals), `"0"`,
		qt.Commentf("the label wrapper's SchemeString leaked into the syntax object's rendering"))
	c.Assert(strings.Contains(got, "a"), qt.IsTrue, qt.Commentf("got %s", got))
	c.Assert(strings.Contains(got, "b"), qt.IsTrue, qt.Commentf("got %s", got))
}

// TestDatumLabel_CircularLabelIsRefusedNotWalked pins the boundary of the
// unwrap. A SELF-REFERENTIAL labeled datum keeps its assignment wrapper, which
// is what stops the expander walking the cycle: Wile's refusal of a circular
// datum label as code lives in the compiler (validateQuotedLiteral), one stage
// AFTER the expander's argument walk, so an unwrapped cycle in operand position
// is an unbounded expansion rather than a diagnostic. Each case below must
// return a bounded ErrInvalidSyntax, and the timeout is the real assertion.
func TestDatumLabel_CircularLabelIsRefusedNotWalked(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{name: "operand position", code: `(car #0=(a . #0#))`},
		{name: "quoted circular list", code: `'#0=(a . #0#)`},
		{name: "quoted circular vector", code: `'#0=#(a #0#)`},
		{name: "operator position", code: `(#0=(a . #0#) 1)`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
			c.Assert(err, qt.IsNil)
			ctx, cancel := context.WithTimeout(context.Background(), 20*time.Second)
			defer cancel()
			_, err = engine.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNotNil)
			c.Assert(err, qt.ErrorIs, werr.ErrInvalidSyntax)
		})
	}
}

// TestDatumLabel_SharedStructureStillReads is a control: unwrapping the
// assignment must not disturb #n# back-references, which resolve out of the
// label table populated before the datum is returned.
func TestDatumLabel_SharedStructureStillReads(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(context.Background(),
		engine.MustParse(context.Background(), `(eq? (car '(#0=(a) #0#)) (cadr '(#0=(a) #0#)))`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

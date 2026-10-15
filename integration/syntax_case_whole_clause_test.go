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
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestI173SyntaxCaseWholeClausePatterns: a syntax-case clause pattern may be any
// R6RS §12.4 pattern, not only a list, and an input of any shape is matched
// against it. The five-shape matrix is the enumeration the old pair assertion
// hid; a narrowing that re-admits only some shapes fails the rest.
//
// Expected column: Chez 10.4.1 (petite -q --script) and Racket 9.2,
// byte-identical.
func TestI173SyntaxCaseWholeClausePatterns(t *testing.T) {
	const shapes = `(define (t x)
	  (list (syntax-case x () (a 1))
	        (syntax-case x () (_ 2))
	        (syntax-case x () (() 3) (_ 0))
	        (syntax-case x () (#(a b) 4) (_ 0))
	        (syntax-case x () (5 5) (_ 0))))`
	tcs := []struct {
		name     string
		program  string
		expected string
	}{
		{
			name:     "five_shapes_list_input",
			program:  shapes + ` (t #'(p q))`,
			expected: "(1 2 0 0 0)",
		},
		{
			name:     "five_shapes_vector_input",
			program:  shapes + ` (t #'#(p q))`,
			expected: "(1 2 0 4 0)",
		},
		{
			name:     "five_shapes_constant_input",
			program:  shapes + ` (t #'5)`,
			expected: "(1 2 0 0 5)",
		},
		{
			name:     "five_shapes_empty_input",
			program:  shapes + ` (t #'())`,
			expected: "(1 2 3 0 0)",
		},
		{
			name:     "bare_variable_binds_whole_input",
			program:  `(syntax->datum (syntax-case #'(p q) () (a #'a)))`,
			expected: "(p q)",
		},
		{
			name:     "empty_input_matches_ellipsis_list",
			program:  `(syntax-case #'() () ((a ...) 'ell) (_ 'other))`,
			expected: "ell",
		},
		{
			name:     "non_pair_input_falls_through_to_next_clause",
			program:  `(syntax-case #'5 () ((a b) 'pair) (_ 'other))`,
			expected: "other",
		},
		{
			name:     "dotted_pattern",
			program:  `(syntax->datum (syntax-case #'(1 . 2) () ((a . b) #'(b a))))`,
			expected: "(2 1)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.program)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestI033SyntaxCaseNonPairInputIsNoMatch: a non-pair input against a list
// pattern is a non-match, so with no other clause syntax-case reports that no
// clause matched (R6RS §12.4) rather than an internal matcher error.
func TestI033SyntaxCaseNonPairInputIsNoMatch(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	_, err = engine.EvalMultiple(context.Background(), `(syntax-case #'5 () ((a b) 'pair))`)
	c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue, qt.Commentf("err: %v", err))
	c.Assert(strings.Contains(err.Error(), "no matching clause"), qt.IsTrue, qt.Commentf("err: %v", err))
}

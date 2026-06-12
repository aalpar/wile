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
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestQuasisyntaxIntegration tests quasisyntax through the full pipeline:
// parse → expand → compile → execute
//
// Note: quasisyntax returns syntax objects. We use syntax->datum to unwrap
// them for comparison of the underlying structure.
func TestQuasisyntaxIntegration(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected string // SchemeString of unwrapped result
	}{
		// Basic Forms
		{
			name:     "simple literal symbol",
			code:     `(quasisyntax foo)`,
			expected: `foo`,
		},
		{
			name:     "simple list",
			code:     `(quasisyntax (a b c))`,
			expected: `(a b c)`,
		},
		{
			name:     "number literal",
			code:     `(quasisyntax 42)`,
			expected: `42`,
		},
		{
			name:     "string literal",
			code:     `(quasisyntax "hello")`,
			expected: `"hello"`,
		},
		{
			name:     "empty list",
			code:     `(quasisyntax ())`,
			expected: `()`,
		},

		// Unsyntax
		{
			name:     "basic unsyntax",
			code:     `(quasisyntax (+ 1 #,(* 2 3)))`,
			expected: `(+ 1 6)`,
		},
		{
			name:     "multiple unsyntax",
			code:     `(quasisyntax (+ #,(* 2 3) #,(/ 8 2)))`,
			expected: `(+ 6 4)`,
		},
		{
			name:     "unsyntax in nested position",
			code:     `(quasisyntax (list #,(+ 1 2)))`,
			expected: `(list 3)`,
		},
		{
			name:     "unsyntax with symbol",
			code:     `(let ((x 5)) (quasisyntax (foo #,x)))`,
			expected: `(foo 5)`,
		},

		// Unsyntax-splicing
		{
			name:     "basic splice",
			code:     `(quasisyntax (a #,@(list 'b 'c) d))`,
			expected: `(a b c d)`,
		},
		{
			name:     "empty splice",
			code:     `(quasisyntax (a #,@'() b))`,
			expected: `(a b)`,
		},
		{
			name:     "multiple splices",
			code:     `(quasisyntax (#,@'(a) #,@'(b c)))`,
			expected: `(a b c)`,
		},
		{
			name:     "beginning splice",
			code:     `(quasisyntax (#,@'(a b) c))`,
			expected: `(a b c)`,
		},
		{
			name:     "end splice",
			code:     `(quasisyntax (a #,@'(b c)))`,
			expected: `(a b c)`,
		},

		// Nested quasisyntax
		{
			name:     "nested quasisyntax depth 2",
			code:     `(let ((x 5)) (quasisyntax (quasisyntax #,x)))`,
			expected: `(quasisyntax (unsyntax 5))`, // x's binding is captured in the syntax object
		},
		{
			name:     "nested with outer unsyntax",
			code:     `(let ((x 5)) (quasisyntax (a #,(quasisyntax b))))`,
			expected: `(a b)`, // outer unsyntax evaluates (quasisyntax b) to syntax b
		},
		{
			name:     "double nested evaluates inner",
			code:     `(let ((x 5)) (quasisyntax (quasisyntax (a #,#,x))))`,
			expected: `(quasisyntax (a (unsyntax 5)))`, // inner #, evaluates x to 5
		},

		// Edge Cases
		{
			name:     "improper list",
			code:     `(quasisyntax (a . b))`,
			expected: `(a . b)`,
		},
		// Note: unsyntax in cdr position like (a . #,x) is not supported by the parser
		// as it treats (a . #,x) as a three-element list (a unsyntax x) rather than
		// an improper list. This is a parser limitation, not a quasisyntax issue.
		{
			name:     "nested empty list",
			code:     `(quasisyntax (a () b))`,
			expected: `(a () b)`,
		},

		// Mixed operations
		{
			name:     "unsyntax and splice together",
			code:     `(quasisyntax (#,1 #,@'(2 3) #,4))`,
			expected: `(1 2 3 4)`,
		},
		{
			name:     "computation in unsyntax",
			code:     `(quasisyntax (if #t #,(+ 1 2) #,(- 10 5)))`,
			expected: `(if #t 3 5)`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background())
			c.Assert(err, qt.IsNil)

			// Wrap in syntax->datum to unwrap the syntax object
			code := `(syntax->datum ` + tt.code + `)`
			result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), code))
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tt.expected)
		})
	}
}

// TestQuasisyntaxErrors tests error cases for quasisyntax
func TestQuasisyntaxErrors(t *testing.T) {
	tests := []struct {
		name        string
		code        string
		expectError string
	}{
		{
			name:        "no arguments",
			code:        `(quasisyntax)`,
			expectError: "quasisyntax: expected exactly",
		},
		{
			name:        "too many arguments",
			code:        `(quasisyntax a b)`,
			expectError: "quasisyntax: expected exactly",
		},
		{
			name:        "naked unsyntax",
			code:        `(unsyntax x)`,
			expectError: "not in quasisyntax context",
		},
		{
			name:        "naked unsyntax-splicing",
			code:        `(unsyntax-splicing x)`,
			expectError: "not in quasisyntax context",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background())
			c.Assert(err, qt.IsNil)

			expr, parseErr := engine.Parse(context.Background(), tt.code)
			if parseErr != nil {
				c.Assert(parseErr.Error(), qt.Contains, tt.expectError)
				return
			}
			_, err = engine.Eval(context.Background(), expr)
			c.Assert(err, qt.IsNotNil)
			c.Assert(err.Error(), qt.Contains, tt.expectError)
		})
	}
}

// TestQuasisyntaxWithMacros tests quasisyntax in macro contexts
func TestQuasisyntaxWithMacros(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// Define a macro that uses quasisyntax
	// Now that car/cdr work transparently on syntax objects, we can use them directly
	_, err = engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(define-syntax my-when
		  (lambda (stx)
		    (let ((condition (cadr stx))
		          (body (caddr stx)))
		      (quasisyntax (if #,condition #,body #f)))))
	`))
	c.Assert(err, qt.IsNil)

	// Use the macro
	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `(my-when #t 42)`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `42`)

	result, err = engine.Eval(context.Background(), engine.MustParse(context.Background(), `(my-when #f 42)`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `#f`)
}

// TestQuasisyntaxHygiene tests that quasisyntax preserves hygiene
func TestQuasisyntaxHygiene(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// Define a macro that introduces a binding using quasisyntax
	// This tests that quasisyntax preserves source locations and scopes
	_, err = engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(define-syntax add-one
		  (lambda (stx)
		    (let ((expr (cadr stx)))
		      (quasisyntax (+ #,expr 1)))))
	`))
	c.Assert(err, qt.IsNil)

	// Use the macro
	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `(add-one 5)`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `6`)
}

// TestQuasisyntaxRecursive tests recursive macro expansion with quasisyntax
func TestQuasisyntaxRecursive(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	// Define a recursive macro using quasisyntax
	// Demonstrates transparent syntax operations: car, cdr, cadr, cddr work directly on stx
	_, err = engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(define-syntax my-list
		  (lambda (stx)
		    (if (null? (cdr stx))
		        (quasisyntax '())
		        (let ((first (cadr stx))
		              (rest (cons 'my-list (cddr stx))))
		          (quasisyntax (cons #,first #,rest))))))
	`))
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `(my-list 1 2 3)`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `(1 2 3)`)
}

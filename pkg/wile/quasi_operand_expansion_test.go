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
	"errors"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// syntaxLayerRow is one program and its expected rendering.
type syntaxLayerRow struct {
	name     string
	program  string
	expected string
}

// runUnderBothSyntaxLayers runs each row under both syntax layers. CI sets no
// WILE_SYNTAX_FORMS, so a row run only under the default never sees the Scheme
// layer.
func runUnderBothSyntaxLayers(t *testing.T, tcs []syntaxLayerRow) {
	layers := []struct {
		name string
		opt  wile.EngineOption
	}{
		{name: "go", opt: wile.WithGoSyntaxForms()},
		{name: "scheme", opt: wile.WithSchemeSyntaxForms()},
	}
	for _, l := range layers {
		for _, tc := range tcs {
			t.Run(l.name+"/"+tc.name, func(t *testing.T) {
				qt.New(t).Assert(evalSyntaxForms(t, tc.program, l.opt), qt.Equals, tc.expected)
			})
		}
	}
}

// TestI144UnquotedOperandIsAnExpression: the operand an unquote, a splice or an
// unsyntax fires is an ordinary expression, so a macro use in it expands. The
// expander used to pass these templates through whole and the compiler took the
// operand raw, compiling `,(when #t 2) as a call to an unbound `when`. The
// operands now expand in the expander pass, which the let-syntax, set! and
// depth rows below discriminate from expanding them at compile time.
//
// Expected column: Chez 10.4.1 (petite -q --script) and Racket 9.2, identical.
func TestI144UnquotedOperandIsAnExpression(t *testing.T) {
	const two = `(define-syntax two (syntax-rules () ((_) 2))) `
	tcs := []syntaxLayerRow{
		{name: "unquote", program: "`(1 ,(when #t 2))", expected: "(1 2)"},
		{name: "splice", program: "`(1 ,@(when #t (list 2 3)))", expected: "(1 2 3)"},
		{name: "dotted_tail", program: "`(1 . ,(when #t 2))", expected: "(1 . 2)"},
		// The improper tail is the first part that changes, so the cells before
		// it are back-filled.
		{name: "improper_vector_tail", program: "`(1 2 . #(a ,(when #t 3)))", expected: "(1 2 . #(a 3))"},
		{name: "vector", program: "`#(1 ,(when #t 2) ,@(when #t (list 3)))", expected: "#(1 2 3)"},
		{name: "local_in_operand", program: "(let ((x 5)) `(1 ,(when #t x)))", expected: "(1 5)"},
		{name: "unsyntax", program: "(syntax->datum #`(1 #,(when #t 2)))", expected: "(1 2)"},
		{name: "syntax_case_input", program: "(syntax-case (when #t '(a)) () ((x) 1))", expected: "1"},
		// A non-syntax input is matched as syntax: the Scheme layer's matcher
		// tested it with syntax-pair? and refused every compound datum.
		{name: "syntax_case_datum_input", program: "(syntax-case (list 1 2) () ((x y) (syntax->datum #'y)))", expected: "2"},
		{name: "with_syntax_rhs", program: "(with-syntax ((a (when #t 1))) (syntax->datum (syntax a)))", expected: "1"},
		// The depth rule: only the depth-1 operand fires, so only it expands.
		// Expanding the inner one as well would render (unquote 2).
		{name: "depth2_macro_operand", program: two + "`(1 `(2 ,(two)) ,(two))", expected: "(1 (quasiquote (2 (unquote (two)))) 2)"},
		// A let-syntax keyword is in scope only in the expander pass; the
		// compiler's environment holds no frame for it, and resolved the outer two.
		{name: "let_syntax_unquote", program: two + "(let-syntax ((two (syntax-rules () ((_) 3)))) (list `(,(two)) (two)))", expected: "((3) 3)"},
		{name: "let_syntax_syntax_case_input", program: two + "(let-syntax ((two (syntax-rules () ((_) 3)))) (syntax-case (two) () (x (syntax->datum #'x))))", expected: "3"},
		// A set! a macro template introduces is visible to validation only once
		// the operand has expanded; otherwise the inliner keeps f's stale body (7).
		{name: "set_in_unquote", program: "(let ((f (lambda () 7))) (define-syntax m (syntax-rules () ((_) (set! f (lambda () 99))))) `(,(m)) (f))", expected: "99"},
		{name: "set_in_syntax_case_input", program: "(let ((f (lambda () 7))) (define-syntax m (syntax-rules () ((_) (set! f (lambda () 99))))) (syntax-case (m) () (_ 1)) (f))", expected: "99"},
		{name: "set_in_with_syntax_rhs", program: "(let ((f (lambda () 7))) (define-syntax m (syntax-rules () ((_) (set! f (lambda () 99))))) (with-syntax ((a (m))) 1) (f))", expected: "99"},
		// The same hole at top level: a legal same-unit set! was rejected under
		// the immutable top level.
		{name: "toplevel_set_in_unquote", program: "(begin (define g 1) (define-syntax setg (syntax-rules () ((_) (set! g 2)))) `(,(setg)) g)", expected: "2"},
		// quasisyntax and with-syntax under the same two timing tests.
		{name: "let_syntax_unsyntax", program: two + "(let-syntax ((two (syntax-rules () ((_) 3)))) (syntax->datum #`(#,(two))))", expected: "(3)"},
		{name: "set_in_unsyntax", program: "(let ((f (lambda () 7))) (define-syntax m (syntax-rules () ((_) (set! f (lambda () 99))))) #`(#,(m)) (f))", expected: "99"},
		{name: "let_syntax_with_syntax_rhs", program: two + "(let-syntax ((two (syntax-rules () ((_) 3)))) (with-syntax ((a (two))) (syntax->datum #'a)))", expected: "3"},
		// A with-syntax with no bindings compiles its body as plain expressions,
		// so the body expands in the expander pass too; the zero-match ellipsis
		// form is what a syntax-rules template emits.
		{name: "with_syntax_no_bindings_body", program: "(with-syntax () `(a ,(when #t 1)))", expected: "(a 1)"},
		{name: "with_syntax_zero_match_body", program: "(define-syntax m (syntax-rules () ((_ (p e) ...) (with-syntax ((p e) ...) `(r ,(when #t 1)))))) (m)", expected: "(r 1)"},
	}
	runUnderBothSyntaxLayers(t, tcs)
}

// TestI144Pins are green on master under the Go layer and stay green; there
// none discriminates the fix, and each guards a regression it could introduce.
// The two capture rows were red under the Scheme layer, which a variable pin
// did not protect.
func TestI144Pins(t *testing.T) {
	tcs := []syntaxLayerRow{
		// Blind to the depth rule: expanding the call (two) yields (two), so a
		// fix that wrongly expanded the inner operand would still pass this.
		{
			name:     "depth2_procedure_operand",
			program:  "(define (two) 2) `(1 `(2 ,(two)) ,(two))",
			expected: "(1 (quasiquote (2 (unquote (two)))) 2)",
		},
		// with-syntax synthesizes a (list expr …) head with no scopes. Were it
		// expanded, a user list macro would capture it. Racket also answers 1.
		// The Scheme layer's generated code calls list through a pinned
		// identifier. A use-site define-syntax of a pinned VARIABLE captured it,
		// so this define-syntax failed while compiling its own syntax-rules.
		{
			name:     "generated_list_not_captured",
			program:  "(define-syntax list (syntax-rules () ((_ x ...) 'captured))) (define-syntax m (syntax-rules () ((_ x ...) '(x ...)))) (cons (list 1) (m 1 2))",
			expected: "(captured 1 2)",
		},
		// A pin to a mutable top-level variable does not stop the macro search:
		// a later (define-syntax helper …) redefines helper, and m's template
		// expands to the macro. Chez and Racket answer 42.
		{
			name:     "toplevel_variable_pin_redefined_as_macro",
			program:  "(define (helper) 0) (define-syntax m (syntax-rules () ((_) (helper)))) (define-syntax helper (syntax-rules () ((_) 42))) (m)",
			expected: "42",
		},
		// A malformed marker is data, so an operand inside it does not fire.
		// Wile's answer. Racket and Chez reject the first; for the second,
		// Racket agrees and Chez fires the operand.
		{
			name:     "malformed_unquote_is_data",
			program:  "(define-syntax two (syntax-rules () ((_) 2))) `(1 (unquote x (unquote (two))))",
			expected: "(1 (unquote x (unquote (two))))",
		},
		{
			name:     "malformed_nesting_is_data",
			program:  "(define-syntax two (syntax-rules () ((_) 2))) `(1 (quasiquote a ,(two)))",
			expected: "(1 (quasiquote a (unquote (two))))",
		},
		// A bare splice does not fire, so its operand is data and stays
		// unexpanded. Wile's answer; Chez rejects the form.
		{
			name:     "bare_splice_operand_not_expanded",
			program:  "(define-syntax two (syntax-rules () ((_) 2))) `,@(two)",
			expected: "(unquote-splicing (two))",
		},
		{
			name:     "with_syntax_synthesized_list_not_captured",
			program:  "(define-syntax list (syntax-rules () ((_ x ...) 'captured))) (with-syntax ((a 1)) (syntax->datum (syntax a)))",
			expected: "1",
		},
	}
	runUnderBothSyntaxLayers(t, tcs)
}

// helperLibFS exports m, whose template calls the library's own helper. The
// library's body definitions sit in its frame's mutable tier, not the sealed one.
var helperLibFS = fstest.MapFS{
	"helperlib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (helperlib)
  (import (scheme base))
  (export m)
  (begin
    (define (helper) 1)
    (define-syntax m (syntax-rules () ((_) (helper))))))`),
	},
}

// TestI144LibraryVariablePinNotCaptured: an importer's (define-syntax helper …)
// does not capture the library's helper in m's template; only a pin into the use
// site's own frame falls through to the macro search. Chez and Racket call the
// library's helper. Go layer only: under the Scheme layer a user library that
// defines a syntax-rules macro fails to load ("not a closure"), on master too.
func TestI144LibraryVariablePinNotCaptured(t *testing.T) {
	got := evalSyntaxForms(t, "(import (helperlib)) (define-syntax helper (syntax-rules () ((_) 42))) (m)",
		wile.WithGoSyntaxForms(), wile.WithSourceFS(stdlib.FS), wile.WithSourceFS(helperLibFS), wile.WithLibraryPaths())
	qt.New(t).Assert(got, qt.Equals, "1")
}

// TestI144MalformedFormsRejected: the expander pass rewrites only a well-formed
// form, so a malformed one still reaches the compiler's shape check. Rewriting
// the quasi forms dropped their improper tail, and with-syntax ignored an extra
// element and an improper binding list.
func TestI144MalformedFormsRejected(t *testing.T) {
	programs := []string{
		"(quasiquote (1 ,(when #t 2)) . junk)",
		"(quasisyntax (1 #,(when #t 2)) . junk)",
		"(with-syntax ((a (when #t 1) junk)) (syntax->datum (syntax a)))",
		"(with-syntax ((a 1) . junk) (syntax->datum (syntax a)))",
		"(with-syntax ())",
	}
	for _, program := range programs {
		t.Run(program, func(t *testing.T) {
			err := evalSyntaxFormsErr(t, program, wile.WithGoSyntaxForms())
			qt.New(t).Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue, qt.Commentf("%v", err))
		})
	}
}

// TestI144OperandRecursionIsBounded: a macro recursing through an operand
// shares the expansion's depth guard, so it raises ErrExpandDepthExceeded. A
// compile-time expander per operand started each one at depth zero, and the
// recursion overflowed the Go stack and killed the process.
func TestI144OperandRecursionIsBounded(t *testing.T) {
	programs := []string{
		"(define-syntax m (syntax-rules () ((_) `(,(m))))) (m)",
		"(define-syntax m (syntax-rules () ((_) (syntax-case (m) () (_ 1))))) (m)",
		"(define-syntax m (syntax-rules () ((_) (with-syntax ((a (m))) 1)))) (m)",
	}
	for _, program := range programs {
		t.Run(program, func(t *testing.T) {
			err := evalSyntaxFormsErr(t, program, wile.WithGoSyntaxForms())
			qt.New(t).Assert(errors.Is(err, werr.ErrExpandDepthExceeded), qt.IsTrue, qt.Commentf("%v", err))
		})
	}
}

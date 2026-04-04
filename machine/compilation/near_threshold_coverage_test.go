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

package compilation_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// TestCondExpandCompoundRequirements exercises compound feature requirement
// parsing and evaluation in compile_cond_expand.go: and, or, not, library,
// and nested combinations.
//
// Target: parseFeatureRequirement, resolveCondExpandClause, CompileCondExpand.
func TestCondExpandCompoundRequirements(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// library requirement -- exercises the (library ...) parsing/evaluation branch.
		// The test environment does not have a file-system library resolver, so even
		// (scheme base) is not "found". Both cases exercise the branch; the second
		// confirms an unknown library name falls through.
		{
			Name:     "library requirement for scheme base (no resolver)",
			Code:     `(cond-expand ((library (scheme base)) 'yes) (else 'no))`,
			Expected: values.NewSymbol("no"),
		},
		{
			Name:     "library requirement for nonexistent lib",
			Code:     `(cond-expand ((library (nonexistent lib)) 'no) (else 'yes))`,
			Expected: values.NewSymbol("yes"),
		},
		// nested compound requirements
		{
			Name:     "nested and-or",
			Code:     `(cond-expand ((and r7rs (or nonexistent wile)) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "nested not-and",
			Code:     `(cond-expand ((not (and r7rs nonexistent)) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "nested not-or both absent",
			Code:     `(cond-expand ((not (or nonexistent also-nonexistent)) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		// and with empty requirements (vacuously true)
		{
			Name:     "and with single feature",
			Code:     `(cond-expand ((and r7rs) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		// or with single feature
		{
			Name:     "or with single nonexistent",
			Code:     `(cond-expand ((or nonexistent) 'no) (else 'yes))`,
			Expected: values.NewSymbol("yes"),
		},
		// empty body clause -- exercises void emission branch
		{
			Name:     "empty body in matched clause",
			Code:     `(cond-expand (r7rs))`,
			Expected: values.Void,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestCaseLambdaRestArgs exercises case-lambda with rest/variadic parameters.
//
// Target: expandCaseLambdaForm in expander_lambda.go (improper formals branch),
// CompileValidatedCaseLambda in compile_validated.go.
func TestCaseLambdaRestArgs(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "case-lambda rest args clause",
			Code: `(begin
				(define f (case-lambda
					((x) x)
					((x y) (+ x y))
					((x y . rest) (apply + x y rest))))
				(f 1 2 3 4))`,
			Expected: values.NewInteger(10),
		},
		{
			Name: "case-lambda single rest arg",
			Code: `(begin
				(define g (case-lambda
					(() 'none)
					(args (length args))))
				(g 1 2 3))`,
			Expected: values.NewInteger(3),
		},
		{
			Name: "case-lambda with define in body",
			Code: `(begin
				(define h (case-lambda
					((x)
					 (define y (* x 2))
					 y)))
				(h 21))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "case-lambda two-arg dispatch",
			Code: `(begin
				(define f (case-lambda
					((x) (- x))
					((x y) (- x y))))
				(list (f 5) (f 10 3)))`,
			Expected: values.List(
				values.NewInteger(-5),
				values.NewInteger(7),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestEvalWhenExpand exercises eval-when with the expand phase, hitting
// evalWhenExecuteAtCompileTime (0% coverage), expandCompileExecute (0%),
// and executeFormsAtCompileTime (0%) in compile_eval_when.go and compile_helpers.go.
func TestEvalWhenExpand(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// eval-when (run) compiles body for runtime
		{
			Name:     "eval-when run produces runtime value",
			Code:     `(eval-when (run) 42)`,
			Expected: values.NewInteger(42),
		},
		// eval-when with both expand and run phases
		{
			Name:     "eval-when expand and run",
			Code:     `(eval-when (expand run) (+ 1 2))`,
			Expected: values.NewInteger(3),
		},
		// Chez-style phase names: load -> runtime
		{
			Name:     "eval-when load phase alias",
			Code:     `(eval-when (load) (+ 10 20))`,
			Expected: values.NewInteger(30),
		},
		// Chez-style phase names: eval -> runtime
		{
			Name:     "eval-when eval phase alias",
			Code:     `(eval-when (eval) (+ 5 5))`,
			Expected: values.NewInteger(10),
		},
		// visit phase has no effect -- should produce void
		{
			Name:     "eval-when visit phase produces void",
			Code:     `(eval-when (visit) 42)`,
			Expected: values.Void,
		},
		// empty phases list -- should produce void
		{
			Name:     "eval-when empty phases",
			Code:     `(eval-when () 42)`,
			Expected: values.Void,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestEvalWhenErrors exercises error paths in eval-when compilation.
func TestEvalWhenErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "unknown phase name",
			Code: `(eval-when (bogus) 1)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestDefineForSyntaxCompilation exercises define-for-syntax compilation,
// verifying that the form compiles, evaluates its expression at compile time,
// and does not produce a runtime error.
//
// Target: CompileDefineForSyntax in compile_define_for_syntax.go (27.9% coverage),
// expandCompileExecute in compile_helpers.go (0% coverage).
//
// define-for-syntax stores values in the expand-phase environment. The runtime
// result is void (no runtime effect). Full end-to-end usage requires library
// contexts where expand-phase bindings are accessible.
func TestDefineForSyntaxCompilation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "define-for-syntax simple value compiles without error",
			Code: `(begin
				(define-for-syntax ct-val 42)
				'ok)`,
			Expected: values.NewSymbol("ok"),
		},
		{
			Name: "define-for-syntax function form compiles without error",
			Code: `(begin
				(define-for-syntax (ct-add a b) (+ a b))
				'ok)`,
			Expected: values.NewSymbol("ok"),
		},
		{
			Name: "define-for-syntax with expression compiles without error",
			Code: `(begin
				(define-for-syntax ct-result (* 6 7))
				'ok)`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestSyntaxRulesCustomEllipsis exercises custom ellipsis identifiers in
// syntax-rules, hitting the custom ellipsis parsing branch in
// CompileSyntaxRules (compile_syntax_rules.go, 61% coverage).
func TestSyntaxRulesCustomEllipsis(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "custom ellipsis identifier",
			Code: `(begin
				(define-syntax my-list
					(syntax-rules ::: ()
						((_ x :::) (list x :::))))
				(my-list 1 2 3))`,
			Expected: values.List(
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
		},
		{
			Name: "custom ellipsis allows literal dots",
			Code: `(begin
				(define-syntax my-or
					(syntax-rules :: ()
						((_) #f)
						((_ e) e)
						((_ e1 e2 ::)
						 (let ((t e1))
							(if t t (my-or e2 ::))))))
				(my-or #f #f 42))`,
			Expected: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestBeginForSyntaxSchemeLevel exercises begin-for-syntax at the Scheme level,
// hitting executeFormsAtCompileTime (0% coverage) in compile_helpers.go.
//
// begin-for-syntax evaluates expressions at compile time in the expand-phase
// environment. The runtime result is not affected.
func TestBeginForSyntaxSchemeLevel(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "begin-for-syntax with single expression",
			Code: `(begin
				(begin-for-syntax (+ 1 2))
				'ok)`,
			Expected: values.NewSymbol("ok"),
		},
		{
			Name: "begin-for-syntax with multiple expressions",
			Code: `(begin
				(begin-for-syntax 1 2 3)
				'ok)`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestSyntaxRulesEllipsisInLiterals exercises the branch where ellipsis
// appears in the literals list, disabling ellipsis functionality.
//
// R7RS 4.3.2: It is an error for ... to appear in <literals>.
// Wile handles this by treating ... as a literal and disabling ellipsis matching.
func TestSyntaxRulesEllipsisInLiterals(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "ellipsis in literals treated as literal",
			Code: `(begin
				(define-syntax literal-dots
					(syntax-rules (...)
						((_ ...) 'matched-dots)))
				(literal-dots ...))`,
			Expected: values.NewSymbol("matched-dots"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

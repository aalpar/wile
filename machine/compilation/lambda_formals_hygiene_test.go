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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestLambdaFormalsHygienicDuplicates verifies that template-introduced lambda
// formals are hygienically distinct across recursive macro-expansion steps.
//
// This is the lambda-binder analogue of the let/define fix from PRs #606/#607
// (SRFI-42 Bug A). A recursive syntax-rules macro that injects a literal formal
// name once per expansion step must produce a lambda with as many distinct
// parameters as there were steps — they print the same but carry distinct intro
// scopes, so the duplicate-parameter check must compare scope sets, not bare
// string keys. This is exactly the property SRFI-26's cut/cute relies on.
func TestLambdaFormalsHygienicDuplicates(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			// Each <> injects a literal `x` formal; two slots -> (lambda (x x) ...)
			// at the source level, but hygienically two distinct `x`s.
			Name: "two slots via recursive syntax-rules",
			Code: `(begin
			  (define-syntax cut2
			    (syntax-rules ()
			      ((cut2 . s) (%w () () . s))))
			  (define-syntax %w
			    (syntax-rules (<>)
			      ((%w (p ...) (a ...))         (lambda (p ...) (a ...)))
			      ((%w (p ...) (a ...) <> . m)  (%w (p ... x) (a ... x) . m))
			      ((%w (p ...) (a ...) e  . m)  (%w (p ...) (a ... e) . m))))
			  ((cut2 + 1 <> 3 <>) 10 20))`,
			Expected: values.NewInteger(34), // (+ 1 10 3 20)
		},
		{
			// Rest-slot path: bindRestParameter must also be scope-aware.
			Name: "slot plus rest-slot",
			Code: `(begin
			  (define-syntax cutr
			    (syntax-rules ()
			      ((cutr . s) (%wr () () . s))))
			  (define-syntax %wr
			    (syntax-rules (<> <...>)
			      ((%wr (p ...) (a ...))            (lambda (p ...) (a ...)))
			      ((%wr (p ...) (a ...) <...>)      (lambda (p ... . rest) (apply a ... rest)))
			      ((%wr (p ...) (a ...) <> . m)     (%wr (p ... x) (a ... x) . m))
			      ((%wr (p ...) (a ...) e  . m)     (%wr (p ...) (a ... e) . m))))
			  ((cutr + 1 <> <...>) 10 20 30))`,
			Expected: values.NewInteger(61), // (+ 1 10 20 30)
		},
		{
			// Direct recursive-formal accumulation, no slots/literals.
			Name: "recursive formal accumulation",
			Code: `(begin
			  (define-syntax mkf
			    (syntax-rules ()
			      ((mkf (p ...))        (lambda (p ...) (+ p ...)))
			      ((mkf (p ...) h . t)  (mkf (p ... q) . t))))
			  ((mkf () 1 2) 10 20))`,
			Expected: values.NewInteger(30),
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

// TestLetSameNameDistinctScopeBindings verifies that a let binding two
// hygienically-distinct same-named variables (from recursive macro expansion)
// stores each init into its OWN slot. The store side must resolve slots with
// scope awareness; a bare-name lookup sends both stores to slot 0, leaving the
// second slot #!void. This is the let-store analogue of the lambda-formals fix,
// and is exactly what SRFI-26's portable cute relies on.
//
// The macro accumulates bindings ((a v2) (a v1)) and references (r1 r2) where
// r1 carries the value v1 and r2 carries v2. Using subtraction makes the result
// sensitive to BOTH presence (a void slot errors or misreads) and order.
func TestLetSameNameDistinctScopeBindings(t *testing.T) {
	macro := `(begin
	  (define-syntax mk
	    (syntax-rules ()
	      ((mk (binds ...) (refs ...))         (let (binds ...) (- refs ...)))
	      ((mk (binds ...) (refs ...) v . more) (mk ((a v) binds ...) (refs ... a) . more))))
	  (define-syntax start (syntax-rules () ((start . vs) (mk () () . vs))))`
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "two same-name bindings", Code: macro + ` (start 10 20))`, Expected: values.NewInteger(-10)},
		{Name: "three same-name bindings", Code: macro + ` (start 3 5 7))`, Expected: values.NewInteger(-9)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestInternalDefineSameNameDistinctScopeBindings is the internal-define
// analogue of TestLetSameNameDistinctScopeBindings. The declare side
// (declareDefineBinding) creates scope-aware slots, so the store side
// (emitDefineStore) must retrieve the slot scope-aware too — otherwise two
// hygienically-distinct same-named internal defines both store into slot 0 and
// the second slot reads #!void.
func TestInternalDefineSameNameDistinctScopeBindings(t *testing.T) {
	macro := `(begin
	  (define-syntax body-acc
	    (syntax-rules ()
	      ((body-acc (defs ...) (refs ...))          (let () defs ... (- refs ...)))
	      ((body-acc (defs ...) (refs ...) v . more) (body-acc ((define a v) defs ...) (refs ... a) . more))))
	  (define-syntax start (syntax-rules () ((start . vs) (body-acc () () . vs))))`
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "two same-name internal defines", Code: macro + ` (start 10 20))`, Expected: values.NewInteger(-10)},
		{Name: "three same-name internal defines", Code: macro + ` (start 3 5 7))`, Expected: values.NewInteger(-9)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestLambdaFormalsGenuineDuplicates verifies the scope-aware fix does NOT
// weaken duplicate detection: genuinely duplicate source-level formals (same
// name, same/no scopes) must still be rejected per R7RS.
func TestLambdaFormalsGenuineDuplicates(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "duplicate required param", Code: `((lambda (x x) x) 1 2)`},
		{Name: "duplicate among three", Code: `((lambda (a b a) a) 1 2 3)`},
		{Name: "rest collides with required", Code: `((lambda (x . x) x) 1 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

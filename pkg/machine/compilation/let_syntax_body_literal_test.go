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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// TestPatternLiteralInLetSyntaxBody pins R7RS §4.3.2 literal matching inside a
// let-syntax/letrec-syntax body.
//
// Every body identifier carries the form's scope, whatever the form binds. The
// matcher used to refuse a literal whose input carried that scope and the pattern
// did not, so an unrelated keyword broke every auxiliary keyword in its body:
// (let-syntax ((foo ...)) (cond (#f 1) (else 2))) raised "syntactic keyword else
// used as a variable". Shadowing is a binding question, and the rows that bind
// the literal's own name are what keep the fix from over-accepting.
//
// Every row agrees with Chez 10 (petite).
func TestPatternLiteralInLetSyntaxBody(t *testing.T) {
	const foo = `(foo (syntax-rules () ((_) 1)))`
	const shadowElse = `(else (syntax-rules () ((_) 1)))`
	const litp = `(define-syntax lit?
		(syntax-rules (else =>) ((_ else) 'lit) ((_ =>) 'arrow) ((_ x) 'other)))`

	lit := values.NewSymbol("lit")
	other := values.NewSymbol("other")

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "cond else",
			Code:     `(let-syntax (` + foo + `) (cond (#f 1) (else 2)))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "cond =>",
			Code:     `(let-syntax (` + foo + `) (cond ((assv 2 '((2 . 3))) => cdr) (else 0)))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "case else",
			Code:     `(let-syntax (` + foo + `) (case 3 ((1) 'a) (else 'b)))`,
			Expected: values.NewSymbol("b"),
		},
		{
			Name:     "guard else",
			Code:     `(let-syntax (` + foo + `) (guard (e (else 'caught)) (raise 'x)))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name:     "empty let-syntax",
			Code:     `(let-syntax () (cond (#f 1) (else 2)))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "letrec-syntax",
			Code:     `(letrec-syntax (` + foo + `) (cond (#f 1) (else 2)))`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "nested let-syntax bodies",
			Code: `(let-syntax (` + foo + `)
				(cond (#f 1)
					(else (let-syntax (` + foo + `)
						(case 2 ((1) 'a) (else (guard (e (else 'g)) (raise 1))))))))`,
			Expected: values.NewSymbol("g"),
		},
		{
			Name: "under lambda and internal define",
			Code: `(let-syntax (` + foo + `)
				((lambda () (define z (cond (#f 1) (else 2))) z)))`,
			Expected: values.NewInteger(2),
		},
		{
			// A control: `_` and `...` are not literals, and matched before the fix.
			Name: "nested syntax-rules with _ and ...",
			Code: `(let-syntax (` + foo + `)
				(let-syntax ((bar (syntax-rules () ((_ _ x ...) (list x ...)))))
					(bar 0 1 2 3)))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "user literal, unrelated keyword",
			Code:     `(begin ` + litp + ` (let-syntax (` + foo + `) (lit? else)))`,
			Expected: lit,
		},
		{
			Name:     "user literal through a second macro",
			Code:     `(begin ` + litp + ` (define-syntax via (syntax-rules () ((_ e) (lit? e)))) (let-syntax (` + foo + `) (via else)))`,
			Expected: lit,
		},
		{
			Name:     "let-syntax binding else shadows the literal",
			Code:     `(begin ` + litp + ` (let-syntax (` + shadowElse + `) (lit? else)))`,
			Expected: other,
		},
		{
			Name:     "letrec-syntax binding else shadows the literal",
			Code:     `(begin ` + litp + ` (letrec-syntax (` + shadowElse + `) (lit? else)))`,
			Expected: other,
		},
		{
			Name:     "let-syntax binding => shadows the literal",
			Code:     `(begin ` + litp + ` (let-syntax ((=> (syntax-rules () ((_) 1)))) (lit? =>)))`,
			Expected: other,
		},
		{
			Name:     "outer let-syntax shadow survives an inner unrelated one",
			Code:     `(begin ` + litp + ` (let-syntax (` + shadowElse + `) (let-syntax (` + foo + `) (lit? else))))`,
			Expected: other,
		},
		{
			Name:     "inner let-syntax shadow",
			Code:     `(begin ` + litp + ` (let-syntax (` + foo + `) (let-syntax (` + shadowElse + `) (lit? else))))`,
			Expected: other,
		},
		{
			Name:     "let binder inside let-syntax body shadows the literal",
			Code:     `(begin ` + litp + ` (let-syntax (` + foo + `) (let ((else 1)) (lit? else))))`,
			Expected: other,
		},
		{
			// A control: nested keyword shadowing (TestScopeResolution_LetSyntaxShadowing).
			Name:     "nested let-syntax keyword shadowing",
			Code:     `(let-syntax ((m (syntax-rules () ((_) 'outer)))) (let-syntax ((m (syntax-rules () ((_) 'inner)))) (m)))`,
			Expected: values.NewSymbol("inner"),
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

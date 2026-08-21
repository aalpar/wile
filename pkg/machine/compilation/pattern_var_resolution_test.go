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

// TestClauseBodyBinderShadowsPatternVariable records that Wile's TWO template
// paths give DIFFERENT answers when a clause-body binder shadows a pattern
// variable, and that neither answer is Chez's.
//
//	(define-syntax m
//	  (lambda (stx)
//	    (syntax-case stx ()
//	      ((_ x) (let ((x 1)) (syntax x))))))
//	(m 5)
//
// | path         | Wile                                   | Chez 10                |
// |--------------|----------------------------------------|------------------------|
// | non-ellipsis | the `let`'s `x` wins; the transformer   | out-of-phase reference |
// |              | returns 1, a non-syntax value, and the  | to identifier x        |
// |              | expander refuses it                     |                        |
// | ellipsis     | the pattern variable wins → (5 6)       | same refusal           |
//
// **This test asserts today's behaviour, which is not the right behaviour.** It
// exists because design C changes exactly the query that decides the
// non-ellipsis answer: `compileSyntaxTemplateToOps` resolves the template symbol
// under `syntax.AllScopes()` (wildcard, first-match innermost-out, no scope
// filter), which reaches the `let`'s binding before the pattern-variable frame.
// C replaces that with a scoped query. Without this pin the suite cannot tell
// "C fixed it", "C broke it", and "C left it alone" apart — and B's own history
// is the argument for caring: the Go suite passed the set-equality regression
// and passed the subset regression, each blind to the other.
//
// The ellipsis row answers differently because that path never consults an
// environment at all: `captureContext.bindings` is `map[string]SyntaxValue`,
// filled by the match VM from pattern-variable NAMES in the compiled bytecode,
// so a lexical binder in the clause body is invisible to it. See
// plans/2026-08-20-pattern-variables-as-bindings-impl.local.md §1 for why that
// path cannot become resolution-driven without redesigning pkg/internal/match.
//
// If a later change makes either row agree with Chez, update the row and say so
// — do not delete it.
func TestClauseBodyBinderShadowsPatternVariable(t *testing.T) {
	t.Run("non-ellipsis: the clause-body binder wins, and the result is refused", func(t *testing.T) {
		_, err := testhelpers.RunSchemeCode(t, `(begin
			(define-syntax m
				(lambda (stx)
					(syntax-case stx ()
						((_ x) (let ((x 1)) (syntax x))))))
			(m 5))`)
		// The `let`-bound x is emitted, so the transformer yields the integer 1
		// rather than syntax. Chez refuses earlier, and for a better reason.
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*not a syntax value.*`)
	})

	t.Run("ellipsis: the pattern variable wins", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(begin
			(define-syntax m
				(lambda (stx)
					(syntax-case stx ()
						((_ x c ...) (let ((x 1)) (syntax (list x c ...)))))))
			(m 5 6))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals,
			values.List(values.NewInteger(5), values.NewInteger(6)))
	})
}

// TestEnclosingClausePatternVariableResolves is design C's Q4: a template in a
// NESTED syntax-case clause body still reaches the ENCLOSING clause's pattern
// variable.
//
// `compileSyntaxTemplateToOps` gates only names in the CURRENT clause's
// patternVarSyntax; an enclosing clause's variable is absent from that map and
// is reached by the nominal frame walk instead. Under C that walk becomes an
// ordinary scoped resolution through the frame chain, so this is the behaviour
// the change must preserve.
//
// Measured 2026-08-20: Wile and Chez 10 both answer (1 2). The design filed Q4
// as "confirm, and add the missing test — there is none today"; this is that
// test, and it records a working baseline rather than a bug.
func TestEnclosingClausePatternVariableResolves(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `(begin
		(define-syntax outer
			(lambda (s1)
				(syntax-case s1 ()
					((_ a b)
					 (syntax-case (syntax b) ()
						 ((c) (syntax (list a c))))))))
		(outer 1 (2)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals,
		values.List(values.NewInteger(1), values.NewInteger(2)))
}

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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestI149_ConstantSubtemplateFollowedByEllipsisIsRefused is the I149 ratchet.
//
// A sub-template followed by `...` that contains no pattern variable has nothing
// to drive the iteration. Wile used to drop it silently and answer `()`, on the
// reading that R7RS §4.3.2 specifies repeating it zero times. That reading is
// refuted: §4.3.2 states only the ∀ direction — every driver must be bound at
// least as deep as the template ellipsis depth — and is silent on the ∃
// direction. Both reference implementations resolve the silence the same way, by
// refusing:
//
//	petite --script o1.ss  → "Exception: extra ellipsis in syntax form
//	                          (syntax (list 9 ...))", exit 255
//	racket o1.rkt          → "syntax/loc: no pattern variables before ellipsis
//	                          in template / at: 9", exit 1
//
// Two oracles agreeing where the spec is silent is this project's standard for
// taking their answer, and Wile's Go layer was alone in answering `()`. Note
// this CONVERGES the two syntax layers rather than tightening one: the Scheme
// layer (WILE_SYNTAX_FORMS=scheme) already refused all four shapes, including at
// definition.
//
// REFUSAL FIRES AT DEFINITION for the syntax-rules shapes, which is why shape d
// exists: petite refuses a definition whose macro is never used, and so does
// Wile now. syntax-case (shape c) can only be caught at expansion, because its
// template is not walked until the macro is used.
//
// TWO SENTINELS, deliberately, because the two paths refuse in different pipeline
// phases and each follows its own package's convention:
//
//   - syntax-rules → werr.ErrInvalidSyntax, raised by checkEllipsisGroupDriver
//     when the TRANSFORMER IS COMPILED (pkg/machine/compilation).
//   - syntax-case  → werr.ErrExpansion, raised by expandSyntaxEllipsis when the
//     TEMPLATE IS EXPANDED (pkg/internal/match, where ErrExpansion is the
//     dominant sentinel and is what the adjacent arms of that same function use).
//
// The assertions are on sentinels, never on rendered wrap chains: shape a's chain
// is nine "failed to …" links deep and the provenance wave rewrites those.
func TestI149_ConstantSubtemplateFollowedByEllipsisIsRefused(t *testing.T) {
	tcs := []struct {
		name     string
		src      string
		sentinel error
	}{
		{
			name:     "syntax-rules, constant literal sub-template",
			src:      `(define-syntax m (syntax-rules () ((_ x ...) (list 9 ...)))) (m 1 2 3)`,
			sentinel: werr.ErrInvalidSyntax,
		},
		{
			// Verbatim the row that syntax_rules_depth_test.go pinned to "()" in
			// its no-false-positives table. The ratchet and the inversion of that
			// pin are the same assertion read two ways.
			name:     "syntax-rules, quoted constant sub-template",
			src:      `(define-syntax m (syntax-rules () ((_ a ...) (list (quote z) ...)))) (m 1 2 3)`,
			sentinel: werr.ErrInvalidSyntax,
		},
		{
			name:     "syntax-case, constant literal sub-template",
			src:      `(define-syntax m (lambda (stx) (syntax-case stx () ((_ x ...) #'(list 9 ...))))) (m 1 2 3)`,
			sentinel: werr.ErrExpansion,
		},
		{
			// Definition only, macro never used: the whole program is the
			// define-syntax, which succeeds silently on master and must refuse
			// after. This is the shape that proves the syntax-rules refusal is a
			// construction-time property, matching petite, which refuses the same
			// definition-only script.
			//
			// Nothing follows the define-syntax on purpose. An earlier draft ended
			// it with (display "ok"), which made the case red on master for an
			// unrelated reason — a bare NewEngine binds no `display` — and so
			// proved nothing about the ellipsis.
			name:     "syntax-rules, definition only, macro never used",
			src:      `(define-syntax m (syntax-rules () ((_ x ...) (list 9 ...))))`,
			sentinel: werr.ErrInvalidSyntax,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)

			_, err = eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("a driverless ellipsis sub-template must be refused, not dropped"))
			qt.Assert(t, errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("want sentinel %v, got: %v", tc.sentinel, err))
		})
	}
}

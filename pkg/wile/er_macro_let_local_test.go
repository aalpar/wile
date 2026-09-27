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

// An er-macro-transformer body cannot reach the enclosing let's RUNTIME
// locals, and the refusal names the identifier.
//
// WHY THIS PIN EXISTS, since it is green the day it is written and is
// therefore not a ratchet. TODO.md carried a row (closed by the same commit
// that added this file) claiming `(let ((c 0)) (define-syntax m
// (er-macro-transformer (lambda (x r c*) c))) (m))` reads `c` as `#<void>`,
// and blaming CompileERMacroTransformerExpr for evaluating the lambda before
// the let's locals exist. Measured: there is no `#<void>` anywhere. The
// program refuses with ErrNoSuchBinding, which is the correct hygiene answer —
// `c` is a phase-0 runtime binding and the transformer body runs at phase 1 —
// and Racket's analogue errors on the same shape ("c: undefined; cannot
// reference an identifier before its definition"). The row as filed asked a
// future agent to replace a correct refusal with a silent wrong answer. This
// file is what stops that from looking like an unclaimed improvement.
//
// NOT A DUPLICATE of TestUnboundDiagnosticNamesThePhase's
// "scoped reference in an er-macro-transformer body" case
// (unbound_phase_diagnostic_test.go) or of
// TestPhase1_ProceduralTransformerUnboundWithoutImport
// (phase_distinctness_test.go). Both already pin this mechanism for a
// TOP-LEVEL binder. The let-local shape the row names is uncovered, and it is
// the shape the row would have had someone "fix".
//
// WHAT THE ASSERTIONS DELIBERATELY DO NOT TOUCH. The rendered wrap chain is
// nine `failed to …` links deep and carries a `line:col` offset. Neither is
// asserted here: the provenance wave rewrites both, and a pin that breaks when
// a diagnostic improves is a tax on the improvement, whose cheapest fix at
// that moment is deletion. What survives is the sentinel (stable identity) and
// the fact that the identifier is named. The second of those IS coupled to the
// message text, in its narrowed form — this pin is not dependency-free with
// respect to the provenance work, and a message change that stops naming the
// offending identifier should land here as a deliberate edit, not a surprise.

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestERMacroTransformerCannotSeeLetLocals pins the refusal and its one
// escape hatch.
//
// The two cases differ by exactly one added line — the `(define-for-syntax c
// 99)` — and the `let` stays in BOTH. That is load-bearing and measured:
// with the `let` removed, the second program still answers 99, so a reader who
// "simplifies" it away leaves a case that proves only that a phase-1 binding
// named `c` resolves, not that the let is what was being refused. The pair
// stops being a controlled contrast at that point.
func TestERMacroTransformerCannotSeeLetLocals(t *testing.T) {
	const letLocalSrc = `(let ((c 0))
	                       (define-syntax m (er-macro-transformer (lambda (x r c*) c)))
	                       (m))`

	const forSyntaxSrc = `(define-for-syntax c 99)
	                      (let ((c 0))
	                        (define-syntax m (er-macro-transformer (lambda (x r c*) c)))
	                        (m))`

	t.Run("the let's runtime local is not reachable at phase 1", func(t *testing.T) {
		ctx := context.Background()
		eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
		qt.Assert(t, err, qt.IsNil)
		defer eng.Close()

		_, err = eng.EvalMultiple(ctx, letLocalSrc)

		qt.Assert(t, err, qt.IsNotNil,
			qt.Commentf("an er-macro-transformer body must not reach the enclosing let's runtime locals"))
		qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
			qt.Commentf("got: %s", err))
		qt.Assert(t, strings.Contains(err.Error(), `no such binding "c"`), qt.IsTrue,
			qt.Commentf("the refusal must name the identifier; got: %s", err))
	})

	t.Run("define-for-syntax is the escape hatch", func(t *testing.T) {
		ctx := context.Background()
		eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
		qt.Assert(t, err, qt.IsNil)
		defer eng.Close()

		v, err := eng.EvalMultiple(ctx, forSyntaxSrc)

		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v, qt.IsNotNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "99")
	})
}

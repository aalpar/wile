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

package compilation

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestQuasisyntaxNestingIsAlwaysRuntime pins quasiKeywords.nestingAlwaysRuntime,
// which is the one divergence between the two dialects that the unified
// quasiNeedsRuntime keeps as a FIELD rather than folding away.
//
// It is semantically load-bearing, not a conservative approximation. The runtime
// expansion materializes (syntax z) for the inner argument, and (syntax z) on a
// bound local yields that local's VALUE where the literal path keeps the symbol:
//
//	(let ((z 5)) #`#`(a #,z))  =>  (quasisyntax (a (unsyntax 5)))
//	(let ((z 5))  ``(a ,z))    =>  (quasiquote (a (unquote z)))
//
// A probe with a FREE identifier is the one input on which the two paths agree,
// which is how this was once mistaken for a pure over-approximation.
//
// It is also what bounds the depth: production enters the quasisyntax predicate
// only at depth 1 (compileQuasisyntaxTemplate's single call site), the nesting
// arm at depth 1 returns here, and the unquote arm at depth 1 returns before its
// depth-1 recursion. So no quasisyntax template reaches this predicate at any
// depth but 1, and the second row below is only observable from a direct call.
func TestQuasisyntaxNestingIsAlwaysRuntime(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	// A nested quasisyntax at depth 1 takes the runtime path whatever is
	// inside it — here, nothing that fires.
	inert := parseSchemeExpr(t, env, "(quasisyntax (a b))")
	c.Assert(ccnt.quasisyntaxNeedsRuntime(inert, 1), qt.IsTrue)

	// quasiquote asks instead of assuming, and answers no on the same shape.
	// This is the divergence the field exists to hold.
	inertQQ := parseSchemeExpr(t, env, "(quasiquote (a b))")
	c.Assert(ccnt.quasiNeedsRuntime(inertQQ, 1, quasiquoteKW, ccnt.newQuasiDepthGuard()), qt.IsFalse)
}

// TestQuasiNeedsRuntimeDottedTailIsDialectGated pins that the dotted-unquote
// spine test is gated on handleDottedUnquote rather than on kw.unquote alone.
//
// `(1 . ,x) READS as (1 unquote x), and quasiquote must answer yes: the tail is
// evaluated (R7RS §4.2.8). quasisyntax does not implement the form, so the same
// spine shape is three ordinary elements there and must answer NO — answering
// yes would move #`(1 . #,x) off the literal path onto (datum->syntax #f …)
// without changing its datum, a deopt no value assertion could see.
func TestQuasiNeedsRuntimeDottedTailIsDialectGated(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	qq := parseSchemeExpr(t, env, "(1 unquote x)")
	c.Assert(ccnt.quasiNeedsRuntime(qq, 1, quasiquoteKW, ccnt.newQuasiDepthGuard()), qt.IsTrue)

	qs := parseSchemeExpr(t, env, "(1 unsyntax x)")
	c.Assert(ccnt.quasiNeedsRuntime(qs, 1, quasisyntaxKW, ccnt.newQuasiDepthGuard()), qt.IsFalse)
}

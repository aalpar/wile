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
	c.Assert(quasiNeedsRuntime(inertQQ, 1, quasiquoteKW, ccnt.newQuasiDepthGuard()), qt.IsFalse)
}

// TestQuasiNeedsRuntimeDottedTail pins the predicate half of dottedTailCell:
// that the two spine shapes R7RS §7.1.4 admits in tail position are recognized
// AS tails, and that the depth each one carries reaches this answer.
//
// `(1 . ,x) READS as (1 unquote x), so both shapes arrive as a bare keyword on
// the spine and the difference between "tail" and "three ordinary elements" is
// invisible to a car-yielding walk. Under the tail reading ,x is ⟨unquotation 1⟩
// and fires; `(1 . `(,x)) puts its ,x at depth 2 instead, so the whole template
// is a literal and the answer inverts on a spine of the same shape.
//
// Both dialects read the tail — dottedTailCell takes no dialect flag — so the
// rows below are a statement about the RULE, not about a switch: the qq and qs
// answers agree wherever nothing else diverges. This test was named
// …IsDialectGated while quasisyntax declined the form, and the flag it pinned
// was deleted with the divergence on 2026-08-23.
func TestQuasiNeedsRuntimeDottedTail(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	qq := parseSchemeExpr(t, env, "(1 unquote x)")
	c.Assert(quasiNeedsRuntime(qq, 1, quasiquoteKW, ccnt.newQuasiDepthGuard()), qt.IsTrue)

	qs := parseSchemeExpr(t, env, "(1 unsyntax x)")
	c.Assert(quasiNeedsRuntime(qs, 1, quasisyntaxKW, ccnt.newQuasiDepthGuard()), qt.IsTrue)

	qqNest := parseSchemeExpr(t, env, "(1 quasiquote ((unquote x)))")
	c.Assert(quasiNeedsRuntime(qqNest, 1, quasiquoteKW, ccnt.newQuasiDepthGuard()), qt.IsFalse)

	// The same spine one level in, where the unquote does come back to depth 1.
	qqNestLive := parseSchemeExpr(t, env, "(1 quasiquote ((unquote (unquote x))))")
	c.Assert(quasiNeedsRuntime(qqNestLive, 1, quasiquoteKW, ccnt.newQuasiDepthGuard()), qt.IsTrue)

	// quasisyntax answers TRUE on the shape quasiquote answers false on, and the
	// cause is nestingAlwaysRuntime, not the tail reading — which is worth
	// pinning because this row read `true` before the flip too, for the opposite
	// reason (the tail was walked element-wise, so #,x sat at depth 1 and fired).
	// The row below isolates the cause: nothing in it could fire at any depth.
	qsNest := parseSchemeExpr(t, env, "(1 quasisyntax ((unsyntax x)))")
	c.Assert(quasiNeedsRuntime(qsNest, 1, quasisyntaxKW, ccnt.newQuasiDepthGuard()), qt.IsTrue)

	qsNestInert := parseSchemeExpr(t, env, "(1 quasisyntax ((a b)))")
	c.Assert(quasiNeedsRuntime(qsNestInert, 1, quasisyntaxKW, ccnt.newQuasiDepthGuard()), qt.IsTrue)

	qqNestInert := parseSchemeExpr(t, env, "(1 quasiquote ((a b)))")
	c.Assert(quasiNeedsRuntime(qqNestInert, 1, quasiquoteKW, ccnt.newQuasiDepthGuard()), qt.IsFalse)
}

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

package validate

import (
	"testing"

	"github.com/aalpar/wile/internal/syntax"

	qt "github.com/frankban/quicktest"
)

// visitRecord captures one symbol-visit event for table-driven
// assertions. Fields are exported so qt.DeepEquals can compare them.
type visitRecord struct {
	Name  string
	Role  RefRole
	Depth int
}

// recordVisits walks expr with WalkBindingRefs and returns every visit
// in order.
func recordVisits(expr ValidatedExpr) []visitRecord {
	var got []visitRecord
	WalkBindingRefs(expr, func(sym *syntax.SyntaxSymbol, role RefRole, depth int) {
		got = append(got, visitRecord{
			Name:  sym.Sym.Key,
			Role:  role,
			Depth: depth,
		})
	})
	return got
}

// TestWalkBindingRefs_PlainSymbol verifies a bare ValidatedSymbol yields
// exactly one RefInBody visit at depth 0.
func TestWalkBindingRefs_PlainSymbol(t *testing.T) {
	got := recordVisits(symRef("x"))
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefInBody, Depth: 0},
	})
}

// TestWalkBindingRefs_CallProcVsArg verifies the operator symbol is
// tagged RefInCallProc while argument symbols are tagged RefInBody, both
// at depth 0.
func TestWalkBindingRefs_CallProcVsArg(t *testing.T) {
	expr := call(symRef("f"), symRef("a"), symRef("b"))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "f", Role: RefInCallProc, Depth: 0},
		{Name: "a", Role: RefInBody, Depth: 0},
		{Name: "b", Role: RefInBody, Depth: 0},
	})
}

// TestWalkBindingRefs_ClosureBodyIncrementsDepth verifies that a non-
// immediately-applied lambda body is walked at depth+1.
func TestWalkBindingRefs_ClosureBodyIncrementsDepth(t *testing.T) {
	// (let-equivalent body) → (lambda () x) — lambda is NOT being called,
	// so its body crosses an escaping closure boundary.
	expr := lam(symRef("x"))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefInBody, Depth: 1},
	})
}

// TestWalkBindingRefs_ImmediatelyAppliedLambdaSameDepth verifies that
// when a lambda IS the operator of a Call, its body is walked at the
// SAME depth (the closure does not escape).
func TestWalkBindingRefs_ImmediatelyAppliedLambdaSameDepth(t *testing.T) {
	// ((lambda () x)) — lambda is immediately applied; body must see
	// depth=0, not depth=1.
	expr := call(lam(symRef("x")))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefInBody, Depth: 0},
	})
}

// TestWalkBindingRefs_ImmediatelyAppliedCaseLambdaSameDepth verifies the
// same optimization for ValidatedCaseLambda.
func TestWalkBindingRefs_ImmediatelyAppliedCaseLambdaSameDepth(t *testing.T) {
	expr := call(caseLam(symRef("x")))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefInBody, Depth: 0},
	})
}

// TestWalkBindingRefs_NestedClosuresAccumulateDepth verifies depth is
// incremented at each escaping closure boundary.
func TestWalkBindingRefs_NestedClosuresAccumulateDepth(t *testing.T) {
	// (lambda () (lambda () x)) — two non-immediately-applied lambdas;
	// inner body sees depth=2.
	expr := lam(lam(symRef("x")))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefInBody, Depth: 2},
	})
}

// TestWalkBindingRefs_SetBangYieldsTargetAndValue verifies that a set!
// emits exactly two events: the target name as RefSetBangTarget, then
// the value's symbol references as a normal walk.
func TestWalkBindingRefs_SetBangYieldsTargetAndValue(t *testing.T) {
	// (set! x y) — two visits: x as target, y as body ref.
	expr := setBang("x", symRef("y"))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefSetBangTarget, Depth: 0},
		{Name: "y", Role: RefInBody, Depth: 0},
	})
}

// TestWalkBindingRefs_SetBangInsideClosureDepth verifies set!-target
// events carry the closure depth — required for the capture analysis
// to mark closure-mutated bindings.
func TestWalkBindingRefs_SetBangInsideClosureDepth(t *testing.T) {
	// (lambda () (set! x y)) — target and value both at depth=1.
	expr := lam(setBang("x", symRef("y")))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefSetBangTarget, Depth: 1},
		{Name: "y", Role: RefInBody, Depth: 1},
	})
}

// TestWalkBindingRefs_NilExprIsNoop verifies that nil input is handled
// silently without invoking the visitor.
func TestWalkBindingRefs_NilExprIsNoop(t *testing.T) {
	calls := 0
	WalkBindingRefs(nil, func(_ *syntax.SyntaxSymbol, _ RefRole, _ int) {
		calls++
	})
	qt.Assert(t, calls, qt.Equals, 0)
}

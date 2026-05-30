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
			Name:  sym.Key(),
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

// TestWalkBindingRefs_NestedCallProcRoleShallow pins the shallow-role
// invariant documented on WalkBindingRefs: RefInCallProc is reported
// only for direct symbol children of Call/Apply. When the call's Proc
// is itself a Call, the INNER operator is yielded as RefInCallProc
// (because it IS the proc of its own immediate call), but the inner
// arguments are RefInBody. The outer Proc — being a non-symbol Call
// node — never produces a RefInCallProc event itself.
func TestWalkBindingRefs_NestedCallProcRoleShallow(t *testing.T) {
	// ((f x) y) — outer proc is itself a call (f x); outer arg is y.
	expr := call(call(symRef("f"), symRef("x")), symRef("y"))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "f", Role: RefInCallProc, Depth: 0},
		{Name: "x", Role: RefInBody, Depth: 0},
		{Name: "y", Role: RefInBody, Depth: 0},
	})
}

// TestWalkBindingRefs_DefineFunctionDepth verifies that ValidatedDefine
// in function form treats its body as crossing a closure boundary —
// references inside the body are reported at depth+1. Required by the
// capture analysis: '(define (f) g)' captures g if f is invoked.
func TestWalkBindingRefs_DefineFunctionDepth(t *testing.T) {
	expr := defineFn("f", symRef("x"))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefInBody, Depth: 1},
	})
}

// TestWalkBindingRefs_DefineValueSameDepth verifies that ValidatedDefine
// in value form (define x expr) treats expr as a normal body
// expression — symbol references appear at the current depth, not
// depth+1. The value form does NOT introduce a closure.
func TestWalkBindingRefs_DefineValueSameDepth(t *testing.T) {
	expr := defineVal("y", symRef("x"))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "x", Role: RefInBody, Depth: 0},
	})
}

// TestWalkBindingRefs_CallProcInsideClosureBody pins the composition
// of two contracts: call-proc tagging composes with depth increment,
// and visit order inside the closure body is operator-before-arg.
// '(lambda () (g x))' must yield g as RefInCallProc at depth=1, then
// x as RefInBody at depth=1.
func TestWalkBindingRefs_CallProcInsideClosureBody(t *testing.T) {
	expr := lam(call(symRef("g"), symRef("x")))
	got := recordVisits(expr)
	qt.Assert(t, got, qt.DeepEquals, []visitRecord{
		{Name: "g", Role: RefInCallProc, Depth: 1},
		{Name: "x", Role: RefInBody, Depth: 1},
	})
}

// TestBuildBindingIdxMap_SilentlyDropsUnresolvable pins the documented
// best-effort contract: any binding whose Name fails ResolveBindingID
// under childEnv is silently dropped from the returned map. The
// markCapturedBindings / markEscapedBindings helpers rely on this
// behavior — if a future refactor changed it (e.g. to panic or return
// an error), the consumers' "best-effort: stays non-captured" promise
// would silently break.
func TestBuildBindingIdxMap_SilentlyDropsUnresolvable(t *testing.T) {
	env, bindings := makeTestEnvAndBindings("x") // x is registered in env
	// Append a binding whose Name was NEVER registered with env.
	unregistered := syntax.NewSyntaxSymbol("y", nil)
	bindings = append(bindings, ValidatedLetBinding{
		Name: unregistered,
		Init: lit(),
	})

	idx := buildBindingIdxMap(env, bindings)

	qt.Assert(t, len(idx), qt.Equals, 1,
		qt.Commentf("unregistered binding 'y' must be silently dropped; only 'x' resolves"))
}

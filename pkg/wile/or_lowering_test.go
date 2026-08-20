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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/wile"
)

// opSites counts an opcode across a template and every nested template reachable
// through its literals, for the same reason selfTailSites does: a loop lambda
// compiles into a child template, so a top-template-only count reports zero for
// every loop-shaped procedure and passes for the wrong reason.
func opSites(tpl *machine.NativeTemplate, op machine.OpCode) int {
	n := 0
	for _, instr := range tpl.Code() {
		if instr.Op == op {
			n++
		}
	}
	for _, lit := range tpl.Literals() {
		sub, isTpl := lit.(*machine.NativeTemplate)
		if !isTpl {
			continue
		}
		n += opSites(sub, op)
	}
	return n
}

// templateOf compiles src and returns the template of the named procedure.
func templateOf(t *testing.T, src, name string) *machine.NativeTemplate {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(ctx, src)
	qt.Assert(t, err, qt.IsNil)
	v, err := eng.EvalMultiple(ctx, name)
	qt.Assert(t, err, qt.IsNil)
	cl, isClosure := v.Internal().(*machine.MachineClosure)
	qt.Assert(t, isClosure, qt.IsTrue)
	return cl.Template()
}

// TestOrEmitsNoEnvFrame is the finding. `or` expands to (let ((t E)) (if t t B)),
// one frame per operand beyond the first, and that frame is unobservable: the
// value E leaves in the value register IS the consequent's value, because
// OpBranchOnFalseValue reads the register without clobbering it.
//
// The cases hold everything constant but the operand count, so a pass cannot be
// explained by "this procedure happens to need no frame".
func TestOrEmitsNoEnvFrame(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"two operands", "(define (f x y) (or x y))"},
		{"three operands", "(define (f x y z) (or x y z))"},
		{"non-tail position", "(define (f x y) (cons (or x y) 1))"},
		{"operands are calls", "(define (f x) (or (pair? x) (null? x)))"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			tpl := templateOf(t, tc.src, "f")
			got := opSites(tpl, machine.OpPushEnv)
			qt.Assert(t, got, qt.Equals, 0,
				qt.Commentf("or introduced %d env frame(s); the bound temp is "+
					"tested and returned and needs no slot", got))
		})
	}
}

// TestOrLoweringPreservesValues is why the lowering does not need a type proof.
// An earlier design lowered to (if E #t B), which is only correct when E yields a
// boolean; the register passthrough returns E's OWN value, so a truthy non-boolean
// operand must come back unchanged.
func TestOrLoweringPreservesValues(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{"truthy non-boolean is returned as itself", "(or 5 1)", "5"},
		{"first false falls through", "(or #f 7)", "7"},
		{"empty list is truthy", "(or '() 1)", "()"},
		{"zero is truthy", "(or 0 1)", "0"},
		{"all false yields the last", "(or #f #f)", "#f"},
		{"nullary", "(or)", "#f"},
		{"unary passes through", "(or 3)", "3"},
		{"n-ary stops at the first truthy", "(or #f #f 4 5)", "4"},
		{"side effect runs once", "(let ((n 0)) (or (begin (set! n (+ n 1)) #f) #f) n)", "1"},
		// cond's => clause expands to the SAME let but its consequent consumes the
		// bound value, so it must not be lowered.
		{"cond => still applies its receiver", "(cond ((assv 2 '((1 a) (2 b))) => cadr) (else 'none))", "b"},
	}
	ctx := context.Background()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			v, err := eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// selfTailPops returns the pop counts of every OpSelfTailCall in a template tree,
// in code order. Since frame-reclaim Phase C the op carries the number of `let`
// frames it unwinds before rebinding, which is where the `or` lowering's effect is
// now visible.
func selfTailPops(tpl *machine.NativeTemplate) []int {
	var q []int
	for _, instr := range tpl.Code() {
		if instr.Op == machine.OpSelfTailCall {
			_, pops := machine.DecodeSelfTailCall(instr.Arg)
			q = append(q, pops)
		}
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if !ok {
			continue
		}
		q = append(q, selfTailPops(sub)...)
	}
	return q
}

// TestOrAlternativeSelfTailCallPopsNoFrame is the frame-reclaim half, and the
// reason the lowering happens in the validated tree rather than only in codegen.
//
// THE DISCRIMINATOR HAS MOVED TWICE, and each move is a real change rather than
// a weakened test. It first asserted that the call was REWRITTEN AT ALL; Phase C
// removed the depth gate, so the site is rewritten either way. It then asserted
// the POP COUNT — `or` emits no frame, a real `let` emits one — and let-slot
// merging removed the `let`'s frame too, so both are 0.
//
// What separates them now is the BINDING: `or`'s lowering emits no store at all,
// because OpBranchOnFalseValue leaves the test's own value in the register for
// the consequent to return. A real `let` still stores its init into a slot, even
// though that slot now lives in the enclosing frame. The pop counts stay here as
// the record of what merging did; the store count is what fails if the lowering
// stops firing.
//
// The controls hold the loop shape constant and vary only the wrapper, so a pass
// cannot be explained by "self-recursive procedures get OpSelfTailCall".
func TestOrAlternativeSelfTailCallPopsNoFrame(t *testing.T) {
	plain := templateOf(t,
		"(define (f n acc) (if (= n 0) acc (f (- n 1) (+ acc n))))", "f")
	qt.Assert(t, selfTailPops(plain), qt.DeepEquals, []int{0},
		qt.Commentf("control: a plain self tail call is already at the parameter frame"))

	wrapped := templateOf(t,
		"(define (f n acc) (if (= n 0) acc (or #f (f (- n 1) (+ acc n)))))", "f")
	qt.Assert(t, selfTailPops(wrapped), qt.DeepEquals, []int{0},
		qt.Commentf("an or's alternative must pop NOTHING: the lowering emits no "+
			"OpPushEnv, so there is no frame between the call and the parameter frame. "+
			"A 1 here means the lowering stopped firing and the op is compensating "+
			"for a frame that should never have existed"))

	qt.Assert(t, countOpInTree(wrapped, machine.OpStoreLocal), qt.Equals, 0,
		qt.Commentf("an or's alternative must bind NOTHING: the consequent reads the "+
			"value register the test already wrote, so there is no slot to store into"))

	letWrapped := templateOf(t,
		"(define (f n acc) (if (= n 0) acc (let ((m (- n 1))) (f m (+ acc n)))))", "f")
	qt.Assert(t, selfTailPops(letWrapped), qt.DeepEquals, []int{0},
		qt.Commentf("a REAL let no longer pushes a frame either, since let-slot "+
			"merging — which is why the store count below, not this one, is what "+
			"makes the assertions above a measurement rather than a constant"))
	qt.Assert(t, countOpInTree(letWrapped, machine.OpStoreLocal), qt.Equals, 1,
		qt.Commentf("control: a REAL let binds, so it stores its init into a slot — "+
			"a 0 here would mean this control had stopped discriminating too"))
}

// countOpInTree counts instructions with opcode op in a template and every
// sub-template reachable through its literal pool.
func countOpInTree(tpl *machine.NativeTemplate, op machine.OpCode) int {
	q := 0
	for _, instr := range tpl.Code() {
		if instr.Op == op {
			q++
		}
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if !ok {
			continue
		}
		q += countOpInTree(sub, op)
	}
	return q
}

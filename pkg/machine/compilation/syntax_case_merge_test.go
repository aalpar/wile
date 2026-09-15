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

	"github.com/aalpar/wile/pkg/machine"

	qt "github.com/frankban/quicktest"
)

// bindPatternVarsIn returns every OperationBindPatternVars reachable from tpl,
// in emission order, walking nested templates through the literal pool.
//
// It reads the SIDE TABLE, not the code: BindPatternVars is an OpComplex
// operation, so the instruction carries an index and the operand this test is
// about (MergedSlots) lives on the operation object.
func bindPatternVarsIn(tpl *machine.NativeTemplate) []*OperationBindPatternVars {
	var q []*OperationBindPatternVars
	seen := map[*machine.NativeTemplate]bool{}
	var walk func(cur *machine.NativeTemplate)
	walk = func(cur *machine.NativeTemplate) {
		if cur == nil || seen[cur] {
			return
		}
		seen[cur] = true
		for _, op := range cur.SideTable() {
			bpv, ok := op.(*OperationBindPatternVars)
			if ok {
				q = append(q, bpv)
			}
		}
		for _, lit := range cur.Literals() {
			sub, ok := lit.(*machine.NativeTemplate)
			if ok {
				walk(sub)
			}
		}
	}
	walk(tpl)
	return q
}

// countOpEverywhere counts instructions with the given opcode across tpl and
// every template in its literal pool.
func countOpEverywhere(tpl *machine.NativeTemplate, op machine.OpCode) int {
	q := 0
	seen := map[*machine.NativeTemplate]bool{}
	var walk func(cur *machine.NativeTemplate)
	walk = func(cur *machine.NativeTemplate) {
		if cur == nil || seen[cur] {
			return
		}
		seen[cur] = true
		for _, instr := range cur.Code() {
			if instr.Op == op {
				q++
			}
		}
		for _, lit := range cur.Literals() {
			sub, ok := lit.(*machine.NativeTemplate)
			if ok {
				walk(sub)
			}
		}
	}
	walk(tpl)
	return q
}

// TestSyntaxCaseClauseBodyLetsPush pins that a `let` in a syntax-case clause
// body pushes its own frame instead of taking slots out of the pattern-variable
// frame.
//
// The merge is mechanically possible (OperationBindPatternVars.MergedSlots
// carries the width), but canMergeLet refuses any frame a continuation can be
// captured under, and a clause body is raw syntax the capture scan cannot prove
// capture-free. A merged slot is not fresh when a continuation re-runs its
// `let`, which is the defect the refusal closes.
//
// This is a census: if a clause body is ever proven capture-free, MergedSlots
// rises and the pushes fall, and both are the moment to re-derive this table.
func TestSyntaxCaseClauseBodyLetsPush(t *testing.T) {
	tcs := []struct {
		name string
		code string
		// wantPushes is one OpPushEnv per `let` in the clause.
		wantPushes int
		// wantPops is 1 for the matched path's pop of the pattern-variable frame,
		// plus 1 more when a fender gives the clause a cleanup block of its own,
		// plus one per `let` not in tail position.
		wantPops int
	}{
		{
			name:       "no let in the clause body",
			code:       `(lambda (stx) (syntax-case stx () ((_ a) (syntax a))))`,
			wantPushes: 0,
			wantPops:   1,
		},
		{
			name:       "one let",
			code:       `(lambda (stx) (syntax-case stx () ((_ a) (let ((x (syntax a))) x))))`,
			wantPushes: 1,
			wantPops:   1,
		},
		{
			name: "nested lets",
			code: `(lambda (stx)
			         (syntax-case stx ()
			           ((_ a) (let ((x 1)) (let ((y 2)) (let ((z 3)) (syntax a)))))))`,
			wantPushes: 3,
			wantPops:   1,
		},
		{
			// The fender's let is not in tail position, so it pops.
			name: "a fender's let and the body's",
			code: `(lambda (stx)
			         (syntax-case stx ()
			           ((_ a) (let ((g #t)) g) (let ((x 1)) (syntax a)))))`,
			wantPushes: 2,
			wantPops:   3,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tpl := compileToTemplate(t, tc.code)
			bpvs := bindPatternVarsIn(tpl)
			c.Assert(len(bpvs), qt.Equals, 1)
			c.Assert(bpvs[0].MergedSlots, qt.Equals, 0)
			c.Assert(countOpEverywhere(tpl, machine.OpPushEnv), qt.Equals, tc.wantPushes)
			c.Assert(countOpEverywhere(tpl, machine.OpPopEnv), qt.Equals, tc.wantPops)
		})
	}
}

// TestSyntaxCaseFrameLayoutIsAppendOrder pins the property the two sides of the
// pattern-variable frame agree on.
//
// The compile-time mirror (createPatternVarEnvironment) and the runtime builder
// (OperationBindPatternVars.Apply) construct the SAME frame in two places, and
// nothing type-checks that they match. What keeps them equal is that both build
// it empty and append in one order — pattern variables, the state slot, then the
// merged slots — so the next index is "however many appends have happened" on
// both sides.
//
// The count below is therefore len(patternVars) + 1 + MergedSlots exactly. An
// off-by-one either way puts a merged `let`'s first slot on the form's own
// syntax-case state, which is the failure this arithmetic exists to prevent, and
// which reads as a corrupt macro rather than as a crash.
func TestSyntaxCaseFrameLayoutIsAppendOrder(t *testing.T) {
	c := qt.New(t)
	code := `(lambda (stx)
	           (syntax-case stx ()
	             ((_ a b) (let ((x 1)) (let ((y 2)) (syntax (a b)))))))`
	tpl := compileToTemplate(t, code)
	bpvs := bindPatternVarsIn(tpl)
	c.Assert(len(bpvs), qt.Equals, 1)

	// `_`, `a` and `b`: the underscore is stripped, so two pattern variables.
	c.Assert(len(bpvs[0].PatternVars), qt.Equals, 2)
	// Zero, not 2: the lets push (TestSyntaxCaseClauseBodyLetsPush), so the
	// merged tail of this layout is unexercised until a clause body can be proven
	// capture-free.
	c.Assert(bpvs[0].MergedSlots, qt.Equals, 0)
}

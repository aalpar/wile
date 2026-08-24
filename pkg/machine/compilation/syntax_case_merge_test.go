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

// TestSyntaxCaseClauseBodyMergesLets pins that a `let` in a syntax-case clause
// body takes its slots out of the pattern-variable frame instead of pushing one.
//
// It could not, for as long as BindPatternVars sized the runtime frame from its
// own PatternVars list: a slot appended to the compile-time mirror had no
// runtime counterpart, so canMergeLet had to refuse. That refusal was the last
// closeable population of depth != 0 emit sites over the pkg/wile suite (4 hits,
// memory/flat-closure-baseline.local.md §7a's "syntax-case clause body").
//
// MergedSlots is the census. A merge that silently stopped happening would still
// evaluate correctly — the pushing form is what it falls back to — so no value
// assertion in the tree would notice.
func TestSyntaxCaseClauseBodyMergesLets(t *testing.T) {
	tcs := []struct {
		name string
		code string
		// wantMerged is the number of slots the clause body's lets take out of
		// the pattern-variable frame.
		wantMerged int
		// wantPops is 1 for the matched path's pop, plus 1 more when a fender
		// gives the clause a cleanup block of its own.
		wantPops int
	}{
		{
			name:       "no let in the clause body merges nothing",
			code:       `(lambda (stx) (syntax-case stx () ((_ a) (syntax a))))`,
			wantMerged: 0,
			wantPops:   1,
		},
		{
			name:       "one binding merges one slot",
			code:       `(lambda (stx) (syntax-case stx () ((_ a) (let ((x (syntax a))) x))))`,
			wantMerged: 1,
			wantPops:   1,
		},
		{
			// Three frames' worth of bindings, one frame. The inner lets merge
			// through the outer ones, which is canMergeLet's transitive walk.
			name: "nested lets merge into one frame",
			code: `(lambda (stx)
			         (syntax-case stx ()
			           ((_ a) (let ((x 1)) (let ((y 2)) (let ((z 3)) (syntax a)))))))`,
			wantMerged: 3,
			wantPops:   1,
		},
		{
			// The fender is compiled BEFORE the body and can merge too, which is
			// why the count is taken once, after both.
			name: "a fender's let merges alongside the body's",
			code: `(lambda (stx)
			         (syntax-case stx ()
			           ((_ a) (let ((g #t)) g) (let ((x 1)) (syntax a)))))`,
			wantMerged: 2,
			// A fender adds the cleanup block's pop: three exit edges out of a
			// clause, two of which unwind the pattern-variable frame.
			wantPops: 2,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tpl := compileToTemplate(t, tc.code)
			bpvs := bindPatternVarsIn(tpl)
			c.Assert(len(bpvs), qt.Equals, 1)
			c.Assert(bpvs[0].MergedSlots, qt.Equals, tc.wantMerged)
			// The merge is only real if no frame was pushed for it. The clause
			// itself pushes none: BindPatternVars is the frame.
			c.Assert(countOpEverywhere(tpl, machine.OpPushEnv), qt.Equals, 0)
			c.Assert(countOpEverywhere(tpl, machine.OpPopEnv), qt.Equals, tc.wantPops,
				qt.Commentf("every pop must balance BindPatternVars, never a merged let"))
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
	c.Assert(bpvs[0].MergedSlots, qt.Equals, 2)
}

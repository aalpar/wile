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

package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestEvalStackDepthsSeparatesGroupStarts is the unit-level statement of what
// the call-fusion guard needs: a push that OPENS a group answers 0, and a push
// that lands inside one already open does not — even when the instruction before
// it is neither a push nor a SaveContinuation, which is exactly the case the old
// guard mistook for a group start.
func TestEvalStackDepthsSeparatesGroupStarts(t *testing.T) {
	cases := []struct {
		name string
		code []Instruction
		want []int
	}{
		{
			// (f x) in tail position: the callee push opens the group.
			name: "tail call",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
			},
			want: []int{0, 1, 2},
		},
		{
			// (+ (begin (set! a 1) a) n). The store POPS, so the push after it
			// sits at depth 1 — inside the group `+` opened — not at a group
			// start. Answering 0 here is what rewrote the apply to call `a`.
			name: "push after a pop, inside an open group",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLiteral, Arg: 0},
				{Op: OpStoreLocal, Arg: 0},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
			},
			want: []int{0, 1, 2, 1, 2, 3},
		},
		{
			// (let ((a E)) (f a)) merged: E's own pushes are consumed by the
			// promoted op and the store, so the callee push IS at a group start.
			// A guard that refused it would deoptimize every merged let body.
			name: "balanced init leaves the next group start at zero",
			code: []Instruction{
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPushLiteral, Arg: 0},
				{Op: OpAdd, Arg: 0},
				{Op: OpPush},
				{Op: OpStoreLocal, Arg: 1},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
			},
			want: []int{0, 1, 2, 0, 1, 0, 1, 2},
		},
		{
			// A nested non-tail call is an atomic net-zero block: the callee runs
			// on a cleared stack, and the return point resumes at the caller's
			// depth. Without that pairing every argument that is itself a call
			// would poison the rest of the template.
			name: "SaveContinuation block is net zero",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpSaveContinuation, Arg: 4},
				{Op: OpPushCachedBinding, Arg: 1},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
				{Op: OpPush},
				{Op: OpPullApply},
			},
			want: []int{0, 1, 0, 1, 2, 1, 2},
		},
		{
			// An unmodelled opcode poisons what follows rather than guessing.
			name: "unknown op poisons its successors",
			code: []Instruction{
				{Op: OpUnpackListToStack},
				{Op: OpPushLocal, Arg: 0},
				{Op: OpPullApply},
			},
			want: []int{0, depthUnknown, depthUnknown},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, evalStackDepths(tc.code), qt.DeepEquals, tc.want)
		})
	}
}

// TestEvalStackDepthsMergeDisagreement pins that two paths reaching one index
// with different heights answer unknown rather than the first one seen.
func TestEvalStackDepthsMergeDisagreement(t *testing.T) {
	code := []Instruction{
		{Op: OpBranchOnFalseValue, Arg: 3}, // →3
		{Op: OpPushLiteral, Arg: 0},
		{Op: OpBranch, Arg: 2}, // →4
		{Op: OpLoadVoid},
		{Op: OpPushLocal, Arg: 0},
	}
	got := evalStackDepths(code)
	qt.Assert(t, got[4], qt.Equals, depthUnknown,
		qt.Commentf("index 4 is reached at height 1 through the consequent and 0 "+
			"through the alternative; answering either would license a fusion on "+
			"a path where it is wrong"))
}

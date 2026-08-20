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

// Value regression for a call-fusion defect: the peephole named a call's callee
// by operand after identifying it positionally, and the positional test was not
// the same question the runtime asks.

package wile

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestCallFusionPicksTheGroupsBottom is the peephole half.
//
// OpPullApply pulls its callee from the BOTTOM of the eval stack, so a fusion
// that names the callee by operand instead is correct only if the push it
// deleted was that bottom element. The pass identified it as "a push whose
// predecessor is neither SaveContinuation nor a push", which is a different
// question: the set! POPS, so the reload after it looked like a fresh group
// start while `+` was still sitting underneath. The apply was rewritten to call
// `a`. See machine/eval_depth.go.
//
// The two arms differ only in the popping form between the operator push and the
// argument pushes, so a pass cannot be explained by "calls work". The `let` arm
// does not reproduce on its own today — a let frame's OpPushEnv/OpPopEnv break
// the push run before the pattern can match — and is here because it is the
// shape that becomes ordinary the moment a let stops pushing a frame.
func TestCallFusionPicksTheGroupsBottom(t *testing.T) {
	cases := []struct {
		name string
		code string
		want string
	}{
		{
			name: "set! between the operator push and the arguments",
			code: `((lambda (a n) (+ (begin (set! a 1) a) n)) 5 3)`,
			want: "4",
		},
		{
			name: "let between the operator push and the arguments",
			code: `((lambda (n) (+ (let ((a (+ n 1))) a) n)) 10)`,
			want: "21",
		},
		{
			name: "control: no pop between them",
			code: `((lambda (a n) (+ a n)) 5 3)`,
			want: "8",
		},
	}
	ctx := context.Background()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			v, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

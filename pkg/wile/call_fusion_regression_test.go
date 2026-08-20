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

// TestTailCallAcrossFrameReclaimInMergedLet is the value guard for the fusion
// gate that let-slot merging made reachable.
//
// Once a merged `let` body keeps its frame-release disposition, a tail call
// inside one emits `<callee push> <arg pushes> ReleaseEnvFrame PullApply`. A
// fusion that deletes the callee push resolves the callee at the apply instead —
// AFTER OpReleaseEnvFrame has handed mc.env to the pool and ResetForPool zeroed
// it. That is sound for a callee read off the template or the free vector, and
// not for one read through mc.env.
//
// WHY THIS TEST EXISTS AS A VALUE ASSERTION. Widening releaseSafeCallee to admit
// OpPushLocal was measured against the whole suite: pkg/machine, pkg/wile and
// every Scheme corpus test still passed, and only the structural pin in
// peephole_test.go caught it. The arm below is the shape that actually fails —
// it reports `no such local binding 1:0`, because slot 1 is the released frame's
// zeroed callee slot. Without it the class is pinned only by a test that
// inspects bytecode, which a later fusion rewrite could satisfy while breaking
// the runtime.
//
// The three arms are the three callee kinds, and each reaches a different
// decision, so a pass cannot be explained by "tail calls work":
//
//   - local callee, release armed -> NOT fused, stays PushLocal…/PullApply
//   - Scheme global (cached binding), release armed -> fused to CallCachedBinding
//   - promoted primitive, release armed -> fused to the promoted tail op
//
// Verified by disassembly at the time of writing; asserted here only by value,
// because the point is that the ANSWER survives whichever way the peephole goes.
func TestTailCallAcrossFrameReclaimInMergedLet(t *testing.T) {
	cases := []struct {
		name     string
		code     string
		want     string
		noInline bool
	}{
		{
			// A let-bound lambda is a KNOWN, capture-safe callee, so the release
			// is armed — and it lives in a merged let slot, so the callee push is
			// a PushLocal. Inlining is disabled to keep the call a real call;
			// with it on, f's body is folded in and there is no callee at all.
			name:     "local callee is not resolved out of the released frame",
			code:     `(define (t n) (let ((f (lambda (x) (* x 2)))) (f n))) (t 21)`,
			want:     "42",
			noInline: true,
		},
		{
			name: "Scheme global callee survives the release it is fused across",
			code: `(define (h x) (* x 2)) (define (t n) (let ((k 1)) (h n))) (t 21)`,
			want: "42",
		},
		{
			name: "promoted primitive tail call survives the release",
			code: `(define (t n) (let ((k 1)) (+ n k))) (t 41)`,
			want: "42",
		},
	}
	ctx := context.Background()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			opts := []EngineOption{}
			if tc.noInline {
				opts = append(opts, WithInlineThreshold(0))
			}
			eng, err := NewEngine(ctx, opts...)
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

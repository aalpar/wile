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

// TestPromotedCompoundArgFusionPicksTheGroupsBottom is the same defect, in the
// pass that was written after the fix above and did not inherit it.
//
// fusePromotedCompoundArgs (peephole pass 4) kept the abandoned predecessor test
// and therefore mistook a promoted primitive pushed as an ARGUMENT for a tail
// callee, deleting the real callee's push and rewriting the apply into the
// promoted opcode. `(h (begin (set! a 1) car) n)` ran as `(car n)` — no error,
// no diagnostic, and the set-cdr! arm mutated a pair the program never asked it
// to. Every arm here returns a wrong value or a wrong effect without the fix,
// not merely a missed optimization.
//
// These must go through the engine, not RunSchemeCode, which skips
// tpl.Optimize() and would pass unfixed. The opposite direction — that the
// promotion this pass exists for did not deoptimize — is pinned structurally by
// TestPromoteCompoundArgsFib, since no value assertion can see a deopt.
func TestPromotedCompoundArgFusionPicksTheGroupsBottom(t *testing.T) {
	cases := []struct {
		name string
		code string
		want string
	}{
		{
			// Arity 1: `car` is pushed as h's first argument, `n` follows, and
			// the ReleaseEnvFrame proof is present — the full trigger shape.
			// Unfixed this answers 7, the value of (car n).
			name: "set! pops between the callee push and a unary promoted argument",
			code: `(define (h x y) (list 'h (procedure? x) y))
			       (define (g a n) (h (begin (set! a 1) car) n))
			       (g 0 '(7 8))`,
			want: "(h #t (7 8))",
		},
		{
			// The same, with an apply rather than a store doing the popping.
			// `(idl 0)` is there only for its drain.
			name: "an apply drains the group before the promoted argument push",
			code: `(define (h x y) (list 'h (procedure? x) y))
			       (define (idl v) v)
			       (define (g n) (h (begin (idl 0) car) n))
			       (g '(7 8))`,
			want: "(h #t (7 8))",
		},
		{
			// walkCallArgs also accepts SaveContinuation..PullApply..Push blocks,
			// so a nested-call argument is in the trigger set too.
			name: "nested-call argument after the misidentified push",
			code: `(define (h x y) (list 'h (procedure? x) y))
			       (define (idl v) v)
			       (define (g a n) (h (begin (set! a 1) car) (idl n)))
			       (g 0 '(7 8))`,
			want: "(h #t (7 8))",
		},
		{
			// Arity 2, where the miscompile is a side effect and not only a
			// value: the promoted `cons` consumed h3's own arguments.
			name: "binary promoted primitive as an argument value does not run",
			code: `(define (h3 x y z) (list 'h3 (procedure? x) y z))
			       (define (g a p v) (h3 (begin (set! a 1) cons) p v))
			       (g 0 1 2)`,
			want: "(h3 #t 1 2)",
		},
		{
			// Control: no pop between the callee push and the argument, so the
			// promoted push is genuinely not a group start under either test.
			// Without it the arms above could pass by total deoptimization.
			name: "control: promoted primitive argument with no pop before it",
			code: `(define (h x y) (list 'h (procedure? x) y))
			       (define (g n) (h car n))
			       (g '(7 8))`,
			want: "(h #t (7 8))",
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

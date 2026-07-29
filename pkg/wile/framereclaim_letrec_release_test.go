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

package wile

import (
	"testing"
)

// letrecSlope measures env-frame allocations per loop iteration for a letrec
// binding group. The fixed per-Run overhead cancels in the subtraction, leaving
// the per-call frame cost.
func letrecSlope(t *testing.T, setup, small, big string) float64 {
	t.Helper()
	return allocSlope(allocsForRun(t, setup, small), allocsForRun(t, setup, big), 10000, 30000)
}

// TestLetrecMutualRecursionFrameRelease is Phase D: a letrec group whose members
// tail-call EACH OTHER releases its activation frames.
//
// This is the shape self-tail structurally cannot reach. bodyIsSelfTailReusable
// requires a depth-0 tail call to the binding's own name; mutual recursion has
// none, so before this predicate every iteration allocated a frame — measured at
// 2.0 allocations per iteration.
//
// The floor cases are the point. Frame release is the optimization with three
// historical reverts behind it, and every one of them passed a green functional
// suite: releasing a frame that something still reaches corrupts a re-entered
// continuation rather than failing a test. So each negative below asserts the
// loop KEEPS allocating, which is the only observable that distinguishes
// "correctly refused" from "silently released anyway".
func TestLetrecMutualRecursionFrameRelease(t *testing.T) {
	const mutual = "(begin (define (mut n) " +
		"(letrec ((ev (lambda (i) (if (= i 0) 'done (od (- i 1))))) " +
		"         (od (lambda (i) (if (= i 0) 'done (ev (- i 1)))))) " +
		"  (ev n)))\n)"

	slope := letrecSlope(t, mutual, "(mut 10000)", "(mut 30000)")
	t.Logf("mutual recursion slope = %.3f frames/iter", slope)
	if slope > 0.1 {
		t.Errorf("letrec mutual recursion leaks env frames: %.3f frames/iter (want < 0.1) — "+
			"neither member has a depth-0 self call, so this is the release path or nothing", slope)
	}
}

// TestLetrecReleaseRefusals pins every path that must NOT arm release. Each case
// keeps allocating; a case that drops to zero means the predicate cleared a
// binding it cannot prove, which is a use-after-release rather than a lost
// optimization.
func TestLetrecReleaseRefusals(t *testing.T) {
	tests := []struct {
		name  string
		setup string
		small string
		big   string
		why   string
	}{
		{
			name: "a member captures the continuation",
			setup: "(begin (define (cc n) " +
				"(letrec ((e (lambda (i) (if (= i 0) 'done (o (- i 1))))) " +
				"         (o (lambda (i) (if (= i 0) 'done (begin (call/cc (lambda (k) i)) (e (- i 1))))))) " +
				"  (e n)))\n)",
			small: "(cc 10000)", big: "(cc 30000)",
			why: "a captured continuation pins the frame — and the group co-induction means " +
				"ONE capturing member must refuse the whole group, not just itself",
		},
		{
			name: "a member calls a procedure-invoking primitive",
			setup: "(begin (define (cm n) " +
				"(letrec ((e (lambda (i) (if (= i 0) 'done (o (- i 1))))) " +
				"         (o (lambda (i) (if (= i 0) 'done (begin (map (lambda (x) x) '(1)) (e (- i 1))))))) " +
				"  (e n)))\n)",
			small: "(cm 10000)", big: "(cm 30000)",
			why: "map invokes an unknown callback, which could capture the continuation that pins the frame",
		},
		{
			name: "a member is set! within the let",
			setup: "(begin (define (cs n) " +
				"(letrec ((e (lambda (i) (if (= i 0) 'done (o (- i 1))))) " +
				"         (o (lambda (i) (if (= i 0) 'done (e (- i 1)))))) " +
				"  (set! o o) (e n)))\n)",
			small: "(cs 10000)", big: "(cs 30000)",
			why: "after a set! the name may denote a capturing procedure, so the init stops " +
				"being evidence — localBinding.mutated is what refuses it",
		},
		{
			name: "a member creates an escaping closure in its own body",
			setup: "(begin (define (cb n) " +
				"(letrec ((e (lambda (i) (if (= i 0) 'done (begin (cons (lambda () i) '()) (o (- i 1)))))) " +
				"         (o (lambda (i) (if (= i 0) 'done (e (- i 1)))))) " +
				"  (e n)))\n)",
			small: "(cb 10000)", big: "(cb 30000)",
			why: "that closure parents the very frame being released — the one escape check " +
				"that is per-binding rather than group-wide",
		},
		{
			name: "plain let is not a recursive binding group",
			setup: "(begin (define (cl n) " +
				"(let ((s (lambda (i) i))) (if (= n 0) 'done (cl (- n 1)))))\n)",
			small: "(cl 10000)", big: "(cl 30000)",
			why: "in a plain let the bindings are not in scope in each other's inits, so the " +
				"group seed would describe OUTER bindings — the InitsInScope precondition",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			slope := letrecSlope(t, tt.setup, tt.small, tt.big)
			t.Logf("slope = %.3f frames/iter", slope)
			if slope < 1.0 {
				t.Errorf("loop stopped allocating (%.3f frames/iter) — release fired on a shape "+
					"it must refuse: %s", slope, tt.why)
			}
		})
	}
}

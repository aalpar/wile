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
	"context"
	"testing"
)

// TestFrameReclaim_LocalHelperVerdicts pins Lever B end to end, at the
// classifier verdict rather than at either predicate feeding it.
//
// That distinction is the point of this file. Two independent refusals had to
// lift together before any verdict moved: the escape fact
// (bodyCreatesEscapingClosure called every let-bound lambda escaping) and the
// callee fact (classifyCallee returned a nil-target edge for every lexically
// shadowed operator, which fails nodeSafe regardless of the escape fact). Unit
// tests on either predicate pass while the composite verdict stays false, so
// only an assertion at this level can tell "the lever works" from "the lever is
// correct and buys nothing" — which is exactly what the first implementation
// pass measured.
//
// Every loop-shaped procedure in the language reaches the classifier through
// this path, because a named let validates to a letrec whose single binding is
// the loop lambda.
func TestFrameReclaim_LocalHelperVerdicts(t *testing.T) {
	tests := []struct {
		name string
		src  string
		fn   string
		want bool
		why  string
	}{
		{
			name: "let-bound helper, called only",
			src:  "(begin (define (run n) (let ((step (lambda (x) (+ x 1)))) (if (= n 0) 'done (run (step (- n 1))))))\n)",
			fn:   "run",
			want: true,
			why:  "the plan's headline probe: a helper that is only invoked never outlives the call",
		},
		{
			name: "named let loop",
			src:  "(begin (define (nl n) (let loop ((i 0) (acc 0)) (if (>= i n) acc (loop (+ i 1) (+ acc i)))))\n)",
			fn:   "nl",
			want: true,
			why:  "the shape that made the corpus-wide difference between 42/68 stamps and 19/68 verdicts",
		},
		{
			name: "named let behind a guard",
			src:  "(begin (define (isp n) (if (< n 2) #f (let loop ((i 2)) (cond ((> (* i i) n) #t) ((= (modulo n i) 0) #f) (else (loop (+ i 1)))))))\n)",
			fn:   "isp",
			want: true,
			why:  "primes.scm's is-prime? verbatim — the let is nested inside an if, not at body top level",
		},
		{
			name: "letrec helper",
			src:  "(begin (define (lr l) (letrec ((rm (lambda (x) (if (null? x) '() (rm (cdr x)))))) (rm l)))\n)",
			fn:   "lr",
			want: true,
			why:  "explicit letrec, not just the named-let desugaring",
		},
		{
			name: "control: no lambda at all",
			src:  "(begin (define (plain n) (if (= n 0) 'done (plain (- n 1))))\n)",
			fn:   "plain",
			want: true,
			why:  "was already reclaimable — keeps the positives above from being read as a shape effect",
		},
		{
			name: "negative: lambda returned",
			src:  "(begin (define (esc n) (if (= n 0) (lambda (x) x) (esc (- n 1))))\n)",
			fn:   "esc",
			want: false,
			why:  "the returned closure parents the frame",
		},
		{
			name: "negative: let-bound then returned",
			src:  "(begin (define (lr2 n) (let ((s (lambda (x) x))) (if (= n 0) s (lr2 (- n 1)))))\n)",
			fn:   "lr2",
			want: false,
			why:  "binding it first must not launder the escape",
		},
		{
			name: "negative: let-bound then consed",
			src:  "(begin (define (lc n) (let ((s (lambda (x) x))) (if (= n 0) 'done (begin (cons s '()) (lc (- n 1))))))\n)",
			fn:   "lc",
			want: false,
			why:  "cons is capture-safe but RETAINS — capture-safety is not a licence to escape",
		},
		{
			name: "negative: let-bound passed to map",
			src:  "(begin (define (lm n) (let ((s (lambda (x) x))) (if (= n 0) 'done (begin (map s '(1 2)) (lm (- n 1))))))\n)",
			fn:   "lm",
			want: false,
			why:  "map invokes an unknown callback, which could capture the continuation pinning the frame",
		},
		{
			name: "negative: let-bound then set!",
			src:  "(begin (define (lsb n) (let ((s (lambda (x) x))) (set! s (lambda (y) y)) (if (= n 0) 'done (begin (s 1) (lsb (- n 1))))))\n)",
			fn:   "lsb",
			want: false,
			why:  "after a set! the init no longer describes what the name denotes",
		},
		{
			name: "negative: helper captured by an escaping closure",
			src:  "(begin (define (ic n) (let ((s (lambda (x) x))) (if (= n 0) 'done (begin (cons (lambda () (s 1)) '()) (ic (- n 1))))))\n)",
			fn:   "ic",
			want: false,
			why: "s is only CALLED, so the escape predicate clears it — the refusal must come from the " +
				"enclosing anonymous closure, which is the induction step the relaxation relies on",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			verdict := classifyAmbient(context.Background(), t, tt.src, true)
			got, present := verdict[tt.fn]
			if !present {
				t.Fatalf("classifier produced no verdict for %q (verdict=%v)", tt.fn, verdict)
			}
			if got != tt.want {
				t.Errorf("reclaimable[%s] = %v, want %v — %s", tt.fn, got, tt.want, tt.why)
			}
		})
	}
}
